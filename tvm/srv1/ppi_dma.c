/*
 * ppi_dma.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

#include "ppi_dma_const.h"

typedef struct _dma_desc_t dma_desc_t;
struct _dma_desc_t {
	dma_desc_t	*next;
	BYTE		*buffer;
	short 		config;
	short		pad0;
	WORDPTR		mobile;
} TVM_PACK;

/* DMA related */
#define			DMA_BUFFERS	2
static dma_desc_t TVM_WORD_ALIGN
			dma[DMA_BUFFERS + 1];
static volatile short	dma_current	= 0;
static volatile	short	dma_ready	= 0;
static volatile short	dma_error	= 0;
static unsigned short	dma_running	= 0;

/* Mode related */
static unsigned short	frame_width	= 0;
static unsigned short	frame_height	= 0;
static unsigned short	frame_bpp	= 0;
static unsigned int	frame_length	= 0;

/* Channel related */
WORDPTR			channel		= (WORDPTR) NOT_PROCESS_P;
static BYTE *volatile 	dma_buffer	= NULL;
static WORDPTR		dma_mobile	= (WORDPTR) NULL_P;

void init_ppi_dma (void)
{
	int i;

	/* Port G: 0-7 = PPI */
	*pPORTG_FER |= 0x00FF;
	/* Port F: 15 = PPI_CLK, 9 = FS1, 8 = FS2 */
	*pPORTF_FER |= 0x8300;

	*pPPI_CONTROL = DLEN_8 | PACK_EN
		| (XFR_TYPE & 0x0C)	/* non ITU-R 656 mode */
		| (PORT_CFG & 0x20);	/* 2 or 3 internal frame syncs */
	*pPPI_DELAY = 0;

	/* Initialise DMA descriptors */
	for (i = 0; i < DMA_BUFFERS; ++i) {
		dma[i].next 	= &(dma[(i + 1) & (DMA_BUFFERS - 1)]);
		dma[i].buffer	= (void *) INVALID_ADDRESS;
		dma[i].config	= 
			FLOW_LARGE | NDSIZE_5 | WDSIZE_16 | DMA2D | WNR | DMAEN | DI_EN;
		dma[i].mobile	= (WORDPTR) NULL_P;
	}

	/* One shot descriptor */
	i = DMA_BUFFERS;
	dma[i].next	= (dma_desc_t *) INVALID_ADDRESS;
	dma[i].buffer	= (void *) INVALID_ADDRESS;
	dma[i].config	= WDSIZE_16 | DMA2D | WNR | DMAEN | DI_EN;
	dma[i].mobile	= (WORDPTR) NULL_P;

	/* Flush */
	SSYNC;

	/* Clear any pending interrupt status */
	*pDMA0_IRQ_STATUS = DMA_DONE | DMA_ERR;
	SSYNC;
	
	/* Enable DMA interrupts on IVG 8 */
	*pSIC_IAR0	= (*pSIC_IAR0 & ~(0xf << 0x10)) | P4_IVG(8);
	*pSIC_IMASK	|= IRQ_DMA0;
	SSYNC;
}

void flush_cache (void *data, unsigned int bytes);

static void allocate_buffer (ECTX ectx, UWORD bytes, BYTE **buffer, WORDPTR *mobile)
{
	WORDPTR ma = (WORDPTR) tvm_mt_alloc (
		ectx, 
		MT_MAKE_ARRAY_TYPE (1, MT_MAKE_NUM (MT_NUM_BYTE)),
		bytes
	);
	
	if (ma != NULL) {
		write_offset (ma, mt_array_t, dimensions[0], bytes);

		*buffer = (BYTE *) wordptr_real_address ((WORDPTR) read_offset (ma, mt_array_t, data));

		flush_cache (*buffer, bytes);
	} else {
		*buffer = NULL;
	}
	
	*mobile = ma;
}

static int dma_start (ECTX ectx, WORD stream)
{
	int i, bufs;

	dma_current	= 0;
	dma_ready	= 0;
	dma_error	= 0;

	/* Allocate buffers */
	bufs 	= stream ? DMA_BUFFERS : 1;
	i 	= stream ? 0 : DMA_BUFFERS;
	while (bufs--) {
		allocate_buffer (
			ectx, frame_length, 
			&(dma[i].buffer), 
			&(dma[i].mobile)
		);

		if (dma[i].buffer == NULL) {
			/* Allocation failed; unwind and return */
			int j;

			for (j = 0; stream && j < i; ++j) {
				dma[j].buffer = (void *) INVALID_ADDRESS;
				tvm_mt_release (ectx, dma[j].mobile);
			}

			ectx->eflags &= ~EFLAG_MT;

			return -1;
		}

		++i;
	}

	/* Toggle running state */
	dma_running = stream;

	/* Setup PPI */
	*pPPI_COUNT = (frame_width * frame_bpp) - 1;
	*pPPI_FRAME = frame_height;

	/* Setup DMA */
	*pDMA0_X_COUNT	= frame_width;
	*pDMA0_X_MODIFY	= frame_bpp;

	*pDMA0_Y_COUNT	= frame_height;
	*pDMA0_Y_MODIFY	= frame_bpp;

	if (stream) {
		*pDMA0_NEXT_DESC_PTR = &(dma[0]);
	} else {
		*pDMA0_NEXT_DESC_PTR = &(dma[DMA_BUFFERS]);
	}

	*pDMA0_CONFIG = FLOW_LARGE | NDSIZE_5 | WDSIZE_16 | DMA2D | WNR;

	/* Flush configuration */
	SSYNC;

	/* Enable DMA */
	*pDMA0_CONFIG |= DMAEN;
	SSYNC;

	/* Enable PPI */
	*pPPI_CONTROL |= PORT_EN;
	SSYNC;

	return 0;
}

static void low_level_stop (void)
{
	/* Disable PPI; stop data-flow */
	*pPPI_CONTROL &= ~PORT_EN;
	SSYNC;

	/* Disable DMA */
	*pDMA0_CONFIG = 0;
	SSYNC;
}

static void dma_stop (ECTX ectx)
{
	int i;

	low_level_stop ();

	/* Wait for DMA to stop */
	while (*pDMA0_IRQ_STATUS & DMA_RUN)
		continue;
	
	/* Allow FIFOs to drain; should already be drained */
	for (i = 0; i < 512; ++i)
		NOP;
	
	/* Release buffers */
	for (i = 0; i < DMA_BUFFERS; ++i) {
		dma[i].buffer = (void *) INVALID_ADDRESS;
		tvm_mt_release (ectx, dma[i].mobile);
	}

	dma_running = 0;
}

void handle_int8 (void)
{
	const unsigned short ppi_errors = LT_ERR_OVR | LT_ERR_UNDR | FT_ERR | OVR | UNDR;
	int wake = 0;

	if (*pDMA0_IRQ_STATUS & DMA_DONE) {
		if (dma_running) {
			/* Streaming mode */
			short current	= dma_current;
			BYTE *buffer	= dma_buffer;

			dma_current = (current + 1) & (DMA_BUFFERS - 1);
			
			if (buffer != NULL) {
				/* Swap buffers */
				WORDPTR r_mobile	= dma[current].mobile;
				dma[current].buffer	= buffer;
				dma[current].mobile	= dma_mobile;
				dma_mobile		= r_mobile;
				wake			= 1;
			} else if (dma_ready < (DMA_BUFFERS - 1)) {
				dma_ready++;
			}
		} else {
			/* One-shot mode */
			low_level_stop ();
			
			if (channel != (WORDPTR) NOT_PROCESS_P) {
				dma_mobile = dma[DMA_BUFFERS].mobile;
				wake = 1;
			}
		}
		
		/* Acknowledge interrupt */
		*pDMA0_IRQ_STATUS = DMA_DONE;
	} else if (*pPPI_STATUS & ppi_errors) {
		/* Save error(s) */
		dma_error |= *pPPI_STATUS;

		/* Stop PPI and DMA engine */
		low_level_stop ();

		/* Clear error(s) */
		*pPPI_STATUS = ppi_errors;

		/* Release Waiting Process */
		wake = 1;
	}

	if (wake) {
		dma_buffer = NULL;
		raise_tvm_interrupt (TVM_INTR_PPI_DMA);
	}	
}

void complete_ppi_dma_interrupt (ECTX ectx)
{
	WORDPTR wptr	= channel;
	WORDPTR ma	= dma_mobile;

	if (!dma_error) {
		WORDPTR pointer = (WORDPTR) WORKSPACE_GET (wptr, WS_POINTER);
		write_word (pointer, (WORD) ma);
	} else if (ma != (WORDPTR) NULL_P) {
		tvm_mt_release (ectx, (void *) ma);
	}

	channel		= (WORDPTR) NOT_PROCESS_P;
	dma_mobile	= (WORDPTR) NULL_P;

	ectx->add_to_queue (ectx, wptr);
}

int ppi_dma_is_blocking (void)
{
	/* Return non-zero if channel is not NOT_PROCESS_P. */
	return ((int) channel) ^ NOT_PROCESS_P;
}

int ppi_dma_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	if (count != sizeof(WORD)) {
		return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
	}

	write_word ((WORDPTR) pointer, dma_error);

	return ECTX_CONTINUE;
}

int ppi_dma_out (ECTX ectx, WORD count, BYTEPTR pointer)
{
	WORDPTR config = (WORDPTR) pointer;
	WORD	mode;
	
	/* mode, width, height, bpp */
	if (count != (sizeof(WORD) * 4)) {
		return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
	}
	
	if (dma_running) {
		dma_stop (ectx);
	}

	mode = read_word (wordptr_plus (config, 0));
	
	if (mode != PPI_DMA_STOP) {
		frame_width 	= read_word (wordptr_plus (config, 1));
		frame_height 	= read_word (wordptr_plus (config, 2));
		frame_bpp 	= read_word (wordptr_plus (config, 3));
		frame_length	= frame_width * frame_height * frame_bpp;
		
		if (mode == PPI_DMA_STREAM) {
			int ret = dma_start (ectx, 1);
			if (ret) {
				dma_error = ret;
			}
		}
	} else {
		frame_length	= 0;
	}
	
	return ECTX_CONTINUE;
}

int ppi_dma_mt_in (ECTX ectx, WORDPTR pointer)
{
	WORDPTR mobile = (WORDPTR) NULL_P;
	short ready = 0;
	short error;
	BYTE *buffer;

	if ((error = dma_error)) {
		/* Bad... */
	} else if (dma_running) {
		allocate_buffer (ectx, frame_length, &buffer, &mobile);
		if (buffer == NULL) {
			ectx->eflags &= ~EFLAG_MT;
			error = 1;
		}
	} else if (frame_length != 0) {
		channel = ectx->wptr;
		BARRIER;

		if ((error = dma_start (ectx, 0))) {
			channel		= (WORDPTR) NOT_PROCESS_P;
			dma_error	= error;
		}
	} else {
		return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
	}

	if (!error && dma_running) {
		unsigned short imask;

		DISABLE_INTERRUPTS (imask);
		
		/* Intentionally test error again with interrupts disabled */
		if ((error = dma_error)) {
			/* Bad... */
		} else if ((ready = dma_ready)) {
			BYTE *curr_buf 	= (BYTE *) *pDMA0_START_ADDR;
			BYTE *curr_pos	= (BYTE *) *pDMA0_CURR_ADDR;

			/* If we are in the last 2.5KiB of the current
			 * transfer then wait for new frame instead.
			 * Last 2.5KiB is 12.5% of a 160x128 frame,
			 * or the last line of a 1280x1024 frame.
			 */
			curr_buf += frame_length - 2560;
			if (curr_pos >= curr_buf) {
				dma_ready = ready = 0;
			} else {
				unsigned short n = (dma_current - ready) & (DMA_BUFFERS - 1);
				dma_desc_t *db = &(dma[n]);
				WORDPTR r_mobile;

				/* Be defensive; never touch the descriptors while
				 * the DMA controller is fetching.  In practice
				 * this is *very* unlikely to happen, but just to 
				 * be sure.
				 */
				while (*pDMA0_IRQ_STATUS & DFETCH)
					continue;

				/* Swap buffers */
				db->buffer = buffer;
				r_mobile = db->mobile;
				db->mobile = mobile;
				mobile = r_mobile;

				SSYNC;

				dma_ready = ready - 1;
			}
		}
		
		if (!error && !ready) {
			channel 	= ectx->wptr;
			dma_buffer	= buffer;
			dma_mobile	= mobile;
		}

		ENABLE_INTERRUPTS (imask);
	}

	if (!error && !ready) {
		/* Save pointer */
		WORKSPACE_SET (ectx->wptr, WS_POINTER, (WORD) pointer);
		/* Save instruction pointer */
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
		/* Reschedule */
		return ectx->run_next_on_queue (ectx);
	} else {
		write_word (pointer, (WORD) mobile);
	
		return ECTX_CONTINUE;
	}
}

