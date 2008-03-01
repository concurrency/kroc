/*
 * ppi_dma.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

#include "camera_const.h"
#include <i2c.h>
#include <ov9655.h>

typedef struct _dma_desc_t dma_desc_t;
struct _dma_desc_t {
	dma_desc_t	*next;
	BYTE		*buffer;
	short 		config;
	short		pad0;
	WORDPTR		mobile;
} TVM_PACK;

typedef const struct _frame_desc_t frame_desc_t;
struct _frame_desc_t {
	UWORD		bytes;
	UWORD		pixels;
	unsigned short	width;
	unsigned short	height;
	BYTE		*cfg;
	UWORD		cfg_len;
} TVM_PACK;

static frame_desc_t	qqvga_frame = {
	.bytes		= 160 * 128 * 2,
	.pixels		= 160 * 128,
	.width		= 160,
	.height		= 128,
	.cfg		= ov9655_qqvga,
	.cfg_len	= sizeof(ov9655_qqvga)
};
static frame_desc_t	qvga_frame = {
	.bytes		= 320 * 256 * 2,
	.pixels		= 320 * 256,
	.width		= 320,
	.height		= 256,
	.cfg		= ov9655_qvga,
	.cfg_len	= sizeof(ov9655_qvga)
};
static frame_desc_t	vga_frame = {
	.bytes		= 640 * 512 * 2,
	.pixels		= 640 * 512,
	.width		= 640,
	.height		= 512,
	.cfg		= ov9655_vga,
	.cfg_len	= sizeof(ov9655_vga)
};
static frame_desc_t	sxga_frame = {
	.bytes		= 1280 * 1024 * 2,
	.pixels		= 1280 * 1024,
	.width		= 1280,
	.height		= 1024,
	.cfg		= ov9655_sxga,
	.cfg_len	= sizeof(ov9655_sxga)
};

/* DMA related */
#define			CAMERA_BUFFERS		2
static dma_desc_t	camera_dma[CAMERA_BUFFERS + 1];
static volatile short	camera_current		= 0;
static volatile	short	camera_ready		= 0;
static volatile short	camera_intr		= 0;

/* State related */
static unsigned short	camera_initialised	= 0;
static volatile short	camera_error		= 0;
static unsigned short	camera_running		= 0;
static frame_desc_t	*frame_setup		= NULL;

/* Channel related */
WORDPTR			camera_channel		= (WORDPTR) NOT_PROCESS_P;
static BYTE *volatile 	camera_buffer		= NULL;
static WORDPTR		camera_mobile		= (WORDPTR) NULL_P;

void init_camera (void)
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
	for (i = 0; i < CAMERA_BUFFERS; ++i) {
		camera_dma[i].next 	= &(camera_dma[(i + 1) & (CAMERA_BUFFERS - 1)]);
		camera_dma[i].buffer	= (void *) INVALID_ADDRESS;
		camera_dma[i].config	= 
			FLOW_LARGE | NDSIZE_5 | WDSIZE_16 | DMA2D | WNR | DMAEN | DI_EN;
		camera_dma[i].mobile	= (WORDPTR) NULL_P;
	}

	i = CAMERA_BUFFERS;
	camera_dma[i].next	= (dma_desc_t *) INVALID_ADDRESS;
	camera_dma[i].buffer	= (void *) INVALID_ADDRESS;
	camera_dma[i].config	= WDSIZE_16 | DMA2D | WNR | DMAEN | DI_EN;
	camera_dma[i].mobile	= (WORDPTR) NULL_P;

	/* Flush */
	SSYNC;

	/* Clear any pending interrupt status */
	*pDMA0_IRQ_STATUS = DMA_DONE | DMA_ERR;
	SSYNC;
	
	/* Enable DMA interrupts */
	*pSIC_IMASK |= IRQ_DMA0;
	SSYNC;
}

void flush_cache (void *data, unsigned int bytes);

static void allocate_camera_buffer (ECTX ectx, UWORD bytes, BYTE **buffer, WORDPTR *mobile)
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

static int camera_start (ECTX ectx, WORD stream)
{
	int i, bufs;

	camera_current	= 0;
	camera_ready	= 0;
	camera_error	= 0;

	/* Allocate buffers */
	bufs 	= stream ? CAMERA_BUFFERS : 1;
	i 	= stream ? 0 : CAMERA_BUFFERS;
	while (bufs--) {
		allocate_camera_buffer (
			ectx, frame_setup->bytes, 
			&(camera_dma[i].buffer), 
			&(camera_dma[i].mobile)
		);

		if (camera_dma[i].buffer == NULL) {
			/* Allocation failed; unwind and return */
			int j;

			for (j = 0; stream && j < i; ++j) {
				camera_dma[j].buffer = (void *) INVALID_ADDRESS;
				tvm_mt_release (ectx, camera_dma[j].mobile);
			}

			ectx->eflags &= ~EFLAG_MT;

			return -1;
		}

		++i;
	}

	/* Toggle running state */
	camera_running = stream;

	/* Setup PPI */
	*pPPI_COUNT = (frame_setup->width * 2) - 1; /* YUYV = 2 bytes per pixel */
	*pPPI_FRAME = frame_setup->height;

	/* Setup DMA */
	*pDMA0_X_COUNT	= frame_setup->width;
	*pDMA0_X_MODIFY	= 2;

	*pDMA0_Y_COUNT	= frame_setup->height;
	*pDMA0_Y_MODIFY	= 2;

	if (stream) {
		*pDMA0_NEXT_DESC_PTR = &(camera_dma[0]);
	} else {
		*pDMA0_NEXT_DESC_PTR = &(camera_dma[CAMERA_BUFFERS]);
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

static void camera_low_level_stop (void)
{
	/* Disable PPI; stop data-flow */
	*pPPI_CONTROL &= ~PORT_EN;
	SSYNC;

	/* Disable DMA */
	*pDMA0_CONFIG = 0;
	SSYNC;
}

static void camera_stop (ECTX ectx)
{
	int i;

	camera_low_level_stop ();

	/* Wait for DMA to stop */
	while (*pDMA0_IRQ_STATUS & DMA_RUN)
		continue;
	
	/* Allow FIFOs to drain; should already be drained */
	for (i = 0; i < 512; ++i)
		NOP;
	
	/* Release buffers */
	for (i = 0; i < CAMERA_BUFFERS; ++i) {
		camera_dma[i].buffer = (void *) INVALID_ADDRESS;
		tvm_mt_release (ectx, camera_dma[i].mobile);
	}

	camera_running = 0;
}

void handle_int8 (void)
{
	const unsigned short ppi_errors = LT_ERR_OVR | LT_ERR_UNDR | FT_ERR | OVR | UNDR;
	int wake = 0;

	if (*pDMA0_IRQ_STATUS & DMA_DONE) {
		if (camera_running) {
			/* Streaming mode */
			short current	= camera_current;
			BYTE *buffer	= camera_buffer;

			camera_current = (current + 1) & (CAMERA_BUFFERS - 1);
			
			if (buffer != NULL) {
				/* Swap buffers */
				WORDPTR r_mobile		= camera_dma[current].mobile;
				camera_dma[current].buffer	= buffer;
				camera_dma[current].mobile	= camera_mobile;
				camera_mobile			= r_mobile;
				wake				= 1;
			} else if (camera_ready < (CAMERA_BUFFERS - 1)) {
				camera_ready++;
			}
		} else {
			/* One-shot mode */
			camera_low_level_stop ();
			
			if (camera_channel != (WORDPTR) NOT_PROCESS_P) {
				camera_mobile = camera_dma[CAMERA_BUFFERS].mobile;
				wake = 1;
			}
		}
		
		/* Acknowledge interrupt */
		*pDMA0_IRQ_STATUS = DMA_DONE;
	} else if (*pPPI_STATUS & ppi_errors) {
		/* Save error(s) */
		camera_error |= *pPPI_STATUS;

		/* Stop PPI and DMA engine */
		camera_low_level_stop ();

		/* Clear error(s) */
		*pPPI_STATUS = ppi_errors;

		/* Release Waiting Process */
		wake = 1;
	}

	if (wake) {
		camera_buffer 		= NULL;
		camera_intr		= 1;
		raise_tvm_interrupt (TVM_INTR_PPI_DMA);
	}	
}

void complete_camera_interrupt (ECTX ectx)
{
	WORDPTR wptr;

	if (camera_intr && ((wptr = camera_channel) != (WORDPTR) NOT_PROCESS_P)) {
		WORDPTR ma = camera_mobile;

		if (!camera_error) {
			WORDPTR pointer = (WORDPTR) WORKSPACE_GET (wptr, WS_POINTER);
			write_word (pointer, (WORD) ma);
		} else if (ma != (WORDPTR) NULL_P) {
			tvm_mt_release (ectx, (void *) ma);
		}

		camera_channel	= (WORDPTR) NOT_PROCESS_P;
		camera_mobile	= (WORDPTR) NULL_P;
		BARRIER;
		camera_intr	= 0;

		ectx->add_to_queue (ectx, wptr);
	}
}

int camera_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	if (count != sizeof(WORD)) {
		return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
	}

	write_word ((WORDPTR) pointer, camera_error);

	return ECTX_CONTINUE;
}

int camera_out (ECTX ectx, WORD count, BYTEPTR pointer)
{
	WORD config, mode;
	
	if (count != sizeof(WORD)) {
		return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
	}

	config 	= read_word ((WORDPTR) pointer);
	mode	= config & CAMERA_MODE_MASK;

	if (camera_running) {
		camera_stop (ectx);
	}

	switch (mode) {
		case CAMERA_160_128:
			frame_setup = &qqvga_frame;
			break;
		case CAMERA_320_256:
			frame_setup = &qvga_frame;
			break;
		case CAMERA_640_512:
			frame_setup = &vga_frame;
			break;
		case CAMERA_1280_1024:
			frame_setup = &sxga_frame;
			break;
		default:
			frame_setup = NULL;
			break;
	}

	if ((frame_setup != NULL && !camera_initialised) || mode == CAMERA_INIT) {
		i2cwrite (0x30, ov9655_setup, sizeof(ov9655_setup) >> 1, 1);
		delay_us (500000);
		i2cwrite (0x30, ov9655_setup, sizeof(ov9655_setup) >> 1, 1);
		camera_initialised++;
	}

	if (frame_setup != NULL) {
		if (config & CAMERA_AUTO_ADJUST) {
			const unsigned char auto_bits_on[] = { 0x13, 0xcf };
			/* COM8 = 
			 * 	FAST AGC/AEC, 
			 * 	AEC unlimited step, 
			 * 	AEC time <1 line, 
			 * 	AGC, AWB, AEC
			 */
			i2cwrite (0x30, (unsigned char *) auto_bits_on, 1, 1);
		} else {
			const unsigned char auto_bits_off[] = { 0x13, 0x02 };
			/* COM8 = AWB */
			i2cwrite (0x30, (unsigned char *) auto_bits_off, 1, 1);
		}

		i2cwrite (0x30, frame_setup->cfg, frame_setup->cfg_len >> 1, 1);
		
		if (config & CAMERA_STREAMING) {
			int ret = camera_start (ectx, 1);
			if (ret) {
				camera_error = ret;
			}
		}
	}
				
	return ECTX_CONTINUE;
}

int camera_mt_in (ECTX ectx, WORDPTR pointer)
{
	WORDPTR mobile = (WORDPTR) NULL_P;
	short ready = 0;
	short error;
	BYTE *buffer;

	if ((error = camera_error)) {
		/* Bad... */
	} else if (camera_running) {
		allocate_camera_buffer (ectx, frame_setup->bytes, &buffer, &mobile);
		if (buffer == NULL) {
			ectx->eflags &= ~EFLAG_MT;
			error = 1;
		}
	} else if (frame_setup != NULL) {
		camera_channel = ectx->wptr;
		BARRIER;

		if ((error = camera_start (ectx, 0))) {
			camera_channel	= (WORDPTR) NOT_PROCESS_P;
			camera_error	= error;
		}
	} else {
		return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
	}

	if (!error && camera_running) {
		unsigned short imask;

		DISABLE_INTERRUPTS (imask);
		
		/* Intentionally test error again with interrupts disabled */
		if ((error = camera_error)) {
			/* Bad... */
		} else if ((ready = camera_ready)) {
			BYTE *curr_buf 	= (BYTE *) *pDMA0_START_ADDR;
			BYTE *curr_pos	= (BYTE *) *pDMA0_CURR_ADDR;

			/* If we are in the last 2.5KiB of the current
			 * transfer then wait for new frame instead.
			 * Last 2.5KiB is 12.5% of a 160x128 frame,
			 * or the last line of a 1280x1024 frame.
			 */
			curr_buf += frame_setup->bytes - 2560;
			if (curr_pos >= curr_buf) {
				camera_ready = ready = 0;
			} else {
				unsigned short n = (camera_current - ready) & (CAMERA_BUFFERS - 1);
				dma_desc_t *db = &(camera_dma[n]);
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

				camera_ready = ready - 1;
			}
		}
		
		if (!error && !ready) {
			camera_channel 	= ectx->wptr;
			camera_buffer	= buffer;
			camera_mobile	= mobile;
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

