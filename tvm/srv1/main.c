/*
 * main.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2007-2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

/* TVM */
#include <tvm.h>
#define MT_TVM
#define MT_DEFINES
#include <mobile_types.h>
/* Blackfin */
#include <cdefBF537.h>
/* Configuration */
#include "bfin_config.h"
#include "camera_const.h"
#include "memory_map.h"
/* Support code */
//#include <camera.h>
#include <font8x8.h>
#include <i2cwrite.h>
#include <jpeg.h>
#include <ov9655.h>

#define BARRIER	__asm__ __volatile__ ("" : : : "memory")
#define CSYNC	__asm__ __volatile__ ("csync;" : : : "memory")
#define SSYNC	__asm__ __volatile__ ("ssync;" : : : "memory")
#define IDLE	__asm__ __volatile__ ("idle;")
#define NOP	__asm__ __volatile__ ("nop;")
#define DISABLE_INTERRUPTS(mask) \
	__asm__ __volatile__ ("cli %0;" : "=d" (mask))
#define ENABLE_INTERRUPTS(mask) \
	__asm__ __volatile__ ("sti %0;" : : "d" (mask))
	

static unsigned char version_string[] = "##TVM SRV-1 Blackfin - " __TIME__ " - " __DATE__;

/*{{{  TVM state */
static tvm_t 		tvm;
static tvm_ectx_t 	firmware_ctx, user_ctx;
/*}}}*/

/*{{{  I/O functionality */
static void init_io (void)
{
	/* Port H = GPIO */
	*pPORTH_FER 	= 0;
	*pPORTHIO	= 0;
}
/*}}}*/

/*{{{  Timer functionality */
volatile unsigned long core_timer_wrap_count;

static void init_timers (void)
{
	core_timer_wrap_count	= 0;
	*pTSCALE		= ((CORE_CLOCK / 2000000) - 1);
	*pTPERIOD		= 0xffffffff;
	*pTCOUNT		= 0xffffffff;
	SSYNC;
	*pTCNTL			= TMREN_P | TAUTORLD_P | TINT_P;
	CSYNC;
}

/* Read the time counter, returns number of microseconds since reset */
static WORD read_time (void)
{
	unsigned long pre_w, post_w;
	unsigned long tcount;

	do {
		pre_w 	= core_timer_wrap_count;
		tcount 	= *pTCOUNT;
		CSYNC;
		post_w	= core_timer_wrap_count;
	} while (pre_w != post_w);

	return (~tcount) >> 1 | (post_w << 31);
}

static void delay_us (WORD delay)
{
	int timeout = read_time () + delay;

	if ((delay < 0) || (delay >= (1 << 30)))
		return;
	while (read_time () < timeout)
		continue;
}

static WORD srv_get_time (ECTX ectx)
{
	return read_time ();
}
/*}}}*/

/*{{{  UART functionality */
static volatile WORDPTR 	uart0_channel 	= (WORDPTR) NOT_PROCESS_P;
static volatile BYTEPTR		uart0_ptr	= (BYTEPTR) NULL_P;
static volatile unsigned char 	uart0_buffer	= '\0';
static volatile short		uart0_pending	= 0;
static volatile short		uart0_intr	= 0;

static void init_uart (void)
{
	unsigned char temp;

	/* Configure port H pin 6 (PH6) for flow control CTS */
	*pPORTHIO_DIR	|= 0x0040;
	/* Configure PH0 for flow control RTS */
	*pPORTHIO_INEN	= 0x0001;
	/* Raise PH6 to block input */
	*pPORTHIO	|= 0x0040;
	
	/* Enable UART pins on port F */
	*pPORTF_FER 	|= 0x000f;

	/* Enable UART0 clocks */
	*pUART0_GCTL	= UCEN;
	/* Switch on divisor programming */
	*pUART0_LCR	= DLAB;
	/* Program divisor */
	*pUART0_DLL	= UART0_DIVIDER;
	*pUART0_DLH	= UART0_DIVIDER >> 8;
	/* Set operational mode (disables divisor programming) */
	*pUART0_LCR	= WLS(8); /* 8 bit, no parity, one stop bit */

	/* Reads to clear possible pending errors / irqs */
	temp = *pUART0_RBR;
	temp = *pUART0_LSR;
	temp = *pUART0_IIR;
	SSYNC;

	/* Enable receive interrupts */
	*pSIC_IMASK |= IRQ_DMA8;
	*pUART0_IER |= ERBFI;
	SSYNC;
}

void handle_int10 (void)
{
	unsigned char buffer;

	/* Raise (clear) CTS */
	*pPORTHIO	= *pPORTHIO | 0x0040;

	/* Read character (clears interrupt condition) */
	buffer		= *pUART0_RBR;

	/* Is anything waiting for the character? */
	if (uart0_ptr != (BYTEPTR) NULL_P) {
		/* Yes; give it the data and trigger requeue */
		write_byte (uart0_ptr, (BYTE) buffer);
		uart0_ptr		= (BYTEPTR) NULL_P;
		uart0_intr		= 1;
		firmware_ctx.sflags	|= SFLAG_INTR;
		user_ctx.sflags		|= SFLAG_INTR;
	} else {
		/* No; buffer the character */
		uart0_buffer = buffer;
		uart0_pending++;
	}
}

static void complete_uart0_interrupt (void)
{
	WORDPTR wptr;

	if (uart0_intr && ((wptr = uart0_channel) != (WORDPTR) NOT_PROCESS_P)) {
		firmware_ctx.add_to_queue (&firmware_ctx, wptr);
		uart0_channel		= (WORDPTR) NOT_PROCESS_P;
		uart0_intr		= 0;
	}
}

static int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	unsigned short imask;
	int reschedule;
	
	DISABLE_INTERRUPTS (imask);

	if (uart0_pending) {
		write_byte (pointer, (BYTE) uart0_buffer);
		uart0_pending	= 0;
		reschedule	= 0;
	} else {
		uart0_channel	= ectx->wptr;
		uart0_ptr	= pointer;
		reschedule	= 1;
	}

	ENABLE_INTERRUPTS (imask);

	if (reschedule) {
		/* Lower (set) CTS */
		*pPORTHIO = *pPORTHIO & (~0x0040);
		/* Save instruction pointer */
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
		/* Reschedule */
		return ectx->run_next_on_queue (ectx);
	} else {
		return ECTX_CONTINUE;
	}
}

static void uart0_send_char (unsigned char c)
{
	/* Wait for RTS to go low (remote ready) */
	while (*pPORTHIO & 0x0001) {
		continue;
	}

	/* Wait for UART0 send buffer to be ready */
	while (!(*pUART0_LSR & THRE)) {
		continue;
	}

	/* Send data */
	*pUART0_THR = c;
}

static void uart0_send_string (unsigned char *str)
{
	unsigned char c;
	while ((c = *str++) != '\0') {
		uart0_send_char (c);
	}
}


static int uart0_out (ECTX ectx, WORD count, BYTEPTR pointer)
{
	/* If count is the size of a word then throw away data,
	 * as it will be the count from a counted array
	 * output.  This should change in future.
	 */
	if (count != sizeof(WORD)) {
		while (count--) {
			uart0_send_char (read_byte (pointer));
			pointer = byteptr_plus (pointer, 1);
		}
	}

	return ECTX_CONTINUE;
}
/*}}}*/

/*{{{  Camera support */
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
static WORDPTR		camera_channel		= (WORDPTR) NOT_PROCESS_P;
static BYTE *volatile 	camera_buffer		= NULL;
static WORDPTR		camera_mobile		= (WORDPTR) NULL_P;

static void init_camera (void)
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
		firmware_ctx.sflags	|= SFLAG_INTR;
		user_ctx.sflags		|= SFLAG_INTR;
	}	
}

static void complete_camera_interrupt (void)
{
	WORDPTR wptr;

	if (camera_intr && ((wptr = camera_channel) != (WORDPTR) NOT_PROCESS_P)) {
		WORDPTR ma = camera_mobile;

		if (!camera_error) {
			WORDPTR pointer = (WORDPTR) WORKSPACE_GET (wptr, WS_POINTER);
			write_word (pointer, (WORD) ma);
		} else if (ma != (WORDPTR) NULL_P) {
			tvm_mt_release (&firmware_ctx, (void *) ma);
		}

		camera_channel	= (WORDPTR) NOT_PROCESS_P;
		camera_mobile	= (WORDPTR) NULL_P;
		BARRIER;
		camera_intr	= 0;

		firmware_ctx.add_to_queue (&firmware_ctx, wptr);
	}
}

static int camera_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	if (count != sizeof(WORD)) {
		return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
	}

	write_word ((WORDPTR) pointer, camera_error);

	return ECTX_CONTINUE;
}

static int camera_out (ECTX ectx, WORD count, BYTEPTR pointer)
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
		i2cwrite (0x30, ov9655_setup, sizeof(ov9655_setup) >> 1);
		delay_us (500000);
		i2cwrite (0x30, ov9655_setup, sizeof(ov9655_setup) >> 1);
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
			i2cwrite (0x30, (unsigned char *) auto_bits_on, 1);
		} else {
			const unsigned char auto_bits_off[] = { 0x13, 0x02 };
			/* COM8 = AWB */
			i2cwrite (0x30, (unsigned char *) auto_bits_off, 1);
		}

		i2cwrite (0x30, frame_setup->cfg, frame_setup->cfg_len >> 1);
		
		if (config & CAMERA_STREAMING) {
			int ret = camera_start (ectx, 1);
			if (ret) {
				camera_error = ret;
			}
		}
	}
				
	return ECTX_CONTINUE;
}

static int camera_mt_in (ECTX ectx, WORDPTR pointer)
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

/* PROC jpeg.encode.frame (VAL INT width, height, quality, 
 * 			VAL []BYTE input, []BYTE output, INT used) */
static int jpeg_encode_frame (ECTX ectx, WORD args[])
{
	WORD	width		= args[0];
	WORD	height		= args[1];
	WORD	quality		= args[2];
	BYTEPTR	input		= (BYTEPTR) args[3];
	WORD	input_len	= args[4];
	BYTEPTR	output		= (BYTEPTR) args[5];
	WORD	output_len	= args[6];
	WORDPTR	used		= (WORDPTR) args[7];
	BYTEPTR end;

	if (quality < 1) {
		quality = 1;
	} else if (quality > 8) {
		quality = 8;
	}

	/* Input buffer must be big enough to be a frame */
	/* Output buffer must be at least as 1/4 of the input buffer */
	if ((((width * height) << 1) > input_len) || (output_len < (input_len >> 4))) {
		/* Bad buffer sizes, return -1 */
		write_word (used, -1);
	} else {
		input	= (BYTEPTR) wordptr_real_address ((WORDPTR) input);
		output 	= (BYTEPTR) wordptr_real_address ((WORDPTR) output);

		end = (BYTEPTR) encode_image (
			(unsigned char *) input,
			(unsigned char *) output,
			quality,
			width,
			height
		);
		/* Return output size */
		write_word (used, ((WORD) end) - ((WORD) output));
	}

	return SFFI_OK;
}

/* PROC draw.caption.on.frame (VAL INT frame.width, VAL []BYTE caption, []BYTE frame) */
static int draw_caption_on_frame (ECTX ectx, WORD args[])
{
	WORD	width		= args[0];
	BYTEPTR	caption		= (BYTEPTR) args[1];
	WORD	caption_len	= args[2];
	BYTE	*frame		= (BYTE *) wordptr_real_address ((WORDPTR) args[3]);
	/* WORD	frame_len	= args[4]; */
	int 	ix, iy, iz;
	
	/* Limit caption length */
	if (caption_len > 40) {
		caption_len = 40;
	}

	/* Move to first character position */
	frame = frame + (((width * 16) + width) - (caption_len * 8));

	for (ix = 0; ix < caption_len; ix++) {
		unsigned int c = (unsigned int) read_byte (byteptr_plus (caption, ix));
		BYTE *fcur = frame;

		for (iy = 0; iy < 8; iy++) {
			BYTE cc = font8x8[(c * 8) + iy];

			for (iz = 0; iz < 8; iz++) {
				if (cc & fontmask[iz]) {
					fcur[0] = 0x80;
					fcur[1] = 0xff;
				}
				fcur += 2;
			}
			
			/* Move to next line */
			fcur += (width * 2) - 16;
		}
		
		/* Move to next character */
		frame += 16;
	}

	return SFFI_OK;
}
/*}}}*/

/*{{{  Scheduling support */
static void srv_modify_sync_flags (ECTX ectx, WORD set, WORD clear)
{
	unsigned short imask;
	
	DISABLE_INTERRUPTS (imask);

	ectx->sflags = (ectx->sflags & (~clear)) | set;

	ENABLE_INTERRUPTS (imask);
}

static void clear_pending_interrupts (void)
{
	unsigned short imask;
	
	DISABLE_INTERRUPTS (imask);
	
	firmware_ctx.sflags	&= ~(SFLAG_INTR);
	user_ctx.sflags		&= ~(SFLAG_INTR);

	ENABLE_INTERRUPTS (imask);

	complete_camera_interrupt ();
	complete_uart0_interrupt ();
}
/*}}}*/ 

/*{{{  External channel definitions */
static EXT_CHAN_ENTRY	ext_chans[] = {
	{ 
		.typehash 	= 0,
		.in 		= uart0_in, 
		.out 		= uart0_out,
		.mt_in		= NULL,
		.mt_out		= NULL
	},
	{
		.typehash	= 0,
		.in		= camera_in,
		.out		= camera_out,
		.mt_in		= camera_mt_in,
		.mt_out		= NULL
	}
};
static const int	ext_chans_length =
				sizeof(ext_chans) / sizeof(EXT_CHAN_ENTRY);
/*}}}*/

/*{{{  User context state */
static BYTEPTR		user_bytecode;
static WORD		user_bytecode_len;
static WORDPTR		user_memory;
static WORD		user_memory_len;
static WORDPTR 		user_parent	= (WORDPTR) NOT_PROCESS_P;
/*}}}*/

/*{{{  Firmware functions for running user bytecode */
/* PROC firmware.run.user (VAL []BYTE bytecode, VAL INT ws, vs, ms, 
 * 				VAL []BYTE tlp, ...) */
static int firmware_run_user (ECTX ectx, WORD args[])
{
	BYTEPTR bytecode	= (BYTEPTR) args[0];
	WORD	bytecode_len	= args[1];
	WORD	ws_size		= args[2];
	WORD	vs_size		= args[3];
	WORD	ms_size		= args[4];
	char	*tlp_fmt	= (char *) wordptr_real_address ((WORDPTR) args[5]);
	WORD	argc		= args[6];
	WORDPTR	argv		= (WORD *) &(args[7]);
	WORDPTR	ws, vs, ms;
	WORD	ret_addr;
	int ret;

	if (user_parent != (WORDPTR) NOT_PROCESS_P) {
		/* User context is already running */
		return ectx->set_error_flag (ectx, EFLAG_FFI);
	}

	tvm_ectx_reset (&user_ctx);
	user_memory_len = tvm_ectx_memory_size (
		&user_ctx, tlp_fmt, argc, 
		ws_size, vs_size, ms_size
	);
	user_memory = (WORDPTR) tvm_malloc (ectx, user_memory_len << WSH);
	tvm_ectx_layout (
		&user_ctx, user_memory, 
		tlp_fmt, argc, 
		ws_size, vs_size, ms_size,
		&ws, &vs, &ms
	);
	ret = tvm_ectx_install_tlp (
		&user_ctx, bytecode, ws, vs, ms,
		tlp_fmt, argc, argv
	);
	if (ret) {
		/* Install TLP failed */
		return ectx->set_error_flag (ectx, EFLAG_FFI);
	}

	/* Save bytecode addresses */
	user_bytecode		= bytecode;
	user_bytecode_len	= bytecode_len;

	/* Simulate return, and deschedule */
	ret_addr	= read_word (ectx->wptr);
	/* Push WPTR up 4 words */
	user_parent 	= wordptr_plus (ectx->wptr, 4);
	/* Store return address as descheduled IPTR */
	WORKSPACE_SET (user_parent, WS_IPTR, ret_addr);
	/* Save execution context for good measure */
	WORKSPACE_SET (user_parent, WS_ECTX, (WORD) ectx);

	return SFFI_RESCHEDULE;
}

/* PROC firmware.kill.user () */
static int firmware_kill_user (ECTX ectx, WORD args[])
{
	if (user_parent != (WORDPTR) NOT_PROCESS_P)
	{
		/* Restore parent in firmware */
		firmware_ctx.add_to_queue (&firmware_ctx, user_parent);
		user_parent = (WORDPTR) NOT_PROCESS_P;
		/* Disconnect any top-level channels. */
		tvm_ectx_disconnect (&user_ctx);
	}

	return SFFI_OK;
}

/* PROC firmware.query.user (BOOL running, INT state, []BYTE context) */
static int firmware_query_user (ECTX ectx, WORD args[])
{
	WORDPTR running = (WORDPTR) args[0];
	WORDPTR state	= (WORDPTR) args[1];
	BYTEPTR	ctx	= (BYTEPTR) args[2];
	WORD	ctx_len	= args[3];
	BYTE 	*uctx	= (BYTE *) &user_ctx;
	int i;

	/* BOOL/WORD running */
	write_word (running, user_parent != (WORDPTR) NOT_PROCESS_P ? 1 : 0);
	/* WORD state */
	write_word (state, (WORD) user_ctx.state);
	/* []BYTE context */
	for (i = 0; i < ctx_len && i < sizeof(user_ctx); ++i)
	{
		write_byte (ctx, *(uctx++));
		ctx = byteptr_plus (ctx, 1);
	}

	return SFFI_OK;
}

/* PROC reset.dynamic.memory () */
static int reset_dynamic_memory (ECTX ectx, WORD args[])
{
	#ifdef TVM_USE_TLSF
	const UWORD dmem_length = SDRAM_TOP - DMEM_START;

	tlsf_init_memory_pool (dmem_length, ectx->mem_pool);

	return SFFI_OK;
	#else
	return ectx->set_error_flag (ectx, EFLAG_FFI);
	#endif
}

/* PROC safe.set.register.16 (VAL INT addr, set, mask) */
static int set_register_16 (ECTX ectx, WORD args[])
{
	volatile unsigned short *addr	= (unsigned short *) args[0];
	unsigned short set		= (unsigned short) args[1];
	unsigned short mask		= (unsigned short) args[2];
	unsigned short imask;

	DISABLE_INTERRUPTS (imask);

	*addr = ((*addr) & mask) | set;

	ENABLE_INTERRUPTS (imask);

	return SFFI_OK;
}

/* PROC test.disconnected (CHAN ANY c, BOOL b) */
static int test_disconnected (ECTX ectx, WORD args[])
{
	WORDPTR	chan_ptr	= (WORDPTR) args[0];
	WORDPTR	out		= (WORDPTR) args[1];

	write_word (out, (read_word (chan_ptr) == (NOT_PROCESS_P | 1)) ? 1 : 0);

	return SFFI_OK;
}
/*}}}*/

/*{{{  SFFI tables */
static SFFI_FUNCTION	firmware_sffi_table[] = {
	firmware_run_user,
	firmware_kill_user,
	firmware_query_user,
	reset_dynamic_memory,
	set_register_16,
	jpeg_encode_frame,
	draw_caption_on_frame,
	test_disconnected
};
static const int	firmware_sffi_table_length =
				sizeof(firmware_sffi_table) / sizeof(SFFI_FUNCTION);

static SFFI_FUNCTION	user_sffi_table[] = {
	NULL,
	NULL,
	NULL,
	NULL,
	set_register_16,
	jpeg_encode_frame,
	draw_caption_on_frame,
	test_disconnected
};
static const int	user_sffi_table_length =
				sizeof(user_sffi_table) / sizeof(SFFI_FUNCTION);
/*}}}*/

/*{{{  Firmware context */
#include "firmware.h"

static WORD firmware_memory[272];

static void init_firmware_memory (void)
{
	WORD *ptr = firmware_memory;
	int words = (sizeof(firmware_memory) / sizeof(WORD));
	
	while ((words--) > 0) {
		*(ptr++) = MIN_INT;
	}
}

static void install_firmware_ctx (void)
{
	WORDPTR ws, vs, ms;
	ECTX firmware = &firmware_ctx;

	/* Initialise firmware execution context */
	tvm_ectx_init (&tvm, firmware);
	firmware->get_time 		= srv_get_time;
	firmware->modify_sync_flags	= srv_modify_sync_flags;
	firmware->ext_chan_table	= ext_chans;
	firmware->ext_chan_table_length	= ext_chans_length;
	firmware->sffi_table		= firmware_sffi_table;
	firmware->sffi_table_length	= firmware_sffi_table_length;
	/* Dynamic memory */
	#ifdef TVM_USE_TLSF
	firmware->mem_pool		= (void *) DMEM_START;
	#endif
	
	/* Setup memory and initial workspace */
	init_firmware_memory ();
	tvm_ectx_layout (
		firmware, firmware_memory,
		"", 0, ws_size, vs_size, ms_size, 
		&ws, &vs, &ms
	);
	tvm_ectx_install_tlp (
		firmware, (BYTEPTR) transputercode, ws, vs, ms, 
		"", 0, NULL
	);
}

static int run_firmware (void)
{
	int ret;

	do {
		ret = tvm_run (&firmware_ctx);

		if (ret == ECTX_SLEEP) {
			return ret; /* OK - timer sleep */
		} else if (ret == ECTX_EMPTY) {
			if (uart0_channel != (WORDPTR) NOT_PROCESS_P) {
				return ret; /* OK - waiting for input */
			} else if (camera_channel != (WORDPTR) NOT_PROCESS_P) {
				return ret; /* OK - waiting for imagery */
			} else if (user_parent != (WORDPTR) NOT_PROCESS_P) {
				if (user_ctx.state == ECTX_EMPTY && user_ctx.fptr == (WORDPTR) NOT_PROCESS_P) {
					if (tvm_ectx_waiting_on (&user_ctx, user_memory, user_memory_len)) {
						/* User code is waiting on us so we are probably
						 * in the wrong; bail...
						 */
					} else {
						/* User code is not waiting on us, so spin and
						 * let it get deadlock detected, if killing it
						 * doesn't release us then we we'll be back
						 * here...
						 */
						return ret;
					}
				} else {
					/* Optimise for the common case by ignoring 
					 * the possibility of deadlock when the
					 * user code can still keep running.
					 */
					return ret;
				}
			}
			/* Fall through indicates deadlock */
		} else if (ret == ECTX_INTERRUPT) {
			clear_pending_interrupts ();
			/* OK; fall through and loop */
		}
	} while (ret == ECTX_INTERRUPT);

	/* Being here means something unexpected happened... */
	
	uart0_send_string ((unsigned char *) "## Firmware failed; state = ");
	uart0_send_char ((unsigned char) ret);
	uart0_send_char ('\n');

	if (user_parent != (WORDPTR) NOT_PROCESS_P) {
		uart0_send_string ((unsigned char *) "## User state = ");
		uart0_send_char ((unsigned char) user_ctx.state);
		uart0_send_char ('\n');
	}

	/* Go into an idle loop */
	for (;;) {
		IDLE;
		SSYNC;
	}

	return ret;
}
/*}}}*/

/*{{{  User context */
static void install_user_ctx (void)
{
	ECTX user = &user_ctx;

	tvm_ectx_init (&tvm, user);
	user->get_time 			= srv_get_time;
	user->modify_sync_flags		= srv_modify_sync_flags;
	user->sffi_table		= user_sffi_table;
	user->sffi_table_length		= user_sffi_table_length;
	
	/* Dynamic memory */
	#ifdef TVM_USE_TLSF
	user->mem_pool			= (void *) DMEM_START;
	#endif
}

static int run_user (void)
{
	int ret = tvm_run_count (&user_ctx, 1000);

	switch (ret) {
		case ECTX_INTERRUPT:
			clear_pending_interrupts ();
			/* fall through */
		case ECTX_PREEMPT:
		case ECTX_SLEEP:
		case ECTX_TIME_SLICE:
			return ret; /* OK */
		case ECTX_EMPTY:
			if (tvm_ectx_waiting_on (&user_ctx, user_memory, user_memory_len)) {
				return ret; /* OK - waiting for firmware */
			}
			break;
		default:
			break;
	}

	/* User context broke down for some reason. */
	/* Restore parent in firmware */
	firmware_ctx.add_to_queue (&firmware_ctx, user_parent);
	user_parent = (WORDPTR) NOT_PROCESS_P;

	/* Disconnect any top-level channels. */
	tvm_ectx_disconnect (&user_ctx);

	return ECTX_ERROR;
}
/*}}}*/

int main (void) {
	/* Initialise Hardware */
	init_io ();
	init_timers ();
	init_uart ();
	init_camera ();

	/* Wait for 7 seconds for MatchPort to boot */
	delay_us (7000000);

	/* Output boot message */
	uart0_send_string (version_string);
	uart0_send_char ('\n');
	
	/* Initialise interpreter */
	tvm_init (&tvm);
	install_firmware_ctx ();
	install_user_ctx ();

	for (;;) {
		int f_ret = run_firmware ();
		int u_ret = ECTX_EMPTY;

		if (user_parent != (WORDPTR) NOT_PROCESS_P) {
			u_ret = run_user ();
		}

		if ((f_ret == ECTX_EMPTY || f_ret == ECTX_SLEEP) && 
			(u_ret == ECTX_EMPTY || u_ret == ECTX_SLEEP)) {
			/* FIXME: power management goes here */
		}
	}
}
