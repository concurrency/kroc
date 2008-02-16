/*
 * main.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2007-2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

/* TVM */
#include <tvm.h>
/* Blackfin */
#include <cdefBF537.h>
/* Configuration */
#include "bfin_config.h"
#include "memory_map.h"
/* Support code */
#include <camera.h>
#include <font8x8.h>
#include <i2cwrite.h>
#include <jpeg.h>
#include <ov9655.h>

#define CSYNC	__asm__ __volatile__ ("csync;" : : : "memory")
#define SSYNC	__asm__ __volatile__ ("ssync;" : : : "memory")
#define IDLE	__asm__ __volatile__ ("idle;")

static char version_string[] = "TVM SRV-1 Blackfin - " __TIME__ " - " __DATE__;

/*{{{  TVM state */
static tvm_t 		tvm;
static tvm_ectx_t 	firmware_ctx, user_ctx;
/*}}}*/

/*{{{  SDRAM support */
static void clear_sdram (void)
{
	const short *end	= (short *) SDRAM_TOP;
	short *cp		= (short *) SDRAM_BOTTOM;

	while (cp < end) {
		*(cp++) = 0;
	}
}
/*}}}*/

/*{{{  Timer functionality */
volatile unsigned long core_timer_wrap_count;

static void init_timers (void)
{
	core_timer_wrap_count = 0;
	*pTSCALE = ((CORE_CLOCK / 2000000) - 1);
	*pTPERIOD = 0xffffffff;
	*pTCOUNT = 0xffffffff;
	SSYNC;
	*pTCNTL = 0x7;
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
static volatile int		uart0_pending	= 0;

static void init_uart (void)
{
	unsigned char temp;

	/* Configure port H pin 6 (PH6) for flow control CTS */
	*pPORTHIO_DIR	|= 0x0040;
	/* Raise PH6 to block input */
	*pPORTHIO	= 0x0040;
	/* Configure PH0 for flow control RTS */
	*pPORTHIO_INEN	= 0x0001;
	
	/* Enable UART pins on port F */
	*pPORTF_FER |= 0x000f;

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
		firmware_ctx.sflags	|= SFLAG_INTR;
		user_ctx.sflags		|= SFLAG_INTR;
	} else {
		/* No; buffer the character */
		uart0_buffer = buffer;
		uart0_pending++;
	}
}

static void acknowledge_int10 (void)
{
	firmware_ctx.sflags	&= ~(SFLAG_INTR);
	user_ctx.sflags		&= ~(SFLAG_INTR);
	firmware_ctx.add_to_queue (&firmware_ctx, uart0_channel);
	uart0_channel		= (WORDPTR) NOT_PROCESS_P;
}

static int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	unsigned short imask;
	int reschedule;
	
	/* Disable interrupts */
	__asm__ __volatile__ ("cli %0;" : "=r" (imask));

	if (uart0_pending) {
		write_byte (pointer, (BYTE) uart0_buffer);
		uart0_pending	= 0;
		reschedule	= 0;
	} else {
		uart0_channel	= ectx->wptr;
		uart0_ptr	= pointer;
		reschedule	= 1;
	}

	/* Enable (restore) interrupts */
	__asm__ __volatile__ ("sti %0;" : : "r" (imask));

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
	unsigned char data = (unsigned char) read_byte (pointer);

	uart0_send_char (data);

	return ECTX_CONTINUE;
}

/* PROC blit.to.uart0 (VAL []BYTE data) */
static int blit_to_uart0 (ECTX ectx, WORD args[])
{
	BYTEPTR src	= (BYTEPTR) args[0];
	WORD	src_len	= args[1];

	while (src_len--) {
		uart0_send_char (read_byte (src));
		src = byteptr_plus (src, 1);
	}

	return SFFI_OK;
}
/*}}}*/

/*{{{  Camera support */
static int camera_frame_words	= 0;
static int camera_initialised	= 0;
static int camera_running	= 0;

/* Move these constants to a shared header */
enum {
	CAMERA_INIT		= -1,
	CAMERA_STOP		= 0,
	CAMERA_160_128		= 1,
	CAMERA_320_256		= 2,
	CAMERA_640_512		= 3,
	CAMERA_1280_1024	= 4
};

/* PROC set.camera.mode (VAL INT mode) */
static int set_camera_mode (ECTX ectx, WORD args[])
{
	unsigned int width, height, cfg_length;
	unsigned char *cfg	= NULL;
	WORD mode		= args[0];

	if (camera_running) {
		camera_stop ();
		camera_running = 0;
	}

	switch (mode) {
		case CAMERA_INIT:
		case CAMERA_160_128: 
			width		= 160;
			height 		= 128; 
			cfg		= ov9655_qqvga; 
			cfg_length	= sizeof(ov9655_qqvga);
			break;
		case CAMERA_320_256:
			width		= 320;
			height 		= 256; 
			cfg		= ov9655_qvga; 
			cfg_length	= sizeof(ov9655_qvga);
			break;
		case CAMERA_640_512:
			width		= 640;
			height 		= 512; 
			cfg		= ov9655_vga; 
			cfg_length	= sizeof(ov9655_vga);
			break;
		case CAMERA_1280_1024:
			width		= 1280;
			height 		= 1024;
			cfg		= ov9655_sxga; 
			cfg_length	= sizeof(ov9655_sxga);
			break;
	}

	if ((cfg != NULL && !camera_initialised) || mode == CAMERA_INIT) {
		i2cwrite (0x30, ov9655_setup, sizeof(ov9655_setup) >> 1);
		delay_us (500000);
		i2cwrite (0x30, ov9655_setup, sizeof(ov9655_setup) >> 1);
		camera_initialised++;
	}

	if (cfg != NULL) {
		i2cwrite (0x30, cfg, cfg_length >> 1);
		
		camera_init (
			(unsigned char *) DMA_BUF1,
			(unsigned char *) DMA_BUF2,
			width, height
		);
		camera_start();

		camera_frame_words = (width * height) >> 1;
		camera_running = 1;
	}
				
	return SFFI_OK;
}

/* PROC get.camera.frame ([]BYTE frame) */
static int get_camera_frame (ECTX ectx, WORD args[])
{
	WORDPTR	dst		= (WORDPTR) args[0];
	WORD	dst_len		= args[1] >> 2;
	WORD	*src		= NULL;
	WORD	len		= dst_len;
	WORD	i;

	/* Make sure we don't copy more data than there is */
	if (len > camera_frame_words) {
		len = camera_frame_words;
	}

	/* Select buffer the hardware isn't writing to */
	if (((unsigned long) *pDMA0_CURR_ADDR) < ((unsigned long) DMA_BUF2)) {
		src = (WORD *) DMA_BUF2;
	} else {
		src = (WORD *) DMA_BUF1;
	}

	/* Copy data (as WORDs to improve throughput) */
	for (i = 0; i < len; i++) {
		write_word (dst, *(src++));
		dst = wordptr_plus (dst, 1);
	}

	if (dst_len > len) {
		/* Blank any excess buffer */
		for (i = len; i < dst_len; i++) {
			write_word (dst, (WORD) 0);
			dst = wordptr_plus (dst, 1);
		}
	}

	return SFFI_OK;
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
	/* Output buffer must be at least as big as input buffer */
	if ((((width * height) << 1) > input_len) || (output_len < input_len)) {
		/* Bad buffer sizes, return -1 */
		write_word (used, -1);
	} else {
		input	= (BYTEPTR) wordptr_real_address ((WORDPTR) input);
		output 	= (BYTEPTR) wordptr_real_address ((WORDPTR) output);

		end = (BYTEPTR) encode_image (
			(unsigned char *) input,
			(unsigned char *) output,
			quality,
			FOUR_TWO_TWO,
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
	WORD	frame_len	= args[4];
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
	
	/* Disable interrupts */
	__asm__ __volatile__ ("cli %0;" : "=r" (imask));

	ectx->sflags = (ectx->sflags & (~clear)) | set;

	/* Enable (restore) interrupts */
	__asm__ __volatile__ ("sti %0;" : : "r" (imask));
}
/*}}}*/ 

/*{{{  External channel definitions */
static EXT_CHAN_ENTRY	ext_chans[] = {
	{ 
		.typehash 	= 0,
		.in 		= uart0_in, 
		.out 		= uart0_out
	}
};
static const int	ext_chans_length =
				sizeof(ext_chans) / sizeof(EXT_CHAN_ENTRY);
/*}}}*/

/*{{{  User context state */
static BYTEPTR		user_bytecode;
static WORD		user_bytecode_len;
static const WORDPTR 	user_memory	= (WORDPTR) USER_MEMORY;
static WORD		user_memory_len	= 0;
static WORDPTR 		user_parent	= (WORDPTR) NOT_PROCESS_P;
/*}}}*/

/*{{{  Firmware functions for running user bytecode */
/* PROC firmware.run.user (VAL []BYTE bytecode, VAL INT ws, vs, ms, 
 * 				VAL []BYTE tlp.fmt, VAL INT argc, ...) */
static int firmware_run_user (ECTX ectx, WORD args[])
{
	BYTEPTR bytecode	= (BYTEPTR) args[0];
	WORD	bytecode_len	= args[1];
	WORD	ws_size		= args[2];
	WORD	vs_size		= args[3];
	WORD	ms_size		= args[4];
	BYTEPTR	tlp_fmt		= (BYTEPTR) args[5];
	WORD	tlp_fmt_len	= args[6];
	WORD	argc		= args[7];
	WORDPTR	argv		= (WORD *) &(args[8]);
	WORDPTR	ws, vs, ms;
	WORD	dmem_length, ret_addr;
	int ret;

	if (user_parent != (WORDPTR) NOT_PROCESS_P)
	{
		/* User context is already running */
		return ectx->set_error_flag (ectx, EFLAG_FFI);
	}

	tvm_ectx_reset (&user_ctx);
	user_memory_len = tvm_ectx_memory_size (
		&user_ctx, tlp_fmt, argc, ws_size, vs_size, ms_size
	);
	tvm_ectx_layout (
		&user_ctx, user_memory, 
		tlp_fmt, argc, ws_size, vs_size, ms_size,
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

	#ifdef TVM_USE_TLSF
	user_ctx.mem_pool	= ((BYTE *) user_memory) + (user_memory_len << WSH);
	firmware_ctx.mem_pool	= user_ctx.mem_pool;
	dmem_length		= SDRAM_TOP - (UWORD) user_ctx.mem_pool;
	tlsf_init_memory_pool (dmem_length, user_ctx.mem_pool);
	#endif

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

/* PROC safe.set.register.16 (VAL INT addr, set, mask) */
static int set_register_16 (ECTX ectx, WORD args[])
{
	volatile unsigned short *addr	= (unsigned short *) args[0];
	unsigned short set		= (unsigned short) args[1];
	unsigned short mask		= (unsigned short) args[2];
	unsigned short imask;

	/* Disable interrupts */
	__asm__ __volatile__ ("cli %0;" : "=r" (imask) : : "memory");

	*addr = ((*addr) & mask) | set;

	/* Enable (restore) interrupts */
	__asm__ __volatile__ ("sti %0;" : : "r" (imask) : "memory");

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
	set_register_16,
	set_camera_mode,
	get_camera_frame,
	jpeg_encode_frame,
	draw_caption_on_frame,
	blit_to_uart0,
	test_disconnected
};
static const int	firmware_sffi_table_length =
				sizeof(firmware_sffi_table) / sizeof(SFFI_FUNCTION);

static SFFI_FUNCTION	user_sffi_table[] = {
	NULL,
	NULL,
	NULL,
	NULL,
	set_camera_mode,
	get_camera_frame,
	jpeg_encode_frame,
	draw_caption_on_frame,
	blit_to_uart0
};
static const int	user_sffi_table_length =
				sizeof(user_sffi_table) / sizeof(SFFI_FUNCTION);
/*}}}*/

/*{{{  Firmware context */
#include "firmware.h"

static WORD firmware_memory[256];

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
	int ret = tvm_run (&firmware_ctx);

	if (ret == ECTX_SLEEP) {
		return ret; /* OK - timer sleep */
	} else if (ret == ECTX_EMPTY) {
		if (uart0_channel != (WORDPTR) NOT_PROCESS_P) {
			return ret; /* OK - waiting for input */
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
		acknowledge_int10 ();
		return ret; /* OK - interrupt */
	}

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
}

static int run_user (void)
{
	int ret = tvm_run_count (&user_ctx, 1000);

	switch (ret) {
		case ECTX_INTERRUPT:
			acknowledge_int10 ();
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
	clear_sdram ();
	init_uart ();
	init_timers ();

	uart0_send_string ((unsigned char *) version_string);
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
