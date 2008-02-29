/*
 * tvm.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

static tvm_t 		tvm;
static tvm_ectx_t 	firmware_ctx, user_ctx;

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

	complete_camera_interrupt (&firmware_ctx);
	complete_uart0_interrupt (&firmware_ctx);
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

/* PROC safe.set.register.16 (INT16 reg, VAL INT set, clear) */
static int set_register_16 (ECTX ectx, WORD args[])
{
	volatile unsigned short *addr	= (unsigned short *) args[0];
	unsigned short set		= (unsigned short) args[1];
	unsigned short clear		= (unsigned short) args[2];
	unsigned short imask;

	DISABLE_INTERRUPTS (imask);

	*addr = ((*addr) & (~clear)) | set;

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
	
	uart0_send_string ("## Firmware failed; state = ");
	uart0_send_char (ret);
	uart0_send_char ('\n');

	if (user_parent != (WORDPTR) NOT_PROCESS_P) {
		uart0_send_string ("## User state = ");
		uart0_send_char (user_ctx.state);
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

static void tvm_sleep (void)
{
	ECTX firmware	= &firmware_ctx;
	ECTX user	= &user_ctx;
	WORD is_timed	= 1;
	WORD timeout;

	if (firmware->state == ECTX_SLEEP && user->state == ECTX_SLEEP) {
		if (TIME_AFTER (user->tnext, firmware->tnext)) {
			timeout	= firmware->tnext;
		} else {
			timeout	= user->tnext;
		}
	} else if (firmware->state == ECTX_SLEEP) {
		timeout	= firmware->tnext;
	} else if (firmware->state == ECTX_SLEEP) {
		timeout	= user->tnext;
	} else {
		is_timed = timeout = 0;
	}

	if (is_timed) {
		sleep_until (timeout);
	} else {
		sleep ();
	}
}
/*}}}*/

/*{{{  Interfacing */
int tvm_interrupt_pending (void)
{
	return (firmware_ctx.sflags | user_ctx.sflags);
}

void raise_tvm_interrupt (void)
{
	firmware_ctx.sflags	|= SFLAG_INTR;
	user_ctx.sflags		|= SFLAG_INTR;
}

int run_tvm (void)
{
	/* Initialise interpreter */
	tvm_init (&tvm);
	install_firmware_ctx ();
	install_user_ctx ();

	/* Run interpreter */
	for (;;) {
		int f_ret = run_firmware ();
		int u_ret = ECTX_EMPTY;

		if (user_parent != (WORDPTR) NOT_PROCESS_P) {
			u_ret = run_user ();
		}

		if ((f_ret == ECTX_EMPTY || f_ret == ECTX_SLEEP) && 
			(u_ret == ECTX_EMPTY || u_ret == ECTX_SLEEP)) {
			if (firmware_ctx.fptr == NOT_PROCESS_P && user_ctx.fptr == NOT_PROCESS_P) {
				tvm_sleep ();
			}
		}
	}

	return 1;
}
/*}}}*/

