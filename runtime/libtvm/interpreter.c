/*
tvm - interpreter.c
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "tvm.h"
#include "instructions.h"
#include "interpreter.h"

#ifdef TVM_DISPATCH_SWITCH
#include "mem.c"
#include "ins_alt.c"
#include "ins_barrier.c"
#include "ins_chan.c"
#include "ins_float.c"
#include "ins_pi.c"
#include "ins_mobile.c"
#include "ins_pri.c"
#include "ins_proc.c"
#include "ins_rmox.c"
#include "ins_sec.c"
#include "ins_timer.c"
#include "ins_t800.c"
#include "ins_t9000.c"
#include "instructions.c"
#include "dispatch_ins.c"
#else
#include "ins_mobile.h"
#include "jumptbl_pri.h"
#include "scheduler.h"
#endif

/* #define DEBUG_INTERPRETER 1 */

#if defined(DEBUG_INTERPRETER)
#include <stdio.h>
#include "ins_names.h"
#endif

#define WS_PAD	4
#define VS_PAD	1

int tvm_init (tvm_t *tvm)
{
	tvm->head 		= NULL;
	tvm->tail		= NULL;
	return 0;
}

void tvm_ectx_init (tvm_t *tvm, ECTX ectx)
{
	tvm_ectx_reset (ectx);
	
	ectx->pri 		= 0;

	#ifdef TVM_CUSTOM_COPY_DATA
	ectx->copy_data		= NULL;
	#endif
	ectx->get_time		= NULL;
	ectx->set_alarm		= NULL;
	ectx->run_hook		= NULL;

	ectx->ffi_table		= NULL;
	ectx->ffi_table_length	= 0;
	ectx->sffi_table	= NULL;
	ectx->sffi_table_length = 0;

	#ifdef TVM_OCCAM_PI
	ectx->ext_chan_table	= NULL;
	ectx->ext_chan_table_length = 0;
	#endif

	ectx->tvm		= tvm;

	if (tvm->head == NULL) {
		tvm->head = ectx;
	} else {
		tvm->tail->next = ectx;
	}
	
	tvm->tail		= ectx;
	ectx->next		= NULL;

	_tvm_install_scheduler (ectx);

	#ifdef TVM_PROFILING
	{
		int i;
		for (i = 0; i < sizeof(ectx->profile.pri) / sizeof(UWORD); ++i)
			ectx->profile.pri[i] = 0;
		for (i = 0; i < sizeof(ectx->profile.sec) / sizeof(UWORD); ++i)
			ectx->profile.sec[i] = 0;
	}
	#endif
}

void tvm_ectx_release (ECTX ectx)
{
	tvm_t *tvm = ectx->tvm;
	
	if (ectx == tvm->head) {
		tvm->head = ectx->next;
	} else {
		ECTX p = tvm->head;
		while (p->next != ectx)
			p = p->next;
		p->next = ectx->next;
	}

	ectx->tvm	= NULL;
	ectx->next	= NULL;
}

void tvm_ectx_reset (ECTX ectx)
{
	ectx->state 	= ECTX_INIT;
	ectx->eflags	= 0;
	ectx->sflags	= 0;

	/* evaluation stack */
	OREG 		= 0;
	AREG 		= 0;
	BREG 		= 0;
	CREG 		= 0;
	SET_AREGt (STYPE_UNDEF);
	SET_BREGt (STYPE_UNDEF);
	SET_CREGt (STYPE_UNDEF);

	/* setup scheduler queues */
	WPTR = (WORDPTR) NOT_PROCESS_P;
	FPTR = (WORDPTR) NOT_PROCESS_P;
	BPTR = (WORDPTR) NOT_PROCESS_P;
	TPTR = (WORDPTR) NOT_PROCESS_P;

	/* type shadow */
	#ifdef TVM_TYPE_SHADOW
	ectx->shadow_start	= 0;
	ectx->shadow_end	= 0;
	ectx->type_store	= NULL;
	#endif
}

static int calc_frame_size (const int tlp_argc, const int vs, const int pad)
{
	int frame_size = 1 + tlp_argc;
	
	frame_size += (vs ? 1 : 0);

	/* Stack frame has a minimum size of 4 */
	if (pad && (frame_size < 4)) {
		frame_size = 4;
	}

	return frame_size;
}

WORD tvm_ectx_memory_size (ECTX ectx,
		const char *tlp_fmt, const int tlp_argc,
		WORD ws_size, WORD vs_size)
{
	int frame_size = calc_frame_size (tlp_argc, vs_size, 1);

	/* The plus 1 in here is for the shutdown bytecode */
	return frame_size + 1 + ws_size + vs_size + WS_PAD + VS_PAD;
}

int tvm_ectx_layout (ECTX ectx, WORDPTR base,
		const char *tlp_fmt, const int tlp_argc,
		WORD ws_size, WORD vs_size,
		WORDPTR *ws, WORDPTR *vs)
{
	int frame_size = calc_frame_size (tlp_argc, vs_size, 1);

	/* The plus 1 in here is for the shutdown bytecode */
	*ws = wordptr_plus (base, frame_size + ws_size + 1 + WS_PAD);
	*vs = vs_size ? *ws : 0;

	return wordptr_minus (*ws, base) + vs_size;
}

/*
 * Setup the initial workspace and instruction pointers,
 * and stack frame for an execution context.
 */
int tvm_ectx_install_tlp (ECTX ectx, BYTEPTR code,
		WORDPTR ws, WORDPTR vs,
		const char *fmt, int argc, const WORD argv[])
{
	WORDPTR fb = 0;
	int i, frame_size;

	/* Make sure we don't have too many arguments */
	if (argc > TVM_ECTX_TLP_ARGS) {
		return -1;
	}

	/* Copy arguments to execution context */
	ectx->tlp_argc = argc;
	for (i = 0; i < argc; ++i) {
		char arg = ectx->tlp_fmt[i] = fmt[i];

		if (arg == 'F') {
			#ifdef TVM_DYNAMIC_OCCAM_PI
			int ret;
			/* Allocate forking barrier */
			if ((ret = mt_alloc (ectx, MT_MAKE_BARRIER(MT_BARRIER_FORKING), 0, &fb))) {
				return ret;
			}
			ectx->tlp_argv[i] = (WORD) fb;
			#else
			return -2;
			#endif
		} else {
			ectx->tlp_argv[i] = argv[i];
		}
	}

	/* Setup initial workspace pointer */
	WPTR = ws;
	/* Setup initial instruction pointer */
	IPTR = code;

	/* Calculate the framesize, and layout data
	 * in initial stack frame.
	 *
	 * Memory ends up layed out as follows:
	 * 
	 * WS =>        [ shutdown bytecode       ]
	 *              [ ... padding ...         ]
	 *              [ forking barrier pointer ]
	 *              [ mobile space pointer    ]
	 *              [ vector space pointer    ]
	 *      +argc:  [ argv[argc - 1]          ]
	 *              [ argv[...]               ]
	 *         +1:  [ argv[0]                 ]
	 * WPTR =>  0:  [ return address          ]
	 *
	 * Where the forking barrier, mobile space
	 * and vector space pointers are optional.
	 *
	 * The total size of the frame must be at
	 * least 4 words, e.g. if argc == 0 and
	 * there are no pointers then, 3 words of
	 * padding will be added to the top of the
	 * frame.
	 *
	 * The return address points at the shutdown
	 * bytecode, allowing the interpreter to
	 * detect normal completion of an execution
	 * context using a instruction sequence.
	 *
	 * The shutdown bytecode lives above the
	 * stack frame, as the compiler is allowed
	 * to use the slots in the frame for volatile
	 * storage.
	 */
	
	/* Put a shutdown instruction in top of the stack frame */
	WPTR = wordptr_minus (WPTR, 1);
	write_byte (byteptr_plus ((BYTEPTR) WPTR, 0), 0x2F);
	write_byte (byteptr_plus ((BYTEPTR) WPTR, 1), 0xFE);
	write_type (ectx, WPTR, STYPE_DATA);

	/* Pad stack frame */
	frame_size = calc_frame_size (argc, (WORD) vs, 0);
	if (frame_size < 4) {
		WPTR = wordptr_minus (WPTR, 4 - frame_size);
		frame_size = 4;
	}
	
	/* Set up forking barrier pointer */
	if (fb) {
		WPTR = wordptr_minus (WPTR, 1);
		write_word (WPTR, (WORD) fb);
		write_type (ectx, WPTR, STYPE_MT);
	}

	/* Set up vectorspace pointer */
	if (vs) {
		WPTR = wordptr_minus (WPTR, 1);
		write_word (WPTR, (WORD) vs);
		write_type (ectx, WPTR, STYPE_VS);
	}
	
	/* Set up arguments */
	for (i = ectx->tlp_argc - 1; i >= 0; i--) {
		if (ectx->tlp_fmt[i] != 'F') {
			WPTR = wordptr_minus (WPTR, 1);
			write_word (WPTR, ectx->tlp_argv[i]);
		}
		#ifdef TVM_TYPE_SHADOW
		switch (ectx->tlp_fmt[i]) {
			case '?': case '!':
				write_type (ectx, WPTR, STYPE_MOBILE);
				break;
			case 'C': case 'S': case 'F': case 'M':
				write_type (ectx, WPTR, STYPE_MT);
				break;
			default:
				write_type (ectx, WPTR, STYPE_DATA);
				break;
		}
		#endif
	}

	/* Store the return pointer, to completion byte code */
	/* FIXME: this won't work for virtual memory right? */
	WPTR = wordptr_minus (WPTR, 1);
	write_word (WPTR, (WORD) wordptr_plus (WPTR, frame_size));
	write_type (ectx, WPTR, STYPE_WS);

	return 0;
}

static void disconnect_channel (WORDPTR ptr)
{
	WORD chan_value = read_word (ptr);

	if ((chan_value & (~1)) != NOT_PROCESS_P) {
		WORDPTR ws	= (WORDPTR) (chan_value & (~1));
		ECTX	ectx	= (ECTX) WORKSPACE_GET (ws, WS_ECTX);
		if(chan_value & 1) {
			WORD alt_state = WORKSPACE_GET (ws, WS_STATE);

			switch (alt_state) {
				case WAITING_P:
					ectx->add_to_queue_external (ectx, NULL, ws);
					/* Disregard return value */
					/* Fall through */
				case ENABLING_P:
					WORKSPACE_SET (ws, WS_STATE, DISABLING_P);
					/* Fall through */
				case DISABLING_P:
					break;
				default:
					break; /* Error state... */
			}
		} else {
			BYTEPTR	data_ptr = (BYTEPTR) WORKSPACE_GET (ws, WS_POINTER);
			WORD	data_len = WORKSPACE_GET (ws, WS_PENDING);

			if (data_len > 0) {
				/* input */
				while (data_len--) {
					write_byte (data_ptr, (BYTE) 0);
					data_ptr = byteptr_plus (data_ptr, 1);
				}
			}
			#ifdef TVM_DYNAMIC_OCCAM_PI
			else if (data_len == MIN_INT) {
				/* mobile input */
				write_word ((WORDPTR) data_ptr, (WORD) NULL_P);
			}
			else if (data_len == (MIN_INT + 1)) {
				/* mobile output */
				WORDPTR src = (WORDPTR) data_ptr;
				WORDPTR ptr = (WORDPTR) read_word (data_ptr);

				if (ptr != (WORDPTR) NULL_P) {
					UWORD move = MT_FALSE;

					mt_io_update (ectx, &ptr, &move);
					/* ignore return; potentially bad */

					if(move == MT_TRUE) {
						/* Pointer moved, delete old reference */
						write_word (src, (WORD) NULL_P);
					}

					mt_release (ectx, ptr);
					/* ignore return; potentially bad */
				}
			}
			#endif /* TVM_DYNAMIC_OCCAM_PI */

			ectx->add_to_queue_external (ectx, NULL, ws);
			/* disregard return value */
		}
	}
	
	write_word(ptr, (NOT_PROCESS_P | 1));
}

void tvm_ectx_disconnect (ECTX ectx)
{
	int i;

	for(i = 0; i < ectx->tlp_argc; i++) {
		switch (ectx->tlp_fmt[i]) {
			case '?':
			case '!':
				disconnect_channel ((WORDPTR) ectx->tlp_argv[i]);
				break;
			#ifdef TVM_DYNAMIC_OCCAM_PI
			case 'C':
			case 'S':
				if (ectx->tlp_argv[i] != NULL_P) {
					WORDPTR ptr;
					UWORD channels, type;
					int j;

					ptr		= (WORDPTR) ectx->tlp_argv[i];
					type		= read_mt_type (ptr);
					channels	= MT_CB_CHANNELS(type);

					for (j = 0; j < channels; ++j) {
						disconnect_channel (wordptr_offset (ptr, mt_cb_t, channels[j]));
					}
				}
				break;
			#endif /* TVM_DYNAMIC_OCCAM_PI */
			default:
				break;
		}
	}
}

/* Look for dependency conditions in top-level channels. */
int tvm_ectx_waiting_on (ECTX ectx, WORDPTR ws_base, WORD ws_len)
{
	WORDPTR ws_end = wordptr_plus (ws_base, ws_len);
	WORDPTR ptr;
	int i;

	if (ws_base > ws_end) {
		WORDPTR tmp	= ws_base;
		ws_base		= ws_end;
		ws_end		= tmp;
	}

	for (i = 0; i < ectx->tlp_argc; ++i) {
		switch (ectx->tlp_fmt[i]) {
			case '?': 
			case '!':
				ptr = (WORDPTR) ectx->tlp_argv[i];
				ptr = (WORDPTR) read_word (ptr);
				if (ptr >= ws_base && ptr <= ws_end)
					return 1; /* dependency */
				break;
			#ifdef TVM_DYNAMIC_OCCAM_PI
			case 'C':
			case 'S':
				if (ectx->tlp_argv[i] != NULL_P) {
					UWORD channels, type;
					int j;

					ptr		= (WORDPTR) ectx->tlp_argv[i];
					type		= read_mt_type (ptr);
					channels	= MT_CB_CHANNELS(type);

					for (j = 0; j < channels; ++j) {
						WORDPTR val = (WORDPTR) read_offset (ptr, mt_cb_t, channels[j]);
						if (val >= ws_base && val <= ws_end)
							return 1; /* dependency */
					}
				}
				break;
			#endif /* TVM_DYNAMIC_OCCAM_PI */
			default:
				break;
		}
	}

	return 0; /* no dependencies */
}

static TVM_INLINE BYTE decode_next (ECTX ectx)
{
	BYTE instr;
	
	/* Read the instruction */
	instr = read_byte (IPTR);
	
	#if defined(DEBUG_INTERPRETER)
	printf ("%p %02x ", IPTR, instr);
	if ((instr & 0xF0) != 0xF0) {
		printf ("%8s", pri_name[(instr >> 4) & 0xF]);
	} else {
		printf ("%8s", sec_name[OREG | (instr & 0xF)]);
	}
	printf (" %08x %08x %08x %08x\n", 
		OREG | (instr & 0xF), AREG, BREG, CREG);
	#endif /* DEBUG_INTERPRETER */

	/* Increment instruction pointer */
	IPTR = byteptr_plus (IPTR, 1);
	
	/* Put the least significant bits in OREG */
	OREG |= (instr & 0x0f);

	return instr;
}

static int do_dispatch (ECTX ectx, BYTE instr)
{
	#ifdef TVM_DISPATCH_SWITCH
	return dispatch_instruction (ectx, instr);
	#else
	/* Use the other bits to index into the jump table */
	return primaries[instr >> 4] (ectx);
	#endif
}

BYTE tvm_decode_instruction (ECTX ectx)
{
	return decode_next (ectx);
}

int tvm_dispatch_instruction (ECTX ectx, BYTE instr)
{
	return do_dispatch (ectx, instr);
}

int tvm_dispatch (ECTX ectx, UWORD cycles)
{
	int ret;

	do {
		ret = do_dispatch (ectx, decode_next (ectx));
		cycles -= 2;
	} while (cycles && !ret);

	return (ectx->state = ret);
}

static int run_pre_init (ECTX ectx)
{
	int ret;

	ectx->state = ECTX_RUNNING;

	if (ectx->run_hook) {
		if ((ret = ectx->run_hook (ectx))) {
			return (ectx->state = ret);
		}
	}

	if (WPTR == (WORDPTR) NOT_PROCESS_P) {
		if ((ret = ectx->run_next_on_queue (ectx))) {
			return (ectx->state = ret);
		}
	}

	return ECTX_CONTINUE;
}

/** 
 * Runs an execution context until it exits for some reason.
 * Returning the exit reason.
 */
int tvm_run (ECTX ectx)
{
	int ret;

	if ((ret = run_pre_init (ectx))) {
		return ret;
	}

	return tvm_dispatch (ectx, 1);
}

/** 
 * Runs an execution context until it exits for some reason,
 * or reaches the instruction count.  Returning the exit reason.
 */
int tvm_run_count (ECTX ectx, UWORD count)
{
    int ret;

    if ((ret = run_pre_init (ectx))) {
        return ret;
    }

    if ((ret = tvm_dispatch (ectx, count << 1))) {
        return ret;
    }
    
    return ECTX_TIME_SLICE;
}

