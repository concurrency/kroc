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

int tvm_init(tvm_t *tvm)
{
	tvm->head 		= NULL;
	tvm->tail		= NULL;
	return 0;
}

void tvm_ectx_init(tvm_t *tvm, ECTX ectx)
{
	tvm_ectx_reset(ectx);
	
	ectx->pri 	= 0;

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

	ectx->tvm	= tvm;

	if (tvm->head == NULL) {
		tvm->head = ectx;
	} else {
		tvm->tail->next = ectx;
	}
	
	tvm->tail	= ectx;
	ectx->next	= NULL;

	_tvm_install_scheduler(ectx);
}

void tvm_ectx_reset(ECTX ectx)
{
	ectx->state 	= ECTX_INIT;
	ectx->eflags	= 0;
	ectx->sflags	= 0;

	/* evaluation stack */
	OREG = 0;
	AREG = 0;
	BREG = 0;
	CREG = 0;

	/* setup scheduler queues */
	WPTR = (WORDPTR)NOT_PROCESS_P;
	FPTR = (WORDPTR)NOT_PROCESS_P;
	BPTR = (WORDPTR)NOT_PROCESS_P;
	TPTR = (WORDPTR)NOT_PROCESS_P;
}

void tvm_ectx_layout(ECTX ectx, WORDPTR base,
		const char *tlp_fmt, const int tlp_argc,
		WORD ws_size, WORD vs_size, WORD ms_size,
		WORD *size, WORDPTR *ws, WORDPTR *vs, WORDPTR *ms)
{
	int frame_size =
		1 + tlp_argc 
		+ (vs_size ? 1 : 0) 
		+ (ms_size ? 1 : 0);
	
	/* Stack frame has a minimum size of 4 */
	if (frame_size < 4)
	{
		frame_size = 4;
	}

	/* The plus 1 in here is for the shutdown bytecode */
	*size 	= frame_size + 1 + ws_size + vs_size + ms_size;
	*ws 	= wordptr_plus(base, frame_size + ws_size + 1);
	*vs	= vs_size ? *ws : 0;
	*ms	= ms_size ? wordptr_plus(*ws, vs_size) : 0;
}

/*
 * Setup the initial workspace and instruction pointers,
 * and stack frame for an execution context.
 */
int tvm_ectx_install_tlp(ECTX ectx, BYTEPTR code,
		WORDPTR ws, WORDPTR vs, WORDPTR ms,
		const char *fmt, int argc, const WORD argv[])
{
	WORDPTR fb = 0;
	int i, frame_size;

	/* Make sure we don't have too many arguments */
	if (argc > TVM_ECTX_TLP_ARGS)
	{
		return -1;
	}

	/* Copy arguments to execution context */
	ectx->tlp_argc = argc;
	for (i = 0; i < argc; ++i)
	{
		char arg = ectx->tlp_fmt[i] = fmt[i];

		if (arg == 'F')
		{
			#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
			int ret;
			/* Allocate forking barrier */
			if ((ret = mt_alloc(ectx, MT_MAKE_BARRIER(MT_BARRIER_FORKING), 0, &fb)))
			{
				return ret;
			}
			ectx->tlp_argv[i] = (WORD) fb;
			#else
			return -2;
			#endif
		} 
		else 
		{
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
	frame_size = 1 + argc 
		+ (vs ? 1 : 0) 
		+ (ms ? 1 : 0);
	
	WPTR = wordptr_minus(WPTR, frame_size < 4 ? 4 - frame_size: 0);

	/* Put a shutdown instruction in top of the stack frame */
	WPTR = wordptr_minus(WPTR, 1);
	write_byte(byteptr_plus((BYTEPTR)WPTR, 0), 0x2F);
	write_byte(byteptr_plus((BYTEPTR)WPTR, 1), 0xFE);

	/* Pad stack frame */
	if (frame_size < 4) 
	{
		WPTR = wordptr_minus(WPTR, 4 - frame_size);
		frame_size = 4;
	}
	
	/* Set up forking barrier pointer */
	if(fb)
	{
		WPTR = wordptr_minus(WPTR, 1);
		write_word(WPTR, (WORD)fb);
	}

	/* Set up mobilespace pointer */
	if(ms)
	{
		WPTR = wordptr_minus(WPTR, 1);
		write_word(WPTR, (WORD)ms);
	}
	
	/* Set up vectorspace pointer */
	if(vs)
	{
		WPTR = wordptr_minus(WPTR, 1);
		write_word(WPTR, (WORD)vs);
	}
	
	/* Set up arguments */
	for(i = ectx->tlp_argc - 1; i >= 0; i--)
	{
		if (ectx->tlp_fmt[i] != 'F')
		{
			WPTR = wordptr_minus(WPTR, 1);
			write_word(WPTR, ectx->tlp_argv[i]);
		}
	}

	/* Store the return pointer, to completion byte code */
	/* FIXME: this won't work for virtual memory right? */
	WPTR = wordptr_minus(WPTR, 1);
	write_word(WPTR, (WORD)wordptr_plus(WPTR, frame_size));

	return 0;
}

static void disconnect_channel(ECTX ectx, char dir, WORDPTR ptr)
{
	WORD chan_value = read_word(ptr);

	if((chan_value & (~1)) != NOT_PROCESS_P)
	{
		WORDPTR ws	= (WORDPTR) (chan_value & (~1));
		ECTX	ws_ectx	= (ECTX)WORKSPACE_GET(ws, WS_ECTX);
		if(chan_value & 1)
		{
			WORD alt_state = WORKSPACE_GET(ws, WS_STATE);
			if(alt_state == EXTENDED_P)
			{
				ws_ectx->add_to_queue_external(ws_ectx, ectx, ws);
				/* disregard return value */
			}
		}
		else
		{
			BYTEPTR data_ptr = (BYTEPTR)WORKSPACE_GET(ws, WS_POINTER);
			UWORD   data_len = (UWORD)WORKSPACE_GET(ws, WS_PENDING);

			if(data_len > 0)
			{
				/* input */
				while(data_len--)
				{
					write_byte(data_ptr, (BYTE) 0);
					data_ptr = byteptr_plus(data_ptr, 1);
				}
			}
			#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
			else if(data_len == MIN_INT)
			{
				/* mobile input */
				write_word((WORDPTR)data_ptr, (WORD) NULL_P);
			}
			else if(data_len == (MIN_INT + 1))
			{
				/* mobile output */
				WORDPTR src = (WORDPTR)data_ptr;
				WORDPTR ptr = (WORDPTR)read_word(data_ptr);

				if(ptr != (WORDPTR) NULL_P)
				{
					UWORD move = MT_FALSE;

					mt_io_update(ws_ectx, &ptr, &move);
					/* ignore return; potentially bad */

					if(move == MT_TRUE)
					{
						/* Pointer moved, delete old reference */
						write_word(src, (WORD) NULL_P);
					}

					mt_release(ws_ectx, ptr);
					/* ignore return; potentially bad */
				}
			}
			#endif /* TVM_DYNAMIC_MEMORY && TVM_OCCAM_PI */

			ws_ectx->add_to_queue_external(ws_ectx, ectx, ws);
			/* disregard return value */
		}
	}
	
	write_word(ptr, (NOT_PROCESS_P | 1));
}

void tvm_ectx_disconnect(ECTX ectx)
{
	int i;

	for(i = 0; i < ectx->tlp_argc; i++)
	{
		switch (ectx->tlp_fmt[i])
		{
			case '?':
			case '!':
				disconnect_channel(
					ectx, 
					ectx->tlp_fmt[i], 
					(WORDPTR) ectx->tlp_argv[i]
				);
				break;
			default:
				break;
		}
	}
}

int tvm_dispatch(ECTX ectx)
{
	BYTE instr;
	
	/* Read the instruction */
#if (defined MEMORY_INTF_BIGENDIAN)
	instr = *IPTR; /* FIXME */
#else
	instr = read_byte(IPTR);
#endif
	
	/* Increment instruction pointer */
	IPTR = byteptr_plus(IPTR, 1);

	/* Put the least significant bits in OREG */
	OREG |= (instr & 0x0f);

#ifdef TVM_DISPATCH_SWITCH
	return dispatch_instruction(ectx, instr);
#else
	/* Use the other bits to index into the jump table */
	return primaries[instr >> 4](ectx);
#endif
}

static int run_pre_init(ECTX ectx)
{
	int ret;

	ectx->state = ECTX_RUNNING;

	if (ectx->run_hook)
	{
		if ((ret = ectx->run_hook(ectx)))
		{
			return ret;
		}
	}

	if (WPTR == (WORDPTR)NOT_PROCESS_P) {
		if ((ret = ectx->run_next_on_queue(ectx)))
		{
			return ret;
		}
	}

	return ECTX_CONTINUE;
}

/** 
 * Runs an execution context until it exits for some reason.
 * Returning the exit reason.
 */
int tvm_run(ECTX ectx)
{
	int ret;

	if ((ret = run_pre_init(ectx)))
	{
		return ret;
	}

	for(;;)
	{
		if ((ret = tvm_dispatch(ectx)))
		{
			return (ectx->state = ret);
		}
	}
}

/** 
 * Runs an execution context until it exits for some reason,
 * or reaches the instruction count.  Returning the exit reason.
 */
int tvm_run_count(ECTX ectx, UWORD count)
{
	int ret;

	if ((ret = run_pre_init(ectx)))
	{
		return ret;
	}

	while(count--)
	{
		if ((ret = tvm_dispatch(ectx)))
		{
			return (ectx->state = ret);
		}
	}

	return ECTX_TIME_SLICE;
}

