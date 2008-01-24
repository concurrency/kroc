/*
tvm - interpreter.h
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

#ifndef INTERPRETER_H
#define INTERPRETER_H

/* ANNO: Various things are exported from this file which are not neccesarily
 * used in the interpreter, but they might be in other parts of a
 * Transterpreter, such as the wrapper. Ignore warnings about exported things
 * from here which are not used.
 */
/*@-exportlocal@*/

#ifdef HAVE_SETJMP_H
#	include <setjmp.h>
#endif

#ifdef HAVE_SETJMP_H
extern jmp_buf runloop_env; 
#else
extern int running;
#endif
extern int exit_value;

/* Interpreter exit codes */
enum {
	EXIT_STACK_BOTTOM	= 0,	/* For what currently amounts to a good exit */
	EXIT_DEADLOCK		= 100,	/* When the running program has deadlocked */
	EXIT_HALTED		= 200,	/* When the program halted due to an error */
	EXIT_DEBUG_TRAP		= 300,	/* When the program halted due to an J 0 */
	EXIT_ERROR		= 400,	/* When a runtime error occurs */
	EXIT_ALIGN_ERROR	= 666,	/* A memory alignment error */
	EXIT_BAD_ADDR		= 667,	/* A bad memory address */
	EXIT_INS_INVALID	= 998,	/* If we hit an invalid instruction */
	EXIT_INS_NOT_IMP	= 999,	/* If we hit an unimplemented instruction */
	EXIT_SCHEDULER_BAD_1	= 5000	/* If the scheduler  has gone bad */
};

/* Stack frame return types */
enum {
	RET_ERROR		= 0,	/* Set error flag on process return */
	RET_REAL,			/* Go to address on process return */
	RET_SHUTDOWN			/* Shutdown interpreter on return */
};

void exit_runloop(int ret_val);
void init_stackframe(POOTER *where, int argc, WORD argv[],
		POOTER vectorspace, POOTER mobilespace, POOTER forkbarrier,
		int ret_type, BPOOTER ret_addr);
void initial_stackframe(POOTER *where, int argc, WORD argv[],
		POOTER vectorspace, POOTER mobilespace, int add_forkingbarrier);
void final_ret(void);
int run(void);

#endif
