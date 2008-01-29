/*
tvm - instructions.c
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

/**
 * This function is placed in the instruction jump table for instructions
 * which have not been implemented, but are otherwise valid.
 */
TVM_INSTRUCTION (ins_not_implemented)
{
	return ECTX_INS_UNSUPPORTED;
}

#if 0
#ifdef DEBUG_ERRORS
#include <stdio.h>
void debug_error_flag(char *file, int line, int error_num)
{
	fprintf(stderr, "error_flag set to %d at %s:%d\n", error_num, file, line);
	error_flag = error_num;
	exit_runloop(EXIT_ERROR);
}
#else /* !DEBUG_ERRORS */
void set_error_flag(int error_num)
{
	/* FIXME: This needs to check HaltOnErrorFlag and halt if that is set, ie
	 *
	 * if errorflag = 0
	 *   if haltonerr = 1
	 *     stop the machine
	 *   else
	 *     errorflag = 1
	 * */
	error_flag = error_num;

	/* FIXME: Should we always exit???? */
	exit_runloop(EXIT_ERROR);
}
#endif

void clear_error_flag(void)
{
	error_flag = 0;
}
#endif
