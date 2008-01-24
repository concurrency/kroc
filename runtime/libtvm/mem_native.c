/*
tvm - mem_native.c
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

/* ANNO: This file redefines the memory interface (functions), this is ok,
 * as only one memory interface file gets compiled in ever. */
/*@-redef@*/

#include "transputer.h"
#include "mem.h"

/*
void write_mem(POOTER ptr, WORD val)
{
	*ptr = val;
}

WORD read_mem(POOTER ptr)
{
	return *ptr;
}

void write_byte(BPOOTER ptr, BYTE val) 
{
	*ptr = val; 
}

BYTE read_byte (BPOOTER ptr)
{
	return *ptr;
}

POOTER pooter_plus(POOTER ptr, WORD inc)
{
	return ptr + inc;
}

POOTER pooter_minus(POOTER ptr, WORD inc)
{
	return ptr - inc;
}

BPOOTER bpooter_plus(BPOOTER ptr, WORD inc)
{
	return ptr + inc;
}

BPOOTER bpooter_minus(BPOOTER ptr, WORD inc)
{
	return ptr - inc;
}
*/
