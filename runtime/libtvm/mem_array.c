/*
tvm - mem_array.c
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

#include "tvm_mem.h"

#ifdef TVM_MEM_INTF_ARRAY

static BYTE *memory_array;
int mem_size;

void setup_mem(WORD *ptr, int size)
{
	memory_array = (BYTE *)ptr;
	mem_size = size;
}

static TVM_INLINE void check_addr(WORD ptr) {
	if (ptr < 0 || ptr >= mem_size) {
		exit_runloop(EXIT_BAD_ADDR);
	}
}

WORD read_word(WORDPTR ptr)
{
	check_addr(ptr);
	return *(WORD *)(&memory_array[ptr]);
}

void write_word(WORDPTR ptr, WORD val)
{
	check_addr(ptr);
	*(WORD *)(&memory_array[ptr]) = val;
}

BYTE read_byte(BYTEPTR ptr)
{
	check_addr(ptr);
	return memory_array[ptr];
}

void write_byte(BYTEPTR ptr, BYTE val) 
{
	check_addr(ptr);
	memory_array[ptr] = val;
}

float read_wordf(WORDPTR ptr)
{
	check_addr(ptr);
	return *(float *)(&memory_array[ptr]);
}

#ifdef TVM_USE_FPU

void write_wordf(WORDPTR ptr, float val)
{
	check_addr(ptr);
	*(float *)(&memory_array[ptr]) = val;
}

double read_wordd(WORDPTR ptr)
{
	check_addr(ptr);
	return *(double *)(&memory_array[ptr]);
}

void write_wordd(WORDPTR ptr, double val)
{
	check_addr(ptr);
	*(double *)(&memory_array[ptr]) = val;
}

int *wordptr_real_address(WORDPTR ptr)
{
	check_addr(ptr);
	return (int *)(&memory_array[ptr]);
}

#endif /* TVM_USE_FPU */

#endif /* TVM_MEM_INTF_ARRAY */
