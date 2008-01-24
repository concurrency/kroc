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

/* ANNO: This file redefines the memory interface (functions), this is ok,
 * as only one memory interface file gets compiled in ever. */
/*@-redef@*/

#include "transputer.h"
#include "interpreter.h"
#include "mem.h"

static BYTE *memory_array;
int mem_size;

void setup_mem(WORD *ptr, int size)
{
	memory_array = (BYTE *)ptr;
	mem_size = size;
}

static inline void check_addr(WORD ptr) {
	if (ptr < 0 || ptr >= mem_size) {
		exit_runloop(EXIT_BAD_ADDR);
	}
}

WORD read_mem(POOTER ptr)
{
	check_addr(ptr);
	return *(WORD *)(&memory_array[ptr]);
}

void write_mem(POOTER ptr, WORD val)
{
	check_addr(ptr);
	*(WORD *)(&memory_array[ptr]) = val;
}

BYTE read_byte(BPOOTER ptr)
{
	check_addr(ptr);
	return memory_array[ptr];
}

void write_byte(BPOOTER ptr, BYTE val) 
{
	check_addr(ptr);
	memory_array[ptr] = val;
}

float read_memf(POOTER ptr)
{
	check_addr(ptr);
	return *(float *)(&memory_array[ptr]);
}

void write_memf(POOTER ptr, float val)
{
	check_addr(ptr);
	*(float *)(&memory_array[ptr]) = val;
}

double read_memd(POOTER ptr)
{
	check_addr(ptr);
	return *(double *)(&memory_array[ptr]);
}

void write_memd(POOTER ptr, double val)
{
	check_addr(ptr);
	*(double *)(&memory_array[ptr]) = val;
}

int *pooter_real_address(POOTER ptr)
{
	check_addr(ptr);
	return (int *)(&memory_array[ptr]);
}

