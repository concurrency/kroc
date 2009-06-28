/*
tvm - mem_array.c
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2009 Adam Sampson, Christian L. Jacobsen, Matthew C. Jadud

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
#include "tvm_mem_intf.h"

#ifdef TVM_MEM_INTF_AVR

#if TVM_WORD_LENGTH != 2
#error AVR memory interface only works with 2-byte words
#endif

static const BYTEPTR progmem_base = (BYTEPTR) 0x8000;

static int is_progmem(BYTEPTR ptr)
{
	return (ptr >= progmem_base);
}

static prog_void *addr_to_progmem(BYTEPTR ptr)
{
	return (prog_void *) (ptr - progmem_base);
}

BYTEPTR tvm_addr_from_progmem(prog_void *ptr)
{
	return (BYTEPTR) ((WORD) ptr + progmem_base);
}

WORD read_word(WORDPTR ptr)
{
	if (is_progmem((BYTEPTR) ptr)) {
		return pgm_read_word(addr_to_progmem((BYTEPTR) ptr));
	} else {
		return *ptr;
	}
}

INT16 read_int16(INT16PTR ptr)
{
	return read_word(ptr);
}

BYTE read_byte(BYTEPTR ptr)
{
	if (is_progmem(ptr)) {
		return pgm_read_byte(addr_to_progmem(ptr));
	} else {
		return *ptr;
	}
}

float read_wordf(WORDPTR ptr)
{
	if (is_progmem((BYTEPTR) ptr)) {
		float f;
		int32_t *p = (int32_t *) &f;
		p[0] = pgm_read_dword(addr_to_progmem((BYTEPTR) ptr));
		return f;
	} else {
		return *ptr;
	}
}

double read_wordd(WORDPTR ptr)
{
	if (is_progmem((BYTEPTR) ptr)) {
		double d;
		int32_t *p = (int32_t *) &d;
		p[0] = pgm_read_dword(addr_to_progmem((BYTEPTR) ptr));
		p[1] = pgm_read_dword(addr_to_progmem(((BYTEPTR) ptr) + 4));
		return d;
	} else {
		return *ptr;
	}
}

#endif
