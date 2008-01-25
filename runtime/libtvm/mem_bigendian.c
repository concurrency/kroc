/*
tvm - mem_bigendian.c
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



void write_word(WORDPTR ptr, WORD val)
{	
	//ASSERT_WORD_ALIGNED(ptr, TVM_WORD_LENGTH, "writing");

#if TVM_WORD_LENGTH == 2
	*ptr = (WORD)SwapTwoBytes((UWORD)val);
#elif TVM_WORD_LENGTH == 4
	*ptr = (WORD)SwapFourBytes((UWORD)val);
#elif
#	error "Unsupported wordlength"
#endif
}

WORD read_word(WORDPTR ptr)
{
	//ASSERT_WORD_ALIGNED(ptr, TVM_WORD_LENGTH, "reading");
	/* ANNO: Cast to UNSIGNED WORD as the behaviour of a shift on negative values
	 * is implementation defined */
#if TVM_WORD_LENGTH == 2
	return (WORD)SwapTwoBytes((UWORD)*ptr);
#elif TVM_WORD_LENGTH == 4
	return (WORD)SwapFourBytes((UWORD)*ptr);
#elif
#	error "Unsupported wordlength"
#endif
}

void write_byte(BYTEPTR ptr, BYTE val) 
{
	*ptr = val;
}

BYTE read_byte (BYTEPTR ptr)
{
	return *ptr;
}

WORDPTR wordptr_plus(WORDPTR ptr, WORD inc)
{
	return ptr + inc;
}

WORDPTR wordptr_minus(WORDPTR ptr, WORD inc)
{
	return ptr - inc;
}

BYTEPTR byteptr_plus(BYTEPTR ptr, WORD inc)
{
	return ptr + inc;
}

BYTEPTR byteptr_minus(BYTEPTR ptr, WORD inc)
{
	return ptr - inc;
}


#ifdef __FPU_SUPPORT__

# define SwapEightBytes(x) \
     (__extension__ \
      ({ union { uint64_t __ll; \
     uint32_t __l[2]; } __v, __r; \
   __v.__ll = (x); \
   __r.__l[0] = SwapFourBytes (__v.__l[1]); \
   __r.__l[1] = SwapFourBytes (__v.__l[0]); \
   __r.__ll; })) 

void write_wordf(WORDPTR ptr, float val)
{	
	*ptr = (float)SwapFourBytes((UWORD)val);
}

float read_wordf(WORDPTR ptr)
{
	return (float)SwapFourBytes((UWORD)*ptr);
}

void write_wordd(WORDPTR ptr, double val)
{	
	*ptr = (double)SwapEightBytes((UWORD)val);
}

double read_wordd(WORDPTR ptr)
{
	return (double)SwapEightBytes((UWORD)*ptr);
}

#endif  /*__FPU_SUPPORT__ */


