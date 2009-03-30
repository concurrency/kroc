/*
tvm - mem_bigendian.h
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

#ifndef MEM_BIGENDIAN_H
#define MEM_BIGENDIAN_H

void write_word(WORDPTR ptr, WORD val);
WORD read_word(WORDPTR ptr);

void write_byte(BYTEPTR ptr, BYTE val);
BYTE read_byte (BYTEPTR ptr);

/* FIXME: Check that this is the right place to put the shared annotations, Ii
 * would have liked (I think) to annotate wptr and iptr as shared, though that
 * does not seem to work. It would however seem to make more sense to me to
 * do that... */
/* FIXME: Right, so I think I have found a better annotation for this, which
 * it @returned@. Which means that a parameter may be aliased by a return value.
 * ie no memory leak error will be reported as splint beleives that what is 
 * returned is the same pointer that was passed in. (I think).
 */
/* ANNO: Marking these functions as using and returning @shared@ pointers, ie
 * pointers into memory which is never deallocated */
WORDPTR wordptr_plus(/*@returned@*/WORDPTR ptr, WORD inc);
WORDPTR wordptr_minus(/*@returned@*/ WORDPTR ptr, WORD inc);
BYTEPTR byteptr_plus(/*@returned@*/ BYTEPTR ptr, WORD inc);
BYTEPTR byteptr_minus(/*@returned@*/ BYTEPTR ptr, WORD inc);

#ifdef __FPU_SUPPORT__
void write_wordf(WORDPTR ptr, float val);
float read_wordf(WORDPTR ptr);
void write_wordd(WORDPTR ptr, double val);
double read_wordd(WORDPTR ptr);
#endif  /*__FPU_SUPPORT__ */

#define wordptr_real_address(LOC) ( LOC )

#endif 

