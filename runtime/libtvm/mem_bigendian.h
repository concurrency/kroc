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

void write_mem(POOTER ptr, WORD val);
WORD read_mem(POOTER ptr);

void write_byte(BPOOTER ptr, BYTE val);
BYTE read_byte (BPOOTER ptr);

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
POOTER pooter_plus(/*@returned@*/POOTER ptr, WORD inc);
POOTER pooter_minus(/*@returned@*/ POOTER ptr, WORD inc);
BPOOTER bpooter_plus(/*@returned@*/ BPOOTER ptr, WORD inc);
BPOOTER bpooter_minus(/*@returned@*/ BPOOTER ptr, WORD inc);

#ifdef __FPU_SUPPORT__
void write_memf(POOTER ptr, float val);
float read_memf(POOTER ptr);
void write_memd(POOTER ptr, double val);
double read_memd(POOTER ptr);
#endif  /*__FPU_SUPPORT__ */

#define pooter_real_address(LOC) ( LOC )

#endif 

