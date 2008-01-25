/*
tvm - mem_native.h
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

#ifndef MEM_NATIVE_H
#define MEM_NATIVE_H

#define read_word(LOC)            ( *((WORDPTR)(LOC)) )
#define write_word(LOC, VAL)      ( *((WORDPTR)(LOC)) ) = (VAL)
#define wordptr_minus(WPTR, LOC)  ( ((WORDPTR)WPTR) - (LOC) )
#define wordptr_plus(WPTR, LOC)   ( ((WORDPTR)WPTR) + (LOC) )

#define read_byte(LOC)           ( *((BYTEPTR)(LOC)) )
#define write_byte(LOC, VAL)     ( *((BYTEPTR)(LOC)) ) = (VAL)
#define byteptr_minus(WPTR, LOC) ( ((BYTEPTR)WPTR) - (LOC) )
#define byteptr_plus(WPTR, LOC)  ( ((BYTEPTR)WPTR) + (LOC) )

#ifdef __FPU_SUPPORT__

#define read_wordf(LOC)           ( *((float *)(LOC)) )
#define write_wordf(LOC, VAL)     ( *((float *)(LOC)) ) = (VAL)

#define read_wordd(LOC)           ( *((double *)(LOC)) )
#define write_wordd(LOC, VAL)     ( *((double *)(LOC)) ) = (VAL)
#endif

#define wordptr_real_address(LOC) ( LOC )

#endif
