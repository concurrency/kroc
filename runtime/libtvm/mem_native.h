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

#define read_mem(LOC)            ( *((POOTER)(LOC)) )
#define write_mem(LOC, VAL)      ( *((POOTER)(LOC)) ) = (VAL)
#define pooter_minus(WPTR, LOC)  ( ((POOTER)WPTR) - (LOC) )
#define pooter_plus(WPTR, LOC)   ( ((POOTER)WPTR) + (LOC) )

#define read_byte(LOC)           ( *((BPOOTER)(LOC)) )
#define write_byte(LOC, VAL)     ( *((BPOOTER)(LOC)) ) = (VAL)
#define bpooter_minus(WPTR, LOC) ( ((BPOOTER)WPTR) - (LOC) )
#define bpooter_plus(WPTR, LOC)  ( ((BPOOTER)WPTR) + (LOC) )

#ifdef __FPU_SUPPORT__

#define read_memf(LOC)           ( *((float *)(LOC)) )
#define write_memf(LOC, VAL)     ( *((float *)(LOC)) ) = (VAL)

#define read_memd(LOC)           ( *((double *)(LOC)) )
#define write_memd(LOC, VAL)     ( *((double *)(LOC)) ) = (VAL)
#endif

#define pooter_real_address(LOC) ( LOC )

#endif
