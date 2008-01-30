/*
tvm - tvm_mem_intf.h
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

#ifndef TVM_MEM_INTF_H
#define TVM_MEM_INTF_H

#ifdef TVM_MEM_INTF_ARRAY
/*{{{  Array memory interface */
extern WORD read_word(WORDPTR ptr);
extern void write_word(WORDPTR ptr, WORD val);
static TVM_INLINE WORDPTR wordptr_minus(WORDPTR ptr, WORD inc) {
	return ptr - inc * TVM_WORD_LENGTH;
}
static TVM_INLINE WORDPTR wordptr_plus(WORDPTR ptr, WORD inc) {
	return ptr + inc * TVM_WORD_LENGTH;
}

extern BYTE read_byte(BYTEPTR ptr);
extern void write_byte(BYTEPTR ptr, BYTE val);
static TVM_INLINE BYTEPTR byteptr_minus(BYTEPTR ptr, WORD inc) {
	return ptr - inc;
}
static TVM_INLINE BYTEPTR byteptr_plus(BYTEPTR ptr, WORD inc) {
	return ptr + inc;
}

extern float read_wordf(WORDPTR ptr);
extern void write_wordf(WORDPTR ptr, float val);

extern double read_wordd(WORDPTR ptr);
extern void write_wordd(WORDPTR ptr, double val);

extern int *wordptr_real_address(WORDPTR ptr);
/*}}}*/
#elif defined(TVM_MEM_INTF_BIGENDIAN)
/*{{{  (deprecated) Big-endian memory interface */
extern void write_word(WORDPTR ptr, WORD val);
extern WORD read_word(WORDPTR ptr);

extern void write_byte(BYTEPTR ptr, BYTE val);
extern BYTE read_byte (BYTEPTR ptr);

extern WORDPTR wordptr_plus(WORDPTR ptr, WORD inc);
extern WORDPTR wordptr_minus(WORDPTR ptr, WORD inc);
extern BYTEPTR byteptr_plus(BYTEPTR ptr, WORD inc);
extern BYTEPTR byteptr_minus(BYTEPTR ptr, WORD inc);

extern void write_wordf(WORDPTR ptr, float val);
extern float read_wordf(WORDPTR ptr);
extern void write_wordd(WORDPTR ptr, double val);
extern double read_wordd(WORDPTR ptr);

#define wordptr_real_address(LOC) ( LOC )
/*}}}*/
#else
/*{{{  Native memory interface */
#define read_word(LOC)            ( *((WORDPTR)(LOC)) )
#define write_word(LOC, VAL)      ( *((WORDPTR)(LOC)) ) = (VAL)
#define wordptr_minus(WPTR, LOC)  ( ((WORDPTR)WPTR) - (LOC) )
#define wordptr_plus(WPTR, LOC)   ( ((WORDPTR)WPTR) + (LOC) )

#define read_byte(LOC)           ( *((BYTEPTR)(LOC)) )
#define write_byte(LOC, VAL)     ( *((BYTEPTR)(LOC)) ) = (VAL)
#define byteptr_minus(WPTR, LOC) ( ((BYTEPTR)WPTR) - (LOC) )
#define byteptr_plus(WPTR, LOC)  ( ((BYTEPTR)WPTR) + (LOC) )

#define read_wordf(LOC)           ( *((float *)(LOC)) )
#define write_wordf(LOC, VAL)     ( *((float *)(LOC)) ) = (VAL)

#define read_wordd(LOC)           ( *((double *)(LOC)) )
#define write_wordd(LOC, VAL)     ( *((double *)(LOC)) ) = (VAL)

#define wordptr_real_address(LOC) ( LOC )
/*}}}*/
#endif

#endif /* !TVM_MEM_INTF */
