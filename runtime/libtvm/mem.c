/*
tvm - mem.c
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud, Carl G. Ritson

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

/* -- Bit twiddling lesson --
 * 
 * For values on a power of two, that value -1 gives you a mask of bits
 * which will be zero in things which are a multiple of that type.
 *
 * e.g. 
 *   4 = 0100b
 *   4 - 1 => 3
 *   3 = 0011b
 *
 * Taking the bitwise-AND of a number and this mask is the same as doing a
 * modulo operation.
 *
 * e.g.
 *   4 & 3 = 4 % 4 = 0
 *   3 & 3 = 3 % 4 = 3
 *   5 & 3 = 5 % 4 = 1
 *
 * The difference is that modulo is typically compiled to integer division
 * which is 10s of times slower than bitwise logic, and sadly not all
 * compilers optimise fixed modulos to bitwise ops.
 */

#if !defined(TVM_USE_MEMCPY)
BYTEPTR _tvm_memcpy (BYTEPTR dst, BYTEPTR src, UWORD n)
{
	/* If everything is word aligned then copy words, else copy
	 * the bytes one by one.
	 */
	if (!((n | (UWORD) dst | (UWORD) src) & (TVM_WORD_LENGTH - 1))) {
		WORDPTR dst_w = (WORDPTR) dst;
		WORDPTR src_w = (WORDPTR) src;
		/* Reduce bytes to words */
		n >>= WSH;
		/* This count is now on words */
		while (n--) {
			write_word (dst_w, read_word (src_w));
			src_w = wordptr_plus (src_w, 1);
			dst_w = wordptr_plus (dst_w, 1);
		}
	} else {
		while (n--) {
			write_byte (dst, read_byte (src));
			src = byteptr_plus (src, 1);
			dst = byteptr_plus (dst, 1);
		}
	}

	return dst;
}
#endif /* !TVM_USE_MEMCPY */

#if !defined(TVM_USE_MEMSET)
BYTEPTR _tvm_memset (BYTEPTR s, WORD c, UWORD n)
{	
	/* If everything is word aligned and there are > 4 words then set
	 * words, else set the bytes one by one.
	 */
	if (!((n | (UWORD) s) & (TVM_WORD_LENGTH - 1)) && (n >> (WSH + 2))) {
		WORDPTR s_w = (WORDPTR) s;
		/* Expand byte data of c to a word */
		c &= 0xff;
		c |= (c << 8) & 0xff00;
		#if TVM_WORD_LENGTH == 4
		c |= (c << 16) & 0xffff0000;
		#endif
		/* Reduce bytes to words */
		n >>= WSH;
		/* This count is now on words */
		while (n--) {
			write_word (s_w, c);
			s_w = wordptr_plus (s_w, 1);
		}
	} else {
		while (n--) {
			write_byte (s, (BYTE) c);
			s = byteptr_plus (s, 1);
		}
	}
	
	return s;
}
#endif

#ifdef TVM_TYPE_SHADOW
void copy_type_shadow (ECTX ectx, BYTEPTR dst, BYTEPTR src, WORD count)
{
	unsigned int s = (unsigned int) src;

	if (s >= ectx->shadow_start && (s + ((unsigned int) count)) <= ectx->shadow_end) {
		unsigned int d = (unsigned int) dst;
		if (d >= ectx->shadow_start && (d + ((unsigned int) count)) <= ectx->shadow_end) {
			unsigned int 	length	= (count + (WORD_BITS - 1)) >> WSH;
			BYTE 		*tc_d	= ectx->type_store + (((d - ectx->shadow_start)) >> WSH);
			BYTE 		*tc_s	= ectx->type_store + (((s - ectx->shadow_start)) >> WSH);
			
			while (length--) {
				*(tc_d++) = *(tc_s++);
			}
		}
	} else {
		fill_type_shadow (ectx, dst, count, STYPE_DATA);
	}
}

void fill_type_shadow (ECTX ectx, BYTEPTR ptr, WORD count, WORD type)
{
	unsigned int p = (unsigned int) ptr;

	if (p >= ectx->shadow_start && (p + ((unsigned int) count)) <= ectx->shadow_end) {
		unsigned int 	offset	= ((p - ectx->shadow_start)) >> WSH;
		unsigned int 	length	= (count + (WORD_BITS - 1)) >> WSH;
		BYTE 		*tc	= ectx->type_store + offset;
		
		while (length--) {
			*(tc++) = (BYTE) type;
		}
	}
}
#endif /* TVM_TYPE_SHADOW */

