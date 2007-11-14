/*{{{  module header */

/*
 *	Standard INMOS header for value definitions
 *	Copyright (C) 1987, 1990 Inmos Limited
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*}}}  */

#ifndef _IMSVALSH
/*{{{  not included header file */
#define _IMSVALSH

/*{{{  integer range values */
/*{{{  8 bit integer values */
#ifndef BitsPerWord8
#define BitsPerWord8  8
#define BytesPerWord8 1
#endif

#ifndef MOSTPOS_INT8
#define MOSTPOS_INT8 127
#define MOSTNEG_INT8 ((-MOSTPOS_INT8) - 1)

#define MaxSignedInt8 MOSTPOS_INT8
#define MinSignedInt8 MOSTNEG_INT8

#define IntegerLimit8 ((MaxSignedInt8 / 10) - 1)
#endif

#ifndef MOSTPOS_INT8_UNSIGNED
#define MOSTPOS_INT8_UNSIGNED 0XFFU
#define MOSTNEG_INT8_UNSIGNED 0U

#define MaxUnsignedInt8 MOSTPOS_INT8_UNSIGNED
#define MinUnsignedInt8 MOSTNEG_INT8_UNSIGNED
#endif
/*}}}  */

/*{{{  16 bit integer values */
#ifndef BitsPerWord16
#define BitsPerWord16  16
#define BytesPerWord16 2
#endif

#ifndef MOSTPOS_INT16
#define MOSTPOS_INT16 32767
#define MOSTNEG_INT16 ((-MOSTPOS_INT16) - 1)

#define MaxSignedInt16 MOSTPOS_INT16
#define MinSignedInt16 MOSTNEG_INT16

#define IntegerLimit16 ((MaxSignedInt16 / 10) - 1)
#endif

#ifndef MOSTPOS_INT16_UNSIGNED
#define MOSTPOS_INT16_UNSIGNED 0XFFFFU
#define MOSTNEG_INT16_UNSIGNED 0U

#define MaxUnsignedInt16 MOSTPOS_INT16_UNSIGNED
#define MinUnsignedInt16 MOSTNEG_INT16_UNSIGNED
#endif
/*}}}  */

/*{{{  32 bit integer values */
#ifndef BitsPerWord32
#define BitsPerWord32  32
#define BytesPerWord32 4
#endif

#ifndef MOSTPOS_INT32
#define MOSTPOS_INT32 2147483647
#define MOSTNEG_INT32 ((-MOSTPOS_INT32) - 1)

#define MaxSignedInt32 MOSTPOS_INT32
#define MinSignedInt32 MOSTNEG_INT32

#define IntegerLimit32 ((MaxSignedInt32 / 10) - 1)
#endif

#ifndef MOSTPOS_INT32_UNSIGNED
#define MOSTPOS_INT32_UNSIGNED 0XFFFFFFFFU
#define MOSTNEG_INT32_UNSIGNED 0U

#define MaxUnsignedInt32 MOSTPOS_INT32_UNSIGNED
#define MinUnsignedInt32 MOSTNEG_INT32_UNSIGNED
#endif
/*}}}  */
/*}}}  */

/*{{{  word selector values */
/*{{{  word manipulation */
#define GetMSBit(T) (((T) 1) << ((sizeof(T) * 8) - 1))
#define SetMSBit(V, T) ((T) ((V) | GetMSBit(T)))
#define ClrMSBit(V, T) ((T) ((V) & (~GetMSBit(T))))
#define TstMSBit(V, T) (((T) ((V) & GetMSBit(T))) == GetMSBit(T))
/*}}}  */

/*{{{  8 bit word values */
#define SignBit8  0X00000080
#define LowBits8  0X000000FF
#define HighBits8 0XFFFFFF00
#define NegBits8  0XFFFFFF80
/*}}}  */

/*{{{  16 bit word values */
#define SignBit16  0X00008000
#define LowBits16  0X0000FFFF
#define HighBits16 0XFFFF0000
#define NegBits16  0XFFFF8000
/*}}}  */

/*{{{  32 bit word values */
#define SignBit32  0X80000000
#define LowBits32  0XFFFFFFFF
#define HighBits32 0X00000000
#define NegBits32  0X80000000
/*}}}  */
/*}}}  */
/*}}}  */
#endif
