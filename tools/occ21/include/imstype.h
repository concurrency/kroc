/*{{{  module header */

/*
 *	Standard INMOS header for type definitions
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

#ifndef _IMSTYPEH
/*{{{  not included header file */
#define _IMSTYPEH

/*{{{  constant definitions  (see also imsvals.h)*/
/*{{{  boolean truth values */
#ifndef TRUE
/*{{{   */
#define TRUE  1
#define FALSE 0
/*}}}  */
#endif
/*}}}  */

#ifndef _IMSVALSH
/*{{{  integer range values */
/*{{{  8 bit integer values */
#define BitsPerWord8  8
#define BytesPerWord8 1

#define MOSTPOS_INT8 127
#define MOSTNEG_INT8 ((-MOSTPOS_INT8) - 1)

#define MOSTPOS_INT8_UNSIGNED 0XFF
#define MOSTNEG_INT8_UNSIGNED 0

#define MaxSignedInt8 MOSTPOS_INT8
#define MinSignedInt8 MOSTNEG_INT8

#define MaxUnsignedInt8 MOSTPOS_INT8_UNSIGNED
#define MinUnsignedInt8 MOSTNEG_INT8_UNSIGNED

#define IntegerLimit8 ((MaxSignedInt8 / 10) - 1)
/*}}}  */

/*{{{  16 bit integer values */
#define BitsPerWord16  16
#define BytesPerWord16 2

#define MOSTPOS_INT16 32767
#define MOSTNEG_INT16 ((-MOSTPOS_INT16) - 1)

#define MOSTPOS_INT16_UNSIGNED 0XFFFF
#define MOSTNEG_INT16_UNSIGNED 0

#define MaxSignedInt16 MOSTPOS_INT16
#define MinSignedInt16 MOSTNEG_INT16

#define MaxUnsignedInt16 MOSTPOS_INT16_UNSIGNED
#define MinUnsignedInt16 MOSTNEG_INT16_UNSIGNED

#define IntegerLimit16 ((MaxSignedInt16 / 10) - 1)
/*}}}  */

/*{{{  32 bit integer values */
#define BitsPerWord32  32
#define BytesPerWord32 4

#ifdef target_cpu_alpha
#define MOSTPOS_INT32 2147483647
#define MOSTPOS_INT32_UNSIGNED 0XFFFFFFFF
#else
#define MOSTPOS_INT32 2147483647L
#define MOSTPOS_INT32_UNSIGNED 0XFFFFFFFFL
#endif

#define MOSTNEG_INT32 ((-MOSTPOS_INT32) - 1)
#define MOSTNEG_INT32_UNSIGNED 0

#define MaxSignedInt32 MOSTPOS_INT32
#define MinSignedInt32 MOSTNEG_INT32

#define MaxUnsignedInt32 MOSTPOS_INT32_UNSIGNED
#define MinUnsignedInt32 MOSTNEG_INT32_UNSIGNED

#define IntegerLimit32 ((MaxSignedInt32 / 10) - 1)
/*}}}  */
/*}}}  */

/*{{{  word selector values */
/*{{{  8 bit word values */
#define SignBit8  0X00000080L
#define LowBits8  0X000000FFL
#define HighBits8 0XFFFFFF00L
#define NegBits8  0XFFFFFF80L
/*}}}  */

/*{{{  16 bit word values */
#define SignBit16  0X00008000L
#define LowBits16  0X0000FFFFL
#define HighBits16 0XFFFF0000L
#define NegBits16  0XFFFF8000L
/*}}}  */

#ifdef target_cpu_alpha
/*{{{  32 bit word values (int)*/
#define SignBit32  0X80000000
#define LowBits32  0XFFFFFFFF
#define HighBits32 0X00000000
#define NegBits32  0X80000000
/*}}}  */
#else
/*{{{  32 bit word values (long int)*/
#define SignBit32  0X80000000L
#define LowBits32  0XFFFFFFFFL
#define HighBits32 0X00000000L
#define NegBits32  0X80000000L
/*}}}  */
#endif
/*}}}  */
#define _IMSVALSH
#endif
/*{{{  useful 32 bit values */
#ifdef target_cpu_alpha
#define ZERO32   0
#define ONE32    1
#define THREE32  3
#define FOUR32   4
#define TEN32   10
#else
#define ZERO32 0L
#define ONE32  1L
#define THREE32  3L
#define FOUR32   4L
#define TEN32   10L
#endif
/*}}}  */
/*}}}  */

/*{{{  host compiler macros */

#undef _HOSTTYPE /* Reset host type flag */

/*{{{  DEC Alpha specific macros (64-bit machine)*/
#ifdef target_cpu_alpha
/*{{{   */
#define _HOSTTYPE /* Set host type flag */

typedef int                BOOL;

typedef long               PTRINT;

typedef void *             POINTER;
typedef const void *       VALPOINTER;

typedef char               CHAR;
typedef unsigned char      BYTE;

typedef int                INT;
typedef char               INT8;
typedef short              INT16;
typedef int                INT32;

typedef unsigned int       BIT;
typedef unsigned char      BIT8;
typedef unsigned short     BIT16;
typedef unsigned int       BIT32;

#define VOID void /* Cannot typedef void */
/*}}}  */
#endif
/*}}}  */

/*{{{  MS C specific macros */
#if defined(HOST_OS_IS_MSDOS) && defined(COMPILER_IS_MSC) && !defined(_HOSTTYPE)
/*{{{   */
#define _HOSTTYPE /* Set host type flag */

typedef int                BOOL;
typedef long               PTRINT;

typedef void *             POINTER;
typedef const void *       VALPOINTER;

typedef char               CHAR;
typedef unsigned char      BYTE;

typedef signed int         INT;
typedef signed char        INT8;
typedef signed short int   INT16;
typedef signed long int    INT32;

typedef unsigned int       BIT;
typedef unsigned char      BIT8;
typedef unsigned short int BIT16;
typedef unsigned long int  BIT32;

#define VOID void /* Cannot typedef void */
/*}}}  */
#endif
/*}}}  */

/*{{{  DEC VAX C specific macros */
#if defined(HOST_OS_IS_VMS) && !defined(_HOSTTYPE)
/*{{{   */
#define _HOSTTYPE /* Set host type flag */

typedef int                BOOL;
typedef long               PTRINT;

typedef void *             POINTER;
typedef const void *       VALPOINTER;

typedef char               CHAR;
typedef unsigned char      BYTE;

typedef int                INT;
typedef char               INT8;
typedef short int          INT16;
typedef long int           INT32;

typedef unsigned int       BIT;
typedef unsigned char      BIT8;
typedef unsigned short int BIT16;
typedef unsigned long int  BIT32;

#define VOID void /* Cannot typedef void */
/*}}}  */
#endif
/*}}}  */

/*{{{  3L C specific macros */
#if defined(COMPILER_IS_LLL) && ! defined (_HOSTTYPE)
/*{{{   */
#define _HOSTTYPE /* Set host type flag */

typedef int                BOOL;
typedef long               PTRINT;

typedef unsigned char *    POINTER;
typedef unsigned char *    VALPOINTER;

typedef char               CHAR;
typedef unsigned char      BYTE;

typedef int                INT;
typedef char               INT8;
typedef short int          INT16;
typedef long int           INT32;

typedef unsigned int       BIT;
typedef unsigned char      BIT8;
typedef unsigned short int BIT16;
typedef unsigned long int  BIT32;

#define VOID void /* Cannot typedef void */
/*}}}  */
#endif
/*}}}  */

/*{{{  UNIX specific macros */
#if defined(HOST_OS_IS_UNIX) && !defined(_HOSTTYPE)
/*{{{   */
#define _HOSTTYPE /* Set host type flag */

typedef int                BOOL;
typedef long               PTRINT;

typedef void *             POINTER;
typedef const void *       VALPOINTER;

typedef char               CHAR;
typedef unsigned char      BYTE;

#if SIZEOF_SIGNED_INT == 0
typedef int                INT;
typedef char               INT8;
typedef short int          INT16;
typedef int                INT32;
#else
typedef signed int         INT;
typedef signed char        INT8;
typedef signed short int   INT16;
typedef signed int         INT32;
#endif

typedef unsigned int       BIT;
typedef unsigned char      BIT8;
typedef unsigned short int BIT16;
typedef unsigned int       BIT32;

#define VOID void /* Cannot typedef void */
/*}}}  */
#endif
/*}}}  */


/*{{{  IMS C specific macros */
#if defined (IMS) && !defined(_HOSTTYPE)
/*{{{   */
#define _HOSTTYPE /* Set host type flag */

typedef int                BOOL;
typedef long               PTRINT;

typedef void *             POINTER;
typedef const void *       VALPOINTER;

typedef char               CHAR;
typedef unsigned char      BYTE;

typedef signed int         INT;
typedef signed char        INT8;
typedef signed short int   INT16;
typedef signed long int    INT32;

typedef unsigned int       BIT;
typedef unsigned char      BIT8;
typedef unsigned short int BIT16;
typedef unsigned long int  BIT32;

#define VOID void /* Cannot typedef void */
/*}}}  */
#endif
/*}}}  */

/*{{{  WATCOM C specific macros */
#if defined(HOST_OS_IS_MSDOS) && defined(COMPILER_IS_WATCOM) && !defined(_HOSTTYPE)
/*{{{   */
#define _HOSTTYPE /* Set host type flag */

typedef int                BOOL;

typedef long               PTRINT;

typedef void *             POINTER;
typedef const void *       VALPOINTER;

typedef char               CHAR;
typedef unsigned char      BYTE;

typedef signed int         INT;
typedef signed char        INT8;
typedef signed short int   INT16;
typedef signed long int    INT32;

typedef unsigned int       BIT;
typedef unsigned char      BIT8;
typedef unsigned short int BIT16;
typedef unsigned long int  BIT32;

#define VOID void /* Cannot typedef void */
/*}}}  */
#endif
/*}}}  */
/*}}}  */

/*{{{  generic type defines */
#ifdef _HOSTTYPE
/*{{{   */
#undef _HOSTTYPE /* Reset host type flag */

typedef BOOL BOOLEAN;

typedef BIT   UINT;
typedef BIT8  UINT8;
typedef BIT16 UINT16;
typedef BIT32 UINT32;

typedef BIT   WORD;
typedef BIT8  WORD8;
typedef BIT16 WORD16;
typedef BIT32 WORD32;
/*}}}  */
#endif
/*}}}  */

/*{{{  manipulation defines */
#define GetMSBit(T) (((T) 1) << ((sizeof(T) * 8) - 1))

#define SetMSBit(V, T) ((T) ((V) | GetMSBit(T)))

#define ClrMSBit(V, T) ((T) ((V) & (~GetMSBit(T))))

#define TstMSBit(V, T) (((T) ((V) & GetMSBit(T))) == GetMSBit(T))
/*}}}  */
/*}}}  */
#endif
