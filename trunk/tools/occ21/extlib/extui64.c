/*{{{  module header */

/*
 *	64 bit unsigned integer arithmetic
 *	Copyright (C) 2008 Fred Barnes  <frmb@kent.ac.uk>
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


/*
 *	based on code from exti64.c, Copyright (C) 1987 Inmos Limited, GPL V2
 */
/*}}}*/

/*{{{  include files */
/* Included files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "imsvals.h"
#include "extlib.h"

/*}}}*/

/* do this using gcc's builtin routines, union to convert */
typedef union U_ui64 {
	unsigned long long val;
	struct {
#if defined(TARGET_IS_BIGENDIAN)
		unsigned long hi;
		unsigned long lo;
#else
		unsigned long lo;
		unsigned long hi;
#endif
	} part;
} ui64;

typedef union U_i64 {
	long long val;
	struct {
#if defined(TARGET_IS_BIGENDIAN)
		long hi;
		long lo;
#else
		long lo;
		long hi;
#endif
	} part;
} i64;


/*{{{  global procedures */
void UInt64Add (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 res, a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	res.val = (a.val + b.val);

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;

	*Error = ((res.val < a.val) || (res.val < b.val));
}
/*}}}*/
void UInt64Plus (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 res, a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	res.val = (a.val + b.val);

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;
}
/*}}}*/
void UInt64Sub (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 res, a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	res.val = (a.val - b.val);

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;

	*Error = (a.val > b.val);
}
/*}}}*/
void UInt64Minus (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 res, a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	res.val = (a.val - b.val);

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;
}
/*}}}*/
void UInt64Mul (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 res, a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	res.val = (a.val * b.val);

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;

	/* overflow check */
	if (!AHi && !BHi) {
		*Error = FALSE;
	} else {
		BIT32 r = AHi;
		int i;

		for (i=0; r; r >>= 1, i++);
		r = BHi;
		for (i=0; r; r >>= 1, i++);
		*Error = (i >= 32);
	}
}
/*}}}*/
void UInt64Times (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 res, a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	res.val = (a.val * b.val);

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;
}
/*}}}*/
void UInt64Div (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	if ((BHi == 0) && (BLo == 0)) {
		*Error = TRUE;
	} else {
		ui64 res, a, b;

		a.part.lo = (unsigned long)ALo;
		a.part.hi = (unsigned long)AHi;
		b.part.lo = (unsigned long)BLo;
		b.part.hi = (unsigned long)BHi;

		res.val = (a.val / b.val);

		*ResultHi = (BIT32)res.part.hi;
		*ResultLo = (BIT32)res.part.lo;
		*Error = FALSE;
	}
}
/*}}}*/
void UInt64Rem (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	if ((BHi == 0) && (BLo == 0)) {
		*Error = TRUE;
	} else {
		ui64 res, a, b;

		a.part.lo = (unsigned long)ALo;
		a.part.hi = (unsigned long)AHi;
		b.part.lo = (unsigned long)BLo;
		b.part.hi = (unsigned long)BHi;

		res.val = (a.val % b.val);

		*ResultHi = (BIT32)res.part.hi;
		*ResultLo = (BIT32)res.part.lo;
		*Error = FALSE;
	}
}
/*}}}*/
void UInt64Eq (BOOL *Result, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	*Result = (a.val == b.val);
}
/*}}}*/
void UInt64Gt (BOOL *Result, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	*Result = (a.val > b.val);
}
/*}}}*/
void UInt64BitAnd (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 res, a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	res.val = (a.val & b.val);

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;
}
/*}}}*/
void UInt64BitOr (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 res, a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	res.val = (a.val | b.val);

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;
}
/*}}}*/
void UInt64Xor (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo) /*{{{*/
{
	ui64 res, a, b;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;
	b.part.lo = (unsigned long)BLo;
	b.part.hi = (unsigned long)BHi;

	res.val = (a.val ^ b.val);

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;
}
/*}}}*/
void UInt64BitNot (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo) /*{{{*/
{
	ui64 res, a;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;

	res.val = ~a.val;

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;
}
/*}}}*/
void UInt64LShift (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 B) /*{{{*/
{
	ui64 res, a;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;

	res.val = a.val << (int)B;

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;
}
/*}}}*/
void UInt64RShift (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 B) /*{{{*/
{
	ui64 res, a;

	a.part.lo = (unsigned long)ALo;
	a.part.hi = (unsigned long)AHi;

	res.val = a.val >> (int)B;

	*ResultHi = (BIT32)res.part.hi;
	*ResultLo = (BIT32)res.part.lo;
}
/*}}}*/

/*}}}*/
