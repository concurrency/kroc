/*{{{  module header */

/*
 *	Unsigned integer arithmetic
 *	Copyright (C) 1987 Inmos Limited
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

static char RcsId[] = "$Id: extui16.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
/*}}}*/

/*{{{  include files */
/* Included files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "imsvals.h"

#include "extlib.h"
/*}}}*/

/*{{{  global procedures */
/*{{{  UInt16Add */
#ifdef ANSI
PUBLIC VOID UInt16Add (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Add (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;

    *Error = (A > (MOSTPOS_INT16_UNSIGNED - B));
}
/*}}}*/

/*{{{  UInt16Plus */
#ifdef ANSI
PUBLIC VOID UInt16Plus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Plus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (A + B) & LowBits16;
}
/*}}}*/

/*{{{  UInt16Sub */
#ifdef ANSI
PUBLIC VOID UInt16Sub (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Sub (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;

    *Error = (A < B);
}
/*}}}*/

/*{{{  UInt16Minus */
#ifdef ANSI
PUBLIC VOID UInt16Minus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Minus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (A - B) & LowBits16;
}
/*}}}*/

/*{{{  UInt16Mul */
#ifdef ANSI
PUBLIC VOID UInt16Mul (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Mul (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A * B;

    *Error = ((B != 0) && (A > (MOSTPOS_INT16_UNSIGNED / B)));
}
/*}}}*/

/*{{{  UInt16Times */
#ifdef ANSI
PUBLIC VOID UInt16Times (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Times (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (A * B) & LowBits16;
}
/*}}}*/

/*{{{  UInt16Div */
#ifdef ANSI
PUBLIC VOID UInt16Div (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Div (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    if (B == 0)
        *Error = TRUE;
    else
    {
        /*{{{   */
        *Error = FALSE;
        
        *Result = A / B;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  UInt16Rem */
#ifdef ANSI
PUBLIC VOID UInt16Rem (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Rem (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    if (B == 0)
        *Error = TRUE;
    else
    {
        /*{{{   */
        *Error = FALSE;
        
        *Result = A % B;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  UInt16Eq */
#ifdef ANSI
PUBLIC VOID UInt16Eq (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Eq (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = (A == B);
}
/*}}}*/

/*{{{  UInt16Gt */
#ifdef ANSI
PUBLIC VOID UInt16Gt (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Gt (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = (A > B);
}
/*}}}*/

/*{{{  UInt16BitAnd */
#ifdef ANSI
PUBLIC VOID UInt16BitAnd (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16BitAnd (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A & B;
}
/*}}}*/

/*{{{  UInt16BitOr */
#ifdef ANSI
PUBLIC VOID UInt16BitOr (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16BitOr (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A | B;
}
/*}}}*/

/*{{{  UInt16Xor */
#ifdef ANSI
PUBLIC VOID UInt16Xor (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16Xor (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A ^ B;
}
/*}}}*/

/*{{{  UInt16BitNot */
#ifdef ANSI
PUBLIC VOID UInt16BitNot (BIT32 *Result, BIT32 A)
#else
PUBLIC VOID UInt16BitNot (Result, A)
    BIT32 *Result, A;
#endif
{
    *Result = ~A & LowBits16;
}
/*}}}*/

/*{{{  UInt16LShift */
#ifdef ANSI
PUBLIC VOID UInt16LShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16LShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 15) ? 0 : ((B == 0) ? A : ((A << B) & LowBits16));
}
/*}}}*/

/*{{{  UInt16RShift */
#ifdef ANSI
PUBLIC VOID UInt16RShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt16RShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 15) ? 0 : ((B == 0) ? A : (A >> B));
}
/*}}}*/
/*}}}*/
