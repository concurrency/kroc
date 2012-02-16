/*{{{  module header */

/*
 *	Arithmetic
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

static char RcsId[] = "$Id: extui8.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  UInt8Add */
#ifdef ANSI
PUBLIC VOID UInt8Add (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Add (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;

    *Error = (A > (MOSTPOS_INT8_UNSIGNED - B));
}
/*}}}*/

/*{{{  UInt8Plus */
#ifdef ANSI
PUBLIC VOID UInt8Plus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Plus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (A + B) & LowBits8;
}
/*}}}*/

/*{{{  UInt8Sub */
#ifdef ANSI
PUBLIC VOID UInt8Sub (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Sub (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;

    *Error = (A < B);
}
/*}}}*/

/*{{{  UInt8Minus */
#ifdef ANSI
PUBLIC VOID UInt8Minus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Minus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (A - B) & LowBits8;
}
/*}}}*/

/*{{{  UInt8Mul */
#ifdef ANSI
PUBLIC VOID UInt8Mul (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Mul (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A * B;

    *Error = ((B != 0) && (A > (MOSTPOS_INT8_UNSIGNED / B)));
}
/*}}}*/

/*{{{  UInt8Times */
#ifdef ANSI
PUBLIC VOID UInt8Times (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Times (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (A * B) & LowBits8;
}
/*}}}*/

/*{{{  UInt8Div */
#ifdef ANSI
PUBLIC VOID UInt8Div (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Div (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    if (B == 0)
        *Error = TRUE;
    else
    {
        /*{{{   */
        Error = FALSE;
        
        *Result = A / B;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  UInt8Rem */
#ifdef ANSI
PUBLIC VOID UInt8Rem (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Rem (Error, Result, A, B)
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

/*{{{  UInt8Eq */
#ifdef ANSI
PUBLIC VOID UInt8Eq (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Eq (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = (A == B);
}
/*}}}*/

/*{{{  UInt8Gt */
#ifdef ANSI
PUBLIC VOID UInt8Gt (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Gt (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = (A > B);
}
/*}}}*/

/*{{{  UInt8BitAnd */
#ifdef ANSI
PUBLIC VOID UInt8BitAnd (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8BitAnd (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A & B;
}
/*}}}*/

/*{{{  UInt8BitOr */
#ifdef ANSI
PUBLIC VOID UInt8BitOr (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8BitOr (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A | B;
}
/*}}}*/

/*{{{  UInt8Xor */
#ifdef ANSI
PUBLIC VOID UInt8Xor (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8Xor (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A ^ B;
}
/*}}}*/

/*{{{  UInt8BitNot */
#ifdef ANSI
PUBLIC VOID UInt8BitNot (BIT32 *Result, BIT32 A)
#else
PUBLIC VOID UInt8BitNot (Result, A)
    BIT32 *Result, A;
#endif
{
    *Result = ~A & LowBits8;
}
/*}}}*/

/*{{{  UInt8LShift */
#ifdef ANSI
PUBLIC VOID UInt8LShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8LShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 7) ? 0 : ((B == 0) ? A : ((A << B) & LowBits8));
}
/*}}}*/

/*{{{  UInt8RSHift */
#ifdef ANSI
PUBLIC VOID UInt8RShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt8RShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 7) ? 0 : ((B == 0) ? A : (A >> B));
}
/*}}}*/
/*}}}*/
