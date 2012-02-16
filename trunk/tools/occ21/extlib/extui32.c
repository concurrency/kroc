/*{{{  module header */

/*
 *	Unsigned 32 bit integer arithmetic
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

static char RcsId[] = "$Id: extui32.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  UInt32Add */
#ifdef ANSI
PUBLIC VOID UInt32Add (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Add (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;

    *Error = (A > (MOSTPOS_INT32_UNSIGNED - B));
}
/*}}}*/

/*{{{  UInt32Plus */
#ifdef ANSI
PUBLIC VOID UInt32Plus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Plus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;
}
/*}}}*/

/*{{{  UInt32Sub */
#ifdef ANSI
PUBLIC VOID UInt32Sub (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Sub (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;

    *Error = (A < B);
}
/*}}}*/

/*{{{  UInt32Minus */
#ifdef ANSI
PUBLIC VOID UInt32Minus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Minus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;
}
/*}}}*/

/*{{{  UInt32Mul */
#ifdef ANSI
PUBLIC VOID UInt32Mul (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Mul (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A * B;

    *Error = ((B != 0) && (A > (MOSTPOS_INT32_UNSIGNED / B)));
}
/*}}}*/

/*{{{  UInt32Times */
#ifdef ANSI
PUBLIC VOID UInt32Times (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Times (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A * B;
}
/*}}}*/

/*{{{  UInt32Div */
#ifdef ANSI
PUBLIC VOID UInt32Div (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Div (Error, Result, A, B)
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

/*{{{  UInt32Rem */
#ifdef ANSI
PUBLIC VOID UInt32Rem (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Rem (Error, Result, A, B)
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

/*{{{  UInt32Eq */
#ifdef ANSI
PUBLIC VOID UInt32Eq (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Eq (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = (A == B);
}
/*}}}*/

/*{{{  UInt32Gt */
#ifdef ANSI
PUBLIC VOID UInt32Gt (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Gt (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = (A > B);
}
/*}}}*/

/*{{{  UInt32BitAnd */
#ifdef ANSI
PUBLIC VOID UInt32BitAnd (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32BitAnd (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A & B;
}
/*}}}*/

/*{{{  UInt32BitOr */
#ifdef ANSI
PUBLIC VOID UInt32BitOr (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32BitOr (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A | B;
}
/*}}}*/

/*{{{  UInt32Xor */
#ifdef ANSI
PUBLIC VOID UInt32Xor (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32Xor (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A ^ B;
}
/*}}}*/

/*{{{  UInt32BitNot */
#ifdef ANSI
PUBLIC VOID UInt32BitNot (BIT32 *Result, BIT32 A)
#else
PUBLIC VOID UInt32BitNot (Result, A)
    BIT32 *Result, A;
#endif
{
    *Result = ~A;
}
/*}}}*/

/*{{{  UInt32LShift */
#ifdef ANSI
PUBLIC VOID UInt32LShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32LShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 31) ? 0 : ((B == 0) ? A : (A << B));
}
/*}}}*/

/*{{{  UInt32RShift */
#ifdef ANSI
PUBLIC VOID UInt32RShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID UInt32RShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 31) ? 0 : ((B == 0) ? A : (A >> B));
}
/*}}}*/
/*}}}*/
