/*{{{  module header */

/*
 *	8 bit integer arithmetic
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

static char RcsId[] = "$Id: exti8.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  Int8Add */
#ifdef ANSI
PUBLIC VOID Int8Add (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Add (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;

    *Error = ((((A ^ B) & SignBit8) == 0) && (((A ^ *Result) & SignBit8) != 0));
}
/*}}}*/

/*{{{  Int8Plus */
#ifdef ANSI
PUBLIC VOID Int8Plus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Plus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;

    if ((*Result & SignBit8) == 0)
        *Result &= LowBits8;
    else
        *Result |= HighBits8;
}
/*}}}*/

/*{{{  Int8Sub */
#ifdef ANSI
PUBLIC VOID Int8Sub (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Sub (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;

    *Error = ((((A ^ B) & SignBit8) != 0) && (((A ^ *Result) & SignBit8) != 0));
}
/*}}}*/

/*{{{  Int8Minus */
#ifdef ANSI
PUBLIC VOID Int8Minus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Minus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;

    if ((*Result & SignBit8) == 0)
        *Result &= LowBits8;
    else
        *Result |= HighBits8;
}
/*}}}*/

/*{{{  Int8Mul */
#ifdef ANSI
PUBLIC VOID Int8Mul (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Mul (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A * B;

    *Error = (((*Result & NegBits8) != 0) && ((*Result & NegBits8) != NegBits8));
}
/*}}}*/

/*{{{  Int8Times */
#ifdef ANSI
PUBLIC VOID Int8Times (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Times (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A * B;

    if ((*Result & SignBit8) == 0)
        *Result &= LowBits8;
    else
        *Result |= HighBits8;
}
/*}}}*/

/*{{{  Int8Div */
#ifdef ANSI
PUBLIC VOID Int8Div (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Div (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    if (B == 0)
        *Error = TRUE;
    else
    {
        /*{{{   */
        if ((A == MOSTNEG_INT8) && (B == -1))
            *Error = TRUE;
        else
        {
            /*{{{   */
            *Error = FALSE;
            
            *Result = (INT32) A / (INT32) B;
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/

/*{{{  Int8Rem */
#ifdef ANSI
PUBLIC VOID Int8Rem (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Rem (Error, Result, A, B)
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
        
        if ((B == 1) || (B == -1))
            *Result = 0;
        else
            *Result = (INT32) A % (INT32) B;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  Int8Eq */
#ifdef ANSI
PUBLIC VOID Int8Eq (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Eq (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = (A == B);
}
/*}}}*/

/*{{{  Int8Gt */
#ifdef ANSI
PUBLIC VOID Int8Gt (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Gt (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = ((INT32) A > (INT32) B);
}
/*}}}*/

/*{{{  Int8BitAnd */
#ifdef ANSI
PUBLIC VOID Int8BitAnd (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8BitAnd (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A & B;
}
/*}}}*/

/*{{{  Int8BitOr */
#ifdef ANSI
PUBLIC VOID Int8BitOr (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8BitOr (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A | B;
}
/*}}}*/

/*{{{  Int8Xor */
#ifdef ANSI
PUBLIC VOID Int8Xor (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8Xor (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A ^ B;
}
/*}}}*/

/*{{{  Int8BitNot */
#ifdef ANSI
PUBLIC VOID Int8BitNot (BIT32 *Result, BIT32 A)
#else
PUBLIC VOID Int8BitNot (Result, A)
    BIT32 *Result, A;
#endif
{
    *Result = ~A;
}
/*}}}*/

/*{{{  Int8LShift */
#ifdef ANSI
PUBLIC VOID Int8LShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8LShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 7) ? 0 : ((B == 0) ? A : (A << B));

    if ((*Result & SignBit8) == 0)
        *Result &= LowBits8;
    else
        *Result |= HighBits8;
}
/*}}}*/

/*{{{  Int8RSHift */
#ifdef ANSI
PUBLIC VOID Int8RShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int8RShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 7) ? 0 : ((B == 0) ? A : ((A & LowBits8) >> B));
}
/*}}}*/
/*}}}*/
