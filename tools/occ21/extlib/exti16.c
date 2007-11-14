/*{{{  module header */

/*
 *	16 bit integer arithmetic
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

static char RcsId[] = "$Id: exti16.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  Int16Add */
#ifdef ANSI
PUBLIC VOID Int16Add (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Add (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;

    *Error = ((((A ^ B) & SignBit16) == 0) && (((A ^ *Result) & SignBit16) != 0));
}
/*}}}*/

/*{{{  Int16Plus */
#ifdef ANSI
PUBLIC VOID Int16Plus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Plus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;

    if ((*Result & SignBit16) == 0)
        *Result &= LowBits16;
    else
        *Result |= HighBits16;
}
/*}}}*/

/*{{{  Int16Sub */
#ifdef ANSI
PUBLIC VOID Int16Sub (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Sub (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;

    *Error = ((((A ^ B) & SignBit16) != 0) && (((A ^ *Result) & SignBit16) != 0));
}
/*}}}*/

/*{{{  Int16Minus */
#ifdef ANSI
PUBLIC VOID Int16Minus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Minus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;

    if ((*Result & SignBit16) == 0)
        *Result &= LowBits16;
    else
        *Result |= HighBits16;
}
/*}}}*/

/*{{{  Int16Mul */
#ifdef ANSI
PUBLIC VOID Int16Mul (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Mul (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A * B;

    *Error = (((*Result & NegBits16) != 0) && ((*Result & NegBits16) != NegBits16));
}
/*}}}*/

/*{{{  Int16Times */
#ifdef ANSI
PUBLIC VOID Int16Times (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Times (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A * B;

    if ((*Result & SignBit16) == 0)
        *Result &= LowBits16;
    else
        *Result |= HighBits16;
}
/*}}}*/

/*{{{  Int16Div */
#ifdef ANSI
PUBLIC VOID Int16Div (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Div (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    if (B == 0)
        *Error = TRUE;
    else
    {
        /*{{{   */
        if ((A == MOSTNEG_INT16) && (B == -1))
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

/*{{{  Int16Rem */
#ifdef ANSI
PUBLIC VOID Int16Rem (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Rem (Error, Result, A, B)
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

/*{{{  Int16Eq */
#ifdef ANSI
PUBLIC VOID Int16Eq (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Eq (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = (A == B);
}
/*}}}*/

/*{{{  Int16Gt */
#ifdef ANSI
PUBLIC VOID Int16Gt (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Gt (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = ((INT32) A > (INT32) B);
}
/*}}}*/

/*{{{  Int16BitAnd */
#ifdef ANSI
PUBLIC VOID Int16BitAnd (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16BitAnd (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A & B;
}
/*}}}*/

/*{{{  Int16BitOr */
#ifdef ANSI
PUBLIC VOID Int16BitOr (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16BitOr (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A | B;
}
/*}}}*/

/*{{{  Int16Xor */
#ifdef ANSI
PUBLIC VOID Int16Xor (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16Xor (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A ^ B;
}
/*}}}*/

/*{{{  Int16BitNot */
#ifdef ANSI
PUBLIC VOID Int16BitNot (BIT32 *Result, BIT32 A)
#else
PUBLIC VOID Int16BitNot (Result, A)
    BIT32 *Result, A;
#endif
{
    *Result = ~A;
}
/*}}}*/

/*{{{  Int16LShift */
#ifdef ANSI
PUBLIC VOID Int16LShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16LShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 15) ? 0 : ((B == 0) ? A : (A << B));

    if ((*Result & SignBit16) == 0)
        *Result &= LowBits16;
    else
        *Result |= HighBits16;
}
/*}}}*/

/*{{{  Int16RShift */
#ifdef ANSI
PUBLIC VOID Int16RShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int16RShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 15) ? 0 : ((B == 0) ? A : ((A & LowBits16) >> B));
}
/*}}}*/
/*}}}*/
