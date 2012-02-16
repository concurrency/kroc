/*{{{  module header */

/*
 *	32 bit integer arithmetic
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

static char RcsId[] = "$Id: exti32.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  Int32Add */
#ifdef ANSI
PUBLIC VOID Int32Add (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Add (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;

    *Error = ((((A ^ B) & SignBit32) == 0) && (((A ^ *Result) & SignBit32) != 0));
}
/*}}}*/

/*{{{  Int32Plus */
#ifdef ANSI
PUBLIC VOID Int32Plus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Plus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A + B;
}
/*}}}*/

/*{{{  Int32Sub */
#ifdef ANSI
PUBLIC VOID Int32Sub (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Sub (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;

    *Error = ((((A ^ B) & SignBit32) != 0) && (((A ^ *Result) & SignBit32) != 0));
}
/*}}}*/

/*{{{  Int32Minus */
#ifdef ANSI
PUBLIC VOID Int32Minus (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Minus (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A - B;
}
/*}}}*/

/*{{{  Int32Mul */
#ifdef ANSI
PUBLIC VOID Int32Mul (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Mul (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    BOOL Negative;
    BIT32 ResultHi;

    /*{{{  convert A and B to positive values */
    Negative = (((A ^ B) & SignBit32) != 0);
    
    if ((A & SignBit32) != 0)
        A = ~A + 1;
    
    if ((B & SignBit32) != 0)
        B = ~B + 1;
    /*}}}*/

    LONGPROD(&ResultHi, Result, A, B, 0);

    if (ResultHi == 0)
    {
        /*{{{   */
        if ((*Result & SignBit32) == 0)
        {
            /*{{{   */
            *Error = FALSE;
            
            if (Negative)
                *Result = ~*Result + 1;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            if (Negative && (*Result == MOSTNEG_INT32))
                *Error = FALSE;
            else
                *Error = TRUE;
            /*}}}*/
        }
        /*}}}*/
    }
    else
        *Error = TRUE;
}
/*}}}*/

/*{{{  Int32Times */
#ifdef ANSI
PUBLIC VOID Int32Times (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Times (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A * B;
}
/*}}}*/

/*{{{  Int32Div */
#ifdef ANSI
PUBLIC VOID Int32Div (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Div (Error, Result, A, B)
    BOOL *Error;
    BIT32 *Result, A, B;
#endif
{
    if (B == 0)
        *Error = TRUE;
    else
    {
        /*{{{   */
        if ((A == MOSTNEG_INT32) && (B == -1))
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

/*{{{  Int32Rem */
#ifdef ANSI
PUBLIC VOID Int32Rem (BOOL *Error, BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Rem (Error, Result, A, B)
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

/*{{{  Int32Eq */
#ifdef ANSI
PUBLIC VOID Int32Eq (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Eq (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = (A == B);
}
/*}}}*/

/*{{{  Int32Gt */
#ifdef ANSI
PUBLIC VOID Int32Gt (BOOL *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Gt (Result, A, B)
    BOOL *Result;
    BIT32 A, B;
#endif
{
    *Result = ((INT32) A > (INT32) B);
}
/*}}}*/

/*{{{  Int32BitAnd */
#ifdef ANSI
PUBLIC VOID Int32BitAnd (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32BitAnd (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A & B;
}
/*}}}*/

/*{{{  Int32BitOr */
#ifdef ANSI
PUBLIC VOID Int32BitOr (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32BitOr (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A | B;
}
/*}}}*/

/*{{{  Int32Xor */
#ifdef ANSI
PUBLIC VOID Int32Xor (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32Xor (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = A ^ B;
}
/*}}}*/

/*{{{  Int32BitNot */
#ifdef ANSI
PUBLIC VOID Int32BitNot (BIT32 *Result, BIT32 A)
#else
PUBLIC VOID Int32BitNot (Result, A)
    BIT32 *Result, A;
#endif
{
    *Result = ~A;
}
/*}}}*/

/*{{{  Int32LShift */
#ifdef ANSI
PUBLIC VOID Int32LShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32LShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 31) ? 0 : ((B == 0) ? A : (A << B));
}
/*}}}*/

/*{{{  Int32RShift */
#ifdef ANSI
PUBLIC VOID Int32RShift (BIT32 *Result, BIT32 A, BIT32 B)
#else
PUBLIC VOID Int32RShift (Result, A, B)
    BIT32 *Result, A, B;
#endif
{
    *Result = (B > 31) ? 0 : ((B == 0) ? A : (A >> B));
}
/*}}}*/
/*}}}*/
