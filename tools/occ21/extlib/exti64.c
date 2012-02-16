/*{{{  module header */

/*
 *	64 bit integer arithmetic
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

static char RcsId[] = "$Id: exti64.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
/*}}}*/

/*{{{  include files */
/* Included files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "imsvals.h"

#include "extlib.h"
/*}}}*/

/*{{{  static procedures */
/*{{{  Int64UnsDiv */
/* Unsigned divide using Knuth's divide algorithm, with error checking */

#ifdef ANSI
PRIVATE VOID Int64UnsDiv (BOOL *Error, BIT32 *QuotHi, BIT32 *QuotLo, BIT32 *RemHi, BIT32 *RemLo, BIT32 DvdHi, BIT32 DvdLo, BIT32 DvsrHi, BIT32 DvsrLo)
#else
PRIVATE VOID Int64UnsDiv (Error, QuotHi, QuotLo, RemHi, RemLo, DvdHi, DvdLo, DvsrHi, DvsrLo)
    BOOL *Error;
    BIT32 *QuotHi, *QuotLo, *RemHi, *RemLo, DvdHi, DvdLo, DvsrHi, DvsrLo;
#endif
{
    *Error = FALSE;

    if ((DvsrLo == 0) && (DvsrHi == 0))
        *Error = TRUE;
    else
    {
        /*{{{   */
        BIT32 Count, X, U[3], W[3], V[2];
        
        if (DvsrHi == 0)
        {
            /*{{{  double by single */
            *RemHi = 0;
            
            LONGDIV(QuotHi, &U[1], 0, DvdHi, DvsrLo);
            LONGDIV(QuotLo, RemLo, U[1], DvdLo, DvsrLo);
            /*}}}*/
        }
        else
        {
            /*{{{  double by double */
            /*{{{  normalise the operands */
            NORMALISE(&Count, &V[1], &V[0], DvsrHi, DvsrLo);
            if (Count == 0)
            {
                /*{{{  already normalised */
                U[2] = 0;
                U[1] = DvdHi;
                U[0] = DvdLo;
                /*}}}*/
            }
            else
            {
                /*{{{  shift the dividend */
                SHIFTLEFT(&U[2], &X, 0, DvdHi, Count);
                SHIFTLEFT(&U[1], &U[0], DvdHi, DvdLo, Count);
                /*}}}*/
            }
            /*}}}*/
            
            /*{{{  evaluate the quotient */
            *QuotHi = 0;
            
            if (U[2] == V[1])
                *QuotLo = MaxUnsignedInt32;
            else
                LONGDIV(QuotLo, &X, U[2], U[1], V[1]);
            
            if (*QuotLo != 0)
            {
                /*{{{  evaluate and check for overflow */
                LONGPROD(&W[1], &W[0], *QuotLo, V[0], 0);
                LONGPROD(&W[2], &W[1], *QuotLo, V[1], W[1]);
                
                LONGDIFF(&X, &U[0], U[0], W[0], 0);
                LONGDIFF(&X, &U[1], U[1], W[1], X);
                LONGDIFF(&X, &U[2], U[2], W[2], X);
                
                while ((U[2] & SignBit32) != 0)
                {
                    /*{{{  add back as required */
                    *QuotLo = *QuotLo - 1;
                    
                    LONGSUM(&X, &U[0], U[0], V[0], 0);
                    LONGSUM(&X, &U[1], U[1], V[1], X);
                    LONGSUM(&X, &U[2], U[2], 0, X);
                    /*}}}*/
                }
                /*}}}*/
            }
            /*}}}*/
            
            /*{{{  evaluate the remainder */
            if (Count == 0)
            {
                /*{{{   */
                *RemLo = U[0];
                *RemHi = U[1];
                /*}}}*/
            }
            else
                SHIFTRIGHT(RemHi, RemLo, U[1], U[0], Count);
            /*}}}*/
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/

/*{{{  Int64DivRem */
/* Signed divide from Knuth (algorithm D, section 4.3.1 - vol 2, pp 257) */

#ifdef ANSI
PRIVATE VOID Int64DivRem (BOOL *Error, BIT32 *QuotHi, BIT32 *QuotLo, BIT32 *RemHi, BIT32 *RemLo, BIT32 DvdHi, BIT32 DvdLo, BIT32 DvsrHi, BIT32 DvsrLo)
#else
PRIVATE VOID Int64DivRem (Error, QuotHi, QuotLo, RemHi, RemLo, DvdHi, DvdLo, DvsrHi, DvsrLo)
    BOOL *Error;
    BIT32 *QuotHi, *QuotLo, *RemHi, *RemLo, DvdHi, DvdLo, DvsrHi, DvsrLo;
#endif
{
    BOOL NegRem, NegQuot;
    BIT32 Borrow, U[2], V[2];

    if ((DvdHi & SignBit32) != 0)
    {
        /*{{{   */
        NegRem = TRUE;
        
        LONGDIFF(&U[1], &U[0], 0, DvdLo, 0);
        LONGDIFF(&Borrow, &U[1], 0, DvdHi, U[1]);
        /*}}}*/
    }
    else
    {
        /*{{{   */
        NegRem = FALSE;
        
        U[0] = DvdLo;
        U[1] = DvdHi;
        /*}}}*/
    }

    if ((DvsrHi & SignBit32) != 0)
    {
        /*{{{   */
        NegQuot = ! NegRem;
        
        LONGDIFF(&V[1], &V[0], 0, DvsrLo, 0);
        LONGDIFF(&Borrow, &V[1], 0, DvsrHi, V[1]);
        /*}}}*/
    }
    else
    {
        /*{{{   */
        NegQuot = NegRem;
        
        V[0] = DvsrLo;
        V[1] = DvsrHi;
        /*}}}*/
    }

    Int64UnsDiv(Error, QuotHi, QuotLo, RemHi, RemLo, U[1], U[0], V[1], V[0]);

    if (! *Error)
    {
        /*{{{   */
        if (((*QuotHi & SignBit32) != 0) && (! NegQuot))
            *Error = TRUE;
        else
        {
            /*{{{   */
            if (NegQuot)
            {
                /*{{{  check if result has negative quotient */
                LONGDIFF(&Borrow, QuotLo, 0, *QuotLo, 0);
                LONGDIFF(&Borrow, QuotHi, 0, *QuotHi, Borrow);
                /*}}}*/
            }
            
            if (NegRem)
            {
                /*{{{  check if result has negative remainder */
                LONGDIFF(&Borrow, RemLo, 0, *RemLo, 0);
                LONGDIFF(&Borrow, RemHi, 0, *RemHi, Borrow);
                /*}}}*/
            }
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/
/*}}}*/

/*{{{  global procedures */
/*{{{  Int64Add */
#ifdef ANSI
PUBLIC VOID Int64Add (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Add (Error, ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BOOL *Error;
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    BIT32 Carry;

    LONGSUM(&Carry, ResultLo, ALo, BLo, 0);
    LONGADD(ResultHi, AHi, BHi, Carry);

    *Error = ((((AHi ^ BHi) & SignBit32) == 0) && (((AHi ^ *ResultHi) & SignBit32) != 0));
}
/*}}}*/

/*{{{  Int64Plus */
#ifdef ANSI
PUBLIC VOID Int64Plus (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Plus (ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    BIT32 Carry;

    LONGSUM(&Carry, ResultLo, ALo, BLo, 0);
    LONGSUM(&Carry, ResultHi, AHi, BHi, Carry);
}
/*}}}*/

/*{{{  Int64Sub */
#ifdef ANSI
PUBLIC VOID Int64Sub (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Sub (Error, ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BOOL *Error;
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    BIT32 Borrow;

    LONGDIFF(&Borrow, ResultLo, ALo, BLo, 0);
    LONGDIFF(&Borrow, ResultHi, AHi, BHi, Borrow);

    *Error = ((((AHi ^ BHi) & SignBit32) != 0) && (((AHi ^ *ResultHi) & SignBit32) != 0));
}
/*}}}*/

/*{{{  Int64Minus */
#ifdef ANSI
PUBLIC VOID Int64Minus (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Minus (ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    BIT32 Borrow;

    LONGDIFF(&Borrow, ResultLo, ALo, BLo, 0);
    LONGDIFF(&Borrow, ResultHi, AHi, BHi, Borrow);
}
/*}}}*/

/*{{{  Int64Mul */
#ifdef ANSI
PUBLIC VOID Int64Mul (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Mul (Error, ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BOOL *Error;
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    BIT32 W[4], Carry1, Carry2, Carry3, Dummy, Asign, Bsign, Borrow;

    Asign = AHi & SignBit32;
    Bsign = BHi & SignBit32;

    LONGPROD(&Carry1, &W[0], ALo, BLo, 0);
    LONGPROD(&Carry1, &W[1], AHi, BLo, Carry1);
    LONGPROD(&Carry2, &W[1], ALo, BHi, W[1]);
    LONGPROD(&Carry3, &Carry2, AHi, BHi, Carry2);

    LONGSUM(&Carry1, &W[2], Carry2, Carry1, 0);
    LONGSUM(&Dummy, &W[3], Carry3, Carry1, 0);

    if (Asign != 0)
    {
        /*{{{  if A < 0 subtract B from carry part */
        LONGDIFF(&Borrow, &W[2], W[2], BLo, 0);
        LONGDIFF(&Borrow, &W[3], W[3], BHi, Borrow);
        /*}}}*/
    }

    if (Bsign != 0)
    {
        /*{{{  if B < 0 subtract A from carry part */
        LONGDIFF(&Borrow, &W[2], W[2], ALo, 0);
        LONGDIFF(&Borrow, &W[3], W[3], AHi, Borrow);
        /*}}}*/
    }

    /*{{{  check for overflow */
    *Error = FALSE;
    
    if (((INT32) W[1]) < 0)
    {
        /*{{{   */
        if ((W[2] & W[3]) != -1)
            *Error = TRUE;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        if ((W[2] | W[3]) != 0)
            *Error = TRUE;
        /*}}}*/
    }
    /*}}}*/

    *ResultLo = W[0];
    *ResultHi = W[1];
}
/*}}}*/

/*{{{  Int64Times */
#ifdef ANSI
PUBLIC VOID Int64Times (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Times (ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    BIT32 Carry;

    LONGPROD(&Carry, ResultLo, ALo, BLo, 0);
    LONGPROD(&Carry, ResultHi, AHi, BLo, Carry);
    LONGPROD(&Carry, ResultHi, ALo, BHi, *ResultHi);
}
/*}}}*/

/*{{{  Int64Div */
#ifdef ANSI
PUBLIC VOID Int64Div (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Div (Error, ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BOOL *Error;
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    BIT32 DummyHi, DummyLo;

    if ((BHi == 0) && (BLo == 0))
        *Error = TRUE;
    else
    {
        /*{{{   */
        if (((AHi == MOSTNEG_INT32) && (ALo == 0)) && ((BHi == -1) && (BLo == -1)))
            *Error = TRUE;
        else
            Int64DivRem(Error, ResultHi, ResultLo, &DummyHi, &DummyLo, AHi, ALo, BHi, BLo);
        /*}}}*/
    }
}
/*}}}*/

/*{{{  Int64Rem */
#ifdef ANSI
PUBLIC VOID Int64Rem (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Rem (Error, ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BOOL *Error;
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    BIT32 DummyHi, DummyLo;

    if ((BHi == 0) && (BLo == 0))
        *Error = TRUE;
    else
    {
        /*{{{   */
        *Error = FALSE;
        
        if (((BHi == 0) && (BLo == 1)) || ((BHi == -1) && (BLo == -1)))
        {
            /*{{{   */
            *ResultHi = 0;
            *ResultLo = 0;
            /*}}}*/
        }
        else
            Int64DivRem(Error, &DummyHi, &DummyLo, ResultHi, ResultLo, AHi, ALo, BHi, BLo);
        /*}}}*/
    }
}
/*}}}*/

/*{{{  Int64Eq */
#ifdef ANSI
PUBLIC VOID Int64Eq (BOOL *Result, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Eq (Result, AHi, ALo, BHi, BLo)
    BOOL *Result;
    BIT32 AHi, ALo, BHi, BLo;
#endif
{
    *Result = ((ALo == BLo) && (AHi == BHi));
}
/*}}}*/

/*{{{  Int64Gt */
#ifdef ANSI
PUBLIC VOID Int64Gt (BOOL *Result, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Gt (Result, AHi, ALo, BHi, BLo)
    BOOL *Result;
    BIT32 AHi, ALo, BHi, BLo;
#endif
{
    *Result = (AHi == BHi) ? (ALo > BLo) : ((INT32) AHi > (INT32) BHi);
}
/*}}}*/

/*{{{  Int64BitAnd */
#ifdef ANSI
PUBLIC VOID Int64BitAnd (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64BitAnd (ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    *ResultLo = ALo & BLo;
    *ResultHi = AHi & BHi;
}
/*}}}*/

/*{{{  Int64BitOr */
#ifdef ANSI
PUBLIC VOID Int64BitOr (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64BitOr (ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    *ResultLo = ALo | BLo;
    *ResultHi = AHi | BHi;
}
/*}}}*/

/*{{{  Int64Xor */
#ifdef ANSI
PUBLIC VOID Int64Xor (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 BHi, BIT32 BLo)
#else
PUBLIC VOID Int64Xor (ResultHi, ResultLo, AHi, ALo, BHi, BLo)
    BIT32 *ResultHi, *ResultLo, AHi, ALo, BHi, BLo;
#endif
{
    *ResultLo = ALo ^ BLo;
    *ResultHi = AHi ^ BHi;
}
/*}}}*/

/*{{{  Int64BitNot */
#ifdef ANSI
PUBLIC VOID Int64BitNot (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo)
#else
PUBLIC VOID Int64BitNot (ResultHi, ResultLo, AHi, ALo)
    BIT32 *ResultHi, *ResultLo, AHi, ALo;
#endif
{
    *ResultLo = ~ALo;
    *ResultHi = ~AHi;
}
/*}}}*/

/*{{{  Int64LShift */
#ifdef ANSI
PUBLIC VOID Int64LShift (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 B)
#else
PUBLIC VOID Int64LShift (ResultHi, ResultLo, AHi, ALo, B)
    BIT32 *ResultHi, *ResultLo, AHi, ALo, B;
#endif
{
    SHIFTLEFT(ResultHi, ResultLo, AHi, ALo, B);
}
/*}}}*/

/*{{{  Int64RShift */
#ifdef ANSI
PUBLIC VOID Int64RShift (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 AHi, BIT32 ALo, BIT32 B)
#else
PUBLIC VOID Int64RShift (ResultHi, ResultLo, AHi, ALo, B)
    BIT32 *ResultHi, *ResultLo, AHi, ALo, B;
#endif
{
    SHIFTRIGHT(ResultHi, ResultLo, AHi, ALo, B);
}
/*}}}*/
/*}}}*/
