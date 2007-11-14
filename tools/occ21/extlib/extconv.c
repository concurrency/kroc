/*{{{  module header */

/*
 *	Conversion functions
 *	Copyright (C) 1989 Inmos Limited
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

static char RcsId[] = "$Id: extconv.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  I32ToI64 */
#ifdef ANSI
PUBLIC VOID I32ToI64 (BIT32 *ResultHi, BIT32 *ResultLo, BIT32 A)
#else
PUBLIC VOID I32ToI64 (ResultHi, ResultLo, A)
    BIT32 *ResultHi, *ResultLo, A;
#endif
{
    *ResultLo = A;

    if ((A & SignBit32) != 0)
        *ResultHi = -1;
    else
        *ResultHi = 0;
}
/*}}}*/

/*{{{  I32ToR32 */
#ifdef ANSI
PUBLIC VOID I32ToR32 (BIT32 *X, INT Mode, BIT32 N)
#else
PUBLIC VOID I32ToR32 (X, Mode, N)
    INT Mode;
    BIT32 *X, N;
#endif
{
    if (N == 0)
        *X = 0;
    else
    {
        /*{{{   */
        INT32 Guard, Places, Xexp, Xfrac, Xint, Xsign;
        
        if ((N & SignBit32) != 0)
        {
            /*{{{   */
            Xsign = SignBit32;
            Xint = (N ^ (INT32) (-1)) + 1;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            Xsign = 0;
            Xint = N;
            /*}}}*/
        }
        NORMALISE((BIT32 *) &Places, (BIT32 *) &Xint, (BIT32 *) &Guard, Xint, 0);
        
        Xexp = (RealXcess - 1) + (BitsPerWord32 - Places);
        
        SHIFTRIGHT((BIT32 *) &Xexp, (BIT32 *) &Xfrac, Xexp, Xint << 1, (BIT32) (RealShift + 1));
        
        if (Places > RealShift)
            ;
        else
        {
            /*{{{   */
            Xint = Xint & RealExp;
            if ((Mode == Truncate) || ((Xint & RealRBit) == 0))
                ;
            else if (((Xint & RealXcess) | (Xfrac & 1)) == 0)
                ;
            else
                Xfrac = Xfrac + 1;
            /*}}}*/
        }
        *X = Xsign | Xfrac;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  I32ToR64 */
#ifdef ANSI
PUBLIC VOID I32ToR64 (BIT32 *XHi, BIT32 *XLo, BIT32 N)
#else
PUBLIC VOID I32ToR64 (XHi, XLo, N)
    BIT32 *XHi, *XLo, N;
#endif
{
    if (N == 0)
    {
        /*{{{   */
        *XHi = 0;
        *XLo = 0;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        INT32 Dump, Places, Xexp, Xfrac, Xint, Xsign;
        
        if ((N & SignBit32) != 0)
        {
            /*{{{   */
            Xsign = SignBit32;
            
            if (N == SignBit32)
                Xint = N;
            else
                Xint = -N;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            Xsign = 0;
            Xint = N;
            /*}}}*/
        }
        NORMALISE((BIT32 *) &Places, (BIT32 *) &Xint, (BIT32 *) &Dump, Xint, 0);
        
        Xexp = (DRealXcess - 1) + (BitsPerWord32 - Places);
        Xint = Xint << 1;
        
        SHIFTRIGHT((BIT32 *) &Xexp, (BIT32 *) &Xfrac, Xexp, Xint, (BIT32) (DRealShift + 1));
        SHIFTRIGHT((BIT32 *) &Xexp, XLo, Xint, 0, (BIT32) (DRealShift + 1));
        
        *XHi = Xsign | Xfrac;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  I64ToI32 */
#ifdef ANSI
PUBLIC VOID I64ToI32 (BOOL *Error, BIT32 *Result, BIT32 AHi, BIT32 ALo)
#else
PUBLIC VOID I64ToI32 (Error, Result, AHi, ALo)
    BOOL *Error;
    BIT32 *Result, AHi, ALo;
#endif
{
    INT32 Res;

    Res = ALo;

    if (((Res < 0) && (AHi != -1)) || ((Res >= 0) && (AHi != 0)))
        *Error = TRUE;
    else
        *Error = FALSE;

    *Result = Res;
}
/*}}}*/

/*{{{  I64ToR32 */
#ifdef ANSI
PUBLIC VOID I64ToR32 (BIT32 *Result, INT Mode, BIT32 AHi, BIT32 ALo)
#else
PUBLIC VOID I64ToR32 (Result, Mode, AHi, ALo)
    INT Mode;
    BIT32 *Result, AHi, ALo;
#endif
{
    INT32 B[2], Xfrac, Xexp, Ans, Carry, Places;

    if ((AHi & SignBit32) != 0)
    {
        /*{{{   */
        LONGDIFF((BIT32 *) &Carry, (BIT32 *) &B[0], 0, ALo, 0);
        LONGDIFF((BIT32 *) &Carry, (BIT32 *) &B[1], 0, AHi, Carry);
        Ans = SignBit32;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        B[1] = AHi;
        B[0] = ALo;
        Ans = 0;
        /*}}}*/
    }

    NORMALISE((BIT32 *) &Places, (BIT32 *) &B[1], (BIT32 *) &B[0], B[1], B[0]);

    if (Places == (2 * BitsPerWord32))
        *Result = 0;
    else
    {
        /*{{{   */
        Xexp = (((2 * BitsPerWord32) - 1) - Places) + RealXcess;
        
        Places = B[1] & RealExp;
        
        SHIFTRIGHT((BIT32 *) &Xexp, (BIT32 *) &Xfrac, Xexp, B[1] << 1, (BIT32) (RealShift + 1));
        
        if ((Mode == Truncate) || ((Places & RealRBit) == 0))
            ;
        else if (((B[0] | (Places & RealXcess)) | (Xfrac & 1)) == 0)
            ;
        else
            Xfrac = Xfrac + 1;
        
        *Result = Ans | Xfrac;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  I64ToR64 */
#ifdef ANSI
PUBLIC VOID I64ToR64 (BIT32 *XHi, BIT32 *XLo, INT Mode, BIT32 NHi, BIT32 NLo)
#else
PUBLIC VOID I64ToR64 (XHi, XLo, Mode, NHi, NLo)
    INT Mode;
    BIT32 *XHi, *XLo, NHi, NLo;
#endif
{
    if ((NHi | NLo) == 0)
    {
        /*{{{   */
        *XHi = 0;
        *XLo = 0;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        INT32 Carry, Places, Xexp, Xfrac[2], Xint[2], Xsign;
        
        if ((NHi & SignBit32) != 0)
        {
            /*{{{   */
            Xsign = SignBit32;
            LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xint[0], 0, NLo, 0);
            LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xint[1], 0, NHi, Carry);
            /*}}}*/
        }
        else
        {
            /*{{{   */
            Xsign = 0;
            Xint[1] = NHi;
            Xint[0] = NLo;
            /*}}}*/
        }
        
        NORMALISE((BIT32 *) &Places, (BIT32 *) &Xint[1], (BIT32 *) &Xint[0], Xint[1], Xint[0]);
        
        Xexp = (DRealXcess - 1) + ((2 * BitsPerWord32) - Places);
        
        SHIFTLEFT((BIT32 *) &Xint[1], (BIT32 *) &Xint[0], Xint[1], Xint[0], 1);
        SHIFTRIGHT((BIT32 *) &Xexp, (BIT32 *) &Xfrac[1], Xexp, Xint[1], (BIT32)(DRealShift + 1));
        SHIFTRIGHT((BIT32 *) &Xexp, (BIT32 *) &Xfrac[0], Xint[1], Xint[0], (BIT32) (DRealShift + 1));
        
        if (Places > (BitsPerWord32 + DRealShift))
            ;
        else
        {
            /*{{{   */
            Xint[0] = Xint[0] & DRealExp;
            if ((Mode == Truncate) || ((Xint[0] & DRealRBit) == 0))
                ;
            else if (((Xint[0] & DRealXcess) | (Xfrac[0] & 1)) == 0)
                ;
            else
            {
                /*{{{   */
                LONGSUM((BIT32 *) &Carry, (BIT32 *) &Xfrac[0], Xfrac[0], 1, 0);
                LONGSUM((BIT32 *) &Carry, (BIT32 *) &Xfrac[1], Xfrac[1], 0, Carry);
                /*}}}*/
            }
            /*}}}*/
        }
        
        *XHi = Xsign | Xfrac[1];
        *XLo = Xfrac[0];
        /*}}}*/
    }
}
/*}}}*/

/*{{{  R32ToI32 */
#ifdef ANSI
PUBLIC VOID R32ToI32 (BOOL *Error, BIT32 *N, INT Mode, BIT32 X)
#else
PUBLIC VOID R32ToI32 (Error, N, Mode, X)
    INT Mode;
    BOOL *Error;
    BIT32 *N, X;
#endif
{
    INT32 Guard, Xexp, Xfrac;

    *Error = FALSE;

    SHIFTLEFT((BIT32 *) &Xexp, (BIT32 *) &Xfrac, 0, X & (~SignBit32), (BIT32) (RealShift + 1));

    if (Xexp == RealExp)
        *Error = TRUE;
    else if (Xexp < (RealXcess - 1))
        *N = 0;
    else if (Xexp < RealXcess)
    {
        /*{{{   */
        if ((Mode == Truncate) || (Xfrac == 0))
            *N = 0;
        else if ((X & SignBit32) == 0)
            *N = 1;
        else
            *N = -1;
        /*}}}*/
    }
    else if (X == REAL32MININT32)
        *N = SignBit32;
    else
    {
        /*{{{   */
        Xexp = Xexp - RealXcess;
        
        Guard = 1;
        
        if (Xexp > (BitsPerWord32 - 2))
        {
            /*{{{   */
            *Error = TRUE;
            SHIFTLEFT((BIT32 *) &Guard, (BIT32 *) &Xfrac, Guard, Xfrac, Xexp - BitsPerWord32);
            /*}}}*/
        }
        else
            SHIFTLEFT((BIT32 *) &Guard, (BIT32 *) &Xfrac, Guard, Xfrac, Xexp);
        
        if ((Mode == Truncate) || ((Xfrac & SignBit32) == 0))
            ;
        else if (((Xfrac & (~SignBit32)) | (Guard & 1)) == 0)
            ;
        else
        {
            /*{{{   */
            Guard = Guard + 1;
            
            if ((Guard & SignBit32) == 0)
                ;
            else
                *Error = TRUE;
            /*}}}*/
        }
        
        if ((X & SignBit32) == 0)
            *N = Guard & (~SignBit32);
        else
            *N = -(Guard & (~SignBit32));
        /*}}}*/
    }
}
/*}}}*/

/*{{{  R32ToI64 */
#ifdef ANSI
PUBLIC VOID R32ToI64 (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, INT Mode, BIT32 A)
#else
PUBLIC VOID R32ToI64 (Error, ResultHi, ResultLo, Mode, A)
    INT Mode;
    BOOL *Error;
    BIT32 *ResultHi, *ResultLo, A;
#endif
{
    *Error = FALSE;

    if (A == REAL32MININT64)
    {
        /*{{{   */
        *ResultHi = SignBit32;
        *ResultLo = 0;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        BIT32 B[2], Xfrac, Xexp, Carry;
        
        SHIFTLEFT((BIT32 *) &Xexp, (BIT32 *) &Xfrac, 0, A << 1, (BIT32) RealShift);
        
        if (Xexp < (RealXcess - 1))
        {
            /*{{{   */
            *ResultHi = 0;
            *ResultLo = 0;
            /*}}}*/
        }
        else if (Xexp > (((2 * BitsPerWord32) - 2) + RealXcess))
            *Error = TRUE;
        else
        {
            /*{{{   */
            SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &Xfrac, 1, Xfrac, 1);
            
            if (Xexp < (RealXcess + BitsPerWord32))
            {
                /*{{{   */
                SHIFTLEFT((BIT32 *) &B[0], (BIT32 *) &Xfrac, 0, Xfrac, (1 + Xexp) - RealXcess);
                
                B[1] = 0;
                
                if ((Mode == Truncate) || ((Xfrac & SignBit32) == 0))
                    ;
                else if (((Xfrac & (~SignBit32)) | (B[0] & 1)) == 0)
                    ;
                else
                {
                    /*{{{   */
                    LONGSUM((BIT32 *) &Carry, (BIT32 *) &B[0], 1, B[0], 0);
                    LONGADD(&B[1], B[1], 0, Carry);
                    /*}}}*/
                }
                /*}}}*/
            }
            else
                SHIFTLEFT((BIT32 *) &B[1], (BIT32 *) &B[0], 0, Xfrac, ((1 + Xexp) - RealXcess) - BitsPerWord32);
            
            if ((A & SignBit32) != 0)
            {
                /*{{{   */
                LONGDIFF((BIT32 *) &Carry, ResultLo, 0, B[0], 0);
                LONGDIFF((BIT32 *) &Carry, ResultHi, 0, B[1], Carry);
                /*}}}*/
            }
            else
            {
                /*{{{   */
                *ResultLo = B[0];
                *ResultHi = B[1];
                /*}}}*/
            }
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/

/*{{{  R32ToR64 */
#ifdef ANSI
PUBLIC VOID R32ToR64 (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 X)
#else
PUBLIC VOID R32ToR64 (Error, ResultHi, ResultLo, X)
    BOOL *Error;
    BIT32 *ResultHi, *ResultLo, X;
#endif
{
    BIT32 Xfrac;
    INT32 Yexp, Yfrac[2], Xexp, Places, Carry;

    *Error = FALSE;

    SHIFTLEFT((BIT32 *) &Xexp, &Xfrac, 0, X << 1, (BIT32) RealShift);

    Xfrac = Xfrac >> 1;

    if (Xexp != RealExp)
    {
        /*{{{   */
        if ((Xexp != 0))
        {
            /*{{{   */
            Yfrac[1] = Xfrac | SignBit32;
            Yexp = Xexp + (DRealXcess - RealXcess);
            /*}}}*/
        }
        else if ((Xexp | Xfrac) == 0)
        {
            /*{{{   */
            Yfrac[1] = 0;
            Yexp  = 0;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            NORMALISE((BIT32 *) &Places, (BIT32 *) &Yfrac[1], (BIT32 *) &Yfrac[0], Xfrac, 0);
            
            Yexp = ((DRealXcess - RealXcess) + (1 - Places));
            /*}}}*/
        }
        /*}}}*/
    }
    else
    {
        /*{{{   */
        Yexp = DRealExp;
        Yfrac[1] = Xfrac;
        
        *Error = TRUE;
        /*}}}*/
    }

    SHIFTRIGHT((BIT32 *) &Yfrac[1], (BIT32 *) &Yfrac[0], Yfrac[1] & (~SignBit32), 0, (BIT32) DRealShift);
    SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &Yexp, Yexp, 0, (BIT32) (DRealShift + 1));

    Yfrac[1] = Yfrac[1] | Yexp;

    *ResultHi = (Yfrac[1] | (X & SignBit32));
    *ResultLo = Yfrac[0];
}
/*}}}*/

/*{{{  R64ToI32 */
#ifdef ANSI
PUBLIC VOID R64ToI32 (BOOL *Error, BIT32 *N, INT Mode, BIT32 XHi, BIT32 XLo)
#else
PUBLIC VOID R64ToI32 (Error, N, Mode, XHi, XLo)
    INT Mode;
    BOOL *Error;
    BIT32 *N, XHi, XLo;
#endif
{
    INT32 Dump, Guard, Xexp, Xfrac[2];

    *Error = FALSE;

    SHIFTLEFT((BIT32 *) &Xexp, (BIT32 *) &Dump, 0, XHi & (~SignBit32), (BIT32) (DRealShift + 1));
    SHIFTLEFT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], XHi, XLo, (BIT32) (DRealShift + 1));

    if (Xexp == DRealExp)
        *Error = TRUE;
    else if ((XHi == REAL64MININT32Hi) && ((Xfrac[1] == 0) || ((Mode == Truncate) && (Xfrac[1] == 1))))
        *N = SignBit32;
    else if (Xexp < (DRealXcess - 1))
        *N = 0;
    else if (Xexp < DRealXcess)
    {
        /*{{{   */
        if ((Mode == Truncate) || ((Xfrac[1] | Xfrac[0]) == 0))
            *N = 0;
        else if ((XHi & SignBit32) == 0)
            *N = 1;
        else
            *N = -1;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        Xexp = Xexp - DRealXcess;
        
        Guard = 1;
        
        if (Xexp > (BitsPerWord32 - 2))
            *Error = TRUE;
        
        SHIFTLEFT((BIT32 *) &Guard, (BIT32 *) &Dump, Guard, Xfrac[1], Xexp);
        SHIFTLEFT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0], Xexp);
        
        if ((Mode == Truncate) || ((Xfrac[1] & SignBit32) == 0))
            ;
        else if ((((Xfrac[1] & (~SignBit32)) | Xfrac[0]) | (Guard & 1)) == 0)
            ;
        else
        {
            /*{{{   */
            LONGSUM((BIT32 *) &Dump, (BIT32 *) &Guard, Guard, 1, 0);
            
            if (Guard < 0)
                *Error = TRUE;
            /*}}}*/
        }
        
        if ((XHi & SignBit32) == 0)
            ;
        else
            Guard = -Guard;
        
        *N = Guard;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  R64ToI64 */
#ifdef ANSI
PUBLIC VOID R64ToI64 (BOOL *Error, BIT32 *NHi, BIT32 *NLo, INT Mode, BIT32 XHi, BIT32 XLo)
#else
PUBLIC VOID R64ToI64 (Error, NHi, NLo, Mode, XHi, XLo)
    INT Mode;
    BOOL *Error;
    BIT32 *NHi, *NLo, XHi, XLo;
#endif
{
    INT32 Carry, Dump, Guard, Xexp, Xfrac[2], Xsign;

    *Error = FALSE;

    SHIFTLEFT((BIT32 *) &Xexp, (BIT32 *) &Dump, 0, XHi & (~SignBit32), (BIT32) (DRealShift + 1));
    SHIFTLEFT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], XHi, XLo, (BIT32) (DRealShift + 1));

    if (Xexp == DRealExp)
        *Error = TRUE;
    else if ((XHi == REAL64MININT64Hi) && (XLo == 0))
    {
        /*{{{   */
        *NHi = SignBit32;
        *NLo = 0;
        /*}}}*/
    }
    else if (Xexp < (DRealXcess - 1))
    {
        /*{{{   */
        *NHi = 0;
        *NLo = 0;
        /*}}}*/
    }
    else if (Xexp < DRealXcess)
    {
        /*{{{   */
        if ((Mode == Truncate) || ((Xfrac[1] | Xfrac[0]) == 0))
        {
            /*{{{   */
            *NHi = 0;
            *NLo = 0;
            /*}}}*/
        }
        else if ((XHi & SignBit32) == 0)
        {
            /*{{{   */
            *NHi = 0;
            *NLo = 1;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            *NHi = -1;
            *NLo = -1;
            /*}}}*/
        }
        /*}}}*/
    }
    else
    {
        /*{{{   */
        Xsign = XHi & SignBit32;
        Xexp = Xexp - DRealXcess;
        
        if (Xexp < BitsPerWord32)
        {
            /*{{{   */
            Carry = Xfrac[0];
            Xfrac[0] = Xfrac[1];
            Xfrac[1] = 1;
            Guard = 0;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            Xexp = Xexp - BitsPerWord32;
            Carry = 0;
            Guard = 1;
            /*}}}*/
        }
        
        if (Xexp > (BitsPerWord32 - 2))
            *Error = TRUE;
        
        SHIFTLEFT((BIT32 *) &Guard, (BIT32 *) &Dump, Guard, Xfrac[1], Xexp);
        SHIFTLEFT((BIT32 *) &Xfrac[1], (BIT32 *) &Dump, Xfrac[1], Xfrac[0], Xexp);
        SHIFTLEFT((BIT32 *) &Xfrac[0], (BIT32 *) &Carry, Xfrac[0], Carry, Xexp);
        
        if ((Mode == Truncate) || ((Xfrac[0] & SignBit32) == 0))
            ;
        else if ((((Xfrac[0] & (~SignBit32)) | Carry) | (Xfrac[1] & 1)) == 0) 
            ;
        else
        {
            /*{{{   */
            LONGSUM((BIT32 *) &Carry, (BIT32 *) &Xfrac[1], Xfrac[1], 1, 0);
            LONGSUM((BIT32 *) &Carry, (BIT32 *) &Guard, Guard, 0, Carry);
            /*}}}*/
        }
        
        *NHi = Guard & (~SignBit32);
        *NLo = Xfrac[1];
        
        if (Xsign == 0)
            ;
        else
        {
            /*{{{   */
            LONGDIFF((BIT32 *) &Carry, NLo, 0, *NLo, 0);
            LONGDIFF((BIT32 *) &Carry, NHi, 0, *NHi, Carry);
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/

/*{{{  R64ToR32 */
#ifdef ANSI
PUBLIC VOID R64ToR32 (BOOL *Error, BIT32 *Result, INT Mode, BIT32 XHi, BIT32 XLo)
#else
PUBLIC VOID R64ToR32 (Error, Result, Mode, XHi, XLo)
    INT Mode;
    BOOL *Error;
    BIT32 *Result, XHi, XLo;
#endif
{
    INT32 Xfrac[2], Xexp, Yfrac, Yexp, Places, Guard;

    *Error = FALSE;
    
    SHIFTLEFT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], XHi, XLo, (BIT32) DRealShift);

    Xexp = (XHi >> DRealExpSh) & DRealExp;

    if (Xexp != DRealExp)
    {
        /*{{{   */
        if (Xexp != 0)
        {
            /*{{{   */
            Yfrac = Xfrac[1] | SignBit32;
            Guard = Xfrac[0];
            Yexp = Xexp - (DRealXcess - RealXcess);
            
            if (Yexp >= RealExp)
            {
                /*{{{   */
                Yexp = RealExp;
                Yfrac = SignBit32;
                *Error = TRUE;
                /*}}}*/
            }
            else if (Yexp > 0)
                ;
            else if (Yexp > (1 - BitsPerWord32))
            {
                /*{{{   */
                Places = Guard;
                
                SHIFTRIGHT((BIT32 *) &Yfrac, (BIT32 *) &Guard, Yfrac, Guard, 1 - Yexp);
                
                Guard = Places | Guard;
                Yexp = 0;
                /*}}}*/
            }
            else
            {
                /*{{{   */
                Guard = Guard | Yfrac;
                Yfrac = 0;
                /*}}}*/
            }
            /*}}}*/
        }
        else
        {
            /*{{{   */
            Yexp = 0;
            Yfrac = 0;
            Guard = Xfrac[1] | Xfrac[0];
            /*}}}*/
        }
        /*}}}*/
    }
    else
    {
        /*{{{   */
        Xfrac[1] = Xfrac[1] & (~SignBit32);
        Yexp = RealExp;
        Yfrac = Xfrac[1] & (~(RealXcess | RealRBit));
        Guard = 0;
        
        if ((Yfrac == 0) && ((Xfrac[1] | Xfrac[0]) != 0))
            Yfrac = Real64to32NaNfrac;
        
        *Error = TRUE;
        /*}}}*/
    }

    if (Yexp < RealExp)
    {
        /*{{{   */
        Places = Yfrac & RealExp;
        
        SHIFTRIGHT((BIT32 *) &Yexp, (BIT32 *) &Yfrac, Yexp, Yfrac << 1, (BIT32) (RealShift + 1));
        
        if ((Mode == Truncate) || ((Places & RealRBit) == 0))
            ;
        else if (((Guard | (Places & RealXcess)) | (Yfrac & 1)) == 0)
            ;
        else
        {
            /*{{{   */
            Yfrac = Yfrac + 1;
            
            if (Yfrac == RealInf)
                *Error = TRUE;
            /*}}}*/
        }
        /*}}}*/
    }
    else
    {
        /*{{{   */
        SHIFTRIGHT((BIT32 *) &Yexp, (BIT32 *) &Yfrac, Yexp, Yfrac << 1, (BIT32) (RealShift + 1));
        *Error = TRUE;
        /*}}}*/
    }

    *Result = (XHi & SignBit32) | Yfrac;
}
/*}}}*/
/*}}}*/
