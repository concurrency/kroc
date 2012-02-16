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

static char RcsId[] = "$Id: extr32.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  MulDiv */
#ifdef ANSI
PRIVATE VOID MulDiv (INT Op, INT32 *Xexp, BIT32 *Xfrac, INT32 *Yexp, BIT32 *Yfrac, INT32 *Guard)
#else
PRIVATE VOID MulDiv (Op, Xexp, Xfrac, Yexp, Yfrac, Guard)
    INT Op;
    BIT32 *Xfrac, *Yfrac;
    INT32 *Xexp, *Guard, *Yexp;
#endif
{
    INT32 Places, Carry;

    if (Op == Op_Mul)
    {
        /*{{{   */
        LONGPROD(Xfrac, (BIT32 *) Guard, *Xfrac, *Yfrac, 0);
        
        *Xexp = (*Xexp + *Yexp) + (1 - RealXcess);
        /*}}}*/
    }
    else
    {
        /*{{{   */
        NORMALISE((BIT32 *) &Places, Yfrac, (BIT32 *) &Carry, *Yfrac, 0);
        
        *Yexp = *Yexp - Places;
        
        NORMALISE((BIT32 *) &Places, Xfrac, (BIT32 *) &Carry, *Xfrac, 0);
        LONGDIV(Xfrac, (BIT32 *) &Carry, (BIT32) (*Xfrac >> 1), 0, *Yfrac);
        
        if (Carry == 0)
            *Guard = 0;
        else
            *Guard = 1;
        
        *Xexp = (*Xexp - *Yexp) + (RealXcess - Places);
        /*}}}*/
    }

    NORMALISE((BIT32 *) &Places, Xfrac, (BIT32 *) Guard, *Xfrac, *Guard);

    *Xexp = *Xexp - Places;
    if (*Xexp >= RealExp)
        ;
    else if (*Xexp > 0)
        ;
    else
    {
        /*{{{   */
        Places = 1 - *Xexp;
        
        if (Places > BitsPerWord32)
        {
            /*{{{   */
            *Guard = *Guard | *Xfrac;
            *Xfrac = 0;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            Carry = *Guard;
            
            SHIFTRIGHT(Xfrac, (BIT32 *) Guard, *Xfrac, *Guard, Places);
            
            *Guard = Carry | *Guard;
            /*}}}*/
        }
        *Xexp = 0;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  RndPack */
#ifdef ANSI
PRIVATE VOID RndPack (BOOL *Error, INT32 *Result, INT32 Ans, INT32 Xexp, BIT32 Xfrac, INT RoundMode, INT32 Guard)
#else
PRIVATE VOID RndPack (Error, Result, Ans, Xexp, Xfrac, RoundMode, Guard)
    BOOL *Error;
    INT RoundMode;
    INT32 *Result, Ans, Xexp, Guard;
    BIT32 Xfrac;
#endif
{
    INT32 Places;
    
    if (Xexp < RealExp)
    {
        /*{{{   */
        Places = Xfrac & RealExp;
        
        SHIFTRIGHT((BIT32 *) &Xexp, &Xfrac, Xexp, (BIT32) (Xfrac << 1), (BIT32) (RealShift + 1));
        
        if (RoundMode == RN)
        {
            /*{{{   */
            if ((Places & RealRBit) == 0)
                ;
            else if (((Guard | (Places & RealXcess)) | (Xfrac & 1)) == 0)
                ;
            else
                Xfrac = Xfrac + 1;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            if (((RoundMode == RM) && (Ans == 0)) ||  /* RM and +ve */
                (((RoundMode == RP) && (Ans != 0)) || /* RP and -ve */
                (RoundMode == RZ)))                        /* RZ */
                ;
            else
            {
                /*{{{   */
                if ((Places | Guard) == 0)
                    ;
                else
                    Xfrac = Xfrac + 1;
                /*}}}*/
            }
            /*}}}*/
        }
        
        *Error = (Xfrac == RealInf);
        
        *Result = Ans | Xfrac;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        if (((RoundMode == RM) && (Ans == 0)) ||  /* RM and unrounded +inf */
            (((RoundMode == RP) && (Ans != 0)) || /* RP and unrounded -inf */
            (RoundMode == RZ)))                        /* RZ and unrounded  inf */
        {
            /*{{{   */
            *Result = Ans | BiggestFinite;
            *Error = TRUE;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            *Result = Ans | RealInf;
            *Error = TRUE;
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/
/*}}}*/

/*{{{  global procedures */
/*{{{  Real32Op */
#ifdef ANSI
PUBLIC VOID Real32Op (BOOL *Error, BIT32 *Result, BIT32 X, INT RMOp, BIT32 Yp)
#else
PUBLIC VOID Real32Op (Error, Result, X, RMOp, Yp)
    INT RMOp;
    BOOL *Error;
    BIT32 *Result, X, Yp;
#endif
{
    INT RoundMode, Op;
    BIT32 Xfrac, Yfrac;
    INT32 Xexp, Yexp, Carry, Guard, Places, Ans, Y;
    
    /*{{{  unpack the operands */
    Op = RMOp & OpMask;
    RoundMode = RMOp >> RMShift;
    
    if (Op == Op_Sub)
        Y = Yp ^ SignBit32;
    else
        Y = Yp;
    
    SHIFTLEFT((BIT32 *) &Xexp, &Xfrac, 0, (X << 1), (BIT32) RealShift);
    Xfrac = Xfrac >> 1;
    SHIFTLEFT((BIT32 *) &Yexp, &Yfrac, 0, (Y << 1), (BIT32) RealShift);
    Yfrac = Yfrac >> 1;
    /*}}}*/
    
    /*{{{  determine special case */
    /*{{{  assume not normal case */
    Carry = 0;
    *Error = FALSE;
    /*}}}*/
    
    if ((Xexp != RealExp) && (Yexp != RealExp))
    {
        /*{{{   */
        if ((Xexp != 0) && (Yexp != 0))
        {
            /*{{{   */
            Carry = 1;
            Xfrac = Xfrac | SignBit32;
            Yfrac = Yfrac | SignBit32;
            /*}}}*/
        }
        else if ((Xexp | Xfrac) == 0)
        {
            /*{{{   */
            if ((Yexp | Yfrac) == 0)
            {
                /*{{{   */
                if (Op < Op_Mul)
                {
                    /*{{{   */
                    if (RoundMode == RM)
                        Ans = X | Y;
                    else
                        Ans = X & Y;
                    /*}}}*/
                }
                else if (Op == Op_Mul)
                    Ans = X ^ Y;
                else
                {
                    /*{{{   */
                    Ans = ZeroZeroDivNaN;
                    *Error = TRUE;
                    /*}}}*/
                }
                /*}}}*/
            }
            else if (Op < Op_Mul)
                Ans = Y;
            else
                Ans = (X ^ Y) & SignBit32;
            /*}}}*/
        }
        else if ((Yexp | Yfrac) == 0)
        {
            /*{{{   */
            if (Op < Op_Mul)
                Ans = X;
            else if (Op == Op_Mul)
                Ans = (X ^ Y) & SignBit32;
            else
            {
                /*{{{   */
                Ans = ((X ^ Y) & SignBit32) | RealInf;
                *Error = TRUE;
                /*}}}*/
            }
            /*}}}*/
        }
        else
        {
            /*{{{   */
            Carry = 1;
            
            if (Xexp != 0)
                Xfrac = Xfrac | SignBit32;
            else
                Xexp = 1;
            
            if (Yexp != 0)
                Yfrac = Yfrac | SignBit32;
            else
                Yexp = 1;
            /*}}}*/
        }
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Error = TRUE;
        
        if ((Xexp == RealExp) && (Yexp == RealExp))
        {
            /*{{{   */
            if ((Xfrac | Yfrac) != 0)
            {
                /*{{{   */
                if (Yfrac > Xfrac)
                    Ans = Yp;
                else
                    Ans = X;
                
                if (Op >= Op_Mul)
                    Ans = (Ans & (~SignBit32)) | ((X ^ Y) & SignBit32);
                /*}}}*/
            }
            else if ((Op < Op_Mul) && (((X ^ Y) & SignBit32) == 0))
                Ans = X;
            else if (Op == Op_Mul)
                Ans = ((X ^ Y) & SignBit32) | RealInf;
            else
            {
                /*{{{   */
                if (Op == Op_Div)
                    Ans = InfInfDivNaN;
                else
                    Ans = AddSubInfInfNaN;
                /*}}}*/
            }
            /*}}}*/
        }
        else if (Xexp == RealExp)
        {
            /*{{{   */
            if (Op < Op_Mul)
                Ans = X;
            else if (Xfrac != 0)
                Ans = X ^ (Y & SignBit32);
            else if (Op == Op_Mul)
            {
                /*{{{   */
                if ((Yexp | Yfrac) == 0)
                    Ans = ZeroInfMulNaN;
                else
                    Ans = ((X ^ Y) & SignBit32) | RealInf;
                /*}}}*/
            }
            else
                Ans = ((X ^ Y) & SignBit32) | RealInf;
            /*}}}*/
        }
        else if (Yfrac != 0)
        {
            /*{{{   */
            if (Op < Op_Mul)
                Ans = Yp;
            else
                Ans = Y ^ (X & SignBit32);
            /*}}}*/
        }
        else
        {
            /*{{{   */
            if (Op < Op_Mul)
                Ans = Y;
            else if (Op == Op_Mul)
            {
                /*{{{   */
                if ((Xexp | Xfrac) == 0)
                    Ans = ZeroInfMulNaN;
                else
                    Ans = ((X ^ Y) & SignBit32) | RealInf;
                /*}}}*/
            }
            else
                Ans = (X ^ Y) & SignBit32;
            /*}}}*/
        }
        /*}}}*/
    }
    /*}}}*/
    
    if (Carry == 0)
        *Result = Ans;
    else
    {
        /*{{{   */
        if (Op < Op_Mul)
        {
            /*{{{  add/sub */
            Places = Xexp - Yexp;
            
            if (Places == 0)
                Guard = 0;
            else if (Places > BitsPerWord32)
            {
                /*{{{   */
                Yfrac = 0;
                Guard = 1;
                /*}}}*/
            }
            else if (Places > 0)
                SHIFTRIGHT(&Yfrac, (BIT32 *) &Guard, Yfrac, 0, Places);
            else if (Places < (-BitsPerWord32))
            {
                /*{{{   */
                Xfrac = 0;
                Guard = 1;
                Xexp = Yexp;
                /*}}}*/
            }
            else
            {
                /*{{{   */
                SHIFTRIGHT(&Xfrac, (BIT32 *) &Guard, Xfrac, 0, (BIT32) (-Places));
                Xexp = Yexp;
                /*}}}*/
            }
            
            if (((X ^ Y) & SignBit32) == 0)
            {
                /*{{{   */
                Ans = X & SignBit32;
                
                LONGSUM((BIT32 *) &Carry, &Xfrac, Xfrac, Yfrac, 0);
                
                if (Carry != 0)
                {
                    /*{{{   */
                    Xexp = Xexp + 1;
                    
                    if (Xexp == RealExp)
                        ;
                    else
                    {
                        /*{{{   */
                        SHIFTRIGHT(&Xfrac, (BIT32 *) &Guard, Xfrac, Guard, 1);
                        Xfrac = Xfrac | SignBit32;
                        /*}}}*/
                    }
                    /*}}}*/
                }
                else if (Xexp == 1)
                {
                    /*{{{   */
                    if ((Xfrac & SignBit32) == 0)
                        Xexp = 0;
                    /*}}}*/
                }
                /*}}}*/
            }
            else
            {
                /*{{{   */
                LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Guard, 0, Guard, 0);
                LONGDIFF((BIT32 *) &Places, (BIT32 *) &Ans, Xfrac, Yfrac, Carry);
                
                if (Places == 0)
                {
                    /*{{{   */
                    Xfrac = Ans;
                    Ans = X & SignBit32;
                    /*}}}*/
                }
                else
                {
                    /*{{{   */
                    LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xfrac, Yfrac, Xfrac, Carry);
                    Ans = Y & SignBit32;
                    /*}}}*/
                }
                
                if ((Xfrac & SignBit32) != 0)
                    ;
                else if ((Xfrac | Guard) == 0)
                {
                    /*{{{   */
                    if (RoundMode == RM)
                        Ans = SignBit32;
                    else
                        Ans = 0;
                    
                    Xexp = 0;
                    /*}}}*/
                }
                else if (Xexp > 1)
                {
                    /*{{{   */
                    NORMALISE((BIT32 *) &Places, &Xfrac, (BIT32 *) &Guard, Xfrac, Guard);
                    
                    Xexp = Xexp - Places;
                    if (Xexp > 0)
                        ;
                    else
                    {
                        /*{{{   */
                        SHIFTRIGHT(&Xfrac, (BIT32 *) &Guard, Xfrac, Guard, (BIT32) (1 - Xexp));
                        Xexp = 0;
                        /*}}}*/
                    }
                    /*}}}*/
                }
                else
                    Xexp = 0;
                /*}}}*/
            }
            /*}}}*/
        }
        else
        {
            /*{{{  mul/div */
            Ans = (X ^ Y) & SignBit32;
            
            MulDiv(Op, &Xexp, &Xfrac, &Yexp, &Yfrac, &Guard);
            /*}}}*/
        }
        
        RndPack(Error, (INT32 *) Result, Ans, Xexp, Xfrac, RoundMode, Guard);
        /*}}}*/
    }
}
/*}}}*/

/*{{{  Real32Rem */
#ifdef ANSI
PUBLIC VOID Real32Rem (BOOL *Error, BIT32 *Result, BIT32 X, BIT32 Y)
#else
PUBLIC VOID Real32Rem (Error, Result, X, Y)
    BOOL *Error;
    BIT32 *Result, X, Y;
#endif
{
    BIT32 Xfrac, Yfrac;
    INT32 Xexp, Yexp, Carry, Guard, Places, Ans = 0;
    
    /*{{{  unpack the operands */
    SHIFTLEFT((BIT32 *) &Xexp, &Xfrac, 0, X << 1, (BIT32) RealShift);
    Xfrac = Xfrac >> 1;
    SHIFTLEFT((BIT32 *) &Yexp, &Yfrac, 0, Y << 1, (BIT32) RealShift);
    Yfrac = Yfrac >> 1;
    /*}}}*/

    /*{{{  determine special cases */
    /*{{{  assume not normal case */
    *Error = FALSE;
    Carry = 0;
    /*}}}*/
    
    if ((Xexp != RealExp) && (Yexp != RealExp))
    {
        /*{{{   */
        if ((Xexp != 0) && (Yexp != 0))
        {
            /*{{{   */
            Carry = 1;
            Xfrac = Xfrac | SignBit32;
            Yfrac = Yfrac | SignBit32;
            /*}}}*/
        }
        else if ((Yexp | Yfrac) == 0)
        {
            /*{{{   */
            Ans = RemZeroNaN;
            *Error = TRUE;
            /*}}}*/
        }
        else if ((Xexp | Xfrac) == 0)
            Ans = X;
        else
        {
            /*{{{   */
            Carry = 1;
            
            if (Xexp != 0)
                Xfrac = Xfrac | SignBit32;
            else
                Xexp = 1;
            
            if (Yexp != 0)
                Yfrac = Yfrac | SignBit32;
            else
                Yexp = 1;
            /*}}}*/
        }
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Error = TRUE;
        
        if ((Xexp == RealExp) && (Yexp == RealExp))
        {
            /*{{{   */
            if ((Xfrac | Yfrac) == 0)
                Ans = RemInfNaN;
            else if (Yfrac > Xfrac)
                Ans = Y;
            else
                Ans = (X & (~SignBit32)) | (Y & SignBit32);
            /*}}}*/
        }
        else if (Xexp == RealExp)
        {
            /*{{{   */
            if (Xfrac != 0)
                Ans = (X & (~SignBit32)) | (Y & SignBit32);
            else
                Ans = RemInfNaN;
            /*}}}*/
        }
        else if (Yfrac != 0)
            Ans = Y;
        else
            Ans = X;
        /*}}}*/
    }
    /*}}}*/

    if (Carry == 0)
        *Result = Ans;
    else
    {
        /*{{{   */
        if (Yexp == 1)
        {
            /*{{{   */
            NORMALISE((BIT32 *) &Places, &Yfrac, (BIT32 *) &Guard, Yfrac, 0);
            Yexp = Yexp - Places;
            NORMALISE((BIT32 *) &Places, &Xfrac, (BIT32 *) &Guard, Xfrac, 0);
            Xexp = Xexp - Places;
            /*}}}*/
        }
        
        Ans = X & SignBit32;
        Places = Xexp - Yexp;
        
        if (Places >= 0)
        {
            /*{{{   */
            Carry = BitsPerWord32 - (Places & (BitsPerWord32 - 1));
            
            SHIFTRIGHT(&Xfrac, (BIT32 *) &Guard, Xfrac, 0, Carry);
            LONGDIV((BIT32 *) &Guard, &Xfrac, Xfrac, Guard, Yfrac);
            
            while (Places >= BitsPerWord32)
            {
                /*{{{   */
                LONGDIV((BIT32 *) &Guard, &Xfrac, Xfrac, 0, Yfrac);
                Places = Places - BitsPerWord32;
                /*}}}*/
            }
            
            if (Xfrac == 0)
                Xexp = 0;
            else
            {
                /*{{{   */
                LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xexp, Yfrac >> 1, Xfrac, 0);
                
                if ((Carry != 0) || ((Xexp == 0) && ((Guard & 1) != 0)))
                {
                    /*{{{   */
                    Ans = Ans ^ SignBit32;
                    Xfrac = Yfrac - Xfrac;
                    /*}}}*/
                }
                
                Xexp = Yexp;
                /*}}}*/
            }
            /*}}}*/
        }
        else if (Places == -1)
        {
            /*{{{   */
            LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Guard, Yfrac, Xfrac, 0);
            
            if (Carry == 0)
                ;
            else
            {
                /*{{{   */
                Ans = Ans ^ SignBit32;
                Xfrac = Yfrac - (Xfrac >> 1);
                Xexp = Yexp;
                /*}}}*/
            }
            /*}}}*/
        }
        
        NORMALISE((BIT32 *) &Places, &Xfrac, (BIT32 *) &Guard, Xfrac, 0);
        
        Xexp = Xexp - Places;
        
        if (Xexp <= 0)
        {
            /*{{{   */
            SHIFTRIGHT(&Xfrac, (BIT32 *) &Guard, Xfrac, 0, 1 - Xexp);
            Xexp = 0;
            /*}}}*/
        }
        
        Places = Xfrac & RealExp;
        
        SHIFTRIGHT((BIT32 *) &Xexp, &Xfrac, Xexp, Xfrac << 1, (BIT32) (RealShift + 1));
        
        if ((Places & RealRBit) == 0)
            ;
        else if (((Guard | (Places & RealXcess)) | (Xfrac & 1)) == 0)
            ;
        else
            Xfrac = Xfrac + 1;
        
        *Result = Ans | Xfrac;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  Real32Eq */
#ifdef ANSI
PUBLIC VOID Real32Eq (BOOL *Equal, BIT32 X, BIT32 Y)
#else
PUBLIC VOID Real32Eq (Equal, X, Y)
    BOOL *Equal;
    BIT32 X, Y;
#endif
{
    INT32 MagX, MagY;

    MagX = X & MOSTPOS_INT32;
    MagY = Y & MOSTPOS_INT32;

    *Equal = ((X == Y) || ((MagX | MagY) == 0));
}
/*}}}*/

/*{{{  Real32Gt */
#ifdef ANSI
PUBLIC VOID Real32Gt (BOOL *Greater, BIT32 X, BIT32 Y)
#else
PUBLIC VOID Real32Gt (Greater, X, Y)
    BOOL *Greater;
    BIT32 X, Y;
#endif
{
    INT32 MagX, MagY;

    MagX = X & MOSTPOS_INT32;
    MagY = Y & MOSTPOS_INT32;

    if (((X ^ Y) & SignBit32) != 0)
        *Greater = ((((Y & SignBit32) != 0) && ((MagX | MagY) != 0)));
    else if ((X & SignBit32) != 0)
        *Greater = (MagY > MagX);
    else
        *Greater = (MagX > MagY);
}
/*}}}*/
/*}}}*/
