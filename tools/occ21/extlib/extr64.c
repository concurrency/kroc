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

static char RcsId[] = "$Id: extr64.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  Specials */
#ifdef ANSI
PRIVATE VOID Specials (INT32 *Carry, BOOL *Error, INT RoundMode, INT Op, INT32 Ans[], INT32 Xfrac[], INT32 Yfrac[], INT32 *Xexp, INT32 *Yexp, BIT32 YpHi, BIT32 YpLo, BIT32 XHi, BIT32 XLo, INT32 Y[])
#else
PRIVATE VOID Specials (Carry, Error, RoundMode, Op, Ans, Xfrac, Yfrac, Xexp, Yexp, YpHi, YpLo, XHi, XLo, Y)
    BOOL *Error;
    INT RoundMode, Op;
    BIT32 XHi, XLo, YpHi, YpLo;
    INT32 *Carry, Ans[], Xfrac[], Yfrac[], *Xexp, *Yexp, Y[];
#endif
{
    INT32 Places, Guard;
    
    Ans[0] = 0;
    
    /*{{{  assume not normal case */
    *Error = FALSE;
    *Carry = 0;
    /*}}}*/

    if ((*Xexp != DRealExp) && (*Yexp != DRealExp))
    {
        /*{{{    */
        if ((*Xexp != 0) && (*Yexp != 0))
        {
            /*{{{  normalised case */
            *Carry = 1;
            
            Xfrac[1] = Xfrac[1] | SignBit32;
            Yfrac[1] = Yfrac[1] | SignBit32;
            /*}}}*/
        }
        else if (((*Xexp | Xfrac[1]) | Xfrac[0]) == 0)
        {
            /*{{{  X is zero */
            if (((*Yexp | Yfrac[1]) | Yfrac[0]) == 0)
            {
                /*{{{   */
                if (Op < Op_Mul)
                {
                    /*{{{   */
                    if (RoundMode == RM)
                    {
                        /*{{{   */
                        Ans[0] = 0;
                        Ans[1] = XHi | Y[1];
                        /*}}}*/
                    }
                    else
                    {
                        /*{{{   */
                        Ans[0] = 0;
                        Ans[1] = XHi & Y[1];
                        /*}}}*/
                    }
                    /*}}}*/
                }
                else
                {
                    /*{{{   */
                    if (Op == Op_Mul)
                        Ans[1] = XHi ^ Y[1];
                    else
                    {
                        /*{{{   */
                        *Error = TRUE;
                        
                        Ans[1] = DZeroZeroDivNaN;
                        Ans[0] = 0;
                        /*}}}*/
                    }
                    /*}}}*/
                }
                /*}}}*/
            }
            else
            {
                /*{{{   */
                if (Op < Op_Mul)
                {
                    /*{{{   */
                    Ans[1] = Y[1];
                    Ans[0] = Y[0];
                    /*}}}*/
                }
                else
                    Ans[1] = (XHi ^ Y[1]) & SignBit32;
                /*}}}*/
            }
            /*}}}*/
        }
        else if (((*Yexp | Yfrac[1]) | Yfrac[0]) == 0)
        {
            /*{{{  Y is zero */
            if (Op < Op_Mul)
            {
                /*{{{   */
                Ans[1] = XHi;
                Ans[0] = XLo;
                /*}}}*/
            }
            else
            {
                /*{{{   */
                if (Op == Op_Mul)
                    Ans[1] = (XHi ^ Y[1]) & SignBit32;
                else
                {
                    /*{{{   */
                    *Error = TRUE;
                    
                    Ans[1] = ((XHi ^ Y[1]) & SignBit32) | DRealInf;
                    /*}}}*/
                }
                /*}}}*/
            }
            /*}}}*/
        }
        else
        {
            /*{{{  denormalised case */
            /*
            Put in implicit MSB, normalise any denormalised values and correct the
            exponents if the operation is multiply or divide leave unnormalised for
            add and subtract.
            */
            
            *Carry = 1;
            
            if (*Xexp != 0)
                Xfrac[1] = Xfrac[1] | SignBit32;
            else
            {
                /*{{{   */
                if (Op > Op_Sub)
                {
                    /*{{{   */
                    NORMALISE((BIT32 *) &Places, (BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0]);
                    *Xexp = 1 - Places;
                    /*}}}*/
                }
                else
                    *Xexp = 1;
                /*}}}*/
            }
            
            if (*Yexp != 0)
                Yfrac[1] = Yfrac[1] | SignBit32;
            else
            {
                /*{{{   */
                if (Op > Op_Sub)
                {
                    /*{{{   */
                    NORMALISE((BIT32 *) &Places, (BIT32 *) &Yfrac[1], (BIT32 *) &Yfrac[0], Yfrac[1], Yfrac[0]);
                    *Yexp = 1 - Places;
                    /*}}}*/
                }
                else
                    *Yexp = 1;
                /*}}}*/
            }
            /*}}}*/
        }
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Error = TRUE;
        
        /*{{{  clear top sign bits */
        Xfrac[1] = Xfrac[1] & (~SignBit32);
        Yfrac[1] = Yfrac[1] & (~SignBit32);
        /*}}}*/
        
        if ((*Xexp == DRealExp) && (*Yexp == DRealExp))
        {
            /*{{{  both inf/NaN */
            if ((((Xfrac[1] | Xfrac[0]) | Yfrac[1]) | Yfrac[0]) != 0)
            {
                /*{{{   */
                LONGDIFF((BIT32 *) &Guard, (BIT32 *) &Places, Xfrac[0], Yfrac[0], 0);
                LONGDIFF((BIT32 *) &Guard, (BIT32 *) &Places, Xfrac[1], Yfrac[1], Guard);
                
                if (Places < 0)
                {
                    /*{{{   */
                    Ans[1] = YpHi;
                    Ans[0] = YpLo;
                    /*}}}*/
                }
                else
                {
                    /*{{{   */
                    Ans[1] = XHi;
                    Ans[0] = XLo;
                    /*}}}*/
                }
                
                if (Op < Op_Mul)
                    ;
                else
                    Ans[1] = (Ans[1] & (~SignBit32)) | ((XHi ^ Y[1]) & SignBit32);
                /*}}}*/
            }
            else if ((Op < Op_Mul) && (((XHi ^ Y[1]) & SignBit32) == 0))
                Ans[1] = XHi;
            else if (Op == Op_Mul)
                Ans[1] = ((XHi ^ Y[1]) & SignBit32) | DRealInf;
            else
            {
                /*{{{   */
                if (Op == Op_Div)
                {
                    /*{{{   */
                    Ans[1] = DInfInfDivNaN;
                    Ans[0] = 0;
                    /*}}}*/
                }
                else
                {
                    /*{{{   */
                    Ans[1] = DAddSubInfInfNaN;
                    Ans[0] = 0;
                    /*}}}*/
                }
                /*}}}*/
            }
            /*}}}*/
        }
        else if (*Xexp == DRealExp)
        {
            /*{{{  X is inf/NaN */
            if (Op < Op_Mul)
            {
                /*{{{   */
                Ans[1] = XHi;
                Ans[0] = XLo;
                /*}}}*/
            }
            else if ((Xfrac[1] | Xfrac[0]) != 0)
            {
                /*{{{   */
                Ans[0] = XLo;
                Ans[1] = XHi ^ (Y[1] & SignBit32);
                /*}}}*/
            }
            else if (Op == Op_Mul)
            {
                /*{{{   */
                if (((*Yexp | Yfrac[1]) | Yfrac[0]) == 0)
                {
                    /*{{{   */
                    Ans[1] = DZeroInfMulNaN;
                    Ans[0] = 0;
                    /*}}}*/
                }
                else
                    Ans[1] = ((XHi ^ Y[1]) & SignBit32) | DRealInf;
                /*}}}*/
            }
            else
                Ans[1] = ((XHi ^ Y[1]) & SignBit32) | DRealInf;
            /*}}}*/
        }
        else if ((Yfrac[1] | Yfrac[0]) != 0)
        {
            /*{{{  Y is NaN */
            if (Op < Op_Mul)
            {
                /*{{{   */
                Ans[1] = YpHi;
                Ans[0] = YpLo;
                /*}}}*/
            }
            else
            {
                /*{{{   */
                Ans[1] = Y[1] ^ (XHi & SignBit32);
                Ans[0] = Y[0];
                /*}}}*/
            }
            /*}}}*/
        }
        else
        {
            /*{{{  Y is inf */
            if (Op < Op_Mul)
            {
                /*{{{   */
                Ans[1] = Y[1];
                Ans[0] = Y[0];
                /*}}}*/
            }
            else if (Op == Op_Mul)
            {
                /*{{{   */
                if (((*Xexp | Xfrac[1]) | Xfrac[0]) == 0)
                {
                    /*{{{   */
                    Ans[1] = DZeroInfMulNaN;
                    Ans[0] = 0;
                    /*}}}*/
                }
                else
                    Ans[1] = ((XHi ^ Y[1]) & SignBit32) | DRealInf;
                /*}}}*/
            }
            else
                Ans[1] = (XHi ^ Y[1]) & SignBit32;
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/

/*{{{  MulDiv64 */
#ifdef ANSI
PRIVATE VOID MulDiv64 (INT Op, INT32 *Xexp, BIT32 Xfrac[], INT32 *Yexp, BIT32 Yfrac[], INT32 *Guard)
#else
PRIVATE VOID MulDiv64 (Op, Xexp, Xfrac, Yexp, Yfrac, Guard)
    INT Op;
    BIT32 Xfrac[], Yfrac[];
    INT32 *Xexp, *Guard, *Yexp;
#endif
{
    INT32 Places, Carry;

    if (Op == Op_Mul)
    {
        /*{{{   */
        *Xexp = (*Xexp + *Yexp) + (1 - DRealXcess);
        
        RealIMul((INT32 *) Xfrac, Guard, Yfrac[1], Yfrac[0]);
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Xexp = (*Xexp - *Yexp) + DRealXcess;
        
        SHIFTRIGHT(&Xfrac[1], &Xfrac[0], Xfrac[1], Xfrac[0], 1);
        
        *Guard = 0;
        
        RealIDiv((INT32 *) &Xfrac[1], (INT32 *) &Xfrac[1], (INT32 *) &Xfrac[0], Guard, (INT32 *) Yfrac);
        
        Carry = 0;
        
        RealIDiv((INT32 *) &Xfrac[0], (INT32 *) &Xfrac[0], Guard, &Carry, (INT32 *) Yfrac);
        
        if (Carry)
            *Guard = *Guard | 1;
        /*}}}*/
    }

    NORMALISE((BIT32 *) &Places, &Xfrac[1], (BIT32 *) &Carry, Xfrac[1], Xfrac[0]);
    
    if (Places > BitsPerWord32)
        Xfrac[1] = Xfrac[1] | (*Guard >> ((2 * BitsPerWord32) - Places));
    
    SHIFTLEFT(&Xfrac[0], (BIT32 *) Guard, Xfrac[0], *Guard, Places);
    
    *Xexp = *Xexp - Places;

    /*{{{  correct after normalisation */
    if (*Xexp >= DRealExp)
        *Xexp = DRealExp;
    else if (*Xexp > 0)
        ;
    else if (*Xexp < (-(DBitsInFrac - 1)))
    {
        /*{{{   */
        Xfrac[0] = 0;
        Xfrac[1] = 0;
        
        *Guard = 1;
        *Xexp = 0;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        /*
        Must pick up guard bits before any shifting because they will fall off the end.
        */
        
        Places = (Xfrac[0] & (DRealRBit | DRealXcess)) | *Guard;
        
        if (*Xexp < (-(BitsPerWord32 - 2)))
        {
            /*{{{   */
            Places = Places | Xfrac[0];
            
            SHIFTRIGHT(&Xfrac[0], (BIT32 *) Guard, Xfrac[1], Xfrac[0], (-(*Xexp)) - (BitsPerWord32 - 1));
            
            Xfrac[1] = 0;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) Guard, Xfrac[0], *Guard, 1 - *Xexp);
            SHIFTRIGHT(&Xfrac[1], &Xfrac[0], Xfrac[1], Xfrac[0], 1 - *Xexp);
            /*}}}*/
        }
        *Guard = *Guard | Places;
        
        *Xexp = 0;
        /*}}}*/
    }
    /*}}}*/
}
/*}}}*/

/*{{{  RndPack64 */
#ifdef ANSI
PRIVATE VOID RndPack64 (BOOL *Error, INT32 *ResultHi, INT32 *ResultLo, INT32 Ans[2], INT32 Xexp, BIT32 Xfrac[2], INT RoundMode, INT32 Guard)
#else
PRIVATE VOID RndPack64 (Error, ResultHi, ResultLo, Ans, Xexp, Xfrac, RoundMode, Guard)
    BOOL *Error;
    INT RoundMode;
    BIT32 Xfrac[2];
    INT32 *ResultHi, *ResultLo, Ans[2], Xexp, Guard;
#endif
{
    INT32 Places, Carry;

    if (Xexp < DRealExp)
    {
        /*{{{   */
        Places = Xfrac[0];
        
        SHIFTRIGHT(&Xfrac[1], &Xfrac[0], Xfrac[1] & (~SignBit32), Xfrac[0], (BIT32) DRealShift);
        SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &Xexp, Xexp, 0, (BIT32) (DRealShift + 1));
        
        Xfrac[1] = Xfrac[1] | Xexp;
        
        if (RoundMode == RN)
        {
            /*{{{   */
            if ((Places & DRealRBit) == 0)
                ;
            else
            {
                /*{{{   */
                if (((Guard | (Places & DRealXcess)) | (Xfrac[0] & 1)) == 0)
                    ;
                else
                {
                    /*{{{   */
                    LONGSUM((BIT32 *) &Carry, &Xfrac[0], Xfrac[0], 1, 0);
                    LONGSUM((BIT32 *) &Carry, &Xfrac[1], Xfrac[1], 0, Carry);
                    /*}}}*/
                }
                /*}}}*/
            }
            /*}}}*/
        }
        else
        {
            /*{{{   */
            if (((RoundMode == RM) && (Ans[1] == 0)) || /* RM and +ve */
               (((RoundMode == RP) && (Ans[1] != 0)) || /* RP and -ve */
               (RoundMode == RZ)))                           /* RZ */
                ;
            else
            {
                /*{{{   */
                if (((Places & DRealExp) | Guard) == 0)
                    ;
                else
                {
                    /*{{{   */
                    LONGSUM((BIT32 *) &Carry, &Xfrac[0], Xfrac[0], 1, 0);
                    LONGSUM((BIT32 *) &Carry, &Xfrac[1], Xfrac[1], 0, Carry);
                    /*}}}*/
                }
                /*}}}*/
            }
            /*}}}*/
        }
        
        *ResultHi = Ans[1] | Xfrac[1];
        *ResultLo = Xfrac[0];
        
        *Error = (Xfrac[1] == DRealInf);
        /*}}}*/
    }
    else
    {
        /*{{{   */
        if (((RoundMode == RM) && (Ans[1] == 0)) || /* RM and unrounded +inf */
           (((RoundMode == RP) && (Ans[1] != 0)) || /* RP and unrounded -inf */
           (RoundMode == RZ)))                           /* RZ and unrounded  inf */
        {
            /*{{{   */
            *Error = TRUE;
            
            *ResultHi = Ans[1] | DBiggestFiniteHi;
            *ResultLo = DBiggestFiniteLo;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            *Error = TRUE;
            
            *ResultHi = Ans[1] | DRealInf;
            *ResultLo = 0;
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/
/*}}}*/

/*{{{  global procedures */
/*{{{  Real64Op */
#ifdef ANSI
PUBLIC VOID Real64Op (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 XHi, BIT32 XLo, INT RMOp, BIT32 YpHi, BIT32 YpLo)
#else
PUBLIC VOID Real64Op (Error, ResultHi, ResultLo, XHi, XLo, RMOp, YpHi, YpLo)
    INT RMOp;
    BOOL *Error;
    BIT32 *ResultHi, *ResultLo, XHi, XLo, YpHi, YpLo;
#endif
{
    INT Op, RoundMode;
    INT32 Xexp, Yexp, Xfrac[2], Yfrac[2], Carry, Guard, Places, Ans[2], Y[2];

    Op = RMOp & OpMask;
    RoundMode = RMOp >> RMShift;

    /*{{{  unpack operand */
    Y[0] = YpLo;
    if (Op == Op_Sub)
        Y[1] = YpHi ^ SignBit32;
    else
        Y[1] = YpHi;
    
    SHIFTLEFT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], XHi, XLo, (BIT32) DRealShift);
    SHIFTLEFT((BIT32 *) &Yfrac[1], (BIT32 *) &Yfrac[0], Y[1], YpLo, (BIT32) DRealShift);
    
    Xexp = (XHi >> DRealExpSh) & DRealExp;
    Yexp = (Y[1] >> DRealExpSh) & DRealExp;
    /*}}}*/

    Specials(&Carry, Error, RoundMode, Op, Ans, Xfrac, Yfrac, &Xexp, &Yexp, YpHi, YpLo, XHi, XLo, Y);

    if (Carry == 0)
    {
        /*{{{   */
        *ResultHi = Ans[1];
        *ResultLo = Ans[0];
        /*}}}*/
    }
    else
    {
        /*{{{   */
        if (Op < Op_Mul)
        {
            /*{{{   */
            /*{{{  add / subtract */
            Places = Xexp - Yexp;
            
            if (Places == 0)
                Guard = 0;
            else if (Places > 0)
            {
                /*{{{   */
                if (Places > ((3 * BitsPerWord32) - (DBitsInFrac + 1)))
                {
                    /*{{{   */
                    if (Places > (DBitsInFrac + 1))
                    {
                        /*{{{   */
                        Yfrac[1] = 0;
                        Yfrac[0] = 0;
                        Guard = 1;
                        /*}}}*/
                    }
                    else
                    {
                        /*{{{   */
                        /*
                        Some of the bits will be shifted out of the register triplet and so
                        need to be ORed into the guard/sticky register it is safe to use the
                        whole of the lower part of the fraction and OR it if for a particular
                        length machine, the above conditional is true and 2*BitsPerWord >
                        DBitsInFrac ie 2*16 > 24 and 2*32 > 52.
                        */
                        
                        Carry = Yfrac[0];
                        
                        SHIFTRIGHT((BIT32 *) &Yfrac[0], (BIT32 *) &Guard, Yfrac[1], Yfrac[0], Places - BitsPerWord32);
                        
                        Guard = Guard | Carry;
                        Yfrac[1] = 0;
                        /*}}}*/
                    }
                    /*}}}*/
                }
                else
                {
                    /*{{{   */
                    SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &Guard, Yfrac[0], 0, Places);
                    
                    if (Places > BitsPerWord32)
                    {
                        /*{{{   */
                        SHIFTRIGHT((BIT32 *) &Yexp, (BIT32 *) &Carry, Yfrac[1], 0, Places - BitsPerWord32);
                        
                        Guard = Guard | Carry;
                        /*}}}*/
                    }
                    
                    SHIFTRIGHT((BIT32 *) &Yfrac[1], (BIT32 *) &Yfrac[0], Yfrac[1], Yfrac[0], Places);
                    /*}}}*/
                }    
                /*}}}*/
            }
            else
            {
                /*{{{   */
                if (Places < (-((3 * BitsPerWord32) - (DBitsInFrac + 1))))
                {
                    /*{{{   */
                    if (Places < (-(DBitsInFrac + 1)))
                    {
                        /*{{{   */
                        Xfrac[1] = 0;
                        Xfrac[0] = 0;
                        
                        Guard = 1;
                        Xexp = Yexp;
                        /*}}}*/
                    }
                    else
                    {
                        /*{{{   */
                        Carry = Xfrac[0];
                        
                        SHIFTRIGHT((BIT32 *) &Xfrac[0], (BIT32 *) &Guard, Xfrac[1], Carry, (-Places) - BitsPerWord32);
                        
                        Guard = Guard | Carry;
                        
                        Xfrac[1] = 0;
                        Xexp = Yexp;
                        /*}}}*/
                    }
                    /*}}}*/
                }
                else
                {
                    /*{{{   */
                    Xexp = Yexp;
                    
                    SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &Guard, Xfrac[0], 0, -Places);
                    
                    if (Places < (-BitsPerWord32))
                    {
                      /*{{{   */
                      SHIFTRIGHT((BIT32 *) &Yexp, (BIT32 *) &Carry, Xfrac[1], 0, (-Places) - BitsPerWord32);
                      
                      Guard = Guard | Carry;
                      /*}}}*/
                    }
                    
                    SHIFTRIGHT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0], -Places);
                    /*}}}*/
                }
                /*}}}*/
            }        
            /*}}}*/
            
            if (((XHi ^ Y[1]) & SignBit32) == 0)
            {
                /*{{{   */
                Ans[1] = XHi & SignBit32;
                
                LONGSUM((BIT32 *) &Carry, (BIT32 *) &Xfrac[0], Xfrac[0], Yfrac[0], 0);
                LONGSUM((BIT32 *) &Carry, (BIT32 *) &Xfrac[1], Xfrac[1], Yfrac[1], Carry);
                
                if (Carry != 0)
                {
                    /*{{{   */
                    Xexp = Xexp + 1;
                    SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &Guard, Xfrac[0], Guard, 1);
                    SHIFTRIGHT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0], 1);
                    /*}}}*/
                }
                else if (Xexp == 1)
                {
                    /*{{{   */
                    if ((Xfrac[1] & SignBit32) == 0)
                        Xexp = 0;
                    /*}}}*/
                }
                /*}}}*/
            }
            else
            {
                /*{{{   */
                LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Guard, 0, Guard, 0);
                LONGDIFF((BIT32 *) &Places, (BIT32 *) &Ans[0], Xfrac[0], Yfrac[0], Carry);
                LONGDIFF((BIT32 *) &Places, (BIT32 *) &Ans[1], Xfrac[1], Yfrac[1], Places);
                
                if (Places == 0)
                {
                    /*{{{   */
                    Xfrac[1] = Ans[1];
                    Xfrac[0] = Ans[0];
                    Ans[1] = XHi & SignBit32;
                    /*}}}*/
                }
                else
                {
                    /*{{{   */
                    Ans[1] = Y[1] & SignBit32;
                    
                    LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xfrac[0], Yfrac[0], Xfrac[0], Carry);
                    LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xfrac[1], Yfrac[1], Xfrac[1], Carry);
                    /*}}}*/
                }
                
                if ((Xfrac[1] & SignBit32) != 0)
                    ;
                else
                {
                    /*{{{   */
                    if (((Xfrac[1] | Xfrac[0]) | Guard) == 0)
                    {
                        /*{{{   */
                        if (RoundMode == RM)
                        {
                            /*{{{   */
                            Xexp = 0;
                            Ans[1] = SignBit32;
                            /*}}}*/
                        }
                        else
                        {
                            /*{{{   */
                            Xexp = 0;
                            Ans[1] = 0;
                            /*}}}*/
                        }
                        /*}}}*/
                    }
                    else if (Xexp > 1)
                    {
                        /*{{{   */
                        NORMALISE((BIT32 *) &Places, (BIT32 *) &Xfrac[1], (BIT32 *) &Carry, Xfrac[1], Xfrac[0]);
                        Xexp = Xexp - Places;
                        
                        if (Xexp > 0)
                            SHIFTLEFT((BIT32 *) &Xfrac[0], (BIT32 *) &Guard, Xfrac[0], Guard, Places);
                        else
                        {
                            /*{{{   */
                            SHIFTRIGHT((BIT32 *) &Xfrac[1], (BIT32 *) &Carry, Xfrac[1], 0, 1 - Xexp);
                            SHIFTLEFT((BIT32 *) &Xfrac[0], (BIT32 *) &Guard, Xfrac[0], Guard, Places-(1-Xexp));
                            
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
            /*}}}*/
        }
        else
        {
            /*{{{   */
            Ans[1] = (XHi ^ Y[1]) & SignBit32;
            
            MulDiv64(Op, &Xexp, (BIT32 *) Xfrac, &Yexp, (BIT32 *) Yfrac, &Guard);
            /*}}}*/
        }
        
        RndPack64(Error, (INT32 *) ResultHi, (INT32 *) ResultLo, Ans, Xexp, (BIT32 *) Xfrac, RoundMode, Guard);
        /*}}}*/
    }
}
/*}}}*/

/*{{{  Real64Rem */
#ifdef ANSI
PUBLIC VOID Real64Rem (BOOL *Error, BIT32 *ResultHi, BIT32 *ResultLo, BIT32 XHi, BIT32 XLo, BIT32 YHi, BIT32 YLo)
#else
PUBLIC VOID Real64Rem (Error, ResultHi, ResultLo, XHi, XLo, YHi, YLo)
    BOOL *Error;
    BIT32 *ResultHi, *ResultLo, XHi, XLo, YHi, YLo;
#endif
{
    INT32 Xexp, Yexp, Xfrac[2], Yfrac[2], Carry, Guard, Places, Ans[2];

    /*{{{  unpack operand */
    SHIFTLEFT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], XHi, XLo, (BIT32) DRealShift);
    SHIFTLEFT((BIT32 *) &Yfrac[1], (BIT32 *) &Yfrac[0], YHi, YLo, (BIT32) DRealShift);
    
    Xexp = (XHi >> DRealExpSh) & DRealExp;
    Yexp = (YHi >> DRealExpSh) & DRealExp;
    
    Ans[0] = 0;
    
    /*{{{  assume not normal case */
    Carry = 0;
    *Error = FALSE;
    /*}}}*/
    
    if ((Xexp != DRealExp) && (Yexp != DRealExp))
    {
        /*{{{   */
        if ((Xexp != 0) && (Yexp != 0))
        {
            /*{{{   */
            Carry = 1;
            Xfrac[1] = Xfrac[1] | SignBit32;
            Yfrac[1] = Yfrac[1] | SignBit32;
            /*}}}*/
        }
        else if (((Yexp | Yfrac[1]) | Yfrac[0]) == 0)
        {
            /*{{{   */
            *Error = TRUE;
            Ans[1] = DRemZeroNaN;
            Ans[0] = 0;
            /*}}}*/
        }
        else if (((Xexp | Xfrac[1]) | Xfrac[0]) == 0)
        {
            /*{{{   */
            Ans[0] = XLo;
            Ans[1] = XHi;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            /*
            Put in implicit MSB, normalise any denormalised values and correct
            their exponents if the operation is multiply, divide or rem leave
            unnormalised for add and subtract.
            */
            
            Carry = 1;
            
            if (Xexp != 0)
                Xfrac[1] = Xfrac[1] | SignBit32;
            else
            {
                /*{{{   */
                NORMALISE((BIT32 *) &Places, (BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0]);
                Xexp = 1 - Places;
                /*}}}*/
            }
            
            if (Yexp != 0)
                Yfrac[1] = Yfrac[1] | SignBit32;
            else
            {
                /*{{{   */
                NORMALISE((BIT32 *) &Places, (BIT32 *) &Yfrac[1], (BIT32 *) &Yfrac[0], Yfrac[1], Yfrac[0]);
                Yexp = 1 - Places;
                /*}}}*/
            }
            /*}}}*/
        }
        /*}}}*/
    }
    else
    {
        /*{{{   */
        /*{{{  clear top sign bits */
        Xfrac[1] = Xfrac[1] & (~SignBit32);
        Yfrac[1] = Yfrac[1] & (~SignBit32);
        /*}}}*/
        
        *Error = TRUE;
        
        if ((Xexp == DRealExp) && (Yexp == DRealExp))
        {
            /*{{{  both inf/NaN */
            if (((Xfrac[1] | Xfrac[0]) | (Yfrac[1] | Yfrac[0])) == 0)
            {
                /*{{{   */
                Ans[1] = DRemInfNaN;
                Ans[0] = 0;
                /*}}}*/
            }
            else
            {
                /*{{{   */
                LONGDIFF((BIT32 *) &Guard, (BIT32 *) &Places, Xfrac[0], Yfrac[0], 0);
                LONGDIFF((BIT32 *) &Guard, (BIT32 *) &Places, Xfrac[1], Yfrac[1], Guard);
                
                if (Places < 0)
                {
                    /*{{{   */
                    Ans[0] = YLo;
                    Ans[1] = YHi;
                    /*}}}*/
                }
                else
                {
                    /*{{{   */
                    Ans[0] = XLo;
                    Ans[1] = (XHi & (~SignBit32)) | (YHi & SignBit32);
                    /*}}}*/
                }
                /*}}}*/
            }
            /*}}}*/
        }
        else if (Xexp == DRealExp)
        {
            /*{{{  X is inf/NaN */
            if ((Xfrac[1] | Xfrac[0]) != 0)
            {
                /*{{{   */
                Ans[1] = (XHi & (~SignBit32)) | (YHi & SignBit32);
                Ans[0] = XLo;
                /*}}}*/
            }
            else
            {
                /*{{{   */
                Ans[1] = DRemInfNaN;
                Ans[0] = 0;
                /*}}}*/
            }
            /*}}}*/
        }
        else if ((Yfrac[1] | Yfrac[0]) != 0)
        {
            /*{{{  Y is NaN */
            Ans[0] = YLo;
            Ans[1] = YHi;
            /*}}}*/
        }
        else
        {
            /*{{{  Y is inf */
            Ans[0] = XLo;
            Ans[1] = XHi;
            /*}}}*/
        }
        /*}}}*/
    }
    /*}}}*/

    if (Carry == 0)
    {
        /*{{{   */
        *ResultHi = Ans[1];
        *ResultLo = Ans[0];
        /*}}}*/
    }
    else
    {
        /*{{{   */
        Ans[1] = XHi & SignBit32;
        Places = Xexp - Yexp;
        
        if (Places >= 0)
        {
            /*{{{  general case */
            Carry = Places & (BitsPerWord32 - 1);
            
            SHIFTLEFT((BIT32 *) &Guard, (BIT32 *) &Xexp, 0, Xfrac[1], Carry);
            SHIFTLEFT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0], Carry);
            
            RealIDiv(&Guard, &Guard, &Xfrac[1], &Xfrac[0], Yfrac);
            
            Places = Places - Carry;
            while (Places > 0)
            {
                /*{{{   */
                Guard = Xfrac[1];
                Xfrac[1] = Xfrac[0];
                Xfrac[0] = 0;
                
                LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xexp, Xfrac[1], Yfrac[1], 0);
                
                if ((Guard == 0) && (Carry != 0))
                    ;
                else
                    RealIDiv(&Guard, &Guard, &Xfrac[1], &Xfrac[0], Yfrac);
                
                Places = Places - BitsPerWord32;
                /*}}}*/
            }
            
            if ((Xfrac[1] | Xfrac[0]) == 0)
                Xexp = 0;
            else
            {
                /*{{{   */
                SHIFTRIGHT((BIT32 *) &Ans[1], (BIT32 *) &Ans[0], Yfrac[1], Yfrac[0], 1);
                LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Ans[0], Ans[0], Xfrac[0], 0);
                LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Ans[1], Ans[1], Xfrac[1], Carry);
                
                if ((Carry != 0) || (((Ans[1] | Ans[0]) == 0) && ((Guard & 1) != 0)))
                {
                    /*{{{   */
                    Ans[1] = (XHi ^ SignBit32) & SignBit32;
                    
                    LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xfrac[0], Yfrac[0], Xfrac[0], 0);
                    LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xfrac[1], Yfrac[1], Xfrac[1], Carry);
                    /*}}}*/
                }
                else
                    Ans[1] = XHi & SignBit32;
                
                Xexp = Yexp;
                /*}}}*/
            }
            /*}}}*/
        }
        else if (Places == -1)
        {
            /*{{{  half to nearly one */
            LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Guard, Yfrac[0], Xfrac[0], 0);
            LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Guard, Yfrac[1], Xfrac[1], Carry);
            
            if (Carry == 0)
                ;
            else
            {
                /*{{{   */
                Ans[1] = Ans[1] ^ SignBit32;
                
                SHIFTRIGHT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0], 1);
                LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xfrac[0], Yfrac[0], Xfrac[0], 0);
                LONGDIFF((BIT32 *) &Carry, (BIT32 *) &Xfrac[1], Yfrac[1], Xfrac[1], Carry);
                
                Xexp = Yexp;
                /*}}}*/
            }
            /*}}}*/
        }
        
        /*{{{  less than half */
        Guard = 0;
        
        NORMALISE((BIT32 *) &Places, (BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0]);
        
        Xexp = Xexp - Places;
        
        if (Xexp <= 0)
        {
            /*{{{   */
            SHIFTRIGHT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0], 1 - Xexp);
            
            Xexp = 0;
            /*}}}*/
        }
        /*}}}*/
        
        /*{{{  round and pack result */
        Places = Xfrac[0];
        
        SHIFTRIGHT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1] & (~SignBit32), Xfrac[0], (BIT32) DRealShift);
        SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &Xexp, Xexp, 0, (BIT32) (DRealShift + 1));
        
        Xfrac[1] = Xfrac[1] | Xexp;
        
        if ((Places & DRealRBit) == 0)
            ;
        else if (((Guard | (Places & DRealXcess)) | (Xfrac[0] & 1)) == 0)
            ;
        else
        {
            /*{{{   */
            LONGSUM((BIT32 *) &Carry, (BIT32 *) &Xfrac[0], Xfrac[0], 1, 0);
            LONGSUM((BIT32 *) &Carry, (BIT32 *) &Xfrac[1], Xfrac[1], 0, Carry);
            /*}}}*/
        }
        
        *ResultHi = Ans[1] | Xfrac[1];
        *ResultLo = Xfrac[0];
        /*}}}*/
        /*}}}*/
    }
}
/*}}}*/

/*{{{  Real64Eq */
#ifdef ANSI
PUBLIC VOID Real64Eq (BOOL *Equal, BIT32 XHi, BIT32 XLo, BIT32 YHi, BIT32 YLo)
#else
PUBLIC VOID Real64Eq (Equal, XHi, XLo, YHi, YLo)
    BOOL *Equal;
    BIT32 XHi, XLo, YHi, YLo;
#endif
{
    INT32 MagX, MagY;

    MagX = XHi & MOSTPOS_INT32;
    MagY = YHi & MOSTPOS_INT32;

    *Equal = (((XHi == YHi) && (XLo == YLo)) || ((((MagX + MagY) | XLo) | YLo) == 0));
}
/*}}}*/

/*{{{  Real64Gt */
#ifdef ANSI
PUBLIC VOID Real64Gt (BOOL *Greater, BIT32 XHi, BIT32 XLo, BIT32 YHi, BIT32 YLo)
#else
PUBLIC VOID Real64Gt (Greater, XHi, XLo, YHi, YLo)
    BOOL *Greater;
    BIT32 XHi, XLo, YHi, YLo;
#endif
{
    INT32 MagX, MagY;

    MagX = XHi & MOSTPOS_INT32;
    MagY = YHi & MOSTPOS_INT32;

    if (((XHi ^ YHi) & SignBit32) != 0)
        *Greater = (((YHi & SignBit32) != 0) && ((((MagX + MagY) | XLo) | YLo) != 0));
    else if ((XHi & SignBit32) != 0)
    {
        /*{{{   */
        if (MagY == MagX)
            *Greater = ((INT32) (YLo ^ SignBit32) > (INT32) (XLo ^ SignBit32));
        else
            *Greater = (MagY > MagX);
        /*}}}*/
    }
    else
    {
        /*{{{   */
        if (MagX == MagY)
            *Greater = ((INT32) (XLo ^ SignBit32) > (INT32) (YLo ^ SignBit32));
        else
            *Greater = (MagX > MagY);
        /*}}}*/
    }
}
/*}}}*/
/*}}}*/
