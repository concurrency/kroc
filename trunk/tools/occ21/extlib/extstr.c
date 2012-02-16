/*{{{  module header */

/*
 *	Exponential arithmetic
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

static char RcsId[] = "$Id: extstr.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  ValidDigit */
#ifdef ANSI
PRIVATE INT ValidDigit (BOOL *Valid, INT Ch)
#else
PRIVATE INT ValidDigit (Valid, Ch)
    INT Ch;
    BOOL *Valid;
#endif
{
    if (isdigit(Ch))
    {
        /*{{{   */
        *Valid = TRUE;
        return(Ch - '0');
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Valid = FALSE;
        return(Ch);
        /*}}}*/
    }
}
/*}}}*/

/*{{{  ValidHexDigit */
#ifdef ANSI
PRIVATE INT ValidHexDigit (BOOL *Valid, INT Ch)
#else
PRIVATE INT ValidHexDigit (Valid, Ch)
    INT Ch;
    BOOL *Valid;
#endif
{
    if (isxdigit(Ch))
    {
        /*{{{   */
        *Valid = TRUE;
        
        if (isdigit(Ch))
            return(Ch - '0');
        else if (isupper(Ch))
            return((Ch - 'A') + 10);
        else
            return((Ch - 'a') + 10);
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Valid = FALSE;
        return(Ch);
        /*}}}*/
    }
}
/*}}}*/

/*{{{  ValidOctDigit */
#ifdef ANSI
PRIVATE INT ValidOctDigit (BOOL *Valid, INT Ch)
#else
PRIVATE INT ValidOctDigit (Valid, Ch)
    INT Ch;
    BOOL *Valid;
#endif
{
    if (('0' <= Ch) && (Ch <= '7'))
    {
        /*{{{   */
        *Valid = TRUE;
        return(Ch - '0');
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Valid = FALSE;
        return(Ch);
        /*}}}*/
    }
}
/*}}}*/

/*{{{  GetChar */
#ifdef ANSI
PRIVATE INT GetChar (VAL CHAR *String, INT StringSize, INT *CharsScanned)
#else
PRIVATE INT GetChar (String, StringSize, CharsScanned)
    CHAR *String;
    INT StringSize, *CharsScanned;
#endif
{
    if ((*CharsScanned >= StringSize) || (String[*CharsScanned] == '\0'))
    {
        /*{{{   */
        (*CharsScanned)++;
        return(' ');
        /*}}}*/
    }
    else
        return(String[(*CharsScanned)++]);
}
/*}}}*/

/*{{{  DScaleX */
#define Bias10_32  4
#define Table10_32 27

#ifdef ANSI
PRIVATE VOID DScaleX (INT32 *Xexp, INT32 *Xfrac, INT32 Scale)
#else
PRIVATE VOID DScaleX (Xexp, Xfrac, Scale)
    INT32 *Xexp, *Xfrac, Scale;
#endif
{
    /*{{{  constant table definitions */
    PRIVATE INT32 PowerTenFrac[] =
    {
        /*{{{   */
        0X00000000, 0X80000000, 0X00000000, 0XA0000000,
        0X00000000, 0XC8000000, 0X00000000, 0XFA000000,
        0X00000000, 0X9C400000, 0X00000000, 0XC3500000,
        0X00000000, 0XF4240000, 0X00000000, 0X98968000,
        0X00000000, 0XBEBC2000, 0X00000000, 0XEE6B2800,
        0X00000000, 0X9502F900, 0X00000000, 0XBA43B740,
        0X00000000, 0XE8D4A510, 0X00000000, 0X9184E72A,
        0X80000000, 0XB5E620F4, 0XA0000000, 0XE35FA931,
        0X04000000, 0X8E1BC9BF, 0XC5000000, 0XB1A2BC2E,
        0X76400000, 0XDE0B6B3A, 0X89E80000, 0X8AC72304,
        0XAC620000, 0XAD78EBC5, 0X177A8000, 0XD8D726B7,
        0X6EAC9000, 0X87867832, 0X0A57B400, 0XA968163F,
        0XCCEDA100, 0XD3C21BCE, 0X401484A0, 0X84595161,
        0X9019A5C8, 0XA56FA5B9, 0XF4200F3A, 0XCECB8F27
        /*}}}*/
    };
    
    PRIVATE INT32 PowerTenExp[] =
    {
        /*{{{   */
         0,  3,  6,  9, 13, 16, 19, 23, 26, 29, 33, 36, 39, 43,
        46, 49, 53, 56, 59, 63, 66, 69, 73, 76, 79, 83, 86, 89
        /*}}}*/
    };
    
    PRIVATE INT32 BiasTenFrac[] =
    {
        /*{{{   */
                 0,          0, 0XF4200F3A, 0XCECB8F27, 0XCFE20766,
        0XD0CF4B50, 0X1A708DEA, 0XDA01EE64, 0XBD203E41, 0X9F79A169
        /*}}}*/
    };
    
    PRIVATE INT32 BiasTenExp[] = {0, 89, 182, 358, 684};
    
    PRIVATE INT32 BiasTen[] = {0, 27, 55, 108, 206};
    /*}}}*/

    INT32 Carry, Guard, N, Places, S, Zexp, Zfrac[2];

    if (Scale < 0)
        S = -Scale;
    else
        S = Scale;

    Zexp = 0;
    Zfrac[1] = SignBit32;
    Zfrac[0] = 0;

    N = Bias10_32;
    while (N >= 0)
    {
        /*{{{   */
        if ((N > 0) && (S < BiasTen[N]))
            N = N - 1;
        else
        {
            /*{{{   */
            if ((N == 0) && (S <= Table10_32))
            {
                /*{{{   */
                RealIMul(Zfrac, &Guard, PowerTenFrac[(S + S) + 1], PowerTenFrac[S + S]);
                Zexp = (Zexp + PowerTenExp[S]) + 1;
                /*}}}*/
            }
            else
            {
                /*{{{   */
                RealIMul(Zfrac, &Guard, BiasTenFrac[(N + N) + 1], BiasTenFrac[N + N]);
                S = S - BiasTen[N];
                Zexp = (Zexp + BiasTenExp[N]) + 1;
                /*}}}*/
            }
            
            NORMALISE((BIT32 *) &Places, (BIT32 *) &Zfrac[1], (BIT32 *) &Carry, Zfrac[1], Zfrac[0]);
            SHIFTLEFT((BIT32 *) &Zfrac[0], (BIT32 *) &Guard, Zfrac[0], Guard, Places);
            
            Zexp = Zexp - Places;
            
            if ((Guard & SignBit32) == 0)
                ;
            else
            {
                /*{{{   */
                LONGSUM((BIT32 *) &Carry, (BIT32 *) &Zfrac[0], Zfrac[0], 1, 0);
                LONGSUM((BIT32 *) &Carry, (BIT32 *) &Zfrac[1], Zfrac[1], 0, Carry);
                
                if (Carry != 0)
                {
                    /*{{{   */
                    Zexp = Zexp + 1;
                    Zfrac[1] = SignBit32;
                    /*}}}*/
                }
                /*}}}*/
            }
            N = N - 1;
            /*}}}*/
        }
        /*}}}*/
    }

    if (S > Table10_32)
    {
        /*{{{   */
        if (Scale > 0)
            *Xexp = DRealExp;
        else
            *Xexp = -DRealExp;
        
        Xfrac[1] = SignBit32;
        /*}}}*/
    }
    else if (Scale < 0)
    {
        /*{{{   */
        *Xexp = *Xexp - Zexp;
        
        SHIFTRIGHT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0], 1);
        
        Guard = 0;
        Carry = 0;
        
        RealIDiv(&Xfrac[1], &Xfrac[1], &Xfrac[0], &Guard, Zfrac);
        RealIDiv(&Xfrac[0], &Xfrac[0], &Guard, &Carry, Zfrac);
        
        SHIFTRIGHT((BIT32 *) &Zfrac[1], (BIT32 *) &Zfrac[0], Zfrac[1], Zfrac[0], 1);
        LONGDIFF((BIT32 *) &Carry, (BIT32 *) &N, Carry, Zfrac[0], 0);
        LONGDIFF((BIT32 *) &Carry, (BIT32 *) &N, Guard, Zfrac[1], Carry);
        
        if (Carry == 0)
            Guard = SignBit32;
        else
            Guard = 0;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Xexp = (*Xexp + Zexp) + 1;
        RealIMul(Xfrac, &Guard, Zfrac[1], Zfrac[0]);
        /*}}}*/
    }

    NORMALISE((BIT32 *) &Places, (BIT32 *) &Xfrac[1], (BIT32 *) &Carry, Xfrac[1], Xfrac[0]);
    SHIFTLEFT((BIT32 *) &Xfrac[0], (BIT32 *) &Guard, Xfrac[0], Guard, Places);

    *Xexp = *Xexp - Places;

    if ((Guard & SignBit32) == 0)
        ;
    else
    {
        /*{{{   */
        LONGSUM((BIT32 *) &Carry, (BIT32 *) &Xfrac[0], Xfrac[0], 1, 0);
        LONGSUM((BIT32 *) &Carry, (BIT32 *) &Xfrac[1], Xfrac[1], 0, Carry);
        
        if (Carry)
            *Xexp = *Xexp + 1;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  ScaleX */
#define Bias10   3
#define Table10 13

#ifdef ANSI
PRIVATE VOID ScaleX (INT32 *Xexp, INT32 *Xfrac, INT32 Scale)
#else
PRIVATE VOID ScaleX (Xexp, Xfrac, Scale)
    INT32 *Xexp, *Xfrac, Scale;
#endif
{
    /*{{{  constant table definitions */
    PRIVATE INT32 PowerTenFrac[] =
    {
        /*{{{   */
        0X80000000, 0XA0000000,
        0XC8000000, 0XFA000000,
        0X9C400000, 0XC3500000,
        0XF4240000, 0X98968000,
        0XBEBC2000, 0XEE6B2800, 
        0X9502F900, 0XBA43B740,
        0XE8D4A510, 0X9184E72A
        /*}}}*/
    };
    
    PRIVATE INT32 PowerTenExp[] =
    {
        /*{{{   */
        0, 3, 6, 9, 13, 16, 19, 23, 26, 29, 33, 36, 39, 43
        /*}}}*/
    };
    
    PRIVATE INT32 BiasTenFrac[] = {0X9184E72A, 0XCECB8F28, 0XEB194F8E};
    
    PRIVATE INT32 BiasTenExp[] = {43, 89, 132};
    
    PRIVATE INT32 BiasTen[] = {14, 28, 41, 54};
    /*}}}*/
    
    INT32 Carry, N, Places, Sb, St, Zexp, Zfrac;
    
    if (Scale < 0)
        St = -Scale;
    else
        St = Scale;

    if (St <= Table10)
    {
        /*{{{   */
        Zexp = PowerTenExp[St];
        Zfrac = PowerTenFrac[St];
        /*}}}*/
    }
    else if (St == (BiasTen[1] - 1))
    {
        /*{{{   */
        Zexp = BiasTenExp[1];
        Zfrac = BiasTenFrac[1];
        /*}}}*/
    }
    else
    {
        /*{{{   */
        INT Index;
        
        Sb = -1;
        
        for (Index = 0; Index < Bias10; Index++)
        {
            /*{{{   */
            if (St < BiasTen[Index + 1])
            {
                /*{{{   */
                Sb = Index;
                break;
                /*}}}*/
            }
            /*}}}*/
        }
        
        if (Sb < 0)
            Zexp = -1;
        else
        {
            /*{{{   */
            St = (St - BiasTen[Sb]) + 1;
            Zexp = (PowerTenExp[St] + BiasTenExp[Sb]) + 1;
            
            LONGPROD((BIT32 *) &Zfrac, (BIT32 *) &Carry, PowerTenFrac[St], BiasTenFrac[Sb], 0);
            NORMALISE((BIT32 *) &Places, (BIT32 *) &Zfrac, (BIT32 *) &Carry, Zfrac, Carry);
            
            Zexp = Zexp - Places;
            /*}}}*/
        }
        /*}}}*/
    }

    if (Zexp < 0)
    {
        /*{{{   */
        if (Scale < 0)
            *Xexp = -RealExp;
        else
            *Xexp = RealExp;
        
        *Xfrac = SignBit32;
        /*}}}*/
    }
    else if (Scale < 0)
    {
        /*{{{   */
        *Xexp = *Xexp - Zexp;
        
        LONGDIV((BIT32 *) Xfrac, (BIT32 *) &Carry, (*Xfrac >> 1) & MOSTPOS_INT32, 0, Zfrac);
        LONGDIFF((BIT32 *) &Carry, (BIT32 *) &N, Carry, (Zfrac >> 1) & MOSTPOS_INT32, 0);
        
        if (Carry != 0)
            Carry = 0;
        else
            Carry = SignBit32;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Xexp = (*Xexp + Zexp) + 1;
        LONGPROD((BIT32 *) Xfrac, (BIT32 *) &Carry, *Xfrac, Zfrac, 0);
        /*}}}*/
    }
    
    NORMALISE((BIT32 *) &Places, (BIT32 *) Xfrac, (BIT32 *) &Carry, *Xfrac, Carry);

    *Xexp = *Xexp - Places;
    
    if (Carry & SignBit32)
    {
        /*{{{   */
        LONGSUM((BIT32 *) &Carry, (BIT32 *) Xfrac, *Xfrac, 1, 0);
        
        if (Carry != 0)
        {
           /*{{{   */
           *Xexp = *Xexp + 1;
           *Xfrac = SignBit32;
           /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/
/*}}}*/

/*{{{  global procedures */
/*{{{  RealIMul */
#ifdef ANSI
PUBLIC VOID RealIMul (INT32 X[2], INT32 *Guard, INT32 Hi, INT32 Lo)
#else
PUBLIC VOID RealIMul (X, Guard, Hi, Lo)
    INT32 X[2], *Guard, Hi, Lo;
#endif
{
    BIT32 Carry, D[4];

    LONGPROD(&Carry, &D[0], X[0], Lo, 0);
    LONGPROD(&Carry, &D[1], X[0], Hi, Carry);
    LONGPROD(&D[3], &D[2], X[1], Lo, 0);
    LONGPROD((BIT32 *) &X[1], (BIT32 *) &X[0], X[1], Hi, Carry);
    LONGSUM(&Carry, (BIT32 *) Guard, D[2], D[1], 0);
    LONGSUM(&Carry, (BIT32 *) &X[0], X[0], D[3], Carry);
    LONGSUM(&Carry, (BIT32 *) &X[1], X[1], 0, Carry);

    if (D[0] == 0)
        ;
    else
        *Guard = *Guard | 1;
}
/*}}}*/

/*{{{  RealIDiv */
#ifdef ANSI
PUBLIC VOID RealIDiv (INT32 *Quot, INT32 *Hi, INT32 *Lo, INT32 *Guard, INT32 Yfrac[2])
#else
PUBLIC VOID RealIDiv (Quot, Hi, Lo, Guard, Yfrac)
    INT32 *Quot, *Hi, *Lo, *Guard, Yfrac[2];
#endif
{
  BIT32 Carry, Q, W[3];

    if (Yfrac[1] == *Hi)
        Q = MaxUnsignedInt32;
    else
        LONGDIV(&Q, &Carry, *Hi, *Lo, Yfrac[1]);

    if (Q != 0)
    {
        /*{{{   */
        LONGPROD(&W[1], &W[0], Q, Yfrac[0], 0);
        LONGPROD(&W[2], &W[1], Q, Yfrac[1], W[1]);
        LONGDIFF(&Carry, (BIT32 *) Guard, *Guard, W[0], 0);
        LONGDIFF(&Carry, (BIT32 *) Lo, *Lo, W[1], Carry);
        LONGDIFF(&Carry, (BIT32 *) Hi, *Hi, W[2], Carry);
        
        while ((*Hi & SignBit32) != 0)
        {
            /*{{{   */
            LONGDIFF(&Carry, &Q, Q, 1, 0);
            LONGSUM(&Carry, (BIT32 *) Guard, *Guard, Yfrac[0], 0);
            LONGSUM(&Carry, (BIT32 *) Lo, *Lo, Yfrac[1], Carry);
            LONGSUM(&Carry, (BIT32 *) Hi, *Hi, 0, Carry);
            /*}}}*/
        }
        /*}}}*/
    }
    *Quot = Q;
}
/*}}}*/

/*{{{  StrToR64 */
#ifdef ANSI
PUBLIC VOID StrToR64 (BOOL *Error, BIT32 *Xhi, BIT32 *Xlo, VAL CHAR *String)
#else
PUBLIC VOID StrToR64 (Error, Xhi, Xlo, String)
    BOOL *Error;
    CHAR *String;
    BIT32 *Xhi, *Xlo;
#endif
{
    BOOL More, Neg;
    INT Ch, StringSize, Index, CharsScanned;
    INT32 Carry, Guard, Lost, M[2], N[2], Xfrac[2], Scale, Xexp;

    Neg = FALSE;
    *Error = FALSE;
    CharsScanned = 0;

    StringSize = strlen(String);
    Ch = GetChar (String, StringSize, &CharsScanned);

    if ((Ch == '-') || (Ch == '+'))
    {
        /*{{{   */
        Neg = (Ch == '-');
        Ch = GetChar(String, StringSize, &CharsScanned);
        /*}}}*/
    }

    Scale = 0;
    N[0] = 0;
    N[1] = 0;
    M[0] = 0;
    M[1] = 0;
    Carry = 0;
    Lost = 0;
    
    Ch = ValidDigit(&More, Ch);
    for (Index = -1; Index < 1; Index++)
    {
      /*{{{   */
      if ((Index == 0) && (Ch == '.'))
      {
          /*{{{   */
          Ch = GetChar(String, StringSize, &CharsScanned);
          Ch = ValidDigit(&More, Ch);
          /*}}}*/
      }
      
      *Error = *Error || (! More);
      
      while (More)
      {
          /*{{{   */
          if (Index == 0)
              Scale = Scale - 1;
          
          if (Carry == 0)
          {
              /*{{{   */
              RealIMul(M, &Guard, 10, 0);
              if (M[1] == 0)
              {
                  /*{{{   */
                  LONGSUM((BIT32 *) &Carry, (BIT32 *) &Guard, Guard, (BIT32) Ch, 0);
                  LONGSUM((BIT32 *) &M[1], (BIT32 *) &M[0], M[0], 0, Carry);
                  /*}}}*/
              }
              
              Carry = M[1];
              if (Carry == 0)
              {
                  /*{{{   */
                  M[1] = M[0];
                  M[0] = Guard;
                  N[1] = M[1];
                  N[0] = M[0];
                  /*}}}*/
              }
              else
              {
                  /*{{{   */
                  Lost = Lost | Ch;
                  Scale = Scale + 1;
                  /*}}}*/
              }
              /*}}}*/
          }
          else
          {
              /*{{{   */
              Lost = Lost | Ch;
              Scale = Scale + 1;
              /*}}}*/
          }
          
          Ch = GetChar(String, StringSize, &CharsScanned);
          Ch = ValidDigit(&More, Ch);
          /*}}}*/
      }
      /*}}}*/
    }
    
    if (Ch == 'E')
    {
        /*{{{   */
        Guard = 0;
        
        Ch = GetChar(String, StringSize, &CharsScanned);
        if ((Ch == '+') || (Ch == '-'))
        {
            /*{{{   */
            if (Ch == '-')
                Guard = 1;
            
            Ch = GetChar(String, StringSize, &CharsScanned);
            /*}}}*/
        }
        else
            *Error = TRUE;
        
        Xexp = 0;
        Ch = ValidDigit(&More, Ch);
        *Error = *Error || (! More);
        
        while (More)
        {
            /*{{{   */
            if (IntegerLimit32 >= Xexp)
            {
                /*{{{   */
                Xexp = (Xexp * 10) + Ch;
                Ch = GetChar(String, StringSize, &CharsScanned);
                Ch = ValidDigit(&More, Ch);
                /*}}}*/
            }
            else
            {
                /*{{{   */
                INT StillChecking = TRUE;
                
                More = FALSE;
                Scale = 0;
                Xexp = MOSTPOS_INT32;
                
                while (StillChecking)
                {
                    /*{{{   */
                    Ch = GetChar(String, StringSize, &CharsScanned);
                    Ch = ValidDigit(&StillChecking, Ch);
                    /*}}}*/
                }
                /*}}}*/
            }
            /*}}}*/
        }
        
        if (Guard == 0)
            Scale = Scale + Xexp;
        else
            Scale = Scale - Xexp;
        /*}}}*/
    }
    
    Xexp = 0;
    if ((N[1] | N[0]) == 0)
        ;
    else
    {
        /*{{{   */
        NORMALISE((BIT32 *) &Carry, (BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], N[1], N[0]);
        
        Xexp = Xexp + ((BitsPerWord32 * 2) - (Carry + 1));
        
        if (Scale != 0)
            DScaleX(&Xexp, Xfrac, Scale);
        Xexp = Xexp + DRealXcess;
        
        if (Xexp >= DRealExp)
        {
            /*{{{   */
            N[1] = DRealInf;
            N[0] = 0;
            
            /* NOTE: remove this line if overflow is OK */
            
            *Error = TRUE;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            if (Xexp <= 0)
            {
                /*{{{   */
                SHIFTRIGHT((BIT32 *) &Xfrac[1], (BIT32 *) &Xfrac[0], Xfrac[1], Xfrac[0], 1 - Xexp);
                SHIFTRIGHT((BIT32 *) &Guard, (BIT32 *) &Carry, Xfrac[0], 0, 1 - Xexp);
                
                Xexp = 0;
                Lost = Lost | Carry;
                
                /* NOTE: remove this line if underflow is OK */
                
                *Error = TRUE;
                /*}}}*/
            }
            
            if (Lost)
                Xfrac[0] = Xfrac[0] | 1;
            
            SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &N[0], Xfrac[1], Xfrac[0], (BIT32) DRealShift);
            SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &N[1], Xexp, Xfrac[1] << 1, (BIT32) (DRealShift + 1));
            
            if ((Xfrac[0] & DRealRBit) == 0)
                ;
            else if ((Xfrac[0] & ((DRealRBit << 1) | DRealXcess)) == 0)
                ;
            else
            {
                /*{{{   */
                LONGSUM((BIT32 *) &Carry, (BIT32 *) &N[0], N[0], 1, 0);
                LONGSUM((BIT32 *) &Carry, (BIT32 *) &N[1], N[1], 0, Carry);
                /*}}}*/
            }
            /*}}}*/
        }
        /*}}}*/
    }
    
    if (Neg)
        *Xhi = N[1] | SignBit32;
    else
        *Xhi = N[1];
    *Xlo = N[0];

    if (*Error)
    {
        /*{{{   */
        *Xhi = DRealNaNHi;
        *Xlo = DRealNaNLo;
        /*}}}*/
    }
    *Error = *Error || (CharsScanned <= StringSize);
}
/*}}}*/

/*{{{  StrToR32 */
#ifdef ANSI
PUBLIC VOID StrToR32 (BOOL *Error, BIT32 *Xout, VAL CHAR *String)
#else
PUBLIC VOID StrToR32 (Error, Xout, String)
    BOOL *Error;
    BIT32 *Xout;
    CHAR *String;
#endif
{
    BOOL More, Neg;
    INT Ch, Index, CharsScanned, StringSize;
    INT32 X, Carry, Lost, M, N, Scale, Xexp, Xfrac;

    StringSize = strlen(String);
    
    Neg = FALSE;
    *Error = FALSE;
    CharsScanned = 0;

    Ch = GetChar(String, StringSize, &CharsScanned);
    if ((Ch == '-') || (Ch == '+'))
    {
        /*{{{   */
        Neg = (Ch == '-');
        Ch = GetChar(String, StringSize, &CharsScanned);
        /*}}}*/
    }
    
    Scale = 0;
    N = 0;
    Carry = 0;
    Lost = 0;

    Ch = ValidDigit(&More, Ch);
    for (Index = -1; Index < 1; Index++)
    {
        /*{{{   */
        if ((Index == 0) && (Ch == '.'))
        {
            /*{{{   */
            Ch = GetChar(String, StringSize, &CharsScanned);
            Ch = ValidDigit(&More, Ch);
            /*}}}*/
        }
        
        *Error = *Error || (! More);
        while (More)
        {
            /*{{{   */
            if (Index == 0)
                Scale--;
            
            if (Carry == 0)
            {
                /*{{{   */
                LONGPROD((BIT32 *) &Carry, (BIT32 *) &M, N, 10, 0);
                
                if (Carry == 0)
                    LONGSUM((BIT32 *) &Carry, (BIT32 *) &M, M, (BIT32) Ch, 0);
                
                if (Carry == 0)
                    N = M;
                else
                {
                    /*{{{   */
                    Lost = Lost | Ch;
                    Scale = Scale + 1;
                    /*}}}*/
                }
                /*}}}*/
            }
            else
            {
                /*{{{   */
                Lost = Lost | Ch;
                Scale = Scale + 1;
                /*}}}*/
            }
              
            Ch = GetChar(String, StringSize, &CharsScanned);
            Ch = ValidDigit(&More, Ch);
            /*}}}*/
        }
        /*}}}*/
    }

    if (Ch == 'E')
    {
        /*{{{   */
        M = 0;
        
        Ch = GetChar(String, StringSize, &CharsScanned);
        if ((Ch == '+') || (Ch == '-'))
        {
            /*{{{   */
            if (Ch == '-')
                M = 1;
            
            Ch = GetChar(String, StringSize, &CharsScanned);
            /*}}}*/
        }
        else
            *Error = TRUE;
        
        Xexp = 0;
        
        Ch = ValidDigit(&More, Ch);
        *Error = *Error || (! More);
        
        while (More)
        {
            /*{{{   */
            if (IntegerLimit32 >= Xexp)
            {
                /*{{{   */
                Xexp = (Xexp * 10) + Ch;
                Ch = GetChar(String, StringSize, &CharsScanned);
                Ch = ValidDigit(&More, Ch);
                /*}}}*/
            }
            else
            {
                /*{{{   */
                INT StillChecking = TRUE;
                
                More = FALSE;
                Scale = 0;
                Xexp = MOSTPOS_INT32;
                
                while (StillChecking)
                {
                    /*{{{   */
                    Ch = GetChar(String, StringSize, &CharsScanned);
                    Ch = ValidDigit(&StillChecking, Ch);
                    /*}}}*/
                }
                /*}}}*/
            }
            /*}}}*/
        }
        
        if (M == 0)
            Scale = Scale + Xexp;
        else
            Scale = Scale - Xexp;
        /*}}}*/
    }

    Xexp = 0;
    if (N != 0)
    {
        /*{{{   */
        NORMALISE((BIT32 *) &Carry, (BIT32 *) &Xfrac, (BIT32 *) &N, N, 0);
        
        Xexp = Xexp + (BitsPerWord32 - (Carry + 1));
        
        if (Scale != 0)
            ScaleX(&Xexp, &Xfrac, Scale);
        
        Xexp = Xexp + RealXcess;
        if (Xexp >= RealExp)
        {
            /*{{{   */
            N = RealInf;
            
            /* NOTE: remove this line if overflow is OK */
            
            *Error = TRUE;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            if (Xexp <= 0)
            {
                /*{{{   */
                SHIFTRIGHT((BIT32 *) &Xfrac, (BIT32 *) &N, Xfrac, 0, 1 - Xexp);
                
                Xexp = 0;
                Lost = Lost | N;
                
                /* NOTE: remove this line if underflow is OK */
                
                *Error = TRUE;
                /*}}}*/
            }
            
            if (Lost != 0)
                Xfrac = Xfrac | 1;
            
            SHIFTRIGHT((BIT32 *) &Carry, (BIT32 *) &N, Xexp, Xfrac << 1, (BIT32) (RealShift + 1));
            
            if ((Xfrac & RealRBit) == 0)
                ;
            else if ((Xfrac & ((RealRBit << 1) | RealXcess)) == 0)
                ;
            else
                N = N + 1;
            /*}}}*/
        }
        /*}}}*/
    }

    if (Neg)
        X = N | SignBit32;
    else
        X = N;
    
    if (*Error)
        X = RealNaN;

    *Error = *Error || (CharsScanned <= StringSize);

    *Xout = X;
}
/*}}}*/

/*{{{  StrToInt */
#ifdef ANSI
PUBLIC VOID StrToInt (BOOL *Error, BIT32 *n, VAL CHAR *String)
#else
PUBLIC VOID StrToInt (Error, n, String)
    BIT32 *n;
    BOOL *Error;
    CHAR *String;
#endif
{
    INT32 Carry;
    BOOL Neg, More;
    INT CharsScanned, Digit, StringSize;
    
    *n = 0;
    *Error = FALSE;
    Neg = (String[0] == '-');

    if ((String[0] == '+') || (String[0] == '-'))
        CharsScanned = 1;
    else
        CharsScanned = 0;

    More = TRUE;
    Digit = 0;
    StringSize = strlen(String);

    while (More && (CharsScanned < StringSize))
    {
        /*{{{   */
        LONGPROD((BIT32 *) &Carry, (BIT32 *) n, *n, 10, (INT32) Digit);
        
        *Error = *Error || (Carry != 0);
        
        Digit = ValidDigit(&More, String[CharsScanned++]);
        /*}}}*/
    }

    if (More)
    {
        /*{{{   */
        LONGPROD((BIT32 *) &Carry, (BIT32 *) n, *n, 10, (INT32) Digit);
        
        *Error = *Error || (Carry != 0);
        /*}}}*/
    }
    else
        *Error = TRUE;

    if (Neg)
    {
        /*{{{   */
        *n = (~(*n)) + 1;
        
        *Error = (*Error || ((*n != 0) && ((*n & SignBit32) == 0)));
        /*}}}*/
    }
    else
        *Error = (*Error || ((*n & SignBit32) != 0));
}
/*}}}*/

/*{{{  StrToI8 */
#ifdef ANSI
PUBLIC VOID StrToI8 (BOOL *Error, BIT32 *n, VAL CHAR *String)
#else
PUBLIC VOID StrToI8 (Error, n, String)
    BIT32 *n;
    BOOL *Error;
    CHAR *String;
#endif
{
    StrToInt(Error, n, String);

    if ((*Error == FALSE) && ((*n & HighBits8) != 0))
        *Error = TRUE;
}
/*}}}*/

/*{{{  StrToI16 */
#ifdef ANSI
PUBLIC VOID StrToI16 (BOOL *Error, BIT32 *n, VAL CHAR *String)
#else
PUBLIC VOID StrToI16 (Error, n, String)
    BIT32 *n;
    BOOL *Error;
    CHAR *String;
#endif
{
    StrToInt(Error, n, String);

    if ((*Error == FALSE) && ((*n & NegBits16) != 0) && ((*n & NegBits16) != NegBits16))
        *Error = TRUE;
}
/*}}}*/

/*{{{  StrToI64 */
#ifdef ANSI
PUBLIC VOID StrToI64 (BOOL *Error, BIT32 *hi, BIT32 *lo, VAL CHAR *String)
#else
PUBLIC VOID StrToI64 (Error, hi, lo, String)
    BOOL *Error;
    CHAR *String;
    BIT32 *hi, *lo;
#endif
{
    BOOL Neg, More;
    INT32 Carry, n1, n0;
    INT Digit, CharsScanned, StringSize;

    n0 = 0;
    n1 = 0;
    *Error = FALSE;
    Neg = (String[0] == '-');

    if ((String[0] == '+') || (String[0] == '-'))
        CharsScanned = 1;
    else
        CharsScanned = 0;

    More = TRUE;
    Digit = 0;
    StringSize = strlen(String);

    while (More && (CharsScanned < StringSize))
    {
        /*{{{   */
        LONGPROD((BIT32 *) &Carry, (BIT32 *) &n0, n0, 10, (BIT32) Digit);
        LONGPROD((BIT32 *) &Carry, (BIT32 *) &n1, n1, 10, Carry);
        
        *Error = *Error || (Carry != 0);
        
        Digit = ValidDigit(&More, String[CharsScanned++]);
        /*}}}*/
    }

    if (More)
    {
        /*{{{   */
        LONGPROD((BIT32 *) &Carry, (BIT32 *) &n0, n0, 10, (BIT32) Digit);
        LONGPROD((BIT32 *) &Carry, (BIT32 *) &n1, n1, 10, Carry);
        
        *Error = *Error || (Carry != 0);
        /*}}}*/
    }
    else
        *Error = TRUE;

    if (Neg)
    {
        /*{{{   */
        LONGDIFF((BIT32 *) &Carry, (BIT32 *) &n0, 0, n0, 0);
        LONGDIFF((BIT32 *) &Carry, (BIT32 *) &n1, 0, n1, Carry);
        
        *Error = *Error || (n1 > 0);
        /*}}}*/
    }
    else
        *Error = *Error || (n1 < 0);

    *hi = n1;
    *lo = n0;
}
/*}}}*/

/*{{{  StrToHex */
#ifdef ANSI
PUBLIC VOID StrToHex (BOOL *Error, BIT32 *n, VAL CHAR *String)
#else
PUBLIC VOID StrToHex (Error, n, String)
    BIT32 *n;
    BOOL *Error;
    CHAR *String;
#endif
{
    BOOL More;
    INT32 Carry;
    INT CharsScanned, Digit, StringSize;
    
    *n = 0;
    *Error = FALSE;
    CharsScanned = 0;

    More = TRUE;
    Digit = 0;
    StringSize = strlen(String);

    while (More && (CharsScanned < StringSize))
    {
        /*{{{   */
        SHIFTLEFT((BIT32 *) &Carry, n, 0, *n, 4);
        
        *n = *n | Digit;
        
        *Error = *Error || (Carry != 0);
        
        Digit = ValidHexDigit (&More, String[CharsScanned++]);
        /*}}}*/
    }

    if (More)
    {
        /*{{{   */
        SHIFTLEFT((BIT32 *) &Carry, n, 0, *n, 4);
        
        *n = *n | Digit;
        
        *Error = *Error || (Carry != 0);
        /*}}}*/
    }
    else
        *Error = TRUE;
}
/*}}}*/

/*{{{  StrToH8 */
#ifdef ANSI
PUBLIC VOID StrToH8 (BOOL *Error, BIT32 *n, VAL CHAR *String)
#else
PUBLIC VOID StrToH8 (Error, n, String)
    BIT32 *n;
    BOOL *Error;
    CHAR *String;
#endif
{
    StrToHex(Error, n, String);

    if ((*Error==FALSE) && ((*n & HighBits8) != 0))
        *Error = TRUE;
}
/*}}}*/

/*{{{  StrToH16 */
#ifdef ANSI
PUBLIC VOID StrToH16 (BOOL *Error, BIT32 *n, VAL CHAR *String)
#else
PUBLIC VOID StrToH16 (Error, n, String)
    BIT32 *n;
    BOOL *Error;
    CHAR *String;
#endif
{
    StrToHex(Error, n, String);

    if ((*Error==FALSE) && ((*n & HighBits16) != 0))
        *Error = TRUE;

    if ((*Error==FALSE) && ((*n & SignBit16) != 0))
        *n = *n | HighBits16;
}
/*}}}*/

/*{{{  StrToH64 */
#ifdef ANSI
PUBLIC VOID StrToH64 (BOOL *Error, BIT32 *hi, BIT32 *lo, VAL CHAR *String)
#else
PUBLIC VOID StrToH64 (Error, hi, lo, String)
    BOOL *Error;
    CHAR *String;
    BIT32 *hi, *lo;
#endif
{
    BOOL More;
    INT32 Carry, Dump;
    INT CharsScanned, Digit, StringSize;
    
    *lo = 0;
    *hi = 0;
    *Error = FALSE;
    CharsScanned = 0;

    More = TRUE;
    Digit = 0;
    StringSize = strlen(String);

    while ((More) && (CharsScanned < StringSize))
    {
        /*{{{   */
        SHIFTLEFT((BIT32 *) &Carry, (BIT32 *) &Dump, 0, *hi, 4);
        SHIFTLEFT(hi, lo, *hi, *lo, 4);
        
        *lo = *lo | Digit;
        
        *Error = *Error || (Carry != 0);
        
        Digit = ValidHexDigit (&More, String[CharsScanned++]);
        /*}}}*/
    }

    if (More)
    {
        /*{{{   */
        SHIFTLEFT((BIT32 *) &Carry, (BIT32 *) &Dump, 0, *hi, 4);
        SHIFTLEFT(hi, lo, *hi, *lo, 4);
        
        *lo = *lo | Digit;
        
        *Error = *Error || (Carry != 0);
        /*}}}*/
    }
    else
        *Error = TRUE;
}
/*}}}*/

/*{{{  StrToOct */
#ifdef ANSI
PUBLIC VOID StrToOct (BOOL *Error, BIT32 *n, VAL CHAR *String)
#else
PUBLIC VOID StrToOct (Error, n, String)
    BIT32 *n;
    BOOL *Error;
    CHAR *String;
#endif
{
    BOOL More;
    INT32 Carry;
    INT CharsScanned, Digit, StringSize;

    *n = 0;
    *Error = FALSE;
    CharsScanned = 0;

    More = TRUE;
    Digit = 0;
    StringSize = strlen(String);

    while (More && (CharsScanned < StringSize))
    {
        /*{{{   */
        SHIFTLEFT((BIT32 *) &Carry, n, 0, *n, 3);
        
        *n = *n | Digit;
        
        *Error = *Error || (Carry != 0);
        
        Digit = ValidOctDigit (&More, String[CharsScanned++]);
        /*}}}*/
    }

    if (More)
    {
        /*{{{   */
        SHIFTLEFT((BIT32 *) &Carry, n, 0, *n, 3);
        
        *n = *n | Digit;
        
        *Error = *Error || (Carry != 0);
        /*}}}*/
    }
    else
        *Error = TRUE;
}
/*}}}*/
/*}}}*/
