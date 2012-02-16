/*{{{  module header */

/*
 *	long arithmetic
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

static char RcsId[] = "$Id: extlong.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ Copyright INMOS Limited";
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
/*{{{  LONGSUM */
#ifdef ANSI
PUBLIC VOID LONGSUM (BIT32 *CarryOut, BIT32 *Sum, BIT32 Left, BIT32 Right, BIT32 CarryIn)
#else
PUBLIC VOID LONGSUM (CarryOut, Sum, Left, Right, CarryIn)
    BIT32 *CarryOut, *Sum, Left, Right, CarryIn;
#endif
{
    CarryIn &= 1;

    if ((MaxUnsignedInt32 - Left) >= Right)
    {
        /*{{{   */
        *Sum = Left + Right;
        
        if ((CarryIn != 0) && (*Sum == MaxUnsignedInt32))
        {
            /*{{{   */
            *Sum = 0;
            *CarryOut = 1;
            /*}}}*/
        }
        else
        {
            /*{{{   */
            *Sum = *Sum + CarryIn;
            *CarryOut = 0;
            /*}}}*/
        }
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *CarryOut = 1;
        *Sum = (Right - ((MaxUnsignedInt32 - Left) + 1)) + CarryIn;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  LONGADD */
#ifdef ANSI
PUBLIC VOID LONGADD (BIT32 *Sum, BIT32 Left, BIT32 Right, BIT32 CarryIn)
#else
PUBLIC VOID LONGADD (Sum, Left, Right, CarryIn)
    BIT32 *Sum, Left, Right, CarryIn;
#endif
{
    *Sum = Left + (Right + (CarryIn & 1));
}
/*}}}*/

/*{{{  LONGDIFF */
#ifdef ANSI
PUBLIC VOID LONGDIFF (BIT32 *BorrowOut, BIT32 *Diff, BIT32 Left, BIT32 Right, BIT32 BorrowIn)
#else
PUBLIC VOID LONGDIFF (BorrowOut, Diff, Left, Right, BorrowIn)
    BIT32 *BorrowOut, *Diff, Left, Right, BorrowIn;
#endif
{
    BorrowIn &= 1;
    *BorrowOut = 0;

    if (Left >= Right)
        *Diff = Left - Right;
    else
    {
        /*{{{   */
        *Diff = MaxUnsignedInt32 - Right;
        *Diff = (*Diff + Left) + 1;
        *BorrowOut = 1;
        /*}}}*/
    }

    if (BorrowIn != 0)
    {
        /*{{{   */
        if (*Diff > 0)
            (*Diff)--;
        else
        {
            /*{{{   */
            *Diff = MaxUnsignedInt32;
            *BorrowOut = 1;
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/

/*{{{  LONGPROD */
#ifdef ANSI
PUBLIC VOID LONGPROD (BIT32 *ProdHi, BIT32 *ProdLo, BIT32 Left, BIT32 Right, BIT32 CarryIn)
#else
PUBLIC VOID LONGPROD (ProdHi, ProdLo, Left, Right, CarryIn)
    BIT32 *ProdHi, *ProdLo, Left, Right, CarryIn;
#endif
{
    BIT32 Temp1, Temp2, Temp3, Temp4, TempLo, TempHi, Carry;

    Temp1 = Left & LowBits16;          /* Left MOD 2^16 */
    Temp2 = (Left >> 16) & LowBits16;  /* Left DIV 2^16 */
    Temp3 = Right & LowBits16;         /* Right MOD 2^16 */
    Temp4 = (Right >> 16) & LowBits16; /* Right DIV 2^16 */

    *ProdLo = Temp1 * Temp3;
    TempLo = Temp2 * Temp3;
    TempHi = (TempLo >> 16) & LowBits16;
    TempLo = TempLo << 16;

    LONGSUM(ProdHi, ProdLo, *ProdLo, TempLo, 0);

    *ProdHi = *ProdHi + TempHi;
    TempLo = Temp1 * Temp4;
    TempHi = (TempLo >> 16) & LowBits16;
    TempLo = TempLo << 16;

    LONGSUM(&Carry, ProdLo, *ProdLo, TempLo, 0);

    *ProdHi = (*ProdHi + Carry) + TempHi;

    LONGSUM(&Carry, ProdLo, *ProdLo, CarryIn, 0);

    *ProdHi = (*ProdHi + Carry) + (Temp2 * Temp4);
}
/*}}}*/

/*{{{  LONGDIV */
#ifdef ANSI
PUBLIC VOID LONGDIV (BIT32 *Quot, BIT32 *Rem, BIT32 DividHi, BIT32 DividLo, BIT32 Divis)
#else
PUBLIC VOID LONGDIV (Quot, Rem, DividHi, DividLo, Divis)
    BIT32 *Quot, *Rem, DividHi, DividLo, Divis;
#endif
{
    if (Divis == 0)
    {
        /*{{{   */
        *Quot = 0;
        *Rem = 0;
        /*}}}*/
    }
    else if ((DividHi == 0) && (DividLo == 0))
    {
        /*{{{   */
        *Quot = 0;
        *Rem = 0;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        INT BitCount;
        BIT32 Places, RemHi, RemLo, QuotHi, QuotLo;
        
        RemHi = 0;
        RemLo = 0;
        QuotHi = DividHi;
        QuotLo = DividLo;
        
        NORMALISE(&Places, &QuotHi, &QuotLo, QuotHi, QuotLo);
        
        BitCount = 64 - ((INT) Places);
        do
        {
            /*{{{   */
            SHIFTLEFT(&RemHi, &RemLo, RemHi, RemLo, 1);
            
            if ((QuotHi & SignBit32) != 0)
                RemLo++;
            
            SHIFTLEFT(&QuotHi, &QuotLo, QuotHi, QuotLo, 1);
            
            if ((RemHi > 0) || ((RemHi == 0) && (RemLo >= Divis)))
            {
                /*{{{   */
                BIT32 Borrow;
                
                LONGDIFF(&Borrow, &RemLo, RemLo, Divis, 0);
                LONGDIFF(&Borrow, &RemHi, RemHi, 0, Borrow);
                QuotLo++;
                /*}}}*/
            }
            
            BitCount--;
            /*}}}*/
        }
        while (BitCount > 0);
        
        *Quot = QuotLo;
        *Rem = RemLo;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  SHIFTRIGHT */
#ifdef ANSI
PUBLIC VOID SHIFTRIGHT (BIT32 *OutHi, BIT32 *OutLo, BIT32 InHi, BIT32 InLo, BIT32 Places)
#else
PUBLIC VOID SHIFTRIGHT (OutHi, OutLo, InHi, InLo, Places)
    BIT32 *OutHi, *OutLo, InHi, InLo, Places;
#endif
{
    if (Places >= 64)
    {
        /*{{{   */
        *OutHi = 0;
        *OutLo = 0;
        /*}}}*/
    }
    else if (Places >= 32)
    {
        /*{{{   */
        *OutHi = 0;
        *OutLo = InHi >> (Places - 32);
        /*}}}*/
    }
    else if (Places == 0)
    {
        /*{{{   */
        *OutLo = InLo;
        *OutHi = InHi;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *OutLo = (InLo >> Places) | (InHi << (32 - Places));
        *OutHi = InHi >> Places;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  SHIFTLEFT */
#ifdef ANSI
PUBLIC VOID SHIFTLEFT (BIT32 *OutHi, BIT32 *OutLo, BIT32 InHi, BIT32 InLo, BIT32 Places)
#else
PUBLIC VOID SHIFTLEFT (OutHi, OutLo, InHi, InLo, Places)
    BIT32 *OutHi, *OutLo, InHi, InLo, Places;
#endif
{
    if (Places >= 64)
    {
        /*{{{   */
        *OutHi = 0;
        *OutLo = 0;
        /*}}}*/
    }
    else if (Places >= 32)
    {
        /*{{{   */
        *OutLo = 0;
        *OutHi = InLo << (Places - 32);
        /*}}}*/
    }
    else if (Places == 0)
    {
        /*{{{   */
        *OutLo = InLo;
        *OutHi = InHi;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *OutHi = (InHi << Places) | (InLo >> (32 - Places));
        *OutLo = InLo << Places;
        /*}}}*/
    }
}
/*}}}*/

/*{{{  NORMALISE */
#ifdef ANSI
PUBLIC VOID NORMALISE (BIT32 *Places, BIT32 *OutHi, BIT32 *OutLo, BIT32 InHi, BIT32 InLo)
#else
PUBLIC VOID NORMALISE (Places, OutHi, OutLo, InHi, InLo)
    BIT32 *Places, *OutHi, *OutLo, InHi, InLo;
#endif
{
    if ((InHi == 0) && (InLo == 0))
    {
        /*{{{   */
        *Places = 64;
        *OutHi = 0;
        *OutLo = 0;
        /*}}}*/
    }
    else
    {
        /*{{{   */
        *Places = 0;
        *OutHi = InHi;
        *OutLo = InLo;
        
        while ((*OutHi & SignBit32) == 0)
        {
            /*{{{   */
            SHIFTLEFT(OutHi, OutLo, *OutHi, *OutLo, 1);
            (*Places)++;
            /*}}}*/
        }
        /*}}}*/
    }
}
/*}}}*/
/*}}}*/
