/*
tvm - ins_t9000.c
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2008 Carl G. Ritson <cgr@kent.ac.uk>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "tvm.h"
#include "instructions.h"
#include "ins_t9000.h"

/* Selected T9000 Transputer Instructions */

#ifdef TVM_SHORT_OPS

/****************************************************************************
 *              0x2B 0xF_         0x2B 0xF_         0x2B 0xF_               *
 ****************************************************************************/

/* 0xB8 - 0x2B 0xF8 - xbword - sign extend byte to word */
TVM_INSTRUCTION (ins_xbword)
{
	STACK_RET ((WORD)((SBYTE) (AREG & 0xff)), BREG, CREG, STYPE_DATA, BREGt, CREGt);
}

/* 0xB9 - 0x2B 0xF9 - lbx - load byte and extend */
TVM_INSTRUCTION (ins_lbx)
{
	AREG = (WORD) ((SBYTE) read_byte ((BYTEPTR) AREG));
	STACK_RET (AREG, BREG, CREG, STYPE_DATA, BREGt, CREGt);
}

/* 0xBA - 0x2B 0xFA - cb - check byte */
TVM_INSTRUCTION (ins_cb)
{
	if ((AREG < (-128)) || (AREG >= 128))
	{
		SET_ERROR_FLAG (EFLAG_INTERR);
	}
	STACK_RET (AREG, BREG, CREG, AREGt, BREGt, CREGt);
}

/* 0xBB - 0x2B 0xFB - cbu - check byte unsigned */
TVM_INSTRUCTION (ins_cbu)
{
	if ((AREG & (~0xff)))
	{
		SET_ERROR_FLAG (EFLAG_INTERR);
	}
	STACK_RET (AREG, BREG, CREG, AREGt, BREGt, CREGt);
}


/****************************************************************************
 *              0x2C 0xF_         0x2C 0xF_         0x2C 0xF_               *
 ****************************************************************************/

/* 0xC1 - 0x2C 0xF1 - ssub - sixteen subscript */
TVM_INSTRUCTION (ins_ssub)
{
	INT16PTR ptr = int16ptr_plus ((INT16PTR) AREG, BREG);
	STACK2_RET ((WORD) ptr, CREG, AREGt, CREGt);
}

/* 0xC7 - 0x2C 0xF7 - cir - check in range */
TVM_INSTRUCTION (ins_cir)
{
	if ((CREG < AREG) || (CREG > BREG))
	{
		SET_ERROR_FLAG (EFLAG_INTERR);
	}
	STACK1_RET (CREG, CREGt);
}

/* 0xC8 - 0x2C 0xF8 - ss - store sixteen */
TVM_INSTRUCTION (ins_ss)
{
	write_int16 ((INT16PTR) AREG, (INT16) (BREG & 0xffff));
	write_type (ectx, AREG, STYPE_DATA);
	STACK1_RET (CREG, CREGt);
}

/* 0xCA - 0x2C 0xFA - ls - load sixteen */
TVM_INSTRUCTION (ins_ls)
{
	INT16 s = (INT16) read_int16 ((INT16PTR) AREG);
	STACK_RET ((((WORD) s) & 0xffff), BREG, CREG, STYPE_DATA, BREGt, CREGt);
}

/* 0xCC - 0x2C 0xFC - ciru - check in range unsigned */
TVM_INSTRUCTION (ins_ciru)
{
	UWORD a = (UWORD) AREG;
	UWORD b = (UWORD) BREG;
	UWORD c = (UWORD) CREG;
	if ((c < a) || (c > b))
	{
		SET_ERROR_FLAG (EFLAG_INTERR);
	}
	STACK1_RET (CREG, CREGt);
}


/****************************************************************************
 *              0x2F 0xF_         0x2F 0xF_         0x2F 0xF_               *
 ****************************************************************************/

/* 0xF8 - 0x2F 0xF8 - xsword - sign extend sixteen to word */
TVM_INSTRUCTION (ins_xsword)
{
	STACK_RET ((WORD)((INT16) (AREG & 0xffff)), BREG, CREG, STYPE_DATA, BREGt, CREGt);
}

/* 0xF9 - 0x2F 0xF9 - lsx - load sixteen and extend */
TVM_INSTRUCTION (ins_lsx)
{
	INT16 s = (INT16) read_int16 ((INT16PTR) AREG);
	STACK_RET (((WORD) s), BREG, CREG, STYPE_DATA, BREGt, CREGt);
}

/* 0xFA - 0x2F 0xFA - cs - check sixteen */
TVM_INSTRUCTION (ins_cs)
{
	if ((AREG < (-32768)) || (AREG >= 32768))
	{
		SET_ERROR_FLAG (EFLAG_INTERR);
	}
	STACK_RET (AREG, BREG, CREG, AREGt, BREGt, CREGt);
}

/* 0xFB - 0x2F 0xFB - csu - check sixteen unsigned */
TVM_INSTRUCTION (ins_csu)
{
	if ((AREG & (~0xffff)))
	{
		SET_ERROR_FLAG (EFLAG_INTERR);
	}
	STACK_RET (AREG, BREG, CREG, AREGt, BREGt, CREGt);
}

#endif /* TVM_SHORT_OPS */
