/*
 *	intel.h - Intel related constants (i386 target)
 *	Copyright (C) 2000-2006 Fred Barnes <frmb@kent.ac.uk>
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


#ifndef __INTEL_H
#define __INTEL_H

/* 16-bit regs */
#define REG_AX 0
#define REG_CX 1
#define REG_DX 2
#define REG_BX 3
#define REG_SP 4
#define REG_BP 5
#define REG_SI 6
#define REG_DI 7

/* 8-bit regs */
#define REG_AL 0
#define REG_CL 1
#define REG_DL 2
#define REG_BL 3
#define REG_AH 4
#define REG_CH 5
#define REG_DH 6
#define REG_BH 7

/* 32-bit regs */
#define REG_EAX 0
#define REG_ECX 1
#define REG_EDX 2
#define REG_EBX 3
#define REG_ESP 4
#define REG_EBP 5
#define REG_ESI 6
#define REG_EDI 7

/* 32-bit alternate register names */
#define REG_ALT_EAX -128
#define REG_ALT_EBX -127
#define REG_ALT_ECX -126
#define REG_ALT_EDX -125
#define REG_ALT_EDI -124

/* segment regs */
#define REG_ES 0
#define REG_CS 1
#define REG_SS 2
#define REG_DS 3

/* FPU rounding modes */
#define I386_FPU_N 0
#define I386_FPU_M 1
#define I386_FPU_P 2
#define I386_FPU_Z 3

/* instruction encoding stuff */
typedef struct TAG_ins_encoding {
	int ins;
	char *input;
	char *output;
} ins_encoding;

#ifdef WANT_I386_CODINGS
static ins_encoding i386_instrs[] =
	{{INS_UNDEFINED, NULL, NULL}}
#endif	/* !WANT_I386_CODINGS */

#endif	/* !__INTEL_H */

