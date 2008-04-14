/*
 *	ppc.h - PPC related constants
 *	Copyright (C) 2005 Fred Barnes <frmb@kent.ac.uk>
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


#ifndef __PPC_H
#define __PPC_H

/* 32-bit regs */
#define REG_R0 0
#define REG_R1 1
#define REG_R2 2
#define REG_R3 3
#define REG_R4 4
#define REG_R5 5
#define REG_R6 6
#define REG_R7 7
#define REG_R8 8
#define REG_R9 9
#define REG_R10 10
#define REG_R11 11
#define REG_R12 12
#define REG_R13 13
#define REG_R14 14
#define REG_R15 15
#define REG_R16 16
#define REG_R17 17
#define REG_R18 18
#define REG_R19 19
#define REG_R20 20
#define REG_R21 21
#define REG_R22 22
#define REG_R23 23
#define REG_R24 24
#define REG_R25 25
#define REG_R26 26
#define REG_R27 27
#define REG_R28 28
#define REG_R29 29
#define REG_R30 30
#define REG_R31 31

#define FIRST_REAL_REG REG_R0
#define LAST_REAL_REG REG_R31

#define REG_F0 0
#define REG_F1 1
#define REG_F2 2
#define REG_F3 3
#define REG_F4 4
#define REG_F5 5
#define REG_F6 6
#define REG_F7 7
#define REG_F8 8
#define REG_F9 9
#define REG_F10 11
#define REG_F11 12
#define REG_F12 12
#define REG_F13 13
#define REG_F14 14
#define REG_F15 15
#define REG_F16 16
#define REG_F17 17
#define REG_F18 18
#define REG_F19 19
#define REG_F20 20
#define REG_F21 21
#define REG_F22 22
#define REG_F23 23
#define REG_F24 24
#define REG_F25 25
#define REG_F26 26
#define REG_F27 27
#define REG_F28 28
#define REG_F29 29
#define REG_F30 30
#define REG_F31 31

/* 32-bit alternate register names */
#define REG_ALT_R0 -128
#define REG_ALT_R1 -127
#define REG_ALT_R2 -126
#define REG_ALT_R3 -125
#define REG_ALT_R4 -124
#define REG_ALT_R5 -123
#define REG_ALT_R6 -122
#define REG_ALT_R7 -121
#define REG_ALT_R8 -120
#define REG_ALT_R9 -119
#define REG_ALT_R10 -118
#define REG_ALT_R11 -117
#define REG_ALT_R12 -116
#define REG_ALT_R13 -115
#define REG_ALT_R14 -114
#define REG_ALT_R15 -113
#define REG_ALT_R16 -112
#define REG_ALT_R17 -111
#define REG_ALT_R18 -110
#define REG_ALT_R19 -109
#define REG_ALT_R20 -108
#define REG_ALT_R21 -107
#define REG_ALT_R22 -106
#define REG_ALT_R23 -105
#define REG_ALT_R24 -104
#define REG_ALT_R25 -103
#define REG_ALT_R26 -102
#define REG_ALT_R27 -101
#define REG_ALT_R28 -100
#define REG_ALT_R29 -99
#define REG_ALT_R30 -98
#define REG_ALT_R31 -97


#define FIRST_ALT_REG REG_ALT_R0
#define LAST_ALT_REG REG_ALT_R31

/* various FPU things */
#define PPC_FPU_M 0
#define PPC_FPU_Z 1
#define PPC_FPU_P 2
#define PPC_FPU_N 3

/* instruction encoding stuff */
typedef struct TAG_ins_encoding {
	int ins;
	char *input;
	char *output;
} ins_encoding;

#ifdef WANT_PPC_CODINGS
static ins_encoding ppc_instrs[] =
	{{INS_UNDEFINED, NULL, NULL}}
#endif	/* !WANT_PPC_CODINGS */

#endif	/* !__PPC_H */

