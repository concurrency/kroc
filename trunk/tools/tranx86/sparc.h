/*
 *	sparc.h - Sparc related constants
 *	Copyright (C) 2004 Fred Barnes <frmb@kent.ac.uk>
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


#ifndef __SPARC_H
#define __SPARC_H

/* 32-bit regs */
#define REG_R0 0
#define REG_G1 1
#define REG_G2 2
#define REG_G3 3
#define REG_G4 4
#define REG_G5 5
#define REG_G6 6
#define REG_G7 7
#define REG_O0 8
#define REG_O1 9
#define REG_O2 10
#define REG_O3 11
#define REG_O4 12
#define REG_O5 13
#define REG_SP 14
#define REG_O7 15
#define REG_L0 16
#define REG_L1 17
#define REG_L2 18
#define REG_L3 19
#define REG_L4 20
#define REG_L5 21
#define REG_L6 22
#define REG_L7 23
#define REG_I0 24
#define REG_I1 25
#define REG_I2 26
#define REG_I3 27
#define REG_I4 28
#define REG_I5 29
#define REG_FP 30
#define REG_I7 31
#define REG_Y 32

#define FIRST_REAL_REG REG_R0
#define LAST_REAL_REG REG_Y

/* 4 32-bit alternate register names */
#define REG_ALT_R0 -128
#define REG_ALT_G1 -127
#define REG_ALT_G2 -126
#define REG_ALT_G3 -125
#define REG_ALT_G4 -124
#define REG_ALT_G5 -123
#define REG_ALT_G6 -122
#define REG_ALT_G7 -121
#define REG_ALT_O0 -120
#define REG_ALT_O1 -119
#define REG_ALT_O2 -118
#define REG_ALT_O3 -117
#define REG_ALT_O4 -116
#define REG_ALT_O5 -115
#define REG_ALT_SP -114
#define REG_ALT_O7 -113
#define REG_ALT_L0 -112
#define REG_ALT_L1 -111
#define REG_ALT_L2 -110
#define REG_ALT_L3 -109
#define REG_ALT_L4 -108
#define REG_ALT_L5 -107
#define REG_ALT_L6 -106
#define REG_ALT_L7 -105
#define REG_ALT_I0 -104
#define REG_ALT_I1 -103
#define REG_ALT_I2 -102
#define REG_ALT_I3 -101
#define REG_ALT_I4 -100
#define REG_ALT_I5 -99
#define REG_ALT_FP -98
#define REG_ALT_I7 -97
#define REG_ALT_Y -96

#define FIRST_ALT_REG REG_ALT_R0
#define LAST_ALT_REG REG_ALT_Y


/* various FPU things */
#define SPARC_FPU_N 0
#define SPARC_FPU_P 1
#define SPARC_FPU_M 2
#define SPARC_FPU_Z 3

/* instruction encoding stuff */
typedef struct TAG_ins_encoding {
	int ins;
	char *input;
	char *output;
} ins_encoding;

#ifdef WANT_SPARC_CODINGS
static ins_encoding sparc_instrs[] =
	{{INS_UNDEFINED, NULL, NULL}}
#endif	/* !WANT_SOARC_CODINGS */

#endif	/* !__SPARC_H */

