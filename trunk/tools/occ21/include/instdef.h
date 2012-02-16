/* $Id: instdef.h,v 1.1 1996/04/15 10:52:11 djb1 Exp $ */

/*
 *	Instruction definitions
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


/*{{{  constants */
#define SEQUENTIALGUYS 1
#define FULLGUYS 2

/* Pseudo instructions: (Can overlay the real instructions OK) */
#define I_AJWRET        0x00
#define I_LDRETP        0x01
#define I_LD            0x02
#define I_LDAB          0x03
#define I_LDABC         0x04
#define I_ST            0x05
#define I_STAB          0x06
#define I_STABC         0x07
#define I_BYTE          0x08
#define I_WORD          0x09
#define I_ALIGN         0x0a
#define I_LDLABELDIFF   0x0b
#define I_RESERVELOWWS  0x0c
#define I_LDLABELP	0x0d
#define I_THROWAWAY	0x0e

/*}}}*/

INT32 lookupinstruction(
  const char *const inst, const BOOL guy_not_asm,
  const int guyinserts,
  int *err, int *operands,
  int *base_instruction,
  BOOL *primary,
  BOOL *byte_directive,
  BOOL *word_directive,
  BOOL *loading_address,
  BOOL *ptrs_allowed,
  BOOL *element_required,
  BOOL *labels_permitted,
  BOOL *const_required);

const char *primaryname   (int inst);
const char *secondaryname (int inst);
