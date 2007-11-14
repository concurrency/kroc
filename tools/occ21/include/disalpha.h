/*
 *	Alpha disassembly definitions
 *	Copyright (C) 1995 Michael Poole
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

/*{{{  external variables*/
extern BIT32 * code_ptr;
/*}}}*/
/*{{{  functions*/
void look_at_alpha_instruction (const BIT32 ilword,
        const char **instr, const char **operands,
        const INT32 disaddr,
        const BOOL asm_only, int * special);
void print_instruction_word (FILE *df,
                               const BIT32 iword,
                               const INT32 start,
                               const INT32 address,
                               int *special, const BOOL asm_only);
void print_const_word (FILE * df,
                   const BIT32 dword,
                   const INT32 start,
                   const INT32 address,
                   const BOOL asm_only);
void disassemble_code_block (FILE * df, const INT32 start,
                             BIT32 (*get_lword)(void),
                             const int code_offset,
                             const int code_length, const int indent,
                             const BOOL asm_only);
/*}}}*/
