/* $Id: debugdef.h,v 1.2 1997/09/19 14:02:05 mdp2 Exp $ */

/*
 *	debugging output definitions
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


/*{{{  data declarations*/
extern BOOL symbolic_debugoutput;
/*}}}*/

/*{{{  routines*/
void debug_gencodemark  (const treenode *tptr, BIT32 offset);
void debug_genlibrpatch (const treenode *tptr, BIT32 offset);
void debug_genaddrfix   (const treenode *tptr, BIT32 offset);
void flush_debug_buffers (void);
void debugmain (treenode *tptr);
BOOL need_locate (int tag); /* (unsigned char tag); */
int get_from_index_table (const treenode *const address);
/*int get_from_linemark_list (int index);*/
int get_file_number (int this_index);

void debugfree (void);

void debug_write_mapfile(treenode *tptr);
/*}}}*/

/*{{{  side-effects pragmas*/
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (need_locate)
#endif
/*}}}*/


