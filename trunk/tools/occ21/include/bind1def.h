/* $Id: bind1def.h,v 1.1 1996/04/15 10:51:58 djb1 Exp $ */

/*
 *	bind1 declarations
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

/*{{{  variables */
extern int globchannelmode;
extern treenode **globchannelptr;
extern treenode *templist;
extern treenode *constantnptr;
extern treenode *consttablechain;

extern struct trans_params_s *be_trans_params;

/*}}}*/

/*{{{  routines */
void mappreprocess (treenode *tptr);
void mapdeclandbody (treenode *tptr, void (*mbody)(treenode *),
                           BOOL preprocess_first, BOOL preprocess_rest);
void freetemplist(void);
void mapconstruction (treenode *tptr, void (*mapproc)(treenode *));
void mapchoice (treenode *tptr);
void mapprocess (treenode *tptr);
/*void mapblock (treenode *tptr);*/ /* now PRIVATE */
/*void mapnestedblocks (treenode *tptr);*/ /* now PRIVATE */
void mapmain (treenode *tptr);
treenode *libentry (wordnode *libcallstr, SOURCEPOSN locn);
treenode *vlibentry (wordnode *libcallstr);
void addtemplist (treenode *tptr);
void maprepl (treenode *tptr, void (*mbody)(treenode *));
void initbind (void);
int setup_asm_operands (treenode *tptr, treenode **operands[MAXREGS], int opmodes[MAXREGS]);
BOOL is_indirect_name (treenode *nptr);
/*}}}*/
