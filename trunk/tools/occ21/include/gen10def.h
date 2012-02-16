/* $Id: gen10def.h,v 1.1 1996/04/15 10:52:04 djb1 Exp $ */

/*
 *	gen10 declarations
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

void set_alt_ds(treenode *tptr);

void alt_check_enabling_functions(treenode *tptr);

/*void mapoutputitem (int channelmode, treenode **channel,
                          int outputitemmode, treenode **outputitem);*/ /* now PRIVATE */
void mapchantypeop (treenode **ctname);
void mapinputitem (int channelmode, treenode **channel,
                         int inputitemmode, treenode **inputitem,
                         treenode *prottype, treenode *comm, treenode *x_process);
void mapoutput (treenode *tptr);
void mapinput (treenode *tptr);
void mapxinputoutput (treenode *tptr);
void mapalt (treenode *tptr);
/*void toutputitem (int channelmode, treenode *channel,
                        int outputitemmode, treenode *outputitem,
                        treenode *prottype);*/ /* now PRIVATE */
void tinputitem (int channelmode, treenode *channel,
                       int inputitemmode, treenode *inputitem,
                       treenode *prottype, treenode *comm, treenode *x_process);

void toutput (treenode *tptr);
void tinput (treenode *tptr);
void txinputoutput (treenode *tptr);
void talt (treenode *tptr);
void tioop (int ioinst, BOOL io_by_call);

