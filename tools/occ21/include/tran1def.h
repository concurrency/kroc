/* $Id: tran1def.h,v 1.1 1996/04/15 10:52:22 djb1 Exp $ */

/*
 *	code generator (tran1) local declarations
 *	Copyright (C) 1993 Inmos Limited
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


/*{{{  error codes */
#define TRANS_RUN_TIME_OVERLAP_CHECKS 1
#define TRANS_RUN_TIME_OVERLAP_CHECK  2
#define TRANS_RETYPE_TYPE_MISMATCH    3
#define TRANS_ENCODE_CHANNEL_ERROR    4
#define TRANS_DECODE_CHANNEL_ERROR    5
#define TRANS_INTERNAL_ERROR          6
/*}}}*/

/*{{{  variables */
extern const trans_params_t *trans_params;
/*}}}*/

/*{{{  routines */
treenode *add_to (treenode *const t, int doptag, treenode *newnode);

/*}}}*/
