/* $Id: objtrans.h,v 1.1 1996/04/15 10:52:15 djb1 Exp $ */

/*
 *	object filename translation handling
 *	Copyright (C) 1991 Inmos Limited
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


/*{{{  routines */
wordnode *local_translate_from_internal(const fe_translate_data_t *handle, wordnode *name);

char *translate_descriptor(wordnode **name, const int index,
                           char *descriptor, const int len);
void throwawayname(wordnode *name);
void rememberallnames(libentry_t *libentries);

BOOL setup_translation (wordnode *local_name, const char *outside_name, int olen,
                       SOURCEPOSN locn);

void init_translate(fe_translate_data_t **handle);
/*}}}*/

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (local_translate_from_internal)
#endif
/*}}}*/


