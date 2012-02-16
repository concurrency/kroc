/* $Id: usedef.h,v 1.1 1996/04/15 10:52:24 djb1 Exp $ */

/*
 *	use definitions visible to rest of compiler
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

/*{{{  routines */
BOOL local_paraminputoroutputon (treenode *n, BOOL inputnotoutput);
BOOL local_isafreevarof         (treenode *v, treenode *n);

void subscripts_accessed      (treenode *n, INT32 *first, INT32 *last, BOOL check_subscripts);
int chan_use         (treenode *n, treenode *name);
BOOL name_used_in_exp (treenode *name, treenode *tptr);

void alias_and_usage_check (treenode *n, BOOL check_aliases, BOOL check_usage, FILE *fptr);
void formalmodelcheck (treenode *n, BOOL check_formalmodels, const char *filename, fe_handle_t *const handle);

varlist *freevarsin           (treenode *n, int scope, varlist *vlist,
                                     int usage_check);
void printfreelist   (FILE *fptr, varlist *list);
varlist *merge       (varlist *vlist1, int scope, varlist *vlist2);

void use_mark_free_vars(treenode *tptr, const BOOL check_usage);
void use_walk_free_vars(treenode *nptr,
                        int (*fn)(treenode **, void *), void *);
void use_copy_free_vars(treenode *orig_nptr, treenode *new_nptr);

char *use_udvstatestringof (treenode *const nptr);
/*}}}*/

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (local_paraminputoroutputon)
#pragma IMS_nosideeffects (local_isafreevarof)
#endif
/*}}}*/


