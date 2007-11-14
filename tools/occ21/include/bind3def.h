/* $Id: bind3def.h,v 1.1 1996/04/15 10:51:58 djb1 Exp $ */

/*
 *	bind3 declarations
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

/*{{{  global variables */
extern BIT32 vsp, maxvsp;
extern treenode *enclosing_vsp_nptr;
extern BIT32 datasize;
extern int staticlinkused;
extern INT32 loop_weight;
extern int var_base, num_var;
extern treenode *current_proc;

extern int alloc_strategy;
#define ALLOC_BYSCOPE     0x01 /* Allocate by scope rather than usage */
#define ALLOC_NOVSOPT     0x02 /* Don't optimise vars whose vs offset is zero */
#define ALLOC_NOOPERANDS  0x04 /* Don't insert operands in assembly output */
#define ALLOC_NODIVBYSIZE 0x08 /* Don't divide usage count of large objects */
#define ALLOC_NOLINENUMS  0x10 /* Don't insert filename:linenum in assembly output */
#define ALLOC_NOCOMMENTS  0x20 /* Don't insert comments in assembly output */
#define ALLOC_MAXBELOW    0x40 /* Always allocate maximum below w/s size */
#define ALLOC_DEFAULT 0
/*}}}*/

/*{{{  routines */
#ifdef MOBILES
void initmobilespace (void);
int initmobilenewproc (treenode *procdef);
int mobileoutproc (void);
void mobilereplparstart (const int replcount);
void mobilereplparfinish (treenode *spacenode);
int mobilereplparnmapped (void);
int mobilereplparusage (void);
int mobilewordsin (treenode *nptr);
void newmobileinstance (treenode *tptr, INT32 nwords);
void mobilegeninitsequence (treenode *nptr, void (*fn)(const int), int is_namenode);
#endif
void initvsp (INT32 n);
/*void setvsp (BIT32 n);*/ /* now PRIVATE */
INT32 newvs (INT32 n);
void create_var (treenode *nptr);
void create_var_subsidiary (treenode *nptr);
void kill_var (treenode *nptr);
treenode *get_var (int n);
void resurrect_var (treenode *nptr);
int create_immortalvar (treenode *nptr);
treenode *alloc_fixedws (INT32 slot, INT32 size);
void free_fixedwslist (void);
INT32 allocsize (treenode *tptr);
void initallocvars (const BOOL needmap);
void save_wsallocmap (treenode *nptr);
int size_wsallocmap (treenode *nptr);
BYTE *bytesof_wsallocmap (treenode *nptr, int *sizereturn);
void freebytes_wsallocmap (BYTE *data);
void free_wsallocmap (treenode *nptr);
INT32 allocvars (int first_var, int free_paramslots, BOOL inside_par);
void mark_hidden_var (int nodetype, INT32 wsoffset);
void wsmap_hidden_var (int offset, int size, int type);
/*void offsetvars (int first_var, int offset);*/ /* unused */
PUBLIC void *sp_saveallocvars (const BOOL needmap);
PUBLIC void sp_restoreallocvars (void *saved);
void addnamechain (treenode *chain, treenode *item);
void reservelowworkspace (INT32 n);
void reservelowworkspacetagged (INT32 n, int tag);
int newlab (void);
void initlabels (void);
void setusecount(treenode *nptr, INT32 n);
void upusecount (treenode *nptr, INT32 n);
void addusecount (treenode *nptr, INT32 n);
void uploop_weight (INT32 n);
void alloctemp (treenode *tempptr);
treenode *newtempnode (int t, treenode *tptr, int mode, int current_lexlevel);
/*treenode *newtypedtempnode
  (int t, treenode *tptr, int mode, treenode *type);*/ /* now PRIVATE */
treenode *gettemp (treenode *tptr, int mode);
treenode *gettypedtemp (treenode *tptr, int mode, treenode *type);
treenode *getopdtemp (int opdmode, treenode *opd);
void freetemp (treenode *temp);
void freeiftemp (treenode *tptr);
void allocparams (treenode *nptr);
int *save_scope (void);
void restore_scope (int *scope);
treenode *resurrect_specs (treenode *tptr);
void init_mapping (void);

void initalloc (void);
void freevarmap (void);

/*}}}*/
