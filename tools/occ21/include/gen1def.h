/* $Id: gen1def.h,v 1.1 1996/04/15 10:52:05 djb1 Exp $ */

/*
 *	gen1 declarations
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
extern SOURCEPOSN genlocn;
extern BIT32 vspoffset;
extern BIT32 fbpoffset;
extern int fbp_lexlevel;	/* not per REPL-PAR, so need to know what lexlevel it lives at */
#ifdef MOBILES
extern BIT32 mspoffset;
extern BIT32 mppoffset;
extern int mpp_lexlevel;	/* like fbp_lexlevel */
#endif
extern BIT32 constptr;              /* Offset of constant pointer in workspace */
extern wordnode *tempname_p;
extern int instancedlevel;
extern int be_lexlevel;
extern BOOL insidealtguard; /* TRUE when we are mapping, or code generating
                              an alternative Boolean guard. */
extern int alt_ds;
extern BOOL inside_asm;     /* Used to prevent error flag checking inside ASM */
extern BOOL disable_csub0;  /* Used to prevent checking when disabling ALTs */

extern BOOL use_bsub_not_adc; /* For addressing operations */
extern int pripar_trap_offset;
extern treenode *current_routine_nptr;

/*}}}*/

/*{{{  routines */
void tstop (treenode *tptr);
treenode *tspecs (treenode *tptr);
void tdespecs (treenode *tptr);
/*void tspecification (treenode *tptr);*/ /* now PRIVATE */
void tpreexp (treenode *tptr);
void tloadconsttable(void);
void tprocess (treenode *tptr);
void tmain (treenode *tptr);
void beinit (void);
/*void bereinit (int l);*/
#define bereinit() beinit()
void trepl (treenode *tptr, void (*tbody)(treenode *, void *, INT32),  
	    void *p1, INT32 p2, BOOL mark_endrepl);
void tprofcountupdate(INT32 index);

void tinitbarrier (treenode *bar);
void tfreebarrier (treenode *bar);
void tsync (treenode *bar);
void tresign (treenode *bar, const int n);
void tresign_exp (treenode *bar, treenode *n, int adjust);
void tenroll (treenode *bar, const int n);
void tenroll_exp (treenode *bar, treenode *n, const int adjust);
/*}}}*/
