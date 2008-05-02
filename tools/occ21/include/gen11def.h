/* $Id: gen11def.h,v 1.1 1996/04/15 10:52:05 djb1 Exp $ */

/*
 *	gen11 declarations
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
void adjustworkspace (INT32 adjustment);
void setadjust (int lexlevel, INT32 v);
void setsloffset (int lexlevel, INT32 v);
void loadstaticlink (int newlevel);
void loadreplreturnlink (void);
void loadnewvsp (BIT32 offset);
#ifdef MOBILES
void loadnewmsp (BIT32 offset);
#endif

void temp_clear_all_evaluated(void);
void temp_mark_as_evaluated(treenode *nptr);
void temp_unevaluate_all(void);
int simplify(int opdmode, treenode *opd);
void loadconstant(INT32 c);
void loadname (treenode *nptr, INT32 w);
void storeinname (treenode *nptr, INT32 w);
void zero_local_var (treenode *nptr);
void loadnamepointer (treenode *nptr, INT32 w);
void loadelement (treenode *tptr, INT32 word, int regs, BOOL signextend_result);
void storeinelement (treenode *tptr, INT32 word, int regs);
void loadelementpointer (treenode *tptr, INT32 word, int regs);
INT32 loadelementpointeroffset (treenode *tptr, int regs);
void loadopd (int opdmode, treenode *opd, INT32 word);
void storeinopd (int opdmode, treenode *opd, INT32 word, int regs);
#ifdef MOBILES
void gencondfreedynmobile (treenode *const nptr);
void genmobileunpack (treenode *const nptr, const BOOL nochk, const BOOL dimensions);
void loadmsp (void);
void loadmpp (void);
void loadfb (void);
void loadmobilepointer (treenode *var);
void loadmobile_real (treenode *var);
void loadmobile (treenode *var);
void loadmobile_nochk (treenode *var);
void loaddynmobilesize (treenode *const var, const int dim);
void storemobile (treenode *var);
void storemobile_nochk (treenode *var);
void storemobilesize (treenode *const var, const int dim);
void genmobileundefine (treenode *const nptr);
void gencleardynchantype (treenode *const nptr);
void gencleardynproctype (treenode *const nptr);
void gencleardynarray (treenode *const nptr, const BOOL postout);
void gencleardynbarriertype (treenode *const nptr);
void gencondfreedynproctype (treenode *const nptr);
void gencondfreedynchantype (treenode *const nptr);
void gencondfreedynbarriertype (treenode *const nptr, const BOOL resign);
void storehiddentypeof (treenode *tptr, int regs);
void loadhiddentypeof (treenode *tptr, int regs);
void loadhiddentypeptr (treenode *tptr, int regs);

void mapcondfreedynchantype (treenode **nptr);
void genaddrof (treenode *tptr);
void mobilearray_base (treenode *const nptr, treenode **const basetype, int *basebytes);
#endif
/*void loadopdpointer (int opdmode, treenode *opd);*/ /* unused */
#if 0 /* no constructors are passed to the backend any more */
void mapconstructorassign (int destmode,   treenode **destptr,
                           int sourcemode, treenode **sourceptr);
#endif
void mapsimpleassign (int type,
                      int  destmode, treenode **dest,
                      int sourcemode, treenode **source);
void mapassign (treenode **destptr, treenode **sourceptr);
void mapmoveopd (int destmode, treenode **destptr,
                 int sourcemode, treenode **sourceptr);
void moveopd (int destmode, treenode *dest,
              int sourcemode, treenode *source);

void tsimpleassign (int type, int destmode, treenode *dest, int sourcemode,
                   treenode *source, int regs);
void tassign (treenode *dest, treenode *source);
INT32 nameoffsetof (int level);
/*}}}*/

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (nameoffsetof)
#endif
/*}}}*/


