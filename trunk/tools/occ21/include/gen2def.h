/* $Id: gen2def.h,v 1.1 1996/04/15 10:52:06 djb1 Exp $ */

/*
 *	gen2 declarations
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

/*{{{  constants*/
#define BOOL_CHECK_MASK      0x0002
#define BYTE_CHECK_MASK      0x0100
#define INT16_CHECK_MASK     0x8000
#define INT32_CHECK_MASK 0x80000000
/*}}}  */

/*{{{  routines*/
wordnode *processlibname (const char *string, int suffix_len, const char *new_suffix);
BOOL pdinline (int pdno);
wordnode *pdlibname (wordnode * pdname, int pdno);
int pdparams (const treenode *tptr);
int implicitparams (int op, int type);
/*void declare_sc_entries (treenode *nptr, INT32 prevcodesize);*/
treenode *constantmatch (treenode *c, treenode *clist);
BOOL cancauseerror (treenode *tptr);
BOOL isconversion (treenode *tptr);
BOOL needtemptoload (int opdmode, treenode *opd);
treenode *makeconversion(int sourcetype, int desttype, treenode *source,
                         int mode, SOURCEPOSN locn);
treenode *makedopfunction (treenode *tptr);
treenode *firstresultof (treenode *tptr);
treenode *nextresultof (treenode *tptr);
wordnode *libcallstring (int op, int type);
BOOL istrueguard (treenode *tptr);
BOOL isfalseguard (treenode *tptr);
BOOL isskipbody (treenode *tptr);
BOOL complexinitialise (treenode *typeptr);
BOOL channel_basetype(treenode *type);
BOOL timerguardinalt (treenode *tptr);
/*BOOL fitsin (int sourcetype, int desttype);*/ /* unused */
BOOL commutes (treenode *tptr);
BOOL issimpleopd (int opdmode, treenode *opd);
BOOL issimplelocalopd (int opdmode, treenode *opd);
BOOL isaddressableopd (int opdmode, treenode *opd);
BOOL iscomplexopd (int opdmode, treenode *opd);
BOOL preeval (int mode, treenode *opd);
BOOL hasgreaterrange (int type1, int type2);
BOOL issignedtype (int type);
BIT32 checkmask (int type);
BIT32 typemask (int type);
treenode *consttableof (treenode *tptr);
/* BOOL separatelycompiled  (treenode *nptr); gone to frontend */
BOOL isinlibrary (treenode *nptr);
/*void applytovalofs (treenode *tptr, void (*p)();*/ /* unused */
int ptrmodeof (int mode);
int tempmodeof (int mode);
BOOL usedinopd (int opdmode, treenode *opd, treenode *exptree, int my_lexlevel);
BOOL isconstexpnd (treenode *tptr);
BOOL isconstopd (int opdmode, treenode *opd);
BOOL directstore (int destmode, treenode *dest, int my_lexlevel);
BOOL directload (int sourcemode, treenode *source, int my_lexlevel);
int check_aligned (treenode *tptr, int required_align);
INT32 wordof (treenode *tptr, int word);
BOOL is_typed_mostpos (int type, BIT32 loval, BIT32 hival);
BOOL is_typed_mostneg (int type, BIT32 loval, BIT32 hival);
/*BOOL ismint (INT32 c);*/
#define ismint(x) (is_typed_mostneg(S_INT, (x), 0))
BOOL maybevirtualchan (treenode *nptr);
BOOL is_channel_constructor(treenode *nptr);
BOOL is_devaccess(treenode *tptr);
const char *opdmode_string(int mode);

abbrevmode_t be_abbrevmode (treenode *tptr);

BOOL multiloadaddr (treenode *source);

/*}}}  */

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (isconversion)
#pragma IMS_nosideeffects (firstresultof)
#pragma IMS_nosideeffects (nextresultof)
#pragma IMS_nosideeffects (implicitparams)
#pragma IMS_nosideeffects (istrueguard)
#pragma IMS_nosideeffects (isfalseguard)
#pragma IMS_nosideeffects (isskipbody)
#pragma IMS_nosideeffects (issimplelocal)
#pragma IMS_nosideeffects (issimpleopd)
#pragma IMS_nosideeffects (preeval)
#pragma IMS_nosideeffects (issignedtype)
#pragma IMS_nosideeffects (consttableof)
#pragma IMS_nosideeffects (isinlibrary)
#pragma IMS_nosideeffects (ptrmodeof)
#pragma IMS_nosideeffects (tempmodeof)
#pragma IMS_nosideeffects (is_typed_mostpos)
#pragma IMS_nosideeffects (is_typed_mostneg)
#pragma IMS_nosideeffects (maybevirtualchan)
#pragma IMS_nosideeffects (is_channel_constructor)
#pragma IMS_nosideeffects (is_devaccess)
#pragma IMS_nosideeffects (opdmode_string)
/*#pragma IMS_nosideeffects (ismint)*/
#endif
/*}}}  */

