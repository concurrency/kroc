/* $Id: gen3.c,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	code generator - general support routines (overspill from gen2)
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

/*#define DEBUG*/

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "includes.h"
#include "generror.h"
#include "genhdr.h"
#include "gen1def.h"
#include "gen2def.h"
/*}}}*/

/*{{{  PUBLIC BOOL check_aligned (treenode *tptr, int required_align)*/
/*****************************************************************************
 *
 *  check_aligned takes an element 'tptr' and returns TRUE if it is
 *                definitely aligned on a word or half word boundary,
 *                FALSE if it is not, or it is unknown.
 *                required_align will be bytesperword, or maybe 2 on a 32bit
 *****************************************************************************/
PUBLIC BOOL check_aligned (treenode * const tptr, const int required_align)
{
	DEBUG_MSG (("check_aligned: tag is %d\n", TagOf (tptr)));
	switch (TagOf (tptr)) {
	default:
		return FALSE;
	case N_DECL:
	case N_REPL:
		return TRUE;
	case N_VALABBR:
	case N_VALRETYPE:
	case N_VALPARAM:
		/* bug 1129 28/1/91: the following comment about bug 1015 is now
		   irrelevant. We now align ALL constant valabbrs. CON.
		 */
		/* bug 1015: this assumption is a little strong. Constant tables
		   are N_VALABBRs, but are not necessarily word aligned.
		   We have made use of the fact that the item we are checking
		   will always be bytesperword long (ie check_aligned is only called when
		   bytesin(gettype(tptr)) == bytesperword)
		   and we specifically word align constant tables which are a word
		   long (see tconsttable), so all this ends up fine.
		   CON 12/10/90 */
		return (NModeOf (tptr) != NM_POINTER);
		/*{{{  S_ARRAYITEM S_SEGMENTITEM */
	case S_ARRAYITEM:
	case S_SEGMENTITEM:
	case S_RECORDITEM:
		{
			const BOOL arrayitem = TagOf (tptr) == S_ARRAYITEM || TagOf (tptr) == S_RECORDITEM;
			treenode *const base = arrayitem ? ASBaseOf (tptr) : SNameOf (tptr);
			treenode *const exp = arrayitem ? ASExpOf (tptr) : SSubscriptExpOf (tptr);
			const INT32 offset = arrayitem ? ASOffsetOf (tptr) : SOffsetOf (tptr);

			if (check_aligned (base, required_align)) {
				const int b = bytesinscalar (basetype (gettype (tptr)));
				DEBUG_MSG (("check_aligned: base is ok; b is %d, exp==NULL?:%d, offset is %ld\n", b, exp == NULL, offset));
				return (b >= bytesperword) || (exp == NULL && (offset % (int) (required_align / b)) == 0);
			} else
				return FALSE;
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC INT32 wordof (treenode *tptr, int word)*/
/*****************************************************************************
 *
 *  wordof takes a constant node 'tptr' and returns the constant value of
 *         the word'th word of it.
 *
 *****************************************************************************/
PUBLIC INT32 wordof (treenode * const tptr, const int word)
{
	INT32 result = 0;	/* initialised to shut up gcc's optimiser */
	if (word == 0)
		result = (targetintsize == S_INT16) ? LoValOf (tptr) & 0xffff : LoValOf (tptr);
	else if (word == 1)
		result = (targetintsize == S_INT16) ? (LoValOf (tptr) >> 16) & 0xffff : HiValOf (tptr);
	else if (word == 2 && targetintsize == S_INT16)
		result = HiValOf (tptr) & 0xffff;
	else if (word == 3 && targetintsize == S_INT16)
		result = (HiValOf (tptr) >> 16) & 0xffff;
	else
		/*badtag(genlocn, word, "wordof"); */
		geninternal_is (GEN_BAD_OPD, word, "wordof");

	/* bug 1333 19/8/91 */
	if (targetintsize == S_INT16 && ((result & 0x8000) != 0))
		result |= 0xFFFF0000;
	DEBUG_MSG (("\nwordof: returning %lX", result));
	return result;
}

/*}}}*/
/*{{{  PUBLIC BOOL is_typed_mostpos(int type, BIT32 loval, BIT32 hival)*/
/*****************************************************************************
 *
 *  is_typed_mostpos returns TRUE if (loval, hival) represents the MOSTPOS value
 *            of type 'type' : 'type' must be integer or byte type.
 *
 *****************************************************************************/
PUBLIC BOOL is_typed_mostpos (int type, const BIT32 loval, const BIT32 hival)
{
	if (type == S_INT)
		type = targetintsize;
	switch (type) {
	case S_BYTE:
		return (loval == 255);
	case S_INT16:
		return (loval == MOSTPOS_INT16);
	case S_INT32:
		return (loval == MOSTPOS_INT32);
	case S_INT64:
		return (loval == 0xffffffffl && hival == MOSTPOS_INT32);
	}
	return (FALSE);		/* Not reached */
}

/*}}}*/
/*{{{  PUBLIC BOOL is_typed_mostneg(int type, BIT32 loval, BIT32 hival)*/
/*****************************************************************************
 *
 *  is_typed_mostneg returns TRUE if (loval, hival) represents the MOSTNEG value
 *            of type 'type' : 'type' must be integer or byte type.
 *
 *****************************************************************************/
PUBLIC BOOL is_typed_mostneg (int type, const BIT32 loval, const BIT32 hival)
{
	if (type == S_INT)
		type = targetintsize;
	switch (type) {
	case S_BYTE:
		return (loval == 0);
	case S_INT16:
		return (loval == MOSTNEG_INT16);
	case S_INT32:
		return (loval == MOSTNEG_INT32);
	case S_INT64:
		return (loval == 0 && hival == MOSTNEG_INT32);
	}
	return (FALSE);		/* Not reached */
}

/*}}}*/
/*{{{  PUBLIC BOOL is_channel_constructor*/
PUBLIC BOOL is_channel_constructor (treenode * const nptr)
{
	/* returns TRUE if nptr is a channel constructor */
	return DValOf (NDeclOf (nptr)) != NULL;
}

/*}}}*/
/*{{{  PUBLIC BOOL maybevirtualchan(nptr)*/
/*****************************************************************************
 * BOOL maybevirtualchan returns TRUE if the element nptr is a channel which
 *                       may be a virtual channel.
 *****************************************************************************/
PUBLIC BOOL maybevirtualchan (treenode * nptr)
{
	while (nptr != NULL)
		switch (TagOf (nptr)) {
		case S_RECORDSUB:
		case S_RECORDITEM:
		case S_ARRAYSUB:
		case S_ARRAYITEM:
			nptr = ASBaseOf (nptr);
			break;
		case S_SEGMENT:
		case S_SEGMENTITEM:
			nptr = SNameOf (nptr);
			break;
		case N_ABBR:
			nptr = DValOf (NDeclOf (nptr));
			break;
		case T_PREEVALTEMP:
		case T_TEMP:
			nptr = NDeclOf (nptr);
			break;
			/*case N_DECL:        return FALSE; */
		case N_DECL:
			return is_channel_constructor (nptr);	/* bug TS/2039 15/01/93 */
		default:
			return TRUE;
		}
	return TRUE;
}

/*}}}*/
/*{{{  PUBLIC BOOL is_devaccess*/
PUBLIC BOOL is_devaccess (treenode * const tptr)
{
	if (tx_global.hasdevaccess && !T9000_alpha_nodevaccess (&tx_global)) {
		treenode *const nptr = nameof (tptr);
		return (nodetypeoftag (TagOf (nptr)) == NAMENODE)
			&& NVDeviceOf (nptr);
	}
	return FALSE;
}

/*}}}*/
/*{{{  PUBLIC const char *opdmode_string*/
PUBLIC const char *opdmode_string (const int mode)
{
	switch (mode) {
	case P_EXP:
		return "P_EXP";
	case P_TEMP:
		return "P_TEMP";
	case P_TEMPPTR:
		return "P_TEMPPTR";
	case P_PTR:
		return "P_PTR";
	}
	return "???";
}

/*}}}*/
/*{{{  PRIVATE BOOL is_temp*/
PRIVATE BOOL is_temp (const int tag)
{
	return tag == T_TEMP || tag == T_PREEVALTEMP;
}

/*}}}*/
/*{{{  PRIVATE abbrevmode_t abbrevmode*/
/*****************************************************************************
 *
 *  abbrevmode takes an abbreviation specification tree 'tptr' and returns
 *               AM_CONST if the abbreviation is a constant
 *               AM_ISRHS if the abbreviation is the same object as the rhs
 *               AM_PTR   if the abbreviation is a pointer to the rhs
 *               AM_VAL   if the abbreviation contains the value of the rhs
 *
 *****************************************************************************/
PRIVATE abbrevmode_t abbrevmode (treenode * const tptr)
{
	treenode *const name = DNameOf (tptr);
	treenode *const rhs = DValOf (tptr);
	treenode *const rhs_name = nameof (rhs);	/* This is safe to do for any tag value */
	const int my_lexlevel = NLexLevelOf (name);
	const int type = basetype (NTypeOf (name));
	const BOOL val = (TagOf (tptr) == S_VALABBR) || (TagOf (tptr) == S_VALRETYPE);

#if 0
fprintf (stderr, "gen3: abbrevmode: val=%d, type=%d, rhs=", val, type);
printtreenl (stderr, 4, rhs);
#endif
	/*{{{  special case channels when chanaspointer */
	if (chanaspointer && (type == S_CHAN)) {
#if 1
		/* this added after bug TS/2023 07/01/93 */
		/* HOWEVER - the `obvious' doesn't seem to work with RETYPEs */
		if (issimplelocal (rhs, my_lexlevel) && !issimplechan (rhs) && TagOf (tptr) != S_RETYPE)
			return AM_ISRHS;
		else if (TagOf (NTypeOf (name)) == S_CHAN)	/* scalar channel */
			return AM_VAL;	/* bug TS/2023 07/01/93 */
#endif

#if 0				/* bug TS/2023 07/01/93 */
		/* CON - I don't know why all this faffing is here - it seems to me
		   to be simpler to do the obvious! */
		if ((TagOf (rhs) == S_ARRAYITEM) || (TagOf (rhs) == S_SEGMENTITEM) ||	/* bug TS/1783 29/07/92 */
		    ((TagOf (rhs) != S_CONSTRUCTOR) && (ispointer (rhs_name) && !issimplechan (rhs))	/* bug TS/1751 01/07/92 */
		    ))
			return (AM_PTR);
		else
			return (AM_VAL);
#else
		else if (islocal (rhs, my_lexlevel))	/* must already be a pointer */
			return AM_ISRHS;
		else if (TagOf (rhs) == S_CONSTRUCTOR) {
			/*printf("abbrevmode; locn: #%lX, bug TS/2035\n", LocnOf(rhs)); */
			return AM_VAL;	/* bug TS/2035 14/01/93 */
		} else
			return AM_PTR;
#endif
	}
	/*}}} */
	/*{{{  if constant */
	if (isconst (rhs))
		return (AM_CONST);
	/*}}} */
	/*{{{  if issimplelocal */
	if (issimplelocal (rhs, my_lexlevel))
		return is_temp (TagOf (rhs)) ? AM_VAL : AM_ISRHS;	/* INSdi02414 */
	/*}}} */
	/*{{{  constant subscript into array in local w/s (NO LONGER USED) */
#if 0				/* now in issimplelocal */
	if (istargetintsize (basetype (NTypeOf (name))) &&
	    (TagOf (rhs) == S_ARRAYITEM || TagOf (rhs) == S_RECORDITEM) && islocal (rhs_name, my_lexlevel) && !ispointer (rhs_name) &&
	    /* It might have been a constructor put into a temp */
	    !is_temp (TagOf (rhs_name)) && (ASExpOf (rhs) == NULL))
		/* Optimisation to allow word-sized elements at a constant position
		   of an array placed in local workspace to be used directly */
		/* CON - 07/01/93 - now subsumed into 'issimplelocal' */
		return (AM_ISRHS);
#endif
	/*}}} */
	/*{{{  constant segment into array in local w/s, or via pointer + zero */
	/* bug INSdi01513 */
	if (istargetintsize (basetype (NTypeOf (name))) &&
	    (TagOf (rhs) == S_SEGMENTITEM) &&
	    (SSubscriptExpOf (rhs) == NULL) &&
	    islocal (rhs_name, my_lexlevel) && !is_temp (TagOf (rhs_name)) && (!ispointer (rhs_name) || (SOffsetOf (rhs) == 0)))
		/* Optimisation to allow word-sized elements at a constant position
		   of a segment to be used directly */
		return (AM_ISRHS);
	/*}}} */
	/*{{{  scalar VAL abbreviation */
	if (val && fitsinregister (ntypeof (name)))
		return (AM_VAL);
	/*}}} */
	/*{{{  zero subscript into array via pointer */
	if (istargetintsize (basetype (NTypeOf (name))) &&
	    (TagOf (rhs) == S_ARRAYITEM || TagOf (rhs) == S_RECORDITEM) &&
	    islocal (rhs_name, my_lexlevel) && !is_temp (TagOf (rhs_name)) && (ASExpOf (rhs) == NULL) && (ASOffsetOf (rhs) == 0))
		/* Optimisation to allow word-sized elements at zero offset
		   of an array accessed via a local pointer to be used directly */
		return (AM_ISRHS);
	/*}}} */
	/*{{{  miscellaneous odd VAL bits */
	if (val) {
		if (islocal (rhs, my_lexlevel))
			/* must already be a pointer */
			/* bug TS/1692 12/05/92 */
			return is_temp (TagOf (rhs)) ? AM_PTR : AM_ISRHS;	/* INSdi02414 */
		else if (isaddressable (rhs))
			return (AM_PTR);
		else
			return (AM_VAL);
	}
	/*}}} */
	/*{{{  miscellaneous odd VAR bits */
	{
		if (type == S_TIMER) {
			return (AM_ISRHS);			/* 23/4/90 CO'N; bug 287; Stops it creating a pointer */
		} else if (islocal (rhs, my_lexlevel)) {	/* must already be a pointer */
			return (AM_ISRHS);			/* bug TS/1692 12/05/92 */
		} else {
			return (AM_PTR);
		}
	}
	/*}}} */
}

/*}}}*/

/*{{{  PUBLIC abbrevmode_t be_abbrevmode*/
PUBLIC abbrevmode_t be_abbrevmode (treenode * const tptr)
/* This is a private backend's copy of abbrevmode, so that any fixes
   which are specific to this backend can be put here.
*/
{
	const abbrevmode_t am = abbrevmode (tptr);

	if (am == AM_ISRHS) {
		treenode *const rhs = DValOf (tptr);
		if ((TagOf (rhs) == N_REPL) && parrepl (TagOf (NDeclOf (rhs)))) {
			return AM_VAL;	/* bug TS/2066 29/01/93 */
		}
	}
#if 0
fprintf (stderr, "be_abbrevmode: returning: %s for %s\n", abbrevmode_string(am), WNameOf(NNameOf(DNameOf(tptr))));
#endif
	return am;
}

/*}}}*/
