/* $Id: tran2.c,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	extra functions for tree transformer
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


/*#define DEBUG*/

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <limits.h>
#include "midinc.h"
#include "occompfe.h"
#include "harndef.h"

#include "trandef.h"
#include "tran1def.h"

/*}}}  */

/*{{{  definitions */
#define MAX_REGRESULTS 3
/*}}}  */
/*{{{  constants used when deciding how to load a constant */
/* If a constant takes more than CONST_MAX_BYTES to load, then place it in a
   constant table */
/* There are different tradeoffs depending on instruction timings:
   bug 1371 15/8/91
*/
#define CONST_MAX_BYTES       6
#define CONST_MAX_BYTES_T9000 4
#define CONST_MAX_BYTES_SPACE 6 /* when optimising for space */	/* bug TS/1504 07/04/92 */

#define ADCNEG_INT16 (-32753)	/* 0xFFFF800F */
#define ADCNEG_INT32 (-2146435073)	/* 0x800FFFFF */

#define LDNLPNEG_INT16 (-32705)	/* 0xFFFF803F */
#define LDNLPNEG_INT32 (-2143289345)	/* 0x803FFFFF */

#define LDINF_NEG_WORD (0x7F7FC000)
#define LDINF_POS_WORD (0x7F803FFF)
#define LDINF_NEG_BYTE (0x7F7FF000)
#define LDINF_POS_BYTE (0x7F800FFF)
/*}}}  */

/*{{{  PUBLIC BOOL islocal */
/*****************************************************************************
 *
 *  islocal returns TRUE if 'tptr' is a variable and it is local.
 *
 *****************************************************************************/
PUBLIC BOOL islocal (const treenode * const tptr, const int my_lexlevel)
{
	/* We return FALSE for subscripts or segments */
	switch (TagOf (tptr)) {
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_DECL:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_REPL:
	case T_TEMP:
	case T_PREEVALTEMP:
	case S_FNFORMALRESULT:
	case S_FNACTUALRESULT:
	case S_HIDDEN_PARAM:
	case S_PARAM_VSP:
	case S_PARAM_FB:
	case S_PARAM_WS:
#ifdef MOBILES
	case S_PARAM_MSP:
	case S_PARAM_MPP:
	case S_HIDDEN_TYPE:
#endif
	case S_PARAM_STATICLINK:
		return ((NLexLevelOf (tptr) == my_lexlevel) && (NModeOf (tptr) != NM_PLACED));
	default:
		return (FALSE);
	}
}

/*}}}  */
/*{{{  PRIVATE BOOL set_vectorspace */
PRIVATE BOOL set_vectorspace (treenode * const nptr)
{
	/* Put object in vector space if
	   i.   vector space is enabled
	   ii.  the object is large enough to warrant it
	   We used to use this criteria too: bug TS/2039 15/01/93:
	   iii. we don't want to load pointers to the object all the time
	   eg. [4]INT16, [3]BYTE ...
	 */
	/* Modified 29/1/91 to put ALL objects smaller than a word into workspace */

	/*{{{  current version */
	/* bug TS/2039 15/01/93; simplify this again! */
	treenode *const type = follow_user_type (NTypeOf (nptr));
	const BOOL invecspace = vsenabled && (((TagOf (type) == S_ARRAY)
					       && (basetype (type) != S_TIMER)
					       && (bytesin (type) > MAX_WS_ARRAY_BYTES))
#ifdef OCCAM2_5
					      || ((TagOf (type) == S_RECORD)
						  && (bytesin (type) > MAX_WS_RECORD_BYTES))
#endif
		);
	/*}}}  */

	/* don't put things with type-attribute PLACED into vecspace.. */
	if ((TagOf (nptr) == N_DECL) && (nodetypeoftag (TagOf (type)) == TYPENODE) && (TypeAttrOf (type) & TypeAttr_placed)) {
#if 0
fprintf (stderr, "set_vectorspace: setting POINTER on nptr = ");
printtreenl (stderr, 4, nptr);
fprintf (stderr, "set_vectorspace: type tree, typeattrof(type) = 0x%8.8x, type = ", (unsigned int)TypeAttrOf (type));
printtreenl (stderr, 4, type);
#endif
		SetNMode (nptr, NM_POINTER);
		return FALSE;
	}
	SetNMode (nptr, invecspace ? NM_VECSPACE : NM_WORKSPACE);
	return invecspace;
}
/*}}}  */
#ifdef MOBILES
/*{{{  PRIVATE void sub_setmobilespace (treenode *const tptr)*/
/* prototype */
PRIVATE BOOL set_mobilespace (treenode *const nptr);

PRIVATE void sub_setmobilespace (treenode *const tptr)
{
	treenode *type;

	switch (TagOf (tptr)) {
	case S_ARRAY:
		type = follow_user_type (ARTypeOf (tptr));
		if (TagOf (type) == S_MOBILE) {
			type = MTypeOf (type);
			switch (TagOf (type)) {
			case N_TYPEDECL:
				SetNMode (type, NM_POINTER);
				break;
			}
		}
		break;
	}
#if 0
printf ("set_submobilespace: tptr (after munging) = ");
printtree (stdout, 4, tptr);
printf ("\n");
#endif
	return;
}
/*}}}*/
/*{{{  PRIVATE BOOL set_mobilespace (treenode *const nptr)*/
PRIVATE BOOL set_mobilespace (treenode *const nptr)
{
	/* object is a pointer if MOBILE */

	treenode *const type = follow_user_type (NTypeOf (nptr));
	if (TagOf (type) == S_MOBILE) {
		SetNMode (nptr, NM_POINTER);
		/* better examine the rest of the type to set MOBILEs where appropiate too */
		sub_setmobilespace (MTypeOf (type));
		/* not in mobilespace if this is a dynamic mobile or a record containing channels */
		if ((TagOf (MTypeOf (type)) == S_ARRAY) && ((ARDimLengthOf (MTypeOf (type)) == NULL) || (TypeAttrOf (type) & TypeAttr_dynmobile))) {
			return FALSE;
		} else if (TagOf (MTypeOf (type)) == N_PROCTYPEDECL) {
			return FALSE;
		} else if (TagOf (MTypeOf (type)) == S_FULLBARRIER) {
			return FALSE;		/* these live in dynamic mobilespace */
		} else if (TagOf (MTypeOf (type)) == S_RECORD) {
			treenode *field = ARTypeOf (MTypeOf (type));

#if 0
fprintf (stderr, "set_mobilespace: (found RECORD) field =");
printtreenl (stderr, 4, field);
#endif
			while (field && (TagOf (field) == S_DECL)) {
				treenode *type = NTypeOf (DNameOf (field));

				if (TagOf (type) == S_CHAN) {
					return FALSE;
				}
				field = DBodyOf (field);
			}
			return TRUE;
		} else {
			return TRUE;
		}
	}
	return FALSE;
}
/*}}}*/
#endif	/* MOBILES */
/*{{{  PUBLIC BOOL isinvectorspace */
/*{{{  comment */
/*****************************************************************************
 *
 *  isinvectorspace takes a namenode, nptr, and returns TRUE if the object is
 *                  in vector space, FALSE otherwise.
 *
 *****************************************************************************/
/*}}}  */
PUBLIC BOOL isinvectorspace (treenode * const nptr)
{
	switch (TagOf (nptr)) {
		/*{{{  N_DECL */
	case N_DECL:
		switch (NModeOf (nptr)) {
		case NM_DEFAULT:
			return set_vectorspace (nptr);
		case NM_VECSPACE:
			return (TRUE);
		default:
			return (FALSE);
		}
		/*}}}  */
		/*{{{  N_ABBR N_VALABBR N_RETYPE N_VALRETYPE */
	case N_ABBR:
	case N_VALABBR:
	case N_RETYPE:
	case N_VALRETYPE:
		switch (NModeOf (nptr)) {
			/*{{{  NM_DEFAULT          return(invecspace) */
		case NM_DEFAULT:
			{
				const abbrevmode_t am = trans_params ? trans_params->abbrevmode_fn (NDeclOf (nptr)) : AM_ISRHS;

				if (am == AM_VAL || am == AM_COPYINOUT) {
					return set_vectorspace (nptr);
				} else {
					/*printf("isinvectorspace: abbrevmode is %d on %s\n",
					   am, WNameOf(NNameOf(nptr))); */
					SetNMode (nptr, NM_WORKSPACE);
					return FALSE;
				}
			}
			/*}}}  */
		case NM_VECSPACE:
			return (TRUE);
		default:
			return (FALSE);
		}
		/*}}}  */
		/*{{{  other variables */
	case T_TEMP:
	case T_PREEVALTEMP:
	case T_RESERVEDWS:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
	case N_REPL:
		return (FALSE);
		/*}}}  */
	default:
		badtag (LocnOf (nptr), TagOf (nptr), "isinvectorspace");
	}
	return (FALSE);		/* not reached */
}

/*}}}  */
#ifdef MOBILES
/*{{{  PUBLIC BOOL isinmobilespace (treenode *const nptr)*/
PUBLIC BOOL isinmobilespace (treenode *const nptr)
{
	switch (TagOf (nptr)) {
	case N_DECL:
		/* chk has already examined the type to make sure MOBILES are outermost */
		return set_mobilespace (nptr);
	case N_ABBR:
	case N_VALABBR:
	case N_RETYPE:
	case N_VALRETYPE:
		return FALSE;
	case T_TEMP:
	case T_PREEVALTEMP:
	case T_RESERVEDWS:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
	case N_REPL:
		return FALSE;
	default:
		badtag (LocnOf (nptr), TagOf (nptr), "isinmobilespace");
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynmobilechantypetype (treenode *const tptr)*/
PUBLIC BOOL isdynmobilechantypetype (treenode *const tptr)
{
	treenode *type;

	if (!tptr) {
		return FALSE;
	}
	type = tptr;
	if (TagOf (type) == N_TYPEDECL) {
		type = follow_user_type (type);
	}
	if (TagOf (type) == S_ANYCHANTYPE) {
		return TRUE;
	}
	if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_RECORD)) {
		/* MOBILE record, check for channel presence */
		treenode *field = ARTypeOf (MTypeOf (type));
		BOOL havechans = FALSE;

		while (field && (TagOf (field) == S_DECL)) {
			treenode *item = NTypeOf (DNameOf (field));

			if (TagOf (item) == S_CHAN) {
				havechans = TRUE;
				break;	/* while () */
			}
			field = DBodyOf (field);
		}
		return havechans;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynmobilechantype (treenode *nptr)*/
PUBLIC BOOL isdynmobilechantype (treenode *nptr)
{
	treenode *tt;

	if (!nptr) {
		return FALSE;
	}
	if ((TagOf (nptr) == T_TEMP) || (TagOf (nptr) == T_PREEVALTEMP)) {
		nptr = NDeclOf (nptr);
	}
	switch (TagOf (nptr)) {
	case S_ARRAYSUB:
	case S_ARRAYITEM:
	case S_RECORDSUB:
	case S_RECORDITEM:
		tt = gettype (nptr);
#if 0
fprintf (stderr, "isdynmobilechantype: nptr = ");
printtreenl (stderr, 4, nptr);
fprintf (stderr, "isdynmobilechantype: gettype(nptr) = ");
printtreenl (stderr, 4, tt);
#endif
		if (tt && (isdynmobilechantypetype (tt))) {
			return TRUE;
		}
		break;
	case N_DECL:
	case N_PARAM:
	case N_ABBR:
	case N_RESULTPARAM:
		{
			treenode *type = NTypeOf (nptr);

			if (TagOf (type) == N_TYPEDECL) {
				type = follow_user_type (type);
			}
			if (TagOf (type) == S_ANYCHANTYPE) {
				return TRUE;
			}
			if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_RECORD)) {
				/* MOBILE record, check for channel presence */
				treenode *field = ARTypeOf (MTypeOf (type));
				BOOL havechans = FALSE;

				while (field && (TagOf (field) == S_DECL)) {
					treenode *item = NTypeOf (DNameOf (field));

					if (TagOf (item) == S_CHAN) {
						havechans = TRUE;
						break;	/* while () */
					}
					field = DBodyOf (field);
				}
				return havechans;
			}
		}
		return FALSE;
	case S_UNDEFINED:
		return isdynmobilechantype (OpOf (nptr));
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynmobilebarriertype (treenode *const tptr)*/
/*
 *	returns TRUE if this is a dynamic MOBILE BARRIER type
 */
PUBLIC BOOL isdynmobilebarriertype (treenode *const tptr)
{
#if 0
fprintf (stderr, "tran2: isdynmobilebarriertype(): tptr = ");
printtreenl (stderr, 4, tptr);
#endif
	if (!tptr) {
		return FALSE;
	}
	if ((TagOf (tptr) == S_MOBILE) && (TagOf (MTypeOf (tptr)) == S_FULLBARRIER)) {
		return TRUE;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynmobilebarrier (treenode *nptr)*/
/*
 *	returns TRUE if this is a dynamic MOBILE BARRIER
 */
PUBLIC BOOL isdynmobilebarrier (treenode *nptr)
{
	if (!nptr) {
		return FALSE;
	}
	if (TagOf (nptr) == S_UNDEFINED) {
		nptr = OpOf (nptr);
	}
	if ((TagOf (nptr) == T_TEMP) || (TagOf (nptr) == T_PREEVALTEMP)) {
		nptr = NDeclOf (nptr);
	}
	switch (TagOf (nptr)) {
	case S_ARRAYSUB:
	case S_ARRAYITEM:
	case S_RECORDSUB:
	case S_RECORDITEM:
		{
			treenode *tt = gettype (nptr);

			if (tt && isdynmobilebarriertype (tt)) {
				return TRUE;
			}
		}
		break;
	case N_DECL:
	case N_ABBR:
	case N_PARAM:
	case N_RESULTPARAM:		/* bit odd for barriers, but.. */
		{
			treenode *type = NTypeOf (nptr);

			if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_FULLBARRIER)) {
				return TRUE;
			}
		}
		break;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynmobileproctypetype (treenode *const tptr)*/
/*
 *	returns TRUE if this is a dynamic MOBILE process type
 */
PUBLIC BOOL isdynmobileproctypetype (treenode *const tptr)
{
	if (!tptr) {
		return FALSE;
	}
	if (TagOf (tptr) == S_ANYPROCTYPE) {
		return TRUE;
	}
	if (((TagOf (tptr) == S_MOBILE) && (TagOf (MTypeOf (tptr)) == N_PROCTYPEDECL)) || (TagOf (tptr) == N_PROCTYPEDECL)) {
		return TRUE;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynmobileproctype (treenode *const nptr)*/
/*
 *	returns TRUE if this is a dynamic MOBILE process
 */
PUBLIC BOOL isdynmobileproctype (treenode *nptr)
{
	if (!nptr) {
		return FALSE;
	}
	if ((TagOf (nptr) == T_TEMP) || (TagOf (nptr) == T_PREEVALTEMP)) {
		nptr = NDeclOf (nptr);
	}
	switch (TagOf (nptr)) {
	case S_ARRAYSUB:
	case S_ARRAYITEM:
	case S_RECORDSUB:
	case S_RECORDITEM:
		{
			treenode *tt = gettype (nptr);

			if (tt && isdynmobileproctypetype (tt)) {
				return TRUE;
			}
		}
		break;
	case N_DECL:
	case N_PARAM:
	case N_ABBR:
	case N_RESULTPARAM:
		{
			treenode *type = NTypeOf (nptr);

#if 0
fprintf (stderr, "tran2: isdynmobileproctype NAMENODEish: type =");
printtreenl (stderr, 4, type);
#endif
			if (TagOf (type) == S_ANYPROCTYPE) {
				return TRUE;
			}
			if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == N_PROCTYPEDECL)) {
				return TRUE;
			}
		}
		return FALSE;
	case S_UNDEFINED:
		return isdynmobileproctype (OpOf (nptr));
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynmobilearray (treenode *const nptr)*/
PUBLIC BOOL isdynmobilearray (treenode *nptr)
{
	if (!nptr) {
		/* this can happen sometimes */
		return FALSE;
	}
	if ((TagOf (nptr) == T_TEMP) || (TagOf (nptr) == T_PREEVALTEMP)) {
		nptr = NDeclOf (nptr);
	}
	switch (TagOf (nptr)) {
	case S_RECORDSUB:
	case S_RECORDITEM:
		return (ismobile (ASBaseOf (nptr)) && isdynmobilearray (ASIndexOf (nptr)));
	case S_ARRAYSUB:
	case S_ARRAYITEM:
		{
			int dimcount = 0;
			treenode *n = nptr;

			while ((TagOf (n) == S_ARRAYSUB) || (TagOf (n) == S_ARRAYITEM)) {
				dimcount++;
				n = ASBaseOf (n);
			}
			if (ismobile (n)) {
				treenode *basetype = follow_user_type (gettype (n));

#if 0
fprintf (stderr, "isdynmobilearray: ARRAYSUB of something MOBILE.  dimcount = %d, basetype =", dimcount);
printtreenl (stderr, 4, basetype);
#endif
				while (dimcount > 0) {
					if (TagOf (basetype) == S_MOBILE) {
						basetype = MTypeOf (basetype);
					}
					if (TagOf (basetype) != S_ARRAY) {
						return FALSE;
					}
					while ((TagOf (basetype) == S_ARRAY) && (dimcount > 0)) {
						basetype = ARTypeOf (basetype);
						dimcount--;
					}
					basetype = follow_user_type (basetype);
				}
				/* TRUE if basetype is a dynamic mobile array! */
				if ((TagOf (basetype) == S_MOBILE) && (TagOf (MTypeOf (basetype)) == S_ARRAY)) {
					treenode *atype = MTypeOf (basetype);

					if ((ARDimLengthOf (atype) == NULL) || (TagOf (ARDimLengthOf (atype)) == S_NTH_DIMENSION)) {
#if 0
fprintf (stderr, "isdynmobilearray: returning TRUE!\n");
#endif
						return TRUE;
					}
				}
			}
		}
		return FALSE;
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_FIELD:
	case N_ABBR:
	case T_TEMP:
	case T_PREEVALTEMP:
		{
			treenode *type = NTypeOf (nptr);
			treenode *orig_type = type;

			if (TagOf (type) == N_TYPEDECL) {
				type = follow_user_type (type);
			}
			if (TagOf (type) == S_MOBILE) {
				/* we have to adjust the current type-tree as well as build a new one in here -- whoever is calling us might have
				 * the current type-tree in a variable.  Hence the check to see if an existing NTH_DIMENSION'd tree is really ours
				 * or is a left-over from some time previous.
				 */
				if ((TagOf (MTypeOf (type)) == S_ARRAY) && ((ARDimLengthOf (MTypeOf (type)) == NULL) ||
							((TagOf (ARDimLengthOf (MTypeOf (type))) == S_NTH_DIMENSION) && (LeftOpOf (ARDimLengthOf (MTypeOf (type))) != nptr)))) {
					SetTypeAttr (type, TypeAttrOf (type) | TypeAttr_dynmobile);
					if (1 || (TagOf (nptr) == N_DECL)) {
						/* walk down dimension tree inserting SIZE nodes -- only on DECL nodes though (why?? -- because other types have it already..) */
						treenode *atype = MTypeOf (type);
						treenode *newtypetree, **newtypeptr;
						int dimension = 1;

#if 0
fprintf (stderr, "isdynmobilearray: Inserting ARDimLengthOf() node into MOBILE type-tree\n");
#endif
						newtypetree = newtypenode (S_MOBILE, NOPOSN, NULL, NULL);
						SetTypeAttr (newtypetree, TypeAttrOf (type));				/* grab dynmobile tag..! */
						newtypeptr = MTypeAddr (newtypetree);

						while (TagOf (atype) == S_ARRAY) {
							*newtypeptr = newtypenode (S_ARRAY, LocnOf(atype), NULL, NULL);
							SetARDimLength (*newtypeptr, newdopnode (S_NTH_DIMENSION, NOPOSN, nptr, newconstant (dimension), S_INT));
							SetARDimLength (atype, newdopnode (S_NTH_DIMENSION, NOPOSN, nptr, newconstant (dimension), S_INT));
							SetARDim (*newtypeptr, ARDimOf (atype));
							SetTypeAttr (*newtypeptr, TypeAttrOf (atype));

							atype = ARTypeOf (atype);
							newtypeptr = ARTypeAddr (*newtypeptr);
							dimension++;
						}

						/* link in base type */
						*newtypeptr = atype;

						/* oki, if we followed a user-defined type to get here, put it back! */
						if (TagOf (orig_type) == N_TYPEDECL) {
							treenode *nc = newnamenode (N_TYPEDECL, NOPOSN, NNameOf (orig_type), newtypetree, NULL, NLexLevelOf (orig_type), NScopeOf (orig_type), NModeOf (orig_type));

							SetNTypeAttr (nc, NTypeAttrOf (orig_type));
							SetNType (nptr, nc);
						} else {
							SetNType (nptr, newtypetree);
						}
#if 0
fprintf (stderr, "isdynmobilearray: New type-tree is");
printtreenl (stderr, 4, NTypeOf (nptr));
#endif
					}
					return TRUE;
				} else if ((TypeAttrOf (type) & TypeAttr_dynmobile) && (TagOf (MTypeOf (type)) == S_ARRAY)) {
					return TRUE;
				}
			}
			return FALSE;
		}
	case S_UNDEFINED:
		return isdynmobilearray (OpOf (nptr));
	}
#if 0
fprintf (stderr, "isdynmobilearray: returning FALSE from nptr =");
printtreenl (stderr, 4, nptr);
#endif
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynmobilearraypiece (treenode *const nptr)*/
PUBLIC BOOL isdynmobilearraypiece (treenode *nptr)
{
	if (!nptr) {
		/* this can happen sometimes */
		return FALSE;
	}

	if (isdynmobilearray (nptr)) {
		return TRUE;
	}
	if ((TagOf (nptr) == T_TEMP) || (TagOf (nptr) == T_PREEVALTEMP)) {
		nptr = NDeclOf (nptr);
	}

	switch (TagOf (nptr)) {
	case S_RECORDSUB:
	case S_RECORDITEM:
		return (ismobile (ASBaseOf (nptr)) && isdynmobilearray (ASIndexOf (nptr)));
	case S_ARRAYSUB:
	case S_ARRAYITEM:
		{
			int dimcount = 0;
			treenode *n = nptr;

			while ((TagOf (n) == S_ARRAYSUB) || (TagOf (n) == S_ARRAYITEM)) {
				dimcount++;
				n = ASBaseOf (n);
			}
			if (isdynmobilearray (n)) {
				treenode *basetype = follow_user_type (gettype (n));

#if 0
fprintf (stderr, "isdynmobilearraypiece: ARRAYSUB of something MOBILE.  dimcount = %d, basetype =", dimcount);
printtreenl (stderr, 4, basetype);
#endif
				while (dimcount > 0) {
					if (TagOf (basetype) == S_MOBILE) {
						basetype = MTypeOf (basetype);
					}
					if (TagOf (basetype) != S_ARRAY) {
						return FALSE;
					}
					while ((TagOf (basetype) == S_ARRAY) && (dimcount > 0)) {
						basetype = ARTypeOf (basetype);
						dimcount--;
					}
					basetype = follow_user_type (basetype);
				}
				/* TRUE if basetype is a dynamic mobile array! -- or if it is a subscript of one, but not a total subscript */
				if (TagOf (basetype) == S_ARRAY) {
					return TRUE;
				} else if (isdynmobilearraytype (basetype)) {
					return TRUE;
				}
			}
		}
		return FALSE;
	case S_UNDEFINED:
		return isdynmobilearraypiece (OpOf (nptr));
	}
#if 0
fprintf (stderr, "isdynmobilearraypiece: returning FALSE from nptr =");
printtreenl (stderr, 4, nptr);
#endif
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynmobilearraytype (treenode *const tptr)*/
PUBLIC BOOL isdynmobilearraytype (treenode *const tptr)
{
	treenode *type;

	if (!tptr) {
		/* this can happen sometimes */
		return FALSE;
	}
	type = tptr;

	if (TagOf (type) == N_TYPEDECL) {
		type = follow_user_type (type);
	}
	if (TagOf (type) == S_MOBILE) {
		/* we have to adjust the current type-tree as well as build a new one in here -- whoever is calling us might have
		 * the current type-tree in a variable.  Hence the check to see if an existing NTH_DIMENSION'd tree is really ours
		 * or is a left-over from some time previous.
		 */
		if ((TagOf (MTypeOf (type)) == S_ARRAY) && ((ARDimLengthOf (MTypeOf (type)) == NULL) ||
					((TagOf (ARDimLengthOf (MTypeOf (type))) == S_NTH_DIMENSION)))) {
			SetTypeAttr (type, TypeAttrOf (type) | TypeAttr_dynmobile);

			return TRUE;
		} else if ((TypeAttrOf (type) & TypeAttr_dynmobile) && (TagOf (MTypeOf (type)) == S_ARRAY)) {
			return TRUE;
		}
	}
#if 0
fprintf (stderr, "isdynmobilearray: returning FALSE from type =");
printtreenl (stderr, 4, type);
#endif
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC INT dynmobiledimensioncount (treenode *const nptr)*/
PUBLIC INT dynmobiledimensioncount (treenode *const nptr)
{
	treenode *n = nptr;

	if (!n) {
		return 0;
	}
#if 0
fprintf (stderr, "dynmobiledimensioncount: nptr = ");
printtreenl (stderr, 4, nptr);
#endif
	if ((TagOf (n) == T_TEMP) || (TagOf (n) == T_PREEVALTEMP)) {
		n = NDeclOf (n);
	}

	switch (TagOf (n)) {
	case S_RECORDITEM:
		return dynmobiledimensioncount (ASIndexOf (n));
		break;
	case S_ARRAYITEM:
		{
			int dimcount = 0;
			treenode *type = n;

			while (TagOf (type) == S_ARRAYITEM) {
				dimcount++;
				type = ASBaseOf (type);
			}
			type = follow_user_type (gettype (type));
			/* oki, should have some MOBILE base-type */
			if (TagOf (type) != S_MOBILE) {
#if 0
fprintf (stderr, "dynmobiledimensioncount: type at bottom of ARRAYITEMs not MOBILE!\n");
#endif
				return 0;
			}
			while (dimcount > 0) {
				if (TagOf (type) == S_MOBILE) {
					type = MTypeOf (type);
				}
				if (TagOf (type) != S_ARRAY) {
#if 0
fprintf (stderr, "dynmobiledimensioncount: type of MOBILE is not ARRAY!\n");
#endif
					return 0;
				}
				while ((TagOf (type) == S_ARRAY) && (dimcount > 0)) {
					dimcount--;
					type = ARTypeOf (type);
				}
				type = follow_user_type (type);
			}
#if 0
fprintf (stderr, "dynmobiledimensioncount: out of loop: type = ");
printtreenl (stderr, 4, type);
#endif

			/* type now at the desired type */
			return dynmobiledimensioncount (type);
		}
		break;
	case N_DECL:
	case N_ABBR:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_FIELD:			/* for nested MOBILEs */
		{
			treenode *type = NTypeOf (n);
			int dimcount = 0;

			if (TagOf (type) == N_TYPEDECL) {
				type = follow_user_type (type);
			}
			if (TagOf (type) == S_MOBILE) {
				treenode *dim = MTypeOf (type);

#if 0
fprintf (stderr, "dynmobiledimensioncount: mobile-type = ");
printtreenl (stderr, 4, dim);
#endif
				while (TagOf (dim) == S_ARRAY) {
					dimcount++;
					dim = ARTypeOf (dim);
				}
			}
#if 0
fprintf (stderr, "dynmobiledimensioncount: returning dimcount = %d\n", dimcount);
#endif
			return dimcount;
		}
		break;
	case S_MOBILE:			/* permit asking about a MOBILE type directly */
		{
			int dimcount = 0;
			treenode *dim = MTypeOf (n);

			while (TagOf (dim) == S_ARRAY) {
				dimcount++;
				dim = ARTypeOf (dim);
			}

			return dimcount;
		}
		break;
	case S_UNDEFINED:
		return dynmobiledimensioncount (OpOf (n));
		break;
	}
	return 0;
}
/*}}}*/
/*{{{  PUBLIC BOOL isanychantype (treenode *nptr)*/
/*
 *	returns non-zero if the name given is a MOBILE.CHAN
 */
PUBLIC BOOL isanychantype (treenode *nptr)
{
	if (!nptr) {
		return FALSE;
	}
	if ((TagOf (nptr) == T_TEMP) || (TagOf (nptr) == T_PREEVALTEMP)) {
		nptr = NDeclOf (nptr);
	}

	switch (TagOf (nptr)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		{
			treenode *type = NTypeOf (nptr);

			if (TagOf (type) == S_ANYCHANTYPE) {
				return TRUE;
			}
		}
		return FALSE;
	case S_UNDEFINED:
		return isanychantype (OpOf (nptr));
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isanyproctype (treenode *nptr)*/
/*
 *	returns non-zero if the name given is a MOBILE.PROC
 */
PUBLIC BOOL isanyproctype (treenode *nptr)
{
	if (!nptr) {
		return FALSE;
	}
	if ((TagOf (nptr) == T_TEMP) || (TagOf (nptr) == T_PREEVALTEMP)) {
		nptr = NDeclOf (nptr);
	}

	switch (TagOf (nptr)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		{
			treenode *type = NTypeOf (nptr);

			if (TagOf (type) == S_ANYPROCTYPE) {
				return TRUE;
			}
		}
		return FALSE;
	case S_UNDEFINED:
		return isanyproctype (OpOf (nptr));
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdeepmobiletype (treenode *tptr)*/
/*
 *	determines whether the given type is a "deep" mobile (e.g. RECORD contaning sub-mobiles,
 *	or a dynamic-array of mobile channel-ends)
 */
PUBLIC BOOL isdeepmobiletype (treenode *tptr)
{
	treenode *type;

#if 0
fprintf (stderr, "tran2: isdeepmobiletype(): tptr =");
printtreenl (stderr, 4, tptr);
#endif
	if (!tptr) {
		return FALSE;
	}
	type = tptr;

	if (TagOf (type) == N_TYPEDECL) {
		type = follow_user_type (type);
	}
	if (TagOf (type) == S_MOBILE) {
		type = MTypeOf (type);

		while (TagOf (type) == S_ARRAY) {
			type = ARTypeOf (type);
		}

		if (TagOf (type) == S_RECORD) {
			/* look for mobile sub-fields */
			type = ARTypeOf (type);

			while (type && (TagOf (type) == S_DECL)) {
				treenode *subtype = NTypeOf (DNameOf (type));

				if (isdynmobilechantypetype (subtype) || isdynmobilearraytype (subtype) || isdynmobileproctypetype (subtype) ||
						isanychantype (subtype) || isanyproctype (subtype)) {
					return TRUE;
				}
				type = DBodyOf (type);
			}
		} else {
			if (isdynmobilechantypetype (type) || isdynmobilearraytype (type) || isdynmobileproctypetype (type) ||
					isanychantype (type) || isanyproctype (type)) {
				return TRUE;
			}
		}
	}
	return FALSE;
}
/*}}}*/
#endif	/* MOBILES */
/*{{{  PUBLIC BOOL issimplelocal */
/*****************************************************************************
 *
 *  issimplelocal returns TRUE if the element 'tptr' is  a simple
 *                (not subscripted or segmented) local variable.
 *
 *****************************************************************************/
PUBLIC BOOL issimplelocal (treenode * const tptr, const int my_lexlevel)
{
	if (islocal (tptr, my_lexlevel))
		/*{{{  check it isn't a pointer */
	{
		if (nodetypeoftag (TagOf (tptr)) == NAMENODE) {
			const int m = NModeOf (tptr);
			return ((m == NM_WORKSPACE) || (m == NM_WSPLACED) || (m == NM_DEFAULT));
		} else {		/* hidden parameter node */
			return TagOf (tptr) == S_HIDDEN_PARAM;
		}
	}
	/*}}}  */

	/* bug TS/2023 07/01/93 */
	/* Optimisation to allow word-sized elements at a constant position
	   of an array placed in local workspace to be used directly */
#if 0
	/*{{{  old way of doing it */
	if (istargetintsize (basetype (NTypeOf (name))) && (TagOf (rhs) == S_ARRAYITEM) && islocal (rhs_name, my_lexlevel) && !ispointer (rhs_name) &&
	    /* It might have been a constructor put into a temp */
	    TagOf (rhs_name) != T_TEMP && (ASExpOf (rhs) == NULL))
		/* Optimisation to allow elements at a constant position
		   of an array placed in local workspace to be used directly */
		return TRUE;
	/*}}}  */
#endif
#if 0
	if (((TagOf (tptr) == S_ARRAYITEM) || (TagOf (tptr) == S_ARRAYSUB)) &&
	    issimplelocal (ASBaseOf (tptr), my_lexlevel) && isconst (ASIndexOf (tptr)) && istargetintsize (basetype (gettype (tptr))))
#endif
		switch (TagOf (tptr)) {
		case S_ARRAYITEM:
		case S_ARRAYSUB:
		case S_RECORDITEM:
		case S_RECORDSUB:
			if (issimplelocal (ASBaseOf (tptr), my_lexlevel) && isconst (ASIndexOf (tptr)) && istargetintsize (basetype (gettype (tptr))))
				return TRUE;
			break;
		}
	return FALSE;
}

/*}}}  */
#ifndef COMPILING_TO_JCODE
/*{{{  PUBLIC int whichpowerof2 */
PUBLIC int whichpowerof2 (BIT32 x)
/*****************************************************************************
 *
 *  whichpowerof2 returns -1 if not a power of 2.
 *              If it is, (ie. if only one bit is set), it returns that
 *              bit number.
 *
 *****************************************************************************/
{
	/* WARNING  - TWOS COMPLEMENT host specific */
	/* we use the fact that x & (x - 1) clears the lowest bit which is set
	   in x. Thus if the result is zero, only 1 bit was set.
	 */
	/* we use BIT32 not INT32 so that x-1 doesn't underflow if x = 0x80000000 */

	int i, j;
	if ((x != 0) && ((x & (x - 1)) == 0))
		for (i = 0, j = 1; i < (sizeof (BIT32) * CHAR_BIT); i++, j <<= 1)
			if ((x & j) != 0)
				return i;

	return -1;
}

/*}}}  */
#endif

/*{{{  PUBLIC int inputtypeof */
/*{{{  comment */
/*****************************************************************************
 *
 *  inputtypeof takes an input tree and returns its type:
 *              types are  INP_INPUT - channel ?
 *                         INP_CASE_INPUT - channel ? CASE
 *                         INP_TAGGED_INPUT - channel ? CASE ..
 *                         INP_DELAYED_INPUT - timer ? AFTER
 *                         INP_PORT_INPUT - port ?
 *                         INP_TIMER_INPUT - timer ?
 *                         INP_X_INPUT - extended channel ?
 *                         INP_X_CASE_INPUT - extended channel ? CASE
 *                         INP_X_TAGGED_INPUT - extended channel ? CASE ..
 *                         INP_SKIP - no input
 *
 *****************************************************************************/
/*}}}  */
PUBLIC int inputtypeof (treenode * const tptr)
{
	switch (TagOf (tptr)) {
	case S_SKIP:
		return (INP_SKIP);
	case S_DELAYED_INPUT:
		return (INP_DELAYED_INPUT);
	case S_CASE_INPUT:
		return (INP_CASE_INPUT);
	case S_TAGGED_INPUT:
		return (INP_TAGGED_INPUT);
	case S_X_INPUT:
		return (INP_X_INPUT);
	case S_X_CASE_INPUT:
		return (INP_X_CASE_INPUT);
	case S_X_TAGGED_INPUT:
		return (INP_X_TAGGED_INPUT);
		/*{{{  S_INPUT */
	case S_INPUT:
		{
			treenode *t = gettype (LHSOf (tptr));
			while (TagOf (t) == S_ARRAY)
				t = ARTypeOf (t);
			switch (TagOf (t)) {
			case S_CHAN:
				return INP_INPUT;
			case S_PORT:
				return INP_PORT_INPUT;
			case S_TIMER:
				return INP_TIMER_INPUT;
			default:
				badtag (LocnOf (tptr), TagOf (t), "inputtypeof");
			}
		}
		/*}}}  */
	}
	return (0);		/* not reached */
}

/*}}}  */
/*{{{  PUBLIC BOOL isshortint */
/*****************************************************************************
 *
 *  isshortint returns TRUE if 'type' is an integer type and an object
 *              of type 'type' is smaller than an object of type S_INT.
 *
 *****************************************************************************/
PUBLIC BOOL isshortint (const int type)
{
	return ((type == S_INT16) || (type == S_UINT16)) && ((targetintsize == S_INT32) || (targetintsize == S_UINT32));
}

/*}}}  */
/*{{{  PUBLIC BOOL isshorttype */
/*****************************************************************************
 *
 *  isshorttype returns TRUE if an object of type 'type' is smaller than
 *              an object of type S_INT.
 *
 *****************************************************************************/
PUBLIC BOOL isshorttype (const int type)
{
	return (type == S_BOOL) || (type == S_BYTE) ||
		(((targetintsize == S_INT32) || (targetintsize == S_UINT32)) && ((type == S_INT16) || (type == S_UINT16)));
}

/*}}}  */
/*{{{  PUBLIC BOOL istargetintsize */
/*****************************************************************************
 *
 *  istargetintsize returns TRUE if an object of type 'type' is the same
 *                  size as an object of type 'S_INT'.
 *
 *****************************************************************************/
PUBLIC BOOL istargetintsize (int type)
{
	return ((type == S_INT) || (type == S_UINT) || (type == targetintsize) || ((type == S_REAL32) && ((targetintsize == S_INT32) || (targetintsize == S_UINT32))) || (type == S_CHAN));
}

/*}}}  */
/*{{{  PUBLIC BOOL istargetbytesize */
/*****************************************************************************
 *
 *  istargetbytesize returns TRUE if an object of type 'type' is the same size
 *                   as an object of type S_BYTE.
 *
 *****************************************************************************/
PUBLIC BOOL istargetbytesize (int type)
{
	return ((type == S_BOOL) || (type == S_BYTE));
}

/*}}}  */
/*{{{  PUBLIC BOOL fitsinregister */
/*****************************************************************************
 *
 *  fitsinregister returns TRUE if a value of type 'type' can be held in a
 *                 register.
 *
 *****************************************************************************/
PUBLIC BOOL fitsinregister (int type)
{
	return (istargetintsize (type) || isshorttype (type));
}

/*}}}  */
/*{{{  PUBLIC BOOL fitsinword */
/*****************************************************************************
 *
 *  fitsinword returns TRUE if a value of type 'type' can be held in a
 *             machine word.
 *
 *****************************************************************************/
PUBLIC BOOL fitsinword (const int type)
{
	return (istargetintsize (type) || isshorttype (type));
}

/*}}}  */
/*{{{  PUBLIC BOOL isdoublelength */
/*****************************************************************************
 *
 *  isdoublelength returns TRUE if a (scalar) object of type 'type' occupies
 *                 two machine words.
 *
 *****************************************************************************/
PUBLIC BOOL isdoublelength (const int type)
{
	return (((targetintsize == S_INT16) || (targetintsize == S_UINT16)) && ((type == S_INT32) || (type == S_UINT32) || (type == S_REAL32)))
		|| (((targetintsize == S_INT32) || (targetintsize == S_UINT32)) && ((type == S_INT64) || (type == S_UINT64) || (type == S_REAL64)));
}

/*}}}  */
/*{{{  PUBLIC BOOL isquadlength */
/*****************************************************************************
 *
 *  isquadlength returns TRUE if an object of type 'type' occupies four
 *               machine words.
 *
 *****************************************************************************/
PUBLIC BOOL isquadlength (const int type)
{
	return ((targetintsize == S_INT16) || (targetintsize == S_UINT16)) && ((type == S_INT64) || (type == S_UINT64) || (type == S_REAL64));
}

/*}}}  */

/*{{{  PUBLIC BOOL issimplechan */
/*****************************************************************************
 *  BOOL issimplechan returns TRUE if the name nptr is a channel which
 *                   is not accessed via a pointer.
 *
 *                   Only ever called if chanaspointer is TRUE.
 *
 *                   This is used to optimise local channels which
 *                   don't need a pointer too.
 *
 *****************************************************************************/
PUBLIC BOOL issimplechan (treenode * nptr)
{
#if OPTIMISE_LOCAL_CHANS
	/* treat all local channels as `simple' */
	/* return (TagOf(nptr) == N_DECL && TagOf(NTypeOf(nptr)) == S_CHAN); */

	/* treat all local channels as simple, as long as they're not marked
	   as accessed by 'nasty' things */
	return (TagOf (nptr) == N_DECL && TagOf (NTypeOf (nptr)) == S_CHAN && (!NChanMarkOf (nptr) || (NModeOf (nptr) == NM_PLACED)));
#else
/* we treat this as a `simple' channel only if it is a PLACED scalar channel*/
	return (TagOf (nptr) == N_DECL && TagOf (NTypeOf (nptr)) == S_CHAN && NModeOf (nptr) == NM_PLACED);
#endif
}

/*}}}  */

/*{{{  PUBLIC BOOL isplaced */
/*****************************************************************************
 *
 *  isplaced returns TRUE if the symbol table entry nptr represents a PLACEd
 *           variable.
 *
 *****************************************************************************/
PUBLIC BOOL isplaced (treenode *nptr)
{
	if (nodetypeoftag (TagOf (nptr)) != NAMENODE) {
		return FALSE;
	}
	return (NModeOf (nptr) == NM_PLACED);
}

/*}}}  */
/*{{{  PUBLIC BOOL iswsplaced */
/*****************************************************************************
 *
 *  iswsplaced returns TRUE if the symbol table entry nptr has been PLACEd
 *             at a specific workspace offset.
 *
 *****************************************************************************/
PUBLIC BOOL iswsplaced (treenode * nptr)
{
	return (NModeOf (nptr) == NM_WSPLACED);
}

/*}}}  */
#ifdef MOBILES
/*{{{  PUBLIC BOOL ismobile */
/*****************************************************************************
 *
 * ismobile returns TRUE if `nptr' is a MOBILE variable.  MOBILES are always
 *          accessed through pointers..
 *
 *****************************************************************************/
PUBLIC BOOL ismobile (treenode *const nptr)
{
#if 0
fprintf (stderr, "tran2: ismobile(): nptr = ");
printtreenl (stderr, 4, nptr);
#endif
	switch (TagOf (nptr)) {
	case S_RECORDSUB:
	case S_RECORDITEM:
		return (ismobile (ASBaseOf (nptr)) && ismobile (ASIndexOf (nptr)));
	case S_ARRAYSUB:
	case S_ARRAYITEM:
		{
			int dimcount = 0;
			treenode *n = nptr;

			while ((TagOf (n) == S_ARRAYSUB) || (TagOf (n) == S_ARRAYITEM)) {
				dimcount++;
				n = ASBaseOf (n);
			}
			if (ismobile (n)) {
				treenode *basetype = follow_user_type (gettype (n));

				while (dimcount > 0) {
					if (TagOf (basetype) == S_MOBILE) {
						basetype = MTypeOf (basetype);
					}
					if (TagOf (basetype) != S_ARRAY) {
						return FALSE;
					}
					while ((TagOf (basetype) == S_ARRAY) && (dimcount > 0)) {
						basetype = ARTypeOf (basetype);
						dimcount--;
					}
					basetype = follow_user_type (basetype);
				}
				if (TagOf (basetype) == S_MOBILE) {
					return TRUE;
				}
			}
		}
		return FALSE;
	case N_DECL:
		if (TagOf (follow_user_type (NTypeOf (nptr))) == S_MOBILE) {
			if (NModeOf (nptr) == NM_DEFAULT) {
				SetNMode (nptr, NM_POINTER);
			}
			return TRUE;
		}
		break;
	case N_ABBR:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_FIELD:
	case T_TEMP:
	case T_PREEVALTEMP:
		{
			treenode *type = NTypeOf (nptr);

			/* FIXME: N_PROCTYPEDECL is a dynamic mobile */
			if (TagOf (type) == N_TYPEDECL) {
				type = follow_user_type (type);
			}
			if (TagOf (type) == S_MOBILE) {
				return TRUE;
			}
		}
		return FALSE;
	}
	return FALSE;
}
/*}}}  */
#endif
/*{{{  PUBLIC BOOL ispointer */
/*****************************************************************************
 *
 *  ispointer returns TRUE if we access the element 'tptr' through a pointer,
 *            FALSE if we access it directly.  N.B. Non-local simple variables
 *            are not strictly accessed through a pointer.
 *
 *****************************************************************************/
PUBLIC BOOL ispointer (treenode * const tptr)
{

#if 0
fprintf (stderr, "tran2: ispointer: tptr = ");
printtreenl (stderr, 4, tptr);
#endif
	switch (TagOf (tptr)) {
		/*{{{  N_DECL */
	case N_DECL:
		{
			BOOL r;

#ifdef MOBILES
			r = isinmobilespace (tptr);
			r |= isinvectorspace (tptr);
			/* if it's a dynamic MOBILE, it's still a pointer */
			if (isdynmobilearray (tptr)) {
				r |= TRUE;
			}
			/* likewise for dynamic MOBILE CHAN TYPEs */
			if (isdynmobilechantype (tptr)) {
				r |= TRUE;
			}
			/* and MOBILE PROC TYPEs too */
			if (isdynmobileproctype (tptr)) {
				r |= TRUE;
			}
			/* and MOBILE BARRIERs */
			if (isdynmobilebarrier (tptr)) {
				r |= TRUE;
			}
			/* and MOBILE.CHANs/MOBILE.PROCs too, although these get handled slightly specially */
			if (TagOf (NTypeOf (tptr)) == S_ANYCHANTYPE) {
				r |= TRUE;
			}
			if (TagOf (NTypeOf (tptr)) == S_ANYPROCTYPE) {
				r |= TRUE;
			}
			#if 0
			if (TagOf (NTypeOf (tptr)) == S_FULLBARRIER) {
				fprintf (stderr, "X1\n");
				r |= TRUE;
			}
			#endif
#endif
			r |= isplaced (tptr);	/* bug TS/1467 8/11/91 */
#if 0
fprintf (stderr, "ispointer() returning %d, tptr =", r);
printtreenl (stderr, 4, tptr);
#endif
			return r;
		}
		/*}}}  */
		/*{{{  N_ABBR N_VALABBR N_RETYPE N_VALRETYPE N_PARAM N_VALPARAM N_RESULTPARAM */
	case N_ABBR:
	case N_VALABBR:
	case N_RETYPE:
	case N_VALRETYPE:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
		{
			const int m = NModeOf (tptr);

#if 0
if ((TagOf (tptr) == N_RESULTPARAM) || (TagOf (tptr) == N_PARAM) || (TagOf (tptr) == N_VALPARAM)) {
fprintf (stderr, "ispointer: N_{,RESULT,VAL}PARAM, m = %d, tptr =", m);
printtreenl (stderr, 4, tptr);
}
#endif
			return ((m == NM_POINTER) || (m == NM_VECSPACE));
		}
		/*}}}  */
		/*{{{  T_TEMP T_PREEVALTEMP */
	case T_TEMP:
	case T_PREEVALTEMP:
		/*switch(NModeOf(tptr))
		   {
		   case NM_POINTER:   return(TRUE);
		   case NM_WORKSPACE: return(FALSE);
		   } */
		return (NModeOf (tptr) == NM_POINTER);
		/*}}}  */
		/*{{{  various things return TRUE */
	case S_FNACTUALRESULT:
	case S_FNFORMALRESULT:
	case S_STRING:
	case S_CONSTCONSTRUCTOR:
	case S_ARRAYITEM:	/* bug TS/1682 01/05/92 */
	case S_RECORDITEM:
	case S_SEGMENTITEM:	/* MDP */
	case S_SEGMENT:	/* MDP */
	case S_PARAM_VSP:	/* MDP */
	case S_PARAM_FB:
	case S_NULLARRAY:
#ifdef MOBILES
	case S_PARAM_MSP:
	case S_PARAM_MPP:
	case S_CLONE:
	case S_ANYCHANTYPE:
	case S_ANYPROCTYPE:
#endif
		return (TRUE);
		/*}}}  */
		/*{{{  various things return FALSE */
		/*{{{  extras needed to suppress PTR comment in .t file MDP */
	case S_FINSTANCE:
	case S_ARRAYSUB:
	case S_RECORDSUB:
	case S_AND:
	case S_OR:
	case S_ADD:
	case S_SUBTRACT:
	case S_MULT:
	case S_DIV:
	case S_REM:
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
	case S_LSHIFT:
	case S_RSHIFT:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
	case S_EQ:
	case S_NE:
	case S_LS:
	case S_LE:
	case S_GR:
	case S_GE:
	case S_AFTER:
	case S_CSUB0:
	case S_CCNT1:
	case S_EVAL:
	case S_OVERLAPCHECK:
	case S_FOR:
	case S_MOSTPOS:
	case S_MOSTNEG:
	case S_NEG:
	case S_BITNOT:
	case S_NOT:
	case S_SIZE:
	case S_UMINUS:
	case S_ADDRESSOF:
	case S_BYTESIN:
	case S_ELSIZE:
	case S_SEGSTART:
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
	case S_CONSTEXP:
	case S_HIDDEN_PARAM:
	case S_HIDDEN_TYPE:
	case S_PARAM_WS:
		/*}}}  */
	case N_REPL:
#if 0				/* this doesn't work! */
	case S_ARRAYITEM:	/* bug 1238 22/8/91 */
#endif
#ifdef MOBILES
	case S_ADDROF:
	case S_HWADDROF:
#endif
		return FALSE;
		/*}}}  */
	default:
#if 0
		fprintf (outfile, "\nispointer: tree is ");
		printtree (0, tptr);
		fputc ('\n', outfile);
#endif
		badtag (LocnOf (tptr), TagOf (tptr), "ispointer");
	}
	return (FALSE);		/* not reached */
}

/*}}}  */

/*{{{  PUBLIC BOOL isinconstanttable */
/*****************************************************************************
 *
 *  isinconstanttable returns TRUE if the constant node 'tptr' is in a
 *                    constant table.
 *
 *****************************************************************************/
PUBLIC BOOL isinconstanttable (treenode *tptr)
{
#if 0
fprintf (stderr, "tran2: isinconstanttable: tptr = ");
printtreenl (stderr, 4, tptr);
#endif
	if (TagOf (tptr) == N_VALABBR) {
		/* we sometimes end up here -- test the value of it */
		tptr = DValOf (NDeclOf (tptr));
	}
	if (TagOf (tptr) != S_CONSTEXP) {
		badtag (LocnOf (tptr), TagOf (tptr), "isinconstanttable");
	}
	return (CEOffsetOf (tptr) >= 0);
}

/*}}}  */
/*{{{  PUBLIC BOOL isaddressable */
/*****************************************************************************
 *
 *  isaddressable returns TRUE if the tree 'tptr' is addressable, ie. if it
 *                is an element not an expression.
 *
 *****************************************************************************/
PUBLIC BOOL isaddressable (treenode *tptr)
{
	switch (TagOf (tptr)) {
	case S_ARRAYSUB:
	case S_ARRAYITEM:
	case S_RECORDSUB:
	case S_RECORDITEM:
	case S_SEGMENT:
	case S_SEGMENTITEM:
	case N_ABBR:
	case N_VALABBR:
	case N_RETYPE:
	case N_VALRETYPE:
	case N_DECL:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
	case N_REPL:
	case T_PREEVALTEMP:
	case S_STRING:
	case S_CONSTCONSTRUCTOR:
	case S_FNFORMALRESULT:	/* INSdi03203 */
	case S_NULLARRAY:
		return (TRUE);
	case S_CONSTEXP:
		return (isinconstanttable (tptr));
	case T_TEMP:
#ifdef COMPILING_TO_JCODE
		return TRUE;
#else
		/* This is a bit strange:
		   we use the result here to decide if the temporary means we are
		   to load a pointer to the exp, or if we are to load the exp */
		return (isaddressable (NDeclOf (tptr)));
#endif
#ifdef MOBILES
	case S_CLONE:
		/* clones are addressable if their parameter is */
		return (isaddressable (OpOf (tptr)));
#endif
	default:
		return (FALSE);
	}
}

/*}}}  */

FORWARD PRIVATE treenode *create_hiddenparam (const int current_lexlevel);

/*{{{  PRIVATE treenode *hiddenparamsof */
/*****************************************************************************
 *
 *  hiddenparamsof takes an actual parameter 'aparam' and a formal
 *                 parameter corresponding to 'aparam'
 *                 and returns a tree representing
 *                 any associated hidden parameters
 *                 (ie. sizes of an unsized array).
 *  added by frmb: if mobile_size_field is enabled this adds an extra
 *                 hidden-parameter which will hold the size of the
 *                 underlying (base) type in bytes.
 *  (12/02/2003)   added handling for NULLARRAY
 *
 *****************************************************************************/
PRIVATE treenode *hiddenparamsof (treenode *const aparam, treenode *const fparam, const int arg_no)
{
	treenode *hparams = NULL;
	treenode *ftype = follow_user_type (gettype (fparam));
	int dimension = 0;
#ifdef MOBILES

#if 0
fprintf (stderr, "hiddenparamsof: aparam = ");
printtreenl (stderr, 4, aparam);
fprintf (stderr, "hiddenparamsof: fparam = ");
printtreenl (stderr, 4, fparam);
#endif
	/*{{{  walk the formal type tree searching for unknown sizes */
	if ((TagOf (ftype) == S_MOBILE) && isdynmobilearray (fparam)) {
		wordnode *const name = lookupword ("$hiddenptr", 10);
		treenode *const p = newnamenode (S_HIDDEN_PARAM, NOPOSN, name, newleafnode (S_INT, NOPOSN),
					dimexpof (aparam, -1), NLexLevelOf (fparam), 0, NM_DEFAULT);

		SetHExp (p, create_hiddenparam (NLexLevelOf (fparam)));
		SetHDimension (p, -1);
		SetHArgNo (p, arg_no);
		hparams = appendnode (p, hparams);

		ftype = MTypeOf (ftype);
	}
#endif
	while (TagOf (ftype) == S_ARRAY) {
		if (ARDimOf (ftype) == (-1)) {	/* we have a hidden parameter */
			/*hparams = appendnode(hiddenparamnodeof(aparam, dimension), hparams); */
			wordnode *const name = lookupword ("$arraydim", 9);
			treenode *const p = newnamenode (S_HIDDEN_PARAM, NOPOSN, name, NULL,
							 dimexpof (aparam, dimension), NLexLevelOf (fparam), 0, NM_DEFAULT);
#if 0
fprintf (stderr, "hiddenparamsof: creating hidden param:");
printtreenl (stderr, 4, p);
#endif
#ifdef MOBILES
			if (isdynmobilearray (aparam)) {
				SetHExp (p, create_hiddenparam (NLexLevelOf (fparam)));
			}
#endif
			SetHDimension (p, dimension);
			SetHArgNo (p, arg_no);
			hparams = appendnode (p, hparams);
		}
		dimension++;
		ftype = follow_user_type (ARTypeOf (ftype));
#ifdef MOBILES
		if (TagOf (ftype) == S_MOBILE && !isdynmobilearraytype (ftype)) {
			ftype = MTypeOf (ftype);
		}
#endif
	}
	/*}}}  */
#ifdef MOBILES
	if (TagOf (ftype) == S_ANYCHANTYPE) {
		/* need hidden type parameter */
		wordnode *const name = lookupword ("$htype", 6);
		treenode *const p = newnamenode (S_HIDDEN_TYPE, NOPOSN, name, newleafnode (S_INT, NOPOSN),
					NULL, NLexLevelOf (fparam), 0, NM_DEFAULT);

		SetHExp (p, NULL);
		SetHDimension (p, dimension);
		SetHArgNo (p, arg_no);
		hparams = appendnode (p, hparams);
	} else if (TagOf (ftype) == S_ANYPROCTYPE) {
		/* need hidden type parameter */
		wordnode *const name = lookupword ("$htype", 6);
		treenode *const p = newnamenode (S_HIDDEN_TYPE, NOPOSN, name, newleafnode (S_INT, NOPOSN),
					NULL, NLexLevelOf (fparam), 0, NM_DEFAULT);

		SetHExp (p, NULL);
		SetHDimension (p, dimension);
		SetHArgNo (p, arg_no);
		hparams = appendnode (p, hparams);
	}
#endif
	return (hparams);
}

/*}}}  */
/*{{{  PRIVATE treenode *create_hiddenparam */
PRIVATE treenode *create_hiddenparam (const int current_lexlevel)
{
/*return newtempnode(T_PREEVALTEMP,dummyexp_p,NM_WORKSPACE, current_lexlevel);*/

	wordnode *tempname_p = lookupword ("temp", 4);
	treenode *tempptr;
	tempptr = newnamenode (T_PREEVALTEMP, NOPOSN, tempname_p, gettype_main (dummyexp_p), dummyexp_p, current_lexlevel, 0, NM_WORKSPACE);
	return tempptr;
}

/*}}}*/

/*{{{  PUBLIC BOOL word_length_record */
PUBLIC BOOL word_length_record (treenode * const type)
{
	return (TagOf (follow_user_type (type)) == S_RECORD)
		&& (bytesin (type) <= bytesperword);
}

/*}}}  */
/*{{{  PUBLIC return_style_t fn_return_style */
PUBLIC return_style_t fn_return_style (treenode * const nptr)
{
	treenode *const fresulttypes = FnTypeListOf (NTypeOf (nptr));
	if (!EmptyList (fresulttypes) && EmptyList (NextItem (fresulttypes)))
		/*{{{  single result */
	{
		treenode *const result_type = follow_user_type (ThisItem (fresulttypes));

		/* Special case a single-valued REALnn function with inline fp */
		if (has_fpu_core && isreal (TagOf (result_type)))
			return return_style_fpu;

#ifdef OCCAM2_5
		if (word_length_record (result_type))
			return return_style_alu;
#endif
	}
	/*}}}  */
	return return_style_other;
}

/*}}}  */
/*{{{  PUBLIC return_style_t valof_return_style */
PUBLIC return_style_t valof_return_style (treenode * const resultlist)
{
	if (!EmptyList (resultlist) && EmptyList (NextItem (resultlist)))
		/*{{{  single result */
	{
		treenode *const result_type = gettype (ThisItem (resultlist));

#if 0
		/* Special case a single-valued REALnn function with inline fp */
		if (has_fpu_core && isreal (TagOf (result_type)))
			return return_style_fpu;
#endif

		if (fitsinregister (TagOf (result_type)))
			return return_style_alu;

#ifdef OCCAM2_5
		if (word_length_record (result_type))
			return return_style_alu;
#endif
	}
	/*}}}  */
	return return_style_other;
}

/*}}}  */
/*{{{  PUBLIC return_style_t result_return_style */
PUBLIC return_style_t result_return_style (treenode * const type)
{
	treenode *const result_type = follow_user_type (type);

	if (fitsinregister (TagOf (result_type)))
		return return_style_alu;
#ifdef OCCAM2_5
	if (word_length_record (result_type))
		return return_style_alu;
#endif
	return return_style_other;
}

/*}}}  */
/*{{{  PRIVATE BOOL valparam_pass_by_ref */
PRIVATE BOOL valparam_pass_by_ref (treenode * const type)
{
	if (isscalartype (TagOf (type)) && (wordsin (type) <= 1))
		return FALSE;
#ifdef OCCAM2_5
	else if (word_length_record (type))
		return FALSE;
#endif
	else
		return TRUE;
}

/*}}}  */

/*{{{  PUBLIC void augmentformals */
/*{{{  comment */
/*****************************************************************************
 *
 *  augmentformals adds hidden array dimensions and result pointer parameters
 *                 to a formal parameter list.  Note that it does not add
 *                 a static link parameter or a vector space parameter as
 *                 we don't know if they are needed until we have mapped the
 *                 routine body.
 *
 *                 This routine is applied in the trans phase.
 *
 *  updated (frmb): if this is a MOBILE PROC declaration, adds a special "this"
 *                  pointer to the parameter list.  Also if a regular PROC
 *                  that SUSPENDs.
 *
 *****************************************************************************/
/*}}}  */
PUBLIC void augmentformals (treenode *const nptr, const int current_lexlevel)
{
	treenode *fparams;

	DEBUG_MSG (("augmentformals: %s\n", WNameOf (NNameOf (nptr))));
	fparams = NParamListOf (nptr);

#if 0
fprintf (stderr, "tran2: augmentformals (in): nptr = ");
printtreenl (stderr, 4, nptr);
#endif

	/*{{{  augment the parameter list */
	{
		treenode *fptr = fparams;
		int arg_no = 0;
		while (!EndOfList (fptr)) {
			treenode *thisformal = ThisItem (fptr);
			treenode *nextformal = NextItem (fptr);
			treenode *h;
			treenode *type = follow_user_type (NTypeOf (thisformal));

/*SetNVNext(thisformal, NULL); *//* This is done by the mapper */
			/*{{{  put temporaries in unknown array dimensions */
			{
				treenode *thistype;
				for (thistype = type; TagOf (thistype) == S_ARRAY; thistype = ARTypeOf (thistype)) {
					if (ARDimOf (thistype) == (-1)) {
						SetARDimLength (thistype, create_hiddenparam (current_lexlevel));
					}
				}
			}
			/*}}}  */
			/*{{{  paremeters passed by val or by reference? */
			/* bug 1156 - insert the modes of the parameters while still in trans! */
			/* bug frmb - this just eluded me as well, with RESULT params! */
			{
				int nm;
				
				if (((TagOf (thisformal) == N_PARAM) || (TagOf (thisformal) == N_RESULTPARAM))
#ifndef COMPILING_TO_JCODE
						&& (!chanaspointer || (basetype (type) != S_CHAN))
#endif
						) {
					nm = NM_POINTER;
				} else {
					nm = valparam_pass_by_ref (type) ? NM_POINTER : NM_WORKSPACE;
				}
				SetNMode (thisformal, nm);
			}
			/*}}}  */

			h = hiddenparamsof (thisformal, thisformal, arg_no);
			(void) insertlist (h, fptr);
			fptr = nextformal;
			arg_no++;
		}
	}
	/*}}}  */
	/*{{{  augment the result list */
	switch (TagOf (nptr)) {
	default:		/* PROCs */
		break;
	case N_SFUNCDEF:
	case N_LFUNCDEF:
	case N_SCFUNCDEF:
	case N_LIBFUNCDEF:
	case N_STDLIBFUNCDEF:
		/*{{{  prepend result pointer parameters for FUNCTIONS */
		{
			treenode *resultptrlist = NULL;
			treenode *fresulttypes = FnTypeListOf (NTypeOf (nptr));
			int result_no = 0;
			int regresults = 0;
			if (fn_return_style (nptr) != return_style_fpu)
				for (; !EndOfList (fresulttypes); result_no++, fresulttypes = NextItem (fresulttypes)) {
					treenode *const thisresulttype = ThisItem (fresulttypes);
					if ((regresults < MAX_REGRESULTS) && (result_return_style (thisresulttype) == return_style_alu))
						regresults++;
					else {
#if 0
						treenode *rnode = newhiddenparamnode (S_FNFORMALRESULT, NOPOSN,
										      thisresulttype, 0, current_lexlevel, result_no);
#else
						wordnode *name;
						treenode *rnode;
						char str[50];	/* plenty enough for 'formalresultnnn' */
						sprintf (str, "$formalresult%d", result_no);
						name = lookupword (str, strlen (str));
						rnode = newnamenode (S_FNFORMALRESULT, NOPOSN, name,
								     copytree (thisresulttype, current_lexlevel), NULL,
								     current_lexlevel, 0, NM_POINTER);
						SetHArgNo (rnode, result_no);
#endif
						resultptrlist = addtofront (rnode, resultptrlist);
					}
				}
			/* this has created a list in reverse order, so */
			reverselist (&resultptrlist);
			if (resultptrlist != NULL)
				/* Swapped fparams with resulttptrlist */
				SetFnParams (NTypeOf (nptr), appendlist (fparams, resultptrlist));
		}
		break;
		/*}}}  */
	}
	/*}}}  */
#ifdef MOBILES
	if ((TagOf (nptr) == N_MPROCDECL) || (TagOf (nptr) == N_LIBMPROCDECL) || ((nodetypeoftag (TagOf (nptr)) == NAMENODE) && NPSuspendsOf (nptr))) {
		/*{{{  add magic "self" to the parameter list -- we make it the first parameter*/
		wordnode *const name = lookupword ("$mpp", 4);
		treenode *const p = newnamenode (S_PARAM_MPP, NOPOSN, name, newtypenode (S_MOBILE, NOPOSN, NULL, NDeclOf (nptr) ? DExtraOf (NDeclOf (nptr)) : NULL), NULL, current_lexlevel, 0, NM_DEFAULT);
		treenode *fparams = NParamListOf (nptr);

		fparams = newlistnode (S_LIST, NOPOSN, p, fparams);
		SetNParamList (nptr, fparams);

		/*}}}*/
	}
#endif
#if 0
fprintf (stderr, "tran2: augmentformals (out): NTypeOf (nptr) = ");
printtreenl (stderr, 4, NTypeOf (nptr));
#endif
}
/*}}}*/
/*{{{  PUBLIC void augmentproctype (treenode *const nptr, const int current_lexlevel)*/
/*
 *	this augments formal parameters in a PROC TYPE definition to include the "MOBILE <proc-type>" parameter
 *	also adds $VSP and $MSP hidden parameters
 */
PUBLIC void augmentproctype (treenode *const nptr, const int current_lexlevel)
{
	treenode *fparams, **tmp;
	wordnode *const mppname = lookupword ("$mpp", 4);
	treenode *const mpp = newnamenode (S_PARAM_MPP, NOPOSN, mppname, newtypenode (S_MOBILE, NOPOSN, NULL, nptr), NULL, current_lexlevel, 0, NM_DEFAULT);
	wordnode *const vspname = lookupword ("$VSP", 4);
	treenode *const vsp = newnamenode (S_PARAM_VSP, NOPOSN, vspname, NULL, NULL, current_lexlevel, 0, NM_DEFAULT);
	wordnode *const mspname = lookupword ("$MSP", 4);
	treenode *const msp = newnamenode (S_PARAM_MSP, NOPOSN, mspname, NULL, NULL, current_lexlevel, 0, NM_DEFAULT);
	int nparams;

	/* call augmentformals to setup existing parameters (hidden dimensions, etc.) */
	augmentformals (nptr, current_lexlevel);

	fparams = NParamListOf (nptr);
	fparams = newlistnode (S_LIST, NOPOSN, mpp, fparams);
	/* scan to end of parameters and add VSP, MSP */
	for (tmp = NextItemAddr (fparams); !EndOfList (*tmp); tmp = NextItemAddr (*tmp));
	*tmp = newlistnode (S_LIST, NOPOSN, vsp, newlistnode (S_LIST, NOPOSN, msp, NULL));
	
	SetNParamList (nptr, fparams);
	/* need to set NPParams on this -- and, also make sure parameters have their mode set correctly */
	for (nparams = 0; !EndOfList (fparams); fparams = NextItem (fparams)) {
		treenode *fparam = ThisItem (fparams);

		switch (TagOf (fparam)) {
		case S_HIDDEN_PARAM:
		case S_TIMER:
			break;
		default:
			nparams++;
			break;
		}
	}
	SetNPParams (nptr, nparams);
#if 0
fprintf (stderr, "tran2: augmentproctype (end): nparams = %d, nptr = ", nparams);
printtreenl (stderr, 4, nptr);
#endif
	return;
}
/*}}}*/

/*{{{  PUBLIC BOOL mightbealiased(treenode *const t) */
PUBLIC BOOL mightbealiased (treenode * const t)
{
	if (!checkalias)
		return TRUE;

	{
		treenode *const basename = nameof (t);
		if ((nodetypeoftag (TagOf (basename)) == NAMENODE) && NVAliasedOf (basename))
			return TRUE;
	}

	return FALSE;
}

/*}}}  */

/*{{{  PUBLIC int ilength(operand) */
/***************************************************************************
 *
 * ilength returns the number of bytes required by a primary instruction
 *         whose operand is operand.
 *
 **************************************************************************/
PUBLIC int ilength (const INT32 operand)
{
	if ((0 <= operand) && (operand <= 15))
		return (1);
	else if ((-256 <= operand) && (operand <= 255))
		return (2);
	else if ((-4096 <= operand) && (operand <= 4095))
		return (3);
	else
		/*{{{  others */
	{
		int mask = -1 << 16, adj = 4, done;
		if (operand < 0)
			done = mask;
		else
			done = 0;
		while ((operand & mask) != done) {
			adj++;
			mask <<= 4;
			done <<= 4;
		}
		return (adj);
	}
	/*}}}  */
}

/*}}}  */
/*{{{  PUBLIC constformat_t constformat_of_const */
PUBLIC constformat_t constformat_of_const (const int type, const INT32 lo, const INT32 hi)
{
	const BOOL opt_space = ((code_style_flags & CODE_STYLE_SPACE_NOT_TIME) != 0);
	const int const_max_bytes = opt_space ? CONST_MAX_BYTES_SPACE	/* bug TS/1504 07/04/92 */
		: T9000_instruction_timings ? CONST_MAX_BYTES_T9000	/* bug 1371 15/8/91 */
		: CONST_MAX_BYTES;

	if (istargetintsize (type)) {
		if (targetintsize == S_INT32) {
			if ((lo <= LDNLPNEG_INT32) && ((lo & 0x3) /*(lo % bytesperword) */  == 0)
			    && !T9000_alpha_badmint (&tx_global))
				return constformat_mint32_ldnlp;
			if (!T9000_instruction_timings || opt_space) {	/* bug 1371 15/8/91 */
				if (lo <= ADCNEG_INT32 && !T9000_alpha_badmint (&tx_global)) {
					return constformat_mint32_adc;
				}
				if (lo == MOSTPOS_INT32 && !T9000_alpha_badmint (&tx_global)) {
					return constformat_mint32_not;
				}
				if (has_fp_support) {
					if ((lo >= LDINF_NEG_WORD) && (lo <= LDINF_POS_WORD) && ((lo & 0x3) == 0)) {
						return constformat_ldinf_ldnlp;
					}
					if ((lo >= LDINF_NEG_BYTE) && (lo <= LDINF_POS_BYTE)) {
						return constformat_ldinf_adc;
					}
				}
			}
		} else if (targetintsize == S_INT16) {
			if ((lo <= LDNLPNEG_INT16) && ((lo & 0x1) /*(lo % bytesperword) */  == 0)) {
				return constformat_mint16_ldnlp;
			}
			if (lo <= ADCNEG_INT16) {
				return constformat_mint16_adc;
			}
		}

		if (ilength (lo) > const_max_bytes) {
			return constformat_table;
		}
	} else if (isdoublelength (type)) {
		if (((lo != MOSTNEG_INT32) && (ilength (lo) > const_max_bytes))
				|| ((bytesperword > 2) &&	/* added 16/7/91 for bug 1195 - CON */
				(hi != MOSTNEG_INT32) && (ilength (hi) > const_max_bytes))) {
			return constformat_table;
		}
	}

	return constformat_ldc;
}

/*}}}  */
/*{{{  PUBLIC BOOL shouldbeinconstanttable */
PUBLIC BOOL shouldbeinconstanttable (treenode * const t)
{
	return constformat_of_const (ntypeof (t), LoValOf (t), HiValOf (t)) == constformat_table;
}

/*}}}  */

/*{{{  PUBLIC BOOL usedin(t, tptr) */
/*****************************************************************************
 *
 *  usedin checks if the element 't' is used or aliased in the expression
 *         or list of expressions 'tptr'. Does not assume alias checking.
 *
 *****************************************************************************/
/*{{{  comment on definition of 'used or aliased' */
/*  For r := f(r) op exp :
    We want to evaluate f(r) and wish to know if we can assign the result
    directly into r.

We may not assign to r if:
  1. r' is used directly, or an alias is used directly, in exp
  2. if r' is a var. parameter and there are free variables (or other var.
     parameters) in exp
     Tighten this to  if r' is a var. parameter
  3. if r' is a free variable and there are var. parameters
where
  r' is the base decl. of r (unlocal-aliased) (variable n in this function)
*/
/*}}}  */
PUBLIC BOOL usedin (treenode *const t, treenode *tptr, const int my_lexlevel)
{
	treenode *const n = basedecl (t);

	/*{{{  say it is used if alias checking is disabled */
	if (mightbealiased (t))
		return TRUE;
	/*}}}  */

	while (tptr != NULL) {
		/*{{{  check n is not used or aliased in tptr */
		switch (TagOf (tptr)) {
			/*{{{  var abbreviation/retype */
		case N_ABBR:	/* Check if used in the abbreviation */
		case N_RETYPE:	/* Check if used in the retype */
			if (n == tptr)
				return TRUE;	/* bug 865 28/1/91 */
			tptr = DValOf (NDeclOf (tptr));
			break;
			/*}}}  */
			/*{{{  var parameter */
		case N_PARAM:
		case N_RESULTPARAM:
			/* The expression contains a var parameter:
			   if n is local we are ok, otherwise return FALSE. */
			return ((NLexLevelOf (n) != my_lexlevel) || (n == tptr));
			/* n is a free variable if its lexlevel is not the current lexlevel,
			   otherwise n is local (n cannot be var. param) */
			/*}}}  */
			/*{{{  val abbrev/retype/param   constants   replicator */
		case S_CONSTEXP:
		case S_STRING:
		case S_CONSTCONSTRUCTOR:
			return (FALSE);
			/*}}}  */
			/*{{{  decl */
		case N_VALABBR:
		case N_VALRETYPE:
		case N_VALPARAM:
		case N_DECL:
		case N_REPL:
			return (n == tptr);	/* the name is the same n */
			/*}}}  */
			/*{{{  temporary */
		case T_TEMP:
		case T_REGTEMP:
		case T_PREEVALTEMP:
			return (FALSE);
			/*}}}  */
			/*{{{  monadic operators conversions */
		case S_SIZE:
		case S_ELSIZE:
			return (FALSE);
		case S_NEG:
		case S_BITNOT:
		case S_NOT:
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
		case S_DEFINED:
			/*return (usedin(n, OpOf(tptr))); */
			tptr = OpOf (tptr);
			break;
			/*}}}  */
			/*{{{  constructor */
			/* frmb (Jan 2005): this can happen when we're looking at a constructor that's got nested constructors in it */
		case S_CONSTRUCTOR:
			tptr = LitExpOf (tptr);
			break;
			/*}}}  */
			/*{{{  dyadic operators */
		case S_AND:
		case S_OR:
		case S_ADD:
		case S_SUBTRACT:
		case S_MULT:
		case S_DIV:
		case S_REM:
		case S_BITAND:
		case S_BITOR:
		case S_XOR:
		case S_LSHIFT:
		case S_RSHIFT:
		case S_PLUS:
		case S_MINUS:
		case S_TIMES:
		case S_EQ:
		case S_NE:
		case S_LS:
		case S_LE:
		case S_GR:
		case S_GE:
		case S_AFTER:
		case S_CSUB0:
		case S_CCNT1:
		case S_EVAL:
		case S_COLON2:
			return (usedin (n, LeftOpOf (tptr), my_lexlevel) || usedin (n, RightOpOf (tptr), my_lexlevel));
			/*}}}  */
			/*{{{  array subscript */
		case S_ARRAYITEM:
		case S_ARRAYSUB:
		case S_RECORDSUB:
		case S_RECORDITEM:
			return (usedin (n, ASBaseOf (tptr), my_lexlevel) || usedin (n, ASExpOf (tptr), my_lexlevel));
			/*}}}  */
			/*{{{  segment */
		case S_SEGMENTITEM:
		case S_SEGMENT:
			return (usedin (n, SNameOf (tptr), my_lexlevel) ||
				usedin (n, SStartExpOf (tptr), my_lexlevel) || usedin (n, SLengthExpOf (tptr), my_lexlevel));
			break;
			/*}}}  */
			/*{{{  function instance */
		case S_FINSTANCE:
			/* We say it's used if it is visible to the function,
			   or if it is used in the parameter list */
			return (isafreevarof (n, INameOf (tptr)) || usedin (n, IParamListOf (tptr), my_lexlevel));
			/*}}}  */
			/*{{{  specification ... valof */
		case S_VALABBR:
		case S_VALRETYPE:
		case S_PROCDEF:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_DECL:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_ABBR:
		case S_RETYPE:
		case S_VALOF:
		case S_PRAGMA:	/* bug 829 19/9/91 */
		case S_TYPEDECL:
#ifdef MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			/* Things are getting a bit complicated: take the easy option */
			return (TRUE);	/* Yes it is in danger of being used */
			/*}}}  */
			/*{{{  list    break / return */
		case S_LIST:
			if (usedin (n, ThisItem (tptr), my_lexlevel))
				return (TRUE);
			tptr = NextItem (tptr);
			break;
			/*}}}  */
			/*{{{  hidden parameters dummy expression */
		case S_HIDDEN_PARAM:
		case S_PARAM_STATICLINK:
		case S_PARAM_VSP:
#ifdef MOBILES
		case S_PARAM_MSP:
		case S_PARAM_MPP:
		case S_HIDDEN_TYPE:
#endif
		case S_FNACTUALRESULT:
		case S_DUMMYEXP:
			return (FALSE);
		case S_FNFORMALRESULT:	/* INSdi03203 */
			return (n == tptr);

			/*}}}  */
		default:
			badtag (LocnOf (tptr), TagOf (tptr), "usedin");
		}
		/*}}}*/
	}
	return (FALSE);
}

/*}}}  */
/*{{{  PUBLIC BOOL can_use_wsubdb */
PUBLIC BOOL can_use_wsubdb (const int b, const int bpw, treenode * tptr)
{
	if (b > bpw && has_wsubdb)
#ifdef OCCAM2_5
		/*{{{  check for badly aligned records */
		while (TRUE) {
			switch (TagOf (tptr)) {
				/*{{{  segment */
			case S_SEGMENTITEM:
			case S_SEGMENT:
				tptr = SNameOf (tptr);
				break;
				/*}}}  */
				/*{{{  subscript */
			case S_ARRAYITEM:
			case S_ARRAYSUB:
				tptr = ASBaseOf (tptr);
				break;
				/*}}}  */
				/*{{{  record - check for non-aligned fields */
			case S_RECORDITEM:
			case S_RECORDSUB:
				{
					treenode *const r = ASIndexOf (tptr);
					const INT32 natural_alignment = bytesinscalar_tree (gettype (r));
					if ((NVOffsetOf (r) % natural_alignment) != 0)
						return FALSE;
				}
				tptr = ASBaseOf (tptr);
				break;
				/*}}}  */
			default:
				return TRUE;
			}
		}
	/*}}}  */
#else
	{
		USE_VAR (tptr);
		return TRUE;
	}
#endif
	return FALSE;
}

/*}}}  */

/*{{{  PUBLIC const char *transmessagestring (n) */
PUBLIC const char *transmessagestring (const int n)
{
	switch (n) {
	case TRANS_RUN_TIME_OVERLAP_CHECK:
		return "Run-time disjointness check inserted";
	case TRANS_RUN_TIME_OVERLAP_CHECKS:
		return "%d run-time disjointness checks inserted";
	case TRANS_RETYPE_TYPE_MISMATCH:
		return "Size of RETYPE for %s does not match the expression size";
	case TRANS_ENCODE_CHANNEL_ERROR:
		return "Error generating ENCODE.CHANNEL";
	case TRANS_DECODE_CHANNEL_ERROR:
		return "Error generating DECODE.CHANNEL";
	case TRANS_INTERNAL_ERROR:
		return "Internal error in trans";
	default:
		return (NULL);
	}
}

/*}}}  */
/*{{{  PUBLIC const char *abbrevmode_string */
PUBLIC const char *abbrevmode_string (const abbrevmode_t am)
{
	switch (am) {
	case AM_CONST:
		return "AM_CONST";
	case AM_ISRHS:
		return "AM_ISRHS";
	case AM_VAL:
		return "AM_VAL";
	case AM_PTR:
		return "AM_PTR";
	case AM_COPYINOUT:
		return "AM_COPYINOUT";
	case AM_NOTHING:
		return "AM_NOTHING";
	}
	return "AM_???";	/* to shut up icc's warning */
}

/*}}}  */
