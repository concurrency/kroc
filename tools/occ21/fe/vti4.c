/* $Id: vti4.c,v 1.4 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	occam two virtual tree extra access routines
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

/*#define DEBUG*/

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "midinc.h"

#include "vtierror.h"

#include "chkdef.h"		/* current_fe_data */
#include "harndef.h"		/* for mobile_size_field */
#include "trandef.h"		/* for isdynmobilechantypetype */
#include "genkroc.h"		/* for WSH */
/*}}}*/

/*{{{  definitions*/
/* Negative sizes totally confuse the workspace allocator */
#define MAX_16BIT_RETYPE_SIZE     ((BIT32)0x7fff)	/* bug 1359 24/9/91 */
#define MAX_16BIT_ARRAY_SIZE     ((BIT32)0x10000)	/* bug 1359 24/9/91 */
#define MAX_32BIT_ARRAY_SIZE ((BIT32)0x7fffffff)
/*}}}*/

/*{{{  PUBLIC BOOL parrepl*/
PUBLIC BOOL parrepl (const int tag)
{
	switch (tag) {
	case S_REPLPAR:
	case S_PRIREPLPAR:
	case S_PLACEDREPLPAR:
	case S_REPLDO:
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL network_datatype(int type)*/
PUBLIC BOOL network_datatype (int type)
{
	switch (type) {
		CASE_CONFIG_TYPE return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/

/*{{{  PUBLIC INT32 min_INT32 (x, y)*/
PUBLIC INT32 min_INT32 (INT32 x, INT32 y)
{
	return (x < y ? x : y);
}

/*}}}*/
/*{{{  PUBLIC INT32 max_INT32 (x, y)*/
PUBLIC INT32 max_INT32 (INT32 x, INT32 y)
{
	return (x > y ? x : y);
}

/*}}}*/

/*{{{  PUBLIC treenode *follow_user_type*/
#ifdef OCCAM2_5
PUBLIC treenode *follow_user_type (treenode * tptr)
{
	for (;;) {
		if (TagOf (tptr) == N_TYPEDECL) {
			tptr = NTypeOf (tptr);
		} else {
			break;
		}
	}
	return tptr;
}
#endif

/*}}}*/

/*{{{  PUBLIC BOOL isint (t)*/
PUBLIC BOOL isint (const int t)
{
	switch (t) {
	case S_INT:
	case S_INT16:
	case S_INT32:
	case S_INT64:
	case S_UINT:
	case S_UINT16:
	case S_UINT32:
	case S_UINT64:
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL isintorbyte (t)*/
PUBLIC BOOL isintorbyte (const int t)
{
	switch (t) {
	case S_INT:
	case S_INT16:
	case S_INT32:
	case S_INT64:
	case S_UINT:
	case S_UINT16:
	case S_UINT32:
	case S_UINT64:
	case S_BYTE:
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL isreal (t)*/
PUBLIC BOOL isreal (const int t)
{
	return (t == S_REAL32 || t == S_REAL64);
}

/*}}}*/
/*{{{  PUBLIC BOOL isscalartype (t)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  isscalartype returns TRUE if t is a scalar type, FALSE otherwise.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC BOOL isscalartype (const int t)
{
	switch (t)
		/*{{{  cases */
	{
	default:
		return (FALSE);
	case S_BOOL:
	case S_BYTE:
	case S_INT:
	case S_INT16:
	case S_INT32:
	case S_INT64:
	case S_UINT:
	case S_UINT16:
	case S_UINT32:
	case S_UINT64:
	case S_REAL32:
	case S_REAL64:
	case S_CHAN:
	case S_PORT:
	case S_BARRIER:
	case S_FULLBARRIER:
#ifdef MOBILES
	case S_MOBILE:
	case S_ANYCHANTYPE:
	case S_ANYPROCTYPE:
	case S_ANYMOBILETYPE:
#endif	/* MOBILES */
	CASE_CONFIG_TYPE case S_TIMER:
		return (TRUE);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC BOOL isint_tree*/
PUBLIC BOOL isint_tree (treenode * const tptr)
{
	return isint (TagOf (follow_user_type (tptr)));
}

/*}}}*/
/*{{{  PUBLIC BOOL isintorbyte_tree*/
PUBLIC BOOL isintorbyte_tree (treenode * const tptr)
{
	return isintorbyte (TagOf (follow_user_type (tptr)));
}

/*}}}*/
/*{{{  PUBLIC BOOL isreal_tree*/
PUBLIC BOOL isreal_tree (treenode * const tptr)
{
	return isreal (TagOf (follow_user_type (tptr)));
}

/*}}}*/
/*{{{  PUBLIC BOOL isscalartype_tree*/
PUBLIC BOOL isscalartype_tree (treenode * const tptr)
{
	return isscalartype (TagOf (follow_user_type (tptr)));
}

/*}}}*/
/*{{{  PUBLIC BOOL isdynamicmobiletype (treenode *const t)*/
PUBLIC BOOL isdynamicmobiletype (treenode *const t)
{
	BOOL dynmob = FALSE;

#if 0
fprintf (stderr, "vti4: isdynamicmobiletype, t = ");
printtreenl (stderr, 4, t);
#endif
	if ((TagOf (t) == S_ANYCHANTYPE) || (TagOf (t) == S_ANYPROCTYPE) || (TagOf (t) == S_ANYMOBILETYPE)) {
		dynmob = TRUE;
	} else if (TagOf (t) != S_MOBILE) {
		dynmob = FALSE;
	} else if (TagOf (MTypeOf (t)) == S_RECORD) {
		treenode *rtype = ARTypeOf (MTypeOf (t));

		/* find channel decl. */
		if ((TagOf (rtype) == S_DECL) && (TagOf (DNameOf (rtype)) == N_FIELD)) {
			if (TagOf (NTypeOf (DNameOf (rtype))) == S_CHAN) {
				dynmob = TRUE;
			}
		}
	} else if (TagOf (MTypeOf (t)) == S_ARRAY) {
		treenode *arr = MTypeOf (t);

		if ((ARDimOf (arr) == -1) && ((ARDimLengthOf (arr) == NULL) || (TagOf (ARDimLengthOf (arr)) == S_NTH_DIMENSION))) {
			dynmob = TRUE;
		}
	}
#if 0
fprintf (stderr, "vti4: isdynamicmobiletype, returning %d\n", dynmob);
#endif
	return dynmob;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynamicmobilearraytype (treenode *const t)*/
PUBLIC BOOL isdynamicmobilearraytype (treenode *const t)
{
	if (!t || (TagOf (t) != S_MOBILE)) {
		return FALSE;
	} else if (TagOf (MTypeOf (t)) == S_ARRAY) {
		treenode *arr = MTypeOf (t);

		if ((ARDimOf (arr) == -1) && ((ARDimLengthOf (arr) == NULL) || (TagOf (ARDimLengthOf (arr)) == S_NTH_DIMENSION))) {
			return TRUE;
		}
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL isdynamicmobilechantype (treenode *const t)*/
PUBLIC BOOL isdynamicmobilechantype (treenode *const t)
{
	if (!t) {
		return FALSE;
	}
	if (TagOf (t) == S_ANYCHANTYPE) {
		return TRUE;
	} else if (TagOf (t) != S_MOBILE) {
		return FALSE;
	} else if (TagOf (MTypeOf (t)) == S_RECORD) {
		treenode *rtype = ARTypeOf (MTypeOf (t));

		/* find channel decl. */
		if ((TagOf (rtype) == S_DECL) && (TagOf (DNameOf (rtype)) == N_FIELD)) {
			if (TagOf (NTypeOf (DNameOf (rtype))) == S_CHAN) {
				return TRUE;
			}
		}
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL issamedynamicmobilechantype (treenode *const t1, treenode *const t2)*/
/*
 *	determines whether two mobile channel structures are the "same".
 */
PUBLIC BOOL issamedynamicmobilechantype (treenode *const t1, treenode *const t2)
{
	if (!t1 || !t2) {
		return FALSE;
	}
	if (TagOf (t1) != TagOf (t2)) {
		return FALSE;
	}
	if (TagOf (t1) == N_TYPEDECL) {
		treenode *n1 = (treenode *)NNameOf (t1);
		treenode *n2 = (treenode *)NNameOf (t2);

		if (n1 == n2) {
			return TRUE;
		}
#if 0
fprintf (stderr, "issamedynamicmobilechantype(): n1 at %p, n2 at %p\n", n1, n2);
#endif
	} else {
		return issame (t1, t2);
	}
	return FALSE;
}
/*}}}*/

/*{{{  PUBLIC BOOL istypetag*/
PUBLIC BOOL istypetag (const int tag)
{
	switch (tag) {
	case S_BOOL:
	case S_BYTE:
	case S_INT:
	case S_INT16:
	case S_INT32:
	case S_INT64:
	case S_UINT:
	case S_UINT16:
	case S_UINT32:
	case S_UINT64:
	case S_REAL32:
	case S_REAL64:
	case S_TIMER:
	CASE_CONFIG_TYPE case S_RECORD:
	case S_CHAN:
	case S_PORT:
	#ifdef MOBILES
		case S_MOBILE:
		case S_ANYCHANTYPE:
		case S_ANYPROCTYPE:
		case S_ANYMOBILETYPE:
	#endif
	case S_ARRAY:
	case N_TYPEDECL:
	#ifdef MOBILES
		case N_PROCTYPEDECL:
	#endif
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/
/*{{{  PUBLIC int bytesinscalar*/
PUBLIC int bytesinscalar (const int tag)
{
	switch (tag) {
		/*{{{  cases */
	case S_BOOL:
	case S_BYTE:
		return 1;
	case S_INT16:
	case S_UINT16:
		return 2;
	case S_INT32:
	case S_UINT32:
	case S_REAL32:
		return 4;
	case S_INT64:
	case S_UINT64:
	case S_REAL64:
		return 8;
#ifdef MOBILES
	case S_MOBILE:
	case S_ANYMOBILETYPE:
		/* happens if we're called from processing something like "LD x", where x is a MOBILE var.. */
		/* only interested in the word quantity.. */
		return (1 << WSH);
		/* badtag (0, tag, "bytesinscalar");
		return -1; */
	case S_ANYCHANTYPE:
	case S_ANYPROCTYPE:
		/* MOBILE.CHAN and MOBILE.PROC declarations need 2 words of workspace -- one for the typehash, one for the pointer.
		 * pointer goes in first. */
		return (2 << WSH);
#endif
	case S_INT:
	case S_UINT:
	case S_CHAN:
		return current_fe_data->fe_txlib->bpw;
		/*case S_PORT:   chkreport_i(CHK_UNKNOWN_TYPE, chklocn, t); return(-1); */
	case S_TIMER:
		return 0;	/* Added 23/4/90 by CO'N for bug 287 */
	case S_BARRIER:
	case S_FULLBARRIER:
		return (1 << WSH);
	case S_RECORD:
	case S_ANONCHANTYPE:
	case S_ARRAY:
	case S_UNDECLARED:
		return -1;
	case N_DECL:
		/* this happens in various error conditions */
		return 0;
	default:
		badtag (0, tag, "bytesinscalar");
		return (-1);
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC int bytesinscalar_tree*/
PUBLIC int bytesinscalar_tree (treenode * const tptr)
{
	return bytesinscalar (TagOf (basetype_tree (tptr)));
}

/*}}}*/
/*{{{  PUBLIC BOOL check_array_overflow*/
PUBLIC BOOL check_array_overflow (const SOURCEPOSN locn, const BIT32 elemsize, const BIT32 nelements, const BOOL in_RETYPE)
/* returns TRUE if there was an overflow */
{
	const BIT32 max_array_size = (current_fe_data->fe_txlib->bpw != 2) ? MAX_32BIT_ARRAY_SIZE :
		(in_RETYPE) ? MAX_16BIT_RETYPE_SIZE : MAX_32BIT_ARRAY_SIZE;

	if (nelements == 0) {
		return FALSE;
	} else if (elemsize > (max_array_size / nelements)) {
		vtiabort (VTI_ARRAY_SIZE_OVERFLOW, locn);
		return TRUE;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC INT32 bytesin (t)*/
PUBLIC INT32 bytesin (const treenode *const t)
{
	switch (TagOf (t)) {
	default:
		return bytesinscalar (TagOf (t));
#ifdef OCCAM2_5
	case N_TYPEDECL:
		return bytesin (NTypeOf (t));
	case S_RECORD:
		return ARDimOf (t);
#endif	/* OCCAM2_5 */
#ifdef MOBILES
	case S_MOBILE:
		return bytesin (MTypeOf (t));
	case N_PROCTYPEDECL:
	case N_MPROCDECL:
		return -1;
#endif
	case S_PORT:
		return bytesin (ProtocolOf (t));
	case S_ARRAY:
		{
			INT32 b;
			const INT32 d = ARDimOf (t);

			if (d == (-1)) {
				return (-1);
			}
			b = bytesin (ARTypeOf (t));
			if (b == (-1)) {
				return (-1);
			}
			if (d != 0) {
				(void)check_array_overflow (LocnOf (t), b, d, FALSE);
			}

			return ((INT32) ((BIT32) d * (BIT32) b));
		}
	}
}
/*}}}*/
#ifdef MOBILES
/*{{{  PUBLIC INT32 bytesin_raw (const treenode *const t)*/
PUBLIC INT32 bytesin_raw (const treenode *const t)
{
	switch (TagOf (t)) {
	default:
		return bytesinscalar (TagOf (t));
#ifdef OCCAM2_5
	case N_TYPEDECL:
		return bytesin_raw (NTypeOf (t));
	case S_RECORD:
		return ARDimOf (t);
#endif
#ifdef MOBILES
	case S_MOBILE:
	case N_PROCTYPEDECL:
		return current_fe_data->fe_txlib->bpw;
	case N_MPROCDECL:
		/* FIXME: return number of bytes required to store this in mobilespace */
		return -1;
#endif
	case S_PORT:
		return bytesin_raw (ProtocolOf (t));
	case S_ARRAY:
		{
			INT32 b;
			const INT32 d = ARDimOf (t);
			if (d == (-1))
				return (-1);
			b = bytesin_raw (ARTypeOf (t));
			if (b == (-1))
				return (-1);
			if (d != 0)
				(void) check_array_overflow (LocnOf (t), b, d, FALSE);
			return ((INT32) ((BIT32) d * (BIT32) b));
		}
	}
}
/*}}}*/
#endif
/*{{{  PUBLIC BIT32 wordsin (tptr)*/
PUBLIC BIT32 wordsin (treenode * const tptr)
{
	const INT32 b = bytesin (tptr);

	if (b <= 0) {
		return (0);
	} else {
		return (((b - 1) / current_fe_data->fe_txlib->bpw) + 1);	/* round up to nearest word */
	}
}

/*}}}*/
/*{{{  PUBLIC INT32 elementsin (t)*/
PUBLIC INT32 elementsin (treenode * const t)
{
	switch (TagOf (t)) {
		/*{{{  ARRAY */
	case S_ARRAY:
		if (ARDimOf (t) == (-1))
			return (-1);
		else {
			const INT32 e = elementsin (ARTypeOf (t));
			return (e == (-1)) ? (-1) : (ARDimOf (t) * e);
		}
		/*}}} */
		/*{{{  PORT */
	case S_PORT:
		return elementsin (ProtocolOf (t));
		/*}}} */
		/*{{{  RECORD */
#ifdef OCCAM2_5
	case S_RECORD:
		return bytesin (t);
#ifdef MOBILES
	case S_MOBILE:
		return elementsin (MTypeOf (t));
#endif	/* MOBILES */
	case N_TYPEDECL:
		return elementsin (NTypeOf (t));
#endif
		/*}}} */
	default:
		return 1;
	}
}

/*}}}*/
/*{{{  PUBLIC INT32 known_bytesin*/
PUBLIC INT32 known_bytesin (const treenode * type)
{
	switch (TagOf (type)) {
	case N_TYPEDECL:
		return known_bytesin (NTypeOf (type));
	case S_ARRAY: {
		BIT32 dim = ARDimOf (type);
		if (dim == -1) {
			dim = 1;
		}
		return dim * known_bytesin (ARTypeOf (type));
	}
	case S_MOBILE:
		return known_bytesin (MTypeOf (type));
	default:
		return bytesin (type);
	}
}

/*}}}*/

/*{{{  PUBLIC treenode *basetype_tree (tptr)*/
PUBLIC treenode *basetype_tree (treenode * tptr)
{
	while (TRUE)
		switch (TagOf (tptr)) {
		case S_ARRAY:
			tptr = ARTypeOf (tptr);
			break;
		case S_PORT:
			tptr = ProtocolOf (tptr);
			break;
#ifdef OCCAM2_5
		case N_TYPEDECL:
			tptr = NTypeOf (tptr);
			break;
#endif
#ifdef MOBILES
		case S_MOBILE:
			if (!MTypeOf (tptr)) {
				/* unknown MOBILE types */
				return tptr;
			}
			tptr = MTypeOf (tptr);
			break;
#endif	/* MOBILES */
		default:
			return tptr;
		}
}

/*}}}*/
/*{{{  PUBLIC int basetype (tptr)*/
PUBLIC int basetype (treenode * const tptr)
{
	return TagOf (basetype_tree (tptr));
}

/*}}}*/

/*{{{  PUBLIC treenode *usertype_tree (tptr)*/
PUBLIC treenode *usertype_tree (treenode * tptr)
{
	while (TRUE)
		switch (TagOf (tptr)) {
		case S_ARRAY:
			tptr = ARTypeOf (tptr);
			break;
		case S_PORT:
			tptr = ProtocolOf (tptr);
			break;
#ifdef MOBILES
		case S_MOBILE:
			tptr = MTypeOf (tptr);
			break;
#endif	/* MOBILES */
		default:
			return tptr;
		}
}

/*}}}*/

/*{{{  typehash stuff*/

#define THFLAG_NONE 0x0000
#define THFLAG_NAMES 0x0001		/* include names in the type-hash */
#define THFLAG_EXT 0x0002		/* don't include type attributes from names */

/*{{{  PRIVATE INT32 typehash_int (treenode *tptr, int thflags)*/
/* never returns 0, that's a special thing.. */
PRIVATE INT32 typehash_int (treenode *tptr, int thflags)
{
	INT32 hashcode = 0xdefaced;

	if (!tptr) {
		return hashcode;
	}
#if 0
fprintf (stderr, "vti4: typehash: tptr = ");
printtreenl (stderr, 4, tptr);
#endif
	switch (TagOf (tptr)) {
	case N_DECL:
	case N_ABBR:
	case N_PARAM:
	case N_RETYPE:
		tptr = NTypeOf (tptr);
		break;
	}
	for (; tptr;) {
		switch (TagOf (tptr)) {
		case S_ARRAY:
			hashcode += 0xf00d;
			tptr = ARTypeOf (tptr);
			break;
		case S_PORT:
			hashcode ^= 0xf00d;
			tptr = ProtocolOf (tptr);
			break;
		case S_CHAN:
			/* don't consider channel direction _or_ PLACEd */
			hashcode ^= (0xbeef0 + ((TypeAttrOf (tptr) & ~(TypeAttr_placed | TypeAttr_marked_in | TypeAttr_marked_out))));
			tptr = ProtocolOf (tptr);
			break;
		case N_SPROTDEF:
			/* hashcode <<= 4; */
			/* silent */
			tptr = NTypeOf (tptr);
			break;
		case N_TPROTDEF:
			hashcode <<= 3;
			tptr = NTypeOf (tptr);
			break;
		case N_TYPEDECL:
			hashcode += 3;
			if (!(thflags & THFLAG_EXT)) {
				hashcode ^= NTypeAttrOf (tptr);
			}
			if (isdynmobilechantypetype (tptr) && kroc_chantype_desc) {
				/* alter hashcode slightly */
				hashcode ^= 0xfeed0000;
			}
			tptr = NTypeOf (tptr);
			break;
		case N_PROCTYPEDECL:
			hashcode += 25;
			tptr = NTypeOf (tptr);
			/* must not typehash augmented params, though! */
			while (!EndOfList (tptr)) {
				treenode *itm = ThisItem (tptr);

				switch (TagOf (itm)) {
				case S_PARAM_STATICLINK:
				case S_PARAM_VSP:
				case S_PARAM_MSP:
				case S_PARAM_MPP:
				case S_PARAM_FB:
				case S_HIDDEN_PARAM:
				case S_FNFORMALRESULT:
				case S_FNACTUALRESULT:
				case S_HIDDEN_TYPE:
					/* ignore hidden parameters! */
					break;
				default:
					hashcode <<= 3;
					hashcode |= 0x04;
					hashcode ^= typehash (itm);
					break;
				}

				tptr = NextItem (tptr);
			}
			break;
		case N_MPROCDECL:
			hashcode += 29;
			/* just typehash the parameters .. */
			tptr = NTypeOf (tptr);
			break;
		case S_LIST:
			if (!EndOfList (tptr) && EndOfList (NextItem (tptr))) {
				/* single-item list, singleton */
				tptr = ThisItem (tptr);
			} else {
				while (!EndOfList (tptr)) {
					hashcode <<= 3;
					hashcode |= 0x4;
					hashcode ^= typehash (ThisItem (tptr));

					tptr = NextItem (tptr);
				}
				return hashcode;
			}
			break;
		case N_FIELD:
			hashcode += 3;
			tptr = NTypeOf (tptr);
			break;
		case S_DECL:
			hashcode <<= 2;
			hashcode |= 0x1;
			if (thflags & THFLAG_NAMES) {
				if (TagOf (DNameOf (tptr)) == N_FIELD) {
					const char *fieldname = WNameOf (NNameOf (DNameOf (tptr)));
					const int fieldlen = WLengthOf (NNameOf (DNameOf (tptr)));
					int i;

					for (i=0; i<fieldlen; i++) {
						INT32 savebits = (hashcode >> 26) & 0x3f;

						hashcode <<= 4;
						hashcode |= ((fieldname[i] - 32) & 0x3f);
						hashcode ^= savebits;
					}
				}
				/* hashcode += typehash (DNameOf (tptr));		** disabled ** */
			}
			tptr = DBodyOf (tptr);
			break;
		case S_RECORD:
			tptr = ARTypeOf (tptr);
			break;
		case S_COLON2:
			hashcode += typehash (LeftOpOf (tptr));
			tptr = RightOpOf (tptr);
			break;
		case S_MOBILE:
			hashcode <<= 5;
			hashcode |= 0x3;
			tptr = MTypeOf (tptr);
			break;
		case S_FULLBARRIER:
			hashcode <<= 1;
			hashcode ^= 0x12345678;
			break;
		default:
#if 0
fprintf (stderr, "unknown typetree in typehash(), tptr =");
printtreenl (stderr, 4, tptr);
#endif
			hashcode += TagOf (tptr);
			return (hashcode ? hashcode : 0xdeadbeef);
		}
	}
	return (hashcode ? hashcode : 0xdeadbeef);
}
/*}}}  */
/*{{{  PUBLIC INT32 typehash (treenode *tptr)*/
/*
 *	generates a basic type-hash for some type tree
 */
PUBLIC INT32 typehash (treenode *tptr)
{
	INT32 hashcode = typehash_int (tptr, THFLAG_NONE);

	return hashcode;
}
/*}}}*/
/*{{{  PUBLIC INT32 typehash_names (treenode *tptr)*/
/*
 *	generates a type-hash for some type-tree that includes internal names
 */
PUBLIC INT32 typehash_names (treenode *tptr)
{
	INT32 hashcode = typehash_int (tptr, THFLAG_NAMES | THFLAG_EXT);

#if 0
fprintf (stderr, "generated hashcode 0x%8.8x for tptr = ", (unsigned int)hashcode);
printtreenl (stderr, 4, tptr);
if (TagOf (tptr) == N_PROCTYPEDECL) {
	fprintf (stderr, "NTypeOf (tptr) = ");
	printtreenl (stderr, 4, NTypeOf (tptr));
}
#endif
	return hashcode;
}
/*}}}*/

/*}}}*/

/*{{{  PUBLIC treenode *nameof(tptr)*/
/*****************************************************************************
 *
 *  return the underlying name of element 'tptr'
 *
 *****************************************************************************/
PUBLIC treenode *nameof (treenode *tptr)
{
	while (TRUE) {
		switch (nodetypeoftag (TagOf (tptr))) {
		case ARRAYSUBNODE:
			tptr = ASBaseOf (tptr);
			break;
		case SEGMENTNODE:
			tptr = SNameOf (tptr);
			break;
		default:
			return (tptr);
		}
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *basedecl(tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  basedecl returns the base declaration of the element 'tptr',
 *           ie. the subscripted array, the segmented array, or the
 *           abbreviated variable.  It cannot, however, dealias through
 *           formal parameters.
 *           Value abbreviations and retypes are not dealiased, as that
 *           might take us through to an expression.
 *           So the declaration returned is an
 *           N_DECL, N_REPL, N_PARAM, N_VALPARAM, N_VALABBR, N_VALRETYPE
 *           namenode, or a S_FNFORMALRESULT hidden parameter node.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC treenode *basedecl (treenode * tptr)
{
	while (TRUE)
		switch (TagOf (tptr))
			/*{{{  cases */
		{
			/*{{{  ARRAYSUB ARRAYITEM                   move to the subscripted array */
		case S_ARRAYSUB:
		case S_RECORDSUB:
		case S_ARRAYITEM:
		case S_RECORDITEM:
			tptr = ASBaseOf (tptr);
			break;
			/*}}} */
			/*{{{  SEGMENT  SEGMENTITEM                 move to the segmented array */
		case S_SEGMENT:
		case S_SEGMENTITEM:
			tptr = SNameOf (tptr);
			break;
			/*}}} */
			/*{{{  ABBR RETYPE                          move to the aliased variable */
		case N_ABBR:
		case N_RETYPE:
			/* If name is a channel constructor, return name */
			/* This fixes bug 273, but would be unnecessary if we didn't call
			   basedecl from transformalt */
			if (TagOf (DValOf (NDeclOf (tptr))) == S_CONSTRUCTOR)
				return (tptr);	/* can only be a channel constructor */
			tptr = DValOf (NDeclOf (tptr));
			break;
			/*}}} */
			/*{{{  DECL REPL PARAM VALPARAM RESULTPARAM FNFORMALRESULT VALABBR VALRETYPE */
		case N_DECL:
		case N_REPL:
		case N_PARAM:
		case N_VALPARAM:
		case N_RESULTPARAM:
		case N_VALABBR:
		case N_VALRETYPE:
		case N_FIELD:
		case T_TEMP:
		case T_PREEVALTEMP:
		case S_FNFORMALRESULT:
		case S_CONSTCONSTRUCTOR:	/* bug INSdi02268 */
		case S_CONSTRUCTOR:	/* ditto */
#ifdef OCCAM2_5
		case S_FINSTANCE:
#endif
			return (tptr);
			/*}}} */
		default:	/* for safety's sake */
			badtag (LocnOf (tptr), TagOf (tptr), "basedecl");
		}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC BOOL isnotexpression*/
PUBLIC BOOL isnotexpression (treenode * tptr)
/*****************************************************************************
 *
 *  isnotexpression returns TRUE if tptr is a name, or subscripted name, etc
 *                  (infact it is TRUE iff we can call basedecl)
 *
 *****************************************************************************/
{
	switch (nodetypeoftag (TagOf (tptr))) {
	case NAMENODE:
	case ARRAYSUBNODE:
	case SEGMENTNODE:
/*case HIDDENPARAMNODE: *//* incase of formal parameters */
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/

/*{{{  PUBLIC BOOL isspecification (tptr)*/
PUBLIC BOOL isspecification (treenode * tptr)
{
	return ((tptr != NULL) && (nodetypeoftag (TagOf (tptr)) == DECLNODE));
}

/*}}}*/
/*{{{  PUBLIC treenode *skipspecifications(tptr)*/
/*****************************************************************************
 *
 *  skipspecifications takes a tree and walks down from the root until
 *                     it finds a node which isn't a specification, and
 *                     returns a pointer to this node.
 *
 *****************************************************************************/
PUBLIC treenode *skipspecifications (treenode * tptr)
{
	while (isspecification (tptr)) {
		tptr = DBodyOf (tptr);
	}
	return (tptr);
}

/*}}}*/

/*{{{  PUBLIC BOOL separatelycompiled (nptr)*/
/*****************************************************************************
 *
 *  separatelycompiled returns TRUE if the namenode nptr represents an
 *                     item which has been separately compiled.
 *
 *****************************************************************************/
PUBLIC BOOL separatelycompiled (treenode * const nptr)
{
	switch (TagOf (nptr)) {
	case N_SCPROCDEF:
	case N_SCFUNCDEF:
	case N_LIBPROCDEF:
	case N_LIBMPROCDECL:
	case N_LIBFUNCDEF:
	case N_STDLIBPROCDEF:
	case N_STDLIBFUNCDEF:
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL issimple (tptr)*/
/*****************************************************************************
 *
 *  BOOL issimple returns TRUE if the element tptr is simple, ie. not
 *               subscripted or segmented.
 *
 *****************************************************************************/
PUBLIC BOOL issimple (treenode * const tptr)
{
	switch (TagOf (tptr))
		/*{{{  cases */
	{
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_DECL:
	case N_REPL:
	case T_TEMP:
	case T_PREEVALTEMP:
		/* A function result pointer is effectively an abbreviation */
	case S_FNFORMALRESULT:
		return (TRUE);
	default:
		return (FALSE);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC BOOL isinline (nptr)*/
/*****************************************************************************
 *
 * inline returns TRUE if the routine 'nptr' is to be expanded inline
 *
 *****************************************************************************/
PUBLIC BOOL isinline (treenode *const nptr)
{
	return TagOf (nptr) == N_INLINEFUNCDEF || TagOf (nptr) == N_INLINEPROCDEF;
}

/*}}}*/

/*{{{  PUBLIC BOOL isnamedformal*/
PUBLIC BOOL isnamedformal (const treenode *const tptr)
{
	return ((TagOf (tptr) == N_PARAM) || (TagOf (tptr) == N_VALPARAM) || (TagOf (tptr) == N_RESULTPARAM));
}

/*}}}*/
/*{{{  PUBLIC BOOL ishiddenformal*/
PUBLIC BOOL ishiddenformal (const treenode *const tptr)
{
	return !isnamedformal (tptr);
}

/*}}}*/
/*{{{  PUBLIC BOOL ishiddenparam*/
/*****************************************************************************
 *
 *  ishiddenparam takes a parameter node, 'param', and returns TRUE
 *                if it is a hidden parameter, FALSE otherwise.
 *
 *****************************************************************************/
PUBLIC BOOL ishiddenparam (const treenode *const param)
{
	switch (TagOf (param)) {
	case S_PARAM_STATICLINK:
	case S_PARAM_VSP:
	case S_PARAM_FB:
#ifdef MOBILES
	case S_PARAM_MSP:
	/* case S_PARAM_MPP: -- this is not hidden */
	case S_HIDDEN_TYPE:
#endif
	case S_HIDDEN_PARAM:
	case S_FNFORMALRESULT:
	case S_FNACTUALRESULT:
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/

/*{{{  PUBLIC treenode **dimexpaddr(tptr, dimension)*/
/*****************************************************************************
 *
 *  dimexpaddr returns a pointer to the dimension tree for the
 *             dimension'th dimension of element tptr.
 *             The first dimension is dimension 0.
 *
 *****************************************************************************/
PUBLIC treenode **dimexpaddr (treenode *tptr, int dimension)
{
	while (TRUE) {
#if 0
fprintf (stderr, "dimexpaddr: dimension = %d. tptr = ", dimension);
printtreenl (stderr, 4, tptr);
#endif
		switch (TagOf (tptr)) {
			/*{{{  ARRAYSUB ARRAYITEM       break */
		case S_ARRAYITEM:
		case S_ARRAYSUB:
			dimension++;
			tptr = ASBaseOf (tptr);
			break;
		case S_RECORDITEM:
		case S_RECORDSUB:	/* added by MDP 10/3/97, analogy with SK's suggestion */
			tptr = ASIndexOf (tptr);
			break;
			/*}}} */
			/*{{{  SEGMENT SEGMENTITEM      break / return */
		case S_SEGMENT:
		case S_SEGMENTITEM:
			/* The segment length may already have been evaluated into a temp,
			   in which case dimexp will be a temp node */
			if (dimension == 0)
				return (SLengthExpAddr (tptr));
			else
				tptr = SNameOf (tptr);
			break;
			/*}}} */
			/*{{{  CONSTCONSTRUCTOR         break */
		case S_CONSTCONSTRUCTOR:
			tptr = CTExpOf (tptr);
			break;
			/*}}} */
			/*{{{  CONSTRUCTOR              break / return */
		case S_CONSTRUCTOR:
			if (dimension > 0) {
				tptr = ThisItem (LitExpOf (tptr));
				dimension--;
			} else
				return (OpAddr (newmopnode (S_DUMMYEXP, NOPOSN, newconstant (listitems (LitExpOf (tptr))), 0)));
			break;
			/*}}} */
			/*{{{  name                             return */
		case N_VALABBR:
		case N_ABBR:
		case N_VALRETYPE:
		case N_RETYPE:
		case N_DECL:
		case N_VALPARAM:
		case N_PARAM:
		case N_RESULTPARAM:
		case T_TEMP:
		case T_PREEVALTEMP:
		case S_FNFORMALRESULT:
		case N_FIELD:
#ifdef OCCAM2_5
		case S_FINSTANCE:
#endif
		{
#ifdef MOBILES
			BOOL ismobile = FALSE;
#endif

			tptr = chk_gettype (tptr);
#ifdef MOBILES
			if (TagOf (tptr) == S_MOBILE) {
				tptr = MTypeOf (tptr);
				ismobile = TRUE;
			}
#endif
			while (dimension > 0) {
				tptr = follow_user_type (ARTypeOf (tptr));
				dimension--;
			}
#ifdef MOBILES
			if (TagOf (tptr) == S_MOBILE) {
				tptr = MTypeOf (tptr);
			}
#if 0
			if (mobile_size_field && !dimension && ismobile && (bytesin (tptr) > 0)) {
				const int bytes = bytesin (tptr);

				return OpAddr (newmopnode (S_DUMMYEXP, NOPOSN, newconstant (bytes), 0));
			}
#endif
#endif
			return (ARDimLengthAddr (tptr));
		}
			/*}}} */
			/*{{{  string */
		case S_STRING:
			return (OpAddr (newmopnode (S_DUMMYEXP, NOPOSN, newconstant (WLengthOf (CTValOf (tptr))), 0)));
			/*}}} */
		default:
			badtag (LocnOf (tptr), TagOf (tptr), "dimexpaddr");
		}
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *dimexpof(tptr, dimension)*/
/*****************************************************************************
 *
 *  dimexpof returns the dimension tree for the dimension'th dimension
 *           of element tptr.
 *           The first dimension is dimension 0.
 *
 *****************************************************************************/
PUBLIC treenode *dimexpof (treenode * tptr, int dimension)
{
#ifdef MOBILES
	treenode *origtptr = tptr;
	int origdim = dimension;
#endif

#if 0
fprintf (stderr, "dimexpof: dimension = %d, tptr = ", dimension);
printtreenl (stderr, 4, tptr);
#endif
	while (TRUE) {
		/*{{{  cases */
		switch (TagOf (tptr)) {
			/*{{{  ARRAYSUB ARRAYITEM       break */
		case S_ARRAYITEM:
		case S_ARRAYSUB:
			dimension++;
			tptr = ASBaseOf (tptr);
			break;
		case S_RECORDITEM:	/* added by MDP 26/4/96, as suggested by SK */
		case S_RECORDSUB:
			tptr = ASIndexOf (tptr);
			break;
			/*}}} */
			/*{{{  SEGMENT SEGMENTITEM      break / return */
		case S_SEGMENT:
		case S_SEGMENTITEM:
			/* The segment length may already have been evaluated into a temp,
			   in which case dimexp will be a temp node */
			if (dimension == 0)
				return SLengthExpOf (tptr);
			else
				tptr = SNameOf (tptr);
			break;
			/*}}} */
			/*{{{  CONSTCONSTRUCTOR         break */
		case S_CONSTCONSTRUCTOR:
			tptr = CTExpOf (tptr);
			break;
			/*}}} */
			/*{{{  CONSTRUCTOR              break / return */
		case S_CONSTRUCTOR:
			if (dimension > 0) {
				tptr = ThisItem (LitExpOf (tptr));
				dimension--;
			} else
				return newconstant (listitems (LitExpOf (tptr)));
			break;
			/*}}} */
			/*{{{  DECL (skip over to valof) */
		case S_DECL:
			tptr = DBodyOf (tptr);
			break;
			/*}}}  */
			/*{{{  VALOF process */
		case S_VALOF:
			tptr = VLResultListOf (tptr);
			/* should only be one item in here! */
			if (TagOf (tptr) == S_LIST) {
				tptr = ThisItem (tptr);
			}
			break;
			/*}}}  */
			/*{{{  nullarray (frmb) */
		case S_NULLARRAY:
			return newconstant (0);
			/*}}}*/
			/*{{{  name                             return */
		case N_VALABBR:
		case N_ABBR:
		case N_VALRETYPE:
		case N_RETYPE:
		case N_DECL:
		case N_VALPARAM:
		case N_PARAM:
		case N_RESULTPARAM:
		case T_TEMP:
		case T_PREEVALTEMP:
		case S_FNFORMALRESULT:
		case N_FIELD:
#ifdef OCCAM2_5
		case S_FINSTANCE:
#endif
			tptr = chk_gettype (tptr);
#if 0
fprintf (stderr, "dimexpof: chk_gettype () = ");
printtreenl (stderr, 4, tptr);
#endif
			while (dimension > 0) {
#ifdef MOBILES
				if (TagOf (tptr) == S_MOBILE) {
					/* bump into this for both static and dynamic MOBILEs */
					tptr = MTypeOf (tptr);
				}
#endif
				tptr = follow_user_type (ARTypeOf (tptr));
				dimension--;
			}
#ifdef MOBILES
			if (!ARDimLengthOf (tptr)) {
				/* no NTH_DIMENSION here, if its a RECORDSUB above, for example */
				/* also get here for nested MOBILE types */
#if 0
fprintf (stderr, "dimexpof: here!  origdim = %d\n", origdim);
#endif
				return newdopnode (S_NTH_DIMENSION, NOPOSN, origtptr, newconstant (origdim + 1), S_INT);
			}
#endif
			return (ARDimLengthOf (tptr));
			/*}}} */
			/*{{{  string                           return */
		case S_STRING:
			return newconstant (WLengthOf (CTValOf (tptr)));
			/*}}} */
		default:
			badtag (LocnOf (tptr), TagOf (tptr), "dimexpof");
			break;
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *skipevals (treenode *tptr)*/
/*****************************************************************************
 *
 *  skipevals skips any leading 'eval' nodes on tree 'tptr'.
 *
 *****************************************************************************/
PUBLIC treenode *skipevals (treenode * tptr)
{
	if (tptr != NULL)
		while (TagOf (tptr) == S_EVAL)
			tptr = RightOpOf (tptr);
	return tptr;
}

/*}}}*/

/*{{{  PRIVATE treenode *skipdims(treenode *tptr)*/
/*****************************************************************************
 *
 *  skipdims takes a treenode tptr, and walks inside any S_ELSIZE or
 *           S_SEGSTART nodes.
 *
 *****************************************************************************/
PRIVATE treenode *skipdims (treenode * tptr)
{
	if (TagOf (tptr) == S_ELSIZE)
		return dimexpof (OpOf (tptr), 0);
	else if (TagOf (tptr) == S_SEGSTART)
		return SStartExpOf (OpOf (tptr));
	else
		return tptr;
}

/*}}}*/
/*{{{  PUBLIC BOOL issame(treenode *t1, treenode *t2)*/
/*****************************************************************************
 *
 *  issame takes two expression trees, 't1' and 't2' and returns TRUE
 *         if they represent the same expression, FALSE otherwise.
 *
 *****************************************************************************/
PUBLIC BOOL issame (treenode * t1, treenode * t2)
{
	while (t1 != NULL && t2 != NULL && (TagOf (t1 = skipdims (t1)) == TagOf (t2 = skipdims (t2))))
		/*{{{  look at the nodes */
		switch (TagOf (t1)) {
		default:
			return FALSE;
			/*{{{  monadics */
		case S_NEG:
		case S_UMINUS:
		case S_UPLUS:
		case S_BITNOT:
		case S_NOT:
		case S_SIZE:
#ifdef OCCAM2_5
		case S_BYTESIN:
#endif
		case S_TYPEHASHOF:
#ifdef MOBILES
		case S_ADDROF:
		case S_HWADDROF:
#endif
			t1 = OpOf (t1);
			t2 = OpOf (t2);
			break;
			/* These two are handled by skipdims :
			   case S_ELSIZE:
			   case S_SEGSTART: */
			/*}}} */
			/*{{{  conversions */
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			if (MOpTypeOf (t1) != MOpTypeOf (t2))
				return FALSE;
			t1 = OpOf (t1);
			t2 = OpOf (t2);
			break;
			/*}}} */
			/*{{{  dyadics */
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
#ifdef OCCAM2_5
		case S_OFFSETOF:
#endif
			if (!issame (LeftOpOf (t1), LeftOpOf (t2)))
				return FALSE;
			t1 = RightOpOf (t1);
			t2 = RightOpOf (t2);
			break;
			/*}}} */
			/*{{{  instance */
		case S_FINSTANCE:
			if (!issame (INameOf (t1), INameOf (t2)))
				return FALSE;
			t1 = IParamListOf (t1);
			t2 = IParamListOf (t2);
			break;
			/*}}} */
			/*{{{  list */
		case S_LIST:
			if (!issame (ThisItem (t1), ThisItem (t2)))
				return FALSE;
			t1 = NextItem (t1);
			t2 = NextItem (t2);
			break;
			/*}}} */
			/*{{{  constant expression */
		case S_CONSTEXP:
			if (LoValOf (t1) != LoValOf (t2))
				return FALSE;
			else
				return (bytesinscalar (fe_typeof (t1)) < 4) || (HiValOf (t1) == HiValOf (t2));
			/*}}} */
			/*{{{  element */
		case S_ARRAYSUB:
		case S_RECORDSUB:
			if (!issame (ASBaseOf (t1), ASBaseOf (t2)))
				return FALSE;
			t1 = ASIndexOf (t1);
			t2 = ASIndexOf (t2);
			break;
		case S_ARRAYITEM:
		case S_RECORDITEM:
			if (!issame (ASBaseOf (t1), ASBaseOf (t2)) || ASOffsetOf (t1) != ASOffsetOf (t2))
				return FALSE;
			t1 = ASExpOf (t1);
			t2 = ASExpOf (t2);
			break;
		case N_DECL:
		case N_REPL:
		case N_ABBR:
		case N_VALABBR:
		case N_RETYPE:
		case N_VALRETYPE:
		case N_PARAM:
		case N_VALPARAM:
		case N_RESULTPARAM:
		case N_TAGDEF:
		case N_SFUNCDEF:
		case N_LFUNCDEF:
		case N_SCFUNCDEF:
		case N_LIBFUNCDEF:
		case N_PREDEFFUNCTION:
		case T_TEMP:
		case T_PREEVALTEMP:
		case N_FIELD:
			return t1 == t2;
			/*}}} */
			/*{{{  special variables */
		case S_HIDDEN_PARAM:
		case S_HIDDEN_TYPE:
			t1 = HExpOf (t1);
			t2 = HExpOf (t2);
			break;
			/*}}} */
		}
	/*}}} */
	return (t1 == t2);
}

/*}}}*/
