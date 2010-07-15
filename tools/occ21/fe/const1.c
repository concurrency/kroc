/* $Id: const1.c,v 1.7 1997/03/25 14:52:40 djb1 Exp $ */


/*
 *	occam two constant expression evaluation
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
/*#define TARGET_BIGENDIAN*/

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>

#include "midinc.h"
#include "extlib.h"		/* IMPORTED */
#include "lexdef.h"		/* for lexmode */

#include "chkerror.h"
#include "chkdef.h"		/* chk_gettype and chk_typeof */
#include "constdef.h"
#include "predefhd.h"		/* checking shift counts for ASHIFTLEFT etc */
/*}}}  */

/*{{{  defines */
#define TARGET_LITTLEENDIAN TRUE	/* transputer is little endian */
/*}}}  */

/*{{{  local variables */
PRIVATE BOOL overlaps_are_const = FALSE;
#ifdef OCCAM2_5
PRIVATE BOOL checking_const_fn = FALSE;
PRIVATE int const_fn_lexlevel;
#endif
/*}}}  */

/*{{{  PUBLIC int checkbounds (sourcetype, desttype, shi, slo) */
/* Check that the byte or integer value of type sourcetype held in shi:slo
can be represented as a byte or integer value of type desttype */
PUBLIC int checkbounds (int sourcetype, int desttype, BIT32 shi, BIT32 slo, const SOURCEPOSN locn)
{
	BIT32 maxhi, maxlo, minhi, minlo;
	BOOL toobig, toosmall;

	if (sourcetype != S_INT64)
		/*{{{  sign extend slo into shi for 64-bit comparison */
		I32ToI64 (&shi, &slo, slo);
	/*}}}  */
	/*{{{  set up maxhi, maxlo, minhi, minlo */
	switch (desttype) {
		/*{{{  S_BOOL */
	case S_BOOL:
		maxhi = 0;
		maxlo = 1;
		minhi = 0;
		minlo = 0;
		break;
		/*}}}  */
		/*{{{  S_BYTE */
	case S_BYTE:
		maxhi = 0;
		maxlo = 255;
		minhi = 0;
		minlo = 0;
		break;
		/*}}}  */
		/*{{{  S_INT16 */
	case S_INT16:
		maxhi = 0;
		maxlo = MOSTPOS_INT16;
		minhi = 0xffffffffl;
		minlo = MOSTNEG_INT16;
		break;
		/*}}}  */
		/*{{{  S_UINT16*/
	case S_UINT16:
		maxhi = 0;
		maxlo = 0x0000ffffl;
		minhi = 0;
		minlo = 0;
		break;
		/*}}}*/
		/*{{{  S_INT32 */
	case S_INT32:
		maxhi = 0;
		maxlo = MOSTPOS_INT32;
		minhi = 0xffffffffl;
		minlo = MOSTNEG_INT32;
		break;
		/*}}}*/
		/*{{{  S_UINT32*/
	case S_UINT32:
		maxhi = 0;
		maxlo = 0xffffffffl;
		minhi = 0;
		minlo = 0;
		break;
		/*}}}*/
		/*{{{  S_INT64 */
	case S_INT64:
		maxhi = MOSTPOS_INT32;
		maxlo = 0xffffffffl;
		minhi = MOSTNEG_INT32;
		minlo = 0;
		break;
		/*}}}  */
		/*{{{  S_UINT64 */
	case S_UINT64:
		maxhi = 0xffffffffl;
		maxlo = 0xffffffffl;
		minhi = 0;
		minlo = 0;
		break;
		/*}}}  */
		/* Should never get here */
	default:
		{
			maxhi = maxlo = minhi = minlo = 0;	/* initialised to shut up gcc's optimiser */
			badtag (locn, desttype, "checkbounds");
		}
	}
	/*}}}  */
	Int64Gt (&toobig, shi, slo, maxhi, maxlo);
	Int64Gt (&toosmall, minhi, minlo, shi, slo);

	if (toobig)
		return (1);
	else if (toosmall)
		return (-1);
	else
		return (0);
}

/*}}}  */
/*{{{  PRIVATE void IStrToInt (inttype, error, hi, lo, string) */
PRIVATE void IStrToInt (const int inttype, int *const error, BIT32 * const hi, BIT32 * const lo, const char *string)
{
	*hi = 0;		/* clean up messy constants - 23/11/90 */
	if (*string == '#') {
		/*{{{  read a hex string */
		string++;
		switch (inttype) {
		case S_BYTE:
			StrToH8 (error, lo, string);
			break;
		case S_INT16:
		case S_UINT16:
			StrToH16 (error, lo, string);
			break;
		case S_INT32:
		case S_UINT32:
			StrToHex (error, lo, string);
			break;
		case S_INT64:
		case S_UINT64:
			StrToH64 (error, hi, lo, string);
			break;
		}
		/*}}}  */
	} else if ((*string == '0') && (string[1] == 'x')) {
		/*{{{  read a C-like hex string */
		string += 2;
		switch (inttype) {
		case S_BYTE:
			StrToH8 (error, lo, string);
			break;
		case S_INT16:
		case S_UINT16:
			StrToH16 (error, lo, string);
			break;
		case S_INT32:
		case S_UINT32:
			StrToHex (error, lo, string);
			break;
		case S_INT64:
		case S_UINT64:
			StrToH64 (error, hi, lo, string);
			break;
		}
		/*}}}  */
	} else if (*string == '\'') {
		/*{{{  read a converted byte literal */
/* *hi = 0; *//* this has been moved up a bit */
		*lo = (BIT32) (int) (string[1] & 0xff);
		*error = FALSE;
		/*}}}  */
	} else {
		/*{{{  read a decimal string */
		/*printf("IStrToInt: inttype: %s, error: %d, lo: %ld, hi: %ld, string: \"%s\"\n",
		   itagstring(inttype), *error, *lo, *hi, string);
		 */
		switch (inttype) {
		case S_BYTE:
			StrToI8 (error, lo, string);
			break;
		case S_INT16:
		case S_UINT16:
			StrToI16 (error, lo, string);
			break;
		case S_INT32:
		case S_UINT32:
			StrToInt (error, lo, string);
			break;
		case S_INT64:
		case S_UINT64:
			StrToI64 (error, hi, lo, string);
			break;
		}
		/*printf("IStrToInt: inttype: %s, error: %d, lo: %ld, hi: %ld, string: \"%s\"\n",
		   itagstring(inttype), *error, *lo, *hi, string);
		 */
		/*}}}  */
	}
}

/*}}}  */

/*{{{  PRIVATE void exactconversion (sourcetype, desttype, dhi, dlo, shi, slo) */
PRIVATE void exactconversion (int sourcetype, int desttype, BIT32 * const dhi, BIT32 * const dlo, const BIT32 shi, const BIT32 slo, const SOURCEPOSN locn)
{
	if (isreal (desttype))
		/*{{{  real to real conversion */
	{
		if ((desttype == S_REAL64) && (sourcetype == S_REAL32))
			/* The only legal conversion which has any effect */
		{
			int error;
			R32ToR64 (&error, dhi, dlo, slo);
			if (error)
				chkreport (CHK_CFOLD_OFLOW, locn);
		}
	}
	/*}}}  */
	else
		/*{{{  bool byte or int conversion */
	{
		const int targetintsize = ((current_fe_data->fe_txlib->bpw == 2) ? S_INT16 : S_INT32);
		const int targetuintsize = ((current_fe_data->fe_txlib->bpw == 2) ? S_UINT16 : S_UINT32);

		if (sourcetype == S_INT) {
			sourcetype = targetintsize;
		} else if (sourcetype == S_UINT) {
			sourcetype = targetuintsize;
		}

		if (desttype == S_INT) {
			desttype = targetintsize;
		} else if (desttype == S_UINT) {
			desttype = targetuintsize;
		}

		if ((desttype == S_INT64) && (sourcetype != S_INT64)) {
			I32ToI64 (dhi, dlo, slo);
		} else if ((desttype == S_UINT64) && (sourcetype != S_UINT64)) {
			I32ToI64 (dhi, dlo, slo);
		} else {
			*dlo = slo;
			*dhi = shi;
		}
		if (checkbounds (sourcetype, desttype, *dhi, *dlo, locn) != 0) {
			chkreport (CHK_CFOLD_OFLOW, locn);
		}
	}
	/*}}}  */
}

/*}}}  */
/*{{{  pointing into contructors and constant tables */
/*{{{  PRIVATE treenode *point_at_list_item(tptr, n) */
/*****************************************************************************
 *
 *  point_at_list_item takes a pointer to a list, 'tptr', and moves
 *                     n items along the list, and returns a pointer to the
 *                     corresponding list item.
 *
 *****************************************************************************/
PRIVATE treenode *point_at_list_item (treenode * tptr, const BIT32 n)
{
	const SOURCEPOSN locn = LocnOf (tptr);
	int i;
	for (i = 0; i < n; i++) {
		if (EndOfList (tptr))	/* We have fallen off the end of the list */
			chkreport_i (CHK_SUBSCRIPT_RANGE, /*chklocn */ locn, n);
		tptr = NextItem (tptr);
	}
	return (ThisItem (tptr));
}

/*}}}  */
/*{{{  PRIVATE treenode *point_at_offset */
/*****************************************************************************
 *
 * point_at_offset takes a pointer to a construct 'sptr'
 *                    and an offset value and returns a pointer to that
 *                    byte offset of the construct.
 *
 *****************************************************************************/
PRIVATE treenode *point_at_offset (treenode * sptr, const BIT32 offset, const int err_msg, const SOURCEPOSN err_locn)
{
	switch (TagOf (sptr)) {
		/*{{{  CONSTCONSTRUCTOR STRING */
	case S_CONSTCONSTRUCTOR:
	case S_STRING:
		{
			/* bug TS/2093 - create it in temp workspace */
			const int old = switch_to_temp_workspace ();
			treenode *const bytenode = newconsttablenode (S_CONSTPTR, NOPOSN, NULL, NULL);
			switch_to_prev_workspace (old);

			SetCTPtr (bytenode, WNameOf (CTValOf (sptr)) + offset);
			return bytenode;
		}
		/*}}}  */
		/*{{{  CONSTPTR */
	case S_CONSTPTR:
		SetCTPtr (sptr, CTPtrOf (sptr) + offset);
		return sptr;
		/*}}}  */
	default:
#if 0
		chkreport_s (CHK_INV_CONSTRUCT, /*chklocn */ LocnOf (sptr), itagstring (TagOf (sptr)));
#else /* bug 1255 22/8/91 */
		chkreport (err_msg, /*LocnOf(sptr) */ err_locn);
#endif
	}
	return NULL;		/* Not reached */
}

/*}}}  */
/*{{{  PRIVATE treenode *point_at_subscript(sptr, type, subscript) */
/*****************************************************************************
 *
 * point_at_subscript takes a pointer to a construct 'sptr' of type 'type'
 *                    and a subscript value and returns a pointer to that
 *                    element of the construct.
 *
 *****************************************************************************/
PRIVATE treenode *point_at_subscript (treenode * const sptr, treenode * const type, const BIT32 subscript, const int err_msg, const SOURCEPOSN err_locn)
{
	switch (TagOf (sptr)) {
		/*{{{  CONSTRUCTOR */
	case S_CONSTRUCTOR:
		return (point_at_list_item (LitExpOf (sptr), subscript));
		/*}}}  */
		/*{{{  CONSTCONSTRUCTOR STRING, CONSTPTR */
	case S_CONSTCONSTRUCTOR:
	case S_STRING:
	case S_CONSTPTR:
		{
			const INT32 element_size = bytesin (type);
			return point_at_offset (sptr, subscript * element_size, err_msg, err_locn);
		}
		/*}}}  */
		/*{{{  LIST */
	case S_LIST:
		return (point_at_list_item (sptr, subscript));
		/*}}}  */
		/*{{{  ARRAYCONSTRUCTOR */
#ifdef ARRAYCONSTRUCTOR
	case S_ARRAYCONSTRUCTOR:
		{
			treenode *newtree, *index;
			static treenode *dummyexp_p = NULL;	/* yes, I _mean_ static */
			if (dummyexp_p == NULL) {
				const int old = switch_to_real_workspace ();
				dummyexp_p = newleafnode (S_DUMMYEXP, NOPOSN);
				switch_to_prev_workspace (old);
			}
			index = newconstexpnode (S_CONSTEXP, LocnOf (sptr), dummyexp_p, 0, subscript);
			index = newdopnode (S_ADD, LocnOf (sptr), copytree (ACStartExpOf (sptr), lexlevel), index, S_INT);
			marknametrans ();
			addnametrans (ACNameOf (sptr), index);
			newtree = transcopytree (ACValExpOf (sptr), 0, lexlevel);
			freenametrans ();
			return newtree;
		}
#endif
		/*}}}  */
	default:
#if 0
		chkreport_s (CHK_INV_CONSTRUCT, /*chklocn */ LocnOf (sptr), itagstring (TagOf (sptr)));
#else /* bug 1255 22/8/91 */
		chkreport (err_msg, /*LocnOf(sptr) */ err_locn);
#endif
	}
	return NULL;		/* Not reached */
}

/*}}}  */
/*{{{  PRIVATE treenode *point_at_construct (tptr, checked) */
/*****************************************************************************
 *
 * point_at_construct descends the element tree tptr, to
 *                    reach the value of the element.
 *                    If checked is FALSE, it will check that subscripts
 *                    and segment ranges are legal.
 *
 *****************************************************************************/
PRIVATE treenode *point_at_construct (treenode * tptr, const BOOL checked, const int err_msg, const SOURCEPOSN err_locn)
{
	switch (TagOf (tptr)) {
		/*{{{  case S_RECORDSUB */
#ifdef OCCAM2_5
	case S_RECORDSUB:
		{
			treenode *const sptr = point_at_construct (ASBaseOf (tptr), checked, err_msg, err_locn);
			const INT32 offset = NVOffsetOf (ASIndexOf (tptr));
			DEBUG_MSG (("point_at_construct: RECORDSUB: offset is %ld\n", offset));
			return point_at_offset (sptr, offset, err_msg, err_locn);
		}
#endif
		/*}}}  */
		/*{{{  case S_ARRAYSUB */
	case S_ARRAYSUB:
		{
			treenode *const sptr = point_at_construct (ASBaseOf (tptr), checked, err_msg, err_locn);
			/* Type of the thing we are subscripting */
			treenode *const type = chk_gettype (ASBaseOf (tptr));
			BIT32 shi, slo;

			foldconstexp (ASIndexOf (tptr), &shi, &slo, CHK_EXP_NOT_CONST, LocnOf (tptr));
			if (!checked && !current_fe_data->fe_disable_rangechecking_in_branches)
				/*{{{  check subcript range */
			{
				if (slo >= ARDimOf (type))
					chkreport_i (CHK_SUBSCRIPT_RANGE, LocnOf (tptr), slo);
			}
			/*}}}  */
			return point_at_subscript (sptr, ARTypeOf (type), slo, err_msg, err_locn);
		}
		/*}}}  */
		/*{{{  case S_SEGMENT */
	case S_SEGMENT:
		{
			treenode *sptr = point_at_construct (SNameOf (tptr), checked, err_msg, err_locn);
			/* Type of the thing we are subscripting */
			treenode *type = chk_gettype (SNameOf (tptr));
			BIT32 shi, slo, lhi, llo;

			/*{{{  fold start and length */
			foldconstexp (SStartExpOf (tptr), &shi, &slo, CHK_EXP_NOT_CONST, LocnOf (tptr));
			foldconstexp (SLengthExpOf (tptr), &lhi, &llo, CHK_EXP_NOT_CONST, LocnOf (tptr));
			/*}}}  */
			if (!checked)
				/*{{{  check start and length are legal */
			{
				INT32 i = (INT32) slo, j = (INT32) llo;
				INT32 d = ARDimOf (type);
				if (d == (-1))	/* dimension is unknown */
					d = MOSTPOS_INT32;
				if (i < 0 || i > d)
					chkreport_i (CHK_SEG_START_RANGE, /*chklocn */ LocnOf (tptr), i);
				if (j < 0)
					chkreport_i (CHK_SEG_LENGTH_RANGE, /*chklocn */ LocnOf (tptr), j);
				if (j > (d - i))
					chkreport_i (CHK_SEG_RANGE, /*chklocn */ LocnOf (tptr), i + j - 1);
			}
			/*}}}  */

			return point_at_subscript (sptr, ARTypeOf (type), slo, err_msg, err_locn);
		}
		/*}}}  */
		/*{{{  case N_VALABBR case N_VALRETYPE */
	case N_VALABBR:
	case N_VALRETYPE:
		return point_at_construct (DValOf (NDeclOf (tptr)), TRUE, err_msg, err_locn);
		/*}}}  */
		/*{{{  case S_CONSTRUCTOR */
#if 1
		/* bug TS/2093 probably wants this uncommented, but bear in mind
		   that it (probably) interferes with the bytenode stuff
		   CON - 16/02/93
		   CON - 20/05/93 - DONE.
		 */
	case S_CONSTRUCTOR:
		/* fold it and  return pointer to the folded version */
		return (point_at_construct (foldexp (tptr), checked, err_msg, err_locn));
#endif
		/*}}}  */
	default:
		return (tptr);
	}
}

/*}}}  */
/*}}}  */
/*{{{  folding constant tables                         ** */
/*{{{  PRIVATE void flip_bytes_into_array (BIT32 lo, BYTE *cptr, int len)*/
#ifdef TARGET_BIGENDIAN_BAD
/*****************************************************************************
 *
 *  flip_bytes_into_array takes a 1 or 2 byte scalar and puts it into
 *  the array cptr with opposite endianism
 *
 *****************************************************************************/
PRIVATE void flip_bytes_into_array (BIT32 lo, BYTE * cptr, int len)
{
	if (len == 1) {
		BYTE *ncptr = (BYTE *) ((long int) cptr ^ 3);
		*ncptr = lo & 0xff;
	} else {
		BYTE *locptr = (BYTE *) ((long int) cptr ^ 2);
		BYTE *hicptr = (BYTE *) (((long int) cptr + 1) ^ 2);
		*hicptr = (lo & 0xff00) >> 8;
		*locptr = lo & 0xff;
	}
	DEBUG_MSG (("flip_bytes_into_array: lo:#%lx, len:%d\n", lo, len));
}
#endif
/*}}}  */
/*{{{  PRIVATE void foldscalar_into_array (BIT32 hi, lo, BYTE *cptr, int len) */
/*****************************************************************************
 *
 *  foldscalar_into_array takes a scalar value in (hi, lo) and puts it
 *                        in the array cptr, of length len.
 *
 *****************************************************************************/
PRIVATE void foldscalar_into_array (BIT32 hi, BIT32 lo, BYTE * cptr, int len)
{
	int i;
	BIT32 v = lo;
#if TARGET_LITTLEENDIAN
	for (i = 0; i < len; i++) {
		cptr[i] = (BYTE) (v & 0xffl);
		v >>= 8;
		if (i == 3)
			v = hi;
	}
#else
	for (i = len - 1; i > 0; i--) {
		cptr[i] = (BYTE) (v & 0xffl);
		v >>= 8;
		if (i == 4)
			v = hi;
	}
#endif
	DEBUG_MSG (("foldscalar_into_array: hi:#%lx, lo:#%lx, len:%d\n", hi, lo, len));
}

/*}}}  */
/*{{{  PRIVATE void foldarray_into_scalar (BYTE *cptr, int len, BIT32 *hi, *lo) */
/*****************************************************************************
 *
 *  foldarray_into_scalar takes an array cptr, of length len, and puts it
 *                        in the scalar (*hi, *lo).
 *
 *****************************************************************************/
PRIVATE void foldarray_into_scalar (const char *cptr, const int len, BIT32 * const hi, BIT32 * const lo)
{
	BYTE ar[8];
	BYTE extendbyte;
	int i;

	for (i = 0; i < len; i++)
		ar[i] = *cptr++;

	extendbyte = (ar[len - 1] & 0x80) ? 0xff : 0;
	for ( /* i = len */ ; i < 8; i++)
		ar[i] = extendbyte;

#if TARGET_LITTLEENDIAN
	*lo = (int) ((((((ar[3] << 8) | ar[2]) << 8) | ar[1]) << 8) | ar[0]);
	*hi = (int) ((((((ar[7] << 8) | ar[6]) << 8) | ar[5]) << 8) | ar[4]);
#else
	*lo = (int) ((((((ar[4] << 8) | ar[5]) << 8) | ar[6]) << 8) | ar[7]);
	*hi = (int) ((((((ar[0] << 8) | ar[1]) << 8) | ar[2]) << 8) | ar[3]);
#endif
	DEBUG_MSG (("foldarray_into_scalar: hi:#%lx, lo:#%lx, len:%d\n", *hi, *lo, len));
}

/*}}}  */
/*{{{  forward declaration of foldconstructor */
PRIVATE BYTE *foldconstructor (treenode * tptr, BYTE * cptr, int e);
/*}}}  */
/*{{{  PRIVATE BYTE *foldconstructorelement(tptr, cptr, e)         ** */
/*****************************************************************************
 *
 * foldconstructorelement takes an element of a constructor in tptr.
 *                        If the element is an expression it is constant
 *                        folded, and stored in e bytes beginning at cptr.
 *                        If the element is a constructor, the routine
 *                        foldconstructor is called to fold each nested
 *                        element.
 *                        The updated cptr is returned.
 *
 *      Note that the byte table should be constructed for target endianism
 *
 *****************************************************************************/
PRIVATE BYTE *foldconstructorelement (treenode * tptr, BYTE * cptr, const int e)
{
	/*{{{  comment */
	/* The element could be
	   exp                    foldconstexp
	   CONSTRUCTOR            foldconstructor
	   STRING                 do it explicitly
	   SEGMENT
	   ARRAYSUB
	   In the future possible RECORDSUB
	   VAL abbrev or retype   walk through to original declaration
	   - this means we can now have CONSTCONSTRUCTORs
	   hmm... point_at_construct will point to the first element
	   then foldconstructorelement for length times.
	   See constraint on what is segmented, below.
	 */
	/*}}}  */
	switch (TagOf (tptr))
		/*{{{  cases */
	{
	default:
		/*{{{  constant fold and store */
		{
			BIT32 rhi, rlo;
			foldconstexp (tptr, &rhi, &rlo, CHK_EXP_NOT_CONST, LocnOf (tptr));
			if (swap_r64words && (chk_typeof (tptr) == S_REAL64))
				foldscalar_into_array (rlo, rhi, cptr, e);
			else
#ifdef TARGET_BIGENDIAN_BAD
				switch (e) {
				case 1:
				case 2:
					flip_bytes_into_array (rlo, cptr, e);
					break;
				default:
					foldscalar_into_array (rhi, rlo, cptr, e);
					break;
				}
#else
				foldscalar_into_array (rhi, rlo, cptr, e);
#endif
			return cptr + e;
		}
		/*}}}  */
		/*{{{  S_CONSTRUCTOR */
	case S_CONSTRUCTOR:
#ifdef ARRAYCONSTRUCTOR
	case S_ARRAYCONSTRUCTOR:
#endif
		return foldconstructor (tptr, cptr, e);
		/*}}}  */
		/*{{{  S_CONSTCONSTRUCTOR */
	case S_CONSTCONSTRUCTOR:
		{
			const char *const sptr = WNameOf (CTValOf (tptr));
			const int len = WLengthOf (CTValOf (tptr));
			memcpy (cptr, sptr, len);
			cptr += len;
			return cptr;
		}
		/*}}}  */
		/*{{{  S_STRING */
	case S_STRING:
		{
			const char *const sptr = WNameOf (CTValOf (tptr));
			const int len = WLengthOf (CTValOf (tptr));
#ifdef TARGET_BIGENDIAN_BAD
			{	/* reverse the bytes of each word in the string */

				int i;
				for (i = 0; i < len; i++) {
					flip_bytes_into_array (sptr[i], cptr, 1);
					cptr++;
				}
			}
#else
			memcpy (cptr, sptr, len);
			cptr += len;
#endif
			return cptr;
		}
		/*}}}  */
		/*{{{  case S_ARRAYSUB */
#ifdef OCCAM2_5
	case S_RECORDSUB:
#endif
	case S_ARRAYSUB:
		{
			treenode *const sptr = point_at_construct (tptr, FALSE, CHK_EXP_NOT_CONST, LocnOf (tptr));
			if (TagOf (sptr) == S_CONSTPTR) {
				int i;
				INT32 element_size;
				treenode *const type = chk_gettype (ASBaseOf (tptr));
				if (TagOf (tptr) == S_ARRAYSUB)
					element_size = bytesin (ARTypeOf (type));
				else
					element_size = bytesin (NTypeOf (ASIndexOf (tptr)));
				for (i = 0; i < element_size; i = i + e) {
					cptr = foldconstructorelement (sptr, cptr, e);
					SetCTPtr (sptr, CTPtrOf (sptr) + e);
				}
			} else
				cptr = foldconstructorelement (sptr, cptr, e);
			return (cptr);
		}
		/*}}}  */
		/*{{{  S_SEGMENT                           ** */
	case S_SEGMENT:
		/* Here I assume you can only segment a string or constant
		   constructor, ie.
		   i.  You can't expect a constant segment of a non-constant
		   constructor to be constant
		   ii. You can't segment (or subscript) a constructor before it
		   has been folded. The language definition covers this at
		   the moment.
		   **  The new Prentice-Hall version of the language allows
		   a constructor to be subscripted. */
		/* CON - 15/02/93. Hmm. Bug TS/2093 refers to this.
		   It doesn't look too simple to me.
		   CON - 20/05/83. This all now works OK
		 */
		{
			treenode *const sptr = point_at_construct (tptr, FALSE, CHK_EXP_NOT_CONST, LocnOf (tptr));
			BIT32 lhi, llo, i;
			if (TagOf (sptr) != S_CONSTPTR)
				badtag (LocnOf (tptr), TagOf (sptr), "foldconstructorelement");
			/*chkreport(CHK_SEGMENT_CONSTRUCTOR, LocnOf(tptr)); */
			foldconstexp (SLengthExpOf (tptr), &lhi, &llo, CHK_EXP_NOT_CONST, LocnOf (tptr));
			for (i = 0; i < llo; i++) {
				cptr = foldconstructorelement (sptr, cptr, e);
				SetCTPtr (sptr, CTPtrOf (sptr) + e);
			}
			return (cptr);
		}
		/*}}}  */
		/*{{{  S_CONSTPTR */
	case S_CONSTPTR:
		{
			/* *cptr++ = *CTPtrOf(tptr); */
			memcpy (cptr, CTPtrOf (tptr), e);
			cptr += e;
			return (cptr);
		}
		/*}}}  */
		/*{{{  case N_VALABBR case N_VALRETYPE */
	case N_VALABBR:
	case N_VALRETYPE:
		return foldconstructorelement (DValOf (NDeclOf (tptr)), cptr, e);
		/*}}}  */
	}
	/*}}}  */
}

/*}}}  */
/*{{{  PRIVATE void setarrayconstructortype (treenode *t, int c, treenode *eltype)*/
PRIVATE void setarrayconstructortype (treenode * t, const int c, treenode * eltype)
{
	treenode *aptr;
	/*printf ("setarrayconstructortype [%d]%s\n", c, itagstring(TagOf(eltype))); MDP */
	aptr = newtypenode (S_ARRAY, NOPOSN, newconstant (c), eltype);
	SetARDim (aptr, c);
	SetLitType (t, aptr);
}

/*}}}  */
/*{{{  PRIVATE BYTE *foldconstructor(tptr, cptr, e) */
/*****************************************************************************
 *
 * foldconstructor constant folds a constructor into a BYTE array.
 *                 tptr points to the constant constructor tree to be folded,
 *                 cptr points to the next free byte of the array,
 *                 e is the number of bytes to be occupied by each base
 *                 element of the constructor.
 *                 The updated value of cptr is returned.
 *
 *****************************************************************************/
PRIVATE BYTE *foldconstructor (treenode *tptr, BYTE *cptr, const int e)
{
#if 0
fprintf (stderr, "const1: foldconstructor(): tptr = ");
printtreenl (stderr, 4, tptr);
#endif
#ifdef ARRAYCONSTRUCTOR
	if (TagOf (tptr) == S_ARRAYCONSTRUCTOR) {
		/*{{{  array constructor */
		INT32 i;
		treenode *index = ACNameOf (tptr);
		INT32 start = eval_const_treenode (ACStartExpOf (tptr));
		INT32 end = start + eval_const_treenode (ACLengthExpOf (tptr));

		SetNReplKnown (index, TRUE);
		for (i = start; i < end; i++) {
			SetNReplValue (index, i);
			cptr = foldconstructorelement (ACValExpOf (tptr), cptr, e);
		}
		SetNReplKnown (index, FALSE);
		/*}}}  */
	} else
#endif
#ifdef OCCAM2_5
	if (chk_typeof (tptr) == S_RECORD) {
		/*{{{  fold a record constructor */
		treenode *const type = follow_user_type (LitTypeOf (tptr));
		const INT32 total_size = bytesin (type);

		/*{{{  zero the 'padding' bytes */
		memset (cptr, '\0', (size_t) total_size);
		/*}}}*/
		/*{{{  fill in each element */
		{
			treenode *decl;
			treenode *this_exp;

			for (decl = ARTypeOf (type), this_exp = LitExpOf (tptr);
			     (decl != NULL) && !EndOfList (this_exp); decl = DBodyOf (decl), this_exp = NextItem (this_exp)) {
				treenode *const field_nptr = DNameOf (decl);
				treenode *const field_type = NTypeOf (field_nptr);
				const int this_e = bytesinscalar (basetype (field_type));
				int fieldoffset = NVOffsetOf (field_nptr);
				(void) foldconstructorelement (ThisItem (this_exp), cptr + fieldoffset, this_e);
			}
		}
		/*}}}*/

		return cptr + total_size;
		/*}}}*/
	} else
#endif
	{
		treenode *t;
		int c = 0;	/* count the elements */
		for (t = LitExpOf (tptr); !EndOfList (t); t = NextItem (t)) {
			cptr = foldconstructorelement (ThisItem (t), cptr, e);
			c++;
		}
		setarrayconstructortype (tptr, c, copytree (chk_gettype_main (ThisItem (LitExpOf (tptr)), FALSE), 0));
	}
	return (cptr);
}

/*}}}  */
/*}}}  */
/*{{{  folding expression operators */
/*{{{  PRIVATE void foldmonadic (tptr, reshi, reslo, e) */
PRIVATE void foldmonadic (treenode * tptr, BIT32 * reshi, BIT32 * reslo, const int err_msg, const SOURCEPOSN err_locn)
{
	int type = MOpTypeOf (tptr);
	BOOL error = FALSE;
	BIT32 ophi, oplo;

	*reshi = 0;		/* reslo will be initialised anyway */
	foldconstexp (OpOf (tptr), &ophi, &oplo, err_msg, err_locn);
	if (type == S_INT) {
		type = (current_fe_data->fe_txlib->bpw == 2) ? S_INT16 : S_INT32;
	} else if (type == S_UINT) {
		type = (current_fe_data->fe_txlib->bpw == 2) ? S_UINT16 : S_UINT32;
	}

	switch (TagOf (tptr)) {
		/*{{{  S_NEG */
	case S_NEG:
		/*{{{  perform operation depending on type */
		switch (type) {
		case S_INT16:
			Int16Sub (&error, reslo, 0, oplo);
			break;
		case S_UINT16:
			UInt16Sub (&error, reslo, 0, oplo);
			break;
		case S_INT32:
			Int32Sub (&error, reslo, 0, oplo);
			break;
		case S_UINT32:
			UInt32Sub (&error, reslo, 0, oplo);
			break;
		case S_INT64:
			Int64Sub (&error, reshi, reslo, 0, 0, ophi, oplo);
			break;
		case S_UINT64:
			UInt64Sub (&error, reshi, reslo, 0, 0, ophi, oplo);
			break;
		case S_REAL32:
			Real32Op (&error, reslo, 0, Op_Sub + (RN << 2), oplo);
			break;
		case S_REAL64:
			Real64Op (&error, reshi, reslo, 0, 0, Op_Sub + (RN << 2), ophi, oplo);
			break;
		}
		/*}}}  */
		if (error)
			chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
		break;
		/*}}}  */
		/*{{{  S_UMINUS */
	case S_UMINUS:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Minus (reslo, 0, oplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16Minus (reslo, 0, oplo);
			break;
			/*}}}  */
			/*{{{  S_UINT16 */
		case S_UINT16:
			UInt16Minus (reslo, 0, oplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32Minus (reslo, 0, oplo);
			break;
			/*}}}  */
			/*{{{  S_UINT32 */
		case S_UINT32:
			UInt32Minus (reslo, 0, oplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64Minus (reshi, reslo, 0, 0, ophi, oplo);
			break;
			/*}}}  */
			/*{{{  S_UINT64 */
		case S_UINT64:
			UInt64Minus (reshi, reslo, 0, 0, ophi, oplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_BITNOT */
	case S_BITNOT:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8BitNot (reslo, oplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16BitNot (reslo, oplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32BitNot (reslo, oplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64BitNot (reshi, reslo, ophi, oplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_NOT */
	case S_NOT:
		if (oplo == TRUE)
			*reslo = FALSE;
		else
			*reslo = TRUE;
		break;
		/*}}}  */
	}
}

/*}}}  */
/*{{{  PRIVATE void folddyadic (tptr, reshi, reslo, e) */
PRIVATE void folddyadic (treenode * tptr, BIT32 * reshi, BIT32 * reslo, const int err_msg, const SOURCEPOSN err_locn)
{
	int type = DOpTypeOf (tptr);
	BOOL error = FALSE;
	BIT32 leftophi, leftoplo, rightophi, rightoplo;

	*reshi = 0;		/* reslo will be initialised anyway */
	foldconstexp (LeftOpOf (tptr), &leftophi, &leftoplo, err_msg, err_locn);
	foldconstexp (RightOpOf (tptr), &rightophi, &rightoplo, err_msg, err_locn);

	if (type == S_INT)
		type = current_fe_data->fe_txlib->bpw == 2 ? S_INT16 : S_INT32;

	switch (TagOf (tptr)) {
		/*{{{  operator */
		/*{{{  S_AND */
	case S_AND:
		*reslo = (leftoplo == 1) && (rightoplo == 1);
		break;
		/*}}}  */
		/*{{{  S_OR */
	case S_OR:
		*reslo = (leftoplo == 1) || (rightoplo == 1);
		break;
		/*}}}  */
		/*{{{  S_ADD */
	case S_ADD:
		/*{{{  perform operation depending on type */
		switch (type) {
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Add (&error, reslo, leftoplo, rightoplo);
			break;
#endif
		case S_INT16:
			Int16Add (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT32:
			Int32Add (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT64:
			Int64Add (&error, reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
		case S_REAL32:
			Real32Op (&error, reslo, leftoplo, Op_Add + (RN << 2), rightoplo);
			break;
		case S_REAL64:
			Real64Op (&error, reshi, reslo, leftophi, leftoplo, Op_Add + (RN << 2), rightophi, rightoplo);
			break;
		}
		/*}}}  */
		if (error)
			chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
		break;
		/*}}}  */
		/*{{{  S_CSUB0, S_CCNT1 */
		/* This can only occur when the backend is trying to fold something */
	case S_CSUB0:
	case S_CCNT1:
		{
			/* csub0 checks that 0 <= leftop <  rightop
			   ccnt1 checks that 0 <  leftop <= rightop
			   Both return leftop.
			 */
			int toobig, toosmall, equal, zero;

			if (type != S_INT64) {
				/*I32ToI64 (&leftophi, &leftoplo, leftoplo);
				   I32ToI64 (&rightophi, &rightoplo, rightoplo); */
				leftophi = 0;	/* do an UNSIGNED conversion to 64 bits */
				rightophi = 0;	/* ditto */
			}
			Int64Gt (&toobig, leftophi, leftoplo, rightophi, rightoplo);
			Int64Eq (&equal, leftophi, leftoplo, rightophi, rightoplo);
			Int64Gt (&toosmall, 0, 0, leftophi, leftoplo);
			Int64Eq (&zero, 0, 0, leftophi, leftoplo);
			if (toobig || toosmall || ((TagOf (tptr) == S_CSUB0) ? equal : zero))
				chkreport (CHK_SUBSCRIPT_OUT_OF_RANGE, LocnOf (tptr));
			else {
				*reslo = leftoplo;
				*reshi = leftophi;
			}
		}
		break;
		/*}}}  */
		/*{{{  S_SUBTRACT */
	case S_SUBTRACT:
		/*{{{  perform operation depending on type */
		switch (type) {
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Sub (&error, reslo, leftoplo, rightoplo);
			break;
#endif
		case S_INT16:
			Int16Sub (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT32:
			Int32Sub (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT64:
			Int64Sub (&error, reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
		case S_REAL32:
			Real32Op (&error, reslo, leftoplo, Op_Sub + (RN << 2), rightoplo);
			break;
		case S_REAL64:
			Real64Op (&error, reshi, reslo, leftophi, leftoplo, Op_Sub + (RN << 2), rightophi, rightoplo);
			break;
		}
		/*}}}  */
		if (error)
			chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
		break;
		/*}}}  */
		/*{{{  S_MULT */
	case S_MULT:
		/*{{{  perform operation depending on type */
		switch (type) {
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Mul (&error, reslo, leftoplo, rightoplo);
			break;
#endif
		case S_INT16:
			Int16Mul (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT32:
			Int32Mul (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT64:
			Int64Mul (&error, reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
		case S_REAL32:
			Real32Op (&error, reslo, leftoplo, Op_Mul + (RN << 2), rightoplo);
			break;
		case S_REAL64:
			Real64Op (&error, reshi, reslo, leftophi, leftoplo, Op_Mul + (RN << 2), rightophi, rightoplo);
			break;
		}
		/*}}}  */
		if (error)
			chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
		break;
		/*}}}  */
		/*{{{  S_DIV */
	case S_DIV:
		/*{{{  perform operation depending on type */
		switch (type) {
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Div (&error, reslo, leftoplo, rightoplo);
			break;
#endif
		case S_INT16:
			Int16Div (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT32:
			Int32Div (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT64:
			Int64Div (&error, reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
		case S_REAL32:
			Real32Op (&error, reslo, leftoplo, Op_Div + (RN << 2), rightoplo);
			break;
		case S_REAL64:
			Real64Op (&error, reshi, reslo, leftophi, leftoplo, Op_Div + (RN << 2), rightophi, rightoplo);
			break;
		}
		/*}}}  */
		if (error)
			chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
		break;
		/*}}}  */
		/*{{{  S_REM */
	case S_REM:
		/*{{{  perform operation depending on type */
		switch (type) {
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Rem (&error, reslo, leftoplo, rightoplo);
			break;
#endif
		case S_INT16:
			Int16Rem (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT32:
			Int32Rem (&error, reslo, leftoplo, rightoplo);
			break;
		case S_INT64:
			Int64Rem (&error, reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
		case S_REAL32:
			Real32Rem (&error, reslo, leftoplo, rightoplo);
			break;
		case S_REAL64:
			Real64Rem (&error, reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
		}
		/*}}}  */
		if (error)
			chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
		break;
		/*}}}  */
		/*{{{  S_BITAND */
	case S_BITAND:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8BitAnd (reslo, leftoplo, rightoplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16BitAnd (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32BitAnd (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64BitAnd (reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_BITOR */
	case S_BITOR:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8BitOr (reslo, leftoplo, rightoplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16BitOr (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32BitOr (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64BitOr (reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_XOR */
	case S_XOR:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Xor (reslo, leftoplo, rightoplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16Xor (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32Xor (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64Xor (reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_LSHIFT */
	case S_LSHIFT:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8LShift (reslo, leftoplo, rightoplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16LShift (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32LShift (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64LShift (reshi, reslo, leftophi, leftoplo, rightoplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_RSHIFT */
	case S_RSHIFT:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8RShift (reslo, leftoplo, rightoplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16RShift (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32RShift (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64RShift (reshi, reslo, leftophi, leftoplo, rightoplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_PLUS */
	case S_PLUS:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Plus (reslo, leftoplo, rightoplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16Plus (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32Plus (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64Plus (reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_MINUS */
	case S_MINUS:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Minus (reslo, leftoplo, rightoplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16Minus (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32Minus (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64Minus (reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_TIMES */
	case S_TIMES:
		switch (type) {
			/*{{{  S_BYTE */
#ifdef OCCAM2_5
		case S_BYTE:
			UInt8Times (reslo, leftoplo, rightoplo);
			break;
#endif
			/*}}}  */
			/*{{{  S_INT16 */
		case S_INT16:
			Int16Times (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT32 */
		case S_INT32:
			Int32Times (reslo, leftoplo, rightoplo);
			break;
			/*}}}  */
			/*{{{  S_INT64 */
		case S_INT64:
			Int64Times (reshi, reslo, leftophi, leftoplo, rightophi, rightoplo);
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  S_EQ S_NE */
	case S_EQ:
	case S_NE:
		{
			BOOL res;
			switch (type) {
				/*{{{  S_BOOL */
			case S_BOOL:
				res = (leftoplo == rightoplo);
				break;
				/*}}}  */
				/*{{{  S_BYTE */
			case S_BYTE:
				/*Int8Eq (&res, leftoplo, rightoplo); */
				UInt8Eq (&res, leftoplo, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_INT16 */
			case S_INT16:
				Int16Eq (&res, leftoplo, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_INT32 */
			case S_INT32:
				Int32Eq (&res, leftoplo, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_INT64 */
			case S_INT64:
				Int64Eq (&res, leftophi, leftoplo, rightophi, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_REAL32 */
			case S_REAL32:
				Real32Eq (&res, leftoplo, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_REAL64 */
			case S_REAL64:
				Real64Eq (&res, leftophi, leftoplo, rightophi, rightoplo);
				break;
				/*}}}  */
			}
			if (TagOf (tptr) == S_NE)
				/*{{{  invert result */
				res = !res;
			/*}}}  */
			*reslo = res;
		}
		break;
		/*}}}  */
		/*{{{  S_GR S_LE S_GE S_LS */
	case S_GR:
	case S_LE:
	case S_GE:
	case S_LS:
		{
			BOOL res;
			if ((TagOf (tptr) == S_GE) || (TagOf (tptr) == S_LS))
				/*{{{  swap the operands */
			{
				BIT32 temp;
				temp = leftoplo;
				leftoplo = rightoplo;
				rightoplo = temp;
				temp = leftophi;
				leftophi = rightophi;
				rightophi = temp;
			}
			/*}}}  */
			switch (type)
				/*{{{  types */
			{
				/*{{{  COMMENT S_BOOL */
	      /**********************  Start comment out ****************************
	      |*{{{  S_BOOL*|
	      |*
	      case S_BOOL:
		*reslo = ((leftoplo == TRUE) && (rightoplo == FALSE)) ? TRUE : FALSE;
		break;
	      *|
	      |*}}}*|
	       **********************   End comment out  ****************************/
				/*}}}  */
				/*{{{  S_BYTE */
			case S_BYTE:
				UInt8Gt (&res, leftoplo, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_INT16 */
			case S_INT16:
				Int16Gt (&res, leftoplo, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_INT32 */
			case S_INT32:
				Int32Gt (&res, leftoplo, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_INT64 */
			case S_INT64:
				Int64Gt (&res, leftophi, leftoplo, rightophi, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_REAL32 */
			case S_REAL32:
				Real32Gt (&res, leftoplo, rightoplo);
				break;
				/*}}}  */
				/*{{{  S_REAL64 */
			case S_REAL64:
				Real64Gt (&res, leftophi, leftoplo, rightophi, rightoplo);
				break;
				/*}}}  */
			}
			/*}}}  */
			if ((TagOf (tptr) == S_LE) || (TagOf (tptr) == S_GE))
				/*{{{  invert result */
				res = !res;
			/*}}}  */
			*reslo = res;
		}
		break;
		/*}}}  */
		/*{{{  S_AFTER */
	case S_AFTER:
		{
			BOOL res;
			switch (type) {
				/*{{{  S_BYTE */
#ifdef OCCAM2_5
			case S_BYTE:
				{
					/* x AFTER y === (signed)(x MINUS y) > 0                  */
					/*           === ((x MINUS y) > 0) OR ((x MINUS y) < 128) */
					/*           === ((x MINUS y) MINUS 1) < 127              */
					/*           === (x MINUS (y PLUS 1)) < 127               */
					BIT32 r;
					UInt8Minus (&r, leftoplo, rightoplo);
					UInt8Minus (&r, r, 1);
					UInt8Gt (&res, 127, r);
				}
				break;
#endif
				/*}}}  */
				/*{{{  S_INT16 */
			case S_INT16:
				{
					BIT32 r;
					Int16Minus (&r, leftoplo, rightoplo);
					Int16Gt (&res, r, 0);
				}
				break;
				/*}}}  */
				/*{{{  S_INT32 */
			case S_INT32:
				{
					BIT32 r;
					Int32Minus (&r, leftoplo, rightoplo);
					Int32Gt (&res, r, 0);
				}
				break;
				/*}}}  */
				/*{{{  S_INT64 */
			case S_INT64:
				{
					BIT32 rhi, rlo;
					Int64Minus (&rhi, &rlo, leftophi, leftoplo, rightophi, rightoplo);
					Int64Gt (&res, rhi, rlo, 0, 0);
				}
				break;
				/*}}}  */
			}
			*reslo = res;
		}
		break;
		/*}}}  */
		/*}}}  */
	}
}

/*}}}  */
/*}}}  */
/*{{{  PRIVATE treenode *foldconstarrayexp */
/*****************************************************************************
 *
 *  foldconstarrayexp folds expression 'tptr' into a constant array node
 *
 *****************************************************************************/
PRIVATE wordnode *foldconstarrayexp (treenode * tptr)
{
	switch (TagOf (tptr)) {
		/*{{{  N_VALABBR N_VALRETYPE */
	case N_VALABBR:
	case N_VALRETYPE:
		{
			treenode *const v = DValOf (NDeclOf (tptr));
			if (TagOf (v) != S_CONSTCONSTRUCTOR && TagOf (v) != S_STRING) {
				badtag (LocnOf (tptr), TagOf (v), "foldcontarrayexp");
			}
			return CTValOf (v);
		}
		/*}}}  */
		/*{{{  S_ARRAYSUB S_SEGMENT */
	case S_ARRAYSUB:
	case S_SEGMENT:
		{
			const BOOL arraysub = (TagOf (tptr) == S_ARRAYSUB);
			treenode *const base = arraysub ? ASBaseOf (tptr) : SNameOf (tptr);
			treenode *const base_type = chk_gettype (base);
			const BIT32 bytesin_base_type = bytesin (ARTypeOf (base_type));
			const char *const baseimage = WNameOf (foldconstarrayexp (base));
			treenode *const startexp = arraysub ? ASIndexOf (tptr) : SStartExpOf (tptr);
			BIT32 start, count, temp;
			foldconstexp (startexp, &temp, &start, CHK_EXP_NOT_CONST, LocnOf (startexp));
			if (arraysub)
				count = 1;
			else {
				foldconstexp (SLengthExpOf (tptr), &temp, &count, CHK_EXP_NOT_CONST, LocnOf (tptr));
				/*{{{  check start and length are legal */
				{
					const INT32 i = start;
					const INT32 j = count;
					INT32 d = ARDimOf (base_type);
					if (d == (-1))	/* if dimension is unknown */
						d = MOSTPOS_INT32;
					if (i < 0 || i > d)
						chkreport_i (CHK_SEG_START_RANGE, /*chklocn */ LocnOf (tptr), i);
					if (j < 0)
						chkreport_i (CHK_SEG_LENGTH_RANGE, /*chklocn */ LocnOf (tptr), j);
					if (j > (d - i))
						chkreport_i (CHK_SEG_RANGE, /*chklocn */ LocnOf (tptr), i + j - 1);
				}
				/*}}}  */
			}
			start *= bytesin_base_type;
			count *= bytesin_base_type;
			return lookupword (baseimage + start, (int) count);	/* 8/1/91 reclaim mem */
		}
		/*}}}  */
		/*{{{  S_RECORDSUB */
#ifdef OCCAM2_5
	case S_RECORDSUB:
		{
			treenode *const base = ASBaseOf (tptr);
			const char *const baseimage = WNameOf (foldconstarrayexp (base));
			const BIT32 offset = NVOffsetOf (ASIndexOf (tptr));
			const BIT32 len = bytesin (NTypeOf (ASIndexOf (tptr)));
			DEBUG_MSG (("foldconstarrayexp: RECORDSUB: offset is %ld, len is %ld\n", offset, len));
			return lookupword (baseimage + offset, (int) len);
		}
#endif
		/*}}}  */
		/*{{{  S_CONSTRUCTOR */
	case S_CONSTRUCTOR:
#ifdef ARRAYCONSTRUCTOR
	case S_ARRAYCONSTRUCTOR:
#endif
		{
			treenode *type = chk_gettype (tptr);
			const INT32 len = bytesin (type);
			const int e = bytesinscalar (basetype (type));
			BYTE *const cptr = (BYTE *) memalloc ((size_t) (len + 1));
			(void) foldconstructor (tptr, cptr, e);
#if 0
			return newwordnode (S_NAME, (char *) cptr, (int) len, NULL);
#else
			{
				wordnode *const result = lookupword ((const char *) cptr, (int) len);
				memfree (cptr);	/* added 8/1/91 for regaining memory */
				return result;
			}
#endif
		}
		/*}}}  */
		/*{{{  S_CONSTCONSTRUCTOR S_STRING */
	case S_CONSTCONSTRUCTOR:
	case S_STRING:
		return CTValOf (tptr);
		/*}}}  */
	default:
		chkreport (CHK_EXP_NOT_CONST, /*chklocn */ LocnOf (tptr));
	}
	return (NULL);		/* Not reached */
}

/*}}}  */
/*{{{  PRIVATE treenode *subscripttype(tptr) */
/*****************************************************************************
 *
 * subscripttype returns the type of the underlying name in a
 *               subscript expression
 *
 *****************************************************************************/
PRIVATE treenode *subscripttype (treenode * tptr)
{
#ifdef OCCAM2_5
	if (TagOf (tptr) == S_RECORDSUB)
		return NTypeOf (ASIndexOf (tptr));
#endif
	tptr = nameof (tptr);
	switch (TagOf (tptr)) {
		/*{{{  name */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_DECL:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_REPL:
		return (NTypeOf (tptr));
		/*}}}  */
		/*{{{  constructor string */
	case S_CONSTRUCTOR:
	case S_STRING:
	case S_CONSTCONSTRUCTOR:	/* bug TS/1618 20/05/92 */
#ifdef ARRAYCONSTRUCTOR
	case S_ARRAYCONSTRUCTOR:
#endif
		return (chk_gettype (tptr));
		/*}}}  */
	default:
		badtag (LocnOf (tptr), TagOf (tptr), "subscripttype");
		return NULL;	/* NOTREACHED */
	}
}

/*}}}  */
/*{{{  PUBLIC void foldconstexp (tptr, reshi, reslo, e) */
PUBLIC void foldconstexp (treenode * tptr, BIT32 * reshi, BIT32 * reslo, const int err_msg, const SOURCEPOSN err_locn)
{
	BOOL error = FALSE;
	*reshi = 0;		/* reslo will be initialised anyway */
	switch (TagOf (tptr)) {
		/*{{{  tags */
		/*{{{  dyadic operator */
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
		folddyadic (tptr, reshi, reslo, err_msg, err_locn);
		break;
#ifdef OCCAM2_5
	case S_OFFSETOF:
		if (TagOf (RightOpOf (tptr)) == N_FIELD)
			*reslo = NVOffsetOf (RightOpOf (tptr));
		break;
#endif
		/*}}}  */
		/*{{{  monadic operator */
	case S_NEG:
	case S_UMINUS:
	case S_BITNOT:
	case S_NOT:
		foldmonadic (tptr, reshi, reslo, err_msg, err_locn);
		break;
		/*}}}  */
		/*{{{  case S_SIZE S_ELSIZE, S_BYTESIN */
	case S_SIZE:
	case S_ELSIZE:
		{
			treenode *t;
#ifdef OCCAM2_5
			t = follow_user_type (OpOf (tptr));
			if (!istypetag (TagOf (t)))
				t = chk_gettype (OpOf (tptr));
#else
			t = chk_gettype (OpOf (tptr));
#endif
			if (TagOf (t) == S_ARRAY) {
				*reslo = ARDimOf (t);
				if (*reslo == (-1))
					/* Array size is unknown, so not constant */
					chkreport (err_msg, err_locn);
			} else if (TagOf (t) == S_UNDECLARED)
				chkreport (err_msg, err_locn);
#if 0				/* INSdi03110 */
			else
				/* Should NEVER be here because it is already type-checked */
				badtag (LocnOf (tptr), TagOf (t), "fold-SIZE");
#endif
			break;
		}
#ifdef OCCAM2_5
	case S_BYTESIN:
		{
			INT32 bytes;
			treenode *t = follow_user_type (OpOf (tptr));
			if (!istypetag (TagOf (t)))
				t = chk_gettype (OpOf (tptr));
			bytes = bytesin (t);
			if (bytes >= 0)
				*reslo = bytes;
			break;
		}
#endif
		/*}}}  */
		/*{{{  case S_TRUE */
	case S_TRUE:
		*reslo = 1;
		break;
		/*}}}  */
		/*{{{  case S_FALSE */
	case S_FALSE:
		*reslo = 0;
		break;
		/*}}}  */
		/*{{{  case S_UINTLIT, S_UBYTELIT */
	case S_UINTLIT:
	case S_UBYTELIT:
		{
			int littype = TagOf (follow_user_type (LitTypeOf (tptr)));
			/*{{{  if INT, littype = target int literal size */
			if (littype == S_INT)
				switch (current_fe_data->fe_txlib->bpw) {
				default:	/* case 2: */
					littype = S_INT16;
					break;
				case 4:
					littype = S_INT32;
					break;
/*case 8: littype = S_INT64; break; *//* not supported */
				}
			/*}}}  */
			IStrToInt (littype, &error, reshi, reslo, WNameOf (StringPtrOf (tptr)));
			if (error) {
#if 0
fprintf (stderr, "generror!: littype: %d, error: %d, reslo: %ld, reshi: %ld, string: %s\n", littype, error, *reslo, *reshi, WNameOf(StringPtrOf(tptr)));
#endif
				chkreport (CHK_CFOLD_OFLOW, LocnOf (tptr));
			}
			break;
		}
		/*{{{  old literal stuff */
#if 0
	case S_BYTELIT:
	case S_INT16LIT:
	case S_INT32LIT:
	case S_INT64LIT:
		IStrToInt (TagOf (tptr), &error, reshi, reslo, WNameOf (StringPtrOf (tptr)));
		if (error)
			chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
		break;
#endif
		/*}}}  */
		/*}}}  */
		/*{{{  case S_UREALLIT */
	case S_UREALLIT:
		{
			const int littype = TagOf (follow_user_type (LitTypeOf (tptr)));
			if (littype == S_REAL32)
				StrToR32 (&error, reslo, WNameOf (StringPtrOf (tptr)));
			else
				StrToR64 (&error, reshi, reslo, WNameOf (StringPtrOf (tptr)));
			if (error)
				chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
			break;
		}
#if 0
		/*{{{  case S_REAL32LIT */
	case S_REAL32LIT:
		StrToR32 (&error, reslo, WNameOf (StringPtrOf (tptr)));
		if (error)
			chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
		break;
		/*}}}  */
		/*{{{  case S_REAL64LIT */
	case S_REAL64LIT:
		StrToR64 (&error, reshi, reslo, WNameOf (StringPtrOf (tptr)));
		if (error)
			chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
		break;
		/*}}}  */
#endif
		/*}}}  */
		/*{{{  case S_ASMNAME */
	case S_ASMNAME:
		{
			const int i = which_asmname (tptr);
			switch (asmvalids[i]) {
			case ASMNAME_VALID:	/* Now value is known */
				*reslo = asmvalues[i];
				break;
			default:
				/* chkreport (err_msg, chklocn LocnOf(tptr) err_locn); */
				chkreport (err_msg, err_locn);
			}
		}
		break;
		/*}}}  */
		/*{{{  case N_VALABBR N_VALRETYPE */
	case N_VALABBR:
	case N_VALRETYPE:
		foldconstexp (DValOf (NDeclOf (tptr)), reshi, reslo, err_msg, err_locn);
		break;
		/*}}}  */
		/*{{{  case N_TAGDEF */
	case N_TAGDEF:
		*reshi = 0;
		*reslo = NTValueOf (tptr);
		break;
		/*}}}  */
		/*{{{  case S_MOSTPOS */
	case S_MOSTPOS:
		{
			int tsize = MOpTypeOf (tptr);
			if (tsize == S_INT)
				tsize = current_fe_data->fe_txlib->bpw == 2 ? S_INT16 : S_INT32;
			switch (tsize) {
				/*{{{  short */
#ifdef OCCAM2_5
			case S_BYTE:
				*reslo = 255;
				break;
#endif
			case S_INT16:
				*reslo = MOSTPOS_INT16;
				break;
			case S_INT32:
				*reslo = MOSTPOS_INT32;
				break;
				/*}}}  */
				/*{{{  long */
			case S_INT64:
				*reshi = MOSTPOS_INT32;
				*reslo = 0xffffffffl;
				break;
				/*}}}  */
#if 0				/* INSdi03110 */
				/* Type checked, so we cannot fall off, theoretically */
			default:
				badtag (LocnOf (tptr), (BIT32) tsize, "fold-mostpos");
#endif
			}
			break;
		}
		/*}}}  */
		/*{{{  case S_MOSTNEG */
	case S_MOSTNEG:
		{
			int tsize = MOpTypeOf (tptr);
			if (tsize == S_INT)
				tsize = current_fe_data->fe_txlib->bpw == 2 ? S_INT16 : S_INT32;
			switch (tsize) {
				/*{{{  short */
#ifdef OCCAM2_5
			case S_BYTE:
				*reslo = 0;
				break;
#endif
			case S_INT16:
				*reslo = MOSTNEG_INT16;
				break;
			case S_INT32:
				*reslo = MOSTNEG_INT32;
				break;
				/*}}}  */
				/*{{{  long */
			case S_INT64:
				*reshi = MOSTNEG_INT32;
				*reslo = 0;
				break;
				/*}}}  */
#if 0				/* INSdi03110 */
				/* Type checked, so we cannot fall off, theoretically */
			default:
				badtag ( /*chklocn */ LocnOf (tptr), (BIT32) tsize, "fold-mostneg");
#endif
			}
			break;
		}
		/*}}}  */
		/*{{{  conversions */
		/*{{{  case S_EXACT */
	case S_EXACT:
		{
			const int sourcetype = chk_typeof (OpOf (tptr));
			const int desttype = MOpTypeOf (tptr);
			BIT32 rhi, rlo;
			foldconstexp (OpOf (tptr), &rhi, &rlo, err_msg, err_locn);
			exactconversion (sourcetype, desttype, reshi, reslo, rhi, rlo, LocnOf (tptr));
		}
		break;
		/*}}}  */
		/*{{{  case S_ROUND S_TRUNC */
	case S_ROUND:
	case S_TRUNC:
		{
			int sourcetype = chk_typeof (OpOf (tptr));
			int desttype = MOpTypeOf (tptr);
			BIT32 rhi, rlo;
			const int roundmode = (TagOf (tptr) == S_ROUND) ? Nearest : Truncate;
			foldconstexp (OpOf (tptr), &rhi, &rlo, err_msg, err_locn);

			/*{{{  convert from sourcetype to desttype */
			if (sourcetype == S_INT)
				sourcetype = current_fe_data->fe_txlib->bpw == 2 ? S_INT16 : S_INT32;
			if (desttype == S_INT)
				desttype = current_fe_data->fe_txlib->bpw == 2 ? S_INT16 : S_INT32;
			switch (desttype) {
#ifdef OCCAM2_5
				/*{{{  case S_BYTE */
			case S_BYTE:
				{
					BIT32 n;
					if (sourcetype == S_REAL32)
						R32ToI32 (&error, &n, roundmode, rlo);
					else
						R64ToI32 (&error, &n, roundmode, rhi, rlo);
					if (error)
						chkreport (CHK_CFOLD_OFLOW, LocnOf (tptr));
					exactconversion (S_INT32, S_BYTE, reshi, reslo, 0, n, LocnOf (tptr));
				}
				break;
				/*}}}  */
#endif
				/*{{{  case S_INT16 */
			case S_INT16:
				if (sourcetype == S_REAL32) {
					BIT32 n;
					R32ToI32 (&error, &n, roundmode, rlo);
					if (error)
						chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
					exactconversion (S_INT32, S_INT16, reshi, reslo, 0, n, LocnOf (tptr));
				} else if (sourcetype == S_REAL64) {
					BIT32 n;
					R64ToI32 (&error, &n, roundmode, rhi, rlo);
					if (error)
						chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
					exactconversion (S_INT32, S_INT16, reshi, reslo, 0, n, LocnOf (tptr));
				}
#if 0				/* INSdi03110 */
				else
					badtag ( /*chklocn */ LocnOf (tptr), (BIT32) sourcetype, "fold-convert");
#endif
				break;
				/*}}}  */
				/*{{{  case S_INT32 */
			case S_INT32:
				if (sourcetype == S_REAL32)
					R32ToI32 (&error, reslo, roundmode, rlo);
				else if (sourcetype == S_REAL64)
					R64ToI32 (&error, reslo, roundmode, rhi, rlo);
#if 0				/* INSdi03110 */
				else
					badtag ( /*chklocn */ LocnOf (tptr), (BIT32) sourcetype, "fold-convert");
#endif
				break;
				/*}}}  */
				/*{{{  case S_INT64 */
			case S_INT64:
				if (sourcetype == S_REAL32)
					R32ToI64 (&error, reshi, reslo, roundmode, rlo);
				else if (sourcetype == S_REAL64)
					R64ToI64 (&error, reshi, reslo, roundmode, rhi, rlo);
#if 0				/* INSdi03110 */
				else
					badtag ( /*chklocn */ LocnOf (tptr), (BIT32) sourcetype, "fold-convert");
#endif
				break;
				/*}}}  */
				/*{{{  case S_REAL32 */
			case S_REAL32:
				switch (sourcetype) {
#ifdef OCCAM2_5
				case S_BYTE:
#endif
				case S_INT16:
				case S_INT32:
					I32ToR32 (reslo, roundmode, rlo);
					break;
				case S_INT64:
					I64ToR32 (reslo, roundmode, rhi, rlo);
					break;
				case S_REAL32:
					*reslo = rlo;
					break;
				case S_REAL64:
					R64ToR32 (&error, reslo, roundmode, rhi, rlo);
					break;
#if 0				/* INSdi03110 */
				default:
					badtag ( /*chklocn */ LocnOf (tptr), (BIT32) sourcetype, "fold-convert");
					break;
#endif
				}
				break;
				/*}}}  */
				/*{{{  case S_REAL64 */
			case S_REAL64:
				switch (sourcetype) {
#ifdef OCCAM2_5
				case S_BYTE:
#endif
				case S_INT16:
				case S_INT32:
					I32ToR64 (reshi, reslo, rlo);
					break;
				case S_INT64:
					I64ToR64 (reshi, reslo, roundmode, rhi, rlo);
					break;
				case S_REAL32:
					R32ToR64 (&error, reshi, reslo, rlo);
					break;
				case S_REAL64:
					*reshi = rhi;
					*reslo = rlo;
					break;
#if 0				/* INSdi03110 */
				default:
					badtag ( /*chklocn */ LocnOf (tptr), (BIT32) sourcetype, "fold-convert");
					break;
#endif
				}
				break;
				/*}}}  */
#if 0				/* INSdi03110 */
			default:
				badtag ( /*chklocn */ LocnOf (tptr), (BIT32) desttype, "fold-convert");
				break;
#endif
			}
			/*}}}  */

			if (error)
				chkreport (CHK_CFOLD_OFLOW, /*chklocn */ LocnOf (tptr));
			break;
		}
		/*}}}  */
		/*}}}  */
		/*{{{  case S_ARRAYSUB / S_RECORDSUB */
	case S_ARRAYSUB:
#ifdef OCCAM2_5
	case S_RECORDSUB:
#endif
		{
			treenode *const cptr = point_at_construct (tptr, FALSE, err_msg, err_locn);
			DEBUG_MSG (("foldconstexp: got %s from point_at_construct\n", itagstring (TagOf (cptr))));
			if (TagOf (cptr) != S_CONSTPTR)
				foldconstexp (cptr, reshi, reslo, err_msg, err_locn);
			else
				/*{{{  pick value out of BYTE array */
			{
				treenode *const type = subscripttype (tptr);
				const BYTE *p = (const BYTE *) CTPtrOf (cptr);
				const int element_type = basetype (type);
				const int b = bytesinscalar (element_type);
				int e = (int) min_INT32 (4, b) * 8;
				int shift;
				BIT32 lo = 0;
				BIT32 hi = 0;
				for (shift = 0; shift < e; shift += 8) {
					lo |= ((BIT32) (*p)) << shift;
					p++;
				}
				if (b > 4) {
					e = (b - 4) * 8;
					for (shift = 0; shift < e; shift += 8) {
						hi |= (BIT32) (*p) << shift;
						p++;
					}
				} else if (b == 2) {	/* must be either INT on T2, or INT16 *//* sign extend reslo just in case *//* bug 1011 24/10/90 */
					if ((lo & 0x8000) != 0)
						lo |= 0xFFFF0000;
				}
				if (swap_r64words & (element_type == S_REAL64)) {
					*reslo = hi;
					*reshi = lo;
				} else {
					*reslo = lo;
					*reshi = hi;
				}
			}
			/*}}}  */
			break;
		}
		/*}}}  */
		/*{{{  case S_CONSTEXP  - it is already folded */
	case S_CONSTEXP:
		*reshi = HiValOf (tptr);
		*reslo = LoValOf (tptr);
		break;
		/*}}}  */
		/*{{{  case N_DECL - if it is an undeclared name */
	case N_DECL:
		if (TagOf (NTypeOf (tptr)) == S_UNDECLARED) {
			*reshi = 0;
			*reslo = 0;
		} else
#if 0
			chkreport (err_msg, /*chklocn *//*LocnOf(tptr) */ err_locn);
#endif
		chkreport (err_msg, err_locn);
		break;
		/*}}}  */
		/*{{{  case N_REPL - used by usage checker */
	case N_REPL:
		DEBUG_MSG (("foldconstexp: N_REPL: known? %d", NReplKnownOf (tptr)));
		if (NReplKnownOf (tptr))
			*reslo = NReplValueOf (tptr);
		else
#if 0
			chkreport (err_msg, /*chklocn *//*LocnOf(tptr) */ err_locn);
#endif
		chkreport (err_msg, err_locn);
		break;
		/*}}}  */
		/*{{{  case S_SEGSTART */
	case S_SEGSTART:
		foldconstexp (SStartExpOf (OpOf (tptr)), reshi, reslo, err_msg, err_locn);
		break;
		/*}}}  */
		/*{{{  case S_CONDEXP */
#ifdef CONDEXP
	case S_CONDEXP:
		{
			BIT32 ophi, oplo;
			foldconstexp (CondExpGuardOf (tptr), &ophi, &oplo, err_msg, err_locn);
			if (oplo == TRUE)
				foldconstexp (CondExpTrueOf (tptr), reshi, reslo, err_msg, err_locn);
			else
				foldconstexp (CondExpFalseOf (tptr), reshi, reslo, err_msg, err_locn);
		}
		break;
#endif
		/*}}}  */
		/*{{{  case S_EVAL */
	case S_EVAL:
		/* Won't be constant, if we used 'isconst', but will be if we used
		   'is_evaluable' in the useage checker. */
		foldconstexp (RightOpOf (tptr), reshi, reslo, err_msg, err_locn);
		break;
		/*}}}  */
		/*{{{  case S_FINSTANCE */
#ifdef OCCAM2_5
	case S_FINSTANCE:
		{
			treenode *t;

			const int old = switch_to_temp_workspace ();
			treenode *const inlined_fn = expand_inline_instance (tptr, LocnOf (tptr), 0);
			switch_to_prev_workspace (old);

			t = skipspecifications (inlined_fn);
			if ((TagOf (t) == S_VALOF) && (TagOf (VLBodyOf (t)) == S_SKIP) && !EndOfList (VLResultListOf (t))) {
				foldconstexp (ThisItem (VLResultListOf (t)), reshi, reslo, err_msg, err_locn);
			} else {
				chkreport (err_msg, err_locn);
			}
		}
		break;
#endif
		/*}}}  */
		/*}}}  */
	default:
#if 0
		chkreport (err_msg, /*chklocn *//*LocnOf(tptr) */ err_locn);
#endif
		chkreport (err_msg, err_locn);
	}
}

/*}}}  */
/*{{{  PUBLIC treenode *newconstexp (tptr)*/
PUBLIC treenode *newconstexp (treenode *tptr)
{
	treenode *r = tptr;
	const int type = chk_typeof (tptr);
	if ((type == S_ARRAY) || (type == S_RECORD)) {
		if (TagOf (tptr) != S_STRING && TagOf (tptr) != S_CONSTCONSTRUCTOR) {
			r = newconsttablenode (S_CONSTCONSTRUCTOR, LocnOf (tptr), foldconstarrayexp (tptr), tptr);
			switch TagOf (tptr) {
			case S_CONSTRUCTOR:
				/*printf ("newconsttable %s\n", itagstring(TagOf(LitTypeOf(tptr))));MDP */
				SetConstTableType (r, LitTypeOf (tptr));
				break;
			case S_SEGMENT:
				{
					treenode *aptr;

					/*printf ("newconsttable SEGMENT\n"); MDP */
					aptr = copytree (chk_gettype_main (SNameOf (tptr), FALSE), 0);
					SetARDim (aptr, -1);
					SetConstTableType (r, aptr);
				}
				break;
			default:
				/*printf ("newconsttable (untyped) %s\n", itagstring(TagOf(tptr)));MDP */
				break;
			}
		}
	} else {
		BIT32 rhi, rlo;
		if (TagOf (tptr) != S_CONSTEXP) {
			foldconstexp (tptr, &rhi, &rlo, CHK_EXP_NOT_CONST, LocnOf (tptr));
			/* moved to coder so gen modules find Hi and Lo in the right places
			   if (swap_r64words && (chk_typeof(tptr) == S_REAL64))
			   r = newconstexpnode (S_CONSTEXP, LocnOf(tptr), tptr, rlo, rhi);
			   else
			 */
			r = newconstexpnode (S_CONSTEXP, LocnOf (tptr), tptr, rhi, rlo);
		}
	}
	return r;
}

/*}}}  */
/*{{{  PUBLIC BOOL wouldbeconst (tptr)  without folding */
PUBLIC BOOL wouldbeconst (treenode *tptr)
/* This returns TRUE if the tree will be constant once special names
   such as .WSSIZE have been translated */
{
	int spare_valids[ASMNAMES_COUNT];
	BOOL constant;
	int i;

	/* To allow things like .WSSIZE, we mark all the special ASM names
	   as valid, so that isconst can return TRUE */
	memcpy (spare_valids, asmvalids, sizeof (int) * ASMNAMES_COUNT);

	for (i = 0; i < ASMNAMES_COUNT; i++)
		asmvalids[i] = ASMNAME_VALID;

	constant = isconst (tptr);
	memcpy (asmvalids, spare_valids, sizeof (int) * ASMNAMES_COUNT);

	return constant;
}

/*}}}  */
/*{{{  PUBLIC BOOL isconst (tptr)  without folding */
/* Test whether the expression tree tptr is constant, return TRUE if it is,
FALSE otherwise.
*/
PUBLIC BOOL isconst (treenode * tptr)
{
	if (tptr != NULL)	/* added for bug 1120 25/1/91 */
		switch (TagOf (tptr)) {
			/*{{{  MOSTPOS MOSTNEG literal constant expression or constructor */
		case S_MOSTPOS:
		case S_MOSTNEG:
#if 0
		case S_INTLIT:
		case S_INT16LIT:
		case S_INT32LIT:
		case S_INT64LIT:
		case S_REAL32LIT:
		case S_REAL64LIT:
		case S_BYTELIT:
#else
		case S_UINTLIT:
		case S_UREALLIT:
		case S_UBYTELIT:
#endif
		case S_TRUE:
		case S_FALSE:
		case S_STRING:
		case S_CONSTEXP:
		case S_CONSTCONSTRUCTOR:
		case S_DUMMYEXP:
			/* S_NULLARRAY added 11/02/2003 (frmb) */
		case S_NULLARRAY:
			return TRUE;
			/*}}}  */
			/*{{{  ASMNAME */
		case S_ASMNAME:
			return (asmvalids[which_asmname (tptr)] == ASMNAME_VALID);
			/*}}}  */
			/*{{{  monadic operator */
		case S_NEG:
		case S_UMINUS:
		case S_BITNOT:
		case S_NOT:
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			return isconst (OpOf (tptr));
		case S_TYPEHASHOF:
			return FALSE;			/* no, but yes.. */
		case S_SIZE:
		case S_ELSIZE:	/* introduced by the backend */
#ifdef OCCAM2_5
			if (istypetag (TagOf (follow_user_type (OpOf (tptr)))))
				return TRUE;	/* all types are of known size */
#endif
			{
				treenode *const t = chk_gettype (OpOf (tptr));
				return ((TagOf (t) == S_ARRAY) && (ARDimOf (t) != (-1)));
			}
#ifdef OCCAM2_5
		case S_BYTESIN:
			if (istypetag (TagOf (follow_user_type (OpOf (tptr)))))
				return TRUE;	/* all types are of known size */
			return bytesin (chk_gettype (OpOf (tptr))) >= 0;
#endif
		case S_SEGSTART:	/* introduced by the backend */
			return isconst (SStartExpOf (OpOf (tptr)));
		case S_ADDRESSOF:
			return FALSE;
#ifdef MOBILES
		case S_ADDROF:
		case S_HWADDROF:
		case S_CLONE:
		case S_DEFINED:
		case S_NTH_DIMENSION:
			return FALSE;
#endif
			/*}}}  */
			/*{{{  dyadic operator */
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
		case S_CCNT1:	/* introduced by the back end */
		case S_EVAL:
			return (isconst (LeftOpOf (tptr)) && isconst (RightOpOf (tptr)));
#ifdef OCCAM2_5
		case S_OFFSETOF:
			return TagOf (RightOpOf (tptr)) == N_FIELD;
#endif
			/*}}}  */
#ifdef USER_DEFINED_OPERATORS
			/*{{{  extra operators for overloading */
#include "casedops.h"
#include "casemops.h"
			return FALSE;	/* Added by Jim 20/1/97 */
			/*}}}  */
#endif
			/*{{{  conditional expression */
#ifdef CONDEXP
		case S_CONDEXP:
			return (isconst (CondExpGuardOf (tptr)) && isconst (CondExpTrueOf (tptr)) && isconst (CondExpFalseOf (tptr)));
#endif
			/*}}}  */
			/*{{{  constructor */
		case S_CONSTRUCTOR:
			for (tptr = LitExpOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr))
				if (!isconst (ThisItem (tptr)))
					return FALSE;
			return TRUE;	/* fallen off the end, so it must be constant */
#if 0
		case S_STRUCTCONSTRUCTOR:
			return FALSE;
#endif
			/*}}}  */
			/*{{{  array constructor */
		case S_ARRAYCONSTRUCTOR:
			/* constant if constant start, length, step and expression */
			if (isconst (ReplCStartExpOf (tptr)) && isconst (ReplCLengthExpOf (tptr)) && (!ReplCStepExpOf (tptr) || isconst (ReplCStepExpOf (tptr))) &&
					isconst (ReplCBodyOf (tptr))) {
				return TRUE;		/* yes, constant */
			}
			return FALSE;
			/*}}}  */
			/*{{{  array subscript */
		case S_ARRAYSUB:
		case S_ARRAYITEM:
		case S_RECORDSUB:
		case S_RECORDITEM:
			return (isconst (ASIndexOf (tptr)) && isconst (ASBaseOf (tptr)));
			/*}}}  */
			/*{{{  array dimension */
		case S_ARRAY:
			return (ARDimOf (tptr) != (-1));
			/*}}}  */
			/*{{{  segment */
		case S_SEGMENT:
			return (isconst (SNameOf (tptr)) && isconst (SStartExpOf (tptr)) && isconst (SLengthExpOf (tptr)));
		case S_SEGMENTITEM:
			return FALSE;
			/*}}}  */
			/*{{{  val abbrev  val retype */
		case N_VALABBR:
		case N_VALRETYPE:
			return isconst (DValOf (NDeclOf (tptr)));
			/*}}}  */
			/*{{{  decl abbrev retype param repl specification valof function ... */
		case N_ABBR:
		case N_RETYPE:
		case N_DECL:
		case N_PARAM:
		case N_RESULTPARAM:
		case N_LABELDEF:	/* added 14/09/92 cos required by backend */
		case S_DECL:
		case S_VALABBR:
		case S_ABBR:
		case S_RETYPE:
		case S_VALRETYPE:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_COLON2:
		case S_VALOF:
		case S_FNFORMALRESULT:
		case T_TEMP:
		case T_PREEVALTEMP:
		case S_FNACTUALRESULT:
			/* required for J_CODE stuff: */
		case S_PARAM_STATICLINK:
		case S_PARAM_VSP:
		case S_PARAM_FB:
		case S_PARAM_WS:
#ifdef MOBILES
		case S_PARAM_MSP:
		case S_PARAM_MPP:
		case S_MPROCDECL:
		case N_MPROCDECL:
#endif
		case S_HIDDEN_PARAM:
		case S_PRAGMA:	/* bug 829 20/9/91 */
		case N_SFUNCDEF:
		case N_PROCDEF:
		case N_LFUNCDEF:
		case N_SCFUNCDEF:
		case N_SCPROCDEF:
		case N_LIBFUNCDEF:
		case N_LIBPROCDEF:
		case N_LIBMPROCDECL:
		case N_PREDEFFUNCTION:
		case N_PREDEFPROC:
		case N_STDLIBFUNCDEF:
		case N_STDLIBPROCDEF:
		case N_INLINEFUNCDEF:
		case N_INLINEPROCDEF:
		case N_TYPEDECL:
			return FALSE;
		case N_TAGDEF:
		case N_FIELD:
			return TRUE;
		case N_REPL:
			DEBUG_MSG (("isconst: N_REPL: known? %d", NReplKnownOf (tptr)));
			return NReplKnownOf (tptr);
		case N_VALPARAM:
#ifdef OCCAM2_5
			return checking_const_fn && (NLexLevelOf (tptr) == (const_fn_lexlevel + 1));
#else
			return FALSE;
#endif
			/*}}}  */
			/*{{{  fn instance */
		case S_FINSTANCE:
#ifdef OCCAM2_5
			if (NPConstantFnOf (INameOf (tptr))) {
				treenode *t;
				for (t = IParamListOf (tptr); !EndOfList (t); t = NextItem (t))
					if (!isconst (ThisItem (t)))
						return FALSE;
				return TRUE;
			}
#endif
			return FALSE;
			/*}}}  */
			/*{{{  overlapcheck */
		case S_OVERLAPCHECK:
			return overlaps_are_const;
			/*}}}  */
			/*{{{  things that are effectively errors (captured elsewhere)*/
		case S_ASOUTPUT:
		case S_ASINPUT:
			return FALSE;
			/*}}}*/
		default:
			badtag (LocnOf (tptr), TagOf (tptr), "isconst");
		}
	return (FALSE);
}

/*}}}  */
/*{{{  PUBLIC treenode *foldexp (tptr) */
/*
Fold constant expressions, return a pointer to the folded tree.
Does not refold previously folded trees.
*/
PUBLIC treenode *foldexp (treenode * tptr)
{
	if (tptr == NULL)
		return NULL;	/* bug 838 20/12/90 */

	switch (TagOf (tptr)) {
		/*{{{  cases */
		/*{{{  val names */
	case N_VALABBR:
	case N_VALRETYPE:
		if (DValOf (NDeclOf (tptr)) == NULL)
			break;
		/* The original declaration will already be folded. */
		/* If the name is not scalar then we do not insert another copy in the tree
		   here */
		/* This is changed so that it DOES fold arrays: 28/2/90 by CO'N */
		/* Arghh! This breaks the way debug info is generated, and causes */
		/* Duplicate copies of the arrays to exist. Take it out again */
		/* if (isconst(tptr)) */
#ifdef OCCAM2_5
		if (isconst (tptr)) {
			const int type_tag = TagOf (follow_user_type (NTypeOf (tptr)));
			if ((type_tag != S_ARRAY) && (type_tag != S_RECORD))
				tptr = newconstexp (tptr);
		}
#else
		if (isconst (tptr) && TagOf (NTypeOf (tptr)) != S_ARRAY)
			tptr = newconstexp (tptr);
#endif
		break;
		/*}}}  */
		/*{{{  literals MOSTPOS MOSTNEG */
	case S_UINTLIT:
	case S_UREALLIT:
	case S_UBYTELIT:
	case S_TRUE:
	case S_FALSE:
	case S_MOSTPOS:
	case S_MOSTNEG:
		tptr = newconstexp (tptr);
		break;
		/*}}}  */
		/*{{{  ASMNAME and other ASM bits */
	case S_ASMNAME:
		if (isconst (tptr))	/* true if has been set to 'valid' */
			tptr = newconstexp (tptr);
		break;		/* Don't yet convert it! */
	case N_PROCDEF:
	case N_LIBPROCDEF:	/* bug TS/1240 14/04/92 */
	case N_LIBMPROCDECL:
	case N_SFUNCDEF:
	case N_LFUNCDEF:
	case N_LIBFUNCDEF:
#ifdef MOBILES
	case N_MPROCDECL:
#endif
		break;
		/*}}}  */
		/*{{{  monadic operator */
	case S_ADDRESSOF:
		SetOp (tptr, foldexp (OpOf (tptr)));
		break;
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
	case S_NEG:
	case S_UMINUS:
	case S_BITNOT:
	case S_NOT:
		if (isconst (OpOf (tptr)))
			tptr = newconstexp (tptr);
		else
			SetOp (tptr, foldexp (OpOf (tptr)));
		break;
	case S_SIZE:
		{
			treenode *t;
#ifdef OCCAM2_5
			t = follow_user_type (OpOf (tptr));
			if (!istypetag (TagOf (t))) {
				t = chk_gettype (OpOf (tptr));
			}
#else
			t = chk_gettype (OpOf (tptr));
#endif
#ifdef MOBILES
			if (TagOf (t) == S_MOBILE) {
				t = MTypeOf (t);
			}
#endif
			if (TagOf (t) == S_ARRAY) {
				if (ARDimOf (t) != (-1)) {
					tptr = newconstexpnode (S_CONSTEXP, /*chklocn */ LocnOf (tptr), tptr, 0, ARDimOf (t));
				} else {
					SetOp (tptr, foldexp (OpOf (tptr)));
				}
			}
		}
		break;
#ifdef OCCAM2_5
	case S_BYTESIN:
		{
			treenode *t = follow_user_type (OpOf (tptr));
			if (istypetag (TagOf (t))) {
				tptr = newconstexpnode (S_CONSTEXP, LocnOf (tptr), tptr, 0, bytesin (t));
			} else {
				INT32 bytes;
				t = chk_gettype (OpOf (tptr));
				bytes = bytesin (t);
				if (bytes >= 0) {
					tptr = newconstexpnode (S_CONSTEXP, LocnOf (tptr), tptr, 0, bytes);
				} else {
					SetOp (tptr, foldexp (OpOf (tptr)));
				}
			}
		}
		break;
#endif
		/*}}}  */
		/*{{{  dyadic operator */
		/*{{{  S_AND S_OR */
	case S_AND:
	case S_OR:
		if (isconst (LeftOpOf (tptr)) && isconst (RightOpOf (tptr)))
			tptr = newconstexp (tptr);
		else if (isconst (LeftOpOf (tptr)))
			/*{{{  fold left, fold out   ( FALSE | TRUE )  ( AND | OR ) */
		{
			BIT32 rhi, rlo;
			foldconstexp (LeftOpOf (tptr), &rhi, &rlo, CHK_EXP_NOT_CONST, LocnOf (tptr));
			if (TagOf (tptr) == S_AND) {
				if (rlo == 0)	/* FALSE AND ... */
					return (newconstexpnode (S_CONSTEXP, /*chklocn */ LocnOf (tptr), tptr, rhi, rlo));
				else	/* TRUE  AND ... */
					return foldexp (RightOpOf (tptr));
			} else {	/* TagOf(tptr) == S_OR */

				if (rlo == 1)	/* TRUE OR ... */
					return (newconstexpnode (S_CONSTEXP, /*chklocn */ LocnOf (tptr), tptr, rhi, rlo));
				else	/* FALSE OR ... */
					return foldexp (RightOpOf (tptr));
			}
		}
		/*}}}  */
		else if (isconst (RightOpOf (tptr)))
			/*{{{  fold right, fold out '... AND FALSE' and '... OR TRUE' */
		{
			BIT32 rhi, rlo;
			foldconstexp (RightOpOf (tptr), &rhi, &rlo, CHK_EXP_NOT_CONST, LocnOf (tptr));
			if (TagOf (tptr) == S_AND) {
				if (rlo == 0)	/* ... AND FALSE */
					return newconstexpnode (S_CONSTEXP, /*chklocn */ LocnOf (tptr), tptr, rhi, rlo);
				else	/* ... AND TRUE  */
					return foldexp (LeftOpOf (tptr));
			} else {	/* TagOf(tptr) == S_OR */

				if (rlo == 1)	/* ... OR TRUE */
					return (newconstexpnode (S_CONSTEXP, /*chklocn */ LocnOf (tptr), tptr, rhi, rlo));
				else	/* ... OR FALSE */
					return foldexp (LeftOpOf (tptr));
			}
		}
		/*}}}  */
		else {
			SetLeftOp (tptr, foldexp (LeftOpOf (tptr)));
			SetRightOp (tptr, foldexp (RightOpOf (tptr)));
		}
		break;
		/*}}}  */
#include "casedops.h"		/* Added by Jim 2/2/97 */
#include "casemops.h"
		{
			return (tptr);
			break;	/* not needed */
		}
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
		{
			const BOOL rightisconst = isconst (RightOpOf (tptr));

			SetLeftOp (tptr, foldexp (LeftOpOf (tptr)));
			SetRightOp (tptr, foldexp (RightOpOf (tptr)));

			/* Moved this test infront of the 'newconstexp' bit for bug TS/1718 12/06/92 */
			if (rightisconst && ((TagOf (tptr) == S_LSHIFT) || (TagOf (tptr) == S_RSHIFT)) &&
			    /* This is an unsigned test, so it catches negative values */
			    /* We know that (HiValOf(RightOpOf(tptr)) == 0) cos type is INT */
			    (LoValOf (RightOpOf (tptr)) > (8 * (INT32) bytesinscalar (DOpTypeOf (tptr))))) {
				chkreport (CHK_INV_SHIFT, /*chklocn */ LocnOf (tptr));
			}

			if (isconst (LeftOpOf (tptr)) && rightisconst) {
				tptr = newconstexp (tptr);
			}
		}
		break;
#ifdef OCCAM2_5
	case S_OFFSETOF:
		if (TagOf (RightOpOf (tptr)) == N_FIELD)
			tptr = newconstexp (tptr);
		break;
#endif
		/*}}}  */
		/*{{{  conditional expression */
#ifdef CONDEXP
	case S_CONDEXP:
		if (isconst (CondExpGuardOf (tptr)) && isconst (CondExpTrueOf (tptr)) && isconst (CondExpFalseOf (tptr)))
			tptr = newconstexp (tptr);
		else {
			SetCondExpGuard (tptr, foldexp (CondExpGuardOf (tptr)));
			SetCondExpTrue (tptr, foldexp (CondExpTrueOf (tptr)));
			SetCondExpFalse (tptr, foldexp (CondExpFalseOf (tptr)));
		}
		break;
#endif
		/*}}}  */
		/*{{{  constructor */
	case S_CONSTRUCTOR:
		if (isconst (tptr))
			tptr = newconstexp (tptr);
		else
			(void) foldexplist (LitExpOf (tptr));
		break;
		/*}}}  */
		/*{{{  structconstructor */
#if 0
	case S_STRUCTCONSTRUCTOR:
		{
			break;
			/*
			   treenode *t = OpOf(tptr);
			   while (!EndOfList(t))
			   {
			   NewItem(foldexp(ThisItem(t)), t);
			   t = NextItem(t);
			   }
			 */
		}
#endif
		/*}}}  */
		/*{{{  arrayconstructor */
	case S_ARRAYCONSTRUCTOR:
		SetReplCStartExp (tptr, foldexp (ReplCStartExpOf (tptr)));
		SetReplCLengthExp (tptr, foldexp (ReplCLengthExpOf (tptr)));
		if (ReplCStepExpOf (tptr)) {
			SetReplCStepExp (tptr, foldexp (ReplCStepExpOf (tptr)));
		}
		SetReplCBody (tptr, foldexp (ReplCBodyOf (tptr)));
		break;
		/*}}}  */
		/*{{{  function instance */
	case S_FINSTANCE:
		{
			treenode *const fname = INameOf (tptr);

			/* special case: if it's a call to PD_PROTOCOL_HASH, first param might be non-sensible, we fold into the hash value early */
			#ifdef PD_PROTOCOL_HASH
			if ((TagOf (fname) == N_PREDEFFUNCTION) && (NModeOf (fname) == PD_PROTOCOL_HASH)) {
				treenode *params = IParamListOf (tptr);
				
				if (NextItem (params)) {
					tptr = ThisItem (NextItem (params));
				}
			} else 
			#endif
			{
				foldexplist (IParamListOf (tptr));

				if (TagOf (fname) == N_PREDEFFUNCTION) {
					switch (NModeOf (fname)) {
					case PD_ASHIFTLEFT:
					case PD_ASHIFTRIGHT:
					case PD_ROTATELEFT:
					case PD_ROTATERIGHT:
					case PD_BITREVNBITS:
						{	/* check the second parameter - places to shift */
							treenode *const param = ThisItem (NextItem (IParamListOf (tptr)));
							if (isconst (param) && (LoValOf (param) > (8 * (INT32) current_fe_data->fe_txlib->bpw)))
								chkreport (CHK_INV_SHIFT, /*chklocn */ LocnOf (tptr));
						}
						break;
					default:
						break;
					}
				}
#ifdef OCCAM2_5
				if (isconst (tptr)) {
					tptr = newconstexp (tptr);
				}
			}
#endif
		}
		break;
		/*}}}  */
		/*{{{  array subscript */
	case S_ARRAYSUB:
		{
			const BOOL rightconst = isconst (ASIndexOf (tptr));

			/* We can fold at this level if both right-hand and left-hand
			   sides are constant */
			if (rightconst && isconst (ASBaseOf (tptr)))
				tptr = newconstexp (tptr);
			else {
				if (!issimple (ASBaseOf (tptr)))
					SetASBase (tptr, foldexp (ASBaseOf (tptr)));
				SetASIndex (tptr, foldexp (ASIndexOf (tptr)));
				if (rightconst && !current_fe_data->fe_disable_rangechecking_in_branches)
				 {	/* NICK: DISABLE THIS FOR CONFIGURER */
					/*{{{  check subcript range */
					treenode *const t = chk_gettype (ASBaseOf (tptr));
					if ((TagOf (t) == S_ARRAY) &&	/* bug INSdi02065 28/04/93 */
					    (LoValOf (ASIndexOf (tptr)) >= ARDimOf (t)))
						chkreport_i (CHK_SUBSCRIPT_RANGE, /*chklocn */ LocnOf (tptr), LoValOf (ASIndexOf (tptr)));
				}
				/*}}}  */
			}
			break;
		}
	case S_RECORDSUB:
		if (isconst (ASBaseOf (tptr)))
			tptr = newconstexp (tptr);
		else {		/* INSdi02478 */
			/* this found by bug INSdi01982 */
			if (!issimple (ASBaseOf (tptr)))
				SetASBase (tptr, foldexp (ASBaseOf (tptr)));
		}
		break;
		/*}}}  */
		/*{{{  colon2 */
	case S_COLON2:
		SetLeftOp (tptr, foldexp (LeftOpOf (tptr)));
		SetRightOp (tptr, foldexp (RightOpOf (tptr)));
		break;
		/*}}}  */
		/*{{{  segment */
	case S_SEGMENT:
		if (isconst (tptr))
			tptr = newconstexp (tptr);
		else {
			if (!issimple (SNameOf (tptr)))
				SetSName (tptr, foldexp (SNameOf (tptr)));
			SetSStartExp (tptr, foldexp (SStartExpOf (tptr)));
			SetSLengthExp (tptr, foldexp (SLengthExpOf (tptr)));
			{
				const BOOL startconst = isconst (SStartExpOf (tptr));
				const BOOL lengthconst = isconst (SLengthExpOf (tptr));
				/*{{{  if start or length are constant, check they are in range */
				if (startconst | lengthconst) {
					treenode *const t = chk_gettype (SNameOf (tptr));
					if (TagOf (t) == S_ARRAY)
					 {	/* INSdi02273 */
						/*{{{  check start and length are legal */
						const INT32 i = startconst ? (INT32) LoValOf (SStartExpOf (tptr)) : 0;
						const INT32 j = lengthconst ? (INT32) LoValOf (SLengthExpOf (tptr)) : 0;
						INT32 d = ARDimOf (t);
						if (d == (-1))	/* if dimension is unknown */
							d = MOSTPOS_INT32;
						if (i < 0 || i > d)
							chkreport_i (CHK_SEG_START_RANGE, /*chklocn */ LocnOf (tptr), i);
						if (j < 0)
							chkreport_i (CHK_SEG_LENGTH_RANGE, /*chklocn */ LocnOf (tptr), j);
						if (j > (d - i))
							chkreport_i (CHK_SEG_RANGE, /*chklocn */ LocnOf (tptr), i + j - 1);
					}
					/*}}}  */
				}
				/*}}}  */
			}
		}
		break;
		/*}}}  */
		/*{{{  var name, string, already folded trees */
	case N_DECL:
	case N_ABBR:
	case N_RETYPE:
	case N_FIELD:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_LABELDEF:
	case S_CONSTEXP:
	case S_CONSTCONSTRUCTOR:
	case S_STRING:
	case T_TEMP:
	case T_PREEVALTEMP:
		/* The following added 28/01/92: */
	case S_FNFORMALRESULT:
	case S_FNACTUALRESULT:
	case S_HIDDEN_PARAM:
	case S_PARAM_STATICLINK:
	case S_PARAM_VSP:
		/* The following added 12/01/93 */
	case N_INLINEFUNCDEF:
	case N_INLINEPROCDEF:
	case N_TYPEDECL:
		/* CLONE operator added 05/11/2000 */
	case S_PARAM_FB:
	case S_PARAM_WS:
#ifdef MOBILES
	case S_PARAM_MSP:
	case S_PARAM_MPP:
	case S_NTH_DIMENSION:
#endif
		/* S_NULLARRAY added 11/02/2003 (frmb) */
	case S_NULLARRAY:
		break;

	case N_TAGDEF:
		tptr = newconstexp (tptr);
		break;

	case N_REPL:
		DEBUG_MSG (("foldexp: N_REPL: known? %d", NReplKnownOf (tptr)));
		if (NReplKnownOf (tptr))
			tptr = newconstexp (tptr);
		break;
		/*}}}  */
#ifdef MOBILES
		/*{{{  new array, allocate proc, new barrier, assorted address/clone/defined*/
	case S_NEW_ARRAY:
		{
			treenode *list = ARDimLengthOf (tptr);

			while (!EmptyList (list)) {
				NewItem (foldexp (ThisItem (list)), list);
				list = NextItem (list);
			}

			if (TypeAttrOf (tptr) & TypeAttr_aligned) {
				SetARAlignment (tptr, foldexp (ARAlignmentOf (tptr)));
			}
		}
		break;
	case S_ALLOC_PROC:
	case S_NEW_BARRIER:
		/* nothing to do */
		break;
	case S_UNDEFINED:
		tptr = foldexp (OpOf (tptr));
		break;
	case S_ADDROF:
	case S_HWADDROF:
	case S_CLONE:
	case S_DEFINED:
		SetOp (tptr, foldexp (OpOf (tptr)));
		break;
		/*}}}*/
#endif
		/*{{{  typehash*/
	case S_TYPEHASHOF:
		SetOp (tptr, foldexp (OpOf (tptr)));
		break;
		/*}}}*/
		/*{{{  specification */
	case S_VALABBR:
	case S_ABBR:
	case S_PROCDEF:
	case S_MPROCDECL:
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_DECL:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
	case S_RETYPE:
	case S_VALRETYPE:
	case S_PRAGMA:		/* bug 829 20/9/91 */
		{
			treenode *t;
			treenode *spec = NULL;	/* initialised to shut up gcc's optimiser */
			for (t = tptr; isspecification (t); t = DBodyOf (t)) {
				spec = t;
			}
			SetDBody (spec, foldexp (t));
		}
		break;
		/*}}}  */
		/*{{{  VALOF */
	case S_VALOF:
		/* Fold the result list */
		SetVLResultList (tptr, foldexplist (VLResultListOf (tptr)));
		break;
		/*}}}  */
		/*{{{  backend array item, segment item, segstart, elsize */
	case S_ARRAYITEM:
	case S_RECORDITEM:
		/* There will be nothing to fold on the ASBase side */
		if (ASExpOf (tptr) != NULL)
			SetASExp (tptr, foldexp (ASExpOf (tptr)));
		break;
	case S_SEGMENTITEM:
		/* There will be nothing to fold on the SName side */
		if (SSubscriptExpOf (tptr) != NULL)
			SetSSubscriptExp (tptr, foldexp (SSubscriptExpOf (tptr)));
		break;
	case S_ELSIZE:
		{
			treenode *const t = chk_gettype (OpOf (tptr));
			if ((TagOf (t) == S_ARRAY) && (ARDimOf (t) != (-1)))
				tptr = newconstexpnode (S_CONSTEXP, /*chklocn */ LocnOf (tptr), tptr, 0, ARDimOf (t));
		}
		break;
	case S_SEGSTART:
		if (isconst (SStartExpOf (OpOf (tptr))))
			tptr = newconstexp (tptr);
		break;
		/*}}}  */
		/*{{{  overlapcheck */
	case S_OVERLAPCHECK:	/* bug TS/1495 31/01/92 */
		SetLeftOp (tptr, foldexplist (LeftOpOf (tptr)));
		SetRightOp (tptr, foldexplist (RightOpOf (tptr)));
		break;
	case S_FOR:		/* bug TS/1495 31/01/92 */
		SetLeftOp (tptr, foldexp (LeftOpOf (tptr)));
		SetRightOp (tptr, foldexp (RightOpOf (tptr)));
		break;
		/*}}}  */

		/*}}}  */
		/*{{{  asinput, asoutput*/
	case S_ASINPUT:
	case S_ASOUTPUT:
		break;
		/*}}}*/
		/*{{{  LIST, N_TPROTDEF, N_SPROTDEF -- bug, but can happen in error cases*/
	case S_LIST:
	case N_TPROTDEF:
	case N_SPROTDEF:
		break;
		/*}}}*/
	default:
		badtag ( /*chklocn */ LocnOf (tptr), TagOf (tptr), "foldexp");
	}
	return (tptr);
}

/*}}}  */
/*{{{  PUBLIC treenode *foldexplist (tptr) */
PUBLIC treenode *foldexplist (treenode * tptr)
{
	treenode *t;
	for (t = tptr; !EndOfList (t); t = NextItem (t))
		NewItem (foldexp (ThisItem (t)), t);
	return (tptr);
}

/*}}}  */
/*{{{  PUBLIC treenode *foldexpinto (treenode *tptr, int type) */
/*****************************************************************************
 *
 *  foldexpinto folds the expression 'tptr', and if it is entirely constant,
 *              makes sure that the constant value node is S_CONSTCONSTRUCTOR
 *              if 'type' is S_ARRAY, S_CONSTEXP otherwise.
 *              This routine is used for storing constant RETYPEs in the
 *              type of the lhs, not the rhs.
 *
 *****************************************************************************/
PUBLIC treenode *foldexpinto (treenode * const tptr, const int type)
{
	const BOOL non_scalar_type = (type == S_ARRAY) || (type == S_RECORD);
	treenode *foldedexp = foldexp (tptr);

	if (TagOf (foldedexp) == S_CONSTEXP && non_scalar_type) {
		/*{{{  convert constexp to constconstructor */
		const INT32 count = bytesin (chk_gettype (tptr));
		BYTE *const cptr = (BYTE *) newvec ((size_t) (count + 1));

		foldscalar_into_array (HiValOf (foldedexp), LoValOf (foldedexp), cptr, (int) count);
		foldedexp = newconsttablenode (S_CONSTCONSTRUCTOR, LocnOf (tptr),
					       newwordnode (S_NAME, (char *) cptr, (int) count, NULL), CExpOf (foldedexp));
		/*}}}  */
	} else if ((TagOf (foldedexp) == S_STRING || TagOf (foldedexp) == S_CONSTCONSTRUCTOR) && !non_scalar_type) {
		/*{{{  convert constconstructor to constexp */
		BIT32 lo = 0, hi = 0;
		wordnode *c = CTValOf (foldedexp);
		int count = WLengthOf (c);

		foldarray_into_scalar (WNameOf (c), count, &hi, &lo);
		foldedexp = newconstexpnode (S_CONSTEXP, LocnOf (tptr), tptr, hi, lo);
		/*}}}  */
	}
	return foldedexp;
}

/*}}}  */

/*{{{  fold tree */
/* Apply constant folding to a tree.
   Not to any nested procedure/function decls
 */

/*{{{  PRIVATEPARAM int do_foldtree(tptr) */
/*
 *  Do constant folding on tree tptr
 */
PRIVATEPARAM int do_foldtree (treenode * t, void *dummy)
{
	dummy = dummy;
	switch (TagOf (t)) {
		/*{{{  spec */
	case S_RETYPE:
	case S_VALRETYPE:
		if (isconst (DValOf (t)))
			SetDVal (t, foldexpinto (newconstexp (DValOf (t)), TagOf (NTypeOf (DNameOf (t)))));
		else
			SetDVal (t, foldexpinto (DValOf (t), TagOf (NTypeOf (DNameOf (t)))));
		break;
	case S_VALABBR:
	case S_ABBR:
		if (isconst (DValOf (t)))
			SetDVal (t, newconstexp (DValOf (t)));
		else
			SetDVal (t, foldexp (DValOf (t)));
		break;
		/*}}}  */
		/*{{{  process */
		/*{{{  REPLSEQ REPLPAR REPLIF REPLALT */
	case S_REPLSEQ:
	case S_REPLPAR:
	case S_REPLIF:
	case S_REPLALT:
	case S_PRIREPLPAR:
	case S_PRIREPLALT:
	case S_PLACEDREPLPAR:
	case S_REPLDO:
		SetReplCStartExp (t, foldexp (ReplCStartExpOf (t)));
		SetReplCLengthExp (t, foldexp (ReplCLengthExpOf (t)));
		if (ReplCStepExpOf (t)) {
			SetReplCStepExp (t, foldexp (ReplCStepExpOf (t)));
		}
		break;
		/*}}}  */
		/*{{{  WHILE CHOICE SELECTION */
	case S_WHILE:
	case S_CHOICE:
		SetCondGuard (t, foldexp (CondGuardOf (t)));
		break;
	case S_SELECTION:
		if (TagOf (CondGuardOf (t)) != S_ELSE)
			SetCondGuard (t, foldexplist (CondGuardOf (t)));
		break;
		/*}}}  */
		/*{{{  ALTERNATIVE */
	case S_ALTERNATIVE:
		SetAltGuard (t, foldexp (AltGuardOf (t)));
		break;
		/*}}}  */
		/*{{{  PINSTANCE FINSTANCE */
	case S_PINSTANCE:
	case S_FINSTANCE:
		SetIParamList (t, foldexplist (IParamListOf (t)));
		break;
		/*}}}  */
		/*{{{  CASE_INPUT, DELAYED_INPUT, OUTPUT, INPUT, TAGGED_INPUT, ASS, CASE  break */
/* case S_CASE_INPUT: *//* removed from here for bug 1034 1/11/90 */
	case S_DELAYED_INPUT:
	case S_OUTPUT:
	case S_INPUT:
	case S_TAGGED_INPUT:
	case S_ASS:
		if (TagOf (LHSOf (t)) == S_LIST)
			SetLHS (t, foldexplist (LHSOf (t)));
		else
			SetLHS (t, foldexp (LHSOf (t)));
		if (TagOf (RHSOf (t)) == S_LIST)
			SetRHS (t, foldexplist (RHSOf (t)));
		else
			SetRHS (t, foldexp (RHSOf (t)));
		break;
	case S_CASE:
	case S_CASE_INPUT:	/* added for bug 1034 1/11/90 */
		SetLHS (t, foldexp (LHSOf (t)));
		break;
		/*}}}  */
		/*{{{  VARIANT             break */
	case S_VARIANT:
		SetVRTaggedList (t, foldexplist (VRTaggedListOf (t)));
		break;
		/*}}}  */
		/*{{{  GUYCODE GUYSTEP     return */
	case S_GUYCODE:
	case S_GUYSTEP:
		SetRightOp (t, foldexplist (RightOpOf (t)));
		break;
		/*}}}  */
		/*}}}  */
		/*{{{  VALOF */
	case S_VALOF:
		SetVLResultList (t, foldexplist (VLResultListOf (t)));
		break;
		/*}}}  */
		/*{{{  allocation */
	case S_PLACE:
	case S_WSPLACE:
	case S_VSPLACE:
		SetPlaceExp (t, foldexp (PlaceExpOf (t)));
		break;
#if 1				/*def CONFIG */
	case S_PLACEON:
		SetDName (t, foldexplist (DNameOf (t)));
		SetDVal (t, foldexp (DValOf (t)));
		break;
#endif
		/*}}}  */
	}
	return CONTINUE_WALK;
}

/*}}}  */
/*{{{  PUBLIC void foldtree(tptr) */
PUBLIC void foldtree (treenode * tptr)
{
	prewalkproctree (tptr, do_foldtree, NULL);
}

/*}}}  */

/*}}}  */

/*{{{  PUBLIC BOOL is_evaluable (n) */
/*****************************************************************************
 *
 *  Test whether an expression can be compile time evaluated.
 *  This is possible if it contains only constants and replicators of
 *  known base and count and no function calls or in-line VALOFs
 *
 *****************************************************************************/
PUBLIC BOOL is_evaluable (treenode * n)
{
	BOOL res;
	overlaps_are_const = TRUE;
	res = isconst (n);
	overlaps_are_const = FALSE;
	return res;
}

/*}}}  */
/*{{{  PUBLIC INT32 eval_const_treenode (n) */
/*****************************************************************************
 *
 *  Evaluate a compile-time evaluable expression and return result.
 *  The expression is assumed to be an int
 *
 *****************************************************************************/
PUBLIC INT32 eval_const_treenode (treenode * n)
{
	BIT32 low, high;
	foldconstexp (n, &high, &low, CHK_EXP_NOT_CONST, LocnOf (n));
	return ((INT32) low);
}

/*}}}  */

/*{{{  PUBLIC BOOL isconst_fn */
#ifdef OCCAM2_5
PUBLIC BOOL isconst_fn (treenode * const tptr)
{
	treenode *const nptr = DNameOf (tptr);
	treenode *const fn_body = DValOf (tptr);
	if (!separatelycompiled (nptr)
	    && (fn_body != NULL)
	    && (TagOf (fn_body) == S_VALOF) && (TagOf (VLBodyOf (fn_body)) == S_SKIP)
	    && (listitems (VLResultListOf (fn_body)) == 1)
	    && isscalartype (TagOf (follow_user_type (ThisItem (FnTypeListOf (NTypeOf (DNameOf (tptr)))))))) {
		BOOL result;
		const int saved_fn_lexlevel = const_fn_lexlevel;
		const BOOL saved_checking_const_fn = checking_const_fn;
		const_fn_lexlevel = NLexLevelOf (nptr);
		checking_const_fn = TRUE;

		result = isconst (ThisItem (VLResultListOf (fn_body)));

		const_fn_lexlevel = saved_fn_lexlevel;
		checking_const_fn = saved_checking_const_fn;
		return result;
	}
	return FALSE;
}
#endif
/*}}}  */
