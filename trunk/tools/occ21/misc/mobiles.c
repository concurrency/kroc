/*
 *	mobiles.c -- stuff for MOBILE data types / chan types
 *	Copyright (C) 2000-2005 Fred Barnes <frmb@kent.ac.uk>
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#include "midinc.h"
#include "occompfe.h"
#include "harndef.h"
#include "suplib.h"

#include "trandef.h"
#include "tran1def.h"
#include "predefhd.h"
#include "bind3def.h"		/* for newlab() */


#ifdef MOBILES

/*{{{  type descriptor constants*/
#define MTID_PRIM 0			/* simple types */
#define MTID_SEQPROTO 1			/* sequential protocol */
#define MTID_CHANTYPE 2			/* channel-type (contents-of) */
#define MTID_MCHANEND_IU 3		/* input-unshared mobile channel-end */
#define MTID_MCHANEND_OU 4		/* output-unshared mobile channel-end */
#define MTID_MCHANEND_IS 5		/* input-shared mobile channel-end */
#define MTID_MCHANEND_OS 6		/* output-shared mobile channel-end */
#define MTID_CHAN_I 7			/* input channel */
#define MTID_CHAN_O 8			/* output channel */
#define MTID_RECORD 9			/* record type */
#define MTID_TAGPROTO 10		/* variant protocol */
#define MTID_MARRAY 11			/* dynamic mobile array */
#define MTID_MOBILE 12			/* static mobile type */
#define MTID_ARRAY 13			/* fixed-size array type */
#define MTID_COUNTED 14			/* counted array protocol */
#define MTID_TAG 15			/* tag of a variant protocol */
#define MTID_MBARRIER 16		/* mobile barrier */
#define MTID_MPROC 17			/* mobile process */
#define MTID_STYPE 18			/* structured type */
#define MTID_FIELD 19			/* record field */
#define MTID_UNKNOWN 255		/* unknown/bad */

/*}}}*/
/*{{{  private types/data*/

static treenode *anontypelist = NULL;		/* this collects a list of anonymous mobile channel-types (no explicit declarations for these) */

typedef struct TAG_mtdescfixup {
	struct TAG_mtdescfixup *next;
	int thislab, offset;			/* relevant label and offset (bytes) from it */
	int otherlab;				/* address we put in there */
	int *otherlabaddr;			/* incase we get it later */
} mtdescfixup_t;

static mtdescfixup_t *fixuplist = NULL;
static int fixuplab = -1;


/*}}}*/


/*{{{  PRIVATE wordnode *get_anon_type_name (treenode *protocol)*/
PRIVATE wordnode *get_anon_type_name (treenode *protocol)
{
	static char t_name[128];
	int t_len;

	switch (TagOf (protocol)) {
	case S_INT:
		return lookupword ("$anon.INT", 9);
	case S_INT16:
		return lookupword ("$anon.INT16", 11);
	case S_INT32:
		return lookupword ("$anon.INT32", 11);
	case S_INT64:
		return lookupword ("$anon.INT64", 11);
	case S_UINT:
		return lookupword ("$anon.UINT", 10);
	case S_UINT16:
		return lookupword ("$anon.UINT16", 12);
	case S_UINT32:
		return lookupword ("$anon.UINT32", 12);
	case S_UINT64:
		return lookupword ("$anon.UINT64", 12);
	case S_BYTE:
		return lookupword ("$anon.BYTE", 10);
	case S_BOOL:
		return lookupword ("$anon.BOOL", 10);
	case S_REAL32:
		return lookupword ("$anon.REAL32", 12);
	case S_REAL64:
		return lookupword ("$anon.REAL64", 12);
	case N_SPROTDEF:
	case N_TPROTDEF:
		strcpy (t_name, "$anon.");
		t_len = 6;
		strcpy (t_name + t_len, WNameOf (NNameOf (protocol)));
		t_len += strlen (WNameOf (NNameOf (protocol)));
		return lookupword (t_name, t_len);
	case N_PROCTYPEDECL:
		strcpy (t_name, "$panon.");
		t_len = 7;
		strcpy (t_name + t_len, WNameOf (NNameOf (protocol)));
		t_len += WLengthOf (NNameOf (protocol));
		return lookupword (t_name, t_len);
	case N_TYPEDECL:
		strcpy (t_name, "$anon.");
		t_len = 6;
		strcpy (t_name + t_len, WNameOf (NNameOf (protocol)));
		t_len += strlen (WNameOf (NNameOf (protocol)));
#if 0
fprintf (stderr, "get_anon_type_name: N_TYPEDECL.  NTypeAttrOf (protocol) = 0x%8.8x, NTypeOf (protocol) =", NTypeAttrOf (protocol));
printtreenl (stderr, 4, NTypeOf (protocol));
#endif
		if ((TagOf (NTypeOf (protocol)) == S_MOBILE) && (NTypeAttrOf (protocol) & (TypeAttr_marked_in | TypeAttr_marked_out))) {
			/* mobile channel-type protocol */
			if (NTypeAttrOf (protocol) & TypeAttr_marked_in) {
				strcpy (t_name + t_len, "?");
				t_len++;
			} else if (NTypeAttrOf (protocol) & TypeAttr_marked_out) {
				strcpy (t_name + t_len, "!");
				t_len++;
			}
			if (NTypeAttrOf (protocol) & TypeAttr_shared) {
				strcpy (t_name + t_len, ".SH");
				t_len += 3;
			}
		}
		return lookupword (t_name, t_len);
	case S_MOBILE:
		{
			wordnode *subtype = get_anon_type_name (MTypeOf (protocol));

			strcpy (t_name, "$anon.MOBILE.");
			t_len = 13;
			strcpy (t_name + t_len, (WNameOf (subtype)) + 6);
			t_len += (strlen (WNameOf (subtype)) - 6);
		}
		return lookupword (t_name, t_len);
	case S_COLON2:
		{
			wordnode *lefttype = get_anon_type_name (LeftOpOf (protocol));
			wordnode *righttype = get_anon_type_name (RightOpOf (protocol));

			strcpy (t_name, "$anon.CC.");
			t_len = 9;
			strcpy (t_name + t_len, (WNameOf (lefttype)) + 6);
			t_len += (strlen (WNameOf (lefttype)) - 6);
			strcpy (t_name + t_len, (WNameOf (righttype)) + 6);
			t_len += (strlen (WNameOf (righttype)) - 6);
		}
		return lookupword (t_name, t_len);
	case S_ARRAY:
		{
			wordnode *basetype = get_anon_type_name (ARTypeOf (protocol));

			strcpy (t_name, "$anon.AA.");
			t_len = 9;
			strcpy (t_name + t_len, (WNameOf (basetype)) + 6);
			t_len += (strlen (WNameOf (basetype)) - 6);
		}
		return lookupword (t_name, t_len);
	default:
#if 0
fprintf (stderr, "get_anon_type_name: protocol =");
printtreenl (stderr, 4, protocol);
#endif
		return lookupword ("$anon.ERROR", 11);
	}
}
/*}}}*/
/*{{{  PUBLIC treenode *mobile_getanontype (treenode *protocol, int lexlevel)*/
PUBLIC treenode *mobile_getanontype (treenode *protocol, int lexlevel)
{
	treenode *list, *result;
	const int old = switch_to_real_workspace ();
	wordnode *typename = get_anon_type_name (protocol);

#if 0
fprintf (stderr, "mobile_getanontype: typename=[%*s], protocol =", WLengthOf(typename), WNameOf(typename));
printtreenl (stderr, 4, protocol);
#endif
	result = NULL;
	for (list = anontypelist; list; list = NextItem (list)) {
		treenode *anontype = ThisItem (list);

		if (typename == NNameOf (anontype)) {
			result = anontype;
			break;
		}
	}

	if (!result) {
		treenode *tmp, *tname;

		/* create new type */
		tmp = newtypenode (S_CHAN, NOPOSN, NULL, protocol);
		SetTypeAttr (tmp, TypeAttr_shared | TypeAttr_marked_in);
		tname = newnamenode (N_FIELD, NOPOSN, lookupword ("c", 1), tmp, NULL, lexlevel, 0, NM_DEFAULT);
		tmp = newdeclnode (S_DECL, NOPOSN, tname, NULL, NULL);
		SetNDecl (tname, tmp);
		tmp = newtypenode (S_RECORD, NOPOSN, NULL, tmp);
		SetARDim (tmp, 4);
		tmp = newtypenode (S_MOBILE, NOPOSN, NULL, tmp);
		result = newnamenode (N_TYPEDECL, NOPOSN, typename, tmp, NULL, lexlevel, 0, NM_DEFAULT);

		list = newlistnode (S_LIST, NOPOSN, result, anontypelist);
		anontypelist = list;
	}

	switch_to_prev_workspace (old);
	return result;
}
/*}}}*/
/*{{{  PUBLIC wordnode *mobile_getanonname_cli (wordnode *vname)*/
PUBLIC wordnode *mobile_getanonname_cli (wordnode *vname)
{
	wordnode *newname;
	static char tname[128];
	int tlen;

	tlen = WLengthOf (vname);
	strncpy (tname, WNameOf (vname), tlen);
	strcpy (tname + tlen, "$.cli");
	newname = lookupword (tname, tlen + 5);
	return newname;
}
/*}}}*/
/*{{{  PUBLIC wordnode *mobile_getanonname_svr (wordnode *vname)*/
PUBLIC wordnode *mobile_getanonname_svr (wordnode *vname)
{
	wordnode *newname;
	static char tname[128];
	int tlen;

	tlen = WLengthOf (vname);
	strncpy (tname, WNameOf (vname), tlen);
	strcpy (tname + tlen, "$.svr");
	newname = lookupword (tname, tlen + 5);
	return newname;
}
/*}}}*/
/*{{{  PUBLIC treenode *mobile_getanon_fromvar (treenode *var, BOOL server, BOOL withchan, treenode *varin)*/
PUBLIC treenode *mobile_getanon_fromvar (treenode *var, BOOL server, BOOL withchan, treenode *varin)
{
	treenode *tmp;
	static char namestr[128];
	int namelen;

	if (!var) {
		return NULL;
	}
	if (!NDeclOf (var)) {
		return var;
	}
	namelen = WLengthOf (NNameOf (var));
	strncpy (namestr, WNameOf (NNameOf (var)), namelen);
	strcpy (namestr + namelen, (server == TRUE) ? "$.svr" : "$.cli");

	tmp = DBodyOf (NDeclOf (var));
	if (TagOf (var) == N_DECL) {
		treenode *namelist;
		treenode *foundvar = NULL;

		/* tmp should be the client declaration */
		if (TagOf (tmp) != S_DECL) {
#if 0
fprintf (stderr, "mobile_getanon_fromvar: failed at 1.\n");
#endif
			return NULL;
		}
		namelist = DNameOf (tmp);
		if (TagOf (namelist) == S_LIST) {
			while (!EmptyList (namelist)) {
				treenode *thisname = ThisItem (namelist);

				if (TagOf (thisname) != N_DECL) {
#if 0
fprintf (stderr, "mobile_getanon_fromvar: failed at 2.  thisname =");
printtreenl (stderr, 4, thisname);
#endif
					return NULL;
				}
				if (server == FALSE) {
					/* looking for one of these */
					if (!strncmp (namestr, WNameOf (NNameOf (thisname)), namelen + 5)) {
						foundvar = thisname;
					}
				}
				namelist = NextItem (namelist);
			}
		} else if (TagOf (namelist) != N_DECL) {
#if 0
fprintf (stderr, "mobile_getanon_fromvar: failed at 3.\n");
#endif
			return NULL;
		} else if (strncmp (namestr, WNameOf (NNameOf (DNameOf (tmp))), (server == TRUE) ? (namelen + 2) : (namelen + 5))) {
#if 0
fprintf (stderr, "mobile_getanon_fromvar: failed at 4.\n");
#endif
			return NULL;
		} else if (server == FALSE) {
			foundvar = namelist;
		}
		if (server == TRUE) {
			tmp = DBodyOf (tmp);	/* advance to server */

			namelist = DNameOf (tmp);
			if (TagOf (namelist) == S_LIST) {
				while (!EmptyList (namelist)) {
					treenode *thisname = ThisItem (namelist);

					if (TagOf (thisname) != N_DECL) {
						return NULL;
					}
					/* looking for one of these */
					if (!strncmp (namestr, WNameOf (NNameOf (thisname)), namelen + 5)) {
						foundvar = thisname;
					}
					namelist = NextItem (namelist);
				}
			} else if (TagOf (namelist) != N_DECL) {
#if 0
fprintf (stderr, "mobile_getanon_fromvar: failed at 5.\n");
#endif
				return NULL;
			} else if (strncmp (namestr, WNameOf (NNameOf (namelist)), namelen + 5)) {
#if 0
fprintf (stderr, "mobile_getanon_fromvar: failed at 6.\n");
#endif
				return NULL;
			} else {
				foundvar = namelist;
			}
		}
		if (!foundvar) {
#if 0
fprintf (stderr, "mobile_getanon_fromvar: failed at 7.\n");
#endif
			return NULL;
		}
		tmp = foundvar;
	} else if (TagOf (var) == N_PARAM) {
		/* this is a parameter, so "tmp" should now be the namenode for the right shared end, check that it matches "server" however */
		if (TagOf (tmp) != N_PARAM) {
#if 0
fprintf (stderr, "mobile_getanon_fromvar: failed at 8\n");
#endif
			return NULL;
		} else if (strncmp (namestr, WNameOf (NNameOf (tmp)), namelen + 5)) {
#if 0
fprintf (stderr, "mobile_getanon_fromvar: failed at 9\n");
#endif
			return NULL;
		}
#if 0
fprintf (stderr, "mobile_getanon_fromvar: PARAM: tmp =");
printtreenl (stderr, 4, tmp);
#endif
	}

#if 0
fprintf (stderr, "mobile_getanon_fromvar: found it.  ARTypeOf (MTypeOf (2*NTypeOf (tmp))) = ");
printtreenl (stderr, 4, ARTypeOf (MTypeOf (NTypeOf (NTypeOf (tmp)))));
#endif
	if (withchan == TRUE) {
		/* wrap it up in an RECORDSUB node */
		tmp = newarraysubnode (S_RECORDSUB, LocnOf (varin), tmp, DNameOf (ARTypeOf (MTypeOf (NTypeOf (NTypeOf (tmp))))));
	}
	return tmp;
}
/*}}}*/
/*{{{  PUBLIC treenode *mobile_getanontypes (void)*/
/*
 *	returns the list of generated anonymous types
 */
PUBLIC treenode *mobile_getanontypes (void)
{
	return anontypelist;
}
/*}}}*/


/*{{{  PRIVATE int mobile_typedesc (treenode *const tptr, unsigned int *buf, int *len, int max)*/
/*
 *	local function for generating type descriptions
 */
PRIVATE int mobile_typedesc (treenode *const tptr, unsigned int *buf, int *len, int max)
{
	int thislen = 0;

	if (*len >= max) {
		return 0;
	}

	switch (TagOf (tptr)) {
		/*{{{  N_TYPEDECL -- type*/
	case N_TYPEDECL:
		if (isdynmobilechantypetype (tptr)) {
			/*{{{  code for a mobile channel-type*/
			const int sels[] = { MTID_MCHANEND_IU, MTID_MCHANEND_OU, MTID_MCHANEND_IS, MTID_MCHANEND_OS };
			int sel = sels[((NTypeAttrOf (tptr) & TypeAttr_shared) ? 2 : 0) + ((NTypeAttrOf (tptr) & TypeAttr_marked_in) ? 0 : 1)];

			thislen = 2;
			*len = *len + 2;
			buf[0] = (sel << 24) | (thislen * sizeof (unsigned int));
			buf[1] = typehash (NTypeOf (tptr));
			/*{{{  generate fixup*/
#if 0
fprintf (stderr, "mobile_typedesc(): want to generate fixup for N_TYPEDECL, *len = %d, , MTDLabOf (NTypeOf (tptr)) = %d, NTypeOf (tptr) =", *len, MTDLabOf (NTypeOf (tptr)));
printtreenl (stderr, 4, NTypeOf (tptr));
#endif
			{
				mtdescfixup_t *fixup = (mtdescfixup_t *)memalloc (sizeof (mtdescfixup_t));

				fixup->next = fixuplist;
				fixuplist = fixup;
				fixup->thislab = fixuplab;
				fixup->offset = (*len - 1) * sizeof (unsigned int);
				if (MTDLabOf (NTypeOf (tptr)) < 0) {
					SetMTDLab (NTypeOf (tptr), newlab ());
				}
				fixup->otherlab = MTDLabOf (NTypeOf (tptr));
				fixup->otherlabaddr = (fixup->otherlab == -1) ? MTDLabAddr (NTypeOf (tptr)) : NULL;
			}
			/*}}}*/
			/*}}}*/
		} else {
			/* regular type declaration */
			thislen = mobile_typedesc (NTypeOf (tptr), buf, len, max);
		}
		break;
		/*}}}*/
		/*{{{  N_DECL, N_FIELD -- name, probably a channel*/
	case N_DECL:
	case N_FIELD:
		thislen = mobile_typedesc (NTypeOf (tptr), buf, len, max);
		break;
		/*}}}*/
		/*{{{  N_TAGDEF -- tagged protocol variant*/
	case N_TAGDEF:
		{
			int itemcount = 0;

			treenode *first, *walk;

			first = NTypeOf (tptr);
			thislen = 3;
			*len = *len + 3;

			buf[1] = (unsigned int)NTValueOf (tptr);

			for (walk=first; !EndOfList (walk); walk = NextItem (walk), itemcount++) {
				thislen += mobile_typedesc (ThisItem (walk), buf + thislen, len, max);
			}

			buf[2] = (unsigned int)itemcount;
			buf[0] = (MTID_TAG << 24) | (thislen * sizeof (unsigned int));
		}
		break;
		/*}}}*/
		/*{{{  N_SPROTDEF -- sequential protocol*/
	case N_SPROTDEF:
		{
			int itemcount = 0;
			treenode *first, *walk;

#if 0
fprintf (stderr, "mobile_typedesc: N_SPROTDEF: NTypeOf (tptr) = ");
printtreenl (stderr, 4, NTypeOf (tptr));
#endif
			first = NTypeOf (tptr);
			thislen = 2;
			*len = *len + 2;

			for (walk=first; !EndOfList (walk); walk = NextItem (walk), itemcount++) {
				thislen += mobile_typedesc (ThisItem (walk), buf + thislen, len, max);
			}
			buf[1] = (unsigned int)itemcount;
			buf[0] = (MTID_SEQPROTO << 24) | (thislen * sizeof (unsigned int));
		}
		break;
		/*}}}*/
		/*{{{  N_TPROTDEF -- tagged protocol*/
	case N_TPROTDEF:
		{
			int vcount = 0;
			treenode *first, *walk;

#if 0
fprintf (stderr, "mobile_typedesc: N_TPROTDEF: NTypeOf (tptr) = ");
printtreenl (stderr, 4, NTypeOf (tptr));
#endif
			first = NTypeOf (tptr);
			thislen = 2;
			*len = *len + 2;

			for (walk=first; !EndOfList (walk); walk = NextItem (walk), vcount++) {
				thislen += mobile_typedesc (ThisItem (walk), buf + thislen, len, max);
			}
			buf[1] = (unsigned int)vcount;
			buf[0] = (MTID_TAGPROTO << 24) | (thislen * sizeof (unsigned int));
		}
		break;
		/*}}}*/
		/*{{{  S_CHAN -- channel*/
	case S_CHAN:
#if 0
fprintf (stderr, "mobile_typedesc: channel: TypeAttrOf(tptr) = 0x%8.8x", (unsigned int)TypeAttrOf (tptr));
printtreenl (stderr, 4, tptr);
#endif
		thislen = 1;
		*len = *len + 1;

		thislen += mobile_typedesc (ProtocolOf (tptr), buf + thislen, len, max);

		buf[0] = (((TypeAttrOf (tptr) & TypeAttr_marked_in) ? MTID_CHAN_I : MTID_CHAN_O) << 24) | (thislen * sizeof (unsigned int));
		break;
		/*}}}*/
		/*{{{  S_INT, etc. -- primitive types*/
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
	case S_BYTE:
	case S_BOOL:
		thislen = 3;
		*len = *len + 3;

		buf[0] = (MTID_PRIM << 24) | (thislen * sizeof (unsigned int));
		buf[1] = (unsigned int)bytesin (tptr);
		buf[2] = typehash (tptr);
		break;
		/*}}}*/
		/*{{{  S_RECORD -- occam2.5 record types*/
	case S_RECORD:
		{
			int submobile = 0;
			treenode *rtype = ARTypeOf (tptr);

			/* see if we have any mobile sub-types */
			while (rtype && (TagOf (rtype) == S_DECL)) {
				treenode *item = DNameOf (rtype);

				if (ismobile (item)) {
					submobile++;
				}
				rtype = DBodyOf (rtype);
			}

			if (submobile) {
#if 0
fprintf (stderr, "mobile_typedesc(): in RECORD, got %d mobile sub-types\n", submobile);
#endif
				thislen = 4;
				*len = *len + 4;

				for (rtype = ARTypeOf (tptr); rtype && (TagOf (rtype) == S_DECL); rtype = DBodyOf (rtype)) {
					treenode *item = DNameOf (rtype);

					if (ismobile (item)) {
						unsigned int *fbuf = buf + thislen;
						int xlen = 2;		/* field */

						*len = *len + 2;
						xlen += mobile_typedesc (NTypeOf (item), buf + thislen + xlen, len, max);

						fbuf[0] = (MTID_FIELD << 24) | (xlen * sizeof (unsigned int));
						fbuf[1] = NVOffsetOf (item);

						thislen += xlen;
					}
				}

				buf[0] = (MTID_STYPE << 24) | (thislen * sizeof (unsigned int));
				buf[1] = (unsigned int)ARDimOf (tptr);
				buf[2] = typehash (tptr);
				buf[3] = submobile;
			} else {
				thislen = 3;
				*len = *len + 3;

				buf[0] = (MTID_RECORD << 24) | (thislen * sizeof (unsigned int));
				buf[1] = (unsigned int)ARDimOf (tptr);
				buf[2] = typehash (tptr);
			}
		}
		break;
		/*}}}*/
		/*{{{  S_MOBILE -- various mobile types */
	case S_MOBILE:
#if 0
fprintf (stderr, "mobile_typedesc: mobile: tptr=");
printtreenl (stderr, 4, tptr);
#endif
		if (isdynmobilebarriertype (tptr)) {
			/* mobile barrier */
			thislen = 1;
			*len = *len + 1;

			buf[0] = (MTID_MBARRIER << 24) | (thislen * sizeof (unsigned int));
		} else if (isdynmobileproctypetype (tptr)) {
			/* mobile process */
			thislen = 2;
			*len = *len + 2;

			buf[1] = typehash (MTypeOf (tptr));
			buf[0] = (MTID_MPROC << 24) | (thislen * sizeof (unsigned int));
		} else if (isdynmobilearraytype (tptr)) {
			/* dynamic mobile array */
			int dcount = dynmobiledimensioncount (tptr);
			int i;
			treenode *basetype;

#if 0
fprintf (stderr, "mobile_typedesc: dynamic mobile array: dcount = %d, tptr =", dcount);
printtreenl (stderr, 4, tptr);
#endif
			thislen = 2;
			*len = *len + 2;

			basetype = MTypeOf (tptr);
			for (i=0; i<dcount; i++) {
				basetype = ARTypeOf (basetype);
			}

			thislen += mobile_typedesc (basetype, buf + thislen, len, max);

			buf[1] = dcount;
			buf[0] = (MTID_MARRAY << 24) | (thislen * sizeof (unsigned int));
		} else {
			/* static mobile */
			thislen = 1;
			*len = *len + 1;

			thislen += mobile_typedesc (MTypeOf (tptr), buf + thislen, len, max);
			buf[0] = (MTID_MOBILE << 24) | (thislen * sizeof (unsigned int));
		}
		break;
		/*}}}*/
		/*{{{  S_ARRAY -- fixed-size arrays*/
	case S_ARRAY:
		thislen = 2;
		*len = *len + 2;

		thislen += mobile_typedesc (ARTypeOf (tptr), buf + thislen, len, max);

#if 0
fprintf (stderr, "mobiles.c: mobile_typedesc(): encoding S_ARRAY, ARDimOf (tptr) = %d, bytesin (tptr) = %d.  tptr = ", (int)ARDimOf (tptr), bytesin (tptr));
printtreenl (stderr, 4, tptr);
#endif
		buf[1] = (unsigned int)ARDimOf (tptr);
		buf[0] = (MTID_ARRAY << 24) | (thislen * sizeof (unsigned int));
		break;
		/*}}}*/
		/*{{{  S_COLON2 -- counted array protocol*/
	case S_COLON2:
		thislen = 1;
		*len = *len + 1;

#if 0
fprintf (stderr, "mobile_typedesc: counted array: tptr=");
printtreenl (stderr, 4, tptr);
#endif
		thislen += mobile_typedesc (LeftOpOf (tptr), buf + thislen, len, max);
		thislen += mobile_typedesc (ARTypeOf (RightOpOf (tptr)), buf + thislen, len, max);

		buf[0] = (MTID_COUNTED << 24) | (thislen * sizeof (unsigned int));
		break;
		/*}}}*/
	default:
#if 0
fprintf (stderr, "mobiles.c: mobile_typedesc(): unhandled type =");
printtreenl (stderr, 4, tptr);
#endif
		thislen = 1;
		*len = *len + 1;
		buf[0] = (MTID_UNKNOWN << 24) | (thislen * sizeof (unsigned int));
		break;
	}

	return thislen;
}
/*}}}*/
/*{{{  PUBLIC int mobile_gettypedesc (treenode *const type, unsigned int *buf, int *len, int max)*/
/*
 *	generates a mobile-type description (currently only for mobile channel-types and "deep" mobile types)
 */
PUBLIC int mobile_gettypedesc (treenode *const type, unsigned int *buf, int *len, int max)
{
	int thislen = 0;
	treenode *subtype;
	int chantypedesc = 0;

#if 0
fprintf (stderr, "mobile.c: mobile_gettypedesc(): type =");
printtreenl (stderr, 4, type);
#endif

	if (TagOf (type) != S_MOBILE) {
		return 0;
	}
	subtype = MTypeOf (type);

	switch (TagOf (subtype)) {
		/*{{{  S_RECORD -- structured type, maybe channel-type*/
	case S_RECORD:
		if ((TagOf (ARTypeOf (subtype)) == S_DECL) && (TagOf (NTypeOf (DNameOf (ARTypeOf (subtype)))) == S_CHAN)) {
			/* channel-type */
			int chancount = 0;
			treenode *firstdecl, *walk;


			thislen = 3;
			*len = 3;
			firstdecl = ARTypeOf (subtype);

			for (chancount = 0, walk = firstdecl; walk && (TagOf (walk) == S_DECL); walk = DBodyOf (walk), chancount++) {
				treenode *chan = DNameOf (walk);

				/* generate descriptor for this channel */
				thislen += mobile_typedesc (chan, buf + thislen, len, max);
			}

			buf[1] = chancount;
			buf[2] = typehash (type);

#if 0
fprintf (stderr, "mobile_gettypedesc(): mobile channel-type, chancount=%d, typehash=0x%8.8x\n", chancount, (unsigned int)typehash (type));
#endif
			buf[0] = (MTID_CHANTYPE << 24) | (thislen * sizeof (unsigned int));

			chantypedesc = 1;
		}
		break;
		/*}}}*/
	}
	if (!chantypedesc) {
		/*{{{  generate ordinary type descriptor*/
		int toadd = mobile_typedesc (type, buf + thislen, len, max);
		
		thislen = toadd;
		/*}}}*/
	}

	return thislen;
}
/*}}}*/
/*{{{  PUBLIC int mobile_typedescfixup_start (int lab)*/
/*
 *	called before a type description is generated in order to catch required fixups
 *	returns 0 on success, non-zero on failure
 */
PUBLIC int mobile_typedescfixup_start (int lab)
{
	mtdescfixup_t *walk, *next;

	/* free any old list */
	for (walk = fixuplist, next = NULL; walk; walk = next) {
		next = walk->next;
		memfree (walk);
	}
	fixuplist = NULL;
	fixuplab = lab;

	return 0;
}
/*}}}*/
/*{{{  PRIVATE int cmp_mtdescfixup (mtdescfixup_t **first, mtdescfixup_t **second)*/
/*
 *	compares two mtdescfixup_t structures
 */
PRIVATE int cmp_mtdescfixup (mtdescfixup_t **first, mtdescfixup_t **second)
{
#if 0
fprintf (stderr, "cmp_mtdescfixup(): (%d,%d,%d) vs. (%d,%d,%d)\n", (*first)->thislab, (*first)->offset, (*first)->otherlab, (*second)->thislab, (*second)->offset, (*second)->otherlab);
#endif
	return (*first)->offset - (*second)->offset;
}
/*}}}*/
/*{{{  PUBLIC int mobile_typedescfixup_finish (int lab, void (*gencomment)(const char *))*/
/*
 *	called after a type description has been generated so that the translator can fixup
 *	references to labels in the generated type-description.
 *	returns 0 on success, non-zero on failure
 */
PUBLIC int mobile_typedescfixup_finish (int lab, void (*gencomment)(const char *))
{
	mtdescfixup_t *walk, **tarray;
	int xcount, i;
	static char xbuf[128];

	for (xcount = 0, walk = fixuplist; walk; walk = walk->next, xcount++);		/* count how many */
	if (!xcount) {
		return 0;			/* nothing needed */
	}
#if 0
fprintf (stderr, "mobile_... : about to sort, xcount = %d\n", xcount);
#endif
	tarray = (mtdescfixup_t **)memalloc ((xcount + 4) * sizeof (mtdescfixup_t *));
	for (xcount = 0, walk = fixuplist; walk; walk = walk->next, xcount++) {
		tarray[xcount] = walk;
#if 0
fprintf (stderr, "mobile_typedescfixup_finish(): presort: tarray[%d] = (%d,%d,%d)\n", xcount, walk->thislab, walk->offset, walk->otherlab);
#endif
	}
	for (i=xcount; i<(xcount + 4); i++) {
		tarray[i] = NULL;
	}
#if 0
fprintf (stderr, "mobile_... : about to sort, xcount = %d\n", xcount);
#endif
	sup_qsort ((void *)tarray, xcount, sizeof (mtdescfixup_t *), (int (*)(const void *, const void *))cmp_mtdescfixup);
	sprintf (xbuf, ".MAGIC TYPEDESC");
	gencomment (xbuf);
	for (i=0; i<xcount; i++) {
		if (tarray[i]->otherlabaddr) {
			tarray[i]->otherlab = *(tarray[i]->otherlabaddr);
		}
		sprintf (xbuf, ".MAGIC FIXUP %d %d %d", tarray[i]->thislab, tarray[i]->offset, tarray[i]->otherlab);
#if 0
fprintf (stderr, "mobile_typedescfixup_finish(): postsort: tarray[%d] = (%d,%d,%d)\n", i, tarray[i]->thislab, tarray[i]->offset, tarray[i]->otherlab);
#endif
		gencomment (xbuf);
	}
	memfree (tarray);

	return 0;
}
/*}}}*/


#endif	/* MOBILES */

