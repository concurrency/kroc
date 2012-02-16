/*
 *	vti5.c -- virtual tree handling - XML
 *	Copyright (C) 2005 Fred Barnes <frmb@kent.ac.uk>
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

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include "midinc.h"
#include "chkdef.h"
/*#include "usehdr.h"*//* PROC/FUNC usage info */


/*}}}*/


/*{{{  PRIVATE void xml_setindent (FILE *const fptr, int indent)*/
/*
 *	sets the indent (multiples of 2 spaces for XML)
 */
PRIVATE void xml_setindent (FILE *const fptr, int indent)
{
	int i;

	for (i=0; i<indent; i++) {
		fprintf (fptr, "  ");
	}
	return;
}
/*}}}*/
#if 0
/*{{{  PRIVATE void xml_ptag (FILE *const fptr, const int t)*/
/*
 *	prints an occam tag-string for XML
 */
PRIVATE void xml_ptag (FILE *const fptr, const int t)
{
	if (t == 0) {
		return;
	}
	fprintf (fptr, "%s", tagstring (t));
	return;
}
/*}}}*/
#endif
/*{{{  PRIVATE void xml_pitag (FILE *const fptr, const int t)*/
/*
 *	prints an internal tag-string for XML
 */
PRIVATE void xml_pitag (FILE *const fptr, const int t)
{
	if (t == 0) {
		return;
	}
	fprintf (fptr, "%s", itagstring (t));
	return;
}
/*}}}*/
/*{{{  PRIVATE int xml_printable_string (const char *ptr, const int len)*/
/*
 *	returns non-zero if the string given is printable sensibly (i.e. contains no special chars)
 */
PRIVATE int xml_printable_string (const char *ptr, const int len)
{
	char *ch;
	int i;

	for (ch=(char *)ptr, i=0; i<len; ch++, i++) {
		if (((unsigned char)*ch < 32) || ((unsigned char)*ch > 127)) {
			return 0;
		}
	}
	return 1;
}
/*}}}*/
/*{{{  PRIVATE void xml_pcdatastring (FILE *const fptr, const char *ptr, const int len)*/
/*
 *	prints a string as a hex-encoded CDATA chunk
 */
PRIVATE void xml_pcdatastring (FILE *const fptr, const char *ptr, const int len)
{
	char *ch;
	int i;

	fprintf (fptr, "<![CDATA[");
	for (ch=(char *)ptr, i=0; i<len; ch++, i++) {
		fprintf (fptr, "%2.2X", (unsigned char)*ch);
	}
	fprintf (fptr, "]]>\n");
	return;
}
/*}}}*/
/*{{{  PRIVATE void xml_pstring (FILE *const fptr, const char *ptr, const int len)*/
/*
 *	prints a string but escapes any XML-incompatible characters
 */
PRIVATE void xml_pstring (FILE *const fptr, const char *ptr, const int len)
{
	int tlen = len;
	char *ch;
	int i;

	for (ch=(char *)ptr, i=0; i<len; ch++, i++) {
		switch (*ch) {
		case '<':
		case '>':
			tlen += 3;
			break;
		case '\"':
			tlen += 5;
			break;
		case '&':
			tlen += 4;
			break;
		case '~':
			tlen += 6;
			break;
		case '`':
			tlen += 6;
			break;
		}
	}
	if (tlen == len) {
		fprintf (fptr, "%*s", len, ptr);
	} else {
		char *nstr = (char *)newvec (tlen + 1);
		char *dh;

		for (ch=(char *)ptr, dh=nstr, i=0; i<len; ch++, dh++, i++) {
			switch (*ch) {
			case '<':
				strcpy (dh, "&lt;");
				dh += 3;
				break;
			case '>':
				strcpy (dh, "&gt;");
				dh += 3;
				break;
			case '\"':
				strcpy (dh, "&quot;");
				dh += 7;
				break;
			case '&':
				strcpy (dh, "&amp;");
				dh += 4;
				break;
			case '~':
				strcpy (dh, "&tilde;");
				dh += 6;
				break;
			case '`':
				strcpy (dh, "&grave;");
				dh += 6;
				break;
			default:
				*dh = *ch;
				break;
			}
		}
		*dh = '\0';
		fprintf (fptr, "%s", nstr);
		freevec (nstr, tlen + 1);
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void xml_printdeclname (FILE *const fptr, int indent, treenode *n)*/
/*
 *	prints the name in a declaration (includes type)
 */
PUBLIC void xml_printdeclname (FILE *const fptr, int indent, treenode *n)
{
	if (!n) {
		xml_setindent (fptr, indent);
		fprintf (fptr, "<null />\n");
		return;
	}
	if (nodetypeoftag (TagOf (n)) != NAMENODE) {
		xml_printtree (fptr, indent, n);
		return;
	}

	xml_setindent (fptr, indent);
	fprintf (fptr, "<name type=\"");
	xml_pitag (fptr, TagOf (n));
	fprintf (fptr, "\" name=\"%*s\"", WLengthOf (NNameOf (n)), WNameOf (NNameOf (n)));

	switch (TagOf (n)) {
	case N_TPROTDEF:
	case N_PROCDEF:
#ifdef MOBILES
	case N_MPROCDECL:
#endif
	case N_INLINEPROCDEF:
		fprintf (fptr, ">\n");
		/* type of PROCs is a parameter list */
#if 0
fprintf (stderr, "vti5: xml_printdeclname: PROCDEF of some kind, NTypeOf (n) = ");
printtreenl (stderr, 4, NTypeOf (n));
#endif
		if (NTypeOf (n) && (TagOf (NTypeOf (n)) == S_LIST)) {
			treenode *walk;

			xml_setindent (fptr, indent + 1);
			fprintf (fptr, "<list>\n");
			for (walk = NTypeOf (n); !EndOfList (walk); walk = NextItem (walk)) {
				xml_printdeclname (fptr, indent + 2, ThisItem (walk));
			}
			xml_setindent (fptr, indent + 1);
			fprintf (fptr, "</list>\n");
		} else {
			xml_printtree (fptr, indent + 1, NTypeOf (n));
		}
		break;
	case N_SFUNCDEF:
	case N_LFUNCDEF:
	case N_INLINEFUNCDEF:
		fprintf (fptr, ">\n");
		xml_printtree (fptr, indent + 1, FnTypeListOf (NTypeOf (n)));
		xml_printtree (fptr, indent + 1, FnParamsOf (NTypeOf (n)));
		break;
	case N_ABBR:
		fprintf (fptr, ">\n");
		xml_printtree (fptr, indent + 1, NTypeOf (n));
		break;
	case N_TAGDEF:
		fprintf (fptr, " ntval=\"%d\">\n", (int)NTValueOf (n));
		xml_printtree (fptr, indent + 1, NTypeOf (n));
		break;
	default:
		fprintf (fptr, ">\n");
		xml_printtree (fptr, indent + 1, NTypeOf (n));
		break;
	}

	/* print free variable lists */
	switch (TagOf (n)) {
	case N_PROCDEF:
#ifdef MOBILES
	case N_MPROCDECL:
#endif
	case N_SFUNCDEF:
	case N_LFUNCDEF:
	case N_INLINEPROCDEF:
	case N_INLINEFUNCDEF:
		{
			xml_setindent (fptr, indent + 1);
			fprintf (fptr, "<freevars>\n");
			xml_setindent (fptr, indent + 1);
			fprintf (fptr, "</freevars>\n");
		}
		break;
	}


	xml_setindent (fptr, indent);
	fprintf (fptr, "</name>\n");

	return;
}
/*}}}*/
/*{{{  PRIVATE void xml_printbardecls (FILE *const fptr, int indent, treenode *bar)*/
/*
 *	prints out a barrier declaration list
 */
PRIVATE void xml_printbardecls (FILE *const fptr, int indent, treenode *bar)
{
	if (!bar) {
		xml_setindent (fptr, indent);
		fprintf (fptr, "<null />\n");
		return;
	}
	if (TagOf (bar) == S_BAREXTEND) {
		treenode *declbars = LeftOpOf (bar);
		treenode *extbars = RightOpOf (bar);

		xml_setindent (fptr, indent);
		fprintf (fptr, "<dopnode type=\"");
		xml_pitag (fptr, TagOf (bar));
		fprintf (fptr, "\">\n");

		/* print declarations whole */
		if (TagOf (declbars) == S_LIST) {
			treenode *walk;

			xml_setindent (fptr, indent + 1);
			fprintf (fptr, "<list>\n");

			for (walk = declbars; !EndOfList (walk); walk = NextItem (walk)) {
				xml_printdeclname (fptr, indent + 2, ThisItem (walk));
			}

			xml_setindent (fptr, indent + 1);
			fprintf (fptr, "</list>\n");
		} else {
			xml_printdeclname (fptr, indent + 1, declbars);
		}

		/* print extensions */
		xml_printtree (fptr, indent + 1, extbars);

		xml_setindent (fptr, indent);
		fprintf (fptr, "</dopnode>\n");
	} else if (TagOf (bar) == S_EXTENDS) {
		/* print extensions */
		xml_printtree (fptr, indent, bar);
	} else if (TagOf (bar) == S_LIST) {
		/* list of declarations */
		treenode *walk;

		xml_setindent (fptr, indent);
		fprintf (fptr, "<list>\n");

		for (walk = bar; !EndOfList (walk); walk = NextItem (walk)) {
			xml_printdeclname (fptr, indent + 1, ThisItem (walk));
		}

		xml_setindent (fptr, indent);
		fprintf (fptr, "</list>\n");
	} else {
		xml_printdeclname (fptr, indent, bar);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void xml_ptypeattrs (FILE *const fptr, INT32 attr)*/
/*
 *	prints type attributes
 */
PRIVATE void xml_ptypeattrs (FILE *const fptr, INT32 attr)
{
	static INT32 attrvals[] = {TypeAttr_datatype, TypeAttr_packed, TypeAttr_chantype, TypeAttr_wordlen, TypeAttr_marked_in, TypeAttr_marked_out, TypeAttr_placed, TypeAttr_iospace,
			TypeAttr_dynmobile, TypeAttr_shared, TypeAttr_claimed, TypeAttr_recursive, TypeAttr_undefined, TypeAttr_fixed, TypeAttr_ufixed, TypeAttr_resigned, 0};
	static char *attrdesc[] = {"datatype", "packed", "chantype", "wordlen", "marked_in", "marked_out", "placed", "iospace",
			"dynmobile", "shared", "claimed", "recursive", "undefined", "fixed", "ufixed", "resigned", NULL};
	int i;

	if (!attr) {
		return;
	}
	fprintf (fptr, "typeattr=\"");
	for (i=0; attrvals[i] && attr; i++) {
		if (attr & attrvals[i]) {
			attr &= ~(attrvals[i]);
			fprintf (fptr, "%s%s", attrdesc[i], (attr ? " " : ""));
		}
	}
	fprintf (fptr, "\"");
	return;
}
/*}}}*/
/*{{{  PRIVATE void xml_pactionflags (FILE *const fptr, INT32 aflags)*/
/*
 *	prints action flags
 */
PRIVATE void xml_pactionflags (FILE *const fptr, INT32 aflags)
{
	static INT32 aflagvals[] = {ActionFlag_skip_xable, ActionFlag_decode, ActionFlag_encode, ActionFlag_mobile, ActionFlag_dynmob, ActionFlag_count, ActionFlag_case,
			ActionFlag_ed3seq, ActionFlag_edacomm, ActionFlag_edmcomm, ActionFlag_precount, ActionFlag_mobproc, ActionFlag_chantype, 0};
	static char *aflagdesc[] = {"skip_xable", "decode", "encode", "mobile", "dynmob", "count", "case",
			"ed3seq", "edacomm", "edmcomm", "precount", "mobproc", "chantype", NULL};
	int i;

	if (!aflags) {
		return;
	}
	fprintf (fptr, "actionflags=\"");
	for (i=0; aflagvals[i] && aflags; i++) {
		if (aflags & aflagvals[i]) {
			aflags &= ~(aflagvals[i]);
			fprintf (fptr, "%s%s", aflagdesc[i], (aflags ? " " : ""));
		}
	}
	fprintf (fptr, "\"");
	return;
}
/*}}}*/
/*{{{  PUBLIC void xml_printtree (FILE *const fptr, int indent, treenode *n)*/
/*
 *	pretty-prints the tree in XML
 */
PUBLIC void xml_printtree (FILE *const fptr, int indent, treenode *n)
{
	if (!n) {
		xml_setindent (fptr, indent);
		fprintf (fptr, "<null />\n");
		return;
	}
	while (n) {
		xml_setindent (fptr, indent);
		switch (nodetypeoftag (TagOf (n))) {
			/*{{{  cases*/
			/*{{{  LEAFNODE*/
		case LEAFNODE:
			fprintf (fptr, "<leafnode type=\"");
			xml_pitag (fptr, TagOf (n));
			if ((TagOf (n) == S_SYNC) && LeafLinkOf (n)) {
				fprintf (fptr, "\">\n");
				xml_printtree (fptr, indent + 1, LeafLinkOf (n));
				xml_setindent (fptr, indent);
				fprintf (fptr, "</leafnode>\n");
			} else {
				fprintf (fptr, "\" />\n");
			}
			return;
			/*}}}*/
			/*{{{  CNODE*/
		case CNODE:
			fprintf (fptr, "<cnode type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\">\n");
			if (CTempOf (n)) {
				/* possibly a barrier declaration.. */
				switch (TagOf (n)) {
				case S_PAR:
				case S_PRIPAR:
					xml_printbardecls (fptr, indent + 1, CTempOf (n));
					break;
				default:
					xml_printtree (fptr, indent + 1, CTempOf (n));
					break;
				}
			}
			xml_printtree (fptr, indent + 1, CBodyOf (n));
			xml_setindent (fptr, indent);
			fprintf (fptr, "</cnode>\n");
			return;
			/*}}}*/
			/*{{{  REPLCNODE*/
		case REPLCNODE:
			fprintf (fptr, "<replcnode type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\">\n");
			if (ReplCTempOf (n)) {
				/* possibly a barrier declaration.. */
				switch (TagOf (n)) {
				case S_REPLPAR:
				case S_PRIREPLPAR:
					xml_printbardecls (fptr, indent + 1, ReplCTempOf (n));
					break;
				default:
					xml_printtree (fptr, indent + 1, ReplCTempOf (n));
					break;
				}
			}

			xml_printdeclname (fptr, indent + 1, ReplCNameOf (n));
			xml_printtree (fptr, indent + 1, ReplCStartExpOf (n));
			xml_printtree (fptr, indent + 1, ReplCLengthExpOf (n));
			xml_printtree (fptr, indent + 1, ReplCStepExpOf (n));
			xml_printtree (fptr, indent + 1, ReplCBodyOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</replcnode>\n");
			return;
			/*}}}*/
			/*{{{  VALOFNODE*/
		case VALOFNODE:
			fprintf (fptr, "<valofnode type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\">\n");

			xml_printtree (fptr, indent + 1, VLBodyOf (n));
			xml_printtree (fptr, indent + 1, VLResultListOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</valofnode>\n");
			return;
			/*}}}*/
			/*{{{  SEGMENTNODE*/
		case SEGMENTNODE:
			fprintf (fptr, "<segmentnode type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\"");
			if (TagOf (n) == S_SEGMENTITEM) {
				fprintf (fptr, " soffset=\"%d\"", (int)SOffsetOf (n));
			}
			fprintf (fptr, ">\n");

			xml_printtree (fptr, indent + 1, SNameOf (n));
			xml_printtree (fptr, indent + 1, SStartExpOf (n));
			xml_printtree (fptr, indent + 1, SLengthExpOf (n));
			
			xml_printtree (fptr, indent + 1, SCheckExpOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</segmentnode>\n");
			return;
			/*}}}*/
			/*{{{  LISTNODE*/
		case LISTNODE:
			fprintf (fptr, "<list>\n");
			while (!EndOfList (n)) {
				xml_printtree (fptr, indent + 1, ThisItem (n));
				n = NextItem (n);
			}
			xml_setindent (fptr, indent);
			fprintf (fptr, "</list>\n");
			return;
			/*}}}*/
			/*{{{  DECLNODE*/
		case DECLNODE:
			fprintf (fptr, "<decl type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\">\n");
			
			xml_printdeclname (fptr, indent + 1, DNameOf (n));
			xml_printtree (fptr, indent + 1, DValOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</decl>\n");

			n = DBodyOf (n);
			break;
			/*}}}*/
			/*{{{  TYPENODE*/
		case TYPENODE:
			fprintf (fptr, "<type type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\" ");
			if (TypeAttrOf (n)) {
				xml_ptypeattrs (fptr, TypeAttrOf (n));
			}
			switch (TagOf (n)) {
			case S_CHAN:
			case S_PORT:
				fprintf (fptr, ">\n");
				xml_printtree (fptr, indent + 1, ProtocolOf (n));
				break;
#ifdef MOBILES
			case S_MOBILE:
				fprintf (fptr, ">\n");
				xml_printtree (fptr, indent + 1, MTypeOf (n));
				break;
#endif
			default:
				fprintf (fptr, " ardimof=\"%d\">\n", (int)ARDimOf (n));
				xml_printtree (fptr, indent + 1, ARDimLengthOf (n));
				xml_printtree (fptr, indent + 1, ARTypeOf (n));
				break;
			}
			xml_setindent (fptr, indent);
			fprintf (fptr, "</type>\n");
			return;
			/*}}}*/
			/*{{{  NAMENODE*/
		case NAMENODE:
			fprintf (fptr, "<name type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\"");
			switch (TagOf (n)) {
			case T_TEMP:
			case T_REGTEMP:
			case T_PREEVALTEMP:
				fprintf (fptr, " varnum=\"%d\" lexlevel=\"%d\"", NVVarNumOf (n), NLexLevelOf (n));
				if ((TagOf (n) != T_REGTEMP) && (NVOffsetOf (n) != vti_no_slot_value)) {
					fprintf (fptr, " offset=\"%d\"", NVOffsetOf (n));
				}
				fprintf (fptr, " />\n");
				/* xml_printtree (fptr, indent + 1, NTypeOf (n)); */
				break;
			case S_FNFORMALRESULT:
			case S_FNACTUALRESULT:
			case S_HIDDEN_PARAM:
			case S_PARAM_STATICLINK:
			case S_PARAM_VSP:
			case S_PARAM_FB:
			case S_PARAM_MSP:
			case S_PARAM_MPP:
			case S_HIDDEN_TYPE:
				fprintf (fptr, " dim=\"%d\" offset=\"%d\" argno=\"%d\">\n", HDimensionOf (n), NVOffsetOf (n), HArgNoOf (n));
				if (TagOf (n) != S_FNACTUALRESULT) {
					xml_printtree (fptr, indent + 1, HExpOf (n));
				}
				xml_setindent (fptr, indent);
				fprintf (fptr, "</name>\n");
				break;
			default:
				{
					wordnode *nptr = NNameOf (n);

					fprintf (fptr, " name=\"%*s\" />\n", (nptr ? WLengthOf (nptr) : 6), (nptr ? WNameOf (nptr) : "(null)"));
					/* xml_printtree (fptr, indent + 1, NTypeOf (n)); */
				}
				break;
			}
			return;
			/*}}}*/
			/*{{{  ACTIONNODE*/
		case ACTIONNODE:
			fprintf (fptr, "<action type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\"");
			if (ActionFlagsOf (n)) {
				fprintf (fptr, " ");
				xml_pactionflags (fptr, ActionFlagsOf (n));
			}
			fprintf (fptr, ">\n");

			xml_printtree (fptr, indent + 1, LHSOf (n));
			xml_printtree (fptr, indent + 1, RHSOf (n));
			xml_printtree (fptr, indent + 1, ActionTypeOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</action>\n");
			return;
			/*}}}*/
			/*{{{  CONSTEXPNODE*/
		case CONSTEXPNODE:
			fprintf (fptr, "<constexp type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\" offset=\"%d\" valhi=\"%d\" vallo=\"%d\">\n", CEOffsetOf (n), (int)HiValOf (n), (int)LoValOf (n));

			xml_printtree (fptr, indent + 1, CExpOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</constexp>\n");
			return;
			/*}}}*/
			/*{{{  CONSTTABLENODE*/
		case CONSTTABLENODE:
			fprintf (fptr, "<consttable type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\">\n");

			if (TagOf (n) == S_STRING) {
				xml_setindent (fptr, indent + 1);
				if (xml_printable_string (WNameOf ((wordnode *)CTValOf (n)), WLengthOf ((wordnode *)CTValOf (n)))) {
					fprintf (fptr, "<string val=\"");
					xml_pstring (fptr, WNameOf ((wordnode *)CTValOf (n)), WLengthOf ((wordnode *)CTValOf (n)));
					fprintf (fptr, "\" />\n");
				} else {
					fprintf (fptr, "<string>\n");
					xml_setindent (fptr, indent + 2);
					xml_pcdatastring (fptr, WNameOf ((wordnode *)CTValOf (n)), WLengthOf ((wordnode *)CTValOf (n)));
					xml_setindent (fptr, indent + 1);
					fprintf (fptr, "</string>\n");
				}
			} else if (TagOf (n) == S_CONSTPTR) {
				const BYTE *b = (const BYTE *)CTPtrOf (n);
				treenode *name = nameof (n);
				treenode *type;
				int basebytes;

				switch (TagOf (name)) {
				case N_VALABBR:
				case N_ABBR:
				case N_VALRETYPE:
				case N_RETYPE:
				case N_DECL:
				case N_VALPARAM:
				case N_PARAM:
				case N_RESULTPARAM:
				case N_REPL:
					type = NTypeOf (name);
					break;
				case S_CONSTRUCTOR:
				case S_STRING:
				case S_CONSTCONSTRUCTOR:
				case S_ARRAYCONSTRUCTOR:
					type = chk_gettype (name);
					break;
				default:
					type = NULL;
					break;
				}
				if (type) {
					basebytes = bytesinscalar (basetype (type));
				} else {
					basebytes = 0;
				}

				xml_setindent (fptr, indent + 1);
				fprintf (fptr, "<constptr basebytes=\"%d\">\n", basebytes);
				xml_setindent (fptr, indent + 2);
				xml_pcdatastring (fptr, (const char *)b, basebytes);
				xml_setindent (fptr, indent + 1);
				fprintf (fptr, "</constptr>\n");
			} else {
				wordnode *nptr = (wordnode *)CTValOf (n);

				xml_setindent (fptr, indent + 1);
				fprintf (fptr, "<constconstructor length=\"%d\">\n", WLengthOf (nptr));
				xml_setindent (fptr, indent + 2);
				xml_pcdatastring (fptr, WNameOf (nptr), WLengthOf (nptr));
				xml_setindent (fptr, indent + 1);
				fprintf (fptr, "</constconstructor>\n");
			}

			xml_setindent (fptr, indent);
			fprintf (fptr, "</consttable>\n");
			return;
			/*}}}*/
			/*{{{  ARRAYSUBNODE*/
		case ARRAYSUBNODE:
			fprintf (fptr, "<arraysub type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\"");
			if ((TagOf (n) == S_ARRAYITEM) || (TagOf (n) == S_RECORDITEM)) {
				fprintf (fptr, " asoffset=\"%d\"", ASOffsetOf (n));
			}
			fprintf (fptr, ">\n");

			xml_printtree (fptr, indent + 1, ASBaseOf (n));
			xml_printtree (fptr, indent + 1, ASIndexOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</arraysub>\n");
			return;
			/*}}}*/
			/*{{{  CONDNODE*/
		case CONDNODE:
			fprintf (fptr, "<cond type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\">\n");

			xml_printtree (fptr, indent + 1, CondGuardOf (n));
			if (TagOf (n) == S_X_VARIANT) {
				xml_printtree (fptr, indent + 1, VRDuringOf (n));
				xml_printtree (fptr, indent + 1, VRAfterOf (n));
			} else {
				xml_printtree (fptr, indent + 1, CondBodyOf (n));
			}

			xml_setindent (fptr, indent);
			fprintf (fptr, "</cond>\n");
			return;
			/*}}}*/
			/*{{{  MOPNODE*/
		case MOPNODE:
			fprintf (fptr, "<mopnode type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\" optype=\"");
			xml_pitag (fptr, MOpTypeOf (n));
			fprintf (fptr, "\">\n");

			xml_printtree (fptr, indent + 1, OpOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</mopnode>\n");
			return;
			/*}}}*/
			/*{{{  DOPNODE*/
		case DOPNODE:
			fprintf (fptr, "<dopnode type=\"");
			xml_pitag (fptr, TagOf (n));

			switch (TagOf (n)) {
			case S_GUYCODE:
			case S_GUYSTEP:
				fprintf (fptr, "\">\n");

				xml_setindent (fptr, indent + 1);
				fprintf (fptr, "<guycode code=\"0x%x\" name=\"%s\">\n", DOpTypeOf (n), WNameOf ((wordnode *)LeftOpOf (n)));
				xml_printtree (fptr, indent + 2, RightOpOf (n));
				xml_setindent (fptr, indent + 1);
				fprintf (fptr, "</guycode>\n");
				break;
			default:
				fprintf (fptr, "\" optype=\"");
				xml_pitag (fptr, DOpTypeOf (n));
				fprintf (fptr, "\">\n");

				xml_printtree (fptr, indent + 1, LeftOpOf (n));
				xml_printtree (fptr, indent + 1, RightOpOf (n));
				break;
			}

			xml_setindent (fptr, indent);
			fprintf (fptr, "</dopnode>\n");
			return;
			/*}}}*/
			/*{{{  LITNODE*/
		case LITNODE:
			fprintf (fptr, "<litnode type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\">\n");
			if (TagOf (n) == S_CONSTRUCTOR) {
				xml_printtree (fptr, indent + 1, LitExpOf (n));
				xml_printtree (fptr, indent + 1, LitTypeOf (n));
			} else {
				wordnode *nptr = StringPtrOf (n);

				xml_setindent (fptr, indent + 1);
				if (xml_printable_string (WNameOf (nptr), WLengthOf (nptr))) {
					fprintf (fptr, "<string val=\"");
					xml_pstring (fptr, WNameOf (nptr), WLengthOf (nptr));
					fprintf (fptr, "\" />\n");
				} else {
					fprintf (fptr, "<string>\n");

					xml_setindent (fptr, indent + 2);
					xml_pcdatastring (fptr, WNameOf (nptr), WLengthOf (nptr));
					xml_setindent (fptr, indent + 1);
					fprintf (fptr, "</string>\n");
				}
				xml_printtree (fptr, indent + 1, LitTypeOf (n));
			}

			xml_setindent (fptr, indent);
			fprintf (fptr, "</litnode>\n");
			return;
			/*}}}*/
			/*{{{  ALTNODE*/
		case ALTNODE:
			fprintf (fptr, "<altnode type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\">\n");

			xml_printtree (fptr, indent + 1, AltGuardOf (n));
			xml_printtree (fptr, indent + 1, AltChanExpOf (n));
			xml_printtree (fptr, indent + 1, AltInputOf (n));
			xml_printtree (fptr, indent + 1, AltBodyOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</altnode>\n");
			return;
			/*}}}*/
			/*{{{  INSTANCENODE*/
		case INSTANCENODE:
			fprintf (fptr, "<instance type=\"");
			xml_pitag (fptr, TagOf (n));
			fprintf (fptr, "\"");
			if (IForkedOf (n)) {
				fprintf (fptr, " forked=\"true\"");
			}
			if (IRecursiveOf (n)) {
				fprintf (fptr, " recursive=\"true\"");
			}
			if (IDynmemOf (n)) {
				fprintf (fptr, " dynmem=\"true\"");
			}
			fprintf (fptr, ">\n");

			xml_printtree (fptr, indent + 1, INameOf (n));
			xml_printtree (fptr, indent + 1, IParamListOf (n));

			xml_setindent (fptr, indent);
			fprintf (fptr, "</instance>\n");
			return;
			/*}}}*/
			/*}}}*/
		default:
			fprintf (fptr, "<unknown tag=\"%d\" />\n", TagOf (n));
			return;
		}
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void xml_printtree_wrapper (FILE *const fptr, int indent, treenode *n, const char *sfname)*/
/*
 *	prints XML tree with a top-level wrapper
 */
PUBLIC void xml_printtree_wrapper (FILE *const fptr, int indent, treenode *n, const char *sfname)
{
	xml_setindent (fptr, indent);
	fprintf (fptr, "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n");
	xml_setindent (fptr, indent);
	fprintf (fptr, "<parsetree version=\"1.0\" language=\"occam-pi\" compiler=\"occ21\" source=\"%s\">\n", sfname ? sfname : "");
	xml_printtree (fptr, indent + 1, n);
	xml_setindent (fptr, indent);
	fprintf (fptr, "</parsetree>\n");
	return;
}

/*}}}*/


