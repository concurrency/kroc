/* $Id: objlib.c,v 1.3 1997/11/21 17:40:19 mdp2 Exp $ */

/*
 *	occam 2 library object file handling
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
#include "tcoff.h"		/* IMPORTED */

#include "midinc.h"

#include "objlib.h"

#include "chkdef.h"
#include "chkerror.h"

/*}}}*/

/*{{{  PUBLIC BOOL compatible_call(callee_instr, callee_attr)*/
PUBLIC BOOL compatible_call (const BIT32 callee_instr, const BIT32 callee_attr)
{
	/* return TRUE if the callee's bits are a subset of the caller's, or check suppressed */
	const txlib_t *const txlib = current_fe_data->fe_txlib;
	if (current_fe_data->fe_suppress_compat)
		return TRUE;
	return ((txlib->ptype | callee_instr) == txlib->ptype) && ((txlib->pattr | callee_attr) == txlib->pattr);
}

/*}}}*/
/*{{{  PUBLIC search_libentries*/
PUBLIC libentry_t *search_libentries (libentry_t *ptr, wordnode *name)
{
	DEBUG_MSG (("search_libentries: name is \"%s\"\n", WNameOf (name)));
	while ((ptr != NULL) && (name != ptr->l_name))
		ptr = ptr->l_next;
	return (ptr);
}

/*}}}*/
/*{{{  PUBLIC search_modulechain*/
PUBLIC module_t *search_modulechain (module_t *ptr, long int seek_val)
{
	DEBUG_MSG (("search_modulechain: seekval is %ld\n", seek_val));
	while ((ptr != NULL) && (seek_val != ptr->m_seek_ptr))
		ptr = ptr->m_next;
	return (ptr);
}

/*}}}*/
/*{{{  PRIVATE procdef *check_fpu_calling_convention*/
PRIVATE procdef_t *check_fpu_calling_convention (procdef_t *procdef, treenode *const nptr, const SOURCEPOSN locn, const BOOL check)
/* this is called if the specified routine only exists compiled
   for the `wrong' calling convention. We allow this as long as the
   code works (when configuring!).
*/
{
	if (procdef != NULL)
		switch (TagOf (nptr)) {
		case N_LIBFUNCDEF:
		case N_SCFUNCDEF:
		case N_STDLIBFUNCDEF:
			{
				treenode *resultlist = FnTypeListOf (NTypeOf (nptr));
				DEBUG_MSG (("check_fpu_calling_convention: found a function\n"));
				if ((listitems (resultlist) == 1) && (isreal (TagOf (ThisItem (resultlist))))) {
					libentry_t *const libentry = NLExternalOf (nptr);
					DEBUG_MSG (("check_fpu_calling_convention: one REAL result param\n"));
					procdef = NULL;
					if (check && ((libentry->l_bits & LIBENTRY_BIT_COMPATIBLE_ERR) == 0)) {
						libentry->l_bits |= LIBENTRY_BIT_COMPATIBLE_ERR;
						chkerr_s (CHK_CALLING_CONVENTION, locn, WNameOf (NNameOf (nptr)));
					}
				}
			}
			break;
		default:
			DEBUG_MSG (("check_fpu_calling_convention: found a procedure\n"));
			break;
		}
	return procdef;
}

/*}}}*/
/*{{{  PRIVATE procdef_t *lookup_procdef*/
PRIVATE procdef_t *lookup_procdef (treenode *const nptr)
{
	libentry_t *const libentry = NLExternalOf (nptr);
	procdef_t *procdef = libentry->l_procdefs;
	/*#ifndef SINGLE_PROCDEF */
	if ((current_fe_data->fe_lang & FE_LANG_OCCAM) == 0) {
		while ((procdef != NULL) && !compatible_call (procdef->p_module->m_instr, procdef->p_module->m_attr)) {
			procdef = procdef->p_next;
		}
	}
	/*#endif */
	return procdef;
}

/*}}}*/
/*{{{  PUBLIC procdef_t *get_procdef*/
PUBLIC procdef_t *get_procdef (treenode *nptr, const SOURCEPOSN locn, const BOOL check, const BOOL allow_fpu_calls)
{
	procdef_t *procdef = lookup_procdef (nptr);
	libentry_t *const libentry = NLExternalOf (nptr);
	txlib_t *const txlib = current_fe_data->fe_txlib;

	/*{{{  TA on a T8 */
	/* This bit here allows us to use eg TA code on a T800 when configuring,
	   as long as it doesn't break the calling convention.
	   I will currently leave this disabled for T9000s.
	   CON - 28/3/91
	 */
	if (allow_fpu_calls &&	/* Allow T800/TA calls        */
	    (procdef == NULL) &&	/* Not immediately compatible */
	    ((txlib->ptype & ARCH_T) != 0) &&	/* It's a T800 series         */
	    ((txlib->pattr & ATTRIB_FPU_CALLING) != 0)) {	/* and has an FPU          */
		const BIT32 saved_attr = txlib->pattr;
		DEBUG_MSG (("get_procdef: trying non-fpu calling convention\n"));
		txlib->pattr = (txlib->pattr & ~ATTRIB_FPU_CALLING_MASK) | ATTRIB_NON_FPU_CALLING;
		procdef = check_fpu_calling_convention (lookup_procdef (nptr), nptr, locn, check);
		txlib->pattr = saved_attr;
	}
	/*}}} */

	/*{{{  TA on a T450 */
	/* This bit here allows us to use eg TA code on a T450 when configuring.
	   NICK - 14/7/94
	 */
	if ((procdef == NULL) &&	/* Not immediately compatible */
	    (txlib->ptype == T450_INSTR)) {	/* It's a T450 series         */
		const BIT32 saved_ptype = txlib->ptype;
		DEBUG_MSG (("get_procdef: trying t450 not worrying about semaphore or device instructions\n"));

		txlib->ptype |= INSTR_NO_SEMAPHORE | INSTR_NO_DEVICE;

		procdef = lookup_procdef (nptr);

		txlib->ptype = saved_ptype;
	}
	/*}}} */

	/*{{{  T9000GAMMA on a T9000 */
	/*{{{  COMMENT */
	/* See bug INSdi03437. This section should be removed when
	   that bug report is resolved.
	   It is only here (temporarily) so that we can configure for
	   T9000 gamma even though the NDL says T9000.
	 */
	/*}}} */
	/* INSdi03368
	   This bit here allows us to use T9000GAMMA code on a T9000 when configuring.
	 */
#if 0
	if (allow_fpu_calls &&	/* Allow T800/TA calls        */
	    (procdef == NULL) &&	/* Not immediately compatible */
	    ((txlib->ptype & ARCH_H) != 0) &&	/* It's a T9000 series         */
	    ((txlib->ptype & H_INSTR_T9000_FULL) != 0)) {	/* and not a gamma          */
		const BIT32 saved_type = txlib->ptype;
		DEBUG_MSG (("get_procdef: trying T9000GAMMA\n"));
		txlib->ptype = (txlib->ptype & ~H_INSTR_T9000_FULL) | H_INSTR_T9000_GAMMA;
		procdef = lookup_procdef (nptr);
		txlib->ptype = saved_type;
	}
#endif
	/*}}} */

	if (check && (procdef == NULL) && ((libentry->l_bits & LIBENTRY_BIT_COMPATIBLE_ERR) == 0))
		/*{{{  error */
	{
		const BOOL configuring = (current_fe_data->fe_lang & FE_LANG_OCCAM) == 0;
		BOOL libio_err = FALSE;
		libentry->l_bits |= LIBENTRY_BIT_COMPATIBLE_ERR;
		if (procdef == NULL) {
			const BIT32 saved_attr = txlib->pattr;
			txlib->pattr = (txlib->pattr & ~ATTRIB_IO_MASK) | ATTRIB_INSTR_IO;
			libio_err = lookup_procdef (nptr) != NULL;
			txlib->pattr = saved_attr;
		}

		/* bug 1055 20/12/90 - changed this back from a warning to an error */
		/* (I think it got accidentally changed when I modified the error handling */
		/* bug INSdi02427 - better error message if configuring */
		msg_out_ss (SEV_ERR, CHK, libio_err ? (configuring ? CHK_LIB_WRONG_IO_CONFIG : CHK_LIB_WRONG_IO)
			    : CHK_LIB_WRONG_TARGET, locn,
			    (TagOf (NDeclOf (nptr)) == S_MPROCDECL) ? "MOBILE PROC" :
			    ((TagOf (NDeclOf (nptr)) == S_PROCDEF) ? "PROC" : "FUNCTION"), WNameOf (NNameOf (nptr)));
	}
	/*}}} */

	return procdef;
}

/*}}}*/
/*{{{  PUBLIC void checklibproctype*/
PUBLIC void checklibproctype (treenode * nptr, const SOURCEPOSN locn, const BOOL allow_fpu_calls)
{
	(void) get_procdef (nptr, locn, TRUE, allow_fpu_calls);
}

/*}}}*/
/*{{{  PUBLIC BOOL compiledforcorrectproc*/
PUBLIC BOOL compiledforcorrectproc (treenode * const nptr, const BOOL allow_fpu_calls)
{
	return (get_procdef (nptr, NOPOSN, FALSE, allow_fpu_calls) != NULL);
}

/*}}}*/
/*{{{  PUBLIC void getlibwsandvs*/
PUBLIC void getlibwsandvs (treenode * const nptr, const SOURCEPOSN locn, const BOOL allow_fpu_calls, INT32 * const ws, INT32 * const vs)
{
	procdef_t *procdef = get_procdef (nptr, locn, TRUE, allow_fpu_calls);
	if (procdef == NULL) {	/* we've just supplied an error message */
		*ws = 0;
		*vs = 0;
	} else {
		*ws = procdef->p_ws;
		*vs = procdef->p_vs;
	}
	return;
}
/*}}}*/
#ifdef MOBILES
/*{{{  PUBLIC void getlibwsandvsandms (treenode *const nptr, const SOURCEPOSN locn, const BOOL allow_fpu_calls, INT32 *const ws, INT32 *const vs, INT32 *const ms)*/
PUBLIC void getlibwsandvsandms (treenode *const nptr, const SOURCEPOSN locn, const BOOL allow_fpu_calls, INT32 *const ws, INT32 *const vs, INT32 *const ms)
{
	procdef_t *procdef = get_procdef (nptr, locn, TRUE, allow_fpu_calls);

	if (!procdef) {
		*ws = 0;
		*vs = 0;
		*ms = 0;
	} else {
		*ws = procdef->p_ws;
		*vs = procdef->p_vs;
		*ms = procdef->p_ms;
	}
	return;
}
/*}}}*/
#endif

/*{{{  generating a descriptor*/
/*{{{  data for descriptor handling*/
/* This is the length where we decide to break descriptor lines. */
#define DESC_LINE_LENGTH 72

/* This is used as a buffer for building descriptors for output */
#define DESC_BUFFER_SIZE 512

/* This is an extensible buffer. desc_buffer_size holds the current size
   if desc_buffer is not NULL. It is incremented in steps of DESC_BUFFER_SIZE
   whenever full
*/
PRIVATE char *desc_buffer = NULL;
PRIVATE int desc_buffer_size = DESC_BUFFER_SIZE;

/* This only hold anything of use while actually creating the descriptor */
PRIVATE int desc_buffer_ptr;
/*}}}*/

/*{{{  PRIVATE BOOL sametypeparam*/
PRIVATE BOOL sametypeparam (treenode *const p1, treenode *const p2)
/* returns TRUE if we can 'merge' adjacent parameter declarations */
{
	treenode *t1;
	treenode *t2;

	if ((p1 == NULL) ||	/* first one in the list */
	    (TagOf (p1) != TagOf (p2)))	/* VAL and non-VAL etc */
		return FALSE;

	t1 = NTypeOf (p1);
	t2 = NTypeOf (p2);

	while (TRUE) {
		if (TagOf (t1) != TagOf (t2))
			return FALSE;
		switch (TagOf (t1)) {
			/*{{{  primitive types */
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
		CASE_CONFIG_TYPE case N_TPROTDEF:
		case N_SPROTDEF:
			return (TRUE);
			/*}}} */
			/*{{{  chan and port */
		case S_CHAN:
		case S_PORT:
			t1 = ProtocolOf (t1);
			t2 = ProtocolOf (t2);
			break;
			/*}}} */
			/*{{{  S_ARRAY */
		case S_ARRAY:
			{
				/* Check dimensions are the same */
				const BIT32 l1 = ARDimOf (t1);
				const BIT32 l2 = ARDimOf (t2);
				if (l1 != l2)
					return FALSE;
				t1 = ARTypeOf (t1);
				t2 = ARTypeOf (t2);
				break;
			}
			/*}}} */
		default:
			return FALSE;
		}
	}
}

/*}}}*/
/*{{{  write_descprotocol forward declaration*/
PRIVATE void write_descprotocol (treenode * pptr, int *line_len);
/*}}}*/
/*{{{  PRIVATE void check_desc_buffer_size*/
PRIVATE void check_desc_buffer_size (const int len)
{
	if ((desc_buffer_ptr + len + 1) >= desc_buffer_size) {
		/*lexfatal(LEX_TCOFF_DESC_OVERFLOW, NOPOSN); */

		/* This is basically a realloc */
		char *new;
		desc_buffer_size *= 2;
		DEBUG_MSG (("write_descstring: extending buffer to size %d\n", desc_buffer_size));
		new = memalloc (desc_buffer_size);
		memcpy (new, desc_buffer, desc_buffer_ptr);
		memfree (desc_buffer);
		desc_buffer = new;
	}
}

/*}}}*/
/*{{{  PRIVATE void write_descstring(s)*/
PRIVATE void write_descstring (const char *const s, int *const line_len)
{
	const int len = strlen (s);
	check_desc_buffer_size (len);
	memcpy (&(desc_buffer[desc_buffer_ptr]), s, len + 1);
	desc_buffer_ptr += len;
	*line_len += len;
}

/*}}}*/
/*{{{  PRIVATE void write_descnumber(n)*/
PRIVATE void write_descnumber (const INT32 n, int *const line_len)
{
	char number_buffer[12];

	sprintf (number_buffer, "%ld", (long) n);
	write_descstring (number_buffer, line_len);
}

/*}}}*/
/*{{{  PRIVATE void write_desctypehash (treenode *const tptr, int *const line_len)*/
PRIVATE void write_desctypehash (treenode *const tptr, int *const line_len)
{
	char number_buffer[12];

	sprintf (number_buffer, "@#%8.8X", (unsigned int)typehash_names (tptr));
	write_descstring (number_buffer, line_len);
}
/*}}}*/

/*{{{  PRIVATE void write_desctype(tptr)*/
PRIVATE void write_desctype (treenode *const tptr, int *const line_len)
{
#if 0
fprintf (stderr, "objlib: write_desctype(): writing type tptr = ");
printtreenl (stderr, 4, tptr);
#endif

	switch (TagOf (tptr)) {
	default:
		write_descstring (tagstring (TagOf (tptr)), line_len);
		break;
		/*{{{  TYPEDECL */
#ifdef OCCAM2_5
	case N_TYPEDECL:
#ifdef MOBILES
		/* could be a channel-type, or an anonymous channel-type */
		if (TagOf (NTypeOf (tptr)) == S_MOBILE) {
#if 0
fprintf (stderr, "write_desctype: MOBILE TYPEDECL, NTypeAttrOf(tptr)=%d, NTy..(MTy..(tptr))=%d, DNameOf(ARTypeOf(MTypeOf(NTypeOf(tptr))))=", NTypeAttrOf (tptr), NTypeAttrOf (MTypeOf (tptr)));
printtreenl (stderr, 4, DNameOf(ARTypeOf (MTypeOf (NTypeOf (tptr)))));
#endif
			if (NTypeAttrOf (tptr) & TypeAttr_shared) {
				write_descstring ("SHARED ", line_len);
			}
			if (!strncmp (WNameOf (NNameOf (tptr)), "$anon.", 6)) {
				write_descstring ("CHAN OF ", line_len);
				write_descprotocol (ProtocolOf (NTypeOf (DNameOf (ARTypeOf (MTypeOf (NTypeOf (tptr)))))), line_len);
			} else {
				write_descstring (WNameOf (NNameOf (tptr)), line_len);
				/* also output typehash of this.. */
				write_desctypehash (tptr, line_len);

				if (NTypeAttrOf (tptr) & TypeAttr_marked_out) {
					write_descstring ("!", line_len);
				} else if (NTypeAttrOf (tptr) & TypeAttr_marked_in) {
					write_descstring ("?", line_len);
				}
			}
		} else {
			write_descstring (WNameOf (NNameOf (tptr)), line_len);
			/* also output typehash of this.. */
			write_desctypehash (tptr, line_len);
		}
#else
		write_descstring (WNameOf (NNameOf (tptr)), line_len);
#endif
		break;
#endif
		/*}}} */
		/*{{{  PROCTYPEDECL*/
	case N_PROCTYPEDECL:
		write_descstring (WNameOf (NNameOf (tptr)), line_len);
		/* also output typehash of this.. */
		write_desctypehash (tptr, line_len);
		break;
		/*}}}*/
		/*{{{  FULLBARRIER*/
	case S_FULLBARRIER:
		write_descstring ("BARRIER", line_len);
		break;
		/*}}}*/
#ifdef MOBILES
		/*{{{  MOBILE */
	case S_MOBILE:
		write_descstring ("MOBILE ", line_len);
		write_desctype (MTypeOf (tptr), line_len);
		break;
		/*}}}  */
		/*{{{  MOBILE.CHAN (ANYCHANTYPE)*/
	case S_ANYCHANTYPE:
		{
			treenode *mtype = LeafLinkOf (tptr);

			if (mtype && (TypeAttrOf (mtype) & TypeAttr_shared)) {
				write_descstring ("SHARED ", line_len);
			}
			write_descstring ("MOBILE.CHAN", line_len);
			if (mtype && (TypeAttrOf (mtype) & TypeAttr_marked_in)) {
				write_descstring ("?", line_len);
			} else if (mtype && (TypeAttrOf (mtype) & TypeAttr_marked_out)) {
				write_descstring ("!", line_len);
			}
		}
		break;
		/*}}}*/
		/*{{{  MOBILE.PROC (ANYPROCTYPE)*/
	case S_ANYPROCTYPE:
		write_descstring ("MOBILE.PROC", line_len);
		break;
		/*}}}*/
		/*{{{  MOBILE.ANY (ANYMOBILETYPE)*/
	case S_ANYMOBILETYPE:
		write_descstring ("MOBILE.ANY", line_len);
		break;
		/*}}}*/
#endif
		/*{{{  ARRAY */
	case S_ARRAY:
		write_descstring ("[", line_len);
		if (ARDimOf (tptr) != (-1)) {
			write_descnumber ((INT32) ARDimOf (tptr), line_len);
		} else if ((ARDimLengthOf (tptr) != NULL) && (TagOf (ARDimLengthOf (tptr)) == S_UINTLIT)) {
			/* bit of a hack for mobiles.. */
			write_descstring (WNameOf (StringPtrOf (ARDimLengthOf (tptr))), line_len);
		}
		write_descstring ("]", line_len);
		write_desctype (ARTypeOf (tptr), line_len);
		break;
		/*}}} */
		/*{{{  CHAN/PORT */
	case S_CHAN:
	case S_PORT:
		write_descstring (tagstring (TagOf (tptr)), line_len);
		write_descstring (" OF ", line_len);
		write_descprotocol (ProtocolOf (tptr), line_len);
		break;
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE void write_desctypelist(tptr)*/
PRIVATE void write_desctypelist (treenode *tptr, int *const line_len)
{
	int count;
#if 0
fprintf (stderr, "objlib: write_desctypelist(): tptr = ");
printtreenl (stderr, 4, tptr);
#endif
	for (count = 0; !EndOfList (tptr); tptr = NextItem (tptr), count++) {
		if (count != 0) {
			write_descstring (",", line_len);
		}
		write_desctype (ThisItem (tptr), line_len);
	}
}

/*}}}*/
/*{{{  PRIVATE void write_descprotocol(pptr)*/
PRIVATE void write_descprotocol (treenode *const pptr, int *const line_len)
{
	switch (TagOf (pptr)) {
	case N_TPROTDEF:
	case N_SPROTDEF:
		write_descstring (WNameOf (NNameOf (pptr)), line_len);
		/* also output typehash of this.. */
		write_desctypehash (pptr, line_len);
		break;
	case S_COLON2:
		write_desctype (LeftOpOf (pptr), line_len);
		write_descstring ("::", line_len);
		write_desctype (RightOpOf (pptr), line_len);
		break;
	default:
		write_desctype (pptr, line_len);
	}
}

/*}}}*/
/*{{{  PRIVATE void write_descparam(tptr)*/
PRIVATE void write_descparam (treenode *const old_nptr, treenode *const nptr, const int paramno, int *const line_len, const BOOL short_names)
{
	treenode *chantype;
	char *ch;


#if 0
fprintf (stderr, "objlib: write_descparam(): paramno = %d, nptr = ", paramno);
printtreenl (stderr, 4, nptr);
#endif
	if (!sametypeparam (old_nptr, nptr)) {
		if (TagOf (nptr) == N_VALPARAM) {
			write_descstring ("VAL ", line_len);
		} else if (TagOf (nptr) == N_RESULTPARAM) {
			write_descstring ("RESULT ", line_len);
		} else if (NTypeAttrOf (nptr) & TypeAttr_undefined) {
			write_descstring ("UNDEFINED ", line_len);
		} else if (NTypeAttrOf (nptr) & TypeAttr_fixed) {
			write_descstring ("FIXED ", line_len);
		}
		write_desctype (NTypeOf (nptr), line_len);
		write_descstring (" ", line_len);
	}
	if (short_names && (WLengthOf (NNameOf (nptr)) > 2)) {
		char buffer[20];
		sprintf (buffer, "p%d", paramno);
		SetNName (nptr, lookupword (buffer, strlen (buffer)));
	}
#ifdef MOBILES
	/* anonymous channel-types spotted through $.cli/$.svr in name */
	ch = strchr (WNameOf (NNameOf (nptr)), '$');
	if (ch && !strcmp (ch, "$.cli")) {
		/* client (output) side anonymous channel-type */
		*ch = '\0';
		write_descstring (WNameOf (NNameOf (nptr)), line_len);
		*ch = '$';
		write_descstring ("!", line_len);
	} else if (ch && !strcmp (ch, "$.svr")) {
		/* server (input) side anonymous channel-type */
		*ch = '\0';
		write_descstring (WNameOf (NNameOf (nptr)), line_len);
		*ch = '$';
		write_descstring ("?", line_len);
	} else {
		write_descstring (WNameOf (NNameOf (nptr)), line_len);
	}
#else
	write_descstring (WNameOf (NNameOf (nptr)), line_len);
#endif
	chantype = NTypeOf (nptr);
	while (TagOf (chantype) == S_ARRAY) {
		chantype = ARTypeOf (chantype);
	}
	if ((TagOf (chantype) == S_CHAN) || (TagOf (chantype) == S_PORT)) {
		if (TypeAttrOf (chantype) & TypeAttr_marked_in) {
			write_descstring ("?", line_len);
		} else if (TypeAttrOf (chantype) & TypeAttr_marked_out) {
			write_descstring ("!", line_len);
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void write_descheader*/
PRIVATE void write_descheader (treenode *const nptr, const wordnode *const nameptr, const BOOL short_names, const BOOL merge_types)
{
	int line_len = 0;

#if 0
fprintf (stderr, "objlib: write_descheader(): nptr = ");
printtreenl (stderr, 4, nptr);
fprintf (stderr, "objlib: write_descheader(): NTypeOf (nptr) = ");
printtreenl (stderr, 4, NTypeOf (nptr));
#endif
	if ((TagOf (nptr) == N_PROCDEF) || (TagOf (nptr) == N_LIBPROCDEF)) {	/* Write a PROC header */
		write_descstring ("PROC ", &line_len);
	} else if (TagOf (nptr) == N_MPROCDECL) {	/* MOBILE PROC header */
		write_descstring ("MOBILE PROC ", &line_len);
	} else {			/* Write a FUNCTION header */

		write_desctypelist (FnTypeListOf (NTypeOf (nptr)), &line_len);
		write_descstring (" FUNCTION ", &line_len);
	}
	write_descstring (nameptr == NULL ? WNameOf (NNameOf (nptr)) : WNameOf (nameptr), &line_len);
	write_descstring ("(", &line_len);
	/*{{{  write the parameters */
	{
		treenode *params = NParamListOf (nptr);
		treenode *lastparam = NULL;	/* initialised to shut up GCC's optimiser */
		int paramno = 1;

		/* Skip leading hidden parameters */
		while (!EndOfList (params) && !isnamedformal (ThisItem (params))) {
			params = NextItem (params);
		}

		/* Write first visible parameter, if any */
		if (!EndOfList (params)) {
			write_descparam (NULL, ThisItem (params), 1, &line_len, short_names);
			lastparam = ThisItem (params);
			params = NextItem (params);
		}

		/* Write subsequent visible parameters, if any */
		for (; !EndOfList (params); params = NextItem (params))
		{
			/*{{{  write this parameter, move to next */
			treenode *thisparam = ThisItem (params);

			if (isnamedformal (thisparam)) {
				int old_len;
				paramno++;
				write_descstring (",", &line_len);
				old_len = line_len;
				if (!merge_types) {
					lastparam = NULL;
				}
				write_descparam (lastparam, thisparam, paramno, &line_len, short_names);
				if (line_len > DESC_LINE_LENGTH) {
					/* desc_insert_nl(old_len, &line_len); */
					desc_buffer_ptr -= (line_len - old_len);
					line_len = old_len;
					write_descstring ("\n", &line_len);
					line_len = 0;
					write_descparam (lastparam, thisparam, paramno, &line_len, short_names);
				}
				lastparam = thisparam;
			}
			/*}}} */
		}
	}
	/*}}} */
	if (TagOf (nptr) == N_MPROCDECL) {
		write_descstring (") IMPLEMENTS ", &line_len);
		write_descstring (WNameOf (NNameOf (DExtraOf (NDeclOf (nptr)))), &line_len);
		/* also output typehash of this.. */
		write_desctypehash (DExtraOf (NDeclOf (nptr)), &line_len);
	} else {
		write_descstring (")", &line_len);
	}
	if (NPForksOf (nptr)) {
		write_descstring (" FORK", &line_len);
	}
	if (NPSuspendsOf (nptr)) {
		write_descstring (" SUSPEND", &line_len);
	}
	write_descstring ("\n", &line_len);
}

/*}}}*/
/*{{{  PRIVATE void write_paramusage(nptr)*/
PRIVATE void write_paramusage (fe_handle_t *const fe_handle, treenode *const nptr, int *const line_len, const BOOL add_spaces)
{
	treenode *params = NParamListOf (nptr);
	for (; !EndOfList (params); params = NextItem (params))
		/*{{{  write usage for this param */
	{
		treenode *const thisparam = ThisItem (params);
		if (TagOf (thisparam) == N_PARAM) {
			treenode *type = NTypeOf (thisparam);
			while (TagOf (type) == S_ARRAY)
				type = ARTypeOf (type);
			if (TagOf (type) == S_CHAN || TagOf (type) == S_PORT) {
				/* bug INSdi01977 07/04/93: pass in correct fe_handle! */
				if (fe_paraminputoroutputon (fe_handle, thisparam, TRUE)) {
					if (add_spaces)
						write_descstring ("    ", line_len);
					write_descstring (WNameOf (NNameOf (thisparam)), line_len);
					write_descstring ("?\n", line_len);
				}
				/* bug INSdi01977 07/04/93: pass in correct fe_handle! */
				if (fe_paraminputoroutputon (fe_handle, thisparam, FALSE)) {
					if (add_spaces)
						write_descstring ("    ", line_len);
					write_descstring (WNameOf (NNameOf (thisparam)), line_len);
					write_descstring ("!\n", line_len);
				}
			}
		}
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void write_desc_attr (const BOOL attribute, const int tag, BOOL *const first, int *const line_len)*/
PRIVATE void write_desc_attr (const BOOL attribute, const int tag, BOOL *const first, int *const line_len)
{
	if (attribute) {
		write_descstring ((*first) ? "--" : " ", line_len);
		write_descstring (tagstring (tag), line_len);
		*first = FALSE;
	}
}

/*}}}*/

/*{{{  PUBLIC const char *create_descriptor_string*/
PUBLIC const char *create_descriptor_string (fe_handle_t *const fe_handle,
			  treenode *const nptr, const wordnode *const nameptr,
			  const BOOL short_names, const BOOL merge_types, const BOOL add_spaces)
{
	BOOL first_attr = TRUE;
	int line_len = 0;
	desc_buffer_ptr = 0;

	if (desc_buffer == NULL) {
		desc_buffer = memalloc (desc_buffer_size);
	}

#ifdef COMPILING_TO_JCODE
	desc_buffer[desc_buffer_ptr++] = '\"';
#endif
	write_descheader (nptr, nameptr, short_names, merge_types);
/*write_descstring(NPNestedPriParOf(nptr) ? "  PRI PAR\n" : "  SEQ\n", &line_len);*/
	if (add_spaces) {
		write_descstring ("  ", &line_len);
	}
	write_descstring (tagstring (NPNestedPriParOf (nptr) ? S_PRIPAR : S_SEQ), &line_len);
#ifdef COMPILING_TO_JCODE
	write_descstring ("\\n", &line_len);
#else
	write_descstring ("\n", &line_len);
#endif
	write_paramusage (fe_handle, nptr, &line_len, add_spaces);
	write_descstring (":", &line_len);
#ifdef COMPILING_TO_JCODE
	desc_buffer[desc_buffer_ptr++] = '\"';
#endif
/*write_descstring("\nblurgle", &line_len);*/

	/* bug TS/1983 17/12/92 */
	write_desc_attr (NPNestedTimerOf (nptr), S_TIMER, &first_attr, &line_len);
	write_desc_attr (NPNestedPlaceOf (nptr), S_PLACE, &first_attr, &line_len);
	write_desc_attr (NPNestedPortOf (nptr), S_PORT, &first_attr, &line_len);

	desc_buffer[desc_buffer_ptr] = '\0';
	return desc_buffer;
}

/*}}}*/

/*}}}*/

/*{{{  PUBLIC BIT32 origin_num_from_origin_str*/
PUBLIC BIT32 origin_num_from_origin_str (const wordnode *const string)
{
	/* origin string looks like:  "filename:hexnum" */

	const char *s;
	const char *ss;
	if (string == NULL)
		return 0;

	s = WNameOf (string);

	/* search backwards for colon */
	ss = strrchr (s, ':');
	if (ss == NULL)
		ss = s;
	else
		ss++;		/* skip past the colon */

	return strtol (ss, (char **) NULL, 16);
}

/*}}}*/


