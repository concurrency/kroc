/* $Id: bind1.c,v 1.5 1997/09/16 15:58:37 djb1 Exp $ */

/*
 *	workspace allocation
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

/*
 * MDP: contains alpha-specific code to force quad word alignment of all workspaces
 */

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include "includes.h"
#include "extlib.h"		/* IMPORTED */

#include "generror.h"
#include "genhdr.h"
#include "instruct.h"		/* for mapping ASM */
#include "instdef.h"
#include "objlib.h"
#include "predefhd.h"
#include "trandef.h"
#include "bind1def.h"
#include "bind2def.h"
#include "bind3def.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen8def.h"
#include "gen9def.h"
#include "gen10def.h"
#include "gen11def.h"
#include "gen12def.h"
#include "code1def.h"
#include "objwrt.h"		/* for #PRAGMA LINKAGE */
#include "profile.h"		/* profiling info */
/*}}}*/

/*{{{  constants*/
#define VAR_USAGE_PER_LOOP ((INT32)8)
/* Average number of times loops are executed; see Roger about this value! */

/* This is used to optimise VALOFs which expand into constants. */
#define OPTIMISE_VALOFS TRUE
/*}}}*/

/*{{{  public variables*/
/* Global variables used by mapvariant */
PUBLIC int globchannelmode;
PUBLIC treenode **globchannelptr;
PUBLIC treenode *constantnptr = NULL;
PUBLIC treenode *consttablechain;
PUBLIC treenode *templist;

/* pointer to a set of parameters for trans, which must be valid during
   the mapping phase. This is initialised by the harness to point to
   something sensible.
*/
PUBLIC trans_params_t *be_trans_params;

extern ProfTab *profile_table;	/* in profile.c */
/*}}}*/

/*{{{  PRIVATE variables*/
typedef struct stdlibentry_s {
	struct stdlibentry_s *l_next;
	treenode *l_name;
} stdlibentry_t;

PRIVATE stdlibentry_t *stdlibentries = NULL;
PRIVATE BOOL stdlibsloaded = FALSE;

PRIVATE stdlibentry_t *vlibentries = NULL;
PRIVATE BOOL vlibsloaded = FALSE;

PRIVATE treenode *namelist;
PRIVATE treenode *inside_suspends = NULL;

typedef struct wsmapreserve_s {
	struct wsmapreserve_s *next;
	int offset;
	int size;
	int type;
} wsmapreserve_t;

PRIVATE wsmapreserve_t *reserved_wsmaps = NULL;		/* built up from I_WSMAP pseudo-ops */

/* PRIVATE treenode *scopelist;*//* Not used */
/*}}}*/
/*{{{  forward declarations*/
PRIVATE void mapnestedblocks (treenode * tptr);

/*}}}*/

/*{{{  std library loading*/
/*{{{  PRIVATE treenode *maplibtree*/
PRIVATE treenode *maplibtree (treenode * libtree)
{
	if (libtree != NULL) {
		const BOOL olddisassemble = disassemble;
		const BOOL oldassembly_output = assembly_output;
		const BOOL olddiagnostics = diagnostics;

		disassemble = FALSE;
		assembly_output = FALSE;
		diagnostics = FALSE;
		transmain (be_trans_params, &libtree);
		mapnestedblocks (libtree);

		genlocn = NOPOSN;

		disassemble = olddisassemble;
		assembly_output = oldassembly_output;
		diagnostics = olddiagnostics;
	}
	return libtree;
}

/*}}}*/
/*{{{  PRIVATE treenode *getstdlib*/
PRIVATE treenode *getstdlib (wordnode * const libcallstr, const SOURCEPOSN locn,
	   const BOOL enabled, BOOL * const loaded, const char *const filename, stdlibentry_t ** libentries)
/*****************************************************************************
 *
 *  getstdlib returns a pointer to the name node representing extended type
 *           function call 'libcallstr'.
 * Used to load both the standard library, and the virtual i/o library
 *
 *****************************************************************************/
{
	stdlibentry_t *entry;
	if (!enabled) {
		genlocn = locn;
		generr_s (GEN_STDLIBS_NOT_ENABLED, WNameOf (libcallstr));
		/* This is a fatal error, so never returns */
	}
	if (!(*loaded))
		/*{{{  load in standard libraries, set up libentries */
	{
		treenode *libtree;
		treenode *s;
		if (information)
			fprintf (outfile, "Loading standard library \"%s\"\n", filename);
		libtree = maplibtree (loadstdlib (filename));
		for (s = libtree; s != NULL && TagOf (s) != S_END; s = DBodyOf (s)) {
			stdlibentry_t *this = (stdlibentry_t *) newvec (sizeof (stdlibentry_t));
			this->l_next = *libentries;
			this->l_name = DNameOf (s);
			*libentries = this;
		}
		*loaded = TRUE;
	}
	/*}}} */

#if 0
	printf ("getstdlib: ");
	for (entry = *libentries; entry != NULL; entry = entry->l_next)
		printf ("%s, ", WNameOf (NNameOf (entry->l_name)));
	printf ("\n");
#endif

	entry = *libentries;
	while ((entry != NULL) && (NNameOf (entry->l_name) != libcallstr))
		entry = entry->l_next;

	if (entry == NULL) {
		genlocn = locn;
		generr_s (GEN_MISSING_STDLIB_ENTRY, WNameOf (libcallstr));
		/* this is a fatal error */
	}

	return (entry->l_name);
}

/*}}}*/
/*{{{  PUBLIC treenode *vlibentry*/
/*****************************************************************************
 *
 *  vlibentry returns a pointer to the name node representing extended type
 *           function call 'libcallstr'.
 *
 *****************************************************************************/
PUBLIC treenode *vlibentry (wordnode * const libcallstr)
{
	return getstdlib (libcallstr, genlocn, vlibsenabled, &vlibsloaded, vlibsfilename, &vlibentries);
}

/*}}}*/
/*{{{  PUBLIC treenode * libentry*/
/*****************************************************************************
 *
 *  libentry returns a pointer to the name node representing extended type
 *           function call 'libcallstr'.
 *
 *****************************************************************************/
PUBLIC treenode *libentry (wordnode * const libcallstr, const SOURCEPOSN locn)
{
	return getstdlib (libcallstr, locn, stdlibsenabled, &stdlibsloaded, extlibfilename, &stdlibentries);
}

/*}}}*/
/*}}}*/
/*{{{  static void clear_reservedwsmaps (void)*/
/*
 *	clears out any reserved workspace maps
 */
static void clear_reservedwsmaps (void)
{
	while (reserved_wsmaps) {
		wsmapreserve_t *next = reserved_wsmaps->next;

		freevec ((void *)reserved_wsmaps, sizeof (wsmapreserve_t));
		reserved_wsmaps = next;
	}
	return;
}
/*}}}*/
/*{{{  static void add_reservedwsmap (int offset, int size, int type)*/
/*
 *	creates and adds a new reserved workspace map entry
 *	(called during the map proper, workspace not mapped until after allocation)
 */
static void add_reservedwsmap (int offset, int size, int type)
{
	wsmapreserve_t *wsmr = (wsmapreserve_t *)newvec (sizeof (wsmapreserve_t));

#if 0
fprintf (stderr, "bind1: add_reservedwsmap(): offset=%d, size=%d, type=%d\n", offset, size, type);
#endif
	wsmr->next = reserved_wsmaps;
	wsmr->offset = offset;
	wsmr->size = size;
	wsmr->type = type;

	reserved_wsmaps = wsmr;
	return;
}
/*}}}*/
/*{{{  PRIVATE treenode *transformalt(spec, alt)*/
/*****************************************************************************
 *
 *  transformalt takes a list of specifications, spec, preceding an
 *               ALTERNATIVE, alt, and attempts to move some or all of the
 *               specifications inside the alt input.  The updated
 *               tree is returned.
 *               N.B. The specification list is terminated by alt.
 *
 *****************************************************************************/
PRIVATE treenode *transformalt (treenode * spec, treenode * const alt)
{
	treenode *result = spec;	/* assume we return what we were given unless we */
	/* find out otherwise */

	if (spec != alt)
	 {			/* There are specifications */
		/*{{{  see if we can move this specification to the input */
		treenode *n = DNameOf (spec);
		SetDBody (spec, transformalt (DBodyOf (spec), alt));
		switch (TagOf (n)) {
		case N_ABBR:
		case N_VALABBR:
		case N_RETYPE:
		case N_VALRETYPE:
		case N_DECL:
			/*{{{  see if it can be moved */
			{
				BOOL moveable = !usedin (n, AltGuardOf (alt), be_lexlevel);
				/*{{{  look at other specs */
				{
					treenode *next;
					for (next = DBodyOf (spec); moveable && (next != alt); next = DBodyOf (next)) {	/* check if this spec is used in a following spec */
						switch (TagOf (DNameOf (next))) {
						default:
							break;
						case N_DECL:	/* bug TS/1263 11/05/92 */
							if (DValOf (next) != NULL)
								moveable = FALSE;
							break;
						case N_ABBR:
						case N_VALABBR:
						case N_RETYPE:
						case N_VALRETYPE:
							moveable = !usedin (n, DValOf (next), be_lexlevel);
							break;
						}
					}
				}
				/*}}} */
				/*{{{  look at Channel expression and Time expression */
				moveable = moveable && !usedin (n, AltChanExpOf (alt), be_lexlevel)	/* channel/timer */
					&&!usedin (n, AltTimeExpOf (alt), be_lexlevel);	/* time */
				/*}}} */
				if (moveable)
					/*{{{  transform */
				{
					DEBUG_MSG (("transformalt: moving %s\n", WNameOf (NNameOf (n))));
					result = DBodyOf (spec);
					SetDBody (spec, AltBodyOf (alt));
					SetAltBody (alt, spec);
				}
				/*}}} */
			}
			/*}}} */
			break;
		default:
			break;
		}
	}
	/*}}} */
	return (result);
}

/*}}}*/
/*{{{  PRIVATE int addstaticandvec(nptr)*/
PRIVATE int addstaticandvec (treenode *const nptr, const INT32 vs)
{
	treenode *fparams = NParamListOf (nptr);
	int nparams = 0;
	const int local_lexlevel = NLexLevelOf (nptr);

	if (TagOf (nptr) == N_LIBPROCDEF) {
		const char *name = WNameOf (NNameOf (nptr));
		if ((strlen (name) >= 5) && (!strncmp (name, "CIF.", 4))) {
			wordnode *const name = lookupword ("$ws.size", 8);
			treenode *const wsnode = newnamenode (S_PARAM_WS, NOPOSN, name, newintconstant (0, S_INT), NULL, local_lexlevel + 1, 0, NM_DEFAULT);
			fparams = addtofront (wsnode, fparams);
		}
	}

	if ((vs != 0) || (TagOf (nptr) == N_MPROCDECL) || (TagOf (nptr) == N_LIBMPROCDECL)) {
		/*{{{  add the vector space pointer */
		/*treenode *vsnode = newleafnode(S_PARAM_VSP, NOPOSN); */
		/* bug 1141 4/2/91 - make sure VSP is always a hiddenparamnode */
		wordnode *const name = lookupword ("$VSP", 4);
		treenode *const vsnode = newnamenode (S_PARAM_VSP, NOPOSN, name, NULL, NULL, local_lexlevel + 1, 0, NM_DEFAULT);
		SetNVVSOffset (vsnode, -1);
		if ((local_lexlevel == 0) || separatelycompiled (nptr)) {
			/* At the outer level, put the vector space pointer at the end
			   of the parameters, to preserve compatibility with existing
			   toolset SCs and libraries. */
			fparams = appendnode (vsnode, fparams);
		} else {
			fparams = addtofront (vsnode, fparams);
		}
		/*}}} */
	}

	if (NPSLUsageOf (nptr) != STATICLINK_NOT_USED) {	/* set up ok for libraries */
		wordnode *const name = lookupword ("$SL", 3);
		treenode *const sl = newnamenode (S_PARAM_STATICLINK, NOPOSN, name, NULL,
						  NULL, local_lexlevel + 1, 0, NM_DEFAULT);
		fparams = addtofront (sl, fparams);
	}

	/* Update param list on definition */
	SetNParamList (nptr, fparams);

	/* Count Number of parameters */
	for (; !EndOfList (fparams); fparams = NextItem (fparams)) {
		/* Don't count TIMER parameters */
		if (basetype (gettype (ThisItem (fparams))) != S_TIMER) {
			nparams++;
		}
	}

	return (nparams);
}

/*}}}*/
#ifdef MOBILES
/*{{{  PRIVATE int addstaticandvecandmob (treenode *const nptr, const INT32 vs, const INT32 ms)*/
/*
 *	adds (hidden) mobile-space parameter, calls addstaticandvec to set the others
 */
PRIVATE int addstaticandvecandmob (treenode *const nptr, const INT32 vs, const INT32 ms)
{
	treenode *fparams;
	int nparams = 0;
	const int local_lexlevel = NLexLevelOf (nptr);

	nparams = addstaticandvec (nptr, vs);

#if 0
fprintf (stderr, "addstaticandvecandmob: vs = %d, ms = %d, nparams = %d, nptr =", vs, ms, nparams);
printtreenl (stderr, 4, nptr);
#endif
#ifdef MOBILES
	fparams = NParamListOf (nptr);
	if ((ms != 0) || (TagOf (nptr) == N_MPROCDECL) || (TagOf (nptr) == N_LIBMPROCDECL)) {
		/* add mobile-space pointer */
		wordnode *const name = lookupword ("$MSP", 4);
		treenode *const msnode = newnamenode (S_PARAM_MSP, NOPOSN, name, NULL, NULL, local_lexlevel + 1, 0, NM_DEFAULT);
		SetNVMSOffset (msnode, -1);
		/* at outer-level or if a seperatly-compiled chunk, put pointer at end of parameters */
		if ((local_lexlevel == 0) || separatelycompiled (nptr)) {
			fparams = appendnode (msnode, fparams);
		} else {
			fparams = addtofront (msnode, fparams);
		}
		nparams++;
	}
#endif /* MOBILES */

	/* maybe add FORK barrier too */
	if (NPForksOf (nptr)) {
		wordnode *const name = lookupword ("$fork.barrier", 13);
		treenode *const fbnode = newnamenode (S_PARAM_FB, NOPOSN, name, newleafnode (S_BARRIER, NOPOSN), NULL, local_lexlevel + 1, 0, NM_DEFAULT);

		/* at outer-level or if a seperatly-compiled chunk, put pointer at end of parameters */
		if ((local_lexlevel == 0) || separatelycompiled (nptr)) {
			fparams = appendnode (fbnode, fparams);
		} else {
			fparams = addtofront (fbnode, fparams);
		}
		nparams++;
	}

	SetNParamList (nptr, fparams);
#if 0
fprintf (stderr, "addstaticandvecandmob: nparams = %d, NParamListOf (nptr) = ", nparams);
printtreenl (stderr, 4, NParamListOf (nptr));
#endif
	return nparams;
}
/*}}}*/
#endif /* MOBILES */
/*{{{  PUBLIC BOOL is_indirect_name (treenode *nptr)*/
/*
 *	this tests to see whether a name is considered `indirect'.
 *	Used principally by mobile processes
 */
PUBLIC BOOL is_indirect_name (treenode *nptr)
{
	switch (TagOf (nptr)) {
	case N_PARAM:
	case N_ABBR:
		return NVIndirectOf (nptr);
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE BOOL need_mapping (treenode *nptr)*/
/*
 *	returns TRUE if the definition of "nptr" needs workspace mapping
 */
PRIVATE BOOL need_mapping (treenode *nptr)
{
	if (NPSuspendsOf (nptr)) {
		return TRUE;
	}
	if (map_all_procs && (TagOf (nptr) == N_PROCDEF)) {
		return TRUE;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE void mapname (treenode *nptr)*/
/*****************************************************************************
 *
 *  mapname gives a workspace allocation number to a name node, and reserves
 *          an appropriate number of workspace slots for that name.
 *          Sets the name mode, if not already set.
 *
 *****************************************************************************/
PRIVATE void mapname (treenode *nptr)
{
	switch (TagOf (nptr)) {
		/*{{{  N_DECL */
	case N_DECL:
		if ((nodetypeoftag (TagOf (NTypeOf (nptr))) == TYPENODE) && (TypeAttrOf (NTypeOf (nptr)) & TypeAttr_placed)) {
			/*{{{  non-constant placement, allocate single word to store the address */
			create_var (nptr);
			/*}}}  */
		} else if (isplaced (nptr)) {
			/*{{{  allocate absolutely */
			const BIT32 placeval = NVOffsetOf (nptr);
			int error;
			BIT32 plhi, pllo;
			const BIT32 mostneg = (targetintsize == S_INT32) ? MOSTNEG_INT32 : MOSTNEG_INT16;

			I32ToI64 (&plhi, &pllo, placeval);
			DEBUG_MSG (("mapname: PLACED at hi:%lu, lo%lu\n", plhi, pllo));

			/*{{{  */
			if ((plhi == 0) && (pllo < tx_global.memstart)) {
				int mess_code = GEN_PLACE_BELOW_MEMSTART;

				if (pllo < (BIT32) (NO_OF_LINKS * 2)) {
					if (tx_global.hseries) {
						mess_code = GEN_H_PLACE_ON_LINKS;
					} else if (iobycall) {
						mess_code = GEN_T_PLACE_ON_LINKS;
					}
				}

				if (memo_err (GEN, mess_code, genlocn)) {
					/* bug TS/2057 22/01/93 */
					genwarn_s (mess_code, WNameOf (NNameOf (nptr)));
				}
			}
			/*}}} */

			Int64Mul (&error, &plhi, &pllo, plhi, pllo, 0, bytesperword);
			Int64Add (&error, &plhi, &pllo, plhi, pllo, 0xffffffffl, mostneg);

			if (checkbounds (S_INT64, targetintsize, plhi, pllo, genlocn) != 0) {
				if (memo_err (GEN, GEN_PLACE_OVERFLOW, genlocn)) {
					/*  bug TS/2057 22/01/93 */
					genwarn_s (GEN_PLACE_OVERFLOW, WNameOf (NNameOf (nptr)));
				}
			}

			SetNVOffset (nptr, pllo);
			/*}}} */
		} else {
			/*{{{  in workspace or vectorspace */
			/* note (frmb): MOBILES (NM_POINTER) get allocated a single word in the workspace,
			 * dynamic MOBILES get allocated 1 + dimension-count words */
			if (iswsplaced (nptr)) {
				/* allocate at fixed point in workspace */
				alloc_fixedws (NVOffsetOf (nptr), allocsize (nptr));
			} else {
				create_var (nptr);
			}
			create_var_subsidiary (nptr);	/* create the vectorspace and initialiser */
			/*}}} */
		}
		break;
		/*}}} */
		/*{{{  N_REPL */
	case N_REPL:
		/* Replicator occupies two words, one for base, one for count */
		create_var (nptr);
		break;
		/*}}} */
	default:
		badtag (genlocn, TagOf (nptr), "mapname");
	}
}

/*}}}*/
/*{{{  PRIVATE void mapnameout (treenode *nptr)*/
/*
 *	maps descoping for a single name
 */
PRIVATE void mapnameout (treenode *nptr)
{
	if (isdynmobilechantype (nptr)) {
		mapcondfreedynchantype (&nptr);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void mapsingledecl (treenode *tptr)*/
PRIVATE void mapsingledecl (treenode *tptr)
{
	/*{{{  debugging */
#ifdef DEBUG
	{
		treenode *name = DNameOf (tptr);
		if (TagOf (name) == S_LIST)
			name = ThisItem (name);
		DEBUG_MSG (("mapsingledecl: mapping %s\n", WNameOf (NNameOf (name))));
	}
#endif
	/*}}} */
	switch (TagOf (tptr)) {
		/*{{{  S_DECL */
	case S_DECL:
		/*{{{  map each of the variables in the declaration list */
		/* Map each of the variables in the declaration list */
		{
			treenode *t = DNameOf (tptr);
			if (TagOf (t) == S_LIST) {
				for (; !EndOfList (t); t = NextItem (t)) {
					mapname (ThisItem (t));
				}
			} else {
				mapname (t);
			}
			if (DValOf (tptr) != NULL) {
				mapprocess (DValOf (tptr));	/* bug TS/1263 11/05/92 */
			}
		}
		/*}}} */
		break;
		/*}}} */
		/*{{{  S_VALABBR S_ABBR S_VALRETYPE S_RETYPE */
	case S_VALABBR:
	case S_ABBR:
	case S_VALRETYPE:
	case S_RETYPE:
		{
			treenode *nptr = DNameOf (tptr);
			const BOOL retype = (TagOf (nptr) == N_VALRETYPE) || (TagOf (nptr) == N_RETYPE);
			treenode *lhstype = NTypeOf (nptr);
			treenode *rhs = DValOf (tptr);
			treenode *rhstype = gettype (rhs);
			const abbrevmode_t am = be_abbrevmode (tptr);
			int type = ntypeof (nptr);

			switch (am) {
				/*{{{  AM_CONST: no space allocated, no offset */
			case AM_CONST:
				DEBUG_MSG (("AM_CONST\n"));
				SetNVOffset (nptr, NO_SLOT);
				break;
				/*}}} */
				/*{{{  AM_ISRHS: no space allocated, set offset to offset of rhs */
			case AM_ISRHS:
				{
					/* Abbreviation inherits attributes of rhs */
					/* rhs is either a namenode, or an ARRAYITEM node with constant offset */
					/* Note that we don't do this if rhs is a segment */
					treenode *const rhsname = nameof (rhs);
					BOOL isnptr_arraytype = TagOf (NTypeOf (nptr)) == S_ARRAY;
#ifdef OCCAM2_5
					SetNVOffset (nptr, (TagOf (rhs) == S_ARRAYITEM) ? ASOffsetOf (rhs)
						     : (TagOf (rhs) == S_RECORDITEM) ? ASOffsetOf (rhs)
						     : (TagOf (rhs) == S_SEGMENTITEM) ?
						     (isnptr_arraytype ? SOffsetOf (rhs) : SOffsetOf (rhs) / bytesperword) : 0);
#else
					SetNVOffset (nptr, (TagOf (rhs) == S_ARRAYITEM) ? ASOffsetOf (rhs)
						     : (TagOf (rhs) == S_SEGMENTITEM) ?
						     (isnptr_arraytype ? SOffsetOf (rhs) : SOffsetOf (rhs) / bytesperword) : 0);
#endif

					/* Bug 1135 31/1/91 if rhs is also an abbreviation, we must add in
					   the rhs's NVOffset.
					 */
					/* Ditto if rhsname is PLACE'd AT WORKSPACE n, so nptr must be at
					   n + possible array offset */
					if (NVOffsetOf (rhsname) != NO_SLOT) {
						DEBUG_MSG (("mapsingledecl: adding rhs offset %ld to is ISRHS abbreviation %s\n",
							    NVOffsetOf (rhsname), WNameOf (NNameOf (nptr))));
						SetNVOffset (nptr, NVOffsetOf (nptr) + NVOffsetOf (rhsname));
					}

					if (!iswsplaced (rhsname)) {
						/* Chain this name onto rhsname: when rhsname is allocated, nptr will
						   be at the same place + possible array offset */
						addnamechain (rhsname, nptr);	/* Put the abbrev onto name chain of rhs */
					}
				}
				break;
				/*}}} */
				/*{{{  AM_PTR:   allocate space for a pointer */
			case AM_PTR:
#ifdef MOBILES
				if ((TagOf (lhstype) == S_MOBILE) && (TagOf (rhstype) == S_MOBILE) && isdynmobilearray (nptr)) {
					/* allocate space for a dynamic mobile array (1 + num-dimensions) */
				}
#endif
				create_var (nptr);
				mapexpopd (P_PTR, DValAddr (tptr));	/* Map the loading of a pointer */
				break;
				/*}}} */
				/*{{{  (AM_VAL)  allocate in workspace or vectorspace */
			default:
				{
					treenode *const lhstype = NTypeOf (nptr);
					const INT32 w = allocsize (nptr);
					if (w == (-1)) {
						geninternal_s (GEN_UNSIZED_ABBREV, WNameOf (NNameOf (nptr)));
					}
					create_var (nptr);
					if (isinvectorspace (nptr)) {
						/* Can never happen cos constructors are currently put in WS */
						SetNVVSOffset (nptr, newvs (w));
					}
					if (retype) {
						/* Pretend the lhs has the same type as the rhs so the mapping works
						   correctly. */
						treenode *const rhstype = gettype (rhs);
						type = TagOf (rhstype);
						SetNType (nptr, rhstype);
					}
					/*{{{  map the initialisation */
					mapsimpleassign (type, P_EXP, DNameAddr (tptr), P_EXP, DValAddr (tptr));
					/*}}} */
					SetNType (nptr, lhstype);
				}
				break;
				/*}}} */
			}
			DEBUG_MSG (("mapsingledecl: %s : be_abbrevmode is %d, mode is %d\n", WNameOf (NNameOf (nptr)), am, NModeOf (nptr)));
		}
		break;
		/*}}} */
		/*{{{  S_PROCDEF S_SFUNCDEF S_LFUNCDEF S_MPROCDECL */
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
#ifdef MOBILES
	case S_MPROCDECL:
#endif
		/* Workspace allocated already */
		break;
		/*}}} */
		/*{{{  S_TPROTDEF S_SPROTDEF S_WSPLACE S_VSPLACE */
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_WSPLACE:
	case S_VSPLACE:
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#if MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
#endif
		break;
		/*}}}  */
		/*{{{  S_PLACE special if we need to map as an assignment */
	case S_PLACE:
		{
			treenode *nptr = DNameOf (tptr);

			if ((nodetypeoftag (TagOf (NTypeOf (nptr))) == TYPENODE) && (TypeAttrOf (NTypeOf (nptr)) & TypeAttr_placed)) {
				/* map the initialisation */
				mapsimpleassign (S_INT, P_EXP, DNameAddr (tptr), P_EXP, DValAddr (tptr));
			}
		}
		break;
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "mapsingledecl");
	}
}

/*}}}*/
/*{{{  PRIVATE void mapsingledeclout (treenode *tptr)*/
/*
 *	maps descoping for a single declaration
 */
PRIVATE void mapsingledeclout (treenode *tptr)
{
	switch (TagOf (tptr)) {
		/*{{{  S_DECL */
	case S_DECL:
		{
			treenode *t = DNameOf (tptr);

			if (TagOf (t) == S_LIST) {
				for (; !EndOfList (t); t = NextItem (t)) {
					mapnameout (ThisItem (t));
				}
			} else {
				mapnameout (t);
			}
		}
		break;
		/*}}}*/
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void mapdeclandbody(tptr, mbody)*/
PUBLIC void mapdeclandbody (treenode * tptr, void (*mbody) (treenode *), BOOL preprocess_first, BOOL preprocess_rest)
{
	const BIT32 savevsp = vsp;
	treenode *const save_vsp_nptr = enclosing_vsp_nptr;
	int *const savelive_var = save_scope ();
	BOOL not_first_time = FALSE;
	treenode *orig_tptr = tptr;

	DEBUG_MSG (("mapdeclandbody: saving scope as: %lX\n", (long) savelive_var));
	while (isspecification (tptr)) {
		genlocn = LocnOf (tptr);
		if (preprocess_rest && (not_first_time || preprocess_first)) {
			mappreprocess (tptr);	/* bug 776 5/10/90 */
		}
		not_first_time = TRUE;
		mapsingledecl (tptr);	/* Allocate for this declaration */
		tptr = DBodyOf (tptr);
	}
	genlocn = LocnOf (tptr);
	(*mbody) (tptr);

	/* map out declarations for any special descoping */
	tptr = orig_tptr;
	while (isspecification (tptr)) {
		genlocn = LocnOf (tptr);
		mapsingledeclout (tptr);
		tptr = DBodyOf (tptr);
	}

	DEBUG_MSG (("mapdeclandbody: restoring scope to: %lX\n", (long) savelive_var));
	restore_scope (savelive_var);
	vsp = savevsp;
	enclosing_vsp_nptr = save_vsp_nptr;
}

/*}}}*/
/*{{{  PUBLIC void addtemplist(tptr)*/
PUBLIC void addtemplist (treenode * tptr)
{
	DEBUG_MSG (("addtemplist: adding temporary %lX\n", (long) tptr));
	SetNVNextTemp (tptr, templist);
	templist = tptr;
}

/*}}}*/
/*{{{  PUBLIC void freetemplist()*/
PUBLIC void freetemplist (void)
{
	treenode *tptr;
	for (tptr = templist; tptr != NULL; tptr = NVNextTempOf (tptr)) {
		DEBUG_MSG (("freetemplist: freeing temporary %lX\n", (long) tptr));
		if (TagOf (tptr) == T_PREEVALTEMP)
			SetTag (tptr, T_TEMP);	/* removed for bug TS/1505 28/11/91 */
		freetemp (tptr);
	}
	templist = NULL;
}

/*}}}*/
/*{{{  PUBLIC void mapconstruction(tptr, mapproc)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapconstruction maps out the workspace for a list of processes
 *                  which are NOT performed in parallel, ie. a list of
 *                  processes in sequence, a list of choices, alternatives
 *                  or selections.  'mapproc' is called to map each item
 *                  of the list.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void mapconstruction (treenode * tptr, void (*mapproc) (treenode *))
{
	for (; !EndOfList (tptr); tptr = NextItem (tptr)) {
		(*mapproc)(ThisItem (tptr));
	}
}

/*}}}*/
/*{{{  PUBLIC void maprepl(tptr, mbody)*/
/*****************************************************************************
 *
 *  maprepl maps a replicator node, tptr: the replicator body is mapped
 *          using the function parameter mbody.
 *
 *****************************************************************************/
PUBLIC void maprepl (treenode * tptr, void (*mbody) (treenode *))
{
	INT32 oldweight = loop_weight;
	mapname (ReplCNameOf (tptr));
	mapexp (ReplCStartExpAddr (tptr));
	mapexp (ReplCLengthExpAddr (tptr));
	if (ReplCStepExpOf (tptr)) {
		mapexp (ReplCStepExpAddr (tptr));
	}
	if (isconstexpnd (ReplCLengthExpOf (tptr)))
		uploop_weight (LoValOf (ReplCLengthExpOf (tptr)));
	else
		uploop_weight (VAR_USAGE_PER_LOOP);
	/* We have to access the loop variable to test for end of loop */
	setusecount (ReplCNameOf (tptr), loop_weight);
	(*mbody) (ReplCBodyOf (tptr));
	loop_weight = oldweight;
	kill_var (ReplCNameOf (tptr));
}

/*}}}*/
/*{{{  PRIVATE void set_constantnptr*/
PRIVATE void set_constantnptr (void)
{
	treenode *cptr = consttablechain;
	while ((cptr != NULL) && (TagOf (cptr) != S_CONSTEXP))
		cptr = CTNextOf (cptr);

	if ((cptr != NULL) && (NVUseCountOf (constantnptr) > 1))	/* bug TS/1745 11/08/92 */
		create_immortalvar (constantnptr);
	else
		SetNVOffset (constantnptr, CONSTANTPOINTER_NOT_USED);

	/*printf("set_constantnptr: usecount is: %ld\n", NVUseCountOf(constantnptr)); */
}

/*}}}*/
/*{{{  PRIVATE void mapreplpar(tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapreplpar allocates workspace for a replicated PAR contruct, tptr.
 *             The space usage is placed in a spacenode inserted in front
 *             of the replicated process.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void mapreplpar (treenode * tptr)
{
	INT32 saveddatasize, savedvsp, savedmaxvsp;
	treenode *savedvsp_nptr;
	const BOOL replisconst = isconst (ReplCLengthExpOf (tptr));
	INT32 replcount;
	INT32 parspace, parvspace;
	INT32 maxwsp;
	INT32 savedweight;
	INT32 savedusecount;
	treenode *savednamelist;
	int savedvar_base;
	treenode *const replnptr = ReplCNameOf (tptr);
	treenode *const pprocess = ReplCBodyOf (tptr);
#ifdef MOBILES
	INT32 sub_msp = 0;
	INT32 after_par_setup_slots = (inside_suspends ? 10 : 0);
	void *saved_map_state = NULL;
#endif
	treenode *spptr = NULL;
	treenode *barrier = ReplCTempOf (tptr);
	treenode *declbars, *extbars;

	/*{{{  debug message */
	if (diagnostics) {
		fprintf (outfile, "\nAllocating workspace for replicated parallel process\n");
	}
	/*}}} */
	if (replisconst) {
		replcount = LoValOf (ReplCLengthExpOf (tptr));
	} else {
		replcount = 1;
	}
	/*{{{  map barrier if present*/
	if (barrier && (TagOf (barrier) == S_BAREXTEND)) {
		declbars = LeftOpOf (barrier);
		extbars = RightOpOf (barrier);
	} else if (barrier && (TagOf (barrier) == S_EXTENDS)) {
		declbars = NULL;
		extbars = barrier;
	} else if (barrier) {
		declbars = barrier;
		extbars = NULL;
	} else {
		declbars = extbars = NULL;
	}

	/* map extended barriers */
	if (extbars && (TagOf (extbars) == S_EXTENDS)) {
		if (TagOf (OpOf (extbars)) == S_LIST) {
			treenode *walk;

			for (walk = OpOf (extbars); !EndOfList (walk); walk = NextItem (walk)) {
				mapexp (ThisItemAddr (walk));
			}
		} else {
			mapexp (OpAddr (extbars));
		}
	} else if (extbars) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "mapreplpar: PAR EXTENDS");
	}

	/* map barrier names */
	if (declbars && (TagOf (declbars) == S_LIST)) {
		treenode *walk;

		for (walk = declbars; !EndOfList (walk); walk = NextItem (walk)) {
			mapname (ThisItem (walk));
		}
	} else if (declbars) {
		mapname (declbars);
	}
	/*}}}*/
	/*{{{  save old environment */
	saveddatasize = datasize;
	savedvsp = vsp;
	savedmaxvsp = maxvsp;
	savedvsp_nptr = enclosing_vsp_nptr;
	savedvar_base = var_base;
	savedweight = loop_weight;
	savedusecount = NVUseCountOf (constantnptr);
	savednamelist = namelist;
	/*}}} */
	/*{{{  initialise new environment */
	initvsp (0);		/* Initialise vector space usage to zero */
	datasize = DS_MIN;	/* Minimum below workspace usage         */
	if ((alloc_strategy & ALLOC_MAXBELOW) != 0) {
		datasize = DS_WAIT;
	}
	/* if (!replisconst) {
		datasize = DS_WAIT;
	} */
	be_lexlevel++;
	SetNLexLevel (replnptr, be_lexlevel);	/* bug TS/2066 29/01/93 */
	var_base = num_var;
	uploop_weight (replcount);
	setusecount (constantnptr, 0);
	namelist = NULL;
#ifdef MOBILES
	if (replisconst) {
		mobilereplparstart (replcount);
	} else {
		mobilereplparstart (-1);
	}
#endif
	#ifdef MOBILES
	if (inside_suspends) {
		saved_map_state = sp_saveallocvars (TRUE);
	}
	#endif
	/*}}} */
#if 0
fprintf (stderr, "mapreplpar: pprocess =");
printtreenl (stderr, 4, pprocess);
#endif
	/*{{{  map replicated process */
	{
		int rpspecials = MIN_REPLPAR_SPECIALS;
		mapprocess (pprocess);
		/* datasize contains our below workspace requirement for PAR branch */
		/* maxvsp gives the vector space usage for the PAR branch */
		/*{{{  check whether we need a constant pointer */
		set_constantnptr ();
		/*}}} */
		maxwsp = allocvars (var_base, 0, TRUE);

		if (maxvsp != 0) {
			rpspecials++;	/* Allocate a slot for the vsp */
		}
#ifdef MOBILES
#if 0
fprintf (stderr, "mapreplpar: done mapping replicated process.\n");
#endif
		if (mobilereplparnmapped () > 0) {
			rpspecials++;	/* allocate slot for msp */
			sub_msp = mobilereplparusage ();
#if 0
fprintf (stderr, "mapreplpar: mobilereplparnmaped() > 0, so allocating MSP slot. (sub_msp = %d)\n", sub_msp);
#endif
		}
#endif
		maxwsp = maxwsp + rpspecials;	/* Add special slots to maxwsp */
		if (needs_quadalign) {
			datasize = (datasize + 1) & (~1);	/* round up to even number */
			maxwsp = (maxwsp + 1) & (~1);	/* round up to even number */
		}
#ifdef MOBILES
		spptr = newspacenode (S_SPACEUSAGE, NOPOSN, pprocess,
				      maxwsp, maxwsp + datasize, maxvsp, savedvsp, sub_msp, -1, consttablechain, (int) NVOffsetOf (constantnptr));
		mobilereplparfinish (spptr);
#else
		spptr = newspacenode (S_SPACEUSAGE, NOPOSN, pprocess,
				      maxwsp, maxwsp + datasize, maxvsp, savedvsp, consttablechain, (int) NVOffsetOf (constantnptr));
#endif
		SetReplCBody (tptr, spptr);
		if (inside_suspends) {
			/* mark locations of hidden workspace entries -- relative to the replicated process */
#if 0
fprintf (stderr, "mapreplpar: marking hidden STATICLINK..\n");
#endif
			mark_hidden_var (S_PARAM_STATICLINK, (maxwsp - rpspecials) + REPLPAR_STATICLINK);
		}
	}
	/*}}} */
	/*{{{  debug message */
	if (diagnostics) {
		fprintf (outfile, "Workspace for replpar: maxwsp = %d, datasize = %d, maxvsp = %d\n\n", maxwsp, datasize, maxvsp);
	}
	/*}}} */
	/*{{{  restore & update environment */
	free_fixedwslist ();
	save_wsallocmap (spptr);
	#ifdef MOBILES
	if (inside_suspends && saved_map_state) {
		sp_restoreallocvars (saved_map_state);
		saved_map_state = NULL;
	}
	#endif
	parspace = ((maxwsp + datasize) * replcount) + DS_MIN;
	parvspace = maxvsp * replcount;
	datasize = max_INT32 (saveddatasize, parspace);
	datasize += after_par_setup_slots;			/* always need these */
	vsp = savedvsp;
	enclosing_vsp_nptr = savedvsp_nptr;
	maxvsp = max_INT32 (savedmaxvsp, vsp + parvspace);
	be_lexlevel--;
	SetNLexLevel (replnptr, be_lexlevel);	/* bug TS/2066 29/01/93 */
	num_var = var_base;
	var_base = savedvar_base;
	setusecount (constantnptr, savedusecount);
	loop_weight = savedweight;
	namelist = savednamelist;
	/*}}} */
	/*{{{  map replicator */
	mapname (ReplCNameOf (tptr));
	upusecount (ReplCNameOf (tptr), replcount);
	mapexp (ReplCStartExpAddr (tptr));
	mapexp (ReplCLengthExpAddr (tptr));
	if (ReplCStepExpOf (tptr)) {
		mapexp (ReplCStepExpAddr (tptr));
	}
	/*}}} */
#ifdef MOBILES
#if 0
		if (inside_suspends) {
			/* we perform an extra call of MPBARRESIGN() here -- better map it */
			treenode *resinst = ThisItem (NextItem (CBodyOf (pprocess)));

#if 0
fprintf (stderr, "call to MPBARRESIGN is:");
printtreenl (stderr, 4, resinst);
#endif
			if (TagOf (resinst) == S_PINSTANCE) {
				mapinstance (resinst);
			} else {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "mapreplpar");
			}
		}
#endif
#endif
	/*{{{  map slots used in setting up the replicated PAR */
	/* Map slots for the PAR join and count
	   temporary used to calculate replicated wptr's,
	   and possibly temporary used to calculate replicated vsp's */
	{
		int replparslots = MIN_REPLPAR_SLOTS;

		if (!replisconst) {
			replparslots += 2; /* allocate a slot for the ws array and a slot for acount */
		}
		if (parvspace != 0) {
			replparslots++; /* Allocate a slot for the vsp temp */
			if (!replisconst) {
				replparslots++; /* Allocate a slot for the vs array */
			}
		}
#ifdef MOBILES
		if (sub_msp != 0) {
			replparslots++; /* allocate one for msp temp */
			if (!replisconst) {
				replparslots += 2;	/* 2 temp slots for handling dynamic ms allocation */
			}
		}
#endif
		reservelowworkspace (replparslots);
	}
	/*}}} */
	/*{{{  descope the replicator */
	kill_var (ReplCNameOf (tptr));
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC int setup_asm_operands()*/
PUBLIC int setup_asm_operands (treenode * tptr, treenode ** operands[MAXREGS], int opmodes[MAXREGS])
{
	int i;
	int ops;
	for (ops = 0; !EndOfList (tptr); tptr = NextItem (tptr), ops++) {
		if (TagOf (ThisItem (tptr)) == S_ADDRESSOF) {
			opmodes[ops] = P_PTR;
			operands[ops] = OpAddr (ThisItem (tptr));
		} else {
			operands[ops] = ThisItemAddr (tptr);
			opmodes[ops] = P_EXP;
		}
	}

	/* These are generated in reverse order, so swap the list */
	for (i = 0; i < ops / 2; i++) {
		/* swap item i with item j (j is i'th from top) */
		const int j = (ops - 1) - i;
		const int opmode_temp = opmodes[i];
		treenode **const operand_temp = operands[i];
		opmodes[i] = opmodes[j];
		operands[i] = operands[j];
		opmodes[j] = opmode_temp;
		operands[j] = operand_temp;
	}

	return ops;
}

/*}}}*/
/*{{{  PRIVATE void mapguy_or_asm*/
PRIVATE void mapguy_or_asm (treenode * tptr, const BOOL guy_not_asm)
{
	treenode *operand = RightOpOf (tptr);
	int instruction = DOpTypeOf (tptr);
	if ((instruction & I_PRIMARY) != 0) {
		/*{{{  PRIMARY */
		switch (TagOf (ThisItem (operand))) {
		case N_LABELDEF:
			break;
		case N_PROCDEF:
		case N_MPROCDECL:
		case N_LIBPROCDEF:	/* bug TS/1240 14/04/92 */
		case N_LIBMPROCDECL:
		case N_SFUNCDEF:
		case N_LFUNCDEF:
		case N_LIBFUNCDEF:
			/* I deliberately do not adjust the vsp here.
			   I assume that _if_ someone wants to call a routine from ASM,
			   then it's because they're doing something nasty.
			   They can either supply their own vectorspace, not use VS,
			   or simply use a normal call if they want all the normal stuff
			 */
			/* frmb mod: assuming non-recursive and mobilespace will behave
			 * the same as vectorspace.
			 */
			maproutinename (ThisItem (operand), NULL, TRUE, FALSE, PROC_NONE);
			break;
		default:
			if (!isconst (ThisItem (operand))) {	/* bug 1391 11/9/91 */
				instruction &= INST_MASK;
				if (guy_not_asm && (instruction == I_LDLP || instruction == I_LDNLP)) {
					mapaddr (ThisItemAddr (operand));
				} else {
					mapexp (ThisItemAddr (operand));
				}
			}
		}
		/*}}} */
	} else if (((instruction & I_PSEUDO_OP) != 0) && !EndOfList (operand)) {
		/*{{{  PSEUDO OP with operands */
		int loadseq;
		treenode **operands[MAXREGS];
		int opmodes[MAXREGS];
		const int ops = setup_asm_operands (operand, operands, opmodes);
		switch (instruction & INST_MASK) {
		default:
			break;
		case I_LD:
			if (opmodes[0] == P_EXP) {
				mapexp (operands[0]);
			} else {
				mapaddr (operands[0]);
			}
			break;
		case I_LDAB:
			loadseq = mapload2regs (opmodes[0], operands[0], opmodes[1], operands[1]);
			break;
		case I_LDABC:
			loadseq = mapload3regs (opmodes[0], operands[0], opmodes[1], operands[1], opmodes[2], operands[2]);
			break;
		case I_ST:
		case I_STAB:
		case I_STABC:
			mapstoreregs (operands, ops);
			break;
		case I_RESERVELOWWS:	/* bug TS/1166 14/04/92 */
			datasize = max_INT32 (datasize, LoValOf (*operands[0]));
			break;
		case I_WSMAP:
			add_reservedwsmap (LoValOf (*operands[1]), 1, LoValOf (*operands[0]));
			break;
		case I_CODEMAP:
			break;
		}
		/*}}} */
	} else if ((instruction & (I_PRIMARY | I_PSEUDO_OP | I_FPU_ENTRY_BIT)) == 0) {			/* it's a secondary */
		/*{{{  SECONDARY */
		switch (instruction & INST_MASK) {
		case I_IN:
		case I_OUT:
		case I_OUTBYTE:
		case I_OUTWORD:
		case I_ALT:
		case I_ALTWT:
		case I_ENBC:
		case I_ENBS:
		case I_DISC:
		case I_DISS:
		case I_VIN:
		case I_VOUT:
		case I_LDCNT:	/* variableio 3/11/91 */
		case I_ENBG:
		case I_DISG:
		case I_GRANT:
			datasize = max_INT32 (datasize, DS_IO);
			break;
		case I_TIN:
		case I_TALT:
		case I_TALTWT:
		case I_ENBT:
		case I_DIST:
			datasize = max_INT32 (datasize, DS_WAIT);
			break;
		default:
			break;
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE void mappreprocesslist(tptr)*/
/*****************************************************************************
 *
 *  mappreprocesslist takes a process list tptr, and calls mappreprocess
 *                     for each item on the list, leaving a list of
 *                     temporaries allocated, but not freed, by mappreprocess
 *
 *****************************************************************************/
PRIVATE void mappreprocesslist (treenode * tptr)
{
	if (tptr != NULL)
		while (!EndOfList (tptr)) {
			mappreprocess (ThisItem (tptr));
			tptr = NextItem (tptr);
		}
	return;
}

/*}}}*/
/*{{{  PRIVATE void mappresubscripts (tptr)*/
/*****************************************************************************
 *
 *  mappresubscripts performs mappreprocess on segments and subscripts.
 *                   For segments, temporaries are inserted where needed on
 *                   the start and length expressions.
 *                   No tree transformation is done, this is performed by
 *                   trans
 *
 *****************************************************************************/
PRIVATE void mappresubscripts (treenode * tptr)
{
	while (TRUE)
		switch (TagOf (tptr)) {
			/*{{{  SEGMENT SEGMENTITEM */
		case S_SEGMENT:
		case S_SEGMENTITEM:
			{
				treenode *lengthexp = SLengthExpOf (tptr);
				treenode *startexp = SStartExpOf (tptr);

				mappreprocess (TagOf (startexp) == T_TEMP ? NDeclOf (startexp) : startexp);
				mappreprocess (TagOf (lengthexp) == T_TEMP ? NDeclOf (lengthexp) : lengthexp);
				mappresubscripts (SNameOf (tptr));

				/*{{{  map start and length expressions, put them in temporaries if necessary */
				/* Mark the start and length temporaries (if there are any) as
				   preevaluated so that we correctly map them when used later.
				   These tags are changed back to T_TEMP when the temporaries are freed. */

				if (TagOf (startexp) == T_TEMP) {
					/*{{{  load start expression to a temporary */
					/* we would evaluate it twice - once in the range check and once in the
					   base, otherwise */
					DEBUG_MSG (("mappresubscripts: mapping/allocating segment start\n"));
					mapexp (NDeclAddr (startexp));
					alloctemp (startexp);
					SetTag (startexp, T_PREEVALTEMP);
					addtemplist (startexp);
					/*}}} */
				}
				if (TagOf (lengthexp) == T_TEMP) {
					/*{{{  insert temporary on slength */
					DEBUG_MSG (("mappresubscripts: mapping segment length\n"));
					mapexp (NDeclAddr (lengthexp));
					DEBUG_MSG (("mappresubscripts: allocating segment length\n"));
					alloctemp (lengthexp);
					SetTag (lengthexp, T_PREEVALTEMP);
					addtemplist (lengthexp);
					/*}}} */
				}
				/*}}} */

				mappreprocess (SSubscriptExpOf (tptr));
				if (SCheckExpOf (tptr) != NULL)
					mapexp (SCheckExpAddr (tptr));
				return;
			}
			/*}}} */
			/*{{{  ARRAYSUB */
		case S_ARRAYSUB:
		case S_ARRAYITEM:
		case S_RECORDSUB:
		case S_RECORDITEM:
			mappreprocess (ASExpOf (tptr));
			tptr = ASBaseOf (tptr);
			break;
			/*}}} */
		default:
			return;
		}
}

/*}}}*/
/*{{{  PUBLIC void mappreprocess(tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mappreprocess maps out workspace requirements for code which has to be
 *                pulled out in front of a process.
 *                At the moment this is segment base and length checking.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void mappreprocess (treenode * tptr)
{
	while (tptr != NULL) {
		DEBUG_MSG (("mappreprocess: %s\n", itagstring (TagOf (tptr))));
		switch (TagOf (tptr)) {
		default:
			return;
			/*{{{  specification         break / return */
		case S_DECL:	/* bug TS/1263 11/05/92 */
		case S_ABBR:
		case S_VALABBR:
		case S_RETYPE:
		case S_VALRETYPE:
			/* changed to only do a single specification:
			   bug 776 05/10/90 */
			tptr = DValOf (tptr);
			break;

			/* changed to only do a single specification:
			   bug 776 05/10/90 */
			/* They now fall through to the default, which is to return; */
#if 0
		case S_DECL:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_TPROTDEF:
		case S_SPROTDEF:

			if (!isspecification (DBodyOf (tptr)))
				return;
			tptr = DBodyOf (tptr);
			break;
#endif
			/*}}} */
			/*{{{  process               break / return */
			/*{{{  IF                          return */
		case S_IF:
			/*{{{  transform choice list */
			/* This whole lot should be moved into trans! */

			{	/* This removes FALSE guards; CO'N 31/5/90 bug 307 */
				treenode **choicelist = CBodyAddr (tptr);
				while (!EndOfList (*choicelist)) {
					treenode *choicenode = skipspecifications (ThisItem (*choicelist));
					treenode **next = NextItemAddr (*choicelist);
					if (TagOf (choicenode) == S_CHOICE) {
						if (istrueguard (CondGuardOf (choicenode)))
							NewNextItem (NULL, *choicelist);
						else if (isfalseguard (CondGuardOf (choicenode))) {
							if (choicenode != ThisItem (*choicelist)) {	/* added 28/8/90 *//* there are leading specifications which may set error */
								SetCondBody (choicenode, newleafnode (S_SKIP, LocnOf (choicenode)));
							} else {	/* remove that guard completely */

								next = choicelist;
								*choicelist = NextItem (*choicelist);
							}
						}
					}
					choicelist = next;
				}
			}
			/*}}} */
			/* This has been removed to avoid calling mappreprocess twice
			   on the guards */
			/* bug 776 8/10/90 */
			/*mappreprocesslist(CBodyOf(tptr)); */
			return;
			/*}}} */
			/*{{{  ALT PRIALT                    return */
		case S_ALT:
		case S_PRIALT:
			/*{{{  transform alternative list */
			{
				/* This should really be done in trans (CON 8/10/90) */
				treenode *altlist = CBodyOf (tptr);
				while (!EndOfList (altlist)) {
					treenode *aptr = ThisItem (altlist);
					treenode *altnode = skipspecifications (aptr);
					if (TagOf (altnode) == S_ALTERNATIVE)
						/*{{{  try and move specifications in front of the input */
						/* We move specifications in front of the input providing:
						   1. They are used in the input item list
						   2. They are not used in the guard or channel
						 */
					{
						treenode *t = transformalt (aptr, altnode);
						NewItem (t, altlist);
					}
					/*}}} */
					altlist = NextItem (altlist);
				}
			}
			/*}}} */
			return;
			/*}}} */
			/*{{{  REPLSEQ REPLPAR PRIREPLPAR    break */
		case S_REPLSEQ:
		case S_REPLPAR:
		case S_PRIREPLPAR:
		case S_REPLIF:	/* bug 776 8/10/90 */
		case S_REPLALT:
		case S_PRIREPLALT:	/* bug 776 5/10/90 */
			mappreprocess (ReplCStartExpOf (tptr));
			if (ReplCStepExpOf (tptr)) {
				mappreprocess (ReplCLengthExpOf (tptr));
				tptr = ReplCStepExpOf (tptr);
			} else {
				tptr = ReplCLengthExpOf (tptr);
			}
			break;
			/*}}} */
			/*{{{  WHILE CHOICE        break */
		case S_WHILE:
		case S_CHOICE:
			tptr = CondGuardOf (tptr);
			break;
			/*}}} */
			/*{{{  ALTERNATIVE         break */
		case S_ALTERNATIVE:
			mappreprocess (AltGuardOf (tptr));
#if 0
			/* This has been changed so that it only maps the input and guard
			   (bug 776 5/10/90) */
			tptr = mappreprocesses (AltInputOf (tptr));
#else
			tptr = skipspecifications (AltInputOf (tptr));
#endif
			break;
			/*}}} */
			/*{{{  PINSTANCE FINSTANCE         return */
		case S_PINSTANCE:
		case S_FINSTANCE:
			/*  Must make sure that the parameter expressions are transformed before
			   the parameter list is augmented, as hidden params may point to
			   proper params. N.B. also that a segment param must have had a tempnode
			   inserted on its length already (this is done by 'trans')
			   if we are to use the temp as a hidden param.
			 */
			mappreprocesslist (IParamListOf (tptr));
			return;
			/*}}} */
			/*{{{  ASS                 break / return */
		case S_ASS:
			{
				treenode *lhs = LHSOf (tptr), *rhs = RHSOf (tptr);
				if (TagOf (lhs) == S_LIST) {
					mappreprocesslist (lhs);
					mappreprocesslist (rhs);
					return;
				} else {
					mappreprocess (lhs);
					tptr = rhs;
				}
				break;
			}
			/*}}} */
			/*{{{  OUTPUT INPUT TAGGED_INPUT X_INPUT X_TAGGED_INPUT X_INPUT_OUTPUT  return */
		case S_OUTPUT:
		case S_INPUT:
		case S_TAGGED_INPUT:
			mappreprocess (LHSOf (tptr));
			mappreprocesslist (RHSOf (tptr));
			return;
		case S_X_INPUT:
		case S_X_TAGGED_INPUT:
			mappreprocess (LHSOf (tptr));
			mappreprocesslist (RHSOf (tptr));
			mappreprocess (ActionDuringOf (tptr));
			mappreprocess (ActionAfterOf (tptr));
			return;
		case S_X_INPUT_OUTPUT:
			mappreprocess (LHSOf (tptr));
			if (RHSOf (tptr)) {
				mappreprocess (RHSOf (tptr));
			}
			return;
			/*}}} */
			/*{{{  DELAYED_INPUT       break */
		case S_DELAYED_INPUT:
			mappreprocess (LHSOf (tptr));
			tptr = RHSOf (tptr);
			break;
			/*}}} */
			/*{{{  CASE                break */
		case S_CASE:
			tptr = LHSOf (tptr);
			break;
			/*}}} */
			/*{{{  CASE_INPUT                  return */
		case S_CASE_INPUT:
			mappreprocess (LHSOf (tptr));
			mappreprocesslist (RHSOf (tptr));
			return;
			/*}}} */
			/*{{{  VARIANT                     return */
		case S_VARIANT:
			mappreprocesslist (VRTaggedListOf (tptr));
			return;
			/*}}} */
			/*}}} */
			/*{{{  expression            break / return */
			/*{{{  monadics            break */
		case S_NEG:
		case S_BITNOT:
		case S_NOT:
		case S_SIZE:
		case S_UMINUS:
		case S_EXACT:
		case S_TRUNC:
		case S_ROUND:
		case S_ADDRESSOF:
			tptr = OpOf (tptr);
			break;
			/*}}} */
			/*{{{  dyadics             break */
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
		case S_PLUS:
		case S_MINUS:
		case S_TIMES:
		case S_EQ:
		case S_NE:
		case S_LS:
		case S_LE:
		case S_GR:
		case S_GE:
		case S_COLON2:
		case S_CSUB0:
		case S_LSHIFT:
		case S_RSHIFT:

#if 1				/* Added 27/01/92 for bug TS/1581 */
			if ((TagOf (tptr) == S_RSHIFT) && isshortint (DOpTypeOf (tptr))) {	/* bug 1345 24/7/91 */
				/* mask to ensure that the bits shifted in are zero */
				/* Note that the stuff in 'tdop' ensures that the masked
				   value is not signextended again!
				 */
				SetLeftOp (tptr, newdopnode (S_BITAND, LocnOf (tptr),
							     LeftOpOf (tptr), newconstant (typemask (DOpTypeOf (tptr))), DOpTypeOf (tptr)));
			}
#endif

			mappreprocess (LeftOpOf (tptr));
			tptr = RightOpOf (tptr);
			break;
		case S_EVAL:
			mappreprocess (LeftOpOf (tptr));
			mapexp (LeftOpAddr (tptr));
			tptr = RightOpOf (tptr);
			break;
		case S_AFTER:
			assert (FALSE);	/* after is processd in trans */
			break;
			/*}}} */
			/*{{{  VALOF                       return */
		case S_VALOF:
			mappreprocesslist (VLResultListOf (tptr));
			return;
			/*}}} */
			/*}}} */
			/*{{{  element                       return */
		case S_ARRAYITEM:
		case S_SEGMENTITEM:
		case S_RECORDSUB:
		case S_RECORDITEM:
			mappresubscripts (tptr);
			return;
			/*}}} */
			/*{{{  configuration         break */
#if 0
			/* changed to only do a single process at a time */
			/* bug 776 5/10/90 */
			/* They now fall through to the default, which is to return; */
		case S_PLACE:
		case S_WSPLACE:
		case S_VSPLACE:
			tptr = DBodyOf (tptr);
			break;
#endif
			/*}}} */
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void mapprocessbody(tptr)*/
/* Allocate workspace offsets for the process tptr */
PRIVATE void mapprocessbody (treenode *tptr, void *const voidptr)
{
	static BOOL guy_not_asm = FALSE;	/* Yes, I mean local to this function */
	DEBUG_MSG (("mapprocessbody: %s\n", itagstring (TagOf (tptr))));
#if 0
fprintf (stderr, "mapprocessbody: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	while (TRUE) {
		switch (TagOf (tptr)) {
			/*{{{  cases */
			/*{{{  first and second halves of extended input */
		case S_X_FIRST_HALF:
		case S_X_SECOND_HALF:
			tptr = OpOf (tptr);
			break;
			/*}}}  */
			/*{{{  FORKING*/
		case S_FORKING:
			/* step over/through it */
			tptr = CBodyOf (tptr);
			break;
			/*}}}*/
			/*{{{  CLAIM*/
		case S_CLAIM:
			/* frmb: 17/03/2003: moved out semaphore ops into ETC specials */
			/* reservelowworkspace (2); */	/* local 0 and 1 for temporaries */
			reservelowworkspace (1);		/* SEMRELEASE implementation needs a temporary around RUNP really.. */
			mapexp (CTempAddr (tptr));

			mapchantypeop (CTempAddr (tptr));

			tptr = CBodyOf (tptr);
			break;
			/*}}}*/
			/*{{{  abbreviation, retype, declaration, procedure, function; return */
		case S_VALABBR:
		case S_ABBR:
		case S_VALRETYPE:
		case S_RETYPE:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_DECL:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_PLACE:
		case S_WSPLACE:
		case S_VSPLACE:
		case S_PRAGMA:	/* bug 829 19/9/91 */
		case S_TYPEDECL:
#ifdef MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			mapdeclandbody (tptr, mapprocess, FALSE, TRUE);
			return;
			/*}}} */
			/*{{{  S_STOP S_SKIP S_END   return */
		case S_STOP:
		case S_SKIP:
		case S_END:	/* Marks the end of an SC */
			return;
			/*}}} */
			/*{{{  S_SYNC*/
		case S_SYNC:
			/* sync is suspendable */
			datasize = max_INT32 (datasize, DS_IO);
			mapexp (LeafLinkAddr (tptr));
			return;
			/*}}}*/
			/*{{{  S_RESIGN*/
		case S_RESIGN:
			/* resign may reschedule processes, but never deschedules itself */
			mapexp (CTempAddr (tptr));
			tptr = CBodyOf (tptr);
			break;
			/*}}}*/
			/*{{{  S_SUSPEND             return*/
		case S_SUSPEND:
			mapsuspend (tptr);
			return;
			/*}}}*/
			/*{{{  S_SEQ/IF              return */
		case S_SEQ:
		case S_IF:
			mapconstruction (CBodyOf (tptr), mapprocess);
			return;
			/*}}} */
			/*{{{  S_REPLSEQ/S_REPLIF    return */
		case S_REPLSEQ:
		case S_REPLIF:
			maprepl (tptr, mapprocess);
			return;
			/*}}} */
			/*{{{  S_PAR S_PRIPAR        return */
		case S_PAR:
		case S_PRIPAR:
			/* Workspace requirement is : local 0 and local 1 for housekeeping,
			   plus the space required by each branch of the PAR */
			{
				const BOOL priparflag = FALSE; /* TagOf (tptr) == S_PRIPAR; */
				treenode *parlist = CBodyOf (tptr);
				INT32 maxwsp;

				if (listitems (parlist) > 0) {
					/* Reserve slot for process count, and slot for join address */
					treenode *barrier = CTempOf (tptr);
					treenode *declbars, *extbars;

					if (barrier && (TagOf (barrier) == S_BAREXTEND)) {
						declbars = LeftOpOf (barrier);
						extbars = RightOpOf (barrier);
					} else if (barrier && (TagOf (barrier) == S_EXTENDS)) {
						/* extends only */
						declbars = NULL;
						extbars = barrier;
					} else if (barrier) {
						/* declarations only */
						declbars = barrier;
						extbars = NULL;
					} else {
						declbars = extbars = NULL;
					}

					/* map barriers extended */
					if (extbars && (TagOf (extbars) == S_EXTENDS)) {
						if (TagOf (OpOf (extbars)) == S_LIST) {
							treenode *walk;

							for (walk = OpOf (extbars); !EndOfList (walk); walk = NextItem (walk)) {
								mapexp (ThisItemAddr (walk));
							}
						} else {
							mapexp (OpAddr (extbars));
						}
					} else if (extbars) {
						geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "mapprocessbody: PAR EXTENDS");
					}

					/* map barriers declared */
					if (declbars) {
						if (TagOf (declbars) == S_LIST) {
							treenode *walk;

							for (walk = declbars; !EndOfList (walk); walk = NextItem (walk)) {
								mapname (ThisItem (walk));
							}
						} else {
							mapname (declbars);
						}
					}
					/* If PRI PAR via an ALT, still use two slots, one for the channel,
					   and one for the ALT clobbering workspace zero */
					#ifdef PROCESS_PRIORITY
						/* frmb update: extra slot for invoking process's priority (local 2) -- restored in endp */
						reservelowworkspacetagged (3, S_PAR);
						/* reservelowworkspace (3); */
					#else
						reservelowworkspacetagged (2, S_PAR);
						/* reservelowworkspace (2); */
					#endif
					if (inside_suspends) {
						/* add a pointer for the join-lab -- only for the first process */
						mark_hidden_var (S_CONSTPTR, 0);
					}
					/*{{{  map each process */
					{
						const INT32 saveddatasize = datasize;
						const INT32 savedvsp = vsp, savedmaxvsp = maxvsp;
						treenode *const saved_vsp_nptr = enclosing_vsp_nptr;
						const BOOL alt_for_pri_par = priparflag && (code_style_flags & CODE_STYLE_ALT_PRI_PAR);
						/* If we are doing a PRI PAR with an ALT, the process doing the setting up
						   will be descheduled (by the ALT) before it terminates,
						   so it needs another DS_IO workspace slots  - CO'N */
						INT32 parspace = alt_for_pri_par ? DS_IO : 0;
						const int savednum_var = num_var;
						const int savedvar_base = var_base;
						int branch_number = 1;

						/*{{{  debug message */
						if (diagnostics)
							fprintf (outfile, "\nAllocating workspace for parallel process %d, %d\n", var_base, num_var);
						/*}}} */
						if (needs_quadalign) {
							parspace = (parspace + 1) & (~1);	/* round up to even number */
						}
						maxvsp = vsp;
						/*{{{  map each branch of the PAR */
						while (!EndOfList (parlist)) {
							treenode *pprocess = ThisItem (parlist);
							treenode *spptr;
							treenode *nameliststart = namelist;
							void *saved_map_state = NULL;

							/*{{{  initialise environment */
							#ifdef MOBILES
							if (inside_suspends) {
								saved_map_state = sp_saveallocvars (TRUE);
							}
							#endif
							num_var = savednum_var;
							var_base = savednum_var;
							/* bug 1365 - if using an ALT for PRI PAR, and this is the high priority
							   branch, allow for the final dummy communication. 14/8/91.
							 */
							datasize = (alt_for_pri_par && (branch_number == 1)) ? DS_IO : DS_MIN;
							/*}}} */
							mapprocess (pprocess);
							if (inside_suspends) {
								/* the call to MPBARRESIGN() is inside the process now */
#if 0
								/* need to map out a call to MPBARRESIGN() */
								treenode *pdname = fe_lookupname (lookupword ("MPBARRESIGN", 11));
#endif

#if 0
fprintf (stderr, "bind1: mapping PAR; inside_suspends = %p = ", inside_suspends);
printtreenl (stderr, 4, inside_suspends);
fprintf (stderr, "bind1: barrier resign is: ");
printtreenl (stderr, 4, pdname);
fprintf (stderr, "bind1: lookup for MPBARSYNC: ");
printtreenl (stderr, 4, fe_lookupname (lookupword ("MPBARSYNC", 9)));
#endif
#if 0
								maproutinename (pdname, newleafnode (S_SKIP, NOPOSN), TRUE, FALSE, PROC_MPA);
#endif
							}
							maxwsp = allocvars (var_base, 0, TRUE);
							if (priparflag && (branch_number == 2) && (TagOf (pprocess) == S_SKIP)) {
								datasize = 0;	/* INSdi01941 */
							}

							/* maxwsp now contains our maximum workspace requirement for PAR branch */
							/* The field vsusage is not needed in this space node */
							if (needs_quadalign) {
								datasize = (datasize + 1) & (~1);	/* round up to even number */
								maxwsp = (maxwsp + 1) & (~1);	/* round up to even number */
							}
							/*{{{  debug message */
							if (diagnostics)
								fprintf (outfile,
									 "for branch of par maxwsp = %d, datasize = %d, maxvsp = %d\n",
									 maxwsp, datasize, maxvsp);

							/*}}} */
#ifdef MOBILES
							spptr = newspacenode (S_SPACEUSAGE, NOPOSN, pprocess, maxwsp,
									      maxwsp + datasize, 0, 0, 0, 0, NULL, 0);
#else
							spptr = newspacenode (S_SPACEUSAGE, NOPOSN, pprocess, maxwsp,
									      maxwsp + datasize, 0, 0, NULL, 0);
#endif
							NewItem (spptr, parlist);
							/*{{{  move all vars declared within this branch below ws already allocated */
							{
								int i;
								treenode *var;
								INT32 adjust = maxwsp + parspace;
								for (i = var_base; i < num_var; i++) {	/* Add these vars to the par namelist */
									var = get_var (i);
									if (TagOf (var) != T_RESERVEDWS)
										while (var != NULL) {
											treenode *nextvar = NVNextOf (var);
											SetNVNext (var, namelist);
											namelist = var;
											var = nextvar;
										}
								}
								/* Move all variables that have been added in this branch */
								var = namelist;
								while (var != nameliststart) {
									INT32 oldoffset;
									if ((oldoffset = NVOffsetOf (var)) != NO_SLOT) {
										SetNVOffset (var, oldoffset - adjust);
										if (diagnostics)
											/*{{{  print some diagnostics */
										{
											if (TagOf (var) == T_TEMP)
												fprintf (outfile, "$temp%d", NVVarNumOf (var));
											else
												fprintf (outfile, WNameOf (NNameOf (var)));
											fprintf (outfile, " moved from %ld to workspace offset %ld\n",
												 (long) oldoffset, (long) NVOffsetOf (var));
										}
										/*}}} */
									}
									var = NVNextOf (var);
								}
							}
							/*}}} */
							parspace += maxwsp + datasize;
							vsp = maxvsp;
							parlist = NextItem (parlist);
							branch_number++;
							free_fixedwslist ();
							save_wsallocmap (spptr);
							#ifdef MOBILES
							if (inside_suspends && saved_map_state) {
								sp_restoreallocvars (saved_map_state);
								saved_map_state = NULL;
							}
							#endif
						}
						/*}}} */
						/*{{{  restore & update environment */
						var_base = savedvar_base;
						num_var = savednum_var;
						vsp = savedvsp;
						maxvsp = max_INT32 (maxvsp, savedmaxvsp);
						enclosing_vsp_nptr = saved_vsp_nptr;
						/* the parspace goes below workspace */
						datasize = max_INT32 (saveddatasize, parspace);
						/*}}} */
					}
					/*}}} */
				}
			}
			return;
			/*}}} */
			/*{{{  S_REPLPAR             return */
		case S_REPLPAR:
			/* Workspace requirement is : local 0 and local 1 for housekeeping,
			   local 2 (and possibly local 3) as temporaries
			   plus the space required for all replications of the PAR,
			   this last goes below our current workspace. */
			/* modified July 2004 (frmb): this is now more if we're in a SUSPENDing mobile process, since the
			 * PAR process must MPBARRESIGN inside the PAR setup workspace.
			 */
			/* Vector space requirement is: a workspace slot for a vsp for each
			   replicated process (if vector space is used), and
			   vector.space for repl.process * repl.count . */
			mapreplpar (tptr);
			return;
			/*}}} */
			/*{{{  S_WHILE               return */
		case S_WHILE:
			{
				const INT32 oldweight = loop_weight;
				uploop_weight (VAR_USAGE_PER_LOOP);
				mapbool (CondGuardAddr (tptr));
				mapprocess (CondBodyOf (tptr));
				loop_weight = oldweight;
			}
			return;
			/*}}} */
			/*{{{  S_CHOICE              return */
		case S_CHOICE:
			{
				mapbool (CondGuardAddr (tptr));
				mapprocess (CondBodyOf (tptr));
				return;
			}

			/*}}} */
			/*{{{  S_SELECTION           return */
		case S_SELECTION:
			{
				/* Map selection list to put constants in a constant table, 
				   if necessary */
				treenode *cguard = CondGuardOf (tptr);
				if (TagOf (cguard) != S_ELSE)
					mapexplist (cguard);
				mapprocess (CondBodyOf (tptr));
			}
			return;
			/*}}} */
			/*{{{  S_VARIANT             return */
		case S_VARIANT:
			{
				treenode *inputlist = NextItem (VRTaggedListOf (tptr));
				/* Skip over the tag */
				while (!EndOfList (inputlist)) {
					mapinputitem (globchannelmode, globchannelptr, P_PTR, ThisItemAddr (inputlist), NULL, NULL, NULL);
					inputlist = NextItem (inputlist);
				}
				mapprocess (VRBodyOf (tptr));
			}
			return;
			/*}}} */
			/*{{{  S_ALT S_REPLALT S_PRIALT S_PRIREPLALT  return */
		case S_ALT:
		case S_PRIALT:
			mapalt (tptr);
			return;
		case S_REPLALT:
		case S_PRIREPLALT:
			mapalt (tptr);
			return;
			/*}}} */
			/*{{{  S_OUTPUT              return */
		case S_OUTPUT:
			mapoutput (tptr);
			return;
			/*}}} */
			/*{{{  S_INPUT S_DELAYED_INPUT S_CASE_INPUT S_TAGGED_INPUT S_X_INPUT S_X_TAGGED_INPUT return */
		case S_INPUT:
		case S_DELAYED_INPUT:
		case S_CASE_INPUT:
		case S_TAGGED_INPUT:
		case S_X_INPUT:
		case S_X_TAGGED_INPUT:
			mapinput (tptr);
			return;
			/*}}} */
			/*{{{  S_X_INPUT_OUTPUT  return */
		#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3) && defined(PD_ENCODE_CHANNEL)
		case S_X_INPUT_OUTPUT:
			mapxinputoutput (tptr);
			return;
		#endif
			/*}}}*/
			/*{{{  S_ASS                 return */
		case S_ASS:
			mapassign (LHSAddr (tptr), RHSAddr (tptr));
			return;
			/*}}} */
			/*{{{  S_PINSTANCE           return */
		case S_PINSTANCE:
			if ((TagOf (INameOf (tptr)) == N_PREDEFPROC) && mappredef (tptr, NULL)) {
				return;	/* Exit if done inline */
			}
			/* check for FORK'd PROC and use forking variable */
			if (IForkedOf (tptr)) {
				treenode *forking = IForkOf (tptr);

				/* might be the global FORKING */
				if (forking) {
					treenode *fork_var = DNameOf (CBodyOf (forking));

					upusecount (fork_var, 1);
				}
			}
#if 0
/* DEBUG */
fprintf (stderr, "mapprocessbody(): PINSTANCE: (of \"%*s\", mobile-requirements = %d words)\n", WLengthOf (NNameOf (INameOf (tptr))), WNameOf (NNameOf (INameOf (tptr))), NPMSUsageOf (INameOf (tptr)));
#endif
#if 0
fprintf (stderr, "mapprocessbody(): augmenting params()...\n");
#endif
			SetIParamList (tptr, augmentparams (IParamListOf (tptr), NParamListOf (INameOf (tptr)), NULL, tptr));
			mapinstance (tptr);
			return;
			/*}}} */
			/*{{{  S_CASE                return */
		case S_CASE:
			mapcase (tptr);
			return;
			/*}}} */
			/*{{{  guys */
		case S_GUY:
		case S_ASM:
			{
				guy_not_asm = (TagOf (tptr) == S_GUY);
				walklist (mapprocessbody, CBodyOf (tptr), voidptr);
			}
			return;
		case S_LABELDEF:
			SetNVOffset (DNameOf (tptr), newlab ());
			return;
		case S_GUYCODE:
			/* guy_not_asm was set by the `enclosing' S_GUY or S_ASM */
			mapguy_or_asm (tptr, guy_not_asm);
			return;
		case S_GUYSTEP:
			return;
			/*}}} */
			/*}}} */
		default:
			badtag (genlocn, TagOf (tptr), "mapprocessbody");
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void mapprocess(tptr)*/
/*****************************************************************************
 *
 *  mapprocess maps out workspace requirements for the process tptr,
 *             including pre- and post- code.
 *
 *****************************************************************************/
PUBLIC void mapprocess (treenode *tptr)
{
	treenode *savedtemplist = templist;
	treenode *const save_vsp_nptr = enclosing_vsp_nptr;

#if 0
fprintf (stderr, "mapprocess: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	templist = NULL;
	genlocn = LocnOf (tptr);
	mappreprocess (tptr);
	mapprocessbody (tptr, NULL);
	freetemplist ();
	templist = savedtemplist;
	enclosing_vsp_nptr = save_vsp_nptr;
}

/*}}}*/
/*{{{  PRIVATE BOOL is_renamed_proc*/
PRIVATE BOOL is_renamed_proc (treenode * const nptr, treenode * const body)
{
	/* bug TS/1842 24/08/92 */
	/* This checks for PROC bodies of the form
	   PROC name (params)
	   name2(params)
	   :
	   IE PROCEDUREs which simply provide another name for a previous one.
	   This may sound esoteric, but in fact there are many such in
	   the hostio library, because of name abstraction issues.
	 */
	if ((TagOf (nptr) == N_PROCDEF) && (TagOf (body) == S_PINSTANCE)) {
		/* check the names of the actuals */
		treenode *alist = IParamListOf (body);
		treenode *flist = NTypeOf (nptr);
		char *txtname;

#if 0
		fprintf (outfile, "Actuals: ");
		printtree (outfile, 0, alist);
		fprintf (outfile, "\nFormals: ");
		printtree (outfile, 0, flist);
		fprintf (outfile, "\n");
#endif

		if (TagOf (INameOf (body)) == N_PREDEFPROC) {
			return FALSE;
		}
		if (IForkedOf (body) || IRecursiveOf (body)) {
			/* can't rename things that get FORKed or are recursive! */
			return FALSE;
		}

		if ((NLexLevelOf (nptr) != NLexLevelOf (INameOf (body))) || (separatelycompiled (INameOf (body)) && (NLexLevelOf (nptr) != 0)))	/* INSdi03371 */
			return FALSE;	/* we don't want to bother with the complication! */

		txtname = (char *)(WNameOf (NNameOf (INameOf (body))));
		if (WLengthOf (NNameOf (INameOf (body))) > 2) {
			if (((txtname[0] == 'C') || (txtname[0] == 'B')) && (txtname[1] == '.')) {
				return FALSE;	/* don't rename PROCs called C., B. */
			} else if ((txtname[2] == '.') && (((txtname[0] == 'B') && (txtname[1] == 'X')) || ((txtname[0] == 'K') && (txtname[1] == 'R')))) {
				return FALSE;	/* don't rename PROCs called BX., KR. */
			}
		}

		while (!EndOfList (flist)) {
			if (isnamedformal (ThisItem (flist))) {
				if (EndOfList (alist) ||	/* bug TS/1917 27/10/92 */
				    (ThisItem (flist) != ThisItem (alist)))
					return FALSE;
				alist = NextItem (alist);
			}
			flist = NextItem (flist);
		}
		return EndOfList (alist);
	}
	return FALSE;
}

/*}}}*/
/*{{{  PRIVATE void remove_result_variables*/
/*{{{  comment*/
/* This removes local variables which are declared to accumulate results,
   where the result is passed via a pointer, and uses the pointer directly.

   This saves temporaries, and removes some unneccessary data moves.
*/
/* If we try to remove vars which were not already pointers,
   we come a cropper with abbreviations to those vars, etc,
   because they have already made the assumptions about the rhs
   being not a pointer.
*/
/*}}}*/
PRIVATE void remove_result_variables (treenode * const tptr)
{
	treenode *const nptr = DNameOf (tptr);
	treenode *const body = DValOf (tptr);
	treenode *const resultlist = VLResultListOf (skipspecifications (body));
	const int fn_scope = NScopeOf (nptr);
	treenode *args;

	for (args = FnParamsOf (NTypeOf (nptr)); !EndOfList (args); args = NextItem (args))
		if (TagOf (ThisItem (args)) == S_FNFORMALRESULT)
			/*{{{  passed via pointer */
		{
			treenode *const formal_res = ThisItem (args);
			treenode *const result_exp = nth_listitem (resultlist, (int) HArgNoOf (formal_res));
			if ((TagOf (result_exp) == N_DECL)
			    && ispointer (result_exp)	/* see comment at top of fn */
			    &&(NScopeOf (result_exp) >= fn_scope))
				/*{{{  remove that result variable */
			{
				treenode *this_body = body;
				treenode **this_decl;
				/*{{{  print a message */
#if 0
				printf ("FUNCTION: %s, result: %d, expr is %s\n",
					WNameOf (NNameOf (nptr)), HArgOf (formal_res), WNameOf (NNameOf (result_exp)));
#endif
				/*}}} */

				this_decl = &this_body;
				while ((TagOf (*this_decl) != S_VALOF) && (DNameOf (*this_decl) != result_exp))
					this_decl = DBodyAddr (*this_decl);
				if (TagOf (*this_decl) != S_VALOF) {
					/*{{{  replace in any initialiser */
					replace_all_names (DValAddr (*this_decl), result_exp, formal_res);
					/*}}} */
					this_decl = DBodyAddr (*this_decl);	/* skip past the actual decl */
				}

				/*printf("this_decl now points at: %s\n", itagstring(TagOf(*this_decl))); */
				replace_all_names (this_decl, result_exp, formal_res);

				/*{{{  ensure we don't allocate vectorspace for this variable */
				if (NModeOf (result_exp) == NM_VECSPACE)
					SetNMode (result_exp, NM_WORKSPACE);
				/*}}} */

				/*{{{  adjust the name of the formal result, to help diagnostics */
				{
					/* simple version - this loses the information that we have
					   done this optimisation!
					 */
					/*SetNName(formal_res, NNameOf(result_exp)); */

					char str[MAXSTRING_SIZE * 2];
					sprintf (str, "%s_%s", WNameOf (NNameOf (formal_res)), WNameOf (NNameOf (result_exp)));
					SetNName (formal_res, lookupword (str, strlen (str)));
				}
				/*}}} */
			}
			/*}}} */
		}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void mapblock(tptr)*/
/*****************************************************************************
 *
 *  mapblock maps out the workspace for the PROC/FUNCTION definition tptr
 *
 *****************************************************************************/
PRIVATE void mapblock (treenode *tptr)
{
	treenode *const n = DNameOf (tptr);
	treenode *body = DValOf (tptr);
	treenode *saved_inside_suspends = inside_suspends;

	/*{{{  debug message */
	if (diagnostics) {
		fprintf (outfile, "\nAllocating workspace for routine %s at lexlevel %d\n", WNameOf (NNameOf (n)), be_lexlevel - 1);
	}
#if 0
fprintf (stderr, "mapblock: allocating workspace for routine %s at lexlevel %d\n", WNameOf (NNameOf (n)), be_lexlevel - 1);
#endif
/*current_proc = n; *//* no longer needed */
	/*}}} */
	/*{{{  initialise environment */
	initvsp (0);		/* Initialise vector space usage to zero */
	datasize = DS_MIN;
	if ((alloc_strategy & ALLOC_MAXBELOW) != 0) {
		datasize = DS_WAIT;
	}
	staticlinkused = STATICLINK_NOT_USED;
	consttablechain = NULL;
	namelist = NULL;
	var_base = 0;
	num_var = 0;
	loop_weight = 1;
	init_mapping ();
	setusecount (constantnptr, 0);
	if (NPSuspendsOf (n)) {
		inside_suspends = n;
	} else {
		inside_suspends = NULL;
	}
	/*}}} */
	/*{{{  map out the routine body */
	if ((TagOf (n) == N_PROCDEF) || (TagOf (n) == N_MPROCDECL)) {
		mapprocess (body);	/* Map out the body of tptr */
	} else {
		treenode *savedtemplist = templist;
		templist = NULL;
		/*{{{  map leading specifications */
		while (isspecification (body)) {
			mappreprocess (body);	/* bug 867 28/1/91 */
			mapsingledecl (body);
			body = DBodyOf (body);
		}
		/*}}} */
		/*{{{  map the valof */
		{
			treenode *resultlist = VLResultListOf (body);
			struct
			{
				treenode **opd;
				int opdmode;
			}
			regresults[MAXREGS];
			mapprocess (VLBodyOf (body));
			mappreprocess (body);	/* Mappreprocess on the result list */
			switch (fn_return_style (n)) {
			case return_style_fpu:
				mapfpexp (ThisItemAddr (resultlist));
				break;
			case return_style_alu:
			case return_style_other:
				/*{{{  map function return list */
				{
					int nregresults = 0;
					int nptrresults = 0;
					BOOL complicated_res_exp = FALSE;
					/* destlist moved here: bug 1012 24/10/90 */
					treenode *destlist = firstresultof (FnParamsOf (NTypeOf (n)));
					for (; !EndOfList (resultlist); resultlist = NextItem (resultlist))
						/*{{{  assign this result through a pointer, or save it for a register */
					{
						treenode *const thisresult = ThisItem (resultlist);
/*treenode *destlist = firstresultof(FnParamsOf(NTypeOf(n))); *//* bug 1012 24/10/90 */
						const int type = ntypeof (thisresult);
						if ((nregresults < MAXREGS) && (result_return_style (gettype (thisresult)) == return_style_alu)) {
							regresults[nregresults].opd = ThisItemAddr (resultlist);
							regresults[nregresults].opdmode = P_EXP;
							nregresults++;
						} else if ((TagOf (thisresult) == S_FNFORMALRESULT) && (NLexLevelOf (thisresult) == be_lexlevel))
							/*{{{  the result has been removed */
						{
							/* The result variable has been removed; instead
							   references to it are done directly onto the
							   formal result parameter
							 */
							complicated_res_exp = TRUE;
						}
						/*}}} */
						else {
							/*printf ("mapblock: type is %s\n", itagstring(type)); */
							mapsimpleassign (type, P_EXP, ThisItemAddr (destlist), P_EXP, ThisItemAddr (resultlist));
							DEBUG_MSG (("mapblock: getting next destlist\n"));
							destlist = nextresultof (destlist);
							nptrresults++;
							complicated_res_exp = complicated_res_exp ||
								!(isaddressable (thisresult) || isconst (thisresult));
						}
					}
					/*}}} */
					/*{{{  load register results */
					/*{{{  preevaluate non-addressable real-valued results on an fp processor */
					if (has_fpu_core) {
						int i;
						for (i = 0; i < nregresults; i++) {
							treenode **expaddr = regresults[i].opd;
							if (isreal (ntypeof (*expaddr)) &&	/* bug INSdi01902 23/03/93 */
							    needtemptoload (regresults[i].opdmode, *expaddr)) {
								mapfpexp (expaddr);
								*expaddr = gettemp (*expaddr, NM_WORKSPACE);
								upusecount (*expaddr, 1);
								regresults[i].opdmode = P_TEMP;
							}
						}
					}
					/*}}} */
					{
						int loadseq;
						/* We go to all the trouble of working the load sequence out,
						   and then throw it away */
						switch (nregresults) {
						case 0:
							break;
						case 1:
							mapexpopd (regresults[0].opdmode, regresults[0].opd);
							loadseq = 1;
							break;
						case 2:
							loadseq = mapload2regs (regresults[1].opdmode, regresults[1].opd,
										regresults[0].opdmode, regresults[0].opd);
							break;
						case 3:
							loadseq = mapload3regs (regresults[2].opdmode, regresults[2].opd,
										regresults[1].opdmode, regresults[1].opd,
										regresults[0].opdmode, regresults[0].opd);
							break;
						}
					}
					/*{{{  free temporaries holding preevaluated reals */
					if (has_fpu_core) {
						int i;
						for (i = 0; i < nregresults; i++)
							if (regresults[i].opdmode == P_TEMP)
								freetemp (*(regresults[i].opd));
					}
					/*}}} */
					/*}}} */

					/*{{{  Decide whether to set NPSafeFnResult */
					/* we set NPSafeFnResult TRUE if we know that the function
					   doesn't read any of its parameters _after_ it has started
					   writing to any of its actual results.
					   This is not true if there are more than one result via a pointer,
					   or if there is a ptr result and a register result,
					   or if any of the result pointers is `complicated'
					   (ie more complex than a simple move)
					   NOTE that this is fail safe - unless we set this flag, we will
					   assume the pessimistic case.
					   Thus all external calls assume the pessimistic case.

					   This is only used to slighly optimise the cases when
					   alias checking is disabled; if it is enabled, we can
					   use the better knowledge that we have anyway.

					   CON - 7/1/93
					 */
					if ((nptrresults < 2) && (nregresults == 0) && !complicated_res_exp)
						SetNPSafeFnResult (n, TRUE);	/* bug TS/2024 07/01/93 */
					/*}}} */
				}
				/*}}} */
				break;
			}
		}
		/*}}} */
		freetemplist ();
		templist = savedtemplist;
	}
	/*{{{  check whether we need a constant pointer */
	set_constantnptr ();
	/*}}} */
	/*}}} */
	/*{{{  update environment*/
	inside_suspends = saved_inside_suspends;
	/*}}}*/
}

/*}}}*/
/*{{{  PRIVATE void mapnestedblocks(tptr)*/
/*****************************************************************************
 *
 *  mapnestedblocks walks the tree 'tptr' mapping nested routines
 *
 *
 *****************************************************************************/
PRIVATE void mapnestedblocks (treenode * tptr)
{
	while (tptr != NULL) {
		const int tag = TagOf (tptr);
		genlocn = LocnOf (tptr);
		switch (tag)
			/*{{{  cases */
		{
		default:
			return;
			/*{{{  specification */
			/*{{{  routine specification */
		case S_PROCDEF:
#ifdef MOBILES
		case S_MPROCDECL:
#endif
		case S_SFUNCDEF:
		case S_LFUNCDEF:
			/* Allocate workspace for procedure or function definition */
			{
				treenode *const n = DNameOf (tptr);
				const int isfunction = ((tag == S_SFUNCDEF) || (tag == S_LFUNCDEF));

				SetNPSafeFnResult (n, FALSE);	/* bug TS/2024 07/01/93 */
				/*{{{  initialise params */
				{
					treenode *fparams;
					for (fparams = NParamListOf (n); !EndOfList (fparams); fparams = NextItem (fparams)) {
						treenode *const thisfparam = ThisItem (fparams);
						switch (TagOf (thisfparam)) {
						case N_PARAM:
						case N_VALPARAM:
						case N_RESULTPARAM:
							SetNVNext (thisfparam, NULL);
							setusecount (thisfparam, 0);
							/* we now set up the param's mode in augmentformals (in trans phase) */
							SetNVOffset (thisfparam, NO_SLOT);	/* bug 1135 31/1/91 */
							break;
						case S_HIDDEN_PARAM:
							{
								treenode *const tempptr = HExpOf (thisfparam);
#ifdef MOBILES
								if (tempptr) {
									setusecount (tempptr, 1);
								}
#else
								setusecount (tempptr, 1);
#endif
							}
							break;
						}
					}
				}
				/*}}} */
				if (separatelycompiled (n)) {
					/*{{{  map as separately compiled */

					INT32 ws, vs;
					void *saved_map_state;
#ifdef MOBILES
					INT32 ms;
#endif
					/* vector space pointer onto front (or end) of params */
					if (!compiledforcorrectproc (n, configuring)) {
						vs = 0;
#ifdef MOBILES
						ms = 0;
#endif
					} else {
#ifdef MOBILES
						getprocwsandvsandms (n, &ws, &vs, &ms);
#else
						getprocwsandvs (n, &ws, &vs);
#endif
					}
					if (diagnostics) {	/* added to track down a bug */
#ifdef MOBILES
						fprintf (outfile, "Workspace allocation for library %s; ws:%d, vs:%d, ms:%d\n",
							 WNameOf (NNameOf (n)), ws, vs, ms);
#else
						fprintf (outfile, "Workspace allocation for library %s; ws:%d, vs:%d\n",
							 WNameOf (NNameOf (n)), ws, vs);
#endif
					}
					SetNPSLUsage (n, STATICLINK_NOT_USED);
#ifdef MOBILES
					SetNPParams (n, addstaticandvecandmob (n, vs, ms));
#else
					SetNPParams (n, addstaticandvec (n, vs));
#endif
					saved_map_state = sp_saveallocvars (FALSE);
					initallocvars (FALSE);	/* added for bug 1061 30/11/90 */
					allocparams (n);
					save_wsallocmap (n);	/* should be a no-op */
					sp_restoreallocvars (saved_map_state);
					/*}}} */
				} else if (is_renamed_proc (n, DValOf (tptr))) {
					/*{{{  map as a renamed PROC */
					/* bug TS/1842 24/08/92 */
					treenode *const target_nptr = INameOf (DValOf (tptr));
					int ptype = PROC_NONE;
					void *saved_map_state;

					if (diagnostics) {	/* added to track down a bug */
						fprintf (outfile, "Workspace allocation for %s which renames %s\n",
							 WNameOf (NNameOf (n)), WNameOf (NNameOf (target_nptr)));
					}

					/*mapblock(tptr); */
					if ((nodetypeoftag (TagOf (DValOf (tptr))) == INSTANCENODE) && IForkedOf (DValOf (tptr))) {
						ptype |= PROC_FORKED;
					}
					if ((nodetypeoftag (TagOf (DValOf (tptr))) == INSTANCENODE) && IRecursiveOf (DValOf (tptr))) {
						ptype |= PROC_REC;
					}
					maproutinename (target_nptr, DValOf (tptr), FALSE, FALSE, ptype);

					SetNPSLUsage (n, NPSLUsageOf (target_nptr));
					SetNPVSUsage (n, NPVSUsageOf (target_nptr));
#ifdef MOBILES
					SetNPMSUsage (n, NPMSUsageOf (target_nptr));
#endif
					SetNPConstTables (n, NULL);
					SetNPCPOffset (n, 0);
					{
						INT32 ws, vs;
#ifdef MOBILES
						INT32 ms;

						getprocwsandvsandms (target_nptr, &ws, &vs, &ms);
#else
						getprocwsandvs (target_nptr, &ws, &vs);
#endif
						SetNPDatasize (n, ws);
						SetNPVSUsage (n, vs);
#ifdef MOBILES
						SetNPMSUsage (n, ms);
						SetNPParams (n, addstaticandvecandmob (n, vs, ms));
#else
						SetNPParams (n, addstaticandvec (n, vs));
#endif
					}
					saved_map_state = sp_saveallocvars (FALSE);
					initallocvars (need_mapping(n));	/* added for bug 1061 30/11/90 */
					allocparams (n);
					save_wsallocmap (n);
					sp_restoreallocvars (saved_map_state);

					SetNPLabel (n, 0);	/* mark as a special routine */
					/*}}} */
				} else if (!isinline (n)) {
					/*{{{  map normal routine */
					INT32 maxwsp;
					int numparams;
					void *saved_map_state;
#ifdef MOBILES
					int mswords;
#endif
					be_lexlevel++;	/* Formal parameters are at a higher lexical level */
					mapnestedblocks (DValOf (tptr));
					/*{{{  optimise vars inserted for RESULT statements */
					if ((TagOf (tptr) != S_PROCDEF) && (TagOf (tptr) != S_MPROCDECL)) {
						remove_result_variables (tptr);	/* INSdi03202 */
					}
					/*}}} */
#ifdef MOBILES
					if (enable_mobilespace) {
						SetNPMSPtr (n, NULL);
						SetNPMSUsage (n, 0);
						initmobilenewproc (tptr);
						clear_reservedwsmaps ();
					}
#endif
					mapblock (tptr);
#ifdef MOBILES
					if (enable_mobilespace) {
						mobileoutproc ();
					}
#endif
					if (staticlinkused >= be_lexlevel) {
						staticlinkused = STATICLINK_NOT_USED;
					}
					SetNPSLUsage (n, staticlinkused);
					SetNPVSUsage (n, maxvsp);
#ifdef MOBILES
					if (enable_mobilespace) {
						mswords = mobilewordsin (n);
						SetNPMSUsage (n, mswords);
					} else {
						mswords = 0;
						SetNPMSUsage (n, mswords);
					}
					numparams = addstaticandvecandmob (n, maxvsp, mswords);
#else
					numparams = addstaticandvec (n, maxvsp);
#endif
					SetNPParams (n, numparams);
#if 0				/* bug TS/1988 09/12/92 */
					/* All outer level procs mustn't use param slots as variables: */
					if (be_lexlevel == 1)
						/* && strcmp(WNameOf(NNameOf(n)), MAIN_ENTRY_POINT) == 0) */
						numparams = REG_PARAMS;
#else /* bug TS/1988 09/12/92 */
					if ((be_lexlevel == 1) && (configuring || ((object_file_wrt_flags & OBJ_FILE_WRT_OCCAM_HARNESS) != 0))) {
						numparams = REG_PARAMS;
					}
#endif
					saved_map_state = sp_saveallocvars (need_mapping (n));
					initallocvars (need_mapping (n));	/* added for bug 1061 30/11/90 */

					/* allocate all variables */
					maxwsp = allocvars (0, REG_PARAMS - numparams, FALSE);
					SetNPConstTables (n, consttablechain);
					SetNPCPOffset (n, NVOffsetOf (constantnptr));
					SetNPMaxwsp (n, maxwsp);
					if (needs_quadalign) {
						datasize = (datasize + 1) & (~1);	/* round up to even number */
					}
					if (isfunction) {
						/* FIXME: is this really needed ? (added by me for kroc -P handling) */
						datasize += DS_MIN;
					}
					if (need_mapping (n)) {
						/* allocate the return-address */
						mark_hidden_var (S_CONSTPTR, maxwsp);
					}
					if (reserved_wsmaps) {
						wsmapreserve_t *wsmr;

						for (wsmr = reserved_wsmaps; wsmr; wsmr = wsmr->next) {
							wsmap_hidden_var (wsmr->offset, wsmr->size, wsmr->type);
						}

						clear_reservedwsmaps ();
					}
					SetNPDatasize (n, datasize + maxwsp);
					free_fixedwslist ();
					allocparams (n);
					SetNPLabel (n, NO_LABEL);

					save_wsallocmap (n);
					sp_restoreallocvars (saved_map_state);
					if (diagnostics) {
#ifdef MOBILES
						fprintf (outfile,
							 "Workspace for routine: maxwsp = %d, datasize = %d, maxvsp = %d, mswords = %d\n",
							 maxwsp, datasize, maxvsp, mswords);
#else
						fprintf (outfile,
							 "Workspace for routine: maxwsp = %d, datasize = %d, maxvsp = %d\n",
							 maxwsp, datasize, maxvsp);
#endif
					}
					be_lexlevel--;
					/*}}} */
				}
				if ((sampling_profiling || line_profiling) &&
				    !(isinline (n) || separatelycompiled (n) || is_renamed_proc (n, DValOf (tptr)))) {

					if ((tag == S_PROCDEF) || (tag == S_MPROCDECL)) {
						ProfTabEntry *profcount;

						if (sampling_profiling) {
							profcount = proftab_add_routine_count (profile_table, n);
							SetDVal (tptr, add_profcountupd_nd (profcount, DValOf (tptr)));
						}
						if (line_profiling) {
							SOURCEPOSN locn;
							treenode *proc_process = DValOf (tptr);
							while (isspecification (proc_process))
								proc_process = DBodyOf (proc_process);
							locn = LocnOf (proc_process);
							profcount = proftab_add_line_count (profile_table,
											    (const char *) lookupfilename (FileNumOf (locn)),
											    (INT32) FileLineOf (locn), DValOf (tptr));
							SetDVal (tptr, add_profcountupd_nd (profcount, DValOf (tptr)));
						}
					} else {
						ProfTabEntry *profcount;
						if (sampling_profiling) {
							treenode *val_process = DValOf (tptr);
							profcount = proftab_add_routine_count (profile_table, n);
							while (isspecification (val_process))
								val_process = DBodyOf (val_process);
							SetVLBody (val_process, add_profcountupd_nd (profcount, VLBodyOf (val_process)));
						}
						if (line_profiling) {
							SOURCEPOSN locn;
							treenode *val_process = DValOf (tptr);
							while (isspecification (val_process))
								val_process = DBodyOf (val_process);
							locn = LocnOf (val_process);
							profcount = proftab_add_line_count (profile_table,
											    (const char *) lookupfilename (FileNumOf (locn)),
											    (INT32) FileLineOf (locn), val_process);
							SetVLBody (val_process, add_profcountupd_nd (profcount, VLBodyOf (val_process)));
						}
					}
				}
			}
			tptr = DBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  pragmas */
		case S_PRAGMA:	/* bug 829 19/9/91 */
			switch ((pragma_name_tag_t) NModeOf (DNameOf (tptr))) {
				/*{{{  LINKAGE */
			case pragma_name_linkage:
				{
					const treenode *const exp = DValOf (tptr);
					setup_text_linkage_name ((exp == NULL) ? NULL : CTValOf (exp));
				}
				break;
				/*}}} */
				/*{{{  COMMENT */
			case pragma_name_comment:
				process_hcomment (CTValOf (DValOf (tptr)));
				break;
				/*}}} */

			default:
				break;
			}
			tptr = DBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  other specification */
		case S_DECL:
			/*{{{  initialise all use counts to zero */
			{
				treenode *t = DNameOf (tptr);
				if (TagOf (t) == S_LIST)
					for (; !EndOfList (t); t = NextItem (t)) {
						setusecount (ThisItem (t), 0);
/*(void)isinvectorspace(ThisItem(t)); *//* does SetNMode *//* bug 1156 - now done in trans */
				} else {
					setusecount (t, 0);
/*(void)isinvectorspace(t); *//* does SetNMode *//* bug 1156 - now done in trans */
				}
				mapnestedblocks (DValOf (tptr));	/* bug TS/1263 11/05/92 */
				tptr = DBodyOf (tptr);
			}
			/*}}} */
			break;

		case S_VALABBR:
		case S_VALRETYPE:
		case S_ABBR:
		case S_RETYPE:
			mapnestedblocks (DValOf (tptr));
			tptr = DBodyOf (tptr);
			break;
		case S_WSPLACE:
		case S_VSPLACE:
		case S_TPROTDEF:
		case S_SPROTDEF:
#if MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
#endif
			tptr = DBodyOf (tptr);
			break;
#ifdef OCCAM2_5
		case S_TYPEDECL:
			if (diagnostics)
				/*{{{  display diagnostics for RECORD types */
			{
				treenode *const nptr = DNameOf (tptr);
				treenode *const type = NTypeOf (nptr);
				if (TagOf (type) == S_RECORD) {
					treenode *decl;
					fprintf (outfile, "\nLayout of %sRECORD %s (%d bytes, %d words)\n",
						 ((TypeAttrOf (type) & TypeAttr_packed) != 0) ? "PACKED " : "",
						 WNameOf (NNameOf (nptr)), ARDimOf (type), ARDimOf (type) / bytesperword);
					for (decl = ARTypeOf (type); decl != NULL; decl = DBodyOf (decl)) {
						treenode *const field_name = DNameOf (decl);
						fprintf (outfile, "  size: %3d at: %3d (%3d) %s\n",
							 bytesin (NTypeOf (field_name)),
							 NVOffsetOf (field_name),
							 NVOffsetOf (field_name) / bytesperword, WNameOf (NNameOf (field_name)));
					}
				}
			}
			/*}}} */
			tptr = DBodyOf (tptr);
			break;
#endif
			/*}}} */
			/*}}} */
			/*{{{  SEQ IF ALT PAR PRIALT PRIPAR */
		case S_PAR:
		case S_SEQ:
		case S_IF:
		case S_ALT:
		case S_PRIALT:
		case S_PRIPAR:
		case S_FORKING:
		case S_CLAIM:
			tptr = CBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  REPLIF REPLALT REPLPAR PRIREPLALT PRIREPLPAR */
		case S_REPLPAR:
		case S_REPLIF:
		case S_REPLALT:
		case S_PRIREPLALT:
		case S_PRIREPLPAR:
			mapnestedblocks (ReplCStartExpOf (tptr));
			mapnestedblocks (ReplCLengthExpOf (tptr));
			if (ReplCStepExpOf (tptr)) {
				mapnestedblocks (ReplCStepExpOf (tptr));
			}
			if (parrepl (TagOf (tptr))) {
				be_lexlevel++;
				mapnestedblocks (ReplCBodyOf (tptr));
				be_lexlevel--;
				return;
			}
			tptr = ReplCBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  REPLSEQ */
		case S_REPLSEQ:
			mapnestedblocks (ReplCStartExpOf (tptr));
			mapnestedblocks (ReplCLengthExpOf (tptr));
			if (ReplCStepExpOf (tptr)) {
				mapnestedblocks (ReplCStepExpOf (tptr));
			}
			if (parrepl (TagOf (tptr))) {
				be_lexlevel++;
				mapnestedblocks (ReplCBodyOf (tptr));
				be_lexlevel--;
				return;
			}
			if (line_profiling) {
				SOURCEPOSN locn = LocnOf (ReplCBodyOf (tptr));
				ProfTabEntry *profcount = proftab_add_line_count (profile_table,
										  (const char *) lookupfilename (FileNumOf (locn)),
										  (INT32) FileLineOf (locn),
										  ReplCBodyOf (tptr));
				SetReplCBody (tptr, add_profcountupd_nd (profcount, ReplCBodyOf (tptr)));
			}
			tptr = ReplCBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  WHILE CHOICE */
		case S_WHILE:
		case S_CHOICE:
			mapnestedblocks (CondGuardOf (tptr));
			if (line_profiling) {
				SOURCEPOSN locn = LocnOf (CondBodyOf (tptr));
				ProfTabEntry *profcount = proftab_add_line_count (profile_table,
										  (const char *) lookupfilename (FileNumOf (locn)),
										  (INT32) FileLineOf (locn),
										  CondBodyOf (tptr));
				SetCondBody (tptr, add_profcountupd_nd (profcount, CondBodyOf (tptr)));
			}
			tptr = CondBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  SELECTION */
		case S_SELECTION:
			if (line_profiling) {
				SOURCEPOSN locn = LocnOf (CondBodyOf (tptr));
				ProfTabEntry *profcount = proftab_add_line_count (profile_table,
										  (const char *) lookupfilename (FileNumOf (locn)),
										  (INT32) FileLineOf (locn),
										  CondBodyOf (tptr));
				SetCondBody (tptr, add_profcountupd_nd (profcount, CondBodyOf (tptr)));
			}
			tptr = CondBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  VARIANT */
		case S_VARIANT:
			mapnestedblocks (VRTaggedListOf (tptr));
			tptr = VRBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  ALTERNATIVE */
		case S_ALTERNATIVE:
			mapnestedblocks (AltGuardOf (tptr));
			mapnestedblocks (AltInputOf (tptr));
			if (line_profiling) {
				SOURCEPOSN locn = LocnOf (AltBodyOf (tptr));
				ProfTabEntry *profcount = proftab_add_line_count (profile_table,
										  (const char *) lookupfilename (FileNumOf (locn)),
										  (INT32) FileLineOf (locn),
										  AltBodyOf (tptr));
				SetAltBody (tptr, add_profcountupd_nd (profcount, AltBodyOf (tptr)));
			}
			tptr = AltBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  LIST */
		case S_LIST:
			mapnestedblocks (ThisItem (tptr));
			tptr = NextItem (tptr);
			break;
			/*}}} */
			/*{{{  configuration */
			/*{{{  PLACE */
		case S_PLACE:
			mapnestedblocks (DValOf (tptr));
			tptr = DBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  PROCESSOR */
		case S_PROCESSOR:
			tptr = ProcessorBodyOf (tptr);
			break;
			/*}}} */
			/*}}} */
			/*{{{  PINSTANCE FINSTANCE */
		case S_PINSTANCE:
		case S_FINSTANCE:
			#if 0
			if (cgraph_profiling && !(TagOf (INameOf (tptr)) == N_PREDEFPROC && NModeOf (INameOf (tptr)) == PD_UPDATE_PROFCOUNT))
				proftab_add_call_count (profile_table, tptr);
			#endif
			tptr = IParamListOf (tptr);
			break;
			/*}}} */
			/*{{{  action */
			/*{{{  ASS OUTPUT INPUT TAGGED_INPUT DELAYED_INPUT CASE CASEINPUT */
		case S_ASS:
		case S_OUTPUT:
		case S_INPUT:
		case S_TAGGED_INPUT:
		case S_DELAYED_INPUT:
		case S_CASE:
		case S_CASE_INPUT:
			mapnestedblocks (LHSOf (tptr));
			tptr = RHSOf (tptr);
			break;
			/*}}} */
			/*{{{  X_INPUT X_TAGGED_INPUT */
		case S_X_INPUT:
		case S_X_TAGGED_INPUT:
			mapnestedblocks (LHSOf (tptr));
			mapnestedblocks (RHSOf (tptr));
			mapnestedblocks (ActionDuringOf (tptr));
			tptr = ActionAfterOf (tptr);
			break;
			/*}}}  */
			/*}}} */
			/*{{{  monadic */
		case S_NEG:
		case S_BITNOT:
		case S_UMINUS:
		case S_NOT:
			/* SIZE ELSIZE and SEGSTART all return */
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			tptr = OpOf (tptr);
			break;
#if 0				/* no constructors are passed to the backend any more */
		case S_CONSTRUCTOR:
			tptr = LitExpOf (tptr);
			break;
#endif
			/*}}} */
			/*{{{  dyadic */
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
		case S_COLON2:
			mapnestedblocks (LeftOpOf (tptr));
			tptr = RightOpOf (tptr);
			break;
		case S_AFTER:
			assert (FALSE);	/* after is processd in trans */
			break;

			/*}}} */
			/*{{{  valof */
		case S_VALOF:
			mapnestedblocks (VLBodyOf (tptr));
			tptr = VLResultListOf (tptr);
			break;
			/*}}} */
			/*{{{  subscript */
		case S_ARRAYSUB:
		case S_ARRAYITEM:
		case S_RECORDSUB:
		case S_RECORDITEM:
			mapnestedblocks (ASBaseOf (tptr));
			tptr = ASExpOf (tptr);
			break;
			/*}}} */
			/*{{{  segment */
		case S_SEGMENTITEM:
		case S_SEGMENT:
			mapnestedblocks (SNameOf (tptr));
			mapnestedblocks (SStartExpOf (tptr));
			tptr = SLengthExpOf (tptr);
			break;
			/*}}} */
			/*{{{  temporary */
		case T_TEMP:
		case T_PREEVALTEMP:
			tptr = NDeclOf (tptr);
			break;
			/*}}} */
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void mapmain(tptr)*/
/* Allocate workspace offsets for main body */
PUBLIC void mapmain (treenode * tptr)
{
	consttablechain = NULL;

	/* when configuring, we don't need a new copy for every processor */
	if (constantnptr == NULL) {
		constantnptr = newnamenode (T_TEMP, NOPOSN, tempname_p, newleafnode (S_INT, NOPOSN), NULL, 0, 0, NM_DEFAULT);
	}
	staticlinkused = STATICLINK_NOT_USED;
	if (sampling_profiling || line_profiling || cgraph_profiling) {
		profile_table = proftab_create (
						((sampling_profiling ? ProfRoutineInfo : 0) |
						 (cgraph_profiling ? ProfCallInfo : 0) |
						 (line_profiling ? ProfLineInfo : 0)), profi_memalloc, profi_memfree, NULL);
	} else {
		profile_table = NULL;
	}
#ifdef COMMENT
	if (usedags) {
		initdfa ();
	}
#endif
	initvsp (0);
#ifdef MOBILES
	initmobilespace ();
#endif
	datasize = DS_MIN;
	be_lexlevel = 0;
	templist = NULL;

	mapnestedblocks (tptr);
}

/*}}}*/

/*{{{  PRIVATE void clearlibentries*/
PRIVATE void clearlibentries (stdlibentry_t * libentries)
{
	/* stdlibentries holds a list of entries in the reverse order to
	   the order in which they were read in.
	   A tree still exists which holds a copy of all the stuff which was
	   read in.
	   This piece of code points decltree at the original tree so that it
	   can be cleaned up, also cleans up the stdlibentries list at the
	   same time.
	   CON 8/1/91
	 */
	while (libentries != NULL) {
		stdlibentry_t *const this = libentries;
		treenode *decltree = NDeclOf (libentries->l_name);
		libentries = libentries->l_next;
		SetDBody (decltree, NULL);
		freetree (&decltree);
		freevec (this, sizeof (stdlibentry_t));
	}
}

/*}}}*/
/*{{{  PUBLIC void initbind(void)*/
PUBLIC void initbind (void)
{
	/* we re-load these for each invocation, if configuring, because their
	   formal parameter lists might be augmented differently.
	 */
	clearlibentries (stdlibentries);
	stdlibentries = NULL;
	stdlibsloaded = FALSE;

	if (vlibentries != NULL) {
		clearlibentries (vlibentries);
		vlibentries = NULL;
	}
	vlibsloaded = FALSE;
}

/*}}}*/
