/* $Id: gen1.c,v 1.13 1997/12/12 16:49:55 mdp2 Exp $ */

/*
 *	code generator - main driver
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
 *  Code generator gen1 - main driver
 *
 *  MDP changes:
 *               add calls to throw_the_result_away where nothing left on tstack
 *               replace use of LDPI by calls to genloadlabptr
 *  additionally for alpha: (using boolean quadalign)
 *               constant table generation modified to generate quadword scalars
 *               separately before other scalars.
 *
 * frmb2 changes:
 *		15/10/2000 -- added code to handle STEP in replicator expressions
 *		--/06/2001 -- mobiles support
 *		--/09/2001 -- dynamic replicated PAR support
 */

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include "includes.h"
#include "instdef.h"
#include "instruct.h"
#include "genhdr.h"
#include "generror.h"
#include "bind1def.h"
#include "bind2def.h"
#include "bind3def.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen4def.h"
#include "gen7def.h"
#include "gen8def.h"
#include "gen9def.h"
#include "gen10def.h"
#include "gen11def.h"
#include "gen12def.h"
#include "code1def.h"
#include "debugdef.h"
#include "objlib.h"
#include "objwrt.h"
#include "profile.h"
#include "predefhd.h"
#include "genkroc.h"
#include "mobiles.h"		/* for mobile_getanontypes() */
#include "mobile_types.h"
/*}}}*/

/*{{{  global variables*/
PUBLIC SOURCEPOSN genlocn;
PUBLIC BIT32 vspoffset;		/* offset of vectorspace pointer in workspace */
PUBLIC BIT32 fbpoffset;		/* offset of fork barrier in workspace */
PUBLIC int fbp_lexlevel;	/* lexical level of fbpoffset */
#ifdef MOBILES
PUBLIC BIT32 mspoffset;		/* offset of mobilespace pointer in workspace */
PUBLIC BIT32 mppoffset;		/* offset of mobile process pointer in workspace */
PUBLIC int mpp_lexlevel;	/* lexical level of mppoffset */
#endif
PUBLIC BIT32 constptr;		/* Offset of constant pointer in workspace */
PUBLIC wordnode *tempname_p = NULL;

PUBLIC int instancedlevel;
PUBLIC int be_lexlevel;

PUBLIC BOOL insidealtguard;	/* TRUE when we are mapping, or code generating
				   an alternative Boolean guard. */
PUBLIC int alt_ds;

PUBLIC BOOL inside_asm;		/* Used to stop error handling while in ASM or GUY */
PUBLIC BOOL disable_csub0 = FALSE;	/* Used to prevent checking when disabling ALTs */

PUBLIC BOOL use_bsub_not_adc = FALSE;	/* For addressing operations */
PUBLIC int pripar_trap_offset = DEFAULT_HIGH_PRI_TRAP_OFFSET;
PUBLIC treenode *current_routine_nptr;
/*}}}*/

/*{{{  local variables*/
/* Label marking the start of the scalar constant table */
PRIVATE int scalarconstlab;
/* Label marking the start of the profiler count table */
PRIVATE int proftabconstlab;
PRIVATE treenode *globaltablechain;

PRIVATE const char *tempname = "temporary";

PRIVATE treenode *replaltvar = NULL;
PRIVATE int replaltlevel;
PRIVATE treenode *inside_suspends = NULL;	/* non-null if generating code inside a [MOBILE] PROC that SUSPENDs */
/*}}}*/

/*{{{  forward definitions*/
PRIVATE void tnestedroutines (treenode * tptr);
PRIVATE void tif (treenode * tptr, int *trueguard, int endlabel, BOOL outer_level);
/*}}}*/

/*{{{  PRIVATE void tguy (tptr)*/
/*****************************************************************************
 *
 *  tguy generates code for a GUY tree
 *
 *****************************************************************************/
PRIVATE void tguy_or_asm (treenode * tptr, const BOOL guy_not_asm)
{
	inside_asm = TRUE;
	genlocn = LocnOf (tptr);
	new_occam_line (tptr, TRUE, need_locate (TagOf (tptr)), FALSE);
	/* if (debugoutput && need_locate(TagOf(tptr)))
	   coder_genlocate(tptr, TRUE); */
	switch (TagOf (tptr)) {
		/*{{{  S_LABELDEF */
	case S_LABELDEF:
		setlab ((int) NVOffsetOf (DNameOf (tptr)));
		break;
		/*}}} */
		/*{{{  S_GUYSTEP */
	case S_GUYSTEP:
		gensecondary (DOpTypeOf (tptr) | I_STEP_BIT);
		break;
		/*}}} */
		/*{{{  S_GUYCODE */
	case S_GUYCODE:
		{
			treenode *operand = RightOpOf (tptr);
			int instruction = DOpTypeOf (tptr);
			operand = foldexplist (operand);	/* fold in any special ASM names */

			if (instruction & I_PRIMARY) {
				/*{{{  generate a primary instruction */
				instruction &= ~I_PRIMARY;
				operand = ThisItem (operand);	/* Only one in list for a primary */
				switch (TagOf (operand)) {
					/*{{{  S_CONSTEXP */
				case S_CONSTEXP:
					genprimary (instruction, LoValOf (operand));
					break;
					/*}}} */
					/*{{{  N_LABELDEF */
				case N_LABELDEF:
					genbranch (instruction, (int) NVOffsetOf (operand));
					break;
					/*}}} */
					/*{{{  Routine name */
				case N_PROCDEF:
				case N_MPROCDECL:
				case N_LIBPROCDEF:	/* bug TS/1240 14/04/92 */
				case N_LIBMPROCDECL:
					genbranch (instruction, NPLabelOf (operand));
					break;
				case N_SFUNCDEF:
				case N_LFUNCDEF:
				case N_LIBFUNCDEF:
					genbranch (instruction, NPLabelOf (operand));
					/*{{{  note function results in registers */
					/* THIS CODE NOT FULLY CHECKED YET - MDP */
					if (fn_return_style (operand) == return_style_fpu) {
						treenode *rtype = ThisItem (FnTypeListOf (NTypeOf (operand)));
						notefpresult (ntypeof (rtype) == S_REAL32 ? "s" : "d");
					} else {
						int regsused = 0;
						treenode *ftypelist = FnTypeListOf (NTypeOf (operand));
						while (!EndOfList (ftypelist) && (regsused < MAXREGS)) {
							if (result_return_style (ThisItem (ftypelist)) == return_style_alu)
								regsused++;
							ftypelist = NextItem (ftypelist);
						}
						gen_func_results (regsused);
					}
					/*}}} */
					break;
					/*}}} */
					/*{{{  other name or ARRAYITEM */
				case N_DECL:
				case N_REPL:
				case N_VALABBR:
				case N_ABBR:
				case N_RETYPE:
				case N_VALRETYPE:
				case N_PARAM:
				case N_VALPARAM:
				case N_RESULTPARAM:
				case S_ARRAYITEM:
				case S_CONSTCONSTRUCTOR:
				case S_RECORDITEM:
					if (guy_not_asm) {
						if (chanaspointer && (basetype (gettype (operand)) == S_CHAN))
							generr (GEN_GUY_NO_CHANS);
						switch (instruction) {
						case I_LDL:
						case I_LDNL:
							loadelement (operand, 0, MANY_REGS, TRUE);
							break;
						case I_LDLP:
						case I_LDNLP:
							loadelementpointer (operand, 0, MANY_REGS);
							break;
						case I_STL:
						case I_STNL:
							storeinelement (operand, 0, MANY_REGS);
							break;
						default:
							DEBUG_MSG (("got a symbolic name for a primary\n"));
							generr_s (GEN_BAD_PRIMARY_OPERAND, WNameOf ((wordnode *) LeftOpOf (tptr)));
							break;
						}
					} else {
						if (TagOf (operand) == S_ARRAYITEM || TagOf (operand) == S_RECORDITEM)
							generr_s (GEN_BAD_PRIMARY_OPERAND, WNameOf ((wordnode *) LeftOpOf (tptr)));
						genprimary (instruction, NVOffsetOf (operand) + nameoffsetof (NLexLevelOf (operand)));
						gencomment0 (WNameOf (NNameOf (operand)));
					}
					break;
					/*}}} */
					/*{{{  error */
				default:
					/*badtag(genlocn, TagOf(tptr), "tguy-primary"); */
					if (wouldbeconst (operand))
						/* must have failed to fold a special ASM name */
						/* This will fail with a sensible error message */
						texp (operand, MANY_REGS);
					else
						generr_s (GEN_BAD_PRIMARY_OPERAND, WNameOf ((wordnode *) LeftOpOf (tptr)));
					/*}}} */
				}
				/*}}} */
			} else if ((instruction & I_PSEUDO_OP) && (((instruction & INST_MASK) == I_BYTE) || ((instruction & INST_MASK) == I_WORD))) {	
				/*{{{  generate the BYTE or WORD values */
				/* these can have any number of operands */
				while (operand != NULL) {
					switch (TagOf (ThisItem (operand))) {
					case S_CONSTEXP:
						{
							INT32 val = LoValOf (ThisItem (operand));
							BYTE buf[4];
							int i;
							int len = (instruction & INST_MASK) == I_BYTE ? 1 : bytesperword;
							int swap = len == bytesperword ? targetintsize : S_BYTE;

							for (i = 0; i < len; i++) {
								buf[i] = (BYTE) (val & 0xff);
								val >>= 8;
							}

							switch (nodetypeoftag (TagOf (CExpOf (ThisItem (operand))))) {
							case NAMENODE:
								add_const_block (len, &buf[0], NTypeOf (CExpOf (ThisItem (operand))), "GUY table", swap);	/* MDP */
								break;
							case LITNODE:
								add_const_block (len, &buf[0], LitTypeOf (CExpOf (ThisItem (operand))), "GUY table", swap);	/* MDP */
								break;
							default:
								add_const_block (len, &buf[0], NULL, "GUY table", swap);	/* MDP */
								break;
							}
						}
						break;
					case S_STRING:
					case S_CONSTCONSTRUCTOR:
						{
							wordnode *const s = (wordnode *) CTValOf (ThisItem (operand));
							add_const_block (WLengthOf (s), (const BYTE *) WNameOf (s), NULL, "GUY STRING or ?", S_BYTE);	/* MDP */
						}
						break;
					case N_VALABBR:
					case N_VALRETYPE:
						/* Must be a name of a constant array */
						{
							wordnode *const s = (wordnode *) CTValOf (DValOf (NDeclOf (ThisItem (operand))));
							add_const_block (WLengthOf (s), (const BYTE *) WNameOf (s), NULL, "GUY VALABBR or ?", targetintsize);	/* MDP */
						}
						break;
					default:
						badtag (genlocn, TagOf (ThisItem (operand)), "tguy-BYTE/WORD");
					}
					operand = NextItem (operand);
				}
				/*}}} */
			} else if (instruction & I_PSEUDO_OP) {
				/*{{{  generate other pseudo ops */
				treenode **operands[MAXREGS];
				int opmodes[MAXREGS];
				int ops = setup_asm_operands (operand, operands, opmodes);
				switch (instruction & INST_MASK) {
				case I_LD:
					texpopd (opmodes[0], *operands[0], MANY_REGS);
					break;
				case I_LDAB:
					tload2regs (opmodes[0], *operands[0], opmodes[1], *operands[1], FALSE, TRUE);
					break;
				case I_LDABC:
					{
						int loadseq, preeval1, preeval2;
						loadseq = giveorder (opmodes[0], *operands[0],
								     opmodes[1], *operands[1], opmodes[2], *operands[2], &preeval1, &preeval2);
						tload3regs (opmodes[0], *operands[0],
							    opmodes[1], *operands[1], opmodes[2], *operands[2], loadseq, TRUE);
					}
					break;
				case I_ST:
				case I_STAB:
				case I_STABC:
					tstoreregs (operands, ops);
					break;
				case I_AJWRET:
					genprimary (I_AJW, asmvalues[ASMNAME_WSSIZE]);
					gensecondary (I_RET);
					break;
				case I_LDRETP:
					genprimary (I_LDLP, asmvalues[ASMNAME_WSSIZE]);
					break;
				case I_LDLABELDIFF:
					genlabeldiff (I_LDC, (int) NVOffsetOf (*operands[1]), (int) NVOffsetOf (*operands[0]));
					break;
				case I_LDLABELP:
#if 0
fprintf (stderr, "gen1: I_LDLABELP: *operands[0] = ");
printtreenl (stderr, 4, *operands[0]);
#endif
					genloadlabptr ((int)NVOffsetOf (*operands[0]), NOLAB, "LDLABELP");
					break;
				case I_RESERVELOWWS:	/* bug TS/1166 14/04/92 */
					break;	/* The work is done in the mapper */
				case I_THROWAWAY:
					throw_the_result_away ();
					break;
				case I_WSMAP:
					break;	/* work is done in the mapper */
				case I_CODEMAP:
					gencomment0 (".MAGIC CODEMAP");
					break;
				default:
					badtag (genlocn, TagOf (tptr), "tguy-pseudo");
				}
				/*}}} */
			} else if (instruction & I_FPU_ENTRY_BIT) {
				genfpuentry (instruction);
			} else {
				gensecondary (instruction);
			}
		}
		break;
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "tguy");
	}
	inside_asm = FALSE;
}
PRIVATEPARAM void tguy (treenode * tptr, void *voidptr)
{
	voidptr = voidptr;
	tguy_or_asm (tptr, TRUE);
}
PRIVATEPARAM void tasm (treenode * tptr, void *voidptr)
{
	voidptr = voidptr;
	tguy_or_asm (tptr, FALSE);
}

/*}}}*/
/*{{{  PRIVATE void tdecl_mark_debug_info*/
PRIVATE void tdecl_mark_debug_info (treenode * const nptr, BOOL * const debug_marked)
{
	/* This writes a marker for the debug info,
	   for thne initialisation of a variable,
	   as long as we haven't already done so.
	   CON - 27/10/92 bug TS/1870
	 */
	new_occam_line (NDeclOf (nptr), TRUE, !*debug_marked, TRUE);
	if (!*debug_marked && debugoutput) {
		/* coder_genlocate(NDeclOf(nptr), TRUE); */
		*debug_marked = TRUE;
	}
}

/*}}}*/
/*{{{  PRIVATE void tdecl_init_from_vsptr*/
PRIVATE void tdecl_init_from_vsptr (treenode * const nptr, BOOL * const debug_marked)
{
	if (isinvectorspace (nptr) && (vspoffset != NVOffsetOf (nptr)))
		/* if (vspoffset == NVOffset), the variable has been overlaid onto the
		   vectorspace pointer */
	{
		treenode *derived_nptr;
		tdecl_mark_debug_info (nptr, debug_marked);

		derived_nptr = NVNextTempOf (nptr);
		while (derived_nptr != NULL && NVUseCountOf (derived_nptr) == 0)
			derived_nptr = NVNextTempOf (derived_nptr);

		if (derived_nptr == NULL)	/* bug INSdi01511 */
			loadnewvsp (NVVSOffsetOf (nptr));
		else {
			/*printf("tdecl_init_from_vsptr: NVNextTemp for %s is %s\n",
			   WNameOf(NNameOf(nptr)), WNameOf(NNameOf(derived_nptr))); */

			loadnamepointer (derived_nptr, 0);
			genprimary (I_LDNLP, NVVSOffsetOf (nptr) - NVVSOffsetOf (derived_nptr));
			gencomment0 ("vsp offset");
		}
		storeinname (nptr, 0);
	}
}

/*}}}*/
/*{{{  PRIVATE void tdecl_init_chanarray*/
PRIVATE void tdecl_init_chanarray (treenode * const nptr, BOOL * const debug_marked)
{
	treenode *const type = NTypeOf (nptr);
#ifdef RECORDS_OF_CHANNELS
	const INT32 e = allocsize (nptr) / 2;
#else
	const INT32 e = elementsin (type);
#endif
	tdecl_mark_debug_info (nptr, debug_marked);
	if (complexinitialise (type)) {
		/*{{{  generate a loop to initialise the variable */
		loadnamepointer (nptr, 0);	/*          ldlp name      */
		gen_init_chanarray (1, 1, e);	/* generate optimised loop */
		/*}}} */
	} else {
		/*{{{  initialise each element individually */
		INT32 i, offset = 0;
		if (chanaspointer) {
			/*{{{  init pointers */
			for (i = 0; i < e; i++) {
				if (ispointer (nptr)) {
					loadnamepointer (nptr, 0);
					genprimary (I_LDNLP, i + e);
					loadnamepointer (nptr, 0);
					genprimary (I_STNL, i);
				} else {
					loadnamepointer (nptr, i + e);
					storeinname (nptr, i);
				}
				gencomment0 ("init channel pointer");
			}
			/*}}} */
			offset = e;
		}
		for (i = 0; i < e; i++) {
			gennotprocess ();
			/* looks OK for T9000_alpha_badmint bug */
			if (ispointer (nptr)) {
				loadnamepointer (nptr, 0);
				genprimary (I_STNL, i + offset);
			} else
				storeinname (nptr, i + offset);
			gencomment0 ("init channel word");
		}
		/*}}} */
	}
}

/*}}}*/
#ifdef MOBILES
#if 0
/*{{{  PRIVATE void tmobilescopein_nested (treenode *const nptr)*/
PRIVATE void tmobilescopein_nested (treenode *const nptr)
{
	treenode *type, *decl;

	if (TagOf (NTypeOf (nptr)) != N_TYPEDECL) {
		return;
	}
	type = follow_user_type (NTypeOf (nptr));
	if (TagOf (type) != S_MOBILE) {
		return;
	}
	type = MTypeOf (type);
	if (TagOf (type) != S_RECORD) {
		return;
	}
	decl = ARTypeOf (type);
	while (decl && (TagOf (decl) == S_DECL)) {
		treenode *name = DNameOf (decl);

		if (isdynmobilearray (name)) {
#if 0
fprintf (stderr, "tmobilescopein_nested: found dynmobile array NVOffsetOf (name) = %d\n", NVOffsetOf (name));
#endif
			gensecondary (I_MINT);
			loadmobile (nptr);		/* load base */
			genprimary (I_STNL, (NVOffsetOf (name) / bytesperword));
			genprimary (I_LDC, 0);
			loadmobile (nptr);		/* load base */
			genprimary (I_STNL, (NVOffsetOf (name) / bytesperword) + 1);
		}
		decl = DBodyOf (decl);
	}
	return;
}
/*}}}*/
#endif
/*{{{  PRIVATE void tmobilescopein (treenode *const nptr, treenode *const type)*/
PRIVATE void tmobilescopein (treenode *const nptr, treenode *const type)
{
#if 0
fprintf (stderr, "tmobilescopein: nptr = ");
printtreenl (stderr, 4, nptr);
#endif
	if (isdynmobilearray (nptr)) {
		/*{{{  scope in dynamic mobile array*/
		gencomment0 ("dynamic MOBILE scope in");
		gencleardynarray (nptr, FALSE);
		/*}}}*/
	} else if (isdynmobilebarrier (nptr)) {
		/*{{{  scope in dynamic mobile barrier*/
		gencomment0 ("dynamic MOBILE BARRIER scope in");
		gencleardynbarriertype (nptr);
		/*}}}*/
	} else if (isdynmobilechantype (nptr)) {
		/*{{{  scope in dynamic mobile channel-type*/
#if 0
fprintf (stderr, "tmobilescopein: scoping in dynamic mobile channel.\n");
#endif
		gencomment0 ("dynamic MOBILE channel scope in");
		gencleardynchantype (nptr);
		/*}}}*/
	} else if (isdynmobileproctype (nptr)) {
		/*{{{  scope in dynamic mobile process-type*/
		gencomment0 ("dynamic MOBILE process scope in");
		gencleardynproctype (nptr);
		/*}}}*/
	} else {
		/*{{{  scope in static mobile*/
		/* treenode *mtype = NTypeOf (nptr); */
		const int n_bytes = bytesin (MTypeOf (type));

		gencomment0 ("MOBILE scope in");
		#if 0
		if (enable_mobilespace) {
			loadmsp ();
			genprimary (I_LDNL, NVMSOffsetOf (nptr));
		} else {
			genprimary (I_LDC, n_bytes);
			gensecondary (I_MALLOC);
		}
		#endif
		loadconstant (n_bytes);
		gensecondary (I_MALLOC);
		storemobile (nptr);
		#if 0
		/* maybe need to initialise sub-fields (mobile-init does this anyhow, but we do it again anyway) */
		gencomment0 ("MOBILE scope in (nested)");
		tmobilescopein_nested (nptr);
		#endif
		/*}}}*/
	}
	return;
}
/*}}}*/
#if 0
/*{{{  PRIVATE void tmobilescopeout_nested (treenode *const nptr)*/
PRIVATE void tmobilescopeout_nested (treenode *const nptr)
{
	treenode *type, *decl;

	if (TagOf (NTypeOf (nptr)) != N_TYPEDECL) {
		return;
	}
	type = follow_user_type (NTypeOf (nptr));
	if (TagOf (type) != S_MOBILE) {
		return;
	}
	type = MTypeOf (type);
	if (TagOf (type) != S_RECORD) {
		return;
	}
	decl = ARTypeOf (type);
	while (decl && (TagOf (decl) == S_DECL)) {
		treenode *name = DNameOf (decl);

		if (isdynmobilearray (name)) {
			int skiplab = newlab ();
			/* better conditional-free */

			loadmobile (nptr);
			genprimary (I_LDNL, (NVOffsetOf (name) / bytesperword) + 1);	/* load first dimension count */
			genbranch (I_CJ, skiplab);
			loadmobile (nptr);
			genprimary (I_LDNL, NVOffsetOf (name) / bytesperword);		/* load pointer */
			gensecondary (I_MRELEASE);
			genprimary (I_LDC, 0);
			loadmobile (nptr);
			genprimary (I_STNL, (NVOffsetOf (name) / bytesperword) + 1);	/* set size = 0 */
			setlab (skiplab);
		}
		decl = DBodyOf (decl);
	}
	return;
}
/*}}}*/
#endif
/*{{{  PRIVATE void tmobilescopeout (treenode *const nptr)*/
PRIVATE void tmobilescopeout (treenode *const nptr)
{
	if (isdynmobilearray (nptr)) {
		/*{{{  dynamic mobile array descope*/
		#if 0
		int skiplab = newlab ();
		treenode *subtype = MTypeOf (follow_user_type (NTypeOf (nptr)));
		int dimcount = 0;

		/* subtype is definitely a array type */
		while (TagOf (subtype) == S_ARRAY) {
			dimcount++;
			subtype = ARTypeOf (subtype);
		}
		/* oki, array of what exactly */
		subtype = follow_user_type (subtype);
		#endif

		gencomment0 ("dynamic MOBILE array scope out");
		gencondfreedynmobile (nptr);
		/*}}}*/
	} else if (isdynmobilebarrier (nptr)) {
		/*{{{  dynamic mobile BARRIER descope*/
		gencomment0 ("dynamic MOBILE barrier scope out");
		gencondfreedynbarriertype (nptr, TRUE);
		/*}}}*/
	} else if (isdynmobilechantype (nptr)) {
		/*{{{  dynamic mobile chan-type descope*/
		gencomment0 ("dynamic MOBILE channel scope out");
		gencondfreedynchantype (nptr);
		/*}}}*/
	} else if (isdynmobileproctype (nptr)) {
		/*{{{  dynamic mobile process descope*/
		gencomment0 ("dynamic MOBILE process scope out");
		gencondfreedynproctype (nptr);
		/*}}}*/
	} else {
		/*{{{  ordinary mobile scope out*/
		#if 0
		gencomment0 ("MOBILE scope out (nested)");
		tmobilescopeout_nested (nptr);
		#endif
		gencomment0 ("MOBILE scope out");
		loadmobile_real (nptr);
		#if 0
		if (enable_mobilespace) {
			loadmsp ();
			genprimary (I_STNL, NVMSOffsetOf (nptr));
		} else {
			gensecondary (I_MRELEASE);
		}
		#endif
		gensecondary (I_MRELEASE);
		/*}}}*/
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void tdedecl (treenode *const nptr)*/
PRIVATE void tdedecl (treenode *const nptr)
{
#if 0
printf ("tdedecl: nptr = ");
printtree (stdout, 4, nptr);
printf ("\n");
#endif
	if ((TagOf (nptr) == N_DECL) && (NVUseCountOf (nptr) != 0) && !isplaced (nptr)) {
		treenode *const type = follow_user_type (NTypeOf (nptr));

		if (DValOf (NDeclOf (nptr)) == NULL) {
			switch (TagOf (type)) {
				case S_MOBILE:
					tmobilescopeout (nptr);
					break;
				case S_ANYCHANTYPE:
					gencomment0 ("dynamic MOBILE channel scope out");
					gencondfreedynchantype (nptr);
					break;
				case S_ANYPROCTYPE:
					gencomment0 ("dynamic MOBILE process scope out");
					gencondfreedynproctype (nptr);
					break;
				case S_FULLBARRIER:
					gencomment0 ("free FULLBARRIER");
					tfreebarrier (nptr);
					break;
				default:
					break;
			}
		}
	}
}
/*}}}*/
#endif	/* MOBILES */
/*{{{  PRIVATE void tdecl (nptr)*/
/*****************************************************************************
 *
 *  tdecl translates a declaration, outputting code to initialise a variable,
 *        if neccessary.
 *
 *****************************************************************************/
PRIVATE void tdecl (treenode *const nptr, BOOL *const debug_marked)
{
	switch (TagOf (nptr)) {
		/*{{{  N_DECL */
	case N_DECL:
#if 0
fprintf (stderr, "tdecl: N_DECL: NVUseCountOf = %d, nptr = ", NVUseCountOf (nptr));
printtreenl (stderr, 4, nptr);
#endif
		if ((NVUseCountOf (nptr) != 0) && !isplaced (nptr)) {
			treenode *const type = follow_user_type (NTypeOf (nptr));
			tdecl_init_from_vsptr (nptr, debug_marked);
			/* if there is an explicit initialisation (eg constructors)
			   then we don't do the default stuff.
			 */
			if (DValOf (NDeclOf (nptr)) == NULL)	/* bug TS/1735 12/06/92 */
				switch (TagOf (type)) {
					/*{{{  CHAN initialised to NotProcess */
				case S_CHAN:
					tdecl_mark_debug_info (nptr, debug_marked);
					/* Generate code to initialise channel */
					if (!chanaspointer || issimplechan (nptr)) {
						gennotprocess ();
						/* looks OK for T9000_alpha_badmint bug */
						storeinname (nptr, 0);
						gencomment0 ("init channel word");
					} else {
						gennotprocess ();
						/* looks OK for T9000_alpha_badmint bug */
						storeinname (nptr, 1);
						gencomment0 ("init channel word");
						loadnamepointer (nptr, 1);
						storeinname (nptr, 0);
						gencomment0 ("init channel pointer");
					}
					break;
					/*}}} */
					/*{{{  MOBILES allocated on declaration for now... */
#ifdef MOBILES
				case S_MOBILE:
					tmobilescopein (nptr, type);
					break;
#endif
					/*}}}  */
					/*{{{  BOOL BYTE initialised to zero */
				case S_BOOL:
				case S_BYTE:
					tdecl_mark_debug_info (nptr, debug_marked);
					zero_local_var (nptr);
					break;
					/*}}} */
					/*{{{  ARRAY if CHAN initialise each element to MINT */
				case S_ARRAY:
#ifdef RECORDS_OF_CHANNELS
				case S_RECORD:
#endif
					if (channel_basetype (type))
						tdecl_init_chanarray (nptr, debug_marked);
					break;
					/*}}} */
					/*{{{  ANYCHANTYPE (MOBILE.CHAN), initialise suitably*/
				case S_ANYCHANTYPE:
					gencomment0 ("dynamic MOBILE channel scope in");
					/* initialise chan-type pointer to NotProcess */
					gensecondary (I_NULL);
					storemobile (nptr);
					/* storeinname (nptr, 0); */
					break;
					/*}}}*/
					/*{{{  ANYPROCTYPE (MOBILE.PROC), initialise suitably*/
				case S_ANYPROCTYPE:
					gencomment0 ("dynamic MOBILE process scope in");
					/* initialise pointer to NotProcess */
					gensecondary (I_NULL);
					storemobile (nptr);
					/*}}}*/
					/*{{{  FULLBARRIER allowed, initialise and enroll this process*/
				case S_FULLBARRIER:
					gencomment0 ("initialising FULLBARRIER with 1 process");
					loadconstant (0);
					loadconstant (MT_MAKE_BARRIER (MT_BARRIER_FULL));
					gensecondary (I_MT_ALLOC);
					storemobile (nptr);
					/* tinitbarrier (nptr); */
					/* tenroll (nptr, 1); */
					break;
					/*}}}*/
				default:
					break;
				}
		}
		break;
		/*}}} */
	default:
		badtag (genlocn, TagOf (nptr), "tdecl");
	}
}

/*}}}*/
/*{{{  PRIVATE int align_requirement_of_type*/
PRIVATE int align_requirement_of_type (treenode *const type)
{
	const int base = basetype (type);
#ifdef OCCAM2_5
	return (base == S_RECORD) ? bytesperword : bytesinscalar (base);
#else
	return bytesinscalar (base);
#endif
}

/*}}}*/
/*{{{  PRIVATE int align_requirement()*/
PRIVATE int align_requirement (treenode * const lhstype, treenode * const rhstype, treenode * const rhs)
{
	const int newsize = align_requirement_of_type (lhstype);
	const int oldsize = align_requirement_of_type (rhstype);

	if ((errormode & ERRORMODE_ALIGNCHECK) && (oldsize < newsize) && (
										 /* byte alignment going to half word alignment: */
										 (newsize < bytesperword) ||
										 /* short alignment going to word alignment: */
										 (oldsize < bytesperword && newsize >= bytesperword)
	    )) {
		/* newsize will be either 2, 4, or 8, because oldsize < newsize */
		const int alignsize = (int) min_INT32 (newsize, bytesperword);	/* half word or word */
		if (check_aligned (rhs, alignsize))
			return 1;	/* means we don't need to check alignment */
		return alignsize;
	}
	return 1;		/* align to any byte boundary */
}

/*}}}*/
/*{{{  PRIVATE void genaligncheck*/
PRIVATE void genaligncheck (const int alignment)
{
	if (!tx_global.hastserieserrors) {
		/* T9 etc - just trap by reading via the pointer */
		if (alignment == bytesperword)
			genprimary (I_LDNL, 0);	/* rely on the trap bit */
		else {
			if (alignment != 2 || !has_shortintops) {
				err_abort ("genaligncheck");
			}
			gensecondary (I_LS);	/* rely on the trap bit */
		}
	} else {
		loadconstant ((int) (alignment - 1));
		gensecondary (I_AND);
		loadconstant (1);
		gensecondary (I_CSUB0);
		throw_the_result_away ();
	}
	gencomment0 ("alignment check");
	checkerror ();		/* added 22/6/90 by CO'N */

	if ((warning_flags & WARNING_ALIGNMENT) != 0)
		genwarn (GEN_ALIGNMENT_CHECK);
}

/*}}}*/
#ifdef MOBILES
/*{{{  PRIVATE void tdespecification (treenode *tptr)*/
PRIVATE void tdespecification (treenode *tptr)
{
#if 0
printf ("tdespecification..()\n");
#endif
	switch (TagOf (tptr)) {
	case S_DECL:
		{
			treenode *t = DNameOf (tptr);

			new_occam_line (tptr, TRUE, TRUE, FALSE);
			if (TagOf (t) == S_LIST) {
				for (; !EndOfList (t); t = NextItem (t)) {
					tdedecl (ThisItem (t));
				}
			} else {
				tdedecl (t);
			}
		}
		break;
	case S_ABBR:
		{
			const abbrevmode_t am = be_abbrevmode (tptr);
			treenode *nptr = DNameOf (tptr);
			/* treenode *lhstype = follow_user_type (NTypeOf (nptr)); */
			/* treenode *rhstype = gettype_main_orig (DValOf (tptr)); */

			if (am == AM_PTR) {
				/* only interested in MOBILEs, and they're all pointers.  ignore ISRHS! */
				if (isdynmobilearray (DValOf (tptr)) && isdynmobilearray (nptr)) {
					/* need to de-scope this back into the RHS */
					/* int i, ndim; */

					new_occam_line (tptr, TRUE, TRUE, FALSE);
					/* ndim = dynmobiledimensioncount (lhstype); */
#if 0
fprintf (stderr, "gen1: tdespecification: dynamic MOBILE array abbreviation.  ndim = %d, lhstype = ", ndim);
printtreenl (stderr, 4, lhstype);
#endif
					loadmobile_real (nptr);
					storemobile (DValOf (tptr));
					genmobileunpack ((DValOf (tptr)), FALSE, TRUE);
				} else if (isdynmobilechantype (nptr)) {
					/* de-scope back to the RHS */
					new_occam_line (tptr, TRUE, TRUE, FALSE);
#if 0
fprintf (stderr, "gen1: tdespecification(): channel-type abbreviation.  rhstype = ");
printtreenl (stderr, 4, rhstype);
fprintf (stderr, "gen1: tdespecification(): channel-type abbreviation.  nptr = ");
printtreenl (stderr, 4, nptr);
#endif
					loadmobile_real (nptr);
					storemobile (DValOf (tptr));
				}
			}
		}
		break;
	}
	return;
}
/*}}}*/
#endif
/*{{{  PRIVATE void tspecification (tptr)*/
/* Generate code for a specification (ie. initialise channel, Boolean, etc
*/
PRIVATE void tspecification (treenode * tptr)
{
	treenode *nptr = DNameOf (tptr);
	genlocn = LocnOf (tptr);

#if 0				/* removed 27/10/92 - bug TS/1870 now it is more selective */
	if (debugoutput && need_locate (TagOf (tptr)) && !separatelycompiled (nptr))
		coder_genlocate (tptr, TRUE);
#endif

	switch (TagOf (tptr)) {
		/*{{{  S_VALABBR S_ABBR S_VALRETYPE S_RETYPE */
	case S_VALABBR:
	case S_ABBR:
	case S_VALRETYPE:
	case S_RETYPE:
		{
			const abbrevmode_t am = be_abbrevmode (tptr);
			treenode *const rhs = DValOf (tptr);
			treenode *const lhstype = follow_user_type (NTypeOf (nptr));
			treenode *const rhstype = gettype (rhs);
			const BOOL retype = TagOf (tptr) == S_VALRETYPE || TagOf (tptr) == S_RETYPE;

			new_occam_line (tptr, TRUE, am != AM_CONST, TRUE);
			/* if (debugoutput && (am != AM_CONST))
			   coder_genlocate(tptr, TRUE); */

			tpreexp (rhs);
			if (retype) {
				/*{{{  look for open dimensions */
				/* bug TS/1505 28/11/91 */
				/* The stuff is all done by tree transformations in trans */
				/*}}} */
			} else {
				tcheckdimensions (lhstype, rhstype);
			}

			if (NVUseCountOf (nptr) != 0) {
				/*{{{  generate the abbreviation */
				const int alignment = retype ? align_requirement (lhstype, rhstype, rhs) : 0;

#if 0
fprintf (stderr, "tspecification: ABBR/RETYPE: alignment=%d, am=%d, nptr =", alignment, (int)am);
printtreenl (stderr, 4, nptr);
fprintf (stderr, "tspecification: ABBR/RETYPE: rhs =");
printtreenl (stderr, 4, rhs);
#endif
				switch (am) {
					/*{{{  default: (AM_CONST) */
				default:
					/*case AM_CONST: */
					break;
					/*}}} */
					/*{{{  AM_PTR, AM_ISRHS */
				case AM_PTR:
				case AM_ISRHS:	/* bug TS/1894 19/10/92 */
					if (am == AM_PTR) {
#ifdef MOBILES
						/* if this abbreviating a dynamic MOBILE array, copy the dimensions over too */
						if ((TagOf (lhstype) == S_MOBILE) && (TagOf (rhstype) == S_MOBILE) && isdynmobilearray (nptr)) {
							/* int i, ndim; */

							/* ndim = dynmobiledimensioncount (lhstype); */
#if 0
fprintf (stderr, "gen1: tspecification: MOBILE AM_PTR abbreviation.  ndim = %d, lhstype = ", ndim);
printtreenl (stderr, 4, lhstype);
#endif
							loadmobile_real (rhs);
							storemobile (nptr);
							genmobileunpack (nptr, FALSE, TRUE);
						} else
#endif
						{
							loadelementpointer (rhs, 0, MANY_REGS);
							storeinname (nptr, 0);
						}
					}
					/*{{{  check source is correctly aligned where necessary */
					if (retype && (alignment != 1)) {
						loadelementpointer (nptr, 0, MANY_REGS);
						genaligncheck (alignment);
					}
					/*}}} */
					/* we check the error flag here, so that we won't */
					/* duplicate any check which was done in genaligncheck */
					/* no problem about doing it after the store, cos */
					/* there couldn't have been anything useful there before */
					checkerror ();	/* added 22/6/90 by CO'N */
					break;
					/*}}} */
					/*{{{  AM_VAL */
				case AM_VAL:
					{
						if (retype && alignment != 1 && isaddressable (rhs)) {
							/*{{{  alignment check */
							/* Special case the alignment requirements */

							/* If we've got to here, the lhs is an INT or INT16 */
							/* Eg VAL INT x RETYPES [a FROM b FOR c] : */

							/* if rhs is an expression, it can't be misaligned */
							/* so if rhs is not addressable, it can't be misaligned! */

							/* Use the location itself to save the address in while */
							/* we check for alignment */
							loadelementpointer (rhs, 0, MANY_REGS);
							/* we DON'T check the error flag here, so that we won't */
							/* duplicate any check which was done in genaligncheck */
							/* no problem about doing it after the store, cos */
							/* there couldn't have been anything useful there before */
							storeinname (nptr, 0);
							/*loadelement(nptr, 0, MANY_REGS); */
							if (tx_global.hastserieserrors) {	/* bug 1360 - if H_series, catch trap later when copying into var */
								loadname (nptr, 0);	/* T9000 shorts 23/7/91 */
								genaligncheck (alignment);	/* this will check the error flag */
							}
							/*loadelement(nptr, 0, MANY_REGS); *//* pointer to source */
							loadname (nptr, 0);	/* T9000 shorts 23/7/91 */
							if (alignment == bytesperword) {
								genprimary (I_LDNL, 0);
								storeinname (nptr, 0);
							} else if (alignment == 2 && use_shortintops) {	/* T9000 shorts 23/7/91 */
								gensecondary (I_LS);	/* I_LSX is not necessary */
								storeinname (nptr, 0);
							} else {	/* it must be smaller than a word */
								loadelementpointer (nptr, 0, 2);	/* pointer to dest */
								loadconstant (alignment);	/* Size of data */
								gensecondary (I_MOVE);
							}
							/*}}} */
#if 0
						} else if ((TagOf (rhs) == S_RECORDITEM) && isdynmobilechantype (ASBaseOf (rhs))) {
							/* abbreviation of a dynamic MOBILE channel */
#if 0
fprintf (stderr, "tspecification: found dynamc mobile chan type on the RHS of an abbreviation.\n");
#endif
#endif
						} else {
							treenode *const orig_lhstype = NTypeOf (nptr);
							BOOL debug_marked = TRUE;

							if (TagOf (tptr) == S_VALRETYPE) {
								/* Initialise lhs if necessary */
								const int t = TagOf (lhstype);

								if (istargetbytesize (t) /*|| isshortint(t) */ ) {	/* bug 328 11/9/90 */
									zero_local_var (nptr);
								}
								/* Pretend it's an abbreviation while we initialise it */
								SetNType (nptr, rhstype);
							}
							tdecl_init_from_vsptr (nptr, &debug_marked);
							tsimpleassign (ntypeof (nptr), P_EXP, nptr, P_EXP, rhs, MANY_REGS);

							SetNType (nptr, orig_lhstype);	/* Give the name it's original type back */
						}
					}
					break;
					/*}}} */
				}
				/*}}} */
			}
		}
		break;
		/*}}} */
		/*{{{  S_DECL */
	case S_DECL:
		/* Declare each of the names in a declaration list */
		{
			BOOL debug_marked = FALSE;	/* bug TS/1870 27/10/92 */
			treenode *t = DNameOf (tptr);
			if (TagOf (t) == S_LIST) {
				for (; !EndOfList (t); t = NextItem (t)) {
					tdecl (ThisItem (t), &debug_marked);
				}
			} else {
				tdecl (t, &debug_marked);
			}
			tprocess (DValOf (tptr));	/* bug TS/1263 11/05/92 */
		}
		break;
		/*}}} */
		/*{{{  S_TPROTDEF S_SPROTDEF S_SFUNCDEF S_LFUNCDEF S_PROCDEF place  ignore */
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
	case S_PROCDEF:
	case S_VSPLACE:
	case S_WSPLACE:
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
	case S_MPROCDECL:
#endif
		break;
		/*}}} */
		/*{{{  S_PLACE might be special */
	case S_PLACE:
		{
			treenode *nptr = DNameOf (tptr);

			if (TagOf (NTypeOf (nptr)) == S_CHAN) {
				if (TypeAttrOf (NTypeOf (nptr)) & TypeAttr_placed) {
					const INT32 hashcode = typehash (nptr);

					texpopd_main (P_EXP, DValOf (tptr), MANY_REGS, FALSE);
					genprimary (I_LDC, (int)hashcode);
					gensecondary (I_EXTVRFY);
					gencomment0 ("verify external (PLACEd) channel");
					texpopd_main (P_EXP, DValOf (tptr), MANY_REGS, FALSE);
					loadconstant (1);
					gensecondary (I_OR);
					storeinname (nptr, 0);
					/* tsimpleassign (S_INT, P_EXP, nptr, P_EXP, DValOf (tptr), MANY_REGS); */
#if 0
fprintf (stderr, "tspecification: placed channel, hashcode is 0x%8.8x\n", (unsigned int)hashcode);
#endif
				}
			} else if ((nodetypeoftag (TagOf (NTypeOf (nptr))) == TYPENODE) && (TypeAttrOf (NTypeOf (nptr)) & TypeAttr_placed)) {
				/* simpler.. */
				texpopd_main (P_EXP, DValOf (tptr), MANY_REGS, FALSE);
				storeinname (nptr, 0);
				gencomment0 ("placed init");
			}
		}
		break;
		/*}}}  */
	default:
		badtag (genlocn, TagOf (tptr), "tspecification");
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *tspecs  (treenode *tptr)*/
PUBLIC treenode *tspecs (treenode * tptr)
{
	while (isspecification (tptr)) {
		tspecification (tptr);
		tptr = DBodyOf (tptr);
	}
	return tptr;
}

/*}}}*/
/*{{{  PUBLIC void tdespecs (treenode *tptr) */
/* this is used to de-scope MOBILES, hence #ifdef stuff */
PUBLIC void tdespecs (treenode *tptr)
{
#ifdef MOBILES
	while (isspecification (tptr)) {
		tdespecification (tptr);
		tptr = DBodyOf (tptr);
	}
#endif
	return;
}
/*}}}  */
/*{{{  PUBLIC void tpreexp(tptr)*/
/*****************************************************************************
 *
 *  tpreexp generates code which has to be pulled out in front of an
 *          expression.
 *          At the moment this is segment base and length checking.
 *          Also generates the left-hand sides of eval nodes.
 *
 *****************************************************************************/
PUBLIC void tpreexp (treenode * tptr)
{
	while (tptr != NULL) {
		switch (TagOf (tptr)) {
		default:
			badtag (genlocn, TagOf (tptr), "tpreexp");
			break;
			/*{{{  expression */
			/*{{{  monadics conversions   break */
		case S_NEG:
		case S_BITNOT:
		case S_UMINUS:
		case S_NOT:
		case S_SIZE:
		case S_EXACT:
		case S_TRUNC:
		case S_ROUND:
		case S_ADDRESSOF:
#ifdef MOBILES
		case S_ADDROF:
		case S_HWADDROF:
		case S_CLONE:
		case S_UNDEFINED:
		case S_TYPEHASHOF:
#endif
			tptr = OpOf (tptr);
			break;
			/*}}} */
			/*{{{  S_NEW_SRRAY */
#ifdef MOBILES
		case S_NEW_ARRAY:
			tptr = ARDimLengthOf (tptr);
			break;
		case S_DEFINED:
			tptr = OpOf (tptr);
			break;
		case S_ALLOC_PROC:
		case S_NEW_BARRIER:
			return;
#endif
			/*}}}  */
			/*{{{  dyadics                break */
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
		case S_CSUB0:
		case S_CCNT1:
			tpreexp (LeftOpOf (tptr));
			tptr = RightOpOf (tptr);
			break;
		case S_AND:
		case S_OR:
		case S_COLON2:
			/* Can't go into the right-hand side of AND or OR yet,
			   as it may not always be evaluated */
			/* Can't go into the right-hand side of counted array communication yet,
			   as it may require the left-hand
			   side to have been inputted first.  Right-hand side must be done
			   explicitly in tinputitem/toutputitem */
			tptr = LeftOpOf (tptr);
			break;
		case S_EVAL:
			if (TagOf (LeftOpOf (tptr)) != S_DUMMYEXP) {
				tpreexp (LeftOpOf (tptr));
				texp (LeftOpOf (tptr), MANY_REGS);
				throw_the_result_away ();
				checkerror ();
			}
			tptr = RightOpOf (tptr);
			break;
			/*}}} */
			/*{{{  specification ... valof      return */
		case S_ABBR:
		case S_VALABBR:
		case S_RETYPE:
		case S_VALRETYPE:
		case S_DECL:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_VALOF:
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
			return;
			/*}}} */
			/*{{{  constructor            break */
#if 0				/* no constructors are passed to the backend any more */
		case S_CONSTRUCTOR:
			tptr = LitExpOf (tptr);
			break;
#endif
			/*}}} */
			/*{{{  instance               break */
		case S_FINSTANCE:
			tptr = IParamListOf (tptr);
			break;
			/*}}} */
		case S_ELSIZE:
		case S_SEGSTART:
		case S_CONSTEXP:
		case S_DUMMYEXP:
		case S_STRING:
		case S_CONSTCONSTRUCTOR:
		case S_NULLARRAY:			/* added 13/02/2002 (frmb) */
			return;
			/*}}} */
			/*{{{  UDV constant expression    return*/
		case S_UDVCONSTEXP:
			if ((int)(LoValOf (tptr)) == -1) {
				tpreexp (CExpOf (tptr));
			}
			return;
			/*}}}*/
			/*{{{  element */
			/*{{{  ARRAYITEM              break */
		case S_ARRAYITEM:
		case S_ARRAYSUB:
		case S_RECORDSUB:
		case S_RECORDITEM:
			{
				treenode *exp = ASExpOf (tptr);
				tpreexp (ASBaseOf (tptr));
				tpreexp (exp);
				/*{{{  range check TIMER arrays */
				if (RANGECHECKING && (TagOf (tptr) == S_ARRAYITEM) &&
				    (basetype (gettype (tptr)) == S_TIMER) && (exp != NULL) && !isconst (exp)) {	/* Bug 288 22/5/90 */
					/* exp should always be TIMES index 0, so we can
					   avoid the multiply by: */
					if ((TagOf (exp) == S_TIMES) && isconst (RightOpOf (exp)))
						texp (LeftOpOf (exp), MANY_REGS);
					else
						texp (exp, MANY_REGS);
					throw_the_result_away ();
				}
				/*}}} */
			}
			return;
			/*}}} */
			/*{{{  SEGMENT SEGMENTITEM          return */
		case S_SEGMENT:
		case S_SEGMENTITEM:
			{
				treenode *startexp = SStartExpOf (tptr), *lengthexp = SLengthExpOf (tptr), *checkexp = SCheckExpOf (tptr);

				tpreexp (SNameOf (tptr));	/* Check nested segments first */
				tpreexp (startexp);
				tpreexp (lengthexp);

				simplify (P_EXP, startexp);
				simplify (P_EXP, lengthexp);

				if (checkexp != NULL) {
					texp (checkexp, MANY_REGS);
					throw_the_result_away ();
				}
			}
			return;
			/*}}} */
			/*{{{  TEMP                   break */
		case T_TEMP:
			tptr = NDeclOf (tptr);
			break;
			/*}}} */
			/*{{{  name */
		case N_ABBR:
		case N_VALABBR:
		case N_RETYPE:
		case N_VALRETYPE:
		case N_PARAM:
		case N_VALPARAM:
		case N_RESULTPARAM:
		case N_DECL:
		case N_REPL:
		case T_PREEVALTEMP:
		case T_REGTEMP:
		case N_LABELDEF:

			/* added 18/08/92 for TS/1797 */
		case N_PROCDEF:
#ifdef MOBILES
		case N_MPROCDECL:
		case N_LIBMPROCDECL:
#endif
		case N_SCPROCDEF:
		case N_LIBPROCDEF:
		case N_STDLIBPROCDEF:
		case N_INLINEPROCDEF:
		case N_LFUNCDEF:
		case N_SFUNCDEF:
		case N_SCFUNCDEF:
		case N_LIBFUNCDEF:
		case N_STDLIBFUNCDEF:
		case N_INLINEFUNCDEF:
			return;
			/*}}} */
			/*{{{  backend names */
		case S_PARAM_STATICLINK:
		case S_PARAM_VSP:
		case S_PARAM_FB:
		case S_PARAM_WS:
#ifdef MOBILES
		case S_PARAM_MSP:
		case S_PARAM_MPP:
		case S_NTH_DIMENSION:
		case S_HIDDEN_TYPE:
#endif
		case S_FNFORMALRESULT:
		case S_FNACTUALRESULT:
		case S_HIDDEN_PARAM:
			return;
			/*}}} */
			/*}}} */
			/*{{{  list                   break */
		case S_LIST:
			tpreexp (ThisItem (tptr));
			tptr = NextItem (tptr);
			break;
			/*}}} */
		}
	}
	return;
}

/*}}}*/
/*{{{  PUBLIC void tstop (address)*/
PUBLIC void tstop (treenode * const address)
{
	new_occam_line (address, TRUE, TRUE, TRUE);
	/* if (debugoutput)
	   coder_genlocate(address, TRUE); */
	if (errormode & ERRORMODE_STOP_IS_SETERR) {
		if (!tx_global.hastserieserrors &&	/* bug TS/1547 07/04/92 */
		    !T9000_alpha_nocauseerror (&tx_global)) {	/* Section 5.8 of SW-0341-2. */
			genprimary (I_LDC, ERR_INTERR);
			gensecondary (I_CAUSEERROR);
		} else {
			gensecondary (I_SETERR);
		}
	} else if (errormode & ERRORMODE_STOP_IS_STOPP) {
		gensecondary (I_STOPP);
	} else if (errormode & ERRORMODE_STOP) {
		gensecondary (I_STOPERR);
	}
	markdeadcode ();
}

/*}}}*/
/*{{{  PUBLIC void tsync (treenode *bar)*/
/*
 *	generates code to synchronise on a barrier
 */
PUBLIC void tsync (treenode *bar)
{
	loadmobile (bar);
	if (isdynmobilebarrier (bar)) {
		genchecknotnull ();
	}
	gensecondary (I_MT_SYNC);
}
/*}}}*/
/*{{{  PRIVATE void tresign_inner (treenode *bar, const int n)*/
PRIVATE void tresign_inner (treenode *bar, const int n)
{
	loadmobile (bar);
	if (isdynmobilebarrier (bar)) {
		genchecknotnull ();
	}
	loadconstant (n);
	gensecondary (I_MT_RESIGN);
}
/*}}}*/
/*{{{  PUBLIC void tresign (treenode *bar, const int n)*/
/*
 *	generates code to resign a constant number of processes from a barrier
 */
PUBLIC void tresign (treenode *bar, const int n)
{
	if (!bar) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tresign: NULL barrier");
	}
	if (!n) {
		return;
	}

	if (TagOf (bar) == S_LIST) {
		treenode *walk;

		for (walk = bar; !EndOfList (walk); walk = NextItem (walk)) {
			tresign_inner (ThisItem (walk), n);
		}
	} else {
		tresign_inner (bar, n);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void tresign_exp_inner (treenode *bar, treenode *n, int adjust)*/
PRIVATE void tresign_exp_inner (treenode *bar, treenode *n, int adjust)
{
	loadmobile (bar);
	if (isdynmobilebarrier (bar)) {
		genchecknotnull ();
	}
	texp (n, MAXREGS - 1);
	if (adjust) {
		genprimary (I_ADC, adjust);
	}
	gensecondary (I_MT_RESIGN);
}
/*}}}*/
/*{{{  PUBLIC void tresign_exp (treenode *bar, treenode *n, int adjust)*/
/*
 *	generates code to resign a number of processes from a barrier
 */
PUBLIC void tresign_exp (treenode *bar, treenode *n, int adjust)
{
	if (!bar) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tresign_exp: NULL barrier");
	}

	if (TagOf (bar) == S_LIST) {
		treenode *walk;

		for (walk = bar; !EndOfList (walk); walk = NextItem (walk)) {
			tresign_exp_inner (ThisItem (walk), n, adjust);
		}
	} else {
		tresign_exp_inner (bar, n, adjust);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void tenroll_inner (treenode *bar, const int n)*/
PRIVATE void tenroll_inner (treenode *bar, const int n)
{
	loadmobile (bar);
	if (isdynmobilebarrier (bar)) {
		genchecknotnull ();
	}
	loadconstant (n);
	gensecondary (I_MT_ENROLL);
}
/*}}}*/
/*{{{  PUBLIC void tenroll (treenode *bar, const int n)*/
/*
 *	generates code to enroll a constant number of process on a barrier
 */
PUBLIC void tenroll (treenode *bar, const int n)
{
	if (!bar) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tenroll: NULL barrier");
	}
	if (!n) {
		return;
	}
	if (TagOf (bar) == S_LIST) {
		treenode *walk;

		for (walk = bar; !EndOfList (walk); walk = NextItem (walk)) {
			if ((TagOf (ThisItem (walk)) != N_DECL) || (NVUseCountOf (ThisItem (walk)) > 0)) {
				tenroll_inner (ThisItem (walk), n);
			}
		}
	} else if ((TagOf (bar) != N_DECL) || (NVUseCountOf (bar) > 0)) {
		tenroll_inner (bar, n);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void tenroll_exp_inner (treenode *bar, treenode *n, const int adjust)*/
PRIVATE void tenroll_exp_inner (treenode *bar, treenode *n, const int adjust)
{
	gencomment0 ("tenroll_exp_inner():");
	loadmobile (bar);
	if (isdynmobilebarrier (bar)) {
		genchecknotnull ();
	}
	texp (n, MAXREGS - 1);
	if (adjust) {
		genprimary (I_ADC, adjust);
	}
	gensecondary (I_MT_ENROLL);
}
/*}}}*/
/*{{{  PUBLIC void tenroll_exp (treenode *bar, treenode *n, const int adjust)*/
/*
 *	generates code to enroll a number of processes on a barrier
 */
PUBLIC void tenroll_exp (treenode *bar, treenode *n, const int adjust)
{
	if (!bar) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tenroll_exp: NULL barrier");
	}
	if (TagOf (bar) == S_LIST) {
		treenode *walk;

		for (walk = bar; !EndOfList (walk); walk = NextItem (walk)) {
			tenroll_exp_inner (ThisItem (walk), n, adjust);
		}
	} else {
		tenroll_exp_inner (bar, n, adjust);
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void tinitbarrier (treenode *bar)*/
/*
 *	initialises barrier(s)
 */
PUBLIC void tinitbarrier (treenode *bar)
{
	if (!bar) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tinitbarrier: NULL barrier");
	}
	if (disable_dynmem) {
		generr (GEN_NO_DYNMEM);
	}
	if (TagOf (bar) == S_LIST) {
		treenode *walk;

		for (walk = bar; !EndOfList (walk); walk = NextItem (walk)) {
			treenode *thisbar = ThisItem (walk);

			if (NVUseCountOf (thisbar) > 0) {
				if (isdynmobilebarrier (ThisItem (walk))) {
					loadmobile (ThisItem (walk));
					genchecknotnull ();
				#if 0
				} else {
					loadconstant (0);
					loadconstant (MT_MAKE_BARRIER (MT_BARRIER_FULL));
					gensecondary (I_MT_ALLOC);
					storemobile (ThisItem (walk));
				#endif
				}
			}
		}
	} else if (NVUseCountOf (bar) > 0) {
		if (isdynmobilebarrier (bar)) {
			loadmobile (bar);
			genchecknotnull ();
		#if 0
		} else {
			loadconstant (0);
			loadconstant (MT_MAKE_BARRIER (MT_BARRIER_FULL));
			gensecondary (I_MT_ALLOC);
			storemobile (bar);
		#endif
		}
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void tfreebarrier (treenode *bar)*/
/*
 *	release barrier(s)
 */
PUBLIC void tfreebarrier (treenode *bar)
{
	if (!bar) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tfreebarrier: NULL barrier");
	}
	if (TagOf (bar) == S_LIST) {
		treenode *walk;

		for (walk = bar; !EndOfList (walk); walk = NextItem (walk)) {
			treenode *thisbar = ThisItem (walk);

			if (NVUseCountOf (thisbar) > 0) {
				if (!isdynmobilebarrier (ThisItem (walk))) {
					loadmobile (ThisItem (walk));
					gensecondary (I_MT_RELEASE);
				}
			}
		}
	} else if (NVUseCountOf (bar) > 0) {
		if (!isdynmobilebarrier (bar)) {
			loadmobile (bar);
			gensecondary (I_MT_RELEASE);
		}
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void trepl (tptr, tbody, p1, p2)*/
/*****************************************************************************
 *
 *  trepl generates code for the replicator node tptr:
 *        the function param tbody is called to generate code for the
 *        body of the replicator.
 *
 *****************************************************************************/
/* p1 and p2 are Parameters to the call of tbody */
PUBLIC void trepl (treenode * const tptr, void (*tbody) (treenode *, void *, INT32), void *const p1, const INT32 p2, const BOOL mark_endrepl)
{
	const int looplab = newlab ();
	const int endlab = newlab ();
	treenode *const start = ReplCStartExpOf (tptr);
	treenode *const length = ReplCLengthExpOf (tptr);
	treenode *const step = ReplCStepExpOf (tptr);
	treenode *const replname = ReplCNameOf (tptr);
	const BOOL constlength = isconstexpnd (length);
	BOOL conststep;
	const int level = NLexLevelOf (replname);
	const INT32 wsposn = NVOffsetOf (replname) + nameoffsetof (level);
	int stepmode = 1;	/* -1 = step backwards, 0 = use ws-var, 1 = step forwards */

	gencomment0 ("trepl: start.");
#if 0
fprintf (stderr, "gen1: trepl: start = ");
printtreenl (stderr, 4, start);
fprintf (stderr, "gen1: trepl: length = ");
printtreenl (stderr, 4, length);
fprintf (stderr, "gen1: trepl: step = ");
if (step) { printtreenl (stderr, 4, step); } else { fprintf (stderr, "(null)\n"); }
#endif
	/*{{{  return if zero replicator */
	if (constlength && (LoValOf (length) == 0)) {
		return;
	}
	if (step) {
		conststep = isconstexpnd (step);
	} else {
		/* only using 2 words of workspace and single step */
		conststep = TRUE;
	}
	/*}}} */
	/*{{{  generate the code */
	/*{{{  generate loop head (start, count, replicator checks) */
	genloopheadtop (wsposn);
	if (!NVNameUsedOf (replname)) {
		/* skip base initialisation if name not used..! */
		gencomment0 ("removed loop-base initialisation");
	} else {
		texp (start, MANY_REGS);	/*          start                */
		storeinname (replname, REPL_BASE);	/*          stl   i              */
	}
	texp (length, MANY_REGS);	/*          count                */
	storeinname (replname, REPL_COUNT);	/*          stl   i + 1          */
	if (step && !NVNameUsedOf (replname)) {
		/* skip step initialisation too if name not used */
		gencomment0 ("removed loop-step initialisation");
	} else if (step) {
		if (conststep && (LoValOf (step) == -1)) {
			stepmode = -1;
		} else if (conststep && (LoValOf (step) == 1)) {
			stepmode = 1;
		} else {
			texp (step, MANY_REGS);
			storeinname (replname, REPL_STEP);
			stepmode = 0;
		}
	}
	/*{{{  check for replicator overflow */
	/* bug TS/1975 26/11/92.
	 * Must check for possible overflow of replicator variable.
	 * Do this by calculating value of last occurrence,
	 * which is (start + (len - 1)).
	 * We make use of the fact that we now know that len > 0, so len - 1 can't
	 * overflow. Also, if start <= 1, then the whole thing can't overflow.
	 * Similarly if len == 1, then no overflow.
	 * CON - 26/11/92.
	 */
	if ((errormode & ERRORMODE_REPLCHECK) != 0) {
		BOOL checked = FALSE;
		const BOOL conststart = isconstexpnd (start);

		if (conststart && constlength) {
			/* skip - check in frontend */
		} else if (conststart) {	/* we know that length is variable, > 0 */
			/* can never overflow if start <= 1 */
			if ((INT32) (LoValOf (start)) > 1) {
				loadname (replname, REPL_COUNT);	/*        count                */
				genprimary (I_ADC, LoValOf (start) - 1);	/* adc  (start - 1)     */
				checked = TRUE;
			}
		} else if (constlength) {	/* start is variable, length is constant */
			if (LoValOf (length) > 1) {
				loadname (replname, REPL_BASE);	/*         start                */
				genprimary (I_ADC, LoValOf (length) - 1);	/* adc  (count - 1)     */
				checked = TRUE;
			}
		} else {	/* start is variable, length is variable */
			loadname (replname, REPL_COUNT);	/*          count                */
			genprimary (I_ADC, -1);	/*          adc -1               */
			loadname (replname, REPL_BASE);	/*          start                */
			gensecondary (I_ADD);	/*          add                  */
			checked = TRUE;
		}
		if (checked) {
			gencomment0 ("check for replicator overflow");
			throw_the_result_away ();
			checkerror ();
		}
	}
	/*}}} */
	/*{{{  check the replicator is valid */
	if (!constlength) {
		BOOL check_needed = (((errormode & ERRORMODE_REPLCHECK) != 0) && (TagOf (length) != S_SIZE));	/* bug TS/1869 15/09/92 */

		loadname (replname, REPL_COUNT);	/*          ldl   i + 1          */
		(void) genreplicatorcheck (check_needed, looplab, endlab);
	}
	/*}}} */
	genloopheadbottom ();
	/*}}} */

	setlab (looplab);	/* looplab:                      */
	genstartblock ();
	(*tbody) (ReplCBodyOf (tptr), p1, p2);	/*          body                 */
	new_occam_line (replname, FALSE, mark_endrepl, TRUE);
#if 0
fprintf (stderr, "gen1: trepl: replname (NVUseCount=%d, NVNameUsed=%d) =", (int)(NVUseCountOf (replname)), (int)(NVNameUsedOf (replname)));
printtreenl (stderr, 4, replname);
#endif
	if (!NVNameUsedOf (replname)) {
		/* output special comment: allows translator to optimise loopend generation -- step is also irrelvant for these! */
		gencomment0 (".MAGIC UNUSED LOOPVAR");
		genloopend (wsposn, endlab, looplab);
	} else {
		/* if (mark_endrepl && debugoutput)
		   coder_genlocate(replname, FALSE); *//* bug TS/1434 10/03/92 */
		if (step) {
			switch (stepmode) {
			case -1:
				/* decrement 1 each time round */
				genloopendb (wsposn, endlab, looplab);
				break;
			case 0:
				/* step is in workspace */
				genloopend3 (wsposn, endlab, looplab);
				break;
			case 1:
				/* increment 1 each time round */
				genloopend (wsposn, endlab, looplab);
				break;
			}
		} else {
			genloopend (wsposn, endlab, looplab);	/* MDP - move work to coder */
		}
	}
	/*}}} */
}

/*}}}*/

/*{{{  PRIVATE void tsetpar (INT32 parcount, int joinlab, treenode *pnode)*/
/*****************************************************************************
 *
 *  tsetpar generates the start up code for a PAR which saves the PAR count
 *          in workspace slot 1, and the join address in workspace slot 0.
 *          frmb: if using process priority, this is saved in workspace slot 2.
 *
 *****************************************************************************/

/* Number of parallel processes */
/* Join address */
PRIVATE void tsetpar (INT32 parcount, int joinlab, treenode *pnode)
{
	int l = newlab ();

#if 0
fprintf (stderr, "gen1: tsetpar: parcount = %d\n", (int)parcount);
#endif
	#ifdef PROCESS_PRIORITY
	gensecondary (I_GETPAS);
	genprimary (I_STL, 2);
	gencomment0 ("INT priority affinity state");
	#endif
	genprimary (I_LDC, parcount);
	genprimary (I_STL, 1);
	gencomment0 ("INT parcount");
	genloadlabptr (joinlab, l, "PTR joinlab");
	genprimary (I_STL, 0);
	gencomment0 ("PTR joinlab");

	/* if we have an MPP, enroll on the barrier (by parcount-1) */
	if (inside_suspends) {
		loadmpp ();
		genprimary (I_LDNL, MPP_BARRIER);
		genprimary (I_LDC, parcount - 1);
		gensecondary (I_MT_ENROLL);
		#if 0
		loadmpp ();
		genprimary (I_LDNL, MPP_BECNT);
		genprimary (I_ADC, parcount - 1);
		loadmpp ();
		genprimary (I_STNL, MPP_BECNT);
		loadmpp ();
		genprimary (I_LDNL, MPP_BCNT);
		genprimary (I_ADC, parcount - 1);
		loadmpp ();
		genprimary (I_STNL, MPP_BCNT);
		gencomment0 ("MPP enroll");
		#endif
	}

	/* if this PAR declares a barrier, initialise and enroll n processes.
	 * if it EXTENDS a barrier, enroll n-1 processes
	 */
	{
		treenode *bar;
		treenode *declbars, *extbars;
		BOOL is_repl;

		if (((TagOf (pnode) == S_PAR) || (TagOf (pnode) == S_PRIPAR)) && CTempOf (pnode)) {
			bar = CTempOf (pnode);
			is_repl = FALSE;
		} else if (((TagOf (pnode) == S_REPLPAR) || (TagOf (pnode) == S_PRIREPLPAR)) && ReplCTempOf (pnode)) {
			bar = ReplCTempOf (pnode);
			is_repl = TRUE;
		} else {
			bar = NULL;
			is_repl = FALSE;
		}

		if (bar && (TagOf (bar) == S_BAREXTEND)) {
			declbars = LeftOpOf (bar);
			extbars = RightOpOf (bar);
		} else if (bar && (TagOf (bar) == S_EXTENDS)) {
			declbars = NULL;
			extbars = bar;
		} else if (bar) {
			declbars = bar;
			extbars = NULL;
		} else {
			declbars = extbars = NULL;
		}

		if (extbars && (TagOf (extbars) == S_EXTENDS)) {
			/* enroll n-2/n-1 processes */
			tenroll (OpOf (extbars), is_repl ? (parcount - 2) : (parcount - 1));
		} else if (extbars) {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tsetpar: bad PAR EXTENDS");
		}

		if (declbars) {
			/* initialise and enroll n-1/n processes */
			int enroll_count = is_repl ? (parcount - 1) : parcount;
			tinitbarrier (declbars);
			tenroll (declbars, enroll_count - 1);
		}
	}

	return;
}
/*}}}*/
/*{{{  PRIVATE void tsetpar_exp (treenode *parcount, int joinlab, int skiplab, treenode *pnode)*/
PRIVATE void tsetpar_exp (treenode *parcount, int joinlab, int skiplab, treenode *pnode)
{
	int l = newlab ();

	#ifdef PROCESS_PRIORITY
	gensecondary (I_GETPRI);
	genprimary (I_STL, 2);
	gencomment0 ("INT priority");
	#endif
	tpreexp (parcount);
	texp (parcount, MANY_REGS);
	genbranch (I_CJ, skiplab);
	texp (parcount, MANY_REGS);
	genprimary (I_LDC, 1);
	gensecondary (I_SUM);
	genprimary (I_STL, 1);
	gencomment0 ("INT (parcount + 1)   -- process doing n-replicated PAR");
	genloadlabptr (joinlab, l, "PTR joinlab");
	genprimary (I_STL, 0);
	gencomment0 ("PTR joinlab");

	/* if we have an MPP, enroll on the barrier (by parcount-1) */
	if (inside_suspends) {
		loadmpp ();
		genprimary (I_LDNL, MPP_BARRIER);
		texp (parcount, MAXREGS - 1);
		genprimary (I_ADC, -1);
		gensecondary (I_MT_ENROLL);

		#if 0
		texp (parcount, MANY_REGS);
		genprimary (I_ADC, -1);
		loadmpp ();
		genprimary (I_LDNL, MPP_BECNT);
		gensecondary (I_SUM);
		loadmpp ();
		genprimary (I_STNL, MPP_BECNT);

		texp (parcount, MANY_REGS);
		genprimary (I_ADC, -1);
		loadmpp ();
		genprimary (I_LDNL, MPP_BCNT);
		gensecondary (I_SUM);
		loadmpp ();
		genprimary (I_STNL, MPP_BCNT);
		gencomment0 ("MPP enroll");
		#endif
	}

	/* if this PAR declares a barrier, initialise and enroll n processes.
	 * if it EXTENDS a barrier, enroll n-1 processes
	 */
	{
		treenode *bar;
		treenode *declbars, *extbars;

		if (((TagOf (pnode) == S_PAR) || (TagOf (pnode) == S_PRIPAR)) && CTempOf (pnode)) {
			bar = CTempOf (pnode);
		} else if (((TagOf (pnode) == S_REPLPAR) || (TagOf (pnode) == S_PRIREPLPAR)) && ReplCTempOf (pnode)) {
			bar = ReplCTempOf (pnode);
		} else {
			bar = NULL;
		}

		if (bar && (TagOf (bar) == S_BAREXTEND)) {
			declbars = LeftOpOf (bar);
			extbars = RightOpOf (bar);
		} else if (bar && (TagOf (bar) == S_EXTENDS)) {
			declbars = NULL;
			extbars = bar;
		} else if (bar) {
			declbars = bar;
			extbars = NULL;
		} else {
			declbars = extbars = NULL;
		}

		if (extbars && (TagOf (extbars) == S_EXTENDS)) {
			/* enroll n-1 processes */
			tenroll_exp (OpOf (extbars), parcount, -1);
		} else if (extbars) {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tsetpar_exp: bad PAR EXTENDS");
		}

		if (declbars) {
			/* initialise and enroll n processes */
			tinitbarrier (declbars);
			tenroll_exp (declbars, parcount, -1);
		}
	}

	return;
}
/*}}}*/
/*{{{  PRIVATE void tendpar (const int parcount, treenode *tptr)*/
/*
 *	called to generate code for ends-of-PAR, e.g. barrier resign
 *	this code only interesting for when barrier_rbpe (resign before PAR end) *isn't* set
 */
PRIVATE void tendpar (const int parcount, treenode *tptr)
{
	treenode *bar;
	treenode *declbars, *extbars;
	BOOL is_repl;

	if (((TagOf (tptr) == S_PAR) || (TagOf (tptr) == S_PRIPAR)) && CTempOf (tptr)) {
		bar = CTempOf (tptr);
		is_repl = FALSE;
	} else if (((TagOf (tptr) == S_REPLPAR) || (TagOf (tptr) == S_PRIREPLPAR)) && ReplCTempOf (tptr)) {
		bar = ReplCTempOf (tptr);
		is_repl = TRUE;
	} else {
		bar = NULL;
		is_repl = FALSE;
	}

	if (bar && (TagOf (bar) == S_BAREXTEND)) {
		declbars = LeftOpOf (bar);
		extbars = RightOpOf (bar);
	} else if (bar && (TagOf (bar) == S_EXTENDS)) {
		declbars = NULL;
		extbars = bar;
	} else if (bar) {
		declbars = bar;
		extbars = NULL;
	} else {
		declbars = extbars = NULL;
	}

	if (!barrier_rbpe) {
		/* resign barriers */
		if (extbars && (TagOf (extbars) == S_EXTENDS)) {
			gencomment0 (".MAGIC EXTENDS RESIGN");
			tresign (OpOf (extbars), is_repl ? (parcount - 2) : (parcount - 1));
		} else if (extbars) {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tendpar: bad PAR EXTENDS");
		}
	}

	return;
}
/*}}}*/
/*{{{  PRIVATE void tendpar_exp (treenode *parcount, treenode *tptr)*/
/*
 *	called to generate code for ends-of-PAR, e.g. barrier resign
 *	this code only interesting for when barrier_rbpe (resign before PAR end) *isn't* set
 */
PRIVATE void tendpar_exp (treenode *parcount, treenode *tptr)
{
	treenode *bar;
	treenode *declbars, *extbars;
	BOOL is_repl;

	if (((TagOf (tptr) == S_PAR) || (TagOf (tptr) == S_PRIPAR)) && CTempOf (tptr)) {
		bar = CTempOf (tptr);
		is_repl = FALSE;
	} else if (((TagOf (tptr) == S_REPLPAR) || (TagOf (tptr) == S_PRIREPLPAR)) && ReplCTempOf (tptr)) {
		bar = ReplCTempOf (tptr);
		is_repl = TRUE;
	} else {
		bar = NULL;
		is_repl = FALSE;
	}

	if (bar && (TagOf (bar) == S_BAREXTEND)) {
		declbars = LeftOpOf (bar);
		extbars = RightOpOf (bar);
	} else if (bar && (TagOf (bar) == S_EXTENDS)) {
		declbars = NULL;
		extbars = bar;
	} else if (bar) {
		declbars = bar;
		extbars = NULL;
	} else {
		declbars = extbars = NULL;
	}

	if (!barrier_rbpe) {
		/* resign barriers */
		if (extbars && (TagOf (extbars) == S_EXTENDS)) {
			gencomment0 (".MAGIC EXTENDS RESIGN");
			tresign_exp (OpOf (extbars), parcount, is_repl ? -2 : -1);
		} else if (extbars) {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tendpar_exp: bad PAR EXTENDS");
		}
	}

	return;
}
/*}}}*/
/*{{{  PRIVATE void tsubpar (treenode *proclist, int start, treenode *parnode)*/
PRIVATE void tsubpar (treenode *proclist, int start, treenode *parnode)
{
	/* walk the process list for processes 2, 3, ... */
	/*int branch = 1; *//* used by source_output code */
	BIT32 parsize = SpDatasizeOf (ThisItem (proclist));
	treenode *bar;
	treenode *declbars, *extbars;

	/*{{{  find barrier decls + extensions*/
	if (((TagOf (parnode) == S_PAR) || (TagOf (parnode) == S_PRIPAR)) && CTempOf (parnode)) {
		bar = CTempOf (parnode);
	} else if (((TagOf (parnode) == S_REPLPAR) || (TagOf (parnode) == S_PRIREPLPAR)) && ReplCTempOf (parnode)) {
		bar = ReplCTempOf (parnode);
	} else {
		bar = NULL;
	}

	/* separate off any barrier */
	if (bar && (TagOf (bar) == S_BAREXTEND)) {
		declbars = LeftOpOf (bar);
		extbars = RightOpOf (bar);
	} else if (bar && (TagOf (bar) == S_EXTENDS)) {
		declbars = NULL;
		extbars = bar;
	} else if (bar) {
		declbars = bar;
		extbars = NULL;
	} else {
		declbars = extbars = NULL;
	}
	/*}}}*/

	/* first process is done separately */
	proclist = NextItem (proclist);
	while (!EndOfList (proclist)) {
		treenode *pprocess = ThisItem (proclist);
		BIT32 thisbranchwsp = SpMaxwspOf (pprocess);
		BIT32 thisbranchsize = SpDatasizeOf (pprocess);
		BIT32 thisbranchadjust = parsize + thisbranchwsp;

		if (start) {
			/*{{{  start a process */
			int l = newlab (), pstartlab = newlab ();
			genlabeldiff (I_LDC, pstartlab, l);
			genprimary (I_LDLP, -thisbranchadjust);
			gensecondary (I_STARTP);
			setlab (l);
#if 0
			/*{{{  T9000_alpha_badrunp */
			if (T9000_alpha_badrunp (&tx_global)) {
				gensecondary (I_NOP);
				gencomment0 ("Fix for scheduler problem");
				gensecondary (I_NOP);
				gencomment0 ("Fix for scheduler problem");
			}
			/*}}} */
#endif
			SetSpLabel (pprocess, pstartlab);
			/*}}} */
		} else {
			treenode *bodyp = SpBodyOf (pprocess);
			treenode *ires = NULL;

			adjustworkspace (thisbranchadjust);
			/*{{{  generate body */
			setlab (SpLabelOf (pprocess));
			if (inside_suspends) {
				/* the body is a SEQ with the original and instances.  Separate off the MPBARRESIGN() call */
				ires = ThisItem (NextItem (CBodyOf (bodyp)));
				bodyp = ThisItem (CBodyOf (bodyp));
			}
			if (SpWSMapOf (pprocess)) {
				genloadwsmap (mppoffset + thisbranchadjust, SpMapLabelOf (pprocess));
			}
			tprocess (bodyp);
			throw_the_result_away ();	/* in case there is dross around */
			if (inside_suspends) {
				/*{{{  resign from mobile-process barrier*/
				int sl = newlab ();

#if 0
fprintf (stderr, "gen1: tsubpar: resign from barrier.  ires = ");
printtreenl (stderr, 4, ires);
#endif
				/* need to resign from the MPP barrier -- first check PAR count > 1 */
				genprimary (I_LDLP, thisbranchadjust);
				genprimary (I_LDNL, 1);
				gencomment0 ("INT parcount");
				genprimary (I_ADC, -1);
				genbranch (I_CJ, sl);
				throw_the_result_away ();

				tinstance (ires);
				throw_the_result_away ();

				setlab (sl);
				/*}}}*/
			}
			if (barrier_rbpe && (declbars || extbars)) {
				/*{{{  resign from BARRIERs*/
				if (declbars) {
					/* resign from these absolutely */
					tresign (declbars, 1);
				}
				if (extbars) {
					int sl = newlab ();

					/* only resign if not the last process */
					genprimary (I_LDLP, thisbranchadjust);
					genprimary (I_LDNL, 1);
					gencomment0 ("INT parcount");
					genprimary (I_ADC, -1);
					genbranch (I_CJ, sl);
					throw_the_result_away ();

					tresign (OpOf (extbars), 1);
					throw_the_result_away ();

					setlab (sl);
				}
				/*}}}*/
			}
			if (SpWSMapOf (pprocess)) {
				genunloadwsmap (mppoffset + thisbranchadjust, SpMapLabelOf (pprocess));
			}
			genprimary (I_LDLP, thisbranchadjust);
			gencomment0 ("joinlab");
			gensecondary (I_ENDP);
			/*}}} */
			adjustworkspace (-thisbranchadjust);
		}

		parsize += thisbranchsize;
		proclist = NextItem (proclist);
	}
}

/*}}}*/
/*{{{  PRIVATE void tpar (treenode *tptr, int paritems, treenode *parnode)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tpar generates the code for a PAR process, tptr.
 *       'paritems' contains the number of processes to be run in parallel.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void tpar (treenode *tptr, int paritems, treenode *parnode)
{
	if (paritems > 1) {
		tsubpar (tptr, TRUE, parnode);	/* startoff each process */
	}
	/*{{{  generate process 1 */
	{
		treenode *firstitem = ThisItem (tptr);
		BIT32 thisbranchwsp = SpMaxwspOf (firstitem);
		treenode *bodyp = SpBodyOf (firstitem);
		treenode *ires = NULL;
		treenode *bar;
		treenode *declbars, *extbars;

		/*{{{  find barrier decls + extensions*/
		if (((TagOf (parnode) == S_PAR) || (TagOf (parnode) == S_PRIPAR)) && CTempOf (parnode)) {
			bar = CTempOf (parnode);
		} else if (((TagOf (parnode) == S_REPLPAR) || (TagOf (parnode) == S_PRIREPLPAR)) && ReplCTempOf (parnode)) {
			bar = ReplCTempOf (parnode);
		} else {
			bar = NULL;
		}

		/* separate off any barrier */
		if (bar && (TagOf (bar) == S_BAREXTEND)) {
			declbars = LeftOpOf (bar);
			extbars = RightOpOf (bar);
		} else if (bar && (TagOf (bar) == S_EXTENDS)) {
			declbars = NULL;
			extbars = bar;
		} else if (bar) {
			declbars = bar;
			extbars = NULL;
		} else {
			declbars = extbars = NULL;
		}
		/*}}}*/

		genprimary (I_AJW, -thisbranchwsp);
		adjustworkspace (thisbranchwsp);
		if (inside_suspends) {
			/* the body is a SEQ with the original and instances.  Separate off the MPBARRESIGN() call */
			ires = ThisItem (NextItem (CBodyOf (bodyp)));
			bodyp = ThisItem (CBodyOf (bodyp));
		}

		/* if there is a workspace-map, load it */
		if (SpWSMapOf (firstitem)) {
			genloadwsmap (mppoffset + thisbranchwsp, SpMapLabelOf (firstitem));
		}

		tprocess (bodyp);
		throw_the_result_away ();	/* in case there is dross around */
		if (inside_suspends) {
			/*{{{  resign from mobile-process barrier if not the last*/
			int sl = newlab ();

			/* resign from the MPP barrier -- first check PAR count > 1 */
			genprimary (I_LDLP, thisbranchwsp);
			genprimary (I_LDNL, 1);
			gencomment0 ("INT parcount");
			genprimary (I_ADC, -1);
			genbranch (I_CJ, sl);
			throw_the_result_away ();

			tinstance (ires);
			throw_the_result_away ();

			setlab (sl);
			/*}}}*/
		}
		if (barrier_rbpe && (declbars || extbars)) {
			/*{{{  resign from BARRIERs*/
			if (declbars) {
				/* resign from these absolutely */
				tresign (declbars, 1);
			}
			if (extbars) {
				int sl = newlab ();

				/* only resign if not the last process */
				genprimary (I_LDLP, thisbranchwsp);
				genprimary (I_LDNL, 1);
				gencomment0 ("INT parcount");
				genprimary (I_ADC, -1);
				genbranch (I_CJ, sl);
				throw_the_result_away ();

				tresign (OpOf (extbars), 1);
				throw_the_result_away ();

				setlab (sl);
			}
			/*}}}*/
		}

		/* if there is a workspace-map, load it */
		if (SpWSMapOf (firstitem)) {
			genunloadwsmap (mppoffset + thisbranchwsp, SpMapLabelOf (firstitem));
		}
		genprimary (I_LDLP, thisbranchwsp);
		gencomment0 ("joinlab");
		gensecondary (I_ENDP);
		adjustworkspace (-thisbranchwsp);
	}
	/*}}} */
	if (paritems > 1) {
		tsubpar (tptr, FALSE, parnode);	/* generate their bodies */
	}
}

/*}}}*/
/*{{{  PRIVATE void tnormalpar (treenode *const tptr)*/
PRIVATE void tnormalpar (treenode *const tptr)
{
	treenode *const t = CBodyOf (tptr);
	const int parcount = listitems (t);

	if (parcount != 0) {
		const int joinlab = newlab ();

		tsetpar (parcount, joinlab, tptr);
		tpar (t, parcount, tptr);	/* Generate code for each of the processes */
		gencomment0 (".MAGIC JOINLAB");
		setlab (joinlab);
		new_occam_line (t, FALSE, TRUE, TRUE);
		tendpar (parcount, tptr);
		/* if (debugoutput)
			coder_genlocate(t, FALSE); *//* bug TS/1434 10/03/92 */
	}
	/* else (parcount == 0) */
}

/*}}}*/
#if 0
/*{{{  PRIVATE void tpripar (treenode *const tptr)*/
PRIVATE void tpripar (treenode *const tptr)
{
	treenode *const t = CBodyOf (tptr);

	if (code_style_flags & CODE_STYLE_ALT_PRI_PAR) {		/* New way to do PRI PAR using an ALT */
		/*{{{  PRI PAR using an ALT */
		/* Note that this is NECESSARY on a H1 L-process */
		const int l = newlab ();
		const int pstartlab = newlab ();
		const int joinlab = newlab ();

		/*{{{  generate the processes */
		{
			/* p is the high priority process, q is the low priority process */
			/* wp is the distance between our workspace and p's workspace,
			   wq is the distance between our workspace and q's workspace    */
			treenode *const p = ThisItem (t);
			treenode *const q = ThisItem (NextItem (t));
			INT32 wp = DS_IO + SpMaxwspOf (p);	/* Need DS_IO here cos of ALT CO'N */
			INT32 wq = DS_IO + SpDatasizeOf (p) + SpMaxwspOf (q);
			if (needs_quadalign) {
				wp = (wp + 1) & (~1);	/* round up to even number */
				wq = (wq + 1) & (~1);	/* round up to even number */
			}
			/*{{{  set up the channel to ALT on */
			gennotprocess ();
			/* looks OK for T9000_alpha_badmint bug */
			genprimary (I_STL, 1);	/*           stl chan            */
			gencomment0 ("chan");
			/* channel uses workspace location 1 */
			/* workspace location 0 is hit by the ALT */
			/*}}} */
			/*{{{  set up and run high priority process */
			genloadlabptr (pstartlab, l, "PTR Iptr.s");
			genprimary (I_STL, (-wp) - 1);	/*           stl   -wp - 1       */
			gencomment0 ("PTR startlab");
			#ifdef PROCESS_PRIORITY
				/* better setup priority for high-priority process (artificial -- we don't do PRI PAR..) */
				gensecondary (I_GETPRI);
				genprimary (I_STL, (-wp) - 3);	/*	stl	-wp - 3		*/
				gencomment0 ("priority");
			#endif

			if (L_process)
				/*{{{  Set up trap handler */
			{
				if ((code_style_flags & CODE_STYLE_NO_NULL_TRAPS) != 0) {
					gensecondary (I_LDTH);	/*           ldth                */
#if 0
					/* hi = &lo + 20; */
					genprimary (I_LDNLP, pripar_trap_offset);	/*        ldnlp n             */
#else
					/* hi = *(lo - 2); */
					genprimary (I_LDNL, pripar_trap_offset);	/*        ldnl  n             */
#endif
				} else	/* bug 1344 23/7/91 */
					/* allow for NULL trap handlers */
#if 0
				{
					/*const int target = newlab(); */
					gensecondary (I_LDTH);	/*           ldth                */
					gensecondary (I_MINT);	/* is it the NULL trap handler? */
					/* looks OK for T9000_alpha_badmint bug */
					gensecondary (I_DIFF);	/* leaves zero if TRUE */
#if 0				/* this is slow */
					genbranch (I_CJ, target);	/* jump if NULL trap handler, leaving zero on the stack */
					gencomment0 ("jumps if NULL, leaving zero in Areg");
					genprimary (I_LDC, bytesperword * pripar_trap_offset);	/* offset for Hi pri trap handler */
					setlab (target);
#else /* this is fast */
					genprimary (I_EQC, 0);
					genprimary (I_EQC, 0);	/* now 0 means NULL, 1 otherwise */
					genprimary (I_LDC, bytesperword * pripar_trap_offset);	/* offset for Hi pri trap handler */
					gensecondary (I_PROD);
#endif
					gensecondary (I_LDTH);	/* reload trap handler */
					gensecondary (I_BSUB);	/* add in offset, or zero if it was NULL */
				}
#else
				{
					/* essentially we're calculating:
					   hi = (lo == mint) ? mint : *(lo - 2);
					   This is faster code which doesn't need a pipeline break
					   CON - 01/12/92
					 */
					const int target = newlab ();
					gensecondary (I_MINT);	/* leave mint on the stack, incase lo == mint */
					/* looks OK for T9000_alpha_badmint bug */
					gensecondary (I_LDTH);
					gensecondary (I_MINT);	/* is it the NULL trap handler? */
					/* looks OK for T9000_alpha_badmint bug */
					gensecondary (I_DIFF);	/* leaves zero if TRUE */
					genbranch (I_CJ, target);	/* leaves zero and mint on the stack if taken */
					gensecondary (I_LDTH);	/* reload lo */
					genprimary (I_LDNL, pripar_trap_offset);	/* get *(lo - 2) */
					genprimary (I_LDC, 0);	/* ready for target of the cj */
					setlab (target);
					gensecondary (I_BSUB);	/* drop the zero off the stack */
				}
#endif
				genprimary (I_STL, (-wp) - 3);	/*           stl   -wp - 3       */
				gencomment0 ("PTR Errorhandler.s");
			}
			/*}}} */

			genprimary (I_LDLP, -wp);	/*           ldlp  -wp           */
			gencomment0 ("Hi Wptr");
			if (T9000_alpha_lprocess (&tx_global) && L_process) {	/* Section 5.1 of SW-0341-2. 14/05/92 */
				genprimary (I_ADC, 2);	/*           (set L_process bit) */
				gencomment0 ("L process");
			}

			gensecondary (I_RUNP);	/*           runp                */
			/*{{{  T9000_alpha_badrunp */
			if (T9000_alpha_badrunp (&tx_global)) {
				gensecondary (I_NOP);
				gencomment0 ("Fix for scheduler problem");
				gensecondary (I_NOP);
				gencomment0 ("Fix for scheduler problem");
			}
			/*}}} */
			/*}}} */
			/*{{{  fall into low priority process */
			if (TagOf (SpBodyOf (q)) != S_SKIP) {	/* INSdi01941 */
				/*{{{  run the low priority process */
				genprimary (I_AJW, -wq);	/*           ajw   -wq           */
				gencomment0 ("lo pri process");
				adjustworkspace (wq);
				tprocess (SpBodyOf (q));	/*           Q                   */
				/*{{{  source output */
				/* removed source_output 14/3/91 */
				/*if (source_output)
				   so_endofpar(2, FALSE); */
				/*}}} */
				genprimary (I_AJW, wq);	/*           ajw   +wq           */
				adjustworkspace (-wq);
			}
			/*}}} */
			gensecondary (I_ALT);	/*           alt                 */
			genprimary (I_LDLP, 1);	/*           ldlp  chan          */
			gencomment0 ("chan");
			genprimary (I_LDC, 1);	/* TRUE *//*           ldc   TRUE          */
			gensecondary (I_ENBC);	/*           enbc                */
			gensecondary (I_ALTWT);	/*           altwt               */
			gencomment0 ("sync with hi pri");
			genjump (joinlab);	/*           j     end           */
			/*}}} */
			/*{{{  high priority process */
			adjustworkspace (wp);
			setlab (pstartlab);	/* pstartlab:                    */
			gencomment0 ("hi pri process");
			tprocess (SpBodyOf (p));	/*           P                   */
			/*{{{  source output */
			/* removed source_output 14/3/91 */
			/* if (source_output)
			   so_endofpar(1, TRUE); */
			/*}}} */
			/* This output 'triggers' the ALT, but is never executed */
			genprimary (I_LDLP, 0);	/*           ldlp  any data      */
			genprimary (I_LDLP, wp + 1);	/*           ldlp  chan          */
			gencomment0 ("chan");
			genprimary (I_LDC, 1);	/*           ldc   any len       */
			gensecondary (I_OUT);	/*           out                 */
			gencomment0 ("sync with lo pri");
			/*{{{  T9000_alpha_badrunp */
			if (T9000_alpha_badrunp (&tx_global)) {
				gensecondary (I_NOP);
				gencomment0 ("Fix for scheduler problem");
				gensecondary (I_NOP);
				gencomment0 ("Fix for scheduler problem");
			}
			/*}}} */
			adjustworkspace (-wp);
			/*}}} */
		}
		/*}}} */
		setlab (joinlab);
		/*}}} */
	} else {						/* Traditional way to do PRI PAR */
		/*{{{  traditional PRI PAR */
		const int l = newlab ();
		const int pstartlab = newlab ();
		const int joinlab = newlab ();
		tsetpar (2, joinlab, tptr);
		/*{{{  generate the processes */
		{
			/* p is the high priority process, q is the low priority process */
			/* wp is the distance between our workspace and p's workspace,
			   wq is the distance between our workspace and q's workspace    */
			treenode *const p = ThisItem (t);
			treenode *const q = ThisItem (NextItem (t));
			const INT32 wp = /* DS_MIN + */ SpMaxwspOf (p);	/* Don't need DS_MIN here - CO'N */
			const INT32 wq = /* DS_MIN + */ SpDatasizeOf (p) + SpMaxwspOf (q);
			/*{{{  set up and run high priority process */
			genloadlabptr (pstartlab, l, "PTR Iptr.s");
			genprimary (I_STL, (-wp) - 1);	/*           stl   -wp - 1       */
			gencomment0 ("PTR startlab");

			#ifdef PROCESS_PRIORITY
				gensecondary (I_GETPAS);
				genprimary (I_STL, (-wp) - 3);	/*	stl  -wp - 3	*/
				gencomment0 ("priority");
			#endif

			genprimary (I_LDLP, -wp);	/*           ldlp  -wp           */
			gencomment0 ("Hi Wptr");
			gensecondary (I_RUNP);	/*           runp                */

			/*}}} */
			/*{{{  fall into low priority process */
			if (TagOf (SpBodyOf (q)) != S_SKIP) {	/* INSdi01941 */
				/*{{{  run the low priority process */
				genprimary (I_AJW, -wq);	/*           ajw   -wq           */
				adjustworkspace (wq);
				tprocess (SpBodyOf (q));	/*           Q                   */
				throw_the_result_away ();	/* in case there is dross around */
				/*{{{  source output */
				/* removed source_output 14/3/91 */
				/*if (source_output)
				   so_endofpar(2, FALSE); */
				/*}}} */
				genprimary (I_LDLP, wq);	/*           ldlp  wq            */
				gencomment0 ("joinlab");
				gensecondary (I_ENDP);	/*           endp                */
				adjustworkspace (-wq);
			}
			/*}}} */
			else
				/*{{{  low priority SKIP process */
			{
				throw_the_result_away ();	/* in case there is dross around */
				genprimary (I_LDLP, 0);	/*           ldlp  0             */
				gencomment0 ("joinlab");
				gensecondary (I_ENDP);	/*           endp                */
			}
			/*}}} */
			/*}}} */
			/*{{{  high priority process */
			adjustworkspace (wp);
			setlab (pstartlab);	/* pstartlab:                    */
			tprocess (SpBodyOf (p));	/*           P                   */
			throw_the_result_away ();	/* in case there is dross around */
			/* -- Come down to low priority  */
			/* frmb: if we've got priority, this just induces deadlock */
			#ifndef PROCESS_PRIORITY
				genprimary (I_LDLP, 0);	/*           ldlp  0             */
				genprimary (I_ADC, 1);	/*           adc   1             */
				gensecondary (I_RUNP);	/*           runp                */
				gensecondary (I_STOPP);	/*           stopp               */
			#endif
			/*{{{  source output */
			/* removed source_output 14/3/91 */
			/*if (source_output)
			   so_endofpar(1, TRUE); */
			/*}}} */
			genprimary (I_LDLP, wp);	/*           ldlp  wp            */
			gencomment0 ("joinlab");
			gensecondary (I_ENDP);	/*           endp                */
			adjustworkspace (-wp);
			/*}}} */
		}
		/*}}} */
		gencomment0 (".MAGIC JOINLAB");
		setlab (joinlab);
		/*}}} */
	}
	new_occam_line (t, FALSE, TRUE, TRUE);
	if (!(code_style_flags & CODE_STYLE_ALT_PRI_PAR)) {
		tendpar (2, tptr);
	}
	/* if (debugoutput)
   coder_genlocate(t, FALSE); *//* bug TS/1434 10/03/92 */
}
#endif
/*}}}*/
/*{{{  PRIVATEPARAM void treplpar*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  treplpar sets up the workspace for a replicated process
 *           It assumes that wsp2 contains a pointer to the first special
 *           slot of the replicated workspace, wsp3 contains a pointer to
 *           the vector space.
 *           'proclab' labels the first instruction of the repl. code.
 *
 *****************************************************************************/
/*}}}*/

PRIVATEPARAM void treplpar (treenode * tptr, void *const voidptr, const INT32 proclab)
{
	treenode *const rcptr = voidptr;	/* Pointer to the whole replicated construct tree */
	const int l = newlab ();
	const BIT32 thisbranchwsp = SpMaxwspOf (tptr);
	const BIT32 thisbranchsize = SpDatasizeOf (tptr);
	treenode *const replname = ReplCNameOf (rcptr);
	int rpspecialslots = MIN_REPLPAR_SPECIALS;
	const BIT32 vectorspace = SpVSUsageOf (tptr);
#ifdef MOBILES
	const BIT32 mobilespace = SpMSUsageOf (tptr);
	INT32 ms_temp_slot = -1;
	INT32 msp_slot = -1;
	INT32 ms_tslot, ms_tslot2;
#endif
	const BOOL replisconst = isconst (ReplCLengthExpOf (rcptr));
	int ws_aslot, vs_aslot, acount;

	/*{{{  update rpspecialslots (for vs, ms) here*/
	if (vectorspace != 0) {
		rpspecialslots++;
	}
	if (mobilespace != 0) {
		rpspecialslots++;
	}
	/*}}}*/
	if (replisconst) {
		ws_aslot = vs_aslot = acount = 0;
#ifdef MOBILES
		ms_tslot = ms_tslot2 = 0;
#endif
	} else {
#ifdef MOBILES
		acount = REPLPAR_WS_TEMP + (vectorspace ? (mobilespace ? 3 : 2) : (mobilespace ? 2 : 1));
		ws_aslot = REPLPAR_WS_TEMP + (vectorspace ? (mobilespace ? 4 : 3) : (mobilespace ? 3 : 2));
		vs_aslot = vectorspace ? (REPLPAR_WS_TEMP + (mobilespace ? 5 : 4)) : 0;
		ms_tslot = mobilespace ? (REPLPAR_WS_TEMP + (vectorspace ? 6 : 4)) : 0;
		ms_tslot2 = mobilespace ? (REPLPAR_WS_TEMP + (vectorspace ? 7 : 5)) : 0;
#else	/* !MOBILES */
		acount = REPLPAR_WS_TEMP + (vectorspace ? 2 : 1);
		ws_aslot = REPLPAR_WS_TEMP + (vectorspace ? 3 : 2);
		vs_aslot = vectorspace ? (REPLPAR_WS_TEMP + 4) : 0;
#endif	/* !MOBILES */
	}
	if (!replisconst) {
		/* hide WS_TEMP away in allocated array */
		genprimary (I_LDL, REPLPAR_WS_TEMP);
		gencomment0 ("PTR wstemp");
		genprimary (I_LDNLP, -(int)(thisbranchsize - rpspecialslots));			/* XXX */
		genprimary (I_LDL, ws_aslot);
		gencomment0 ("ws_aslot");
		genprimary (I_LDL, acount);
		gencomment0 ("acount");
		gensecondary (I_SUM);
		genprimary (I_STNL, 0);
		gencomment0 ("ws stored in array");
	}
	/*{{{  if (vectorspace) */
	if (vectorspace != 0) {
		if (replisconst) {
			genprimary (I_LDL, REPLPAR_VS_TEMP);	/* ldl   vstemp              */
			gencomment0 ("PTR vstemp");
			genprimary (I_LDL, REPLPAR_WS_TEMP);	/* ldl   wstemp              */
			gencomment0 ("PTR wstemp");
			genprimary (I_STNL, REPLPAR_VSP);	/* stnl  rpvsp               */
			gencomment0 ("PTR rpvsp");
			genprimary (I_LDL, REPLPAR_VS_TEMP);	/* ldl   vstemp              */
			gencomment0 ("PTR vstemp");
			genprimary (I_LDNLP, vectorspace);	/* ldnlp vs for repl.process */
			genprimary (I_STL, REPLPAR_VS_TEMP);	/* stl   vstemp              */
			gencomment0 ("PTR vstemp");
		} else if (disable_dynmem) {
			/* dynamic memory disabled, generate error */
			generr (GEN_NO_DYNMEM);
		} else {
			/* pull some vector-space off the heap (need to hide in array as well):
			 *	ldc	<size required>
			 *	malloc
			 *	dup
			 *	ldl	wstemp
			 *	stnl	REPLPAR_VSP
			 *	ldl	vs_aslot
			 *	ldl	acount
			 *	sum
			 *	stnl	0
			 */
			gencomment0 ("allocate some vectorspace");
			genprimary (I_LDC, (vectorspace << WSH));
			gensecondary (I_MALLOC);
			gensecondary (I_DUP);
			genprimary (I_LDL, REPLPAR_WS_TEMP);
			gencomment0 ("PTR wstemp");
			genprimary (I_STNL, REPLPAR_VSP);
			gencomment0 ("rpvsp");
			genprimary (I_LDL, vs_aslot);
			gencomment0 ("vs_aslot");
			genprimary (I_LDL, acount);
			gencomment0 ("acount");
			gensecondary (I_SUM);
			genprimary (I_STNL, 0);
			gencomment0 ("vs stored in array");
		}
	}
	/*}}} */
#ifdef MOBILES
	/*{{{  if (mobilespace) */
	if (enable_mobilespace && (mobilespace != 0)) {
		if (!vectorspace) {
			ms_temp_slot = REPLPAR_VS_TEMP;
			msp_slot = REPLPAR_VSP;
		} else {
			ms_temp_slot = REPLPAR_MS_TEMP;
			msp_slot = REPLPAR_MSP;
		}

		if (replisconst) {
			genprimary (I_LDL, ms_temp_slot);	/* ldl mstemp */
			gencomment0 ("PTR mstemp");
			genprimary (I_LDL, REPLPAR_WS_TEMP);	/* ldl wstemp */
			gencomment0 ("PTR wstemp");
			genprimary (I_STNL, msp_slot);		/* stnl rpmsp */
			gencomment0 ("PTR rpmsp");
			genprimary (I_LDL, ms_temp_slot);	/* ldl mstemp */
			gencomment0 ("PTR mstemp");
			genprimary (I_LDNLP, mobilespace);	/* ldnlp ms for process */
			genprimary (I_STL, ms_temp_slot);	/* stl mstemp */
		} else if (disable_dynmem) {
			generr (GEN_NO_DYNMEM);
		} else {
			const int si_lab = newlab ();
			const int lp_lab = newlab ();
			const int le_lab = newlab ();
			/* if the pointer is MINT, need to allocate + init our mobilespace
			 * (ms_temp_slot has the offset of the 2-word space).*/

			gencomment0 ("fetch mobilespace from array");
			genprimary (I_LDL, ms_temp_slot);
			genprimary (I_LDNL, 0);			/* load array pointer */
			genprimary (I_LDL, acount);		/* load acount */
			gensecondary (I_SUM);
			gensecondary (I_DUP);
			genprimary (I_STL, ms_tslot);		/* stuff addr of ms-pointer in temp slot */
			genprimary (I_LDNL, 0);			/* load repl. process's ms pointer */
			gensecondary (I_MINT);
			gensecondary (I_DIFF);			/* ms-pointer == not here ? */
			genboolinvert ();
			genbranch (I_CJ, si_lab);		/* jump if valid pointer */
			throw_the_result_away ();

			/* allocate and init mobilespace.. */
			genprimary (I_LDC, (mobilespace << WSH));
			gensecondary (I_MALLOC);		/* allocate mobile-space */
			genprimary (I_LDL, ms_tslot);
			genprimary (I_STNL, 0);			/* stuff into array */

			genprimary (I_LDC, ((mobilespace - 1) << WSH));
			genprimary (I_STL, ms_tslot2);		/* store first offset into ms_tslot2 */

			setlab (lp_lab);
			throw_the_result_away ();
			genprimary (I_LDL, ms_tslot);
			genprimary (I_LDNL, 0);			/* load pointer to ms */
			genprimary (I_LDL, ms_tslot2);		/* load offset */
			gensecondary (I_SUM);			/* add to get offset in mobilespace */
			gensecondary (I_MINT);
			gensecondary (I_REV);
			genprimary (I_STNL, 0);			/* store MINT in mobile-space */

			genprimary (I_LDL, ms_tslot2);		/* load offset */
			genbranch (I_CJ, le_lab);
			throw_the_result_away ();
			genprimary (I_LDL, ms_tslot2);
			genprimary (I_LDC, 4);
			gensecondary (I_DIFF);
			genprimary (I_STL, ms_tslot2);
			genbranch (I_J, lp_lab);

			setlab (le_lab);
			throw_the_result_away ();
			genprimary (I_LDL, ms_tslot);
			genprimary (I_LDNL, 0);			/* load repl. process's ms pointer */
			genprimary (I_STL, ms_tslot2);		/* store in ms_tslot2 */

			if (SpMSPtrOf (tptr)) {
				/* need to generate mobile-space init code for this.. */
				genmobileinitdynpar (ms_tslot2, tptr);
			}

			setlab (si_lab);
			throw_the_result_away ();
			genprimary (I_LDL, ms_tslot);
			genprimary (I_LDNL, 0);			/* load repl. process's ms pointer */
			genprimary (I_LDL, REPLPAR_WS_TEMP);
			genprimary (I_STNL, msp_slot);		/* store in repl. process's MSP slot */
			gencomment0 ("ms set in to replicated process");
		}
	}
	/*}}}  */
#endif
	/*{{{  insert repl static link */
	genprimary (I_LDLP, 0);	/*     ldlp   0                   */
	genprimary (I_LDL, REPLPAR_WS_TEMP);	/*     ldl    wstemp              */
	gencomment0 ("PTR wstemp");
	genprimary (I_STNL, REPLPAR_STATICLINK);	/*     stnl   rpstaticlink        */
	gencomment0 ("PTR rpstatic");
	/*}}} */
	/*{{{  insert replicator value */
	loadname (replname, REPL_BASE);	/*        ldl    i                   */
	genprimary (I_LDL, REPLPAR_WS_TEMP);	/*         ldl    wstemp              */
	gencomment0 ("PTR wstemp");
	genprimary (I_STNL, REPLPAR_REPLICATOR);	/*     stnl   rpreplicator        */
	gencomment0 ("rprepl");
	/*}}} */
	/*{{{  start process */
	genlabeldiff (I_LDC, (int) proclab, l);	/*        ldc    proclab - l         */
	genprimary (I_LDL, REPLPAR_WS_TEMP);	/*         ldl    wstemp              */
	gencomment0 ("PTR wstemp");
	genprimary (I_LDNLP, rpspecialslots - thisbranchwsp);
	/*         ldnlp  rpspecials-replwsp  */
	gensecondary (I_STARTP);	/*         startp                     */
	setlab (l);		/* l:                                 */

	/*}}} */
	/*{{{  update wsp temporary */
	if (replisconst) {
		genprimary (I_LDL, REPLPAR_WS_TEMP);	/*         ldl    wstemp              */
		gencomment0 ("PTR wstemp");
		genprimary (I_LDNLP, -thisbranchsize);	/*       ldnlp  -ds                 */
		genprimary (I_STL, REPLPAR_WS_TEMP);	/*         stl    wstemp              */
		gencomment0 ("PTR wstemp");
	} else if (disable_dynmem) {
		generr (GEN_NO_DYNMEM);
	} else {
		genprimary (I_LDC, (int)(thisbranchsize << WSH));
		gensecondary (I_MALLOC);
		genprimary (I_LDNLP, thisbranchsize - rpspecialslots);				/* XXX */
		genprimary (I_STL, REPLPAR_WS_TEMP);
		gencomment0 ("PTR wstemp");
		gencomment0 ("updating acount");
		genprimary (I_LDL, acount);
		genprimary (I_LDC, 4);
		gensecondary (I_SUM);
		genprimary (I_STL, acount);
		gencomment0 ("acount");
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE BOOL issimpleassertionguard*/
PRIVATE BOOL issimpleassertionguard (treenode * tptr)
  /* return FALSE if there is a AND or OR at the top level, excluding NOTs.
     This is because if there aren't any short-circuiting operators,
     then it is quickest to evaluate the whole thing as an assertion
   */
{
	while (TRUE)
		switch (TagOf (tptr)) {
		default:
			return TRUE;
		case S_AND:
		case S_OR:
			return FALSE;
		case S_NOT:
			tptr = OpOf (tptr);
			break;
		}
}

/*}}}*/
/*{{{  PRIVATEPARAM void tchoice(tptr, trueguard, endlabel)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tchoice generates code for a choice node (ie. guard followed by body).
 *          'tptr' is the choice node, 'trueguard' is a var parameter which
 *          we set to TRUE if we find a 'TRUE' guard.
 *          'endlabel' is the label which marks the end of the whole IF.
 *
 *****************************************************************************/
/*}}}*/
PRIVATEPARAM void tchoice (treenode * tptr, void *const voidptr, const INT32 endlabel)
{
	int *const trueguard = voidptr;
	treenode *specsptr = tptr;

	tptr = tspecs (tptr);
	switch (TagOf (tptr)) {
		/*{{{  S_IF S_REPLIF */
	case S_IF:
	case S_REPLIF:
		tif (tptr, trueguard, (int) endlabel, FALSE);
		break;
		/*}}} */
		/*{{{  S_CHOICE */
	case S_CHOICE:
		/*{{{  generate the choice */
		{
			new_occam_line (tptr, TRUE, TRUE, TRUE);
			/* if (debugoutput)
			   coder_genlocate(tptr, TRUE); */
			if (isskipbody (CondBodyOf (tptr)))
				/*{{{  if condition, jump to endlabel, else fall through */
			{
				if (istrueguard (CondGuardOf (tptr)))
					*trueguard = TRUE;	/* A TRUE guard is always the last guard */
				else if (isfalseguard (CondGuardOf (tptr)))	/* bug 307 28/8/90 */
					;	/* skip */
				else
					tguard (CondGuardOf (tptr), FALSE, (int) endlabel);
			}
			/*}}} */
			else if (!istrueguard (CondGuardOf (tptr)) &&
				 (TagOf (CondBodyOf (tptr)) == S_STOP) && issimpleassertionguard (CondGuardOf (tptr)))
				/*{{{  This is a simple 'assertion' */
			{
				/* This is basically ASSERT( ! guard ) */
				genboolassertion (CondGuardOf (tptr), FALSE, CondBodyOf (tptr), errormode);
			}
			/*}}} */
			else {
				int nextchoice = NO_LABEL;
				if (istrueguard (CondGuardOf (tptr)))
					/* if we have a TRUE guard we don't need to generate a STOP after the
					   IF   */
					*trueguard = TRUE;
				else {
					nextchoice = newlab ();
					tpreexp (CondGuardOf (tptr));
					tguard (CondGuardOf (tptr), TRUE, nextchoice);
				}
				tprocess (CondBodyOf (tptr));
				genjump ((int) endlabel);
				if (nextchoice != NO_LABEL)
					setlab (nextchoice);
			}
		}
		/*}}} */
		break;
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "tchoice");
		break;
	}
	tdespecs (specsptr);
}

/*}}}*/
/*{{{  PRIVATE void tif*/
/*{{{  words*/
/*****************************************************************************
 *
 *  tif generates code for an IF process.
 *      tptr is the process, trueguard is set to TRUE if a 'TRUE'
 *      guard is found.
 *      N.B. IF processes need a STOP generated if they don't have a 'TRUE'
 *           guard.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void tif (treenode * const tptr, int *const trueguard, const int endlabel, const BOOL outer_level)
{
	if (TagOf (tptr) == S_REPLIF) {
		new_occam_line (tptr, TRUE, TRUE, TRUE);
		/* if (debugoutput)
		   coder_genlocate(tptr, TRUE); */
		tpreexp (ReplCStartExpOf (tptr));
		tpreexp (ReplCLengthExpOf (tptr));
		if (ReplCStepExpOf (tptr)) {
			tpreexp (ReplCStepExpOf (tptr));
		}
		trepl (tptr, tchoice, trueguard, endlabel, TRUE);
	}
	/*{{{  test for IF test SKIP [TRUE STOP] */
	else if (outer_level &&
		 (listitems (CBodyOf (tptr)) == 1) &&
		 (TagOf (ThisItem (CBodyOf (tptr))) == S_CHOICE) &&
		 isskipbody (CondBodyOf (ThisItem (CBodyOf (tptr)))) && issimpleassertionguard (CondGuardOf (ThisItem (CBodyOf (tptr))))) {
		/* This tests for:
		   IF
		   test
		   SKIP
		   -- with an implicit TRUE STOP
		   NOTE: these are generated by tagged inputs: chan ? CASE tag; data,
		   also, of course, they can appear in user code.
		 */
		treenode *const choice = ThisItem (CBodyOf (tptr));
		new_occam_line (choice, TRUE, TRUE, TRUE);
		/* if (debugoutput)
		   coder_genlocate(choice, TRUE); */
		genboolassertion (CondGuardOf (choice), TRUE, tptr, errormode);
		*trueguard = TRUE;
	}
	/*}}} */
	else {
		treenode *choicelist;
		for (choicelist = CBodyOf (tptr); !EndOfList (choicelist); choicelist = NextItem (choicelist))
			tchoice (ThisItem (choicelist), trueguard, endlabel);
	}
}

/*}}}*/

/*{{{  PRIVATE void tconstcopyswap (BYTE *dst, const BYTE * src, const int length, const int swap)*/
PRIVATE void tconstcopyswap (BYTE *dst, const BYTE *src, const int length, const int swap)
{
	int mask = target_bigendian ? bytesinscalar (swap) - 1 : 0;
	int i;

	for (i = 0; i < length; i++) {
		dst[i ^ mask] = src[i];
	}
}
/*}}}*/
/*{{{  PRIVATE void tconstrecord (const treenode *type, BYTE *dst, const BYTE * src, const int length)*/
PRIVATE void tconstrecord (const treenode *type, BYTE *dst, const BYTE *src, const int length)
{	
	
	/* const BOOL packed = (TypeAttrOf (type) & TypeAttr_packed) != 0; */
	const int elembytes = bytesin (type);
	treenode *decl;
	int pos = 0;

	while (pos < length) {
		for (decl = ARTypeOf (type); decl != NULL; decl = DBodyOf (decl)) {
			treenode *const field_name = DNameOf (decl);
			treenode *const field_type = NTypeOf (field_name);
			const int offset = NVOffsetOf (field_name);
			treenode *basetype = field_type;
			BOOL array = FALSE;
			int bytes = bytesin (field_type);

			while (TagOf (basetype) == S_ARRAY || TagOf (basetype) == N_TYPEDECL) {
				switch (TagOf (basetype)) {
					case S_ARRAY:
						array = TRUE;
						basetype = ARTypeOf (basetype);
						break;
					case N_TYPEDECL:
						basetype = follow_user_type (basetype);
						break;
				}
			}

			#if 0
			fprintf (stderr, "field_type = ");
			printtreenl (stderr, 4, field_type);
			fprintf (stderr, "basetype = ");
			printtreenl (stderr, 4, basetype);
			#endif

			if (TagOf (basetype) == S_RECORD) {
				tconstrecord (basetype, dst + offset, src + offset, bytes);
			} else {
				int swap = TagOf (basetype);
				
				if (isreal (swap) && has_fpu_core) {
					/* skip */
				} else if (bytesinscalar (targetintsize) < bytesinscalar (swap)) {
					swap = targetintsize;
				} 
				
				tconstcopyswap (dst + offset, src + offset, bytes, swap);
			}
		}
		dst += elembytes;
		src += elembytes;
		pos += elembytes;
	}
}
/*}}}*/
/*{{{  PRIVATE void tconsttable (tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tconsttable generates the constant table tptr
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void tconsttable (treenode * tptr, treenode * type_tptr, const char *type_string, const char *description)
{
	/* bug 1129 28/1/91 - we MUST word align everything, so that
	   any constant array is known to be word aligned.
	   This permits any array to be passed to KERNEL.RUN, for instance.
	 */
	const wordnode *w = (wordnode *) (CTValOf (tptr));
	const int length = WLengthOf (w);
	const BYTE *src = (const BYTE *) WNameOf (w);
	treenode *c;
	BYTE *dst = (BYTE *) memalloc (length);
	int swap = targetintsize;
	
	DEBUG_MSG (("tconsttable\n"));
#if 0
fprintf (stderr, "tconsttable(): ..\n");
#endif
	
	memset (dst, 0, length);

	if ((TagOf (tptr) == S_ARRAY) || type_tptr) {
		treenode *basetype = type_tptr;
		
		if (TagOf (tptr) == S_CONSTCONSTRUCTOR) {
			treenode *ctptr = ConstTableTypeOf (tptr);

			if (ctptr != NULL) {
				if ((TagOf (ctptr) == S_ARRAY) || (TagOf (ctptr) == N_TYPEDECL)) {
					basetype = ctptr;
				}
			}
		} else if (TagOf (tptr) == S_ARRAY) {
			basetype = tptr;
		}

		while (TagOf (basetype) == S_ARRAY || TagOf (basetype) == N_TYPEDECL) {
			
			switch (TagOf (basetype)) {
				case S_ARRAY:
					basetype = ARTypeOf (basetype);
					break;
				case N_TYPEDECL:
					basetype = follow_user_type (basetype);
					break;
			}
		}

		swap = TagOf (basetype);

		if (swap == S_RECORD) {
			tconstrecord (basetype, dst, src, length);
		} else {
			if (isreal (swap) && has_fpu_core) {
				/* skip */
			} else if (bytesinscalar (targetintsize) < bytesinscalar (swap)) {
				swap = targetintsize;
			}
			tconstcopyswap (dst, src, length, swap);
		}

		w = lookupword ((char *) dst, length);
	} else if (TagOf (tptr) == S_STRING) {
		/* skip */
	} else {
		tconstcopyswap (dst, src, length, swap);
		w = lookupword ((char *) dst, length);
	}

	SetCTVal (tptr, (wordnode *) w);
	memfree (dst);

	c = constantmatch (tptr, globaltablechain);

	if (c != NULL) {
		SetCTLabel (tptr, CTLabelOf (c));
	} else {
		const BYTE *data = (const BYTE *) WNameOf (w);
		const int l = newlab ();

		genstartconstblock (description);

		/* bug TS/1686 02/07/92 */
		coder_mark_alignment ();	/* the next byte will be word aligned */

		SetCTLabel (tptr, l);
		setsectionlab (l);

		/*if (description != NULL) gencomment0(description); */

		add_const_block (length, data, type_tptr, type_string, S_BYTE);	/* MDP */

		if (!ims_asm_output)	/* bug TS/1686 02/07/92 */
			endsectionalign (length);
		
		compress_code ();

		/* bug 1158 - add all constant tables onto a chain, so that we can use
		   it to prevent duplicates */
		SetCTNext (tptr, globaltablechain);
		globaltablechain = tptr;
		genendconstblock ();
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM void trepl_process*/
PRIVATEPARAM void trepl_process (treenode * const tptr, void *voidptr, INT32 dummy)
{
	voidptr = voidptr;
	dummy = dummy;
	tprocess (tptr);
}

/*}}}*/
/*{{{  PRIVATE void tinitconsttable*/
PRIVATE void tinitconsttable (void)
{
	const int lab = newlab ();
	genloadlabptr (scalarconstlab, lab, "constant table");
}

/*}}}*/
/*{{{  PUBLIC void tloadconsttable(void)*/
PUBLIC void tloadconsttable (void)
{
	if (constptr == CONSTANTPOINTER_NOT_USED)
		tinitconsttable ();	/* bug TS/1745 11/08/92 */
	else {
		genprimary (I_LDL, constptr + nameoffsetof (be_lexlevel));
		gencomment0 ("PTR constptr");
	}
}

/*}}}*/
/*{{{  PRIVATE void tstoreconsttable*/
PRIVATE void tstoreconsttable (const INT32 cp_offset)
{
	constptr = cp_offset;

	if (cp_offset != CONSTANTPOINTER_NOT_USED) {
		tinitconsttable ();
		genprimary (I_STL, cp_offset + nameoffsetof (be_lexlevel));
		gencomment0 ("INT constptr");
	}
}

/*}}}*/
/*{{{  PUBLIC void tprofcountupdate(INT32 index)*/
PUBLIC void tprofcountupdate (INT32 index)
{
	/* We have to generate the following sequence of instructions.
	   ldp count_table; ldnl count_num; ldc 1; sum;
	   ldp count_table; stnl count_num
	 */
	const int lab = newlab ();
	genloadlabptr (proftabconstlab, lab, "Profile table");
	gensecondary (I_DUP);
	genprimary (I_LDNL, index);
	genprimary (I_LDC, 1);
	gensecondary (I_SUM);
	gensecondary (I_REV);
	genprimary (I_STNL, index);
}

/*}}}*/
#ifdef MOBILES
/*{{{  PRIVATE void gendynamicmobilespace (int ms_temp_slot, int ms_tslot, int ms_tslot2, treenode *length_exp)*/
/*
 *	does the tricky bit for allocating dynamic mobile-space
 *	(i've opted for correctness rather than efficiency here.. -- frmb2)
 */
PRIVATE void gendynamicmobilespace (int ms_temp_slot, int ms_tslot, int ms_tslot2, treenode *length_exp)
{
	const int alab = newlab ();
	const int outlab = newlab ();

	genprimary (I_LDL, ms_temp_slot);	/* load pointer to 2-slot space in mobilespace */
	genprimary (I_LDNL, 1);			/* load array count */
	genbranch (I_CJ, alab);			/* branch if 0, to allocate for the first time */
	throw_the_result_away ();

	/* something already allocated */
	texp (length_exp, MANY_REGS);		/* load replication count */
	gensecondary (I_DUP);
	genprimary (I_STL, ms_tslot2);		/* stash away in ms_tslot2 for now */
	genprimary (I_LDL, ms_temp_slot);
	genprimary (I_LDNL, 1);			/* load array count */
	gensecondary (I_GT);			/* repl count > array count ? */
	genbranch (I_CJ, outlab);		/* no, then jump out. */
	throw_the_result_away ();

	/* not enough, allocate some more.. */
	genprimary (I_LDL, ms_tslot2);
	genshiftimmediate (I_SHL, WSH);
	gensecondary (I_MALLOC);
	genprimary (I_STL, ms_tslot);		/* pop new pointer in here */

	/* copy over existing array elements */
	{
		const int loop_start_lab = newlab ();

		genprimary (I_LDL, ms_temp_slot);
		genprimary (I_LDNL, 0);			/* load existing array pointer */
		genprimary (I_LDL, ms_tslot);		/* load new array pointer */
		genprimary (I_LDL, ms_temp_slot);
		genprimary (I_LDNL, 1);			/* load array count */
		genshiftimmediate (I_SHL, WSH);
		gensecondary (I_MOVE);
		throw_the_result_away ();

		/* free old array */
		genprimary (I_LDL, ms_temp_slot);
		genprimary (I_LDNL, 0);			/* load existing array pointer */
		gensecondary (I_MRELEASE);		/* free memory */
		genprimary (I_LDL, ms_tslot);
		genprimary (I_LDL, ms_temp_slot);
		genprimary (I_STNL, 0);			/* store new array pointer */

		/* set new entries to MINT */
		genprimary (I_LDL, ms_tslot);		/* load array pointer */
		genprimary (I_LDL, ms_temp_slot);
		genprimary (I_LDNL, 1);			/* load old array count */
		genshiftimmediate (I_SHL, WSH);
		gensecondary (I_SUM);			/* address of first new slot */
		genprimary (I_STL, ms_tslot);		/* store in ms_tslot */

		genprimary (I_LDL, ms_tslot2);		/* load new array count */
		gensecondary (I_DUP);
		genprimary (I_LDL, ms_temp_slot);
		genprimary (I_LDNL, 1);			/* load old array count */
		gensecondary (I_DIFF);
		genprimary (I_STL, ms_tslot2);		/* store (new_count - old_count) in ms_tslot2 */
		genprimary (I_LDL, ms_temp_slot);
		genprimary (I_STNL, 1);			/* put new array count in count slot */

		setlab (loop_start_lab);
		throw_the_result_away ();
		gensecondary (I_MINT);
		genprimary (I_LDL, ms_tslot);		/* mobilespace array pointer */
		genprimary (I_STNL, 0);			/* store MINT in array  */

		genprimary (I_LDL, ms_tslot);		/* mobilespace array pointer */
		genprimary (I_LDC, 4);
		gensecondary (I_SUM);
		genprimary (I_STL, ms_tslot);		/* next please */

		genprimary (I_LDL, ms_tslot2);
		genprimary (I_LDC, 1);
		gensecondary (I_DIFF);
		gensecondary (I_DUP);
		genprimary (I_STL, ms_tslot2);
		genboolinvert ();
		genbranch (I_CJ, loop_start_lab);
	}
	genbranch (I_J, outlab);

	/* nothing allocated yet */
	setlab (alab);
	throw_the_result_away ();
	texp (length_exp, MANY_REGS);		/* load replication count */

	gensecondary (I_DUP);			/* copy */
	genprimary (I_LDL, ms_temp_slot);	/* load pointer to 2-slots in ms */
	genprimary (I_STNL, 1);			/* store array count */

	genshiftimmediate (I_SHL, WSH);
	gensecondary (I_MALLOC);		/* allocate space */
	genprimary (I_LDL, ms_temp_slot);	/* load pointer to 2-slots in ms */
	genprimary (I_STNL, 0);			/* store array pointer */

	/* MINT array elements */
	{
		const int loop_start_lab = newlab ();

		genprimary (I_LDL, ms_temp_slot);
		genprimary (I_LDNL, 1);		/* load array count */
		genprimary (I_LDC, 1);
		gensecondary (I_DIFF);		/* minus one */
		genprimary (I_STL, ms_tslot);	/* store in ms_tslot */

		setlab (loop_start_lab);
		genprimary (I_LDL, ms_temp_slot);
		genprimary (I_LDNL, 0);		/* load array pointer */
		genprimary (I_LDL, ms_tslot);
		genshiftimmediate (I_SHL, WSH);
		gensecondary (I_SUM);
		gensecondary (I_MINT);
		gencomment0 ("MINT");
		gensecondary (I_REV);
		genprimary (I_STNL, 0);		/* store MINT */

		genprimary (I_LDL, ms_tslot);
		genbranch (I_CJ, outlab);
		throw_the_result_away ();
		genprimary (I_LDL, ms_tslot);
		genprimary (I_LDC, 1);
		gensecondary (I_DIFF);
		genprimary (I_STL, ms_tslot);
		genbranch (I_J, loop_start_lab);
	}

	setlab (outlab);
	throw_the_result_away ();
	return;
}
/*}}}*/
#endif	/* MOBILES */

/*{{{  PUBLIC void tprocess (tptr)*/
/* Generate code for the process represented by tptr */
PUBLIC void tprocess (treenode * tptr)
{
	while (tptr != NULL) {
		treenode *specsptr = tptr;

		genlocn = LocnOf (tptr);
		tptr = tspecs (tptr);
		new_occam_line (tptr, TRUE, need_locate (TagOf (tptr)), TRUE);
		/* if (debugoutput && need_locate(TagOf(tptr)))
		   coder_genlocate(tptr, TRUE); */
		switch (TagOf (tptr))
			/*{{{  cases */
		{
			/*{{{  S_SKIP                 return */
		case S_SKIP:
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_STOP                 return */
		case S_STOP:
			tstop (tptr);
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_SUSPEND              return*/
		case S_SUSPEND:
			tsuspend (tptr);
			tdespecs (specsptr);
			return;
			/*}}}*/
			/*{{{  S_SYNC                 return*/
		case S_SYNC:
			tsync (LeafLinkOf (tptr));
			tdespecs (specsptr);
			return;
			/*}}}*/
			/*{{{  S_FORKING              break*/
		case S_FORKING:
			/* .. generate code to initialise the barrier hanging off here */
			{
				treenode *b_nptr = DNameOf (CBodyOf (tptr));

				/* barrier is initialised here, but it could be done in tdecl() as well */
				/*{{{  initialise the barrier*/
				gencomment0 ("{{{  fork-setup");
				gencomment0 ("{{{  allocate fork barrier");
				loadconstant (0);
				loadconstant (MT_MAKE_BARRIER (MT_BARRIER_FORKING));
				gensecondary (I_MT_ALLOC);
				storeinname (b_nptr, 0);
				gencomment0 ("}}}");
				gencomment0 ("}}}");
				/*}}}*/

				gencomment0 ("{{{  fork-process");
				tprocess (CBodyOf (tptr));
				gencomment0 ("}}}");

				/*{{{  synchronise on the barrier*/
				gencomment0 ("{{{  fork-cleanup");
				gencomment0 ("{{{  synchronise and release fork barrier");
				loadname (b_nptr, 0);
				gensecondary (I_MT_SYNC);
				gencomment0 ("}}}");
				gencomment0 ("}}}");
				/*}}}*/

				tptr = NULL;
			}
			break;
			/*}}}*/
			/*{{{  S_CLAIM                break*/
		case S_CLAIM:
			/* ... generate code to claim the semaphore, then release it again.  Have local 0 for a temporary */
			{
				treenode *ct_nptr = CTempOf (tptr);
				treenode *ct_type = gettype_main_orig (ct_nptr);

				if (isdynmobilechantype (ct_nptr)) {
					/* const int refc_offset = 0; */
					const int chans = (bytesin (ct_type) / bytesperword);		/* client at this + 1, server at this + 4, maybe more if chantype_desc */
					const int is_client = (NTypeAttrOf (ct_type) & TypeAttr_marked_out);
					const int lock = (is_client ? MT_CB_CLIENT : MT_CB_SERVER);
					const int pony_offset = chans;					/* offset of pony state in words */

					gencomment0 ("begin claim semaphore");
					loadmobile (ct_nptr);
					genchecknotnull ();
					loadconstant (lock);
					gensecondary (I_MT_LOCK);
					gencomment0 ("end claim semaphore");
					/*{{{  if chan-type operations or kroc-net specials are enabled, signal claim/set state*/
					if (kroc_chantype_desc) {
						gencomment0 ("{{{  kroc chantype special");
						if (kroc_chantype_knsf && kroc_chantype_uio) {
							/*{{{  KRoC.net code for CLAIM*/
							const int iskiplab = newlab ();
							const int ijoinlab = newlab ();

							loadmobile (ct_nptr);
							genprimary (I_LDNLP, pony_offset + PONY_STATESEM);
							gensemop (SEMOP_CLAIM);

							loadmobile (ct_nptr);
							genprimary (I_LDNL, pony_offset + PONY_UIOHOOK);
							genboolinvert ();
							genbranch (I_CJ, iskiplab);			/* operations-hook is non-null, so skip */

							/* change value in state field */
							loadmobile (ct_nptr);
							genprimary (I_LDNL, pony_offset + PONY_STATE);	/* load current state */
							loadconstant (is_client ? 0x0001 : 0x00010000);
							gensecondary (I_SUM);				/* advance state from 2 to 3 */
							loadmobile (ct_nptr);
							genprimary (I_STNL, pony_offset + PONY_STATE);	/* save modified state */

							/* release semaphore */
							loadmobile (ct_nptr);
							genprimary (I_LDNLP, pony_offset + PONY_STATESEM);
							gensemop (SEMOP_RELEASE);
							genbranch (I_J, ijoinlab);

							/* get here if ops-hook is non-null (i.e. it's a KRoC.net networked channel-type) */
							setlab (iskiplab);

							loadmobile (ct_nptr);
							genprimary (I_LDNLP, pony_offset + PONY_STATESEM);
							gensemop (SEMOP_RELEASE);

							/* do uio communication (to KRoC.net kernel in this case) */
							/* note: the pointer in the chantype is to the first (client)
							 *       channel word, server stacked above that
							 */
							loadmobile (ct_nptr);
							gensecondary (I_DUP);
							genprimary (I_LDNL, pony_offset + PONY_UIOHOOK);
							if (is_client) {
								/* genprimary (I_LDNLP, 0);	*/		/* load address of "client" channel */
							} else {
								genprimary (I_LDNLP, 1);			/* load address of "server" channel */
							}
							gensecondary (I_REV);				/* Areg = ct_nptr, Breg = channel */
							tioop (I_OUTWORD, FALSE);

							setlab (ijoinlab);
							/*}}}*/
						} else if (kroc_chantype_uio) {
							/*{{{  UIO code for CLAIM*/
							const int iskiplab = newlab ();

							loadmobile (ct_nptr);
							genprimary (I_LDNL, pony_offset + PONY_UIOHOOK);
							genbranch (I_CJ, iskiplab);		/* operations-hook is NULL, so skip */
							loadmobile (ct_nptr);
							gensecondary (I_DUP);
							genprimary (I_LDNL, pony_offset + PONY_UIOHOOK);
							if (is_client) {
								genprimary (I_LDNL, 0);			/* load address of "client" channel */
							} else {
								genprimary (I_LDNL, 1);			/* load address of "server" channel */
							}
							gensecondary (I_REV);				/* Areg = ct_nptr, Breg = channel */
							tioop (I_OUTWORD, FALSE);

							setlab (iskiplab);
							/*}}}*/
						}
						gencomment0 ("}}}");
					}
					/*}}}*/

					tprocess (CBodyOf (tptr));

					/*{{{  if chan-type operations are enabled, signal release*/
					if (kroc_chantype_desc) {
						gencomment0 ("{{{  kroc chantype special");
						if (kroc_chantype_knsf && kroc_chantype_uio) {
							/*{{{  KRoC.net code for end of CLAIM (release)*/
							const int iskiplab = newlab ();
							const int ijoinlab = newlab ();

							loadmobile (ct_nptr);
							genprimary (I_LDNLP, pony_offset + PONY_STATESEM);
							gensemop (SEMOP_CLAIM);

							loadmobile (ct_nptr);
							genprimary (I_LDNL, pony_offset + PONY_UIOHOOK);
							genboolinvert ();
							genbranch (I_CJ, iskiplab);			/* operations-hook is non-null, so skip */

							/* change value in state field */
							loadmobile (ct_nptr);
							genprimary (I_LDNL, pony_offset + PONY_STATE);	/* load current state */
							loadconstant (is_client ? 0x0001 : 0x00010000);
							gensecondary (I_DIFF);				/* reduce state from 3 to 2 */
							loadmobile (ct_nptr);
							genprimary (I_STNL, pony_offset + PONY_STATE);	/* save modified state */

							/* release semaphore */
							loadmobile (ct_nptr);
							genprimary (I_LDNLP, pony_offset + PONY_STATESEM);
							gensemop (SEMOP_RELEASE);

							genbranch (I_J, ijoinlab);

							/* get here if channel-type is networked */
							setlab (iskiplab);

							loadmobile (ct_nptr);
							genprimary (I_LDNLP, pony_offset + PONY_STATESEM);
							gensemop (SEMOP_RELEASE);

							/* do uio communication (to KRoC.net kernel in this case) */
							/* note: the pointer in the chantype is to the first (client)
							 *       channel word, server stacked above that
							 */
							loadmobile (ct_nptr);
							genprimary (I_LDNL, pony_offset + PONY_UIOHOOK);
							genbranch (I_CJ, iskiplab);		/* operations-hook is NULL, so skip */
							loadmobile (ct_nptr);
							gensecondary (I_DUP);
							genprimary (I_LDNL, pony_offset + PONY_UIOHOOK);
							if (is_client) {
								/* genprimary (I_LDNL, 0); */			/* load address of "client" channel */
							} else {
								genprimary (I_LDNLP, 1);			/* load address of "server" channel */
							}
							gensecondary (I_REV);				/* Areg = ct_nptr, Breg = channel */
							genloadconstant (1);
							gensecondary (I_OR);				/* or with 1 to indicate release */
							tioop (I_OUTWORD, FALSE);

							setlab (ijoinlab);
							/*}}}*/
						} else if (kroc_chantype_uio) {
							/*{{{  UIO code for end of CLAIM (release)*/
							const int iskiplab = newlab ();

							loadmobile (ct_nptr);
							genprimary (I_LDNL, pony_offset + PONY_UIOHOOK);
							genbranch (I_CJ, iskiplab);		/* operations-hook is NULL, so skip */
							loadmobile (ct_nptr);
							gensecondary (I_DUP);
							genprimary (I_LDNL, pony_offset + PONY_UIOHOOK);
							if (is_client) {
								genprimary (I_LDNL, 0);			/* load address of "client" channel */
							} else {
								genprimary (I_LDNL, 1);			/* load address of "server" channel */
							}
							gensecondary (I_REV);				/* Areg = ct_nptr, Breg = channel */
							genloadconstant (1);
							gensecondary (I_OR);				/* or with 1 to indicate release */
							tioop (I_OUTWORD, FALSE);

							setlab (iskiplab);
							/*}}}*/
						}
						gencomment0 ("}}}");
					}
					/*}}}*/
					/*{{{  code to release the semaphore*/
					gencomment0 ("begin release semaphore");
					loadmobile (ct_nptr);
					loadconstant (lock);
					gensecondary (I_MT_UNLOCK);
					gencomment0 ("end release semaphore");
					/*}}}*/
				} else if (isdynmobilearray (ct_nptr)) {
					treenode *ct_subtype = follow_user_type (ct_type);
					int lastchan_offset, is_client, lock;
					/* int soffset, knsf_offset; */
					int cl_head, cl_tail;
					int rl_head, rl_tail;

					if (TagOf (ct_subtype) == S_MOBILE) {
						ct_subtype = MTypeOf (ct_subtype);
					}
					if (TagOf (ct_subtype) != S_ARRAY) {
						geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tprocess: ct_subtype not an array here..");
						return;
					}
					ct_subtype = ARTypeOf (ct_subtype);

					if (!isdynmobilechantypetype (ct_subtype)) {
						geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "tprocess: ct_subtype not a channel-type here..");
						return;
					}
#if 0
fprintf (stderr, "gen1: tprocess(): S_CLAIM: think I'm claiming a dynmobilearray, ct_subtype=");
printtreenl (stderr, 4, ct_subtype);
#endif
					lastchan_offset = (bytesin (ct_subtype) / bytesperword);		/* client at this + 1, server at this + 4, maybe more if chantype_desc */
					is_client = (NTypeAttrOf (ct_subtype) & TypeAttr_marked_out);
					lock = (is_client ? MT_CB_CLIENT : MT_CB_SERVER);
					cl_head = newlab ();
					cl_tail = newlab ();
					rl_head = newlab ();
					rl_tail = newlab ();

					/*{{{  put 0 into local 0 -- must CLAIM forwards*/
					genloadconstant (0);
					genprimary (I_STL, 0);

					/*}}}*/
					/*{{{  loop doing CLAIMs*/
					gencomment0 ("begin claim semaphores");
					setlab (cl_head);
					loadmobile (ct_nptr);		/* load array base pointer */
					genprimary (I_LDL, 0);
					genshiftimmediate (I_SHL, WSH);
					gensecondary (I_SUM);
					genprimary (I_LDNL, 0);
					genchecknotnull ();
					loadconstant (lock);
					gensecondary (I_MT_LOCK);
					gencomment0 ("end claim semaphores");

					genprimary (I_LDL, 0);
					genprimary (I_ADC, 1);
					genprimary (I_STL, 0);

					loaddynmobilesize (ct_nptr, 1);
					genprimary (I_LDL, 0);
					gensecondary (I_DIFF);		/* zero if this was the last iteration */
					genbranch (I_CJ, cl_tail);
					throw_the_result_away ();

					genbranch (I_J, cl_head);

					setlab (cl_tail);
					throw_the_result_away ();

					/*}}}*/
					/* FIXME: kroc_chantype_desc claim specials */

					tprocess (CBodyOf (tptr));

					/* FIXME: kroc_chantype_desc release specials */
					/*{{{  put array-size - 1 into local 0*/
					loaddynmobilesize (ct_nptr, 1);
					genprimary (I_ADC, -1);
					genprimary (I_STL, 0);

					/*}}}*/
					/*{{{  loop doing RELEASEs*/
					gencomment0 ("begin release semaphores");
					setlab (rl_head);
					loadmobile (ct_nptr);		/* load array base pointer */
					genprimary (I_LDL, 0);
					genshiftimmediate (I_SHL, WSH);
					gensecondary (I_SUM);
					genprimary (I_LDNL, 0);
					genchecknotnull ();
					loadconstant (lock);
					gensecondary (I_MT_UNLOCK);
					gencomment0 ("end release semaphores");

					genprimary (I_LDL, 0);
					genbranch (I_CJ, rl_tail);
					genprimary (I_LDL, 0);
					genprimary (I_ADC, -1);
					genprimary (I_STL, 0);
					genbranch (I_J, rl_head);
					setlab (rl_tail);

					/*}}}*/
				} else {
#if 0
fprintf (stderr, "gen1: tprocess(): S_CLAIM: think I'm claiming something.., ct_nptr=");
printtreenl (stderr, 4, ct_nptr);
fprintf (stderr, "gen1: tprocess(): S_CLAIM: think I'm claiming something.., type=");
printtreenl (stderr, 4, ct_type);
#endif
					geninternal_is (GEN_ERROR_IN_ROUTINE, 3, "tprocess: don\'t know how to claim this..");
					return;
				}
				tptr = NULL;
			}
			break;
			/*}}}*/
			/*{{{  S_RESIGN               break*/
		case S_RESIGN:
			{
				treenode *bar = CTempOf (tptr);

				tresign (bar, 1);
				tprocess (CBodyOf (tptr));
				tenroll (bar, 1);

				tptr = NULL;
			}
			break;
			/*}}}*/
			/*{{{  S_SEQ                  return */
		case S_SEQ:
			{
				treenode *sptr;
				for (sptr = CBodyOf (tptr); !EndOfList (sptr); sptr = NextItem (sptr))
					tprocess (ThisItem (sptr));
				tdespecs (specsptr);
				return;
			}
			/*}}} */
			/*{{{  S_REPLSEQ              return */
		case S_REPLSEQ:
			tpreexp (ReplCStartExpOf (tptr));
			tpreexp (ReplCLengthExpOf (tptr));
			if (ReplCStepExpOf (tptr)) {
				tpreexp (ReplCStepExpOf (tptr));
			}
			trepl (tptr, trepl_process, NULL, 0, TRUE);
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_PAR                  return */
		case S_PAR:
			tnormalpar (tptr);
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_PRIPAR               return */
		case S_PRIPAR:
			/* tpripar (tptr); */
			genwarn (GEN_PRI_PAR_AS_PAR);
			tnormalpar (tptr);
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_REPLPAR              return */
		case S_REPLPAR:
			{
				/* Number of replicated processes */
				INT32 replcount;
				treenode *const replbody = ReplCBodyOf (tptr);
				const BOOL replisconst = isconst (ReplCLengthExpOf (tptr));
				INT32 after_par_setup_slots = (inside_suspends ? 10 : 0);

				/* won't get this far without DYNAMIC_PROCs and !replisconst anyway.. */
#if 0
fprintf (stderr, "tprocess: S_REPLPAR:");
printtreenl (stderr, 4, tptr);
#endif
				if (replisconst) {
					replcount = LoValOf (ReplCLengthExpOf (tptr));
				} else {
					/* check count and skip entire thing at run-time if count <= 0 */
					replcount = 1;
				}

				if (replcount != 0) {
					const BIT32 thisbranchwsp = SpMaxwspOf (replbody);
					const BOOL vectorspace = (SpVSUsageOf (replbody) != 0);
#ifdef MOBILES
					const BOOL mobilespace = (SpMSUsageOf (replbody) != 0);
					INT32 ms_temp_slot = -1;
					const int replparslots = MIN_REPLPAR_SPECIALS + vectorspace + mobilespace;
#else
					const int replparslots = MIN_REPLPAR_SPECIALS + vectorspace;
#endif
					const int joinlab = newlab ();
					const int proclab = newlab ();
					const int skiplab = newlab ();

					int acount, ws_aslot, vs_aslot;
#ifdef MOBILES
					int ms_tslot, ms_tslot2;
#endif
					treenode *bar, *declbars, *extbars;

					/*{{{  find barrier decls + extensions*/
					if (((TagOf (tptr) == S_REPLPAR) || (TagOf (tptr) == S_PRIREPLPAR)) && ReplCTempOf (tptr)) {
						bar = ReplCTempOf (tptr);
					} else {
						bar = NULL;
					}

					/* separate off any barrier */
					if (bar && (TagOf (bar) == S_BAREXTEND)) {
						declbars = LeftOpOf (bar);
						extbars = RightOpOf (bar);
					} else if (bar && (TagOf (bar) == S_EXTENDS)) {
						declbars = NULL;
						extbars = bar;
					} else if (bar) {
						declbars = bar;
						extbars = NULL;
					} else {
						declbars = extbars = NULL;
					}
					/*}}}*/
					/*{{{  set up repl PAR */
					if (replisconst) {
						ws_aslot = vs_aslot = 0;
						acount = 0;
#ifdef MOBILES
						ms_tslot = ms_tslot2 = 0;
#endif
						tsetpar (replcount + 1, joinlab, tptr);	/*     ... set PAR join & count   */
					} else {
						/* setup extra slots */
#ifdef MOBILES
						acount = REPLPAR_WS_TEMP + (vectorspace ? (mobilespace ? 3 : 2) : (mobilespace ? 2 : 1));
						ws_aslot = REPLPAR_WS_TEMP + (vectorspace ? (mobilespace ? 4 : 3) : (mobilespace ? 3 : 2));
						vs_aslot = vectorspace ? (REPLPAR_WS_TEMP + (mobilespace ? 5 : 4)) : 0;
						ms_tslot = mobilespace ? (REPLPAR_WS_TEMP + (vectorspace ? 6 : 4)) : 0;
						ms_tslot2 = mobilespace ? (REPLPAR_WS_TEMP + (vectorspace ? 7 : 5)) : 0;
#else	/* !MOBILES */
						acount = REPLPAR_WS_TEMP + (vectorspace ? 2 : 1);
						ws_aslot = REPLPAR_WS_TEMP + (vectorspace ? 3 : 2);
						vs_aslot = vectorspace ? (REPLPAR_WS_TEMP + 4) : 0;
#endif	/* !MOBILES */
						/* ... set PAR join & count from expression */
						tsetpar_exp (ReplCLengthExpOf (tptr), joinlab, skiplab, tptr);
#if 0
fprintf (stderr, "tprocess (dynamic replpar): acount @%d, ws_aslot @%d, vs_aslot @%d, ms_tslot @%d, ms_tslot2 @%d\n", acount, ws_aslot, vs_aslot, ms_tslot, ms_tslot2);
#endif
					}
					if (replisconst) {
						genprimary (I_LDLP, (int) (-(DS_MIN + replparslots + after_par_setup_slots)));
						genprimary (I_STL, REPLPAR_WS_TEMP);	/*     stl    wstemp              */
						gencomment0 ("PTR wstemp");
					} else if (disable_dynmem) {
						generr (GEN_NO_DYNMEM);
					} else {
						/* store 0 in count slot */
						gencomment0 ("put 0 in acount slot");
						genprimary (I_LDC, 0);
						genprimary (I_STL, acount);
						/* need to allocate a workspace-array type thing:
						 *	ld	<size expression>
						 *	shl	WSH
						 *	malloc
						 *	stl	wsarray-slot
						 */
						gencomment0 ("start allocation of ws array");
						texp (ReplCLengthExpOf (tptr), MANY_REGS);
						genshiftimmediate (I_SHL, WSH);
						gensecondary (I_MALLOC);
						genprimary (I_STL, ws_aslot);
						gencomment0 ("allocate first process ws");
						/* for dynamic process support, do:
						 *	ldc	<size required>
						 *	malloc
						 *	ldnlp   <offset to Wptr>
						 *	stl	<wstemp>
						 */
						genprimary (I_LDC, (int)(SpDatasizeOf (replbody) << WSH));
						gensecondary (I_MALLOC);
						genprimary (I_LDNLP, (int)(SpDatasizeOf (replbody) - replparslots));			/* XXX */
						genprimary (I_STL, REPLPAR_WS_TEMP);
						gencomment0 ("PTR wstemp");
						gencomment0 ("stuff into array");
						genprimary (I_LDL, REPLPAR_WS_TEMP);
						genprimary (I_LDL, ws_aslot);
						genprimary (I_STNL, 0);
					}
					if (vectorspace) {
						if (!replisconst && !disable_dynmem) {
							/* need to allocate vectorspace-array thing:
							 *	ld	<size expression>
							 *	shl	WSH
							 *	malloc
							 *	stl	vsarray-slot
							 */
							gencomment0 ("start allocation of vs array");
							texp (ReplCLengthExpOf (tptr), MANY_REGS);
							genshiftimmediate (I_SHL, WSH);
							gensecondary (I_MALLOC);
							genprimary (I_STL, vs_aslot);
						} else if (!replisconst && disable_dynmem) {
							generr (GEN_NO_DYNMEM);
						}
						loadnewvsp (SpNestedVSOf (replbody));	/*  ldptr  freevectorspace     */
						genprimary (I_STL, REPLPAR_VS_TEMP);	/*  stl    vstemp              */
						gencomment0 ("vstemp");
					}
#ifdef MOBILES
					if (enable_mobilespace && mobilespace) {
						if (vectorspace) {
							ms_temp_slot = REPLPAR_MS_TEMP;
						} else {
							ms_temp_slot = REPLPAR_VS_TEMP;
						}
						loadnewmsp (SpNestedMSOf (replbody));	/* ldptr  freemobilespace	*/
						genprimary (I_STL, ms_temp_slot);	/* stl    mstemp		*/
						gencomment0 ("mstemp");
						if (!replisconst && !disable_dynmem) {
							/* check array size, allocate (new) array if necessary, and
							 * allocate the mobile-space for (new) processes... */
							gencomment0 ("start allocation of mobilespace stuff");
#if 0
fprintf (stderr, "tprocess (ditto): ms_temp_slot @%d, ms_tslot @%d, ms_tslot2 @%d\n", ms_temp_slot, ms_tslot, ms_tslot2);
#endif
							gendynamicmobilespace (ms_temp_slot, ms_tslot, ms_tslot2, ReplCLengthExpOf (tptr));
							gencomment0 ("end allocation of mobilespace stuff");
						} else if (!replisconst && disable_dynmem) {
							generr (GEN_NO_DYNMEM);
						}
					}

#endif /* MOBILES */
					/*}}} */
					/*{{{  preprocess the replicator start expression */
					tpreexp (ReplCStartExpOf (tptr));
					/* tpreexp(ReplCLengthExpOf(tptr));  must be constant */
					/*}}} */
					/*{{{  generate startp's */
					gencomment0("going for a trepl()");
					trepl (tptr, treplpar, tptr, proclab, FALSE);
					/*}}} */
					if (!replisconst) {
						gencomment0 ("free excess ws here");
						genprimary (I_LDL, REPLPAR_WS_TEMP);
						genprimary (I_LDNLP, -(int)(SpDatasizeOf (replbody) - replparslots));			/* XXX */
						gensecondary (I_MRELEASE);
					}
					/*{{{  finish repl PAR */
					/* if we enrolled on the MPP barrier, better resign.. */
					if (inside_suspends) {
						/*{{{  resign from mobile-process barrier if not last process*/
						int sl = newlab ();
						treenode *resinst;

						/* need to resign from the MPP barrier, but check PAR count > 1 first */
						genprimary (I_LDLP, 0);
						genprimary (I_LDNL, 1);
						gencomment0 ("INT parcount");
						genprimary (I_ADC, -1);
						genbranch (I_CJ, sl);
						throw_the_result_away ();

						/* call the barrier resign PROC -- it is always the 2nd SEQ process below the body -- inserted by tran1 */
						resinst = ThisItem (NextItem (CBodyOf (SpBodyOf (replbody))));

#if 0
fprintf (stderr, "tprocess: want to resign main process..  resinst is:");
printtreenl (stderr, 4, resinst);
#endif
						/* loadmpp (); */
						/* gensecondary (I_TRAP); */
						/* throw_the_result_away (); */
						tinstance (resinst);
						throw_the_result_away ();

						setlab (sl);
						/*}}}*/
					}
					/* frmb note: don't need to resign from any BARRIER here:
					 *   we enroll n-2 processes, that accounts for this one
					 *   automatically;  don't matter if there is a leftover ENDP
					 *   after the SYNC (the joining replicator process), it's
					 *   benign -- not the case for SUSPEND though, that must get
					 *   all processes (because it might be duplicated/moved)
					 */

					genprimary (I_LDLP, 0);	/*     ldlp  0                   */
					gencomment0 ("joinlab");
					gensecondary (I_ENDP);	/*     endp                      */
					/*}}} */
					/*{{{  generate code of replicated process */
					{
						const INT32 savedconstptr = constptr;
						const BIT32 savedvspoffset = vspoffset;
						const INT32 savedfbpoffset = fbpoffset;
#ifdef MOBILES
						const BIT32 savedmspoffset = mspoffset;
#endif
						INT32 saved_asmvalues[ASMNAMES_COUNT];
						int saved_asmvalids[ASMNAMES_COUNT];
						int i;
						treenode *bodyp = SpBodyOf (replbody);
						treenode *ires = NULL;

						setlab (proclab);	/* proclab:                           */
						gencomment0 ("after set proclab");
						/*{{{  set environment for replicated process */
						for (i = 0; i < ASMNAMES_COUNT; i++) {
							saved_asmvalues[i] = asmvalues[i];
							saved_asmvalids[i] = asmvalids[i];
						}
						be_lexlevel++;
						setsloffset (be_lexlevel, (thisbranchwsp - replparslots + REPLPAR_STATICLINK) | REPL_FLAG);
						gencomment0 ("after setsloffset");
						setadjust (be_lexlevel, 0);
						gencomment0 ("after setadjust");
/*asmvalids[ASMNAME_WSSIZE] = ASMNAME_VALID; *//* already TRUE */
						asmvalues[ASMNAME_WSSIZE] = thisbranchwsp;
						asmvalids[ASMNAME_STATIC] = ASMNAME_VALID;	/* always a static link */
						asmvalues[ASMNAME_STATIC] = thisbranchwsp - replparslots + REPLPAR_STATICLINK;
						/* Set vector space offset and usage */
						vspoffset = thisbranchwsp - replparslots + REPLPAR_VSP;
						asmvalids[ASMNAME_VSPTR] = (vectorspace ? ASMNAME_VALID : ASMNAME_ERROR);
						asmvalues[ASMNAME_VSPTR] = vspoffset;
#if 0
/* frmb: in theory, this should be accessed via the staticlink.. */
						/* FIXME: ...? */
						asmvalids[ASMNAME_FB] = ASMNAME_VALID;
						asmvalues[ASMNAME_FB] = fbpoffset;
#endif
#ifdef MOBILES
						if (vectorspace) {
							mspoffset = thisbranchwsp - replparslots + REPLPAR_MSP;
						} else {
							mspoffset = thisbranchwsp - replparslots + REPLPAR_VSP;
						}
						/* FIXME: ...? */
						asmvalids[ASMNAME_MSPTR] = ASMNAME_VALID;
						asmvalues[ASMNAME_MSPTR] = mspoffset;
#endif

						/* Alter replicator lexical level and workspace position */
						{
							treenode *const replnptr = ReplCNameOf (tptr);
							SetNLexLevel (replnptr, be_lexlevel);	/* Move lex level into repl. body */
							SetNVOffset (replnptr, thisbranchwsp - replparslots + REPLPAR_REPLICATOR);
						}
						/*}}} */
						gencomment0 ("after repl lex set");
						/*{{{  generate body of replicated process */
						tstoreconsttable (SpCPOffsetOf (replbody));
						gencomment0 ("after tstoreconsttable");

						if (inside_suspends) {
							/* body process is a SEQ with the original process and instance of MPBARRESIGN() */
							ires = ThisItem (NextItem (CBodyOf (bodyp)));
							bodyp = ThisItem (CBodyOf (bodyp));
						}
						/* if there's a workspace-map, load it */
						if (SpWSMapOf (replbody)) {
							/* need to temporarily load into WS 0 */
							loadmpp ();
							genprimary (I_STL, 0);
							genloadwsmap (0, SpMapLabelOf (replbody));
						}
						tprocess (bodyp);			/* P */
						gencomment0 ("after tprocess");
						/*}}} */
						throw_the_result_away ();	/* in case there is dross around */
						if (inside_suspends) {
							int sl = newlab ();

							/* resign from the MPP barrier -- first check PAR count > 1 */
							loadreplreturnlink ();
							genprimary (I_LDNL, 1);
							gencomment0 ("INT parcount");
							genprimary (I_ADC, -1);
							genbranch (I_CJ, sl);

							tinstance (ires);
							throw_the_result_away ();

							setlab (sl);
						}
						if (barrier_rbpe && (declbars || extbars)) {
							/*{{{  resign from BARRIERs*/
							if (declbars) {
								/* resign from these absolutely */
								tresign (declbars, 1);
							}
							if (extbars) {
								int sl = newlab ();

								/* only resign if not the last process */
								loadreplreturnlink ();
								genprimary (I_LDNL, 1);
								gencomment0 ("INT parcount");
								genprimary (I_ADC, -1);
								genbranch (I_CJ, sl);
								throw_the_result_away ();

								tresign (OpOf (extbars), 1);
								throw_the_result_away ();

								setlab (sl);
							}
							/*}}}*/
						}

						if (SpWSMapOf (replbody)) {
							/* need to temporarily load into WS 0 */
							loadmpp ();
							genprimary (I_STL, 0);
							genunloadwsmap (0, SpMapLabelOf (replbody));
						}
						loadreplreturnlink ();	/*          ldl   staticlink          */
						gensecondary (I_ENDP);	/*          endp                      */
						/*{{{  restore environment */
						be_lexlevel--;
						constptr = savedconstptr;
						vspoffset = savedvspoffset;
						fbpoffset = savedfbpoffset;
#ifdef MOBILES
						mspoffset = savedmspoffset;
#endif
						/* Replicator variable goes out of scope so we don't bother to restore
						   its real lexical level and workspace position. */
						for (i = 0; i < ASMNAMES_COUNT; i++) {
							asmvalues[i] = saved_asmvalues[i];
							asmvalids[i] = saved_asmvalids[i];
						}
						/*}}} */
					}
					/*}}} */
					gencomment0 (".MAGIC JOINLAB");
					setlab (joinlab);
					/*{{{  end par -- cleanup*/
					if (!replisconst) {
						int jlab = newlab ();

						/* only get here once; need to free-up memory allocated during replication */
						gencomment0 ("free dynamic stuff");
						genprimary (I_LDL, acount);
						genprimary (I_LDC, 4);
						gensecondary (I_DIFF);
						genprimary (I_STL, acount);		/* acount -= 4 */

						genprimary (I_LDL, ws_aslot);
						genprimary (I_LDL, acount);
						gensecondary (I_SUM);
						genprimary (I_LDNL, 0);
						gensecondary (I_MRELEASE);		/* free workspace */

						if (vectorspace) {
							genprimary (I_LDL, vs_aslot);
							genprimary (I_LDL, acount);
							gensecondary (I_SUM);
							genprimary (I_LDNL, 0);
							gensecondary (I_MRELEASE);
						}

						genprimary (I_LDL, acount);
						genbranch (I_CJ, jlab);
						throw_the_result_away ();
						genbranch (I_J, joinlab);
						setlab (jlab);
						throw_the_result_away ();

						genprimary (I_LDL, ws_aslot);
						gensecondary (I_MRELEASE);
						if (vectorspace) {
							genprimary (I_LDL, vs_aslot);
							gensecondary (I_MRELEASE);
						}
						setlab (skiplab);
						throw_the_result_away ();

						tendpar_exp (ReplCLengthExpOf (tptr), tptr);
					} else {
						tendpar (replcount + 1, tptr);
					}
					/*}}}*/
				}
				new_occam_line (replbody, FALSE, TRUE, TRUE);
				/* if (debugoutput)
				   coder_genlocate(replbody, FALSE);  bug TS/1434 10/03/92 */
			}
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_WHILE                return */
		case S_WHILE:
			if (!isfalseguard (CondGuardOf (tptr))) {	/* WHILE FALSE is thrown away */
				/*{{{  generate code for the WHILE */
				const int startlab = newlab ();
				treenode *guard = CondGuardOf (tptr), *body = CondBodyOf (tptr);

				setlab (startlab);
				genstartblock ();
				if (istrueguard (guard))
					/*{{{  generate WHILE TRUE */
				{
					tprocess (body);
					genjump (startlab);
				}
				/*}}} */
				else
					/*{{{  generate WHILE guard  body */
				{
					const int endlab = newlab ();
					tpreexp (guard);
					tguard (guard, TRUE, endlab);
					tprocess (body);
					genjump (startlab);
					setlab (endlab);
				}
				/*}}} */
			}
			/*}}} */
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_IF S_REPLIF          return */
		case S_IF:
		case S_REPLIF:
			{
				const int endlabel = newlab ();
				BOOL trueguard = FALSE;
				tif (tptr, &trueguard, endlabel, TRUE);
				if (NEED_ERRORS && (!trueguard)) {
					/*{{{  source output */
					/* removed source_output 14/3/91 */
					/*if (source_output)
					   so_stop(); */
					/*}}} */
					tstop (tptr);
				}
				setlab (endlabel);
				genstartblock ();
				tdespecs (specsptr);
				return;
			}
			/*}}} */
			/*{{{  S_ALT S_REPLALT S_PRIALT S_PRIREPLALT return */
		case S_ALT:
		case S_PRIALT:
		case S_REPLALT:
		case S_PRIREPLALT:
			talt (tptr);
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_OUTPUT               return */
		case S_OUTPUT:
			toutput (tptr);
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_INPUT S_DELAYED_INPUT S_CASE_INPUT S_TAGGED_INPUT S_X_INPUT S_X_TAGGED_INPUT return */
		case S_INPUT:
		case S_DELAYED_INPUT:
		case S_CASE_INPUT:
		case S_TAGGED_INPUT:
		case S_X_INPUT:
		case S_X_TAGGED_INPUT:
			tinput (tptr);
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_X_INPUT_OUTPUT  return */
		#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3) && defined(PD_ENCODE_CHANNEL)
		case S_X_INPUT_OUTPUT:
			txinputoutput (tptr);
			tdespecs (specsptr);
			return;
		#endif
			/*}}}*/
			/*{{{  S_ASS                  return */
		case S_ASS:
			tpreexp (RHSOf (tptr));
			tpreexp (LHSOf (tptr));
			tassign (LHSOf (tptr), RHSOf (tptr));
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_PINSTANCE            return */
		case S_PINSTANCE:
			tpreexp (IParamListOf (tptr));
			if (TagOf (INameOf (tptr)) == N_PREDEFPROC) {
				#if 0
				if (cgraph_profiling && NModeOf (INameOf (tptr)) != PD_UPDATE_PROFCOUNT) {
					ProfTabEntry *entry = get_proftab_entry (profile_table, tptr);
					if (entry != NULL) {
						tprofcountupdate (proftab_entry_number_ (entry));
						proftab_calling_nptr_ (entry) = current_routine_nptr;
					}
				}
				#endif
				tpredef (tptr, NULL);
			} else {
				tinstance (tptr);
			}
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_CASE                 return */
		case S_CASE:
			tcase (tptr);
			tdespecs (specsptr);
			return;
			/*}}} */
			/*{{{  S_VALOF                         break */
		case S_VALOF:
			tptr = VLBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  S_PLACE S_WSPLACE S_VSPLACE     break */
		case S_PLACE:
		case S_WSPLACE:
		case S_VSPLACE:
			tptr = DBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  S_GUY, S_ASM                  return */
		case S_GUY:
			walklist (tguy, CBodyOf (tptr), NULL);
			tdespecs (specsptr);
			return;
		case S_ASM:
			walklist (tasm, CBodyOf (tptr), NULL);
			tdespecs (specsptr);
			return;
			/*}}} */
		default:
			badtag (genlocn, TagOf (tptr), "tprocess");
		}
		/*}}} */
		tdespecs (specsptr);
	}
}

/*}}}*/
/*{{{  PRIVATE void write_descriptors_etc*/
PRIVATE void write_descriptors_etc (treenode * const nptr)
{
#if 0
	/*{{{  inmos version */
	/*{{{  diagnostics / descriptor to screen / file */
	if (assembly_output	/*&& !source_output *//* removed source_output 14/3/91 */
	    && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		gencomment0 ((TagOf (nptr) == N_PROCDEF) ? "PROC" : "FUNCTION");
		fprintf (outfile, " %s, lexlevel: %d, WS: %ld, VS: %ld", WNameOf (translate_from_internal (NNameOf (nptr))),	/* MDP */
			 NLexLevelOf (nptr), NPDatasizeOf (nptr), NPVSUsageOf (nptr));
		if ((NLexLevelOf (nptr) == 0) && ims_asm_output)
			write_asm_desc (outfile, nptr);
	}
	/*}}} */
	/*{{{  declare the entry point if neccessary */
	/* N.B. Can't declare the entry point until the section label has been set */
	/*}}} */
#if 0
	if ((NLexLevelOf (nptr) == 0)
	    /*&& ((compilemode == COMP_SC) || (compilemode == COMP_LIB) ||
	       (compilemode == COMP_PROGRAM)) */
		)
#endif
		add_entry_point (nptr);

	/* WP/010 - Add to mapfile */
	coder_add_mapfile_entry (nptr);
	/*}}} */
#else
	/*{{{  mdp version (must call coder_add_entry afterwards) */
	/*{{{  diagnostics / descriptor to screen / file */
	if ((assembly_output || disassemble)
	    /*&& !source_output */
	    /* removed source_output 14/3/91 */
	    &&((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		char procdesc[256];
		sprintf (procdesc, "%s %s, lexlevel: %d, WS: %ld, VS: %ld",
			 (TagOf (nptr) == N_MPROCDECL) ? "MOBILE PROC" :								/* frmb */
			 ((TagOf (nptr) == N_PROCDEF) ? "PROC" : "FUNCTION"), WNameOf (translate_from_internal (NNameOf (nptr))),	/* MDP */
			 NLexLevelOf (nptr), (long) NPDatasizeOf (nptr), (long) NPVSUsageOf (nptr));
		gencomment0 (procdesc);
	}
	/*}}} */
	/*}}} */
#endif
}

/*}}}*/

/*{{{  PRIVATE void troutine(tptr)*/
/* Generate code for the procedure/function represented by tptr */
PRIVATE void troutine (treenode *const tptr)
{
	const int t = TagOf (tptr);
	treenode *nptr = DNameOf (tptr);
	BIT32 wssize = NPMaxwspOf (nptr);
	treenode *body = DValOf (tptr);
	treenode *endbody = (treenode *) ((PTRINT) body + 1);	/* for unique hashing MDP */
	BOOL scalarconstants = FALSE;
	BIT32 vsoffset;
	BIT32 fboffset = 0;
#ifdef MOBILES
	BIT32 msoffset = 0;
	BIT32 mpoffset = 0;
#endif
	BIT32 sloffset = -1;
	const int oldlexlevel = be_lexlevel;
	int nregresults = 0;

	DEBUG_MSG (("troutine for PROC/FUNCTION %s\n", WNameOf (NNameOf (nptr))));

	/*{{{  ignore if we needn't compile this one */
	if (separatelycompiled (nptr) || isinline (nptr) || (!NUsedOf (nptr) && (NLexLevelOf (nptr) != 0))) {
#if 0				/* #SC is no longer supported */
		if ((TagOf (nptr) == N_SCPROCDEF || TagOf (nptr) == N_SCFUNCDEF) && debugoutput)
			/* generate an addressfix record for a procedure or function which is
			   defined in an SC */
			debug_genaddrfix (nptr, NSCEntryOffsetOf (nptr));
#endif
		return;
	}
	/*}}} */

	/*{{{  look for renamed functions */
	if (NPLabelOf (nptr) == 0) {
		/* bug TS/1842 24/08/92 */
		/* This means that it is a 'renamed' procedure */
		treenode *const target_nptr = INameOf (body);
		const BOOL msg = assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0);

		SetNPLabel (nptr, NPLabelOf (target_nptr));

		if (msg || ims_asm_output)
			fputc ('\n', outfile);
		write_descriptors_etc (nptr);
		if (NLexLevelOf (nptr) == 0) {
			coder_add_entry (nptr);
			coder_jumptoentrypoint (target_nptr);	/* MDP */
		} else if (msg) {
			gencomments ("==== %s", WNameOf (NNameOf (target_nptr)));
			coder_add_entry (nptr);
		}
		return;
	}
	/*}}} */

	/*{{{  handle lexical level and workspace position */
	{
		be_lexlevel = NLexLevelOf (nptr) + 1;	/* Lexical level of body of PROC */
		/*{{{  set static link offset */
		{
			treenode *fparams = NParamListOf (nptr);
			BIT32 fpoffset = wssize + INS_EXTRA;
			vsoffset = -1;
			while (!EndOfList (fparams)) {
				switch (TagOf (ThisItem (fparams)))
					/*{{{  tag */
				{
				case N_PARAM:
				case N_VALPARAM:
				case N_RESULTPARAM:
					if (basetype (gettype (ThisItem (fparams))) != S_TIMER)
						fpoffset++;
					break;
				case S_HIDDEN_PARAM:
				case S_FNFORMALRESULT:
				case S_HIDDEN_TYPE:
				case S_PARAM_WS:
					fpoffset++;
					break;
				case S_PARAM_STATICLINK:
					sloffset = fpoffset;
					fpoffset++;
					break;
#ifdef MOBILES
				case S_PARAM_MSP:
					msoffset = fpoffset;
					fpoffset++;
					break;
				case S_PARAM_MPP:
					mpoffset = fpoffset;
					fpoffset++;
					break;
#endif
				case S_PARAM_VSP:
					vsoffset = fpoffset;
					fpoffset++;
					break;
				case S_PARAM_FB:
					fboffset = fpoffset;
					fpoffset++;
					break;
				default:
					badtag (genlocn, TagOf (ThisItem (fparams)), "troutine");
				}
				/*}}} */
				fparams = NextItem (fparams);
			}
			setsloffset (be_lexlevel, sloffset);
		}
		/*}}} */
		setadjust (be_lexlevel, 0);
	}
	/*}}} */

	/*{{{  output nested routines (including const tables in val abbrevs) */
	tnestedroutines (body);
	/*}}} */
	/*{{{  output constants (modified MDP 28/11/95) */
	/* Scalar constants (long integers, reals, etc.) are output together in a
	   table first of all, so after code reversal they come out last,
	   string literals and constant arrays are output as separate blocks
	   before the routine body, so after code reversal they come out
	   after the routine body. */
	/* bug 1158 - we rely on the strings being done after the scalars,
	   because now we collapse the chain of strings on pass 2,
	   because the same field is used to keep track of ALL strings
	   which have been generated. CON 14/2/91
	 */
	{
		int pass;
		int posn = 0;	/* increments for each constant output in passes 0, 1 */
		BOOL displayed_msg = FALSE;
		for (pass = 0; pass < 3; pass++)
			/*{{{  generate either scalar constant table, or other tables */
		{
			/* Pass 0 is quadword scalar constants;
			   Pass 1 is smaller scalar constants;
			   Pass 2 is strings and constructors */
			treenode *cchain = NPConstTablesOf (nptr);
			treenode *cptr = cchain;
			while (cptr != NULL) {
				if (TagOf (cptr) == S_CONSTEXP)
					/*{{{  generate a scalar constant */
				{
					if (pass < 2) {
						BOOL quadalign = needs_quadalign && (wordsin (gettype (cptr)) == 2);
						if (pass != (int) quadalign) {	/* quads in pass 0, others in pass 1 */
							treenode *matchptr = constantmatch (cptr, cchain);
							if (matchptr != NULL)
								SetCEOffset (cptr, CEOffsetOf (matchptr));
							else {
								if (!scalarconstants) {
									genstartconstblock ("LOCAL SCALARS");
									/* bug TS/1686 02/07/92 */
									coder_mark_alignment ();	/* the next byte will be aligned as target requires */

									scalarconstlab = newlab ();
									setsectionlab (scalarconstlab);
									if (disassemble && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
										gencomment0 ("small constant table for routine ");
										fputs (WNameOf (NNameOf (nptr)), outfile);
									}
									scalarconstants = TRUE;
								}
								SetCEOffset (cptr, posn);
								posn += genconstant (cptr);
							}
						}
					}
					cptr = CENextOf (cptr);
				}
				/*}}} */
				else {	/* S_STRING or S_CONSTCONSTRUCTOR */

					treenode *const next_cptr = CTNextOf (cptr);
					if (pass == 2)
						/*{{{  check for duplicate constant array */
					{
						if (WLengthOf (CTValOf (cptr)) != 0) {	/* optimise NULL strings */
							/* bug 1158 14/2/91 - we now 'merge' all copies of similar
							   constant arrays, even across procedures.
							 */
							treenode *typtr = ConstTableTypeOf (cptr);
							/* treenode *eptr = CTExpOf (cptr); */
							if (diagnostics && !displayed_msg) {
								fprintf (outfile, "Array constants for routine %s", WNameOf (NNameOf (nptr)));
								displayed_msg = TRUE;
							}
							if (ims_asm_output)
								fputc ('\n', outfile);
							if (TagOf (cptr) == S_STRING) {
								tconsttable (cptr, NULL, "[]BYTE", "STRING");
							} else {
								tconsttable (cptr, typtr, "CONSTCONSTR", "ARRAY");
							}
						}
					}
					/*}}} */
					cptr = next_cptr;
				}
			}
			if ((pass == 1) && scalarconstants) {
				if (!ims_asm_output)	/* bug TS/1686 02/07/92 */
					alignendofsection (bytesperword);
				compress_code ();
				genendconstblock ();
			}
		}
		/*}}} */
	}
	/*}}} */
	/*{{{  debugging */
	if (debugoutput || sampling_profiling || cgraph_profiling)
		coder_genaddrfix (nptr, -1);	/* `-1' means 'here' */
	/*}}} */

	vspoffset = vsoffset;
	fbpoffset = fboffset;
	fbp_lexlevel = be_lexlevel;
#ifdef MOBILES
	mspoffset = msoffset;
	mppoffset = mpoffset;
	mpp_lexlevel = be_lexlevel;
#endif
	/*{{{  procedure entry */
	/*{{{  set asmvalues */
	{
		int i;
		for (i = 0; i < ASMNAMES_COUNT; i++)
			asmvalids[i] = ASMNAME_ERROR;
		asmvalues[ASMNAME_WSSIZE] = wssize;
		asmvalids[ASMNAME_WSSIZE] = ASMNAME_VALID;
		if (sloffset != -1) {
			asmvalues[ASMNAME_STATIC] = sloffset;
			asmvalids[ASMNAME_STATIC] = ASMNAME_VALID;
		}
		if (vspoffset != -1) {
			asmvalues[ASMNAME_VSPTR] = vspoffset;
			asmvalids[ASMNAME_VSPTR] = ASMNAME_VALID;
		}
		if (fbpoffset != -1) {
			asmvalues[ASMNAME_FB] = fbpoffset;
			asmvalids[ASMNAME_FB] = ASMNAME_VALID;
		}
#ifdef MOBILES
		if (mspoffset != -1) {
			asmvalues[ASMNAME_MSPTR] = mspoffset;
			asmvalids[ASMNAME_MSPTR] = ASMNAME_VALID;
		}
		if (mppoffset != -1) {
			asmvalues[ASMNAME_MPPTR] = mppoffset;
			asmvalids[ASMNAME_MPPTR] = ASMNAME_VALID;
		}
#endif
	}
	/*}}} */
	/*{{{  Set the section label */
	{
		const int entrylabel = newlab ();
		
		if (((TagOf (nptr) == N_PROCDEF) || (TagOf (nptr) == N_MPROCDECL)) && NPWSMapOf (nptr)) {
			/* generate workspace-map */
			const int maplab = newlab ();
			BYTE *mapdata;
			int mapbytes;

			SetNPMapLabel (nptr, maplab);
			mapdata = bytesof_wsallocmap (nptr, &mapbytes);
			setlab (maplab);
#if 0
fprintf (stderr, "troutine: adding %d bytes of workspace-map data\n", mapbytes);
#endif
			add_const_block (mapbytes, mapdata, NULL, "mapdata", S_BYTE);
			freebytes_wsallocmap (mapdata);
		}
		SetNPLabel (nptr, entrylabel);
		setsectionlab (entrylabel);
	}
	/*}}} */
	/*{{{  Set the global variable current_routine_nptr */
	current_routine_nptr = nptr;
	/*}}} */
	/*{{{  write descriptor etc */
	write_descriptors_etc (nptr);
	coder_add_entry (nptr);
	/*}}} */
	/*{{{  mark the start of preamble for the debugger */
	/* Can't use NTypeOf - see bug TS/2047 18/01/93 */
	new_occam_line ((treenode *) NTypeAddr (nptr), FALSE, TRUE, TRUE);
	/* if (debugoutput)
   coder_genlocate((treenode *)NTypeAddr(nptr), FALSE); *//* mark the ajw */
	/*}}} */
	/*{{{  note procedure entry */
	noteprocentry ();	/* MDP allow coder to save return link if nec */
	/*}}} */
	/*{{{  ajw */
	if (((code_style_flags & CODE_STYLE_NO_PREAMBLE) == 0) || (be_lexlevel != 1)) {
		genprimary (I_AJW, -wssize);	/* ajw 0 will be ignored by coder */
	}
	/*}}} */
	/*{{{  constant table pointer */
	/* if nested repl has scalar constants, but none are used by the outer proc,
	   then scalarconstants will be TRUE, but no slot allocated */
	if (scalarconstants && (NPCPOffsetOf (nptr) != CONSTANTPOINTER_NOT_USED)) {
		/*{{{  mark the start of preamble for the debugger */
		new_occam_line (NPConstTablesOf (nptr), FALSE, wssize != 0, TRUE);
		/* if (debugoutput && wssize != 0)
		   coder_genlocate(NPConstTablesOf(nptr), FALSE); */
		/*}}} */
		tstoreconsttable (NPCPOffsetOf (nptr));
	} else {
		constptr = CONSTANTPOINTER_NOT_USED;
	}

/*constptr = CONSTANTPOINTER_NOT_USED; *//* testing bug TS/1745 11/08/92 */

	/*}}} */
	/*{{{  start block (in case of access via a jump) */
	if (NLexLevelOf (nptr) == 0) {
		genstartblock ();	/* we could have got here via a jump */
	}
	/*}}} */
	/*}}} */
#ifdef MOBILES
	/*
	 *	generate the markers which the translator will use to initialise our mobile-space
	 *	mobile-space is at offset `msp_offset'
	 */
	if (enable_mobilespace && NPMSUsageOf (nptr)) {
		genmobileinit (mspoffset, nptr);
	}
	/* if a MOBILE PROC, generate code to save Iptr */
	if (t == S_MPROCDECL) {
#if 0
fprintf (stderr, "gen1: troutine (MPROCDECL): mppoffset = %d\n", mppoffset);
#endif
		genprocentry (t, wssize);
	}
#endif /* MOBILES */
	/*{{{  check for suspending PROCs */
	if (NPSuspendsOf (nptr)) {
		/* indicates that PAR should adjust the MPP barrier */
		inside_suspends = nptr;
	} else {
		inside_suspends = NULL;
	}
	/*}}}*/
	/*{{{  translate body */
	if ((t == S_PROCDEF) || (t == S_MPROCDECL)) {
		/* if there is a workspace-map, load it */
		if (NPWSMapOf (nptr)) {
			if (NPSuspendsOf (nptr)) {
				genloadwsmap (mppoffset, NPMapLabelOf (nptr));
			} else {
				genloadwsmap (NO_SLOT, NPMapLabelOf (nptr));
			}
		}
		tprocess (body);
	} else {
		/*{{{  function body */
		treenode *specsptr = body;

		body = tspecs (body);
		tprocess (VLBodyOf (body));
		{
			treenode *resultlist = VLResultListOf (body);
			/*{{{  debug output */
			new_occam_line (resultlist, TRUE, TRUE, TRUE);
			/* if (debugoutput)
			   coder_genlocate(resultlist, TRUE); */
			/*}}} */
			tpreexp (resultlist);
			if (fn_return_style (nptr) == return_style_fpu) {
				/*{{{  special case a real-valued single result on an fp processor */
				tfpexp (ThisItem (resultlist), MANY_REGS, MANY_REGS);
				/*}}} */
			} else {
				/*{{{  load the results */
				struct
				{
					int opdmode;
					treenode *opd;
				}
				regresults[MAXREGS];
				treenode *destlist = firstresultof (FnParamsOf (NTypeOf (nptr)));
				for (; !EndOfList (resultlist); resultlist = NextItem (resultlist)) {
					treenode *const thisresult = ThisItem (resultlist);
					const int type = ntypeof (thisresult);
					if ((nregresults < MAXREGS) && (result_return_style (gettype (thisresult)) == return_style_alu))
						/*{{{  load result into a register later */
					{
						regresults[nregresults].opd = thisresult;
						regresults[nregresults].opdmode = P_EXP;
						nregresults++;
					}
					/*}}} */
					else if ((TagOf (thisresult) == S_FNFORMALRESULT) && (NLexLevelOf (thisresult) == be_lexlevel))
						/*{{{  the result has been removed */
					{
						/* The result variable has been removed; instead
						   references to it are done directly onto the
						   formal result parameter
						 */
						destlist = nextresultof (destlist);
					}
					/*}}} */
					else
						/*{{{  assign result through a pointer parameter */
					{
						tsimpleassign (type, P_EXP, ThisItem (destlist), P_EXP, thisresult, MANY_REGS);
						destlist = nextresultof (destlist);
					}
					/*}}} */
				}
				/*{{{  load the register results */
				/*{{{  preevaluate non-addressable real-valued results on an fp processor */
				if (has_fpu_core) {
					int i;
					for (i = 0; i < nregresults; i++) {
						treenode *exp = regresults[i].opd;
						int mode = regresults[i].opdmode;
						if (preeval (mode, exp) && needtemptoload (mode, NDeclOf (exp)))
							regresults[i].opdmode = simplify (mode, exp);
					}
				}
				/*}}} */
				switch (nregresults) {
				case 0:
					break;
				case 1:
					texpopd (regresults[0].opdmode, regresults[0].opd, MANY_REGS);
					break;
				case 2:
					tload2regs (regresults[1].opdmode, regresults[1].opd, regresults[0].opdmode, regresults[0].opd, FALSE, TRUE);
					break;
				case 3:
					/*{{{  tload3regs */
					{
						BOOL preeval_e2, preeval_e3;
						/* We only do this call so that we have a loadseq parameter for
						   tload3regs, ideally we should make the binder save loadseq
						   in the tree. */
						int loadseq = giveorder (regresults[2].opdmode, regresults[2].opd,
									 regresults[1].opdmode, regresults[1].opd,
									 regresults[0].opdmode, regresults[0].opd,
									 &preeval_e2, &preeval_e3);
						tload3regs (regresults[2].opdmode, regresults[2].opd,
							    regresults[1].opdmode, regresults[1].opd,
							    regresults[0].opdmode, regresults[0].opd, loadseq, TRUE);
					}
					/*}}} */
					break;
				}
				/*}}} */
				/*}}} */
			}
			checkerror ();	/* bug TS/1968 23/11/92 */
		}
		tdespecs (specsptr);
		/*}}} */
	}
	new_occam_line (endbody, FALSE, TRUE, FALSE);	/* MDP */
	/*}}} */

	/*{{{  procedure exit */
	if ((t == S_PROCDEF) || (t == S_MPROCDECL)) {
		/* if there is a workspace-map, unload it */
		if (NPWSMapOf (nptr)) {
			if (NPSuspendsOf (nptr)) {
				genunloadwsmap (mppoffset, NPMapLabelOf (nptr));
			} else {
				genunloadwsmap (NO_SLOT, NPMapLabelOf (nptr));
			}
		}
	}
	/* Procedure exit - adjust workspace */
	/*{{{  source output */
	/* removed source_output 14/3/91 */
	/*if (source_output)
	   so_endofproc(); */
	/*}}} */
	if (((code_style_flags & CODE_STYLE_NO_PREAMBLE) == 0) || (be_lexlevel != 1)) {
#if 0
		genprimary (I_AJW, wssize);	/* ajw 0 will be ignored by coder */
		gensecondary (I_RET);
#endif
		genprocreturn (t, LocnOf (tptr), nregresults, wssize);	/* MDP */
	}
	/*}}} */

	compress_code ();
	coder_finish_entry (nptr);
	/*{{{  restore lexical level */
	be_lexlevel = oldlexlevel;
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void tnestedroutines(tptr)*/
/*****************************************************************************
 *
 *  tnestedroutines walks the tree 'tptr' generating any nested routines or
 *                  constant tables.
 *
 *****************************************************************************/
PRIVATE void tnestedroutines (treenode * tptr)
{
	DEBUG_MSG (("tnestedroutines\n"));
	while (tptr != NULL) {
		switch (TagOf (tptr)) {
			/*{{{  cases */
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
			{
				treenode *saved_alt_var = replaltvar;	/* bug 1027 22/10/90 */
				treenode *saved_inside_suspends = inside_suspends;

				replaltvar = NULL;	/* incase this PROC is before an ALT guard */
				troutine (tptr);

				replaltvar = saved_alt_var;
				inside_suspends = saved_inside_suspends;

				tptr = DBodyOf (tptr);
			}
			break;
			/*}}} */
			/*{{{  S_VALABBR, S_VALRETYPE - possible constant table */
			/* N.B. Constant tables in val abbreviations are treated as nested routines
			   in order to keep their offsets as short as possible.
			   Constant tables in expressions, eg. writes("hello") are linked together
			   and the head of the list is stored in the routine definition node, they
			   can then be generated just before the body of the routine. */
		case S_VALABBR:
		case S_VALRETYPE:
			{
				treenode *const t = consttableof (tptr);
				if (t != NULL) {
					treenode *const nptr = DNameOf (tptr);
					genlocn = LocnOf (tptr);
					DEBUG_MSG (("tnestedroutines: found a VALABBR with a constant table, usecount is %d\n", NVUseCountOf (nptr)));
					if (NVUseCountOf (nptr) != 0)
						/*{{{  insert into code */
					{
						if (WLengthOf (CTValOf (t)) != 0) {	/* optimise NULL strings */
							/* bug 1158 - we now merge together tables with the same
							   values */
#if 0				/* original code */
							/*{{{  debugging */
							if (debugoutput)
								coder_genaddrfix (nptr);
							/*}}} */
							tconsttable (t, (TagOf (tptr) == S_VALRETYPE) ? nptr : t, WNameOf (NNameOf (nptr)));
#else
							if (debugoutput && !minimal_debugoutput)	/* bug TS/1434 15/09/92 */
								coder_genaddrfix (nptr, -1);	/* `-1' means `here' */
							if (ims_asm_output)
								fputc ('\n', outfile);
							/* gencomment0 ("VALABBR"); */
							tconsttable (t, NTypeOf (nptr), "ABBR", WNameOf (NNameOf (nptr)));
#endif
						}
						/* WP/010 - Add to mapfile */
						coder_add_mapfile_entry (nptr);
					}
					/*}}} */
				} else
					tnestedroutines (DValOf (tptr));
				tptr = DBodyOf (tptr);
				break;
			}
			/*}}} */
			/*{{{  pragmas*/
		case S_PRAGMA:	/* bug 829 19/9/91 */
			switch ((pragma_name_tag_t) NModeOf (DNameOf (tptr))) {
				/*{{{  pragma_name_dyncall*/
			case pragma_name_dyncall:
#if 0
fprintf (stderr, "tnestedroutines(): PRAGMA DYNCALL:, DValOf =\n");
printtreenl (stderr, 4, DValOf (tptr));
#endif
				if (TagOf (DValOf (tptr)) == S_LIST) {
					treenode *v;
					
					for (v = DValOf (tptr); !EndOfList (v); v = NextItem (v)) {
						treenode *vv = ThisItem (v);

						switch (TagOf (vv)) {
						case N_PROCDEF:
						case N_LFUNCDEF:
						case N_SFUNCDEF:
							{
								treenode *plist = NParamListOf (vv);
								INT32 ws, vs, thash;

								getprocwsandvs (vv, &ws, &vs);
								/* remember to carve off any VS parameter before hashing */
								{
									treenode **savep_vsp = NULL;
									treenode *save_vsp = NULL;

									/* if the parameter list has a vectorspace pointer in it, remove for purposes of typehash generation */
									for (savep_vsp = &plist; savep_vsp && !EndOfList (*savep_vsp); savep_vsp = NextItemAddr (*savep_vsp)) {
										save_vsp = ThisItem (*savep_vsp);

										if (TagOf (save_vsp) == S_PARAM_VSP) {
											break;		/* for() */
										}
									}
									if (savep_vsp && EndOfList (*savep_vsp)) {
										savep_vsp = NULL;
									}
									
									if (savep_vsp) {
										save_vsp = *savep_vsp;
										*savep_vsp = NULL;
									}
									thash = typehash (plist);
									if (savep_vsp) {
										*savep_vsp = save_vsp;
									}
								}
#if 0
fprintf (stderr, "tnestedroutines(): DYNCALL for [%s], thash = 0x%8.8X, plist =", WNameOf (NNameOf (vv)), (unsigned int)thash);
printtreenl (stderr, 4, plist);
#endif
								gencommentv (".MAGIC DYNCALL %s %d %d %8.8X", WNameOf (NNameOf (vv)), ws, vs, thash);
							}
							break;
						default:
							geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "tnestedroutines: unsupported type for DYNCALL");
							break;
						}
					}
				} else {
					geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tnestedroutines: PRAGMA DYNCALL is not a list");
				}
				break;
				/*}}}*/
				/*{{{  pragma_name_export*/
			case pragma_name_export:
				if (TagOf (DValOf (tptr)) == S_LIST) {
					treenode *v;
					
					for (v = DValOf (tptr); !EndOfList (v); v = NextItem (v)) {
						treenode *vv = ThisItem (v);

						switch (TagOf (vv)) {
						case N_PROCDEF:
						case N_LFUNCDEF:
						case N_SFUNCDEF:
						case N_LIBPROCDEF:
						case N_LIBFUNCDEF:
							{
								wordnode *nameptr = translate_from_internal (NNameOf (vv));
								const char *desc_buffer;
								char *lcbuf;
								int lclen, i;
								treenode *plist = NParamListOf (vv);
								INT32 ws, vs, thash;
								int pcount = 0;
								int p_doesfork = 0;
								int extra = 0;

								desc_buffer = create_descriptor_string (be_get_fe_handle (), vv, nameptr, 1, 0, 1);
#if 0
fprintf (stderr, "gen1: pragma_name_export: desc_buffer = [%s]\n", desc_buffer);
#endif
								/* the descriptor may be split over several lines; count until we
								 * reach a closing parenthesis */
								for (lclen=0; (desc_buffer[lclen] != '\0') && (desc_buffer[lclen] != '('); lclen++);
								if (desc_buffer[lclen] == '(') {
									/* start of parameter list */
									pcount = 1;
									for (lclen++; (desc_buffer[lclen] != '\0') && pcount; lclen++) {
										switch (desc_buffer[lclen]) {
										case '(':
											pcount++;
											break;
										case ')':
											pcount--;
											break;
										}
									}

									if (desc_buffer[lclen-1] == ')') {
										/* found it! */
									} else {
										geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tnestedroutines: damaged descriptor string for EXPORT");
									}
								} else {
									geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tnestedroutines: damaged descriptor string for EXPORT");
									return;
								}

								switch (TagOf (vv)) {
								case N_PROCDEF:
								case N_LIBPROCDEF:
									if (NPForksOf (vv)) {
										/* needs a FORK barrier */
										p_doesfork = 1;
									}
									break;
								default:
									break;
								}
								
								if (p_doesfork) {
									extra = 6;
								}

								lcbuf = memalloc (lclen + 2 + extra);
								memcpy (lcbuf, desc_buffer, lclen);
								/* go through and turn newlines into spaces */
								for (i=0; i < lclen; i++) {
									if (lcbuf[i] == '\n') {
										lcbuf[i] = ' ';
									}
								}
								lcbuf[lclen] = '\0';

								if (p_doesfork) {
									memcpy (lcbuf + lclen, " FORK", 5);
									lclen += 5;
									lcbuf[lclen] = '\0';
								}
#if 0
fprintf (stderr, "tnestedroutines(): EXPORT for [%s], lcbuf=[%s]\n", WNameOf (NNameOf (vv)), lcbuf);
#endif

								getprocwsandvs (vv, &ws, &vs);

								if (p_doesfork && (vs <= 0)) {
									generr_s (GEN_FORKED_EXPORT_NO_VS, WNameOf (NNameOf (vv)));
									geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tnestedroutines: FORKing externals must have vectorspace");
								}

								gencommentv (".MAGIC EXPORT %s", lcbuf);
								memfree (lcbuf);

								/* remember to carve off any VS parameter before hashing */
								{
									treenode **savep_vsp = NULL;
									treenode *save_vsp = NULL;

									/* if the parameter list has a vectorspace pointer in it, remove for purposes of typehash generation */
									for (savep_vsp = &plist; savep_vsp && !EndOfList (*savep_vsp); savep_vsp = NextItemAddr (*savep_vsp)) {
										save_vsp = ThisItem (*savep_vsp);

										if (TagOf (save_vsp) == S_PARAM_VSP) {
											break;		/* for() */
										}
									}
									if (savep_vsp && EndOfList (*savep_vsp)) {
										savep_vsp = NULL;
									}
									
									if (savep_vsp) {
										save_vsp = *savep_vsp;
										*savep_vsp = NextItem (save_vsp);
									}
#if 0
fprintf (stderr, "tnestedroutines(): calling EXPORT for [%s], typehash tree is:", WNameOf (NNameOf (vv)));
printtreenl (stderr, 4, plist);
#endif
									thash = typehash (plist);
									if (savep_vsp) {
										*savep_vsp = save_vsp;
									}
								}
#if 0
fprintf (stderr, "tnestedroutines(): EXPORT for [%s], thash = 0x%8.8x, raw typehash tree is:", WNameOf (NNameOf (vv)), thash);
printtreenl (stderr, 4, plist);
#endif
								gencommentv (".MAGIC DYNCALL %s %d %d %8.8X", WNameOf (NNameOf (vv)), ws, vs, thash);
							}
							break;
						default:
							geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "tnestedroutines: unsupported type for EXPORT");
							break;
						}
					}
				} else {
					geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tnestedroutines: PRAGMA EXPORT is not a list");
				}
				break;
				/*}}}*/
			default:
				tnestedroutines (DValOf (tptr));
				break;
			}
			tptr = DBodyOf (tptr);
			break;
			/*}}}*/
			/*{{{  other specification */
		case S_ABBR:
		case S_RETYPE:
		case S_DECL:
		case S_TPROTDEF:
		case S_SPROTDEF:
#ifdef MOBILES
		case S_PROCTYPEDECL:
#endif
			tnestedroutines (DValOf (tptr));
			tptr = DBodyOf (tptr);
			break;
			/*}}} */
#ifdef MOBILES
			/*{{{  S_FORWDECL*/
		case S_FORWDECL:
			if (isdynmobilechantypetype (DNameOf (tptr)) && kroc_chantype_desc) {
				/* just set the label if it's not set */
				treenode *mtype = NTypeOf (DNameOf (tptr));

				if (MTDLabOf (mtype) < 0) {
					SetMTDLab (mtype, newlab ());
				}
			}
			tnestedroutines (DValOf (tptr));
			tptr = DBodyOf (tptr);
			break;
			/*}}}*/
#endif
			/*{{{  S_TYPEDECL*/
		case S_TYPEDECL:
			if (isdynmobilechantypetype (DNameOf (tptr)) && kroc_chantype_desc) {
				treenode *mtype = NTypeOf (DNameOf (tptr));
				int tdlab = MTDLabOf (mtype);

				if (tdlab < 0) {
					tdlab = newlab ();
					SetMTDLab (mtype, tdlab);
				}

#if 0
fprintf (stderr, "gen1: tnestedroutines: hit S_TYPEDECL for channel-type, tdlab=%d, NTypeOf(DNameOf(tptr))=", tdlab);
printtreenl (stderr, 4, NTypeOf(DNameOf (tptr)));
#endif
				genmobiletypedescription (mtype);
			} else if (isdeepmobiletype (DNameOf (tptr))) {
				treenode *mtype = NTypeOf (DNameOf (tptr));
				int tdlab = MTDLabOf (mtype);

				if (tdlab < 0) {
					tdlab = newlab ();
					SetMTDLab (mtype, tdlab);
				}

				genmobiletypedescription (mtype);
			}
			tnestedroutines (DValOf (tptr));
			tptr = DBodyOf (tptr);
			break;
			/*}}}*/
			/*}}} */
			/*{{{  SEQ IF */
		case S_SEQ:
		case S_IF:
		case S_CLAIM:
		case S_FORKING:
			tptr = CBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  REPLSEQ REPLIF */
		case S_REPLSEQ:
		case S_REPLIF:
			tnestedroutines (ReplCStartExpOf (tptr));
			tnestedroutines (ReplCLengthExpOf (tptr));
			if (ReplCStepExpOf (tptr)) {
				tnestedroutines (ReplCStepExpOf (tptr));
			}
			tptr = ReplCBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  ALTs */
		case S_ALT:
		case S_PRIALT:
			if (replaltvar == NULL) {	/* an outermost ALT - bug 1027 22/10/90 */
				replaltvar = CTempOf (tptr);
				replaltlevel = 0;
				tnestedroutines (CBodyOf (tptr));
				replaltvar = NULL;	/* no longer inside an ALT */
				return;
			}
			tptr = CBodyOf (tptr);
			break;
		case S_REPLALT:
		case S_PRIREPLALT:
			{	/* all this modified to update info of replicator name, incase
				   any nested procedures read it - bug 1027 22/10/90 */
				treenode *replname = ReplCNameOf (tptr);
				INT32 old_offset = NVOffsetOf (replname);
				INT32 replaltvaluesbase;
				int outermost_alt = (replaltvar == NULL);
				tnestedroutines (ReplCStartExpOf (tptr));
				tnestedroutines (ReplCLengthExpOf (tptr));
				if (ReplCStepExpOf (tptr)) {
					tnestedroutines (ReplCStepExpOf (tptr));
				}
				if (outermost_alt) {
					replaltvar = ReplCTempOf (tptr);
					replaltlevel = 0;
				}
				/* note that the following two calls to 'nameoffsetof' should both
				   return the same values ( == nameoffsetof(be_lexlevel) ),
				   but they are there for safety */
				replaltvaluesbase = NVOffsetOf (replaltvar)
					+ nameoffsetof (NLexLevelOf (replaltvar));
				SetNVOffset (replname, replaltvaluesbase + replaltlevel - nameoffsetof (NLexLevelOf (replname)));
				replaltlevel++;
				tnestedroutines (ReplCBodyOf (tptr));
				replaltlevel--;
				SetNVOffset (replname, old_offset);
				if (outermost_alt)
					replaltvar = NULL;
			}
			return;
			/*}}} */
			/*{{{  ALTERNATIVE */
		case S_ALTERNATIVE:
			{	/* modified 22/10/90 for bug 1027 */
				treenode *saved_var = replaltvar;
				int saved_level = replaltlevel;
				replaltvar = NULL;	/* no longer inside an ALT */
				tnestedroutines (AltGuardOf (tptr));
				tnestedroutines (AltInputOf (tptr));
				tnestedroutines (AltBodyOf (tptr));
				replaltvar = saved_var;
				replaltlevel = saved_level;
			}
			return;
			/*}}} */
			/*{{{  PAR PRIPAR */
		case S_PAR:
		case S_PRIPAR:
			{
				treenode *proclist = CBodyOf (tptr);
				/* 22/10/90 noticed that this didn't allow for the PRI PAR via ALT, so added it: */
				BIT32 parsize = ((TagOf (tptr) == S_PRIPAR) && (code_style_flags & CODE_STYLE_ALT_PRI_PAR)) ? DS_IO : 0;
				if (needs_quadalign) {
					parsize = (parsize + 1) & (~1);	/* increments below already rounded up */
				}
				while (!EndOfList (proclist)) {
					treenode *pprocess = ThisItem (proclist);
					BIT32 thisbranchwsp = SpMaxwspOf (pprocess);
					BIT32 thisbranchsize = SpDatasizeOf (pprocess);
					BIT32 thisbranchadjust = parsize + thisbranchwsp;

					adjustworkspace (thisbranchadjust);	/* changed bug 1028 23/10/90 */
#ifdef MOBILES
					if (SpWSMapOf (pprocess)) {
						/* generate workspace-map */
						const int maplab = newlab ();
						BYTE *mapdata;
						int mapbytes;

						SetSpMapLabel (pprocess, maplab);
						mapdata = bytesof_wsallocmap (pprocess, &mapbytes);
						setlab (maplab);
#if 0
fprintf (stderr, "tnestedroutines: adding %d bytes of workspace-map data (for SPACEUSAGE node)\n", mapbytes);
#endif
						add_const_block (mapbytes, mapdata, NULL, "mapdata", S_BYTE);
						freebytes_wsallocmap (mapdata);
					}
#endif
					tnestedroutines (SpBodyOf (pprocess));

					adjustworkspace (-thisbranchadjust);	/* changed bug 1028 23/10/90 */
					parsize += thisbranchsize;
					proclist = NextItem (proclist);
				}
			}
			return;
			/*}}} */
			/*{{{  REPLPAR */
		case S_REPLPAR:
			tnestedroutines (ReplCStartExpOf (tptr));
			tnestedroutines (ReplCLengthExpOf (tptr));
			if (ReplCStepExpOf (tptr)) {
				tnestedroutines (ReplCStepExpOf (tptr));
			}
			/*{{{  search repl body */
			{
				treenode *replbody = ReplCBodyOf (tptr);
				BIT32 thisbranchwsp = SpMaxwspOf (replbody);

#ifdef MOBILES
				int replparslots = MIN_REPLPAR_SPECIALS + (SpVSUsageOf (replbody) != 0) + (SpMSUsageOf (replbody) != 0);
#else
				int replparslots = MIN_REPLPAR_SPECIALS + (SpVSUsageOf (replbody) != 0);
#endif
				treenode *replnptr = ReplCNameOf (tptr);
				INT32 old_offset = NVOffsetOf (replnptr);

				/*{{{  handle lexical level and workspace position */
				be_lexlevel++;
				setsloffset (be_lexlevel, (thisbranchwsp - replparslots + REPLPAR_STATICLINK) | REPL_FLAG);
				setadjust (be_lexlevel, 0);

				SetNLexLevel (replnptr, be_lexlevel);	/* bug 1026 19/10/90 */
				SetNVOffset (replnptr, thisbranchwsp - replparslots + REPLPAR_REPLICATOR);
				/*}}} */
#ifdef MOBILES
				if (SpWSMapOf (replbody)) {
					/* generate workspace-map */
					const int maplab = newlab ();
					BYTE *mapdata;
					int mapbytes;

					SetSpMapLabel (replbody, maplab);
					mapdata = bytesof_wsallocmap (replbody, &mapbytes);
					setlab (maplab);
					add_const_block (mapbytes, mapdata, NULL, "mapdata", S_BYTE);
					freebytes_wsallocmap (mapdata);
				}
#endif

				tnestedroutines (SpBodyOf (replbody));

				/*{{{  restore lexical level */
				be_lexlevel--;

				SetNLexLevel (replnptr, be_lexlevel);	/* bug 1026 19/10/90 */
				SetNVOffset (replnptr, old_offset);
				/*}}} */
			}
			/*}}} */
			return;
			/*}}} */
			/*{{{  WHILE CHOICE */
		case S_WHILE:
		case S_CHOICE:
			tnestedroutines (CondGuardOf (tptr));
			tptr = CondBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  SELECTION */
		case S_SELECTION:
			tptr = CondBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  VARIANT X_VARIANT */
		case S_VARIANT:
			tnestedroutines (VRTaggedListOf (tptr));
			tptr = VRBodyOf (tptr);
			break;
		case S_X_VARIANT:
			tnestedroutines (VRTaggedListOf (tptr));
			tnestedroutines (VRDuringOf (tptr));
			tptr = VRAfterOf (tptr);
			break;
			/*}}} */
			/*{{{  LIST */
		case S_LIST:
			tnestedroutines (ThisItem (tptr));
			tptr = NextItem (tptr);
			break;
			/*}}} */
			/*{{{  configuration */
			/*{{{  PLACE WSPLACE VSPLACE */
		case S_PLACE:
		case S_WSPLACE:
		case S_VSPLACE:
			tptr = DBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  PROCESSOR */
		case S_PROCESSOR:
			tptr = ProcessorBodyOf (tptr);
			break;
			/*}}} */
			/*}}} */
			/*{{{  instance */
			/*{{{  PINSTANCE FINSTANCE */
		case S_PINSTANCE:
		case S_FINSTANCE:
			tnestedroutines (IParamListOf (tptr));
			return;
			/*}}} */
			/*}}} */
			/*{{{  action */
			/*{{{  ASS OUTPUT INPUT TAGGED_INPUT DELAYED_INPUT CASE CASEINPUT X_INPUT X_TAGGED_INPUT X_CASE_INPUT */
		case S_ASS:
		case S_OUTPUT:
		case S_INPUT:
		case S_TAGGED_INPUT:
		case S_DELAYED_INPUT:
		case S_CASE:
		case S_CASE_INPUT:
		case S_X_CASE_INPUT:
			tnestedroutines (LHSOf (tptr));
			tptr = RHSOf (tptr);
			break;
		case S_X_INPUT:
		case S_X_TAGGED_INPUT:
			tnestedroutines (LHSOf (tptr));
			tnestedroutines (RHSOf (tptr));
			tnestedroutines (ActionDuringOf (tptr));
			tptr = ActionAfterOf (tptr);
			break;
			/*}}} */
			/*}}} */
			/*{{{  monadic */
		case S_NEG:
		case S_BITNOT:
		case S_UMINUS:
		case S_NOT:
		case S_SIZE:
			tptr = OpOf (tptr);
			break;
			/*}}} */
			/*{{{  conversion */
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			tptr = OpOf (tptr);
			break;
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
			tnestedroutines (LeftOpOf (tptr));
			tptr = RightOpOf (tptr);
			break;
		case S_AFTER:
			assert (FALSE);	/* after is processd in trans */
			break;

			/*}}} */
			/*{{{  constructor */
#if 0				/* no constructors are passed to the backend any more */
		case S_CONSTRUCTOR:
			tptr = LitExpOf (tptr);
			break;
#endif
			/*}}} */
			/*{{{  valof */
		case S_VALOF:
			tnestedroutines (VLBodyOf (tptr));
			tptr = VLResultListOf (tptr);
			break;
			/*}}} */
			/*{{{  subscript */
		case S_ARRAYSUB:
		case S_ARRAYITEM:
		case S_RECORDSUB:
		case S_RECORDITEM:
			tnestedroutines (ASBaseOf (tptr));
			tptr = ASExpOf (tptr);
			break;
			/*}}} */
			/*{{{  segment */
		case S_SEGMENTITEM:
		case S_SEGMENT:
			tnestedroutines (SNameOf (tptr));
			tnestedroutines (SStartExpOf (tptr));
			tptr = SLengthExpOf (tptr);
			break;
			/*}}} */
			/*{{{  temporary */
		case T_TEMP:
		case T_PREEVALTEMP:
			tptr = NDeclOf (tptr);
			break;
			/*}}} */
		/*}}} */
		}
	}
}

/*}}}*/
/*{{{  PRIVATE int setmtdlabels (treenode *tptr, void *voidptr)*/
/*
 *	this is called walking the whole tree, looking for CHAN TYPE and other MOBILE type
 *	declarations, doing nothing else than generating the MTDLab values
 */
PRIVATE int setmtdlabels (treenode *tptr, void *voidptr)
{
	if (!tptr) {
		return STOP_WALK;
	}
	if (TagOf (tptr) == S_TYPEDECL) {
		if (isdynmobilechantypetype (DNameOf (tptr)) && kroc_chantype_desc) {
			treenode *mtype = NTypeOf (DNameOf (tptr));
			int tdlab = MTDLabOf (mtype);

			if (tdlab < 0) {
				tdlab = newlab ();
				SetMTDLab (mtype, tdlab);
			}
		} else if (isdeepmobiletype (DNameOf (tptr))) {
			treenode *mtype = NTypeOf (DNameOf (tptr));
			int tdlab = MTDLabOf (mtype);

			if (tdlab < 0) {
				tdlab = newlab ();
				SetMTDLab (mtype, tdlab);
			}
		}
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PUBLIC void tmain(tptr)*/
/* Generate code for main body */
PUBLIC void tmain (treenode * tptr)
{
	/*{{{  source output */
	/* removed source_output 14/3/91 */
	/*if (source_output)
	   init_source_code_output(); */
	/*}}} */
	be_lexlevel = 0;
	/*{{{  declare nested SC entry points */
	/*apply_to_sc_entries(declare_sc_entries); */
	/*}}} */
	/*{{{  jump to entrypoint */
	/* if (ims_asm_output) */
	{
		treenode *nptr = NULL;
		treenode *t;
		int num_procs_declared = 0;

		/* set nptr to the last PROC/FUNCTION declared */
		for (t = tptr; TagOf (t) != S_END; t = DBodyOf (t)) {
			switch (TagOf (t)) {
			case S_PROCDEF:
#ifdef MOBILES
			case S_MPROCDECL:
#endif
			case S_LFUNCDEF:
			case S_SFUNCDEF:
				nptr = DNameOf (t);
				num_procs_declared++;
				break;
			default:
				break;
			}
		}
		if (nptr != NULL) {	/* should suppress if not a main module */
			if (main_dynentry) {
				INT32 ws, vs;
				treenode *nplist = NParamListOf (nptr);
				treenode **savep_vsp = NULL;
				treenode *save_vsp = NULL;
				unsigned int thash;
				
				getprocwsandvs (nptr, &ws, &vs);

				/* if the parameter list has a vectorspace pointer in it, remove for purposes of typehash generation */
				for (savep_vsp = &nplist; savep_vsp && !EndOfList (*savep_vsp); savep_vsp = NextItemAddr (*savep_vsp)) {
					save_vsp = ThisItem (*savep_vsp);

					if (TagOf (save_vsp) == S_PARAM_VSP) {
						break;		/* for() */
					}
				}
				if (savep_vsp && EndOfList (*savep_vsp)) {
					savep_vsp = NULL;
				}
				
				if (savep_vsp) {
					save_vsp = *savep_vsp;
					*savep_vsp = NULL;
				}
				thash = typehash (nplist);
				if (savep_vsp) {
					*savep_vsp = save_vsp;
				}

#if 0
fprintf (stderr, "tmain(): generating dynamic entry-point for main routine, thash is 0x%8.8x, ws = %d, vs = %d\n", thash, ws, vs);
#endif

				gencommentv (".MAGIC MAINDYNCALL %s %d %d %8.8X", WNameOf (NNameOf (nptr)), ws, vs, thash);
			} else {
				coder_jumptoentrypoint (nptr);
			}
		}
	}
	/*}}} */
	if ((sampling_profiling || line_profiling || cgraph_profiling) && profile_table->count_table_size > 0) {
		proftabconstlab = genproftable (profile_table->count_table_size);
	}
	/*{{{  generate library call stubs */
	genlibstubs ();
	/*}}} */
	coder_mark_alignment ();	/* the next byte will be word aligned */

	if (kroc_chantype_desc) {
		/*{{{  generate mobile type descriptions for any anonymous channel-types*/
		treenode *anontypes = mobile_getanontypes ();
		treenode *walk;

		for (walk = anontypes; !EndOfList (walk); walk = NextItem (walk)) {
			treenode *thistype = ThisItem (walk);
			
			if (isdynmobilechantypetype (thistype)) {
				treenode *mtype = NTypeOf (thistype);
				int tdlab = MTDLabOf (mtype);

#if 0
fprintf (stderr, "tmain(): anon mobile chan-type = ");
printtreenl (stderr, 4, thistype);
#endif
				if (tdlab < 0) {
					tdlab = newlab ();
					SetMTDLab (mtype, tdlab);
				}
				genmobiletypedescription (mtype);
			}
		}
		/*}}}*/
		/*{{{  preset labels for other mobile type descriptors*/
		prewalktree (tptr, setmtdlabels, NULL);
		/*}}}*/
	}

	tnestedroutines (tptr);
	/*{{{  source output */
	/* if (source_output)
	   end_source_code_output();  */
	/*}}} */
	alignwholemodule ();	/* align to word length */
	write_object_file ();
}

/*}}}*/
/*{{{  PUBLIC void beinit*/
PUBLIC void beinit (void)
{				/* Can call this instead of bereinit */
	if (diagnostics) {
		fputs ("Initialising backend\n", outfile);
	}
	insidealtguard = FALSE;
	initlabels ();
	caseinit ();
	initalloc ();
	setadjust (0, 0);
	initcode ();
	initbind ();
	globaltablechain = NULL;
	if (tempname_p == NULL) {
		/*tempname_p = newwordnode(S_NAME, tempname, strlen(tempname), NULL); */
		tempname_p = lookupword (tempname, strlen (tempname));
	}
	inside_asm = FALSE;
}

/*}}}*/
