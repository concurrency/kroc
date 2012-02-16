/* $Id: useerror.h,v 1.1 1996/04/15 10:52:24 djb1 Exp $ */

/*
 *	usage checker error numbers
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

/*{{{  error codes */
/*#define USE_BAD_TAG 1*/
/*#define USE_GLOBAL_FREE_VARS 2*/
#define USE_VAR_VAR_ABBREVIATED		3
#define USE_VAR_VAL_ABBREVIATED		4
#define USE_BAD_CHAN_PARAM		5
#define USE_BAD_FREE_CHAN		6
#define USE_WRITTEN_IN_PAR		7
#define USE_READ_AND_WRITTEN_IN_PAR	8
#define USE_OUTPUT_IN_PAR		9
#define USE_INPUT_IN_PAR		10
/*#define USE_VAR_TABLE_SUBSCRIPTS 11*/
#define USE_TIMER_IN_PRI_PAR		11
#define USE_TOO_MANY_OVERLAP_CHECKS	12
#define USE_VAR_FREEVAR			13
#define USE_VAL_FREEVAR			14
/*#define USE_FUTURE_COLON2_INPUT     15*/
#define USE_COLON2_INPUT		16
#define USE_NO_ALIASCHECK		17
#define USE_NO_USAGECHECK		18

/* new stuff for undefined usage checking */
#define USE_VAR_UNDEFINED		19
#define USE_VAR_UNKNOWN			20
#define USE_CHAN_IO_SEQ			21
#define USE_INTERNAL_ERROR		22
#define USE_PARAM_UNDEFINED		23
#define USE_PARAM_UNKNOWN		24
#define USE_INCALLOF			25
/*}}}*/
/*{{{  for errors where channel direction is specified in the formal param */
#define USE_BAD_CHAN_PARAM_IN		26
#define USE_BAD_CHAN_PARAM_OUT		27
#define USE_BAD_CHAN_ARRAY_PARAM_IN	28
#define USE_BAD_CHAN_ARRAY_PARAM_OUT	29
#define USE_BAD_CHAN_ABBR_IN		30
#define USE_BAD_CHAN_ABBR_OUT		31
#define USE_BAD_CHAN_ARRAY_ABBR_IN	32
#define USE_BAD_CHAN_ARRAY_ABBR_OUT	33
#define USE_BAD_CHAN_CONFLICT		34
#define USE_BAD_CHAN_IN			35
#define USE_BAD_CHAN_OUT		36
/*}}}  */
/*{{{  more undefined usage checking */
#define USE_CHAN_IN_XIN			37
/*}}}  */
/*{{{  things pertaining to dynamic usage */
#define USE_WARN_NREPLPAR_NOUSAGE	38
/*}}}  */
/*{{{  usage errors relating to shared chan-types and CLAIM */
#define USE_NESTED_CLAIM		40	/* nested CLAIM */
#define USE_NOT_CLAIMED			41	/* using channel in an un-CLAIM'd chan-type */
#define USE_VAR_CLAIMED_WRITE		42	/* attempting to write to a CLAIM'd chan-type var */
#define USE_PARAM_CLAIMED_WRITE		43	/* attempting to write to a CLAIM'd chan-type param */
/*}}}  */
/*{{{  more undefined-usage errors*/
#define USE_NTH_PARAM_UNDEFINED		44
#define USE_NTH_PARAM_UNKNOWN		45
/*}}}*/
/*{{{  free variables and FORK errors */
#define USE_UNSHARED_FORK_FREE_VAR	46	/* any global variables must be very explicitly shared */
#define USE_UNSHARED_FORK_FREE_MVAR	47	/* mobile chan-types are slightly different */
#define USE_FORK_FREE_VAR_SCOPE		48	/* and declared outside any FORKING */
#define USE_FORK_FREE_VARS		49	/* when we allow FORKING to be missing */
#define USE_BAD_MOBILE_DEFINED		50	/* when the wrong sort of MOBILE is used with DEFINED mop. */

/*}}}*/
/*{{{  more undefined-usage checking*/
#define USE_CHAN_IN_XIN_UNKNOWN		51	/* used when the channel-name isn't simple */
#define USE_VAR_PARTIAL			52	/* variable partially defined */
#define USE_PARAM_PARTIAL		53	/* parameter partially defined */
#define USE_MPP_FREEVARS		54	/* free variables in MOBILE PROC definition */
#define USE_PROCTYPE_NSYNC_PARAMS	55	/* PROC TYPE has non-sync-type parameters */
#define USE_MPP_NSYNC_PARAMS		56	/* MOBILE PROC has non-sync-type parameters */
#define USE_FORK_SUSPEND		57	/* may not FORK a PROC that SUSPENDs */
/*}}}*/
/*{{{  more usage errors*/
#define USE_SUSPEND_INSIDE_CLAIM	58	/* attempt to SUSPEND inside a CLAIM block */
#define USE_SUSPEND_INSIDE_XINPUT	59	/* attempt to SUSPEND inside an extended-input block */
#define USE_FIXED_PARAM_MOVED		60	/* a user-specified FIXED mobile parameter is moved */
#define USE_FIXED_ABBR_MOVED		61	/* a user-specified FIXED mobile abbreviation is moved */
#define USE_FIXED_DECL_MOVED		62	/* a user-specified FIXED mobile declaration is moved */
#define USE_PARAM_MUST_CLAIM		63	/* must CLAIM a SHARED actual before passing to an un-shared FORMAL */
#define USE_ABBR_MUST_CLAIM		64	/* must CLAIM a SHARED RHS before abreviating to un-shared */
#define USE_PARAM_UNFIXED		65	/* passing a CLAIMed SHARED actual to an un-shared formal that moves it */
#define USE_ABBR_UNFIXED		66	/* abbreviating a CLAIMed SHARED RHS to an un-shared LHS that moves it */
#define USE_PARAM_MUST_UNCLAIM		67	/* cannot pass CLAIMed SHARED actual to SHARED formal */
#define USE_ABBR_MUST_UNCLAIM		68	/* cannot abbreviate CLAIMed SHARED RHS to SHARED LHS */
#define USE_PARAM_INVALID_SHARED	69	/* cannot pass non-SHARED argument to SHARED parameter */
#define USE_EMPTY_FORKING		70	/* FORKING without any FORKed things -- moved from chk */
#define USE_BARRIER_PARALLEL		71	/* BARRIER used in PARallel */
#define USE_BARRIER_EXTENDED_UNFIXED	72	/* extended BARRIER unfixed */
#define USE_UNEXPECTED_CHAN_DIR		73	/* unexpected channel-direction specifier */

/*}}}*/
/*{{{  related to formal-model checking/generation*/
#define USE_DUPLICATE_TAG_IN_MERGE	80	/* duplicate tag name in tag merge (-zfmcct) */
#define USE_FM_NO_CHANTYPE		81	/* no type found for channel-type (-zfmcct) */

/*}}}*/

