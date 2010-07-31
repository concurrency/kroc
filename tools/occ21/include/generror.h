/* $Id: generror.h,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	error numbers given by gen
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
#define GEN_BAD_OPD                 1
#define GEN_BAD_MOVEELEMENT_DIRN    2
#define GEN_CODE_BUFFER_FULL        3
/*#define GEN_LABEL_BUFFER_FULL       4*/
#define GEN_MULTIPLY_DEFINED_LABEL  5
#define GEN_ADJUST_ERROR            6
#define GEN_ADJUST_TAG_ERROR        7
#define GEN_CANNOT_OPEN_OBJECT_FILE 8
#define GEN_ASSERT_ERROR            9
/*#define GEN_TOO_MANY_SEGMENTS     9*/
#define GEN_BAD_CONSTANT_LOAD      10
/*#define GEN_ILLEGAL_OPDMODE      11*/
/*#define GEN_BAD_TAG              12*/
#define GEN_PLACE_OVERFLOW         13
/*#define GEN_TOO_MANY_NESTED_ALTS 14*/
/*#define GEN_TOO_MANY_VARS        15*/
/*#define GEN_TOO_MANY_STDLIB_NAMES 16*/
#define GEN_MISSING_STDLIB_ENTRY   17
#define GEN_BAD_PRIMARY_OPERAND    18
#define GEN_KERNEL_RUN_ERROR       19
/*#define GEN_CSE                    20*/
#define GEN_CANNOT_CLOSE_OBJECT_FILE 21
#define GEN_INDEX_HASH_TABLE_ERR   22
#define GEN_UNSIZED_ABBREV         23
/*#define GEN_RUN_TIME_OVERLAP_CHECK 24*/
#define GEN_LINEMARK_LIST_ERR      25
/*#define GEN_UNEXPECTED_EOF         26*/
/*#define GEN_LINE_NOT_IN_LIST     27*/
#define GEN_BAD_USECOUNT           28
/*#define GEN_RUN_TIME_OVERLAP_CHECKS     29*/
#define GEN_ERROR_IN_ROUTINE            30
#define GEN_STDLIBS_NOT_ENABLED         31
/*#define GEN_STDLIB_HAS_VS               32*/
#define GEN_WORKSPACE_CLASH             33
#define GEN_GUY_NO_CHANS                34
#define GEN_BAD_ASMNAME                 35
/* #define GEN_PLACE_ON_LINKS              36*/
#define GEN_REPL_FN_IN_ALT_GUARD        37
#define GEN_ENABLE_CHAN_ALIGNMENT_PROBLEM 38
#define GEN_ALIGNMENT_CHECK             39
#define GEN_DEVMOVE_LATENCY_PROBLEM     40
#define GEN_PLACE_BELOW_MEMSTART        41
#define GEN_H_PLACE_ON_LINKS            42
#define GEN_T_PLACE_ON_LINKS            43
/* Configurer errors: */
#define GEN_CONF_PATCH_TOO_SMALL        50
#define GEN_CONF_CANT_FIND_PATCH        51
/*}}}  */
/*{{{  frmb added errors */
#define GEN_NO_DYNMEM			52
/* #define GEN_NO_RECFORK		53 */
#define GEN_IMPOSSIBLE			54	/* largely for debugging */
#define GEN_TSTACK_UNDERFLOW		55
#define GEN_TSTACK_OVERFLOW		56	/* lets make these nice.. */
#define GEN_NO_FMPARAM			57	/* no forked MOBILE parameters */
#define GEN_INTERNAL_ERROR		58	/* provides a message */
#define GEN_WARN_BADCODE		59	/* means we're generating known broken code (maybe) */
/*}}}  */
/*{{{  cgr added errors */
#define GEN_NO_MOBILESPACE		60	/* tried to use mobilespace, but not enabled */
#define GEN_BAD_MOBILE_SLICE		61	/* tried to copy a slice containing mobiles */
#define GEN_BARRIER_CLONE		62	/* tried to clone a (mobile) barrier */
#define GEN_BAD_ALIGNMENT		63	/* bad alignment expression */
#define GEN_PRI_PAR_AS_PAR		64	/* PRI PAR is compiled as PAR */
#define GEN_ADDROF_BAD_TYPE		65	/* ADDROF/HWADDROF unsupported type */

/*}}}*/
/*{{{  more errors (frmb)*/
#define GEN_FORKED_EXPORT_NO_VS		66	/* exported PROC that FORKs must have VS */
/*}}}*/

/*{{{  error macros */
#define genwarn(N)         msg_out  (SEV_WARN, GEN, (N), genlocn)
#define genwarn_i(N,I)     msg_out_i(SEV_WARN, GEN, (N), genlocn,(I))
#define genwarn_s(N,S)     msg_out_s(SEV_WARN, GEN, (N), genlocn,(S))

#define generr(N)          msg_out  (SEV_FATAL, GEN, (N),genlocn)
#define generr_i(N,I)      msg_out_i(SEV_FATAL, GEN, (N),genlocn,(I))
#define generr_s(N,S)      msg_out_s(SEV_FATAL, GEN, (N),genlocn,(S))

#define geninternal(N)     msg_out  (SEV_INTERNAL, GEN, (N),genlocn)
#define geninternal_i(N,I) msg_out_i(SEV_INTERNAL, GEN, (N),genlocn,(I))
#define geninternal_s(N,S) msg_out_s(SEV_INTERNAL, GEN, (N),genlocn,(S))
#define geninternal_is(N,I,S) msg_out_is(SEV_INTERNAL, GEN, (N),genlocn,(I),(S))
/*}}}  */

/*{{{  routines */
const char *genmessagestring(int n);
/*}}}  */

/*{{{  side-effects pragmas */
#ifdef _ICC
#pragma IMS_nosideeffects (genmessagestring)
#endif
/*}}}  */
