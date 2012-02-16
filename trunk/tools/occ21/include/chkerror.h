/* $Id: chkerror.h,v 1.2 1997/02/14 16:32:12 mdp2 Exp $ */

/*
 *	Checker error numbers
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
/*{{{  normal */
#define CHK_OK 0
/*#define CHK_UNKNOWN_TYPE 1*/
#define CHK_TYPES_DIFF 2
#define CHK_INVTYPE 3
#define CHK_INVCOUNT 4
#define CHK_INV_EXACT 5
#define CHK_INV_ROUND_TRUNC 6
#define CHK_ASS_UNBALANCED 7
#define CHK_INV_SUBSCRIPT 8
#define CHK_INVTYPE_MULTI_ASS 9
#define CHK_ABBR_TYPE_MISMATCH 10
/*#define CHK_UNKNOWN_NODE 11*/
#define CHK_WARN_GUY       11
#define CHK_INVTYPE_FRESULT 12
#define CHK_TOOMANYEXPS 13
#define CHK_TOOFEWEXPS 14
#define CHK_RETYPE_TYPE_MISMATCH 15
#define CHK_INV_FUNC_LIST        16
#define CHK_INV_VALOF_LIST       17
#define CHK_ADIM_NOT_CONST       18
#define CHK_ADIM_NOT_INT         19
#define CHK_INV_PROT             20
/*#define CHK_INVTYPETAG         21*/
#define CHK_NOT_ADJACENT_NAME    21
#define CHK_TOO_FEW_VARS         22
#define CHK_TOO_MANY_VARS        23
/*#define CHK_INV_SIZE_OPERAND   24*/
/*#define CHK_SEGMENT_CONSTRUCTOR  24*/
#define CHK_INV_TYPE_DECORATION  24
#define CHK_CFOLD_OFLOW          25
/*#define CHK_NAMENOTCONST       26*/
#define CHK_UNPLACED_PORT        26
#define CHK_NAMENOTARRAY         27
/*#define CHK_SUBSCRIPT_NOTCONST 28*/
#define CHK_SUBSCRIPT_OUT_OF_RANGE 29
#define CHK_INV_DECL_TAG         30
#define CHK_ADIM_NEGATIVE        31
#define CHK_ADIM_TOOBIG          32
/*#define CHK_INV_CONSTRUCT      33*/
#define CHK_PLACE_IN_VALOF       33
#define CHK_PLACE_IN_VALOF_CALL  34

/*#define CHK_INV_LIST 43*/
#define CHK_INV_SEGMENT_OPERAND 44
/*#define CHK_SLENGTH_NOT_CONST 45*/
#define CHK_TAG_IN_EXPRESSION   45
#define CHK_ADIM_MISSING 46
#define CHK_INVTYPE_SPEC 47
/*#define CHK_INV_EXP 48*/
#define CHK_INVTYPE_CONSTEXP 49
#define CHK_EXP_NOT_CONST 50
#define CHK_TOO_MANY_ELSES 51
#define CHK_MULTIPLE_CASE 52
#define CHK_INVTYPE_SELECTOR 53
#define CHK_INVTYPE_SELN 54
#define CHK_SELN_NOTCONST 55
/*#define CHK_UNRESOLVED_INTTYPE 56*/
#define CHK_TYPE_MISMATCH_EXPECTED 56
#ifndef OCCAM2_5
#define CHK_UNRESOLVED_REALTYPE 57
#endif
#define CHK_TYPE_MISMATCH 58
#define CHK_ASUB_NOT_INT 59
#define CHK_FN_VAR_PARAM 60
#define CHK_PTYPE_MISMATCH 61
#define CHK_TOO_FEW_PITEMS 62
#define CHK_TOO_MANY_PITEMS 63
#define CHK_BAD_PTAG 64
#define CHK_INVTYPE_STARTEXP 65
#define CHK_INVTYPE_LENGTHEXP 66
#define CHK_REPL_NOT_CONST 67
#define CHK_REPL_NEGATIVE 68
#define CHK_INVTYPE_GUARD 69
/*#define CHK_INV_CONFIG 70*/
#define CHK_REPL_OVERFLOW 70
#define CHK_INV_VAL_OCCAM 71
#define CHK_NAME_NOT_PROC 72
#define CHK_INVTYPE_PARAM 73
#define CHK_TOO_FEW_ACTUALS 74
#define CHK_TOO_MANY_ACTUALS 75
#define CHK_INVVARIABLE_PARAM 76
#define CHK_INV_CINDEX 77
#define CHK_NOT_CHANNEL 78
#define CHK_INV_TIMER_INPUT 79
#define CHK_TIME_TYPE_MISMATCH 80
#define CHK_NOT_CHANNEL_OR_TIMER 81
#define CHK_NOT_TIMER 82
#define CHK_INV_PORT_PROT 83
#define CHK_INDISTINCT_TAG 84
#define CHK_NOT_A_TAG 85
#define CHK_LOST_VARIANT 86
#define CHK_NAME_NOT_FUNCTION 87
#define CHK_LOST_VALOF 88
#define CHK_VALOF_SCOPE 89
#define CHK_BAD_VALOF 90
#define CHK_VALOF_CALL_SCOPE 91
#define CHK_SUBSCRIPT_RANGE 92
#define CHK_SEG_START_RANGE 93
#define CHK_SEG_LENGTH_RANGE 94
#define CHK_SEG_RANGE 95
#define CHK_INVTYPE_STEPEXP 96
/*#define CHK_CONSTANT_SEGMENT_ERROR 96*/
#define CHK_BAD_PLACE_NAME 97
#define CHK_INVTYPE_PLACEMENT 98
#define CHK_TOO_MANY_MULTIVALUES 99
#define CHK_NAME_NOT_DECLARED 100
/*#define CHK_TAG_CONFLICT 101*/
#ifndef OCCAM2_5
#define CHK_BAD_FUNCTION_TYPE 102
#endif
#define CHK_BAD_INPUT_PROTOCOL 103
#define CHK_BAD_CASE_INPUT_PROTOCOL 104
#define CHK_BAD_TAGGED_INPUT_PROTOCOL 105
#define CHK_ALT_INPUT_PORT 106
#define CHK_BAD_WSPLACE 107
#define CHK_NAME_MULTIPLY_PLACED 108
#define CHK_MULTIPLE_VARIANT 109
#define CHK_TOO_MANY_TAGS 110
#define CHK_BAD_DEST 111
/*#define CHK_BAD_COUNT 112*/
#define CHK_BAD_COLON2  112
#define CHK_COUNT_OUT_OF_RANGE 113
#define CHK_BAD_INLINE_NAME 114
#define CHK_MISSING_TAG_IN_CASE_INPUT 115
#define CHK_NON_EXHAUSTIVE_CASE_INPUT 116
/*#define CHK_ENTRYPOINT_NOT_FOUND 115*/
/*#define CHK_NAME_STACK_OVERFLOW 116*/
#define CHK_BAD_GUY_OPERAND 117
#define CHK_DISABLED_GUYCODE 118
#define CHK_INVALID_GUYCODE 119
#define CHK_BAD_GUYCODE 120
#define CHK_BAD_STEPCODE 121
#define CHK_MISSING_OPERAND 122
#define CHK_EXTRANEOUS_OPERAND 123
#define CHK_BAD_VALOF_CALL 124
#define CHK_INVTYPE_PROCEXP 125
/*#define CHK_INVPROCTYPE 126*/
#define CHK_NESTED_PRI_PAR 127
#define CHK_NESTED_PRI_PROC 128
#define CHK_INV_PRI_PAR 129
/*#define CHK_NO_PRI_REPL_PAR        130*/
#define CHK_NOT_IMPLEMENTED          130
#define CHK_UNKNOWN_CONSTRUCTOR_SIZE 131
#ifdef CONDEXP
#define CHK_CONDEXP_NOT_BOOL 132
#endif
#define CHK_INV_ABBR 133
#define CHK_CONSTRUCTOR_TYPE_MISMATCH 134
#define CHK_ASS_NAME_USED_MULTIPLY 135
#define CHK_INV_RETYPE 136
#define CHK_NAME_NOT_USED 137
#define CHK_PARAM_NOT_USED 138
#define CHK_ROUTINE_NOT_USED 139
#define CHK_BAD_VALRETYPE       140
#define CHK_BAD_CHANRETYPE      141
#define CHK_RETYPE_NOTYPE       142
#define CHK_BAD_DEST_CONSTRUCTOR        143
#define CHK_BAD_DEST_STRING             144
#define CHK_INV_SHIFT                   145
#define CHK_INV_MULTIASSIGN             146
#define CHK_ASM_TOO_BIG                 147
#define CHK_UNIMPLEMENTED_ASMCODE       148
/*#define CHK_LABEL_INSIDE_INLINE         149*/
#define CHK_LIB_WRONG_IO_CONFIG         149
#define CHK_NAME_IS_DESCOPED            150
/*#define CHK_ARRAY_SIZE_OVERFLOW         151*/
#define CHK_LIB_WRONG_IO                151
#define CHK_RSUB_NOT_FIELD              152
#define CHK_LONELY_FIELD                153
#define CHK_LIB_WRONG_TARGET            154
#define CHK_BAD_ANY_PROT                155
#define CHK_ASM_BAD_STORE               156
#define CHK_INV_ASSERT                  157
#define CHK_DUPLICATE_LABEL             158 
#define CHK_BAD_PRAGMA_SHARED_NAME      159
#ifndef OCCAM2_5
#define CHK_BAD_VALOF_TYPE              160
#endif
#define CHK_NONLOCAL_LABEL              161
#define CHK_CHAN_OF_ANY_USED            162
#define CHK_CHAN_OF_ANY_PARAM_PASSING   163
#define CHK_OUTERLEVEL_CONSTANT         164
#define CHK_COLON2_INPUT                165
#define CHK_BAD_ASM_INLINE_OP           166
#define CHK_RETYPINGREAL64              167
/*}}}  */
/*{{{  occam 2.5 */
#ifdef OCCAM2_5
#define CHK_BAD_USERTYPE                180
#define CHK_ARRAY_CONVERSION            181
#define CHK_ILLEGAL_FIELD_DECL          182
#define CHK_DUPLICATE_FIELD_DECL        183
#define CHK_UNALIGNED_PACKED_FIELD      184
#define CHK_UNALIGNED_PACKED_RECORD     185
#define CHK_BAD_CONSTRUCTOR_TYPE        186
#define CHK_REC_LIT_TYPE_MISMATCH       187
#define CHK_REC_LIT_TOO_SMALL           188
#define CHK_REC_LIT_TOO_LARGE           189
#define CHK_OFFSETOF_NOT_RECORD         190
#define CHK_OFFSETOF_NOT_FIELD          191
#define CHK_BAD_RESHAPES                192
#define CHK_BAD_TYPENAME                193
#define CHK_NO_RECF_ZERO                194
#define CHK_BAD_STRING_TYPE             195
#endif
/*}}}  */
/*{{{  user defined operators */
#ifdef USER_DEFINED_OPERATORS
#define CHK_UDO_NOT_SUPPORTED		196
#endif
/*}}}  */
/*{{{  configurer / NDL reader */
#if 1 /*def CONFIG*/
/* Configuration/NDL errors */

#define CHK_ILLEGAL_CONFIG_CONSTRUCT    200
#define CHK_ILLEGAL_CONSTRUCT           201
#define CHK_DUPLICATE_CONSTRUCT         202
#define CHK_ZERO_CONSTRUCT              203
#if 0
#define CHK_INVTYPE_EDGE                204
#define CHK_INVTYPE_ARC                 205
#define CHK_INVTYPE_DEVICE              206
#else
#define CHK_INVTYPE_CONFIG              204
#define CHK_FIELD_NOT_SET               205
#endif
#define CHK_INVTYPE_ATTR                207
#define CHK_INV_MAPPING_RHS             208
#define CHK_INV_MAPPING_LHS             209
#define CHK_INV_MAPPING_PRI             210
#define CHK_INV_MAPPING_NOPRI           211
#define CHK_INV_PLACEON_RHS             212
#define CHK_ILLEGAL_PLACE               213
#define CHK_CALLING_CONVENTION          214
#define CHK_FIELD_NOT_VALID             215
#define CHK_INV_VAL_NDL                 216
#define CHK_INV_VAL_CONFIG              217
#define CHK_INV_RHS_NDL_MAPPING         218
#define CHK_INV_LHS_NDL_MAPPING         219
#define CHK_NDL_MAPPING_TOO_MANY_SOURCES 220

#endif
/*}}}  */

#ifdef MOBILES
/*{{{  MOBILE checking errors */
#define CHK_PROMOTE_MOBILE		221
#define CHK_DYN_DIM_NOT_INT		222
/*}}}  */
#endif	/* MOBILES */

#define CHK_BAD_LIST_LENGTHS		223
#define CHK_INV_RESULT_OCCAM		224
#define CHK_INV_RESULT_NDL		225
#define CHK_INV_RESULT_CONFIG		226
/*}}}  */
/*{{{  SKIP-guard stuff */
#define CHK_NO_SKIP_PRECOND     	227
#define CHK_SKIP_NOT_LAST_PRI		228
/*}}}  */
/*{{{  PAR/SEQ/IF/ALT emptiness checks */
#define CHK_EMPTY_SEQ			229
#define CHK_EMPTY_PAR			230
#define CHK_EMPTY_IF			231
#define CHK_EMPTY_ALT			232
/*}}}  */
/*{{{  channel direction specifier stuff (specifications in parameters/abbreviations) */
#define CHK_BAD_CHAN_CONFLICT		233
#define CHK_NO_CHANDIR_SPEC		234
#define CHK_NO_CHANDIR_SPEC_ANON	235
/* channel usage oopsing */
#define CHK_CHANDIR_NOT_INPUT		236
#define CHK_CHANDIR_NOT_OUTPUT		237
/*}}}  */
/*{{{  invalid placements*/
#define CHK_INV_VAR_PLACEMENT		238
#define CHK_INV_CHAN_PLACEMENT		239
/*}}}*/
/*{{{  channel direction errors when dealing with (MOBILE) CHAN TYPEs */
#define CHK_INV_CHANTYPE_VAR		240
#define CHK_INV_CHANTYPE_ALLOC		241
#define CHK_INV_CHANTYPE_ALLOC_TARGET	242
#define CHK_INV_CHANTYPE_ALLOC_TYPE	243
#define CHK_INV_CHANTYPE_ALLOC_VARTYPE	244
/*}}}*/
/*{{{  forking related errors*/
#define CHK_NO_LOCAL_FORKING		245
#define CHK_NAME_INSIDE_FORKING		246
#define CHK_EMPTY_FORKING		247
#define CHK_FORK_PARAM_NOT_SHARED	248
#define CHK_FORK_PARAM_NOT_IMPLEMENTED	249
/*}}}*/
/*{{{  more CHAN TYPE errors (sharing)*/
#define CHK_CLAIM_VAR_BADTYPE		250
#define CHK_CLAIM_VAR_NOTSHARED		251
#define CHK_CLAIM_VAR_NOTCHANTYPE	252
#define CHK_FPARAM_NOTSHARED		253
#define CHK_APARAM_NOTSHARED		254
#define CHK_UNSHARABLE_PROTOCOL		255
#define CHK_UNSHARABLE_TYPE		256
#define CHK_UNSHARED_CLONE		257
#define CHK_CLAIM_ANON_NODIRSPEC	258
/* anonymous channel-type misuse */
#define CHK_ANONCHANTYPE_ASSIGN		259
/*}}}*/
/*{{{  SKIP-in-ALT checks */
#define CHK_SKIP_IN_ALT			260
/*}}}*/
/*{{{  more MOBILE errors */
#define CHK_INNER_MOBILE		261
#define CHK_VAL_MOBILE			262
/*}}}*/
/*{{{  extra errors for pre-define misuse*/
#define CHK_DECODE_BAD_CHANDIR		263
#define CHK_ENCODE_BAD_CHANDIR		264
/*}}}*/
/*{{{  more errors*/
#define CHK_CONSTRUCTOR_BAD_CHANDIR	265
#define CHK_BAD_CHAN_CONFLICT_ANON	266
/* #define CHK_NO_REC_FORK		267 */
#define CHK_NO_INLINE_FORK		268
#define CHK_NO_PREDEF_FORK		269
#define CHK_VAR_IN_FREE_FORK		270
#define CHK_UNEXPECTED_LIST		271
#define CHK_UNEXPECTED_CHANDIR		272
#define CHK_NO_CHANTYPE_DIR		273
#define CHK_MOBILE_RETYPE		274
#define CHK_MOBILE_RESHAPE		275
#define CHK_MOBILE_IO			276
#define CHK_MOBILE_REQUIRED		277

#define CHK_MISSING_FORWDECL		278

#define CHK_BAD_EXTENDS			279
#define CHK_EXTENDS_ERROR		280
#define CHK_EXTENDS_NODIR		281
#define CHK_EXTENDS_SPECIALISATION	282
#define CHK_EXTENDS_GENERALISATION	283
#define CHK_EXTENDS_BADTAGMATCH		284

/* some MOBILE PROC related errors */
#define CHK_MPD_BAD_IMPLEMENTS		285
#define CHK_MPD_BAD_IMPLEMENTS_FOR	286
#define CHK_MPD_BAD_IMPLEMENTS_PARAMS	287
#define CHK_MPP_FORKED			288
#define CHK_MPP_NOT_MOBILE		289

/* TRACES related errors */
#define CHK_TRACES_NOT_RECORD		290
#define CHK_BAD_TRACE_ELEMENT		291
#define CHK_BAD_TRACE_DIRECTION		292

/* tagged protocol fixing errors */
#define CHK_PARTIAL_FIXED_TAGS		293
#define CHK_INVALID_TAG_VALUE		294
#define CHK_TAG_VALUE_EXISTS		295

/* separate compilation type-check errors */
#define CHK_EXTERN_TYPE_MISMATCH	296

/* mobile abbreviation errors */
#define CHK_ABBR_LHS_SHARED		297
#define CHK_ABBR_RHS_SHARED		298

/* more MOBILE PROC related errors */
#define CHK_MPD_FIXED_MISMATCH		299
#define CHK_MPD_ALLOC_NOT_MPROCDECL	300
#define CHK_MPD_ALLOC_PROCTYPEDECL	301

/* ANY CHAN TYPE related errors */
#define CHK_ANYCHANTYPE_SIMPLEINPUT	302
#define CHK_ANYCHANTYPE_MISMATCH	303
#define CHK_MULTIPLE_VTYPES		304
#define CHK_ANYCHANTYPE_RESTRICTED	305

/* user-level BARRIER related errors */
#define CHK_SYNC_NOT_BARRIER		306
#define CHK_RESIGN_NOT_BARRIER		307
#define CHK_EXTENDS_NOT_BARRIER		308
#define CHK_BARRIER_RESIGNED		309
#define CHK_BARRIER_BAD_DECL		310
#define CHK_BARRIER_BAD_EXTENDS		311
#define CHK_BARRIER_INTERNAL		312
#define CHK_BARRIER_ASSIGN		313

/* mobile errors */
#define CHK_MOBILE_MIXED_DYNAMIC	314

/* more errors */
#define CHK_ANYCHANTYPE_SPECMISMATCH	315
#define CHK_ANYCHANTYPE_INCOMPAT	316
#define CHK_ANYPROCTYPE_SIMPLEINPUT	317
#define CHK_ANYPROCTYPE_MISMATCH	318
#define CHK_ANYPROCTYPE_INCOMPAT	319
#define CHK_UNEXPECTED_DIRSPEC		320

#define CHK_PROTOCOL_AS_DECLTYPE	321
#define CHK_PROTOCOL_AS_PARAMTYPE	322

#define CHK_CLAIM_VAR_ISCHAN		323
#define CHK_PARAM_MISSING_TYPE		324

#define CHK_BUFCHAN_NOTCONST		325
#define CHK_BUFCHAN_NOTAGPROTO		326
#define CHK_BUFCHAN_MISSINGSIZE		327
#define CHK_BUFCHAN_NEGATIVE		328
#define CHK_BUFCHAN_TOOBIG		329

#define CHK_ANYMOBILETYPE_MISMATCH	330

#define CHK_CLAIM_VAR_BADDIRSPEC	331

/* dynamic call related errors */
#define CHK_CALLAT_NOT_DYNAMIC		332
#define CHK_BAD_DYNCALL_TYPE		333
#define CHK_BAD_DYNCALL_NAMEDTYPE	334
#define CHK_BAD_FMTYPES_TYPE		335
#define CHK_BAD_FMTYPES_NAMEDTYPE	336

/* protocol inclusion errors */
#define CHK_BAD_CASE_INCLUSION		337

/* other */
#define CHK_INVSIZE_CONST		338


/*}}}*/
/*{{{  error macros */
#define chkerr(N,L)      msg_out  (SEV_ERR,CHK,(N),(L))
#define chkerr_s(N,L,S)  msg_out_s(SEV_ERR,CHK,(N),(L),(S))
#define chkerr_i(N,L,I)  msg_out_i(SEV_ERR,CHK,(N),(L),(I))
#define chkerr_ss(N,L,S,T) msg_out_ss(SEV_ERR,CHK,(N),(L),(S),(T))

#define chkwarn(N,L)	msg_out(SEV_WARN,CHK,(N),(L))

#define chkreport(N,L)           msg_out  (SEV_ERR_JMP,CHK,(N),(L))
#define chkreport_i(N,L,P)       msg_out_i(SEV_ERR_JMP,CHK,(N),(L),(P))
#define chkreport_s(N,L,S)       msg_out_s(SEV_ERR_JMP,CHK,(N),(L),(S))
#define chkreport_ss(N,L,S,T)    msg_out_ss(SEV_ERR_JMP,CHK,(N),(L),(S),(T))
/*}}}  */


