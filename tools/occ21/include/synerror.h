/* $Id: synerror.h,v 1.3 1997/09/04 13:48:12 jm40 Exp $ */

/*
 *	syntax eror numbers
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


/*{{{  error codes*/
#define SYN_OK 0
/*#define SYN_E_TOKEN               131*/

#define SYN_E_EXPR_OR_SPEC        132
#define SYN_E_GUARD_OR_SPEC       133
#define SYN_E_COLON               134
#define SYN_TOO_MANY_NAMES        135
#define SYN_E_SPEC                136
/*#define SYN_E_RBOX                137*/
#define SYN_E_PTYPE               138
#define SYN_E_BOX                 139
/*#define SYN_E_PROTOCOL            140*/
/*#define SYN_E_CASE                141*/
/*#define SYN_E_IS                  142*/
#define SYN_E_SPECIFIER		143
/*#define SYN_E_PROC                144*/
#define SYN_E_LPAREN		145
#define SYN_E_RPAREN		146
/*#define SYN_E_FUNCTION            147*/
#define SYN_PROTOCOL_NOT_DECLARED 148
/*#define SYN_E_OF                  149*/
/*#define SYN_E_LBOX                150*/
/*#define SYN_E_FROM                151*/
/*#define SYN_E_FOR                 152*/
#define SYN_E_ELEMENT             153
/*#define SYN_E_VALOF               154*/
#define SYN_E_OPERAND             155
/*#define SYN_E_INTTYPE             156*/
#define SYN_E_EXPR                157
#define SYN_E_IS_OR_RETYPES       158
/*#define SYN_E_VARIANT             159*/
/*#define SYN_E_ALT                 160*/
/*#define SYN_ILLEGAL_CONVERSION    161*/
/*#define SYN_E_INPUT               162*/
#define SYN_E_ACTION              163
/*#define SYN_E_EQ                  164*/
#define SYN_E_PAR_OR_ALT          165
/*#define SYN_E_PAR                 166*/
#define SYN_E_RBOX_OR_FROM        167
#define SYN_E_AT_OR_IN            168
#define SYN_E_NAME                169
#define SYN_INVALID_PROCESS       170
#define SYN_BAD_INDENT            171
/*#define SYN_ILLEGAL_TAG           172*/
#define SYN_E_NEWLINE             173
#define SYN_E_COLON_IS_RETYPES    174
#define SYN_E_COLON_IS            175
/*#define SYN_E_PROCESSOR           175*/
#define SYN_ILLEGAL_DECL_TYPE     176
/*#define SYN_E_ENDOFFILE           177*/
#define SYN_LIST_IN_IO            178
/*#define SYN_E_RESULT              179*/
#define SYN_SC_ERROR              180
#define SYN_STRING_TOO_LONG       181
#define SYN_E_WS_OR_VS            182
#define SYN_PREDEF_ERROR          183
#define SYN_MISSING_FILENAME      184
#define SYN_FILE_OPEN_ERROR       185
#define SYN_BAD_GUYLINE           186
#define SYN_GUY_NOT_ENABLED       187
#define SYN_OPTION_IN_BAD_POS     188
#define SYN_BAD_OPTION            189
#if 0
#define SYN_E_HCOMMENT            190
#define SYN_E_OPTION              191
#else
#define SYN_E_STRING_AFTER        190
#define SYN_GUY_IS_OBSOLETE       191
#endif
#define SYN_ASM_DISALLOWED	  192
/*#define SYN_PROG_ERROR            192*/
/*#define SYN_E_PROCESSOR_TYPE      193*/
#define SYN_BAD_DESCRIPTOR        194
#define SYN_SC_EMPTY              195

/*#define SYN_BAD_PRAGMA_POSN       196*/
#define SYN_BAD_PRAGMA_NAME       197
#define SYN_BAD_PRAGMA_DIRECTIVE  198

/*#define SYN_DUPLICATE_IN_LIB      199*/
#define SYN_NOTHING_IN_LIB        200

/*#define SYN_TRANSLATE_NUL               201*/
#define SYN_TRANSLATE_ILLEGAL_CHAR      201
#define SYN_TRANSLATE_DUPLICATE_IN      202
#define SYN_TRANSLATE_DUPLICATE_EX      203
#define SYN_TRANSLATE_SEQUENCE          204

#define SYN_SC_IS_OBSOLETE         205

/*#define SYN_E_SUBTRACT            206*/
/*#define SYN_E_ELEMENT_OR_SPEC     207*/
#define SYN_DIFFERENT_DESCRIPTORS 208
#define SYN_PLACE_NAME            209
#define SYN_SKIPPING_DEFN         210

#define SYN_STDLIB_HAS_VS         211

/*{{{  User defined operators*/
#ifdef USER_DEFINED_OPERATORS
#define SYN_UDO_NOT_SUPPORTED     212
#define SYN_UDO_NO_RET_PARAM      213
#define SYN_UDO_MULTI_RET_PARAMS  214
#define SYN_UDO_TWO_PARAMS        215
#define SYN_UDO_ONE_PARAM         216
#define SYN_UDO_ONE_OR_TWO_PARAMS 217
#endif
/*}}}*/
/*{{{  Initial Declarations*/
#ifdef INITIAL_DECL
#define SYN_INITDECL_ADIM_MISSING 218
#endif
/*}}}*/
/*{{{  MOBILE stuff */
#ifdef MOBILES
#define SYN_NO_MOBILES		220
#endif
/*}}}  */
/*{{{  Minor support for blocking system-calls */
#define SYN_BSYSCALL_NOTMINWS2	221
#define SYN_BSYSCALL_NOTMINWS3	222
/*}}}  */
/*{{{  channel-direction specifier and chan-type syntax errors*/
#define SYN_BAD_CHAN_CONFLICT	223
#define SYN_E_CHANTYPE 224
/*}}}*/
/*{{{  forking/fork related syntax errors*/
#define SYN_E_FORK_PROCCALL 225
#define SYN_E_FORK_PROCPARAMS 226
/*}}}*/
/*{{{  more channel-direction specifier errors*/
#define SYN_E_CHANTYPEDIRSPEC 227
/*}}}  */
/* frmb: more stuff */
#define SYN_REC_TYPE_NOT_MOBILE 228
#define SYN_UNEXPECTED_CHANDIR 229
#define SYN_MOBILE_SUBTYPE 230

#define SYN_EXTENDS_NOT_TAGGED 231
#define SYN_EXPECTED_CHAN 232
#define SYN_NO_MPROCS 233

#define SYN_BAD_TRACE 234
#define SYN_E_TRACEDEF 235
#define SYN_NONCONST_TAGVAL 236

#define SYN_MISPLACED_FIXED 237

#define SYN_E_TYPE 238

/*{{{  dynamic proc call and related syntax errors*/
#define SYN_E_DYNCALL_PROCCALL 239
#define SYN_E_DYNCALL_PROCPARAMS 240
#define SYN_E_TYPEHASH 241

/*}}}*/

/* These were used by a very early version of the configurer: */
/*#define SYN_INVALID_SET_ITEM      250*/
/*#define SYN_E_LINK_DEL            251*/
/*#define SYN_E_RANDOM              252*/
/*#define SYN_E_LINKNULL            253*/
/*#define SYN_E_NETWORK             254*/
/*#define SYN_E_TO                  255*/
/*#define SYN_E_LINK                256*/
/*#define SYN_E_ARRAY               257*/
/*#define SYN_E_COMMA               259*/
/*#define SYN_E_RIGHTARROW          260*/
/*}}}*/

/*{{{  error macros*/
#define synwarn(N,L)	 msg_out  (SEV_WARN,SYN,(N),(L))
#define synwarn_i(N,L,I) msg_out_i(SEV_WARN,SYN,(N),(L),(I))
#define synwarn_s(N,L,S) msg_out_s(SEV_WARN,SYN,(N),(L),(S))

#define synerr(N,L)      msg_out  (SEV_ERR,SYN,(N),(L))
#define synerr_s(N,L,S)  msg_out_s(SEV_ERR,SYN,(N),(L),(S))
#define synerr_i(N,L,I)  msg_out_i(SEV_ERR,SYN,(N),(L),(I))
#define synerr_e(N,L,T)  msg_out_e(SEV_ERR,SYN,(N),(L),(T))
/*}}}*/



