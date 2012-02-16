/* $Id: lexerror.h,v 1.1 1996/04/15 10:52:12 djb1 Exp $ */

/*
 *	Lexical error numbers
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
#define LEX_OK 0
#define LEX_ILLEGAL_CHAR 1
/*#define LEX_FORMAT_ERROR 2*/
#define LEX_NUMBER_ERROR 3
#define LEX_E_QUOTE 4
#define LEX_E_DOUBLEQUOTE 5
#define LEX_E_HEXDIGIT 6
#define LEX_OUT_OF_SPACE 7
#define LEX_EOF 8
#define LEX_ILLEGAL_STRING_CHAR 9
#define LEX_BAD_STRING_LENGTH 10
#define LEX_BAD_STRING_CONTINUATION 11
#define LEX_BAD_INDENT 12
#define LEX_BAD_STRING_LENGTH_POSN 13
#define LEX_READ_ERROR 14
/*#define LEX_TOO_MANY_FILES 15*/
#define LEX_FILE_OPEN_ERROR 16
/*#define LEX_BAD_SCDESC_FORMAT 17*/
/*#define LEX_BAD_SC_COMPATIBILITY 18*/
#define LEX_MISSING_ENTRYPOINT 19
/*#define LEX_BAD_SC_PROCTYPE 20*/
/*#define LEX_BAD_SC_ERRORMODE 21*/
/*#define LEX_TOO_MANY_NESTED_SCS 22*/
/*#define LEX_OBJFILE_WRITE_ERROR 23*/
#define LEX_TOO_MANY_ENTRYPOINTS 24
#define LEX_BAD_DESC_FORMAT 25
#define LEX_LIBRARY_NOT_SC 26
#define LEX_HASHWORD_ERROR 27
#define LEX_STRING_ERROR 28
#define LEX_CHAR_ERROR 29
#define LEX_ESCAPE_SQUOTE 30
/*OLD #define LEX_LINE_TRUNCATED 30*/

#define LEX_ESCAPE_BLSQUOTE 31
#define LEX_ESCAPE_BLDQUOTE 32
/*OLD #define LEX_TCOFF_IOERR     31*/
/*OLD #define LEX_TCOFF_BAD_LCHAR 32*/
/*#define LEX_TCOFF_BAD_CHAR  33*/
#define LEX_TCOFF_BAD_DESC  34
/*#define LEX_TCOFF_BAD_PROCESSOR       35*/
/*#define LEX_TCOFF_BAD_MODE            36*/
#define LEX_TCOFF_UNMATCHED_ENDMODULE 37
/*#define LEX_TCOFF_DESC_OVERFLOW       38*/
#define LEX_TCOFF_WRONG_FORMAT        39
#define LEX_FILE_LENGTH_ERROR         40
#define LEX_SIGN_ERROR                41
#define LEX_TCOFF_NOT_LINKED          42
#define LEX_NAME_TRUNCATED            43
#define LEX_NUMBER_TRUNCATED          44

/* frmb added */
#define LEX_NO_SAVE_STATE		45
#define LEX_SAVE_STATE_ERROR		46

/* for the pre-processor */
#define LEX_PP_BAD_DEFINE		47
#define LEX_PP_ILLEGAL_NAME		48
#define LEX_PP_BAD_UNDEF		49
#define LEX_PP_BAD_IF			50
#define LEX_PP_BAD_ENDIF		51
#define LEX_PP_BAD_ELIF			52
#define LEX_PP_BAD_ELSE			53
#define LEX_PP_UNBALANCED_ENDIF		54
#define LEX_PP_NESTED_IF_OUTDENTS	55
#define LEX_PP_IF_INDENT		56
#define LEX_PP_UNBALANCED_ELSE		57
#define LEX_PP_UNBALANCED_IF		58
#define LEX_PP_UNBALANCED_ELIF		59
#define LEX_PP_USER_WARNING		60
#define LEX_PP_USER_ERROR		61
#define LEX_PP_EXPECTED_RPAREN		62
#define LEX_PP_GARBAGE			63
#define LEX_PP_BADCMDLINEDEF		64
#define LEX_PP_LOOSE_RELAX		65
#define LEX_PP_OVER_RELAX		66
#define LEX_PP_RELAX_ERROR		67
#define LEX_PP_NOSUCHNAME		68
#define LEX_PP_NOSUCHVAL		69
#define LEX_PP_BAD_COMPARE		70
/*}}}*/

/*{{{  error macros */
#define lexwarn(N,L)       msg_out  (SEV_WARN,SYN,(N),(L))
#define lexwarn_s(N,L,S)   msg_out_s(SEV_WARN,SYN,(N),(L),(S))
#define lexwarn_i(N,L,I)   msg_out_i(SEV_WARN,SYN,(N),(L),(I))

#define lexerr(N,L)        msg_out  (SEV_ERR,SYN,(N),(L))
#define lexerr_s(N,L,S)    msg_out_s(SEV_ERR,SYN,(N),(L),(S))
#define lexerr_i(N,L,I)    msg_out_i(SEV_ERR,SYN,(N),(L),(I))

#define lexfatal(N,L)      msg_out  (SEV_FATAL,SYN,(N),(L))
#define lexfatal_s(N,L,S)  msg_out_s(SEV_FATAL,SYN,(N),(L),(S))
#define lexfatal_i(N,L,I)  msg_out_i(SEV_FATAL,SYN,(N),(L),(I))
/*}}}*/

