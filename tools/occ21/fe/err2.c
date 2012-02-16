/* $Id: err2.c,v 1.4 1997/04/30 11:14:55 jm40 Exp $ */

/*
 *	Error reporting strings
 *	Copyright (C) 1992 Inmos Limited
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
#include <stdio.h>
#include "midinc.h"

#include "err2.h"

#include "lexerror.h"
#include "synerror.h"
#include "chkerror.h"
#include "useerror.h"
#include "vtierror.h"

#include "lexdef.h"	/* for extended_input */

/*}}}*/

/*{{{  PUBLIC const char *anymessagestring*/
PUBLIC const char *anymessagestring (const int n)
{
	switch (n) {
	case ANY_BAD_TAG:
		return "unknown tag %s in %s";
	case ANY_OBJFILE_WRITE_ERROR:
		return "cannot write to object file: error code %d";
	case ANY_OUT_OF_SPACE:
		/*return("Run out of symbol space"); */
#if 0				/*def CONFIG */
		return ("Not enough memory to configure this program");
#else
		return ("not enough memory to compile this program");
#endif
	case ANY_FATAL_ABORT:
		return ("internal inconsistency found in routine \"%s\"");
	case ANY_FILEINFO:
		return ("at line %d of file \"%s\"...");
	default:
		return NULL;
	}
}

/*}}}*/
/*{{{  PUBLIC const char *synmessagestring*/
PUBLIC const char *synmessagestring (const int n)
{
	switch (n) {
		/*{{{  LEX errors */
	case LEX_E_QUOTE:
		return ("expected single quote, found \"%c\"");
	case LEX_E_DOUBLEQUOTE:
		return ("expected double quotes, found \"%c\"");
	case LEX_E_HEXDIGIT:
		return ("expected hex digit, found \"%c\"");
	case LEX_ILLEGAL_CHAR:
		return ("illegal character (ASCII %d)");
		/*case LEX_FORMAT_ERROR:
		   return ("Format error"); */
	case LEX_NUMBER_ERROR:
		/*return("In number"); */
		return ("expected digit, found \"%c\"");
	case LEX_SIGN_ERROR:
		return ("exponent must be signed");
	case LEX_EOF:
		return ("unexpected end of file");
	case LEX_ILLEGAL_STRING_CHAR:
		return ("illegal character \"%c\" following \"*\" in string");
	case LEX_STRING_ERROR:
		return ("illegal character (ASCII %d) in string");
	case LEX_CHAR_ERROR:
		return ("illegal character constant");
	case LEX_ESCAPE_SQUOTE:
		return ("single quote inside string literal should be \'*\'\'");
	case LEX_ESCAPE_BLSQUOTE:
		return ("single quote inside byte literal should be \'*\'\'");
	case LEX_ESCAPE_BLDQUOTE:
		return ("double quote inside byte literal should be \'*\"\'");
	case LEX_BAD_STRING_LENGTH:
		return ("string too long to insert \"*l\" value");
	case LEX_BAD_STRING_CONTINUATION:
		return ("illegal string continuation");
	case LEX_BAD_INDENT:
		return ("incorrect indentation of string continuation");
	case LEX_BAD_STRING_LENGTH_POSN:
		return ("'*l' must be at start of string");
	case LEX_READ_ERROR:
		return ("error code %d when reading file");
		/*case LEX_TOO_MANY_FILES:
		   return("Files nested too deeply: maximum depth is %d"); */
	case LEX_FILE_OPEN_ERROR:
		return ("cannot open file \"%s\"");
	case LEX_FILE_LENGTH_ERROR:
		return ("maximum filename length is %d characters");
		/*
		   case LEX_BAD_SCDESC_FORMAT:
		   return("Bad descriptor file tag %d");
		   case LEX_BAD_SC_COMPATIBILITY:
		   return("Object code not compatible with current version (%s)");
		   case LEX_BAD_SC_PROCTYPE:
		   return("Object code compiled for incompatible target (%s)");
		   case LEX_BAD_SC_ERRORMODE:
		   return("Object code compiled in incompatible error mode (%s)");
		   case LEX_TOO_MANY_NESTED_SCS:
		   return("Cannot open \"%s\" - too many nested SCs");
		 */
	case LEX_MISSING_ENTRYPOINT:
		return ("missing entry-point \"%s\"");
#if 0				/* now done in ANY_MODULE */
	case LEX_OBJFILE_WRITE_ERROR:
		/*return("%d writing to object file"); */
		return ("Cannot write to object file: error code %d");
#endif
		/* No longer relevant cos there's now no hard limit
		   case LEX_TOO_MANY_ENTRYPOINTS:
		   return("Too many entry points in file \"%s\"");
		 */
	case LEX_BAD_DESC_FORMAT:
		return ("bad object file format");
	case LEX_LIBRARY_NOT_SC:
		return ("\"%s\" is a library, not an SC");
	case LEX_HASHWORD_ERROR:
		return ("badly formed compiler directive");
		/*case LEX_TCOFF_IOERR :
		   return ("TCOFF IO ERROR: 64 bit number!"); */
		/*case LEX_TCOFF_BAD_LCHAR :
		   return ("Characters 'l%c' unknown in record string"); */
		/*case LEX_TCOFF_BAD_CHAR :
		   return ("Character %c unknown in record string"); */
	case LEX_TCOFF_BAD_DESC:
		return ("peculiar descriptor : \"%s\"");
		/*
		   case LEX_TCOFF_BAD_PROCESSOR :
		   return ("Unknown processor type in object file");
		   case LEX_TCOFF_BAD_MODE :
		   return ("Unknown error mode in object file");
		 */
	case LEX_TCOFF_UNMATCHED_ENDMODULE:
		return ("unmatched END_MODULE");
		/*case LEX_TCOFF_DESC_OVERFLOW :
		   return ("Descriptor overflow"); */
		/*#ifdef CONFIG */
	case LEX_TCOFF_NOT_LINKED:
		return ("\"%s\" is not a linked object file");
		/*#else */
	case LEX_TCOFF_WRONG_FORMAT:
		return ("\"%s\" is not a valid object file");
		/*#endif */
	case LEX_NAME_TRUNCATED:
		return ("name too long - maximum length is %d characters");
	case LEX_NUMBER_TRUNCATED:
		return ("number too long - maximum length is %d characters");
		/*case LEX_LINE_TRUNCATED:
		   return("Line too long - maximum length is %d characters"); */
	case LEX_NO_SAVE_STATE:
		return ("cannot restore lexer state -- non saved");
	case LEX_SAVE_STATE_ERROR:
		return ("error restoring lexer state");
	case LEX_PP_BAD_DEFINE:
		return ("malformed #DEFINE directive");
	case LEX_PP_ILLEGAL_NAME:
		return ("illegal name in pre-processor directive");
	case LEX_PP_BAD_UNDEF:
		return ("malformed #UNDEF directive");
	case LEX_PP_BAD_IF:
		return ("malformed #IF directive");
	case LEX_PP_BAD_ENDIF:
		return ("malformed #ENDIF directive");
	case LEX_PP_BAD_ELSE:
		return ("malformed #ELSE directive");
	case LEX_PP_BAD_ELIF:
		return ("malformed #ELIF directive");
	case LEX_PP_UNBALANCED_ENDIF:
		return ("unbalanced #ENDIF directive");
	case LEX_PP_UNBALANCED_ELSE:
		return ("unbalanced #ELSE directive");
	case LEX_PP_UNBALANCED_ELIF:
		return ("unbalanced #ELIF directive");
	case LEX_PP_UNBALANCED_IF:
		return ("unbalanced #IF directive");
	case LEX_PP_NESTED_IF_OUTDENTS:
		return ("nested #IF directive is outdented");
	case LEX_PP_IF_INDENT:
		return ("indentation of %s does not match #IF");
	case LEX_PP_USER_WARNING:
		return ("user warning: %s");
	case LEX_PP_USER_ERROR:
		return ("user error: %s");
	case LEX_PP_EXPECTED_RPAREN:
		return ("expected `)\'");
	case LEX_PP_GARBAGE:
		return ("garbage found at end of expression");
	case LEX_PP_BADCMDLINEDEF:
		return ("bad command-line define: %s");
	case LEX_PP_LOOSE_RELAX:
		return ("RELAX not within an IF");
	case LEX_PP_OVER_RELAX:
		return ("RELAXed too many times");
	case LEX_PP_RELAX_ERROR:
		return ("RELAX error");
	case LEX_PP_NOSUCHNAME:
		return ("name %s is not defined");
	case LEX_PP_NOSUCHVAL:
		return ("name %s has no value");
	case LEX_PP_BAD_COMPARE:
		return ("bad types in comparison");
		/*}}} */
		/*{{{  SYN errors */
	case SYN_TOO_MANY_NAMES:
		return ("one name only allowed in specification or retype");
	case SYN_PROTOCOL_NOT_DECLARED:
		return ("protocol is not declared");
		/*case SYN_ILLEGAL_CONVERSION:
		   return ("Illegal conversion"); */
	case SYN_INVALID_PROCESS:
		return ("invalid process");
	case SYN_BAD_INDENT:
		return ("incorrect indentation");
		/*case SYN_ILLEGAL_TAG:
		   return ("Illegal treenode tag"); */
	case SYN_ILLEGAL_DECL_TYPE:
		return ("illegal type %s for declaration");
	case SYN_LIST_IN_IO:
		return ("only one element allowed on left-hand side of input or output");
	case SYN_SC_ERROR:
		return ("item not allowed at outermost level of compilation unit");
	case SYN_STRING_TOO_LONG:
		return ("string is too long for buffer (max %d bytes)");
	case SYN_FILE_OPEN_ERROR:
		return ("cannot open file \"%s\"");
	case SYN_MISSING_FILENAME:
		return ("missing filename");
	case SYN_BAD_GUYLINE:
		return ("badly formed code insert line");
	case SYN_GUY_NOT_ENABLED:
		return ("code insertion is not enabled");
	case SYN_GUY_IS_OBSOLETE:
		return "implementation restriction: GUY is no longer supported: use ASM";
	case SYN_ASM_DISALLOWED:
		return "inline ASM is not permitted";
	case SYN_OPTION_IN_BAD_POS:
		return ("option in illegal position");
	case SYN_BAD_OPTION:
		return ("unrecognised option \"%c\" in option string");
#if 0
	case SYN_E_HCOMMENT:
		return ("Expected string after #COMMENT");
	case SYN_E_OPTION:
		return ("Expected string after #OPTION");
#endif
	case SYN_E_STRING_AFTER:
		return "expected string after %s";
		/*case SYN_PROG_ERROR:
		   return ("Item not allowed in PROGRAM unit"); */
	case SYN_BAD_DESCRIPTOR:
		return ("descriptor has incorrect format");
	case SYN_SC_EMPTY:
		return ("no PROC or FUNCTION declared in SC module");
	case SYN_BAD_PRAGMA_NAME:
		return ("unknown #PRAGMA name: %s; ignored");
		/*case SYN_BAD_PRAGMA_POSN:
		   return ("#PRAGMA not allowed in this position"); */
	case SYN_BAD_PRAGMA_DIRECTIVE:
		return ("badly formed #PRAGMA %s directive");
	case SYN_STDLIB_HAS_VS:
		return ("standard library routine %s requires vectorspace");
		/*case SYN_DUPLICATE_IN_LIB :
		   return ("Routine %s imported by multiple #USEs"); */
	case SYN_NOTHING_IN_LIB:
		return ("no compatible entrypoints found in \"%s\"");
#if 0
	case SYN_TRANSLATE_NUL:
		/*return ("TRANSLATE ignored: String for %s contains NUL character"); */
		return ("TRANSLATE ignored: String for %s contains NUL character");
#endif
	case SYN_TRANSLATE_ILLEGAL_CHAR:
		return ("TRANSLATE ignored: String for %s contains illegal character: %s");
	case SYN_TRANSLATE_DUPLICATE_IN:
		return ("TRANSLATE ignored: Name %s has already been used");
	case SYN_TRANSLATE_DUPLICATE_EX:
		return ("TRANSLATE ignored: String \"%s\" has already been used");
	case SYN_TRANSLATE_SEQUENCE:
		return ("TRANSLATE ignored: Module containing \"%s\" has already been loaded");
	case SYN_SC_IS_OBSOLETE:
		return ("#SC \"%s\" is obsolete; use #USE instead");
	case SYN_DIFFERENT_DESCRIPTORS:
		return ("differing definitions of %s in library; check formal parameters and channel usage");
	case SYN_PLACE_NAME:
		return ("you may only PLACE a name");
	case SYN_SKIPPING_DEFN:
		return ("skipping over %s definition");
	case SYN_PREDEF_ERROR:
		return ("predefine error");	/* internal error */
#ifdef USER_DEFINED_OPERATORS
	case SYN_UDO_NOT_SUPPORTED:
		return ("a binary operator here");
	case SYN_UDO_MULTI_RET_PARAMS:
		return ("operator overloading function cannot have multiple return parameters");
	case SYN_UDO_NO_RET_PARAM:
		return ("operator overloading function must have one return parameter");
	case SYN_UDO_TWO_PARAMS:
		return ("two parameters for binary operator function");
	case SYN_UDO_ONE_PARAM:
		return ("one parameter for unary operator function");
	case SYN_UDO_ONE_OR_TWO_PARAMS:
		return ("either one or two parameters for operator function");
#endif
#ifdef MOBILES
	case SYN_NO_MOBILES:
		return ("MOBILE support not enabled");
	case SYN_NO_MPROCS:
		return ("MOBILE PROC support not enabled");
	case SYN_MOBILE_SUBTYPE:
		return ("unsupported MOBILE sub-typing");
#endif
#ifdef INITIAL_DECL
	case SYN_INITDECL_ADIM_MISSING:
		return ("open arrays not supported in INIITAL declarations");
#endif
		/*}}} */
		/*{{{  expected "%s" found "%s" messages */
/*case SYN_E_TOKEN:  abort(); *//* this is processed externally */
	case SYN_E_EXPR_OR_SPEC:
		return "expression or specification";
	case SYN_E_GUARD_OR_SPEC:
		return "guard or specification";
		/*case SYN_E_ELEMENT_OR_SPEC: return "element or specification"; */
	case SYN_E_SPEC:
		return "specification";
	case SYN_E_PTYPE:
		return "primitive type";
	case SYN_E_SPECIFIER:
		return "specifier";
	case SYN_E_ELEMENT:
		return "element";
	case SYN_E_OPERAND:
		return "operand";
	case SYN_E_LPAREN:
		return "\'(\'";
	case SYN_E_RPAREN:
		return "\')\'";
		/*case SYN_E_INTTYPE:         return "integer type"; */
	case SYN_E_EXPR:
		return "expression";
	case SYN_E_IS_OR_RETYPES:
		return "IS or RETYPES";
	case SYN_E_ACTION:
		if (extended_input) {
			return "\"?\", \"??\", \"!\" or \":=\"";
		} else {
			return "\"?\", \"!\" or \":=\"";
		}
	case SYN_E_PAR_OR_ALT:
		return "PAR or ALT";
	case SYN_E_RBOX_OR_FROM:
		return "\"]\" or FROM";
	case SYN_E_AT_OR_IN:
		return "AT or IN";
	case SYN_E_NAME:
		return "name";
	case SYN_E_NEWLINE:
		return "new line";
	case SYN_E_COLON:
		return "\":\"";
	case SYN_E_COLON_IS_RETYPES:
		return "\":\", IS or RETYPES";
	case SYN_E_COLON_IS:
		return "\":\" or IS";
	case SYN_E_WS_OR_VS:
		return "WORKSPACE or VECSPACE";
	case SYN_E_TYPE:
		return "TYPE";
	case SYN_E_BOX:
		return "\"[]\"";
		/*case SYN_E_PROCESSOR_TYPE:  return "processor type"; */
#if 0
	case SYN_E_LINK_DEL:
		return "INTERVAL or DELETING";
#endif
		/*}}} */
		/*{{{  blocking system-call workspace minimum requirements */
	case SYN_BSYSCALL_NOTMINWS2:
		return "blocking sys-call \"%s\" needs more workspace, increasing to 2";
	case SYN_BSYSCALL_NOTMINWS3:
		return "blocking sys-call \"%s\" needs more workspace, increasing to 3";
		/*}}}*/
		/*{{{  channel direction specifier and chan-type errors*/
	case SYN_BAD_CHAN_CONFLICT:
		return "incompatible channel direction specifier on `%s\'";
	case SYN_E_CHANTYPE:
		return "chan-type";
	case SYN_E_FORK_PROCCALL:
		return "procedure call";
	case SYN_E_FORK_PROCPARAMS:
		return "procedure parameters";
	case SYN_E_CHANTYPEDIRSPEC:
		return "chan-type direction specifier";
		/*}}}*/
		/*{{{  frmb: other stuff*/
	case SYN_E_DYNCALL_PROCCALL:
		return "procedure call";
	case SYN_E_DYNCALL_PROCPARAMS:
		return "procedure parameters";
	case SYN_E_TYPEHASH:
		return "type-hash";
	case SYN_REC_TYPE_NOT_MOBILE:
		return "recursive channel-type must be MOBILE";
	case SYN_UNEXPECTED_CHANDIR:
		return "unexpected direction specifier";
	case SYN_EXTENDS_NOT_TAGGED:
		return "can only extend tagged protocols";
	case SYN_EXPECTED_CHAN:
		return "expected CHAN, found %s";
	case SYN_BAD_TRACE:
		return "bad trace definition";
	case SYN_E_TRACEDEF:
		return "trace definition";
	case SYN_NONCONST_TAGVAL:
		return "tag value not constant";
	case SYN_MISPLACED_FIXED:
		return "FIXED cannot be specified here";
		/*}}}*/
	default:
		return NULL;
	}
}

/*}}}*/
/*{{{  PUBLIC const char *chkmessagestring*/
PUBLIC const char *chkmessagestring (const int n)
{
	switch (n) {
		/*{{{  cases */
		/*case CHK_UNKNOWN_TYPE:
		   return ("Unknown type %s"); */
	case CHK_TYPES_DIFF:
		return ("operands have different types for operator %s");
	case CHK_INVTYPE:
		return ("type of operand invalid for operator %s");
	case CHK_CONSTRUCTOR_TYPE_MISMATCH:
		return ("table elements are of different types");
	case CHK_INVCOUNT:
		return ("shift count must be of type INT");
	case CHK_INV_EXACT:
		return ("type of operand invalid for exact type conversion %s");
	case CHK_INV_ROUND_TRUNC:
		return ("type of operand invalid for \'ROUND\' or \'TRUNC\'");
	case CHK_ASS_UNBALANCED:
		return ("number of variables does not match number of expressions");
	case CHK_INV_SUBSCRIPT:
		return ("%s subscripted too many times");
	case CHK_INVTYPE_MULTI_ASS:
		return ("variable type does not match assigned expression %s");
		/*case CHK_INVTYPE_SINGLE_ASS:
   return ("Variable type does not match assigned expression%s"); *//* expects NUL string */
	case CHK_ABBR_TYPE_MISMATCH:
		return ("expression type does not match abbreviation specifier");
		/*case CHK_UNKNOWN_NODE:
		   return ("Unknown node tag %d encountered"); */
	case CHK_INVTYPE_FRESULT:
		return ("expression %s does not match type of corresponding function result");
	case CHK_TOOMANYEXPS:
		return ("function result list contains too many expressions");
	case CHK_TOOFEWEXPS:
		return ("function result list contains too few expressions");
	case CHK_RETYPE_TYPE_MISMATCH:
		return ("size of RETYPE does not match the expression size");
	case CHK_INV_FUNC_LIST:
		return ("multi-valued function may not be used as expression operand");
	case CHK_INV_VALOF_LIST:
		return ("multi-valued valof may not be used as expression operand");
	case CHK_ADIM_NOT_CONST:
		return ("array dimension is not a constant expression");
	case CHK_ADIM_NOT_INT:
		return ("array dimension is not of type INT");
	case CHK_INV_PROT:
		return ("invalid channel protocol");
		/*case CHK_INVTYPETAG:
		   return ("Invalid type tag %d"); */
	case CHK_TOO_FEW_VARS:
		return ("there are more expressions than variables in multiple assignment");
	case CHK_TOO_MANY_VARS:
		return ("there are more variables than expressions in multiple assignment");
		/*case CHK_INV_SIZE_OPERAND:
		   return ("\'SIZE\' operand has invalid type"); */
	case CHK_CFOLD_OFLOW:
		return ("overflow when evaluating constant expression");
		/*case CHK_NAMENOTCONST:
		   return ("Constant expression contains non-constant name"); */
	case CHK_NAMENOTARRAY:
		/*return ("Subscripted operand is not an array"); */
		return ("item may not be subscripted");
		/*case CHK_SUBSCRIPT_NOTCONST:
		   return ("Constant expression contains non-constant subscript"); */
	case CHK_SUBSCRIPT_OUT_OF_RANGE:
		return ("subscript out of range");
	case CHK_INV_DECL_TAG:
		return ("invalid tag %s in decl node");
	case CHK_ADIM_NEGATIVE:
		return ("array dimension must be greater than zero");
	case CHK_ADIM_TOOBIG:
		return ("array dimension is too large");
		/*case CHK_INV_CONSTRUCT:
		   return ("Invalid construct tag %s"); */
		/*case CHK_INV_LIST:
		   return ("Invalid list tag %d"); */
	case CHK_INV_SEGMENT_OPERAND:
		return ("segmented element is not an array");
		/*case CHK_SLENGTH_NOT_CONST:
		   return ("Segment length not constant"); */
	case CHK_ADIM_MISSING:
		return ("array type must have dimension specified");
	case CHK_INVTYPE_SPEC:
		return ("illegal specifier");
		/*case CHK_INV_EXP:
		   return ("Invalid expression %d"); */
	case CHK_INVTYPE_CONSTEXP:
		return ("constant expression has invalid type %d");
	case CHK_EXP_NOT_CONST:
		return ("expression is not constant");
	case CHK_INVSIZE_CONST:
		return ("constant has invalid size");
	case CHK_TOO_MANY_ELSES:
		return ("more than one ELSE in CASE process");
	case CHK_MULTIPLE_CASE:
		return ("option is used more than once in this CASE");
	case CHK_INVTYPE_SELECTOR:
		return ("invalid selector type");
	case CHK_SELN_NOTCONST:
		return ("option must be a constant expression");
	case CHK_INVTYPE_SELN:
		return ("type mismatch for CASE option");
		/*case CHK_UNRESOLVED_INTTYPE:
		   return ("Integer number value must have a type specifier"); */
#ifndef OCCAM2_5
	case CHK_UNRESOLVED_REALTYPE:
		return ("real number value must have a type specifier");
#endif
	case CHK_TYPE_MISMATCH:
		return ("type mismatch");
	case CHK_TYPE_MISMATCH_EXPECTED:
		return "type mismatch: expected %s";
	case CHK_ASUB_NOT_INT:
		return ("array index expression must be of type INT");
	case CHK_FN_VAR_PARAM:
		return ("formal parameter to function must be VAL");
	case CHK_PTYPE_MISMATCH:
		return ("I/O list item %d does not match protocol");
	case CHK_TOO_FEW_PITEMS:
		return ("not enough items in i/o list");
	case CHK_TOO_MANY_PITEMS:
		return ("too many items in i/o list");
	case CHK_BAD_PTAG:
		return ("illegal tag in i/o list");
	case CHK_BAD_ANY_PROT:
		return ("cannot send zero length segment on CHAN OF ANY");
	case CHK_INVTYPE_STARTEXP:
		return ("replicator start expression not of type INT");
	case CHK_INVTYPE_LENGTHEXP:
		return ("replicator count expression not of type INT");
	case CHK_INVTYPE_STEPEXP:
		return ("replicator step expression not of type INT");
	case CHK_REPL_NOT_CONST:
		return ("implementation restriction: PAR replicator must have a constant count value");
	case CHK_REPL_NEGATIVE:
		return ("negative replicator counts are invalid");
	case CHK_REPL_OVERFLOW:
		return "replicator variable overflows";
	case CHK_INVTYPE_GUARD:
		return ("guard must be of type BOOL");
	case CHK_INV_VAL_OCCAM:
		return ("cannot use VAL on a CHAN, PORT, or TIMER");
	case CHK_NAME_NOT_PROC:
		return ("%s is not a procedure");
	case CHK_INVVARIABLE_PARAM:
		return ("parameter %d is not a variable");
	case CHK_INVTYPE_PARAM:
		return ("type mismatch for parameter %d in call of %s");
	case CHK_TOO_FEW_ACTUALS:
		return ("not enough actual parameters in call of %s");
	case CHK_TOO_MANY_ACTUALS:
		return ("too many actual parameters in call of %s");
	case CHK_INV_CINDEX:
		return ("invalid subscript into table");
	case CHK_NOT_CHANNEL:
		return ("left-hand side must be of type CHAN or PORT");
	case CHK_INV_TIMER_INPUT:
		return ("invalid timer input");
	case CHK_TIME_TYPE_MISMATCH:
		return ("time expression must be of type INT");
	case CHK_NOT_CHANNEL_OR_TIMER:
		return ("left-hand side must be of type CHAN, PORT, or TIMER");
	case CHK_NOT_TIMER:
		return ("left-hand side must be of type TIMER");
	case CHK_INV_PORT_PROT:
		return ("invalid port protocol");
	case CHK_INDISTINCT_TAG:
		return ("tag name %s conflicts with another tag name");
	case CHK_NOT_A_TAG:
		return ("I/O list does not begin with a tag");
	case CHK_LOST_VARIANT:
		return ("cannot find variant");
	case CHK_NAME_NOT_FUNCTION:
		return ("%s is not a function");
	case CHK_LOST_VALOF:
		return ("lost valof, found %d");
	case CHK_VALOF_SCOPE:
		return ("side-effect: %s is not assignable within VALOF");
	case CHK_BAD_VALOF:
		return ("%s is not allowed within a VALOF");
	case CHK_VALOF_CALL_SCOPE:
		return ("side-effect: instanced PROC assigns to free variable %s");
	case CHK_PLACE_IN_VALOF:
		return ("possible side-effect: PLACED variable %s");
	case CHK_PLACE_IN_VALOF_CALL:
		return ("possible side-effect: instanced PROC has PLACED variable %s");
	case CHK_SUBSCRIPT_RANGE:
		return ("subscript value %d is out of range");
	case CHK_SEG_START_RANGE:
		return ("segment start value %d is out of range");
	case CHK_SEG_LENGTH_RANGE:
		return ("segment length %d is longer than array");
	case CHK_SEG_RANGE:
		return ("segment end value %d is out of range");
		/*case CHK_CONSTANT_SEGMENT_ERROR:
		   return ("Error in constant segment folding, tag = %d,"); */
	case CHK_BAD_PLACE_NAME:
		return ("%s may not be placed");
	case CHK_INVTYPE_PLACEMENT:
		return ("placement address must be of type INT");
	case CHK_INV_VAR_PLACEMENT:
		return ("cannot place this type");
	case CHK_INV_CHAN_PLACEMENT:
		return ("placement of channels is disabled");
	case CHK_TOO_MANY_MULTIVALUES:
		return ("only one expression list allowed on right-hand side of assignment");
	case CHK_NAME_NOT_DECLARED:
		return ("%s is not declared");
		/*case CHK_TAG_CONFLICT:
		   return ("Tag name %s conflicts with protocol name"); */
#ifndef OCCAM2_5
	case CHK_BAD_FUNCTION_TYPE:
		return ("function types must be scalar");
	case CHK_BAD_VALOF_TYPE:
		return ("VALOF RESULT types must be scalar");
#endif
	case CHK_BAD_INPUT_PROTOCOL:
		return ("simple input is not allowed on a channel with a tagged protocol");
	case CHK_BAD_CASE_INPUT_PROTOCOL:
	case CHK_BAD_TAGGED_INPUT_PROTOCOL:
		return ("case input is not allowed on a channel without a tagged protocol");
	case CHK_ALT_INPUT_PORT:
		return ("PORT input is not allowed in an alternative");
	case CHK_BAD_WSPLACE:
		return ("only vector declarations may be placed");
	case CHK_NAME_MULTIPLY_PLACED:
		return ("%s has already been placed");
	case CHK_MULTIPLE_VARIANT:
		return ("tag %s is used more than once in case input");
	case CHK_TOO_MANY_TAGS:
		return ("too many tags in protocol definition - %d allowed");
	case CHK_BAD_DEST:
		return ("cannot write to %s");
		/*case CHK_BAD_COUNT:
		   return ("counted array protocol not allowed on anarchic channel"); */
	case CHK_BAD_COLON2:
		return "right hand part of counted array communication is not an array";
	case CHK_COUNT_OUT_OF_RANGE:
		return ("count is out of range for I/O list item %d");
		/*case CHK_ENTRYPOINT_NOT_FOUND:
		   return("Cannot find entry point %s in entry point list"); */
		/*case CHK_NAME_STACK_OVERFLOW:
		   return("Implementation limit: maximum of %d names in scope"); */
	case CHK_UNIMPLEMENTED_ASMCODE:
		return ("implementation restriction: Instruction %s not available");
	case CHK_BAD_GUY_OPERAND:
		return ("instruction operand must be a scalar constant expression or a name");
	case CHK_DISABLED_GUYCODE:
		return ("instruction %s is not available in current code insertion mode");
	case CHK_INVALID_GUYCODE:
		return ("instruction %s is not available on target processor");
	case CHK_BAD_GUYCODE:
		return ("instruction %s does not exist");
	case CHK_BAD_STEPCODE:
		return ("STEP instruction must be a secondary");
	case CHK_ASM_BAD_STORE:
		return ("operand to a store must be an element");
	case CHK_ASM_TOO_BIG:
		return ("operand does not fit into a word");
	case CHK_MISSING_OPERAND:
		/*return("Instruction %s takes an operand"); */
		return ("instruction %s has too few operands");
	case CHK_EXTRANEOUS_OPERAND:
		/*return("Instruction %s does not take an operand"); */
		return ("instruction %s has too many operands");
	case CHK_BAD_VALOF_CALL:
		return ("instanced PROC contains %s, not allowed within a VALOF");
	case CHK_INVTYPE_PROCEXP:
		return ("processor expression must be of type %s");
#if 0				/* #ifndef CONFIG */
	case CHK_INVPROCTYPE:
		return ("Unrecognised processor type `%s'\n");
#endif
	case CHK_NESTED_PRI_PAR:
		return ("PRI PAR is nested inside another PRI PAR");
	case CHK_NESTED_PRI_PROC:
		return ("%s contains a nested PRI PAR");
	case CHK_INV_PRI_PAR:
		return ("implementation restriction: A PRI PAR must have two component processes");
	case CHK_NOT_IMPLEMENTED:
		return ("implementation restriction: %s not implemented");
	case CHK_ASS_NAME_USED_MULTIPLY:
		return ("%s appears more than once on left-hand side of assignment");
#ifdef CONDEXP
	case CHK_CONDEXP_NOT_BOOL:
		return ("condition in expression must be of type BOOL");
#endif
	case CHK_INV_ABBR:
		return ("right hand side of abbreviation must be assignable");
	case CHK_INV_RETYPE:
		/*return ("Retyping an item of type CHAN, PORT, or TIMER is not allowed"); */
		/*return ("Retyping an item of type TIMER is not allowed"); */
		return ("you may only RETYPE data items");
	case CHK_NAME_NOT_USED:
		return ("%s is not used");
	case CHK_PARAM_NOT_USED:
		return ("parameter %s is not used");
	case CHK_ROUTINE_NOT_USED:
		return ("routine %s is not used");
	case CHK_BAD_VALRETYPE:
		return ("RETYPE cannot be VAL for CHAN, PORT or TIMER");
	case CHK_BAD_CHANRETYPE:
		/*return ("CHAN, PORT or TIMER may only be RETYPEd to CHAN etc"); */
		return ("RETYPE of CHAN or PORT must be VAL");
	case CHK_RETYPE_NOTYPE:
		return ("RETYPE must have a type specifier");
	case CHK_BAD_DEST_CONSTRUCTOR:
		return ("cannot write to table");
	case CHK_BAD_DEST_STRING:
		return ("cannot write to string");
	case CHK_INV_SHIFT:
		return ("shift count is out of range");
	case CHK_INV_MULTIASSIGN:
		return ("implementation restriction: Arrays may not be unknown size");
		/*case CHK_LABEL_INSIDE_INLINE:
		   return ("Implementation restriction: No labels inside INLINE routines"); */
	case CHK_NAME_IS_DESCOPED:
		return ("name %s descopes a previous declaration");
		/*case CHK_ARRAY_SIZE_OVERFLOW:
		   return ("Implementation restriction: Array size is too large"); */
	case CHK_RSUB_NOT_FIELD:
#if 0				/*def CONFIG */
		return ("Expected a NODE attribute, found a subscript expression");
		/*return ("Subscript expression is not a NODE attribute"); */
#else
		return ("expected a field name of %s, found a subscript expression");
		/*return ("Subscript expression is not a field name of %s"); */
#endif
	case CHK_LONELY_FIELD:
		/*return ("Field selector %s may not appear on its own"); */
		return ("attribute %s may not appear on its own");
	case CHK_LIB_WRONG_TARGET:
		return ("%s %s is not compiled for a compatible processor type or mode");
	case CHK_LIB_WRONG_IO:
		return ("%s %s is not compiled with library i/o");
	case CHK_LIB_WRONG_IO_CONFIG:
		return
			("%s %s is not compiled with library i/o; this is required unless both interactive debugging and virtual routing are disabled");
	case CHK_INV_ASSERT:
		return ("parameter to ASSERT is FALSE");
	case CHK_DUPLICATE_LABEL:
		return "label `%s' multiply defined in this routine";
	case CHK_BAD_PRAGMA_SHARED_NAME:
		/*return "%s may not be shared"; */
		return "may not apply #PRAGMA %s to `%s'";
	case CHK_NONLOCAL_LABEL:
		/*return "Access to labels at different lexical level is not permitted"; */
		return "access to non-local labels is not permitted";
	case CHK_CHAN_OF_ANY_USED:
		/*return "CHAN OF ANY is obsolete - use PROTOCOL name IS ANY for greater security"; */
		return "CHAN OF ANY is obsolete: use PROTOCOL name IS ANY";
	case CHK_CHAN_OF_ANY_PARAM_PASSING:
		return "obsolete channel type conversion: use channel RETYPE";
	case CHK_OUTERLEVEL_CONSTANT:
		return "implementation restriction: Expression for outermost level VAL must be constant";
	case CHK_COLON2_INPUT:
		/*  return ("Cannot use length `%s' in array part of counted array input"); */
		return ("Using length `%s' in array part of counted array input is obsolete");
	case CHK_BAD_ASM_INLINE_OP:
		/*return ("Cannot access formal parameter `%s' inside INLINE routine"); */
		return ("cannot use formal parameter `%s' as operand to primary instruction inside INLINE routine");
	case CHK_WARN_GUY:
		return "GUY construct is no longer supported. You should use the ASM construct";
	case CHK_MISSING_TAG_IN_CASE_INPUT:
		return "tag %s is not handled in CASE input";
	case CHK_NON_EXHAUSTIVE_CASE_INPUT:
		return "CASE input does not handle all tags";
	case CHK_NOT_ADJACENT_NAME:
		return "PRAGMA or PLACEment must immediately follow declaration of %s";
		/*case CHK_SEGMENT_CONSTRUCTOR:
		   return "Implementation restriction: Cannot take a segment of a constructor"; */
	case CHK_UNPLACED_PORT:
		return "PORT %s must be PLACED";
	case CHK_INV_TYPE_DECORATION:
		return "invalid type for literal";
	case CHK_TAG_IN_EXPRESSION:
		return "illegal use of PROTOCOL tag \"%s\" in an expression";
	case CHK_RETYPINGREAL64:
		return "retyping from/to REAL64 when words are swapped";
		/*}}} */
		/*{{{  occam 2.5 errors */
#ifdef OCCAM2_5
	case CHK_BAD_USERTYPE:
		return "invalid type for type declaration";
	case CHK_ARRAY_CONVERSION:
		return "cannot convert to an array type";
	case CHK_ILLEGAL_FIELD_DECL:
		return "illegal field item declared inside RECORD";
	case CHK_DUPLICATE_FIELD_DECL:
		return "duplicate field name `%s'";
	case CHK_UNALIGNED_PACKED_FIELD:
		return "implementation restriction: Field `%s' must be aligned to a multiple of %s bytes for a PACKED RECORD";
	case CHK_UNALIGNED_PACKED_RECORD:
		return "implementation restriction: PACKED RECORD `%s' must be an integral number of words long";
	case CHK_BAD_CONSTRUCTOR_TYPE:
		return "invalid type decoration for table";
	case CHK_REC_LIT_TYPE_MISMATCH:
		return "incorrect type for field `%s' in record literal";
	case CHK_REC_LIT_TOO_SMALL:
		return "too few items in record literal";
	case CHK_REC_LIT_TOO_LARGE:
		return "too many items in record literal";
	case CHK_OFFSETOF_NOT_RECORD:
		return "%s is not a RECORD type name";
	case CHK_OFFSETOF_NOT_FIELD:
		return "%s is not a field of %s";
	case CHK_BAD_RESHAPES:
		return "can only RESHAPE from an array to an array of similar base type";
	case CHK_BAD_TYPENAME:
		return "name %s is not a TYPE";
	case CHK_NO_RECF_ZERO:
		return ("the number of RECORD fields must be greater than zero");
	case CHK_BAD_STRING_TYPE:
		return "invalid type decoration for string";
#endif
		/*}}} */
#if 1				/*def CONFIG */
		/*{{{  Configurer checker errors */
	case CHK_ILLEGAL_CONFIG_CONSTRUCT:
		return ("illegal item in configuration code");
	case CHK_ILLEGAL_CONSTRUCT:
		return ("illegal item inside %s construct");
	case CHK_DUPLICATE_CONSTRUCT:
		return ("multiple %s constructs not permitted");
	case CHK_ZERO_CONSTRUCT:
		return ("no %s construct");

#if 0
	case CHK_INVTYPE_EDGE:
		return ("CONNECT expression must be of type %s");
	case CHK_INVTYPE_ARC:
		return ("WITH expression must be of type %s");
	case CHK_INVTYPE_DEVICE:
		return ("SET expression must be of type %s");
#else
	case CHK_INVTYPE_CONFIG:
		return ("%s expression must be of type %s");
#endif

	case CHK_INVTYPE_ATTR:
		return ("unknown attribute name \"%s\"");
	case CHK_INV_MAPPING_RHS:
		return ("right hand side of mapping must be of type NODE or ARC");
	case CHK_INV_MAPPING_LHS:
		return ("left hand side of mapping must be of type %s");
	case CHK_INV_MAPPING_PRI:
		return ("priority expression of mapping must be of type INT");
	case CHK_INV_MAPPING_NOPRI:
		return ("no priority expression permitted when mapping CHANs");
	case CHK_INV_PLACEON_RHS:
		return ("right hand side of mapping must be of type ARC");
	case CHK_ILLEGAL_PLACE:
		return ("cannot PLACE %s which was declared outside a PROCESSOR construct");
	case CHK_CALLING_CONVENTION:
		return ("FUNCTION %s returns a REAL result but is compiled for wrong calling convention");
	case CHK_FIELD_NOT_VALID:
		return "attribute %s is not available for a %s";
	case CHK_FIELD_NOT_SET:
		return "attribute %s may not be SET";
	case CHK_INV_VAL_NDL:
		return ("cannot use VAL on a hardware item");
	case CHK_INV_VAL_CONFIG:
		return ("cannot use VAL on a CHAN, PORT, TIMER, or hardware item");
	case CHK_INV_RHS_NDL_MAPPING:
		return ("right hand side of mapping must be of type EDGE");
	case CHK_INV_LHS_NDL_MAPPING:
		return ("left hand side of mapping must be of type EDGE");
	case CHK_NDL_MAPPING_TOO_MANY_SOURCES:
		return ("map statement must have only one source");
#ifdef USER_DEFINED_OPERATORS
	case CHK_UDO_NOT_SUPPORTED:
		return ("overloading on this operator is not currently supported");
#endif
#if 0
	case CHK_INV_CONFIG:
		return ("Invalid configuration");
	case CHK_BAD_NETDECL:
		return ("Invalid declaration in network description");
	case CHK_BAD_ATTRIBUTE:
		return ("Invalid %s attribute");
	case CHK_MISSING_ATTRIBUTE:
		return ("Missing %s attribute");
	case CHK_TOOMANY_ATTRIBUTES:
		return ("Too many attributes for node");
	case CHK_MISSING_INIT:
		return ("Declaration must have initialisation");
#endif
#ifdef MOBILES
	case CHK_PROMOTE_MOBILE:
		return ("type containing MOBILEs not MOBILE");
	case CHK_DYN_DIM_NOT_INT:
		return ("array dimension of dynamic MOBILE array is not an integer");
	case CHK_INNER_MOBILE:
		return ("nested MOBILE types not supported");
	case CHK_VAL_MOBILE:
		return ("VAL parameters may not be MOBILE");
	case CHK_MOBILE_RETYPE:
		return ("retyping to MOBILE not permitted");
	case CHK_MOBILE_RESHAPE:
		return ("reshaping to MOBILE not permitted");
	case CHK_MOBILE_IO:
		return ("I/O item %d must be MOBILE");
	case CHK_MOBILE_REQUIRED:
		return ("MOBILE variable required");
#endif	/* MOBILES */
	case CHK_BAD_LIST_LENGTHS:
		return ("lists are different lengths");

		/*}}} */
#endif
		/*{{{  array-constructor errors*/
	case CHK_UNKNOWN_CONSTRUCTOR_SIZE:
		return ("length of array constructor is unknown");
		/*}}}*/
		/*{{{  RESULT parameter errors*/
	case CHK_INV_RESULT_OCCAM:
		return ("cannot use RESULT on a CHAN, PORT, or TIMER");
	case CHK_INV_RESULT_NDL:
		return ("cannot use RESULT on a hardware item");
	case CHK_INV_RESULT_CONFIG:
		return ("cannot use RESULT on a CHAN, PORT, TIMER or hardware item");
		/*}}}*/
		/*{{{  modified SKIP/ALT handling errors*/
	case CHK_NO_SKIP_PRECOND:
		return ("SKIP guard without pre-condition");
	case CHK_SKIP_NOT_LAST_PRI:
		return ("SKIP guard not last in PRI ALT");
	case CHK_SKIP_IN_ALT:
		return ("SKIP guard used in ALT");
		/*}}}*/
		/*{{{  empty process warnings*/
	case CHK_EMPTY_SEQ:
		return ("empty SEQ process?  Equivalent to SKIP ...");
	case CHK_EMPTY_PAR:
		return ("empty PAR process?  Equivalent to SKIP ...");
	case CHK_EMPTY_IF:
		return ("empty IF process?  Equivalent to STOP ...");
	case CHK_EMPTY_ALT:
		return ("empty ALT process?  Equivalent to STOP ...");
		/*}}}*/
		/*{{{  channel direction specifier and chan-type errors*/
	case CHK_BAD_CHAN_CONFLICT:
		return ("incompatible channel direction specifier on `%s\'");
	case CHK_CONSTRUCTOR_BAD_CHANDIR:
		return ("mixed channel-ends in constructor");
	case CHK_BAD_CHAN_CONFLICT_ANON:
		return ("incompatible channel direction specifier(s)");
	case CHK_DECODE_BAD_CHANDIR:
		return ("incompatible channel direction specifier on parameter %d of DECODE.CHANNEL");
	case CHK_ENCODE_BAD_CHANDIR:
		return ("incompatible channel direction specifier on parameter %d of ENCODE.CHANNEL");
	case CHK_NO_CHANDIR_SPEC:
		return ("must specify channel direction on `%s\'");
	case CHK_NO_CHANDIR_SPEC_ANON:
		return ("missing channel direction specifier");
	case CHK_CHANDIR_NOT_INPUT:
		return ("cannot input from channel marked as output");
	case CHK_CHANDIR_NOT_OUTPUT:
		return ("cannot output to channel marked as input");
	case CHK_INV_CHANTYPE_VAR:
		return ("cannot use direction specifier on non-channel-type variable");
	case CHK_INV_CHANTYPE_ALLOC:
		return ("malformed channel-type allocation");
	case CHK_INV_CHANTYPE_ALLOC_TARGET:
		return ("target of channel-type allocation must be a client and server variable pair");
	case CHK_INV_CHANTYPE_ALLOC_TYPE:
		return ("bad type for MOBILE (expecting a channel-type)");
	case CHK_INV_CHANTYPE_ALLOC_VARTYPE:
		return ("type mismatch for allocation of `%s\'");
	case CHK_CLAIM_VAR_BADTYPE:
		return ("`%s\' cannot be CLAIMed (expecting a channel-type)");
	case CHK_CLAIM_VAR_NOTSHARED:
		return ("channel-type `%s\' is not shared");
	case CHK_CLAIM_VAR_NOTCHANTYPE:
		return ("variable `%s\' is not a channel-type");
	case CHK_FPARAM_NOTSHARED:
		return ("parameter `%s\' may not be shared");
	case CHK_APARAM_NOTSHARED:
		return ("parameter `%s\' must be shared");
	case CHK_UNSHARABLE_PROTOCOL:
		return ("protocol cannot be shared");
	case CHK_UNSHARABLE_TYPE:
		return ("type `%s\' cannot be shared");
	case CHK_UNSHARED_CLONE:
		return ("cannot clone unshared variable `%s\'");
	case CHK_CLAIM_ANON_NODIRSPEC:
		return ("must specify direction when claiming SHARED channel `%s\'");
	case CHK_CLAIM_VAR_ISCHAN:
		return ("can only CLAIM shared channels or channel-type ends");
	case CHK_CLAIM_VAR_BADDIRSPEC:
		return ("bad channel-end specifier on CLAIM");
		/*}}}*/
		/*{{{  fork related errors*/
	case CHK_NO_LOCAL_FORKING:
		return ("FORK not within a FORKING");
	case CHK_NAME_INSIDE_FORKING:
		return ("name \"%s\" declared inside FORKING");
	case CHK_EMPTY_FORKING:
		return ("FORKING does not contain any FORKs");
	case CHK_FORK_PARAM_NOT_SHARED:
		return ("parameter %d to FORKed process is not shared");
	case CHK_FORK_PARAM_NOT_IMPLEMENTED:
		return ("parameter %d type not implemented yet!");
	case CHK_NO_INLINE_FORK:
		return "cannot FORK \"%s\", it is declared INLINE";
	case CHK_NO_PREDEF_FORK:
		return "pre-defined PROC \"%s\" may not be FORKed";
	case CHK_VAR_IN_FREE_FORK:
		return "reference parameter %d in free FORK of \"%s\" not permitted";
	case CHK_UNEXPECTED_LIST:
		return "unexpected list in expression";
	case CHK_UNEXPECTED_CHANDIR:
		return "unexpected direction specifier";
	case CHK_NO_CHANTYPE_DIR:
		return "must specify end on a channel-type protocol";
		/*}}}*/
		/*{{{  anonymous channel-type misuse */
	case CHK_ANONCHANTYPE_ASSIGN:
		return ("cannot assign to/from SHARED channel `%s\'");
		/*}}}*/
		/*{{{  protocol inheritance related errors */
	case CHK_BAD_EXTENDS:
		return ("bad protocol extension");
	case CHK_EXTENDS_ERROR:
		return ("protocol extension error: %s");
	case CHK_EXTENDS_NODIR:
		return ("missing channel-direction specifier for protocol inheritance");
	case CHK_EXTENDS_SPECIALISATION:
		return ("can only specialise output channels");
	case CHK_EXTENDS_GENERALISATION:
		return ("can only generalise input channels");
	case CHK_EXTENDS_BADTAGMATCH:
		return ("cannot inherit from `%s\', different sequential protocols for tag `%s\'");
		/*}}}*/
		/*{{{  other errors*/
	case CHK_MISSING_FORWDECL:
		return ("missing declaration for `%s\'");
	case CHK_MPD_BAD_IMPLEMENTS:
		return ("bad PROC TYPE `%s\' for IMPLEMENTS");
	case CHK_MPD_BAD_IMPLEMENTS_FOR:
		return ("bad IMPLEMENTS on `%s\'");
	case CHK_MPD_BAD_IMPLEMENTS_PARAMS:
		return ("implementation parameters differ");
	case CHK_MPP_FORKED:
		return ("cannot FORK mobile process activation");
	case CHK_MPP_NOT_MOBILE:
		return ("PROC TYPE must be MOBILE");
	case CHK_TRACES_NOT_RECORD:
		return ("invalid use of TRACES in type definition");
	case CHK_BAD_TRACE_ELEMENT:
		return ("invalid event in trace");
	case CHK_BAD_TRACE_DIRECTION:
		return ("invalid channel direction in trace");
	case CHK_PARTIAL_FIXED_TAGS:
		return ("partially fixed tags");
	case CHK_EXTERN_TYPE_MISMATCH:
		return ("external type mismatch for %s (%s)");
	case CHK_INVALID_TAG_VALUE:
		return ("invalid tag value %d");
	case CHK_TAG_VALUE_EXISTS:
		return ("tag value %d already exists");
	case CHK_ABBR_LHS_SHARED:
		return ("right-hand side of abbreviation is not shared");
	case CHK_ABBR_RHS_SHARED:
		return ("right-hand side of abbreviation is shared");
	case CHK_MPD_FIXED_MISMATCH:
		return ("implementation parameters differ in FIXED-ness");
	case CHK_MPD_ALLOC_NOT_MPROCDECL:
		return ("cannot allocate a non-mobile process");
	case CHK_MPD_ALLOC_PROCTYPEDECL:
		return ("cannot allocate a process type (must be implementation)");
	case CHK_ANYCHANTYPE_SIMPLEINPUT:
		return ("cannot do simple input on a variant channel");
	case CHK_ANYCHANTYPE_MISMATCH:
		return ("can only communicate mobile channel-ends on a CHAN OF MOBILE.CHAN");
	case CHK_ANYCHANTYPE_SPECMISMATCH:
		return ("cannot communicate this type of channel-end (type restriction mismatch)");
	case CHK_MULTIPLE_VTYPES:
		return ("indistinct types in ANY CHAN TYPE input");
	case CHK_ANYCHANTYPE_RESTRICTED:
		return ("cannot communicate this channel-end type (restricted)");
	case CHK_SYNC_NOT_BARRIER:
		return ("cannot SYNC on a non-barrier");
	case CHK_RESIGN_NOT_BARRIER:
		return ("cannot RESIGN from a non-barrier");
	case CHK_EXTENDS_NOT_BARRIER:
		return ("cannot PAR-extend a non-barrier");
	case CHK_BARRIER_RESIGNED:
		return ("cannot use BARRIER while resigned");
	case CHK_BARRIER_BAD_DECL:
		return ("bad BARRIER declaration");
	case CHK_BARRIER_BAD_EXTENDS:
		return ("bad BARRIER extension");
	case CHK_MOBILE_MIXED_DYNAMIC:
		return ("mixed dynamic/non-dynamic mobile type");
	case CHK_BARRIER_INTERNAL:
		return ("barrier error");
	case CHK_BARRIER_ASSIGN:
		return ("BARRIER assignment disallowed");
	case CHK_ANYCHANTYPE_INCOMPAT:
		return ("incompatible types");
	case CHK_ANYPROCTYPE_SIMPLEINPUT:
		return ("cannot do simple input on a variant channel");
	case CHK_ANYPROCTYPE_MISMATCH:
		return ("can only communicate mobile processes on a CHAN OF MOBILE.PROC");
	case CHK_ANYPROCTYPE_INCOMPAT:
		return ("incompatible types");
	case CHK_UNEXPECTED_DIRSPEC:
		return ("unexpected channel-direction specifier");
	case CHK_PROTOCOL_AS_DECLTYPE:
		return ("cannot declare variables of a PROTOCOL type");
	case CHK_PROTOCOL_AS_PARAMTYPE:
		return ("cannot declare parameters of a PROTOCOL type");
	case CHK_PARAM_MISSING_TYPE:
		return ("missing type for parameter");
	case CHK_ANYMOBILETYPE_MISMATCH:
		return ("can only communicate mobiles on a CHAN OF MOBILE.ANY");
	case CHK_CALLAT_NOT_DYNAMIC:
		return ("cannot call non-dynamic PROC %s at specified address");
	case CHK_BAD_DYNCALL_TYPE:
		return ("invalid type in DYNCALL pragma");
	case CHK_BAD_DYNCALL_NAMEDTYPE:
		return ("%s cannot be exported as a dynamic call");
	case CHK_BAD_FMTYPES_TYPE:
		return ("invalid type in FMTYPES pragma");
	case CHK_BAD_FMTYPES_NAMEDTYPE:
		return ("%s is not a valid type for FMTYPES pragma");
	case CHK_BAD_CASE_INCLUSION:
		return ("cannot include tags, not a tagged protocol");
		/*}}}*/
		/*{{{  buffered channel errors*/
	case CHK_BUFCHAN_NOTCONST:
		return ("buffered channel size must be constant");
	case CHK_BUFCHAN_NOTAGPROTO:
		return ("buffered channels of tagged protocols not supported yet");
	case CHK_BUFCHAN_MISSINGSIZE:
		return ("buffered channel declaration is missing its size");
	case CHK_BUFCHAN_NEGATIVE:
		return ("buffered channel size must be positive");
	case CHK_BUFCHAN_TOOBIG:
		return ("buffered channel too big (size is 2^n)");
		/*}}}*/
	default:
		return (NULL);
	}
}

/*}}}*/
/*{{{  PUBLIC const char *usemessagestring*/
PUBLIC const char *usemessagestring (const int n)
{
	switch (n) {
		/*{{{  cases */
		/*case USE_BAD_TAG:
		   sprintf(errorstring, "Illegal tag %d in routine %s",
		   (long int)p1, (char *)p2);
		   return(errorstring); */
		/*case USE_GLOBAL_FREE_VARS:
		   return("There are global free variables"); */
	case USE_VAR_VAR_ABBREVIATED:
		return ("`%s' cannot be used (it has been abbreviated)");
	case USE_VAR_VAL_ABBREVIATED:
		return ("`%s' cannot be altered (it has been VAL abbreviated)");
	case USE_BAD_CHAN_PARAM:
		return ("channel parameter `%s' cannot be used for both input and output");
	case USE_BAD_FREE_CHAN:
		return ("free channel `%s' cannot be used for both input and output");
	case USE_WRITTEN_IN_PAR:
		return ("variable `%s' is assigned to in parallel");
	case USE_READ_AND_WRITTEN_IN_PAR:
		return ("variable `%s' is read and assigned to in parallel");
	case USE_OUTPUT_IN_PAR:
		/*return ("Channel `%s' is output on in parallel"); */
		return ("parallel outputs on channel `%s'");
	case USE_INPUT_IN_PAR:
		/*return ("Channel `%s' is input on in parallel"); */
		return ("parallel inputs on channel `%s'");
		/* I've removed the fixed limit - CO'N 17/2/90
		   case USE_TOO_MANY_OVERLAP_CHECKS:
		   return("Too many overlap checks required: maximum is %d");
		 */
		/*case USE_VAR_TABLE_SUBSCRIPTS :
		   return "All subscripts in channel tables must be constant"; */
	case USE_VAR_FREEVAR:
	case USE_VAL_FREEVAR:
		return ("aliasing error, `%s' is a free variable");
		/*case USE_FUTURE_COLON2_INPUT: */
		/*return ("Cannot use length `%s' in array part of counted array input"); */
		/*return ("Future implementation restriction - Cannot use length `%s' in array part of counted array input"); */
	case USE_COLON2_INPUT:
		return ("cannot use length `%s' in array part of counted array input");
	case USE_NO_ALIASCHECK:
		return "alias and usage checking is disabled";
	case USE_NO_USAGECHECK:
		return "usage checking is disabled";
	case USE_TIMER_IN_PRI_PAR:
		return "implementation restriction: TIMER `%s' should not be read in both high and low priority branches of a PRI PAR";
	case USE_FORK_FREE_VAR_SCOPE:
		return "free variable `%s' must be declared outside FORKING";
	case USE_UNSHARED_FORK_FREE_VAR:
		return "free variable `%s' in FORKed process is not shared";
	case USE_UNSHARED_FORK_FREE_MVAR:
		return "free mobile variable `%s' in FORKed process is not shared";
	case USE_FORK_FREE_VARS:
		return "process being FORKed uses free variable '%s', a FORKING block is required to enforce its scope";
		/*}}} */
	case USE_VAR_UNDEFINED:
		return "variable `%s' is undefined here";
	case USE_VAR_UNKNOWN:
		return "variable `%s' might be undefined here";
	case USE_CHAN_IO_SEQ:
		return "channel `%s' has sequential input/output (will deadlock)";
	case USE_INTERNAL_ERROR:
		return "internal error in `%s'";
	case USE_PARAM_UNDEFINED:
		return "parameter `%s' is undefined here";
	case USE_NTH_PARAM_UNDEFINED:
		return "parameter %d is undefined here";
	case USE_PARAM_UNKNOWN:
		return "parameter `%s' might be undefined here";
	case USE_NTH_PARAM_UNKNOWN:
		return "parameter %d might be undefined here";
	case USE_VAR_PARTIAL:
		return "variable `%s' is partially defined here";
	case USE_PARAM_PARTIAL:
		return "parameter `%s' is partially defined here";
	case USE_INCALLOF:
		return "    in call of `%s':";
	case USE_BAD_CHAN_PARAM_IN:
		return "channel parameter `%s' cannot be used for output";
	case USE_BAD_CHAN_PARAM_OUT:
		return "channel parameter `%s' cannot be used for input";
	case USE_BAD_CHAN_ARRAY_PARAM_IN:
		return "elements of channel array parameter `%s' cannot be used for output";
	case USE_BAD_CHAN_ARRAY_PARAM_OUT:
		return "elements of channel array parameter `%s' cannot be used for input";
	case USE_BAD_CHAN_ABBR_IN:
		return "channel abbreviation `%s' cannot be used for output";
	case USE_BAD_CHAN_ABBR_OUT:
		return "channel abbreviation `%s' cannot be used for input";
	case USE_BAD_CHAN_ARRAY_ABBR_IN:
		return "elements of channel array abbreviation `%s' cannot be used for output";
	case USE_BAD_CHAN_ARRAY_ABBR_OUT:
		return "elements of channel array abbreviation `%s' cannot be used for input";
	case USE_BAD_CHAN_CONFLICT:
		return "incompatible channel direction specifier on `%s'";
	case USE_BAD_CHAN_IN:
		return "channel `%s' cannot be used for output";
	case USE_BAD_CHAN_OUT:
		return "channel `%s' cannot be used for input";
	case USE_CHAN_IN_XIN:
		return "channel `%s' cannot be used during extended input";
	case USE_CHAN_IN_XIN_UNKNOWN:
		return "channel cannot be used during extended input";
	case USE_WARN_NREPLPAR_NOUSAGE:
		return "not usage checking dynamic replicated PAR";
	case USE_NESTED_CLAIM:
		return "`%s\' is already CLAIM\'d here";
	case USE_NOT_CLAIMED:
		return "must CLAIM `%s\' before using channels";
	case USE_VAR_CLAIMED_WRITE:
		return "cannot write to `%s\', it has been CLAIM\'d";
	case USE_PARAM_CLAIMED_WRITE:
		return "cannot write to parameter `%s\', it has been CLAIM\'d";
	case USE_BAD_MOBILE_DEFINED:
		return "inappropriate MOBILE type used with DEFINED%s";
	case USE_MPP_FREEVARS:
		return "free variables in MOBILE PROC `%s\'";
	case USE_PROCTYPE_NSYNC_PARAMS:
		return "PROC TYPE contains non-sync parameter(s)";
	case USE_MPP_NSYNC_PARAMS:
		return "MOBILE PROC contains non-sync parameter(s)";
	case USE_FORK_SUSPEND:
		return "may not FORK a PROC that SUSPENDs";
	case USE_SUSPEND_INSIDE_CLAIM:
		return "may not SUSPEND inside a CLAIM";
	case USE_SUSPEND_INSIDE_XINPUT:
		return "may not SUSPEND during an extended rendezvous";
	case USE_FIXED_PARAM_MOVED:
		return "MOBILE parameter `%s\' is specified as FIXED but is moved";
	case USE_FIXED_ABBR_MOVED:
		return "MOBILE abbreviation `%s\' is specified as FIXED but is moved";
	case USE_FIXED_DECL_MOVED:
		return "MOBILE declaration `%s\' is specified as FIXED but is moved";
	case USE_PARAM_MUST_CLAIM:
		return "must CLAIM `%s\' before passing as a non-SHARED parameter";
	case USE_ABBR_MUST_CLAIM:
		return "must CLAIM `%s\' before abbreviating to non-SHARED";
	case USE_PARAM_UNFIXED:
		return "cannot pass `%s\' as a parameter, it is moved";
	case USE_PARAM_INVALID_SHARED:
		return "cannot pass non-SHARED argument `%s\' as a SHARED parameter";
	case USE_PARAM_MUST_UNCLAIM:
		return "cannot pass CLAIMed argument `%s\' as a SHARED parameter";
	case USE_ABBR_MUST_UNCLAIM:
		return "cannot abbreviate CLAIMed `%s\' to SHARED";
	case USE_EMPTY_FORKING:
		return ("FORKING does not contain any FORKs");
	case USE_BARRIER_PARALLEL:
		return ("BARRIER `%s\' used in parallel (not extended)");
	case USE_BARRIER_EXTENDED_UNFIXED:
		return ("cannot move extended barrier `%s\'");
	case USE_UNEXPECTED_CHAN_DIR:
		return ("unexpected channel-direction specifier");
	case USE_DUPLICATE_TAG_IN_MERGE:
		return ("cannot merge tagged protocols, duplicate tag `%s\'");
	case USE_FM_NO_CHANTYPE:
		return ("collated protocol for channel type `%s\' not found");
	default:
		return (NULL);
	}
}

/*}}}*/
/*{{{  PUBLIC const char *vtimessagestring*/
PUBLIC const char *vtimessagestring (const int n)
{
	switch (n) {
		/*{{{  cases */
		/*case VTI_STACK_OVERFLOW:
		   return("Workspace stack overflow"); */
		/*case VTI_STACK_UNDERFLOW:
		   return("Workspace stack underflow"); */
		/*case VTI_BAD_TAG:
		   return("Illegal tag %d in routine %s"); */
		/*case VTI_NAMETRANS_OVERFLOW :
		   return("Name translation stack overflow"); */
	case VTI_NAMETRANS_UNDERFLOW:
		return ("name translation stack underflow");
		/*case VTI_TOO_MANY_NAMETRANS :
		   return("Too many names for name translation"); */
	case VTI_ARRAY_SIZE_OVERFLOW:
		return ("implementation restriction: Array size is too large");
	case VTI_EXPAND_NOT_CONSTANT:
		return "actual parameter corresponding to formal parameter %s must be a constant";
		/*}}} */
	default:
		return (NULL);
	}
}

/*}}}*/
