/* $Id: generror.c,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */


/*
 *	Error reporting
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

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "midinc.h"

#include "generror.h"

/*}}}  */

/*{{{  PUBLIC const char *genmessagestring (n) */
PUBLIC const char *genmessagestring (const int n)
{
	switch (n) {
		/*{{{  cases */
	case GEN_BAD_OPD:
		return ("Bad operand mode %d in routine %s");
	case GEN_BAD_MOVEELEMENT_DIRN:
		return ("Bad dirn parameter %d to routine moveelement");
	case GEN_CODE_BUFFER_FULL:
		return ("Code buffer full (%d bytes); use command line to increase buffer size");
		/*case GEN_LABEL_BUFFER_FULL:
		   return ("Code buffer full (too many labels: %d)"); */
	case GEN_MULTIPLY_DEFINED_LABEL:
		return ("Multiply defined label L%d");
	case GEN_ADJUST_ERROR:
		return ("Reference to undeclared label L%d");
	case GEN_ADJUST_TAG_ERROR:
		return ("Bad tag %d in routine adjust");
	case GEN_CANNOT_OPEN_OBJECT_FILE:
		return ("Cannot open object file: \"%s\"");
	case GEN_CANNOT_CLOSE_OBJECT_FILE:
		return ("Cannot close object file: \"%s\"");
	case GEN_ASSERT_ERROR:
		return ("Parameter to ASSERT is FALSE");
		/*case GEN_TOO_MANY_SEGMENTS:
		   return("Too many entry points (> %d) declared"); */
	case GEN_BAD_CONSTANT_LOAD:
		return ("Bad direction %d for moving a constant table in moveelement");
		/*case GEN_BAD_TAG:
		   return ("Unknown tag %d in routine %s"); */
	case GEN_PLACE_OVERFLOW:
		return ("Placement expression for %s wraps around memory");
		/*case GEN_TOO_MANY_NESTED_ALTS:
		   return("Implementation limit: ALT is nested too deeply, limit is %d"); */
		/*case GEN_TOO_MANY_VARS :
		   return("Implementation limit: Too many variables (max %d) in routine %s"); */
		/*case GEN_TOO_MANY_STDLIB_NAMES:
		   return("Too many entries in standard library, limit is %d"); */
	case GEN_MISSING_STDLIB_ENTRY:
		return ("%s is not declared in the standard library");
	case GEN_BAD_PRIMARY_OPERAND:
		return ("%s may only take a constant or label operand");
	case GEN_KERNEL_RUN_ERROR:
		return ("Fourth parameter to KERNEL.RUN must be a constant >= 3");
		/*case GEN_CSE:
		   return("Common subexpression elimination"); */
	case GEN_INDEX_HASH_TABLE_ERR:
		return ("Address not found in index hash table for node with tag %s");
	case GEN_UNSIZED_ABBREV:
		return ("%s has unknown size");
#if 0
	case GEN_RUN_TIME_OVERLAP_CHECK:
		return ("Run-time disjointness check inserted");
	case GEN_RUN_TIME_OVERLAP_CHECKS:
		return ("%d run-time disjointness checks inserted");
#endif
	case GEN_LINEMARK_LIST_ERR:
		return ("Index not found in linemark list");
		/*case GEN_UNEXPECTED_EOF:
		   return("Unexpected EOF when reading source file for source output"); */
		/*case GEN_LINE_NOT_IN_LIST:
		   return("Line with line number %d and file number %d is not in lines list"); */
	case GEN_BAD_USECOUNT:
		return ("Bad usecount for %s");
	case GEN_ERROR_IN_ROUTINE:
		return ("Error %d in routine %s");
	case GEN_STDLIBS_NOT_ENABLED:
		return ("Routine %s required from the standard library");
		/*case GEN_STDLIB_HAS_VS :
		   return("Standard library routine %s requires vectorspace"); */
	case GEN_WORKSPACE_CLASH:
		return ("Workspace clashes with variable PLACED AT WORKSPACE %d");
	case GEN_GUY_NO_CHANS:
		return ("No access to channels permitted in GUY code; use ASM");
	case GEN_BAD_ASMNAME:
		return ("Special name %s is not permitted at this position");
		/*case GEN_PLACE_ON_LINKS:
		   return("Placement expression for %s clashes with interactive debugger");
		   return("Placement expression for %s clashes with virtual routing system"); */
	case GEN_REPL_FN_IN_ALT_GUARD:
		return
			"Implementation restriction: FUNCTION %s which uses replicated ALT variable %s as a free variable may not be called from a guard of that ALT";
	case GEN_ENABLE_CHAN_ALIGNMENT_PROBLEM:
		/*return "Implementation restriction; cannot 'un-oddify' channels when channel i/o is not by pointer"; */
		return "Implementation restriction; cannot realign virtual channels when channel i/o is not by pointer";
	case GEN_ALIGNMENT_CHECK:
		return "Run-time alignment check required for RETYPE";
	case GEN_DEVMOVE_LATENCY_PROBLEM:
		return "Interrupt latency will be affected because DEVMOVE is not interruptible on this variant of the T9000";
	case GEN_PLACE_BELOW_MEMSTART:
		return ("%s placed below MEMSTART");
	case GEN_H_PLACE_ON_LINKS:
		return ("%s placed below MEMSTART on byte mode link");
	case GEN_T_PLACE_ON_LINKS:
		return ("%s placed below MEMSTART on link, may clash with virtual routing system");
		/*}}}  */
		/*{{{  Configurer errors */
	case GEN_CONF_PATCH_TOO_SMALL:
		return ("Cannot insert a patch of %d bytes");
	case GEN_CONF_CANT_FIND_PATCH:
		return ("Cannot find patch at address %d");
		/*}}}  */
		/*{{{  frmb added errors */
	case GEN_NO_DYNMEM:
		return ("Dynamic memory support is disabled");
	case GEN_IMPOSSIBLE:
		return ("Impossible thing %d in code-gen");
	case GEN_TSTACK_UNDERFLOW:
		return ("Stack underflow");
	case GEN_TSTACK_OVERFLOW:
		return ("Stack overflow");
	case GEN_NO_FMPARAM:
		return ("MOBILE parameters to FORKed processes not supported yet!");
	case GEN_INTERNAL_ERROR:
		return ("code-generation error: %s");
	case GEN_WARN_BADCODE:
		return ("generating possibly bad code -- source may need simplifying, sorry");
	case GEN_FORKED_EXPORT_NO_VS:
		return ("Cannot export name \"%s\", FORKing name must use vectorspace");
		/*}}}  */
		/*{{{  cgr added errors / warnings */
	case GEN_NO_MOBILESPACE:
		return "Tried to use mobilespace, but mobilespace disabled -- mobilespace use in separately compiled code?";
	case GEN_BAD_MOBILE_SLICE:
		return "Tried to copy a slice containing mobiles -- this is not supported, please use a SEQ loop";
	case GEN_BARRIER_CLONE:
		return "Tried to clone a barrier";
	case GEN_BAD_ALIGNMENT:
		return "Bad alignment expression (non-constant?)";
	case GEN_PRI_PAR_AS_PAR:
		return "PRI PAR compiled as PAR";
	case GEN_ADDROF_BAD_TYPE:
		return "ADDROF/HWADDROF unsupported type";
		/*}}}*/
	default:
		return (NULL);
	}
}

/*}}}  */
