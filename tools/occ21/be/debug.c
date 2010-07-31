/* $Id: debug.c,v 1.4 1997/09/19 14:01:07 mdp2 Exp $ */

/*
 *	debug walks the parse tree generating debug information into the debug file
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

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "includes.h"
#include "extlib.h"		/* IMPORTED */
#include "debughdr.h"		/* IMPORTED */

#include "genhdr.h"
#include "gen1def.h"		/* insidealtguard */
#include "gen2def.h"
#include "gen8def.h"		/* get_alt_adjust */
#include "gen10def.h"		/* set_alt_ds */
#include "objwrt.h"
#include "objlib.h"
#include "debugdef.h"
#include "bind2def.h"
#include "generror.h"


#include "maplib.h" /* mapfile handling */	/* IMPORTED */
/*}}}*/

/*{{{  COMMENT format of debug information*/
/**********************  Start comment out ****************************
@*{{{  format of debug information*@
@*{{{  Symbolic information*@
version      index versionnumber

startfold    index
endfold      index
linemark     index line

beginscope   index
endscope     index

workspace    index segtype thisoffset prevoffset
endseg       index

variable     index vartype access dims {dims access offset } offset name
procedure    index proctype dummy workspace vectorspace name
constant     index vartype hi lo name

filename     index filename
@*}}}*@
@*{{{  Code information*@
codemark     index ptr address
addressfix   index ptr address
libpatch     index address
@*}}}*@
@*}}}*@
 **********************   End comment out  ****************************/
/*}}}*/

/*{{{  constants*/
/*{{{  Stuff about keep and throw*/
/********************************************************
*
* Once upon a time we had a system where the _librarian_ decided what
* debug info was to appear in a library.
*
* This meant that it could remove the symbolic junk if the library needed
* to be small. To help this, the compiler lumped up all the `removable'
* stuff into chunks, and all the non-removable stuff into chunks.
*
* The librarian then knew that if anything in a chunk was removable, then
* it all was, and vice-versa.
*
* Of course, now the compiler makes this decision, using the '-d' flag.
* However, it has still been using these two buffers.
*
* The MERGE_OUTPUT_BUFFERS flag controls whether the compiler
* lumps the two types together.
*
********************************************************/

#define MERGE_OUTPUT_BUFFERS

typedef enum
{ BUF_KEEP, BUF_THROW }
keep_t;

/*}}}*/

#define       MAX_DEBUG_STRING_SIZE  255	/* Size of one record */
/*#define   LFF_MAX_DEBUG_STRING_SIZE 255*//* Size of one LFF debug record */
#define TCOFF_MAX_DEBUG_STRING_SIZE 4096	/* Size of one TCOFF debug record */
#define C_PREFIX 0x80
#define INDEX_HASH_TABLE_SIZE 256
#define HASH_MASK 255

/*{{{  source code output*/
#define FILE_START 0
#define FILE_END 1
/*}}}*/

#define DEBUG_VERSION \
  (RTL_LINEMARK_BIT     | RTL_OCCAM_BITS    | RTL_REPLPAR_BIT | \
   RTL_CODEADDR_BIT     | RTL_PROCEDURE_BIT | RTL_ORDER_BIT   | \
   RTL_INDEX_SORTED_BIT )
/* The RTL_COMPACTED_BIT is or-ed in if minimal_debugoutput is TRUE */
/* The RTL_VIRTUAL_LINK_BIT is or-ed in if iobycall is TRUE */

#ifdef DEBUG
/*  #define debug_diagnostics diagnostics attach it to the main diagnostics flag */
#define debug_diagnostics TRUE
#else
#define debug_diagnostics FALSE
#endif

/*}}}*/
/*{{{  PUBLIC variables*/
/* This may be turned off by the configurer */
PUBLIC BOOL symbolic_debugoutput = TRUE;
/*}}}*/
/*{{{  PRIVATE variables*/
PRIVATE INT32 enclosing_this_offset;
PRIVATE INT32 workspace_adjust;
PRIVATE int debuginfo_index;
PRIVATE int filenum;		/* The current file */
PRIVATE int nextfile;		/* The next one to look at */
PRIVATE BOOL debug_to_file;

PRIVATE int last_codemark_line;	/* bug TS/1870 15/09/92 */
PRIVATE BIT32 last_codemark_offset;

PRIVATE int last_linemark_line;	/* INSdi02623 12/10/93 */
PRIVATE int last_linemark_index = -1;

#if 0
PRIVATE int debug_buffer_size;
#else
#define debug_buffer_size TCOFF_MAX_DEBUG_STRING_SIZE
#endif

PRIVATE char *keep = NULL;	/* keep buffer */
PRIVATE int kp;			/* keep buffer pointer */
#ifndef MERGE_OUTPUT_BUFFERS
PRIVATE char *throw = NULL;	/* throw buffer */
PRIVATE int tp;			/* throw buffer pointer */
#endif

#ifdef OCCAM2_5
PRIVATE int debug_typedef_number;
#endif
PRIVATE BOOL alt_starting;		/* allows special handling of enb/dis code MDP */

/*{{{  index hash table*/
typedef BIT32 NODEPTR;
typedef struct index_hash_cell
{
	NODEPTR address;
	int index;
	struct index_hash_cell *next;
}
index_hash_t;

/* note that this will be initialised to NULLs */
PRIVATE index_hash_t *index_hash_table[INDEX_HASH_TABLE_SIZE];
/*}}}*/
/*}}}*/
/*{{{  forward declarations*/
PRIVATE void gendebug (treenode * tptr);
/*}}}*/
/*{{{  low-level debug buffer writing*/
/*{{{  PRIVATE void add_to_debug_buffer(string, length, type)*/
/*{{{  comment*/
/*************************************************************************
*
*  appends the debug record held in 'string' with length 'length' to
*  the debug buffer given by 'type'.
*
**************************************************************************/
/*}}}*/

#ifdef MERGE_OUTPUT_BUFFERS
#define add_to_debug_buffer(S,L,X) sub_add_to_debug_buffer((S),(L))
PRIVATE void
sub_add_to_debug_buffer (const char *const string, const int length)
{
	char *const buffer = keep;
	int bp_len = kp;

#else

PRIVATE void
add_to_debug_buffer (const char *const string, const int length, const keep_t type)
{
	char *const buffer = (type == BUF_KEEP) ? keep : throw;
	int bp_len = (type == BUF_KEEP) ? kp : tp;

#endif

	if ((bp_len + length) > debug_buffer_size) {
		write_debug_string (buffer, bp_len);
		bp_len = 0;
	}

	memcpy (&buffer[bp_len], string, length);
	bp_len += length;

#ifdef MERGE_OUTPUT_BUFFERS
	kp = bp_len;
#else
	if (type == BUF_KEEP)
		kp = bp_len;
	else
		tp = bp_len;
#endif
}

/*}}}*/
/*}}}*/

/*{{{  index hash table routines*/
/*{{{  PRIVATE int hash_function(address)*/
/*{{{  comment*/
/******************************************************************************
*
*  hash_function(address)
*  address = address of a tree node.
*  The hashing algorithm is performed on the address of a tree node. The
*  tree node pointer is cast into a BIT32 type and the following actions are
*  taken. The address word is shifted two bits right thereby removing the two
*  least significant bits. The resulting address word is then ANDed with 255
*  to produce an eight bit number which is used to index the hash table.
*  Andy Whitlow 19.10.88
*
*  Modified to shift by 5 instead; 9/10/90 CON
*
******************************************************************************/
/*}}}*/
#if 0
PRIVATE int hash_function (treenode * address)
{
	return ((((unsigned long) address) >> 5) & HASH_MASK);
}
#else
#define hash_function(address) (int)((((unsigned long)(address)) >> 5) & HASH_MASK)
#endif
/*}}}*/
/*{{{  PRIVATE void add_to_index_table(address, index)*/
/*{{{  comment*/
/******************************************************************************
*
*     add_to_index_table(address, index)
*
*     address = address of treenode, index = debug record index for that node
*     makes a new entry in the index hash table by performing a hashing
*     function on address.
*     Andy Whitlow 19.10.88.
*
******************************************************************************/
/*}}}*/

PRIVATE void add_to_index_table (treenode * const address, const int index)
{
	index_hash_t *hptr;
	BIT32 addr = (BIT32) ((PTRINT) address & 0xffffffff);
	const int hash_value = hash_function (addr);
	BIT32 altaddr = addr + (2 * alt_starting);
	/* modified to distinguish ALT setup code and body */

	if (debug_diagnostics) {
		fprintf (outfile, "add_to_index_table: (index = %d) treenode: %X, tag: %s, hash: %d\n",
			 index, altaddr,
			 address == NULL ? "<<NULL>>" : ((BIT32) address & 1) != 0 ? "ENDBODY" : itagstring (TagOf (address)), hash_value);
	}
	/* If that address is already there, ignore it */
	for (hptr = index_hash_table[hash_value]; hptr != NULL; hptr = hptr->next)
		if (hptr->address == (NODEPTR) altaddr) {
			if (hptr->index != index)
				if (debug_diagnostics)
					fprintf (outfile, "Duplicate hashing of address (index = %d, was %d)\n", index, hptr->index);
			return;
		}
	hptr = (index_hash_t *) newvec (sizeof (*hptr));
	hptr->next = index_hash_table[hash_value];
	index_hash_table[hash_value] = hptr;
	hptr->address = (NODEPTR) altaddr;
	hptr->index = index;
}

/*}}}*/
/*{{{  PUBLIC int get_from_index_table(address)*/
/*{{{  comment*/
/******************************************************************************
*
*    get_from_index_table(address)
*
*    address = address of treenode.
*    finds the record corresponding to address in the index hash table and
*    returns its index field.
*    Andy Whitlow 19.10.88
*
******************************************************************************/
/*}}}*/
PUBLIC int get_from_index_table (const treenode *const address)
{
	index_hash_t *hptr;
	const int hash_value = hash_function ((BIT32) ((PTRINT) address & 0xffffffff));

#ifdef DEBUG
	DEBUG_MSG (("get_from_index_table: treenode: %lX, tag: %s, hash: %d\n",
		    (BIT32) ((PTRINT) address & 0xffffffff), address == NULL ? "<<NULL>>" : itagstring (TagOf (address)), hash_value));
#endif
	/* mask off bottom bits to remove the altstart bit MDP 15.08.97 */
	for (hptr = index_hash_table[hash_value]; hptr != NULL; hptr = hptr->next) {
		if ((hptr->address & 0xfffffffd) == (NODEPTR) ((PTRINT) address & 0xfffffffd)) {
			return (hptr->index);
		}
	}
	msg_out_s (SEV_INTERNAL, GEN, GEN_INDEX_HASH_TABLE_ERR,
		   address == NULL ? NOPOSN : LocnOf (address), address == NULL ? "ENDBODY" : itagstring (TagOf (address)));
	return (0);		/* not reached */
}

/*}}}*/

/*}}}*/
/*{{{  source code output   UNUSED*/
#if 0				/* This is only used for source output */
/*{{{  linemark list*/
/* needed when producing source output */
typedef struct linemark_cell {
	int index;
	int linenumber;
	struct linemark_cell *next;
} linemark_cell_t;

PRIVATE linemark_cell_t *linemark_list = NULL;
/*}}}*/
/*{{{  file list*/
/* needed when producing source output */
typedef struct file_cell {
	int tag;
	int fileid;
	int index;
	struct file_cell *next;
} file_cell_t;

PRIVATE file_cell_t *file_list = NULL;
/*}}}*/
/*{{{  linemark list private routine*/
/*{{{  PRIVATE void add_to_linemark_list(index, lineno)*/
/*{{{  comment*/
/******************************************************************************
*
*     add_to_linemark_or_libpatch_list(index, lineno, type)
*
*     index = linemark debug record index,
*     lineno = corresponding line number
*     type = either LINEMARK or LIBPATCH
*     Adds a new linemark cell to the linemark list.
*     Andy Whitlow 16.12.88.
*
******************************************************************************/
/*}}}*/
PRIVATE void add_to_linemark_list (int index, int lineno)
{
	linemark_cell_t *lptr;

	lptr = (linemark_cell_t *) newvec (sizeof (*lptr));
	lptr->index = index;
	lptr->linenumber = lineno;
	lptr->next = linemark_list;
	linemark_list = lptr;
}

/*}}}*/
/*}}}*/
/*{{{  file list private routine*/
/*{{{  PRIVATE void add_to_file_list(this_index, this_tag, this_fileid)*/
/*{{{  comment*/
/******************************************************************************
*
*     add_to_file_list(this_index, this_tag, this_fileid)
*
*     this_index = index of endfold or startfold mark.
*     this_tag = either FILE_START or FILE_END.
*     this_fileid = file number of corresponding file.
*     Adds a new file cell to the file list.
*     Andy Whitlow 15.12.88.
*
******************************************************************************/
/*}}}*/
PRIVATE void add_to_file_list (int this_index, int this_fileid, int this_tag)
{
	file_cell_t *new, *fptr = file_list;

	/*{{{  create new cell */
	new = (file_cell_t *) newvec (sizeof (*new));
	new->index = this_index;
	new->fileid = this_fileid;
	new->tag = this_tag;
	new->next = NULL;
	/*}}} */
	if (fptr == NULL)
		file_list = new;
	else {
		/*{{{  append cell to list */
		while (fptr->next != NULL)
			fptr = fptr->next;
		fptr->next = new;
		/*}}} */
	}
}

/*}}}*/
/*}}}*/
#endif
/*}}}*/

/*{{{  support*/
/*{{{  PRIVATE char *addbuf_3L_num(buffer, n)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  addbuf_3L_num writes 'n' as a Lattice encoded number to 'buffer'.
 *                         A pointer to the next free byte in 'buffer' is
 *                         returned.
 *
 *****************************************************************************/
/*}}}*/
#define MASK     0x0FF00000l
#define TOP5BITS 0xF8000000l

PRIVATE char *addbuf_3L_num (char *buffer, const INT32 n)
{
	int bits = 28;
	INT32 mask = MASK;
	if (n < 0L) {
		if ((n & TOP5BITS) != TOP5BITS)
			*buffer++ = (char) (0xF0 | (n >> 28));
		else {
			while (((n & mask) == mask) && (bits > 7)) {
				mask >>= 7;
				bits -= 7;
			}
		}
	} else {
		if ((n & TOP5BITS) != 0)
			*buffer++ = (char) (0x80 | (n >> 28));
		else {
			while (((n & mask) == 0) && (bits > 7)) {
				mask >>= 7;
				bits -= 7;
			}
		}
	}
	while (bits > 7) {
		bits -= 7;
		*buffer++ = (char) (0x80 | ((n >> bits) & 0x7F));
	}
	*buffer++ = (char) (n & 0x7F);
	return (buffer);
}

/*}}}*/
/*{{{  PRIVATE char *addbuf_3L_str(buffer, s)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  addbuf_3L_str writes string 's' as a Lattice encoded string to 'buffer'.
 *                         A pointer to the next free byte in 'buffer' is
 *                         returned.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE char *addbuf_3L_str (char *buffer, const char *const s)
{
	const int l = strlen (s);
	buffer = addbuf_3L_num (buffer, (INT32) l);
	memcpy (buffer, s, l);
	return (buffer + l);
}

/*}}}*/

/*{{{  PRIVATE INT32 vartypeof(t)*/
/*****************************************************************************
 *
 *  vartypeof takes a base type t and returns a string representing it.
 *
 *****************************************************************************/
PRIVATE INT32
vartypeof (treenode * const tptr)
{
	switch (TagOf (tptr)) {
	case S_CHAN:
		return (RTL_CHANNEL);
	case S_TIMER:
		return (RTL_TIMER);
	case S_BOOL:
		return (RTL_BOOLEAN);
	case S_BYTE:
		return (RTL_BYTE);
	case S_INT:
		return (RTL_INTEGER);
	case S_INT16:
		return (RTL_INT16);
	case S_INT32:
		return (RTL_INT32);
	case S_INT64:
		return (RTL_INT64);
	case S_REAL32:
		return (RTL_REAL32);
	case S_REAL64:
		return (RTL_REAL64);
	case S_PORT:
		return (RTL_PORT);
#ifdef OCCAM2_5
/*case N_TYPEDECL: return RTL_INTEGER; *//* anything! */
	case N_TYPEDECL:
		return NVOffsetOf (tptr);	/* typedef number */
	case S_ARRAY:
		return get_from_index_table (tptr);
#endif
	default:
		badtag (NOPOSN, TagOf (tptr), "vartypeof");
		return (0);	/* never reached */
	}
}

/*}}}*/
/*{{{  PRIVATE const char *vartypestr(t)*/
/*****************************************************************************
 *
 *  vartypestr takes a type t and returns a string representing it.
 *
 *****************************************************************************/
PRIVATE const char *vartypestr (const INT32 t)
{
	switch (t) {
	case RTL_CHANNEL:
		return ("channel");
	case RTL_TIMER:
		return ("timer");
	case RTL_BOOLEAN:
		return ("Boolean");
	case RTL_BYTE:
		return ("byte");
	case RTL_INTEGER:
		return ("integer");
	case RTL_INT16:
		return ("int16");
	case RTL_INT32:
		return ("int32");
	case RTL_INT64:
		return ("int64");
	case RTL_REAL32:
		return ("real32");
	case RTL_REAL64:
		return ("real64");
	case RTL_PORT:
		return ("port");
	case RTL_PROTOCOL:
		return ("protocol");
	case RTL_CASETAG:
		return ("tag");
	case RTL_CHANPOINTER:
		return ("channelptr");
	default:
		return ("???");
	}
}

/*}}}*/
/*{{{  PRIVATE const char *segtypestr(t)*/
/*****************************************************************************
 *
 *  segtypestr takes a segment type t and returns a string representing it.
 *
 *****************************************************************************/
PRIVATE const char *segtypestr (const INT32 t)
{
	switch (t) {
	case RTL_SEGTYPE_PROC:
		return ("proc");
	case RTL_SEGTYPE_REPL:
		return ("repl");
	case RTL_SEGTYPE_PAR:
		return ("par");
	case RTL_SEGTYPE_PRIPAR:
		return ("pripar");
	case RTL_SEGTYPE_SEQ:
		return ("seq");
	default:
		return ("???");
	}
}

/*}}}*/
/*{{{  PRIVATE const char *accessstr(t)*/
/*****************************************************************************
 *
 *  accessstr takes an access mode t and returns a string representing it.
 *
 *****************************************************************************/
PRIVATE const char *accessstr (const INT32 t)
{
	switch (t) {
	case RTL_CONST:
		return ("const");
	case RTL_PLACED:
		return ("placed");
	case RTL_BYREF:
		return ("byref");
	case RTL_BYVAL:
		return ("byval");
	case RTL_OPTIMISED:
		return ("opt");
	case RTL_VOIDACCESS:
		return ("voidaccess");
	default:
		return ("???");
	}
}

/*}}}*/
/*{{{  PRIVATE const char *proctypestr(t)*/
/*****************************************************************************
 *
 *  proctypestr takes a proc type t and returns a string representing it.
 *
 *****************************************************************************/
PRIVATE const char *proctypestr (const INT32 t)
{
	switch (t) {
	case RTL_PROCFLAG:
		return ("procflag");
	case RTL_FUNCTIONFLAG:
		return ("functionflag");
	case RTL_SCPROCFLAG:
		return ("scprocflag");
	case RTL_SCFUNCTIONFLAG:
		return ("scfunctionflag");
	case RTL_LIBPROCFLAG:
		return ("libprocflag");
	case RTL_LIBFUNCTIONFLAG:
		return ("libfunctionflag");
	default:
		return ("???");
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL isaparentof(f1, f2)*/
/*****************************************************************************
 *
 *  isaparentof returns TRUE if f1 is a parent file of f2.
 *
 *****************************************************************************/
PRIVATE BOOL isaparentof (int f1, int f2)
{
	while (f2 != (-1)) {
		if (f2 == f1)
			return TRUE;
		f2 = parentoffile (f2);
	}
	return (FALSE);
}

/*}}}*/
/*{{{  PRIVATE void calc_var_details*/
PRIVATE void calc_var_details (treenode *const nptr, int *const dims, INT32 *const vartype, INT32 *const access, BIT32 *const output_offset)
{
	*dims = 0;
	/*{{{  set up vartype, access and dims */
	if ((TagOf (nptr) == N_SPROTDEF) || (TagOf (nptr) == N_TPROTDEF)) {
		*vartype = RTL_PROTOCOL;
		*access = RTL_CONST;	/* dims = 0; */
	} else if (TagOf (nptr) == N_TAGDEF) {
		*vartype = RTL_CASETAG;
		*access = RTL_CONST;	/* dims = 0; */
	} else {
		treenode *tptr = NDeclOf (nptr);
		treenode *type = NTypeOf (nptr);
		treenode *t;
		for (t = type; TagOf (t) == S_ARRAY; t = ARTypeOf (t))
			(*dims)++;
		*vartype = vartypeof (t);
		if (chanaspointer && (*vartype == RTL_CHANNEL) && !issimplechan (nptr))
			*vartype = RTL_CHANPOINTER;
		/*{{{  access */
		*access = ((tptr != NULL) && (TagOf (tptr) == S_VALABBR || TagOf (tptr) == S_VALRETYPE) &&
			   isconst (DValOf (tptr))) ? RTL_CONST : isplaced (nptr) ? RTL_PLACED : ispointer (nptr) ? RTL_BYREF : RTL_BYVAL;
		/*}}} */
	}
	/*}}} */
	/*{{{  calculate offset */
	if (*access == RTL_PLACED) {
		int error;
		BIT32 offhi;
		BIT32 mostneg;

		I32ToI64 (&offhi, output_offset, NVOffsetOf (nptr));
		mostneg = (targetintsize == S_INT32) ? MOSTNEG_INT32 : MOSTNEG_INT16;
		Int64Sub (&error, &offhi, output_offset, offhi, *output_offset, 0xffffffffl, mostneg);
		Int64Div (&error, &offhi, output_offset, offhi, *output_offset, 0, bytesperword);
	} else if ((*vartype == RTL_TIMER) || (*vartype == RTL_CASETAG) || (*vartype == RTL_PROTOCOL)) {
		*access = RTL_VOIDACCESS;
		*output_offset = (*vartype == RTL_CASETAG) ? NTValueOf (nptr) : 0;
	} else if ((NVUseCountOf (nptr) == 0) && (TagOf (nptr) != N_PARAM) && (TagOf (nptr) != N_VALPARAM) && (TagOf (nptr) != N_RESULTPARAM)) {
/**output_offset = MOSTNEG_INT32;*//* Old method */
		*access = RTL_OPTIMISED;
		*output_offset = 0;
	} else if (*access == RTL_CONST)
		*output_offset = 0;	/* Will be filled in later by an addressfix */
	else
		*output_offset = NVOffsetOf (nptr) + workspace_adjust;
	/*}}} */
}

/*}}}*/

/*}}}*/
/*{{{  PRIVATE void gendebugstartfold(n)*/
/*****************************************************************************
 *
 *  gendebugstartfold generates a startfold record to the debug file
 *
 *****************************************************************************/
PRIVATE void gendebugstartfold (int n)
{
	if (minimal_debugoutput)
		return;
	if (debug_diagnostics) {
		fprintf (outfile, "%4d Startfold\n", debuginfo_index);
		debuginfo_index++;
		fprintf (outfile, "%4d Filename %s\n", debuginfo_index, lookupfilename (n));
	} else if (debug_to_file)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;
		*b++ = RTL_STARTFILE;
		b = addbuf_3L_num (b, debuginfo_index);
		add_to_debug_buffer (buffer, b - buffer, BUF_THROW);

		debuginfo_index++;
		b = buffer;
		*b++ = RTL_FILENAME;
		b = addbuf_3L_num (b, debuginfo_index);
		b = addbuf_3L_str (b, lookupfilename (n));
		add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
	}
	/*}}} */
	/* removed source_output 14/3/91 */
	/*else if (source_output)
	   add_to_file_list(debuginfo_index, n, FILE_START); */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugendfold()*/
/*****************************************************************************
 *
 *  gendebugendfold generates an endfold record to the debug file
 *
 *****************************************************************************/
PRIVATE void
gendebugendfold (void)
{
	if (minimal_debugoutput)
		return;
	if (debug_diagnostics)
		fprintf (outfile, "%4d Endfold\n", debuginfo_index);
	else if (debug_to_file)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_ENDFILE;
		b = addbuf_3L_num (b, debuginfo_index);
		add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
	}
	/*}}} */
	/* removed source_output 14/3/91 */
	/*else if (source_output)
	   add_to_file_list(debuginfo_index, 0, FILE_END); */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebuglinemark(line)*/
/*****************************************************************************
 *
 *  gendebuglinemark generates a linemark record to the debug file
 *
 *****************************************************************************/
PRIVATE void
gendebuglinemark (int line)
{
	if (minimal_debugoutput)
		return;
#if 0				/* This optimisation produces wrong debugging information */
	if ((line == last_linemark_line) && (debuginfo_index == (last_linemark_index + 1)))
		return;		/* INSdi02623 */
#endif

	if (debug_diagnostics)
		fprintf (outfile, "%4d Line mark : %d\n", debuginfo_index, line);
	else if (debug_to_file)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_LINEMARK;
		b = addbuf_3L_num (b, debuginfo_index);
		b = addbuf_3L_num (b, line);
		add_to_debug_buffer (buffer, b - buffer, BUF_THROW);

		last_linemark_index = debuginfo_index;
		last_linemark_line = line;
	}
	/*}}} */
	/* removed source_output 14/3/91 */
	/*else if (source_output)
	   add_to_linemark_list(debuginfo_index, line); */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void genstartfolds*/
/*****************************************************************************
 *
 *  genstartfolds generates a list of startfolds
 *
 *****************************************************************************/
PRIVATE void
genstartfolds (int startfile, int endfile)
{
	int nested_depth = 0;
	DEBUG_MSG (("genstartfolds: startfile %d, endfile %d\n", startfile, endfile));

	/*{{{  count nested depth */
	{
		int thisfile;
		for (thisfile = endfile; thisfile != startfile; thisfile = parentoffile (thisfile))
			nested_depth++;
	}
	/*}}} */

	/*{{{  move endfile out until it is start file */
	{
		int *filestack = memalloc (nested_depth * sizeof (int) + 1);	/* add one incase nested_depth = 0 */
		int *linestack = memalloc (nested_depth * sizeof (int) + 1);	/* add one incase nested_depth = 0 */
		int files = 0;
		int i;
		while (endfile != startfile) {
			filestack[files] = endfile;
			linestack[files] = parentposnoffile (endfile);
			endfile = parentoffile (endfile);
			files++;
		}
		for (i = files - 1; i >= 0; i--) {
			gendebuglinemark (linestack[i]);
			gendebugstartfold (filestack[i]);
		}
		memfree (linestack);
		memfree (filestack);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void gendebugfoldmarks(startfile, endfile)*/
/*****************************************************************************
 *
 *  gendebugfoldmarks generates the appropriate sequence of start folds and
 *                  end folds to move from 'startfile' to 'endfile'.
 *
 *****************************************************************************/
PRIVATE void
gendebugfoldmarks (int startfile, int endfile)
{
	DEBUG_MSG (("gendebugfoldmarks: startfile %d, endfile %d, nextfile %d\n", startfile, endfile, nextfile));
#if 0				/* This is the version that misses empty files */
	/*{{{  move start file out until it is a parent of endfile */
	while (!isaparentof (startfile, endfile)) {
		gendebugendfold ();
		startfile = parentoffile (startfile);
	}
	/*}}} */
	genstartfolds (startfile, endfile);
#else
	while (startfile != endfile)
		if ((endfile >= nextfile) && (nextfile < numberoffiles ()) && (isaparentof (startfile, nextfile))) {
			if (fileissource (nextfile)) {
				genstartfolds (startfile, nextfile);
				startfile = nextfile;
			} else {
				DEBUG_MSG (("gendebugfoldmarks: ignoring non-source file %d\n", nextfile));
			}
			nextfile++;
		} else if (isaparentof (startfile, endfile)) {
			genstartfolds (startfile, endfile);
			startfile = endfile;
		} else {
			DEBUG_MSG (("gendebugfoldmarks: endfold for %d\n", startfile));
			gendebugendfold ();
			startfile = parentoffile (startfile);
		}
#endif
	filenum = endfile;
}

/*}}}*/
/*{{{  PRIVATE void gendebuglocn(locn)*/
/*****************************************************************************
 *
 *  gendebuglocn generates a linemark record to the debug file
 *
 *****************************************************************************/
PRIVATE void
gendebuglocn (const SOURCEPOSN locn, treenode * const address)
{
	const int locnfile = FileNumOf (locn);
	const int locnline = FileLineOf (locn);

	/*add_to_index_table(address, minimal_debugoutput ? debuginfo_index - 1
	   : debuginfo_index); */
	if (locnfile != filenum)
		gendebugfoldmarks (filenum, locnfile);

	/* this moved to below the foldmark stuff - bug 1140 - 4/2/91 */
	add_to_index_table (address, minimal_debugoutput ? debuginfo_index - 1 : debuginfo_index);
	gendebuglinemark (locnline);
}

/*}}}*/
/*{{{  PRIVATE void gendebugscope()*/
/*****************************************************************************
 *
 *  gendebugscope generates a scope record to the debug file
 *
 *****************************************************************************/
PRIVATE void
gendebugscope (void)
{
	/* removed source_output 14/3/91 */
	if ( /*source_output || */ minimal_debugoutput || !symbolic_debugoutput)
		return;
	if (debug_diagnostics)
		fprintf (outfile, "%4d Scope\n", debuginfo_index);
	else if (debug_to_file) {
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_BEGSCOPE;
		b = addbuf_3L_num (b, debuginfo_index);
		add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
	}
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugendscope()*/
/*****************************************************************************
 *
 *  gendebugendscope generates an endscope record to the debug file
 *
 *****************************************************************************/
PRIVATE void
gendebugendscope (void)
{
	/* removed source_output 14/3/91 */
	if ( /*source_output || */ minimal_debugoutput || !symbolic_debugoutput)
		return;
	if (debug_diagnostics)
		fprintf (outfile, "%4d Endscope\n", debuginfo_index);
	else if (debug_to_file)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_ENDSCOPE;
		b = addbuf_3L_num (b, debuginfo_index);
		add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
	}
	/*}}} */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugvar(nptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  gendebugvar generates a variable record to the debug file
 *              Does not cope with constant tables properly
 *              Altered to handle placed variables 9.9.88
 *
 * variable     index vartype access dims { dims access offset } offset name
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void
gendebugvar (treenode * const nptr)
{
	int dims;
	INT32 vartype;
	INT32 access;
	BIT32 output_offset;

	/* removed source_output 14/3/91 */
	if ( /*source_output || */ minimal_debugoutput || !symbolic_debugoutput)
		return;

	calc_var_details (nptr, &dims, &vartype, &access, &output_offset);

	if (debug_to_file || debug_diagnostics)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		if (debug_diagnostics) {
			fprintf (outfile, "%4d Variable : %s %s ", debuginfo_index, vartypestr (vartype), accessstr (access));
			if (dims > 0)
				fprintf (outfile, "dims:%d ", dims);
		} else {
			*b++ = RTL_VARIABLE;
			b = addbuf_3L_num (b, debuginfo_index);
			b = addbuf_3L_num (b, vartype);
			b = addbuf_3L_num (b, access);
			b = addbuf_3L_num (b, dims);
		}
		/*{{{  write the dimensions */
		{
			treenode *type = NTypeOf (nptr);
			int i;
			for (i = 0; i < dims; i++) {
				treenode *dimexp = ARDimLengthOf (type);
				INT32 dimaccess, dimoffset;
				if (isconst (dimexp)) {
					dimaccess = RTL_CONST;
					dimoffset = ARDimOf (type);
				} else if ((nodetypeoftag (TagOf (dimexp)) == NAMENODE) &&	/* INSdi02610 */
					   (NLexLevelOf (nptr) == NLexLevelOf (dimexp))) {
					dimaccess = RTL_BYVAL;
					dimoffset = NVOffsetOf (dimexp) + workspace_adjust;
				} else if (((TagOf (dimexp) == S_ARRAYITEM) || (TagOf (dimexp) == S_RECORDITEM)) &&	/* INSdi02610 */
					   (ASExpOf (dimexp) == NULL)) {
					dimaccess = RTL_BYVAL;
					dimoffset = NVOffsetOf (nameof (dimexp)) + ASOffsetOf (dimexp) + workspace_adjust;
				} else {	/* No current way of describing this */
					/* bug TS/149, bug TS/1404 */ dimaccess = RTL_VOIDACCESS;
					dimoffset = 0;
				}
				if (debug_diagnostics)
					fprintf (outfile, "%d:%s:%d ", i, accessstr (dimaccess), dimoffset);
				else {
					b = addbuf_3L_num (b, dimaccess);
					b = addbuf_3L_num (b, dimoffset);
				}
				type = ARTypeOf (type);
			}
		}
		/*}}} */
		if (debug_diagnostics) {
			fprintf (outfile, "%d %s\n", output_offset, WNameOf (NNameOf (nptr)));
		} else {
			b = addbuf_3L_num (b, output_offset);
			b = addbuf_3L_str (b, WNameOf (NNameOf (nptr)));
			add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
		}
	}
	/*}}} */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugconstant_sub*/
PRIVATE void
gendebugconstant_sub (const char *const name, const INT32 vartype, const BIT32 hival, const BIT32 loval, const BOOL always_write_to_file)
{
	/* removed source_output 14/3/91 */
	if ( /*source_output || */ minimal_debugoutput || !symbolic_debugoutput) {
		if (!always_write_to_file)
			return;
	}

	if (debug_diagnostics)
		fprintf (outfile, "%4d Constant : %s %ld %ld %s\n", debuginfo_index, vartypestr (vartype), (long) hival, (long) loval, name);
	else if (debug_to_file) {
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_CONSTANT;
		b = addbuf_3L_num (b, debuginfo_index);
		b = addbuf_3L_num (b, vartype);
		b = addbuf_3L_num (b, hival);
		b = addbuf_3L_num (b, loval);
		b = addbuf_3L_str (b, name);
		add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
	}
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugconstant(tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  gendebugconstant generates a constant record to the debug file
 *
 * constant     index vartype hi lo name
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void
gendebugconstant (treenode * tptr)
{
	treenode *const nptr = DNameOf (tptr);
	if (NUsedOf (nptr)) {
		/*treenode *const type_tree = basetype_tree(NTypeOf(nptr)); */
		treenode *const type_tree = NTypeOf (nptr);
		/*const int type = TagOf(type_tree); */
		const INT32 vartype = vartypeof (type_tree);
		treenode *const cptr = DValOf (tptr);
		BIT32 hival = HiValOf (cptr);

		if (bytesinscalar_tree (type_tree) <= 4)	/* set high word to zero if not required */
			hival = 0;

		gendebugconstant_sub (WNameOf (NNameOf (nptr)), vartype, hival, LoValOf (cptr), FALSE);
	}
}

/*}}}*/
/*{{{  PRIVATE void gendebugworkspace(segtype, thisoffset, prevoffset)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  gendebugworkspace generates a workspace record to the debug file
 *
 * workspace    index segtype thisoffset prevoffset
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void
gendebugworkspace (INT32 segtype, INT32 thisoffset, INT32 prevoffset)
{
	/* removed source_output 14/3/91 */
	/*if (source_output)
	   return; */
	if (debug_diagnostics)
		fprintf (outfile, "%4d Workspace : %s %ld %ld\n", debuginfo_index, segtypestr (segtype), (long) thisoffset, (long) prevoffset);
	else if (debug_to_file)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_WORKSPACE;
		b = addbuf_3L_num (b, debuginfo_index);
		b = addbuf_3L_num (b, segtype);
		b = addbuf_3L_num (b, thisoffset);
		b = addbuf_3L_num (b, prevoffset);
		add_to_debug_buffer (buffer, b - buffer, BUF_KEEP);
	}
	/*}}} */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugendseg()*/
/*****************************************************************************
 *
 *  gendebugendseg generates an endseg record to the debug file
 *
 *****************************************************************************/
PRIVATE void
gendebugendseg (void)
{
	/* removed source_output 14/3/91 */
	/*if (source_output)
	   return; */
	if (debug_diagnostics)
		fprintf (outfile, "%4d Endseg\n", debuginfo_index);
	else if (debug_to_file)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;
		*b++ = RTL_ENDSEG;
		b = addbuf_3L_num (b, debuginfo_index);
		add_to_debug_buffer (buffer, b - buffer, BUF_KEEP);
	}
	/*}}} */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugaltstart()*/
/*****************************************************************************
 *
 *  gendebugaltstart generates an altstart record to the debug file
 *
 *****************************************************************************/
PRIVATE void
gendebugaltstart (void)
{
	if (debug_diagnostics)
		fprintf (outfile, "%4d Altstart\n", debuginfo_index);
	else if (debug_to_file)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;
		*b++ = RTL_ALTSTART;
		b = addbuf_3L_num (b, debuginfo_index);
		add_to_debug_buffer (buffer, b - buffer, BUF_KEEP);
	}
	/*}}} */
	alt_starting = TRUE;
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugaltgo()*/
/*****************************************************************************
 *
 *  gendebugaltgo generates an altgo record to the debug file
 *
 *****************************************************************************/
PRIVATE void
gendebugaltgo (void)
{
	if (debug_diagnostics)
		fprintf (outfile, "%4d Altgo\n", debuginfo_index);
	else if (debug_to_file)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;
		*b++ = RTL_ALTGO;
		b = addbuf_3L_num (b, debuginfo_index);
		add_to_debug_buffer (buffer, b - buffer, BUF_KEEP);
	}
	/*}}} */
	alt_starting = FALSE;
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugversion()*/
/*****************************************************************************
 *
 *  gendebugversion generates a version record to the debug file
 *
 *****************************************************************************/
PRIVATE void
gendebugversion (void)
{
	int debug_version = DEBUG_VERSION;
	if (minimal_debugoutput)
		debug_version |= RTL_COMPACTED_BIT;
	if (iobycall)
		debug_version |= RTL_VIRTUAL_LINK_BIT;
	/* removed source_output 14/3/91 */
	/*if (source_output)
	   return; */
	if (debug_diagnostics)
		fprintf (outfile, "%4d Version : #%08X\n", debuginfo_index, debug_version);
	else if (debug_to_file)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_VERSION;
		b = addbuf_3L_num (b, debuginfo_index);
		b = addbuf_3L_num (b, debug_version);
		/*write_debug_string(buffer, b - buffer); */
		add_to_debug_buffer (buffer, b - buffer, BUF_KEEP);
	}
	/*}}} */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void gendebugproc*/
/*****************************************************************************
 *
 *  gendebugproc generates a proedure record to the debug file
 *
 * procedure    index proctype dummy workspace vectorspace name
 *
 *****************************************************************************/
PRIVATE void
gendebugproc (treenode * const nptr)
{
	int proctype;
	INT32 wsusage, vsusage;
	const char *const name = WNameOf (NNameOf (nptr));
	const BOOL allow_fpu_call = configuring;
	BOOL is_external = FALSE;

	if (			/*source_output       || *//* removed source_output 14/3/91 */
/*minimal_debugoutput || *//* removed minimal_debugoutput 21/10/91 bug TS/1434 */
		   !symbolic_debugoutput)
		return;
	if (separatelycompiled (nptr) && (minimal_debugoutput ||	/* bug TS/1434 22/10/91 */
					  !compiledforcorrectproc (nptr, allow_fpu_call)))
		return;
	/*{{{  set up proctype */
	switch (TagOf (nptr)) {
	case N_PROCDEF:
	case N_MPROCDECL:
		proctype = RTL_PROCFLAG;
		break;
	case N_SFUNCDEF:
	case N_LFUNCDEF:
		proctype = RTL_FUNCTIONFLAG;
		break;
		/*case N_SCPROCDEF:  proctype = RTL_SCPROCFLAG; is_external = TRUE; break; */
		/*case N_SCFUNCDEF:  proctype = RTL_SCFUNCTIONFLAG;  is_external = TRUE; break; */
	case N_LIBPROCDEF:
	case N_LIBMPROCDECL:
		proctype = RTL_LIBPROCFLAG;
		is_external = TRUE;
		break;
	case N_LIBFUNCDEF:
		proctype = RTL_LIBFUNCTIONFLAG;
		is_external = TRUE;
		break;
	default:
		proctype = 0;
		break;
	}
	/*}}} */
	if (!is_external || NUsedOf (nptr)) {
		getprocwsandvs (nptr, &wsusage, &vsusage);
		if (debug_diagnostics)
			/*{{{  write text to output file */
			fprintf (outfile, "%4d Procedure : %s dummy %d %d %s\n", debuginfo_index, proctypestr (proctype), wsusage, vsusage, name);
		/*}}} */
		else if (debug_to_file)
			/*{{{  write record to object file */
		{
			char buffer[MAX_DEBUG_STRING_SIZE];
			char *b = buffer;
			*b++ = RTL_PROCEDURE;
			b = addbuf_3L_num (b, debuginfo_index);
			b = addbuf_3L_num (b, proctype);
			b = addbuf_3L_num (b, 0);	/* address - filled in later by addressfix */
			b = addbuf_3L_num (b, wsusage);
			b = addbuf_3L_num (b, vsusage);
			b = addbuf_3L_str (b, name);

			/* write_debug_string(buffer, b - buffer); */
			add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
		}
		/*}}} */
		debuginfo_index++;
	}
}

/*}}}*/
/*{{{  PRIVATE void generate_sub_types*/
#ifdef OCCAM2_5
/*{{{  PRIVATE void generate_array_typedef*/
PRIVATE void
generate_array_typedef (treenode * const tptr)
{
	if (minimal_debugoutput || !symbolic_debugoutput)
		return;

	if (debug_to_file || debug_diagnostics)
		/*{{{  write record to object file */
	{
		const int typedef_number = get_from_index_table (tptr);
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;
		if (debug_diagnostics) {
			fprintf (outfile, "%4d Typedef : <anon>", debuginfo_index);
		} else {
			*b++ = RTL_TYPEDEF;
			b = addbuf_3L_num (b, debuginfo_index);
			b = addbuf_3L_num (b, typedef_number);
			b = addbuf_3L_str (b, "");
			b = addbuf_3L_num (b, RTL_ARRAY);
			/*b = addbuf_3L_num(b, vartypeof(DValOf(NDeclOf(nptr)))); */
			b = addbuf_3L_num (b, vartypeof (ARTypeOf (tptr)));
			b = addbuf_3L_num (b, ARDimOf (tptr));
			add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
		}
	}
	/*}}} */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PRIVATE void generate_sub_types*/
PRIVATE void
generate_sub_types (treenode * const tptr)
{
	switch (TagOf (tptr)) {
	case S_ARRAY:
		generate_sub_types (ARTypeOf (tptr));
		add_to_index_table (tptr, debug_typedef_number);
		debug_typedef_number++;
		generate_array_typedef (tptr);
		break;
	case S_RECORD:
		/*{{{  do each field of the record */
		{
			treenode *decl;
			for (decl = ARTypeOf (tptr); decl != NULL; decl = DBodyOf (decl)) {
				treenode *const field_nptr = DNameOf (decl);
				generate_sub_types (NTypeOf (field_nptr));
			}
		}
		/*}}} */
		break;
	default:
		break;
	}
}

/*}}}*/
#endif
/*}}}*/
/*{{{  PRIVATE void gendebugtypedef*/
#ifdef OCCAM2_5
PRIVATE void
gendebugtypedef (treenode * const nptr)
{
	if (minimal_debugoutput || !symbolic_debugoutput)
		return;

	if (debug_to_file || debug_diagnostics)
		/*{{{  write record to object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;
		treenode *const type = NTypeOf (nptr);

		/*{{{  sub types */
		switch (TagOf (type)) {
		case S_RECORD:
			generate_sub_types (type);
			break;
		case S_ARRAY:
			generate_sub_types (ARTypeOf (type));
			break;
		default:
			break;
		}
		/*}}} */
		SetNVOffset (nptr, debug_typedef_number);
		debug_typedef_number++;

		if (debug_diagnostics) {
			fprintf (outfile, "%4d Typedef : %s (%d)\n", debuginfo_index, WNameOf (NNameOf (nptr)), NVOffsetOf (nptr));
		} else {
			*b++ = RTL_TYPEDEF;
			b = addbuf_3L_num (b, debuginfo_index);
			b = addbuf_3L_num (b, NVOffsetOf (nptr));
			b = addbuf_3L_str (b, WNameOf (NNameOf (nptr)));
			switch (TagOf (type)) {
				/*{{{  RECORD */
			case S_RECORD:
				{
					int elts = 0;
					treenode *decl;
					for (decl = ARTypeOf (type); decl != NULL; decl = DBodyOf (decl))
						elts++;
					b = addbuf_3L_num (b, RTL_RECORD);
					b = addbuf_3L_num (b, bytesin (type));
					b = addbuf_3L_num (b, elts);
				}
				break;
				/*}}} */
				/*{{{  ARRAY */
			case S_ARRAY:
				b = addbuf_3L_num (b, RTL_ARRAY);
				b = addbuf_3L_num (b, vartypeof (ARTypeOf (type)));
				b = addbuf_3L_num (b, ARDimOf (type));
				break;
				/*}}} */
				/*{{{  default - ALIAS */
			default:
				b = addbuf_3L_num (b, RTL_ALIAS);
				b = addbuf_3L_num (b, vartypeof (type));
				b = addbuf_3L_num (b, 0);
				break;
				/*}}} */
			}
			add_to_debug_buffer (buffer, b - buffer, BUF_THROW);

			if (TagOf (type) == S_RECORD)
				/*{{{  do each field of the record */
			{
				treenode *decl;
				for (decl = ARTypeOf (type); decl != NULL; decl = DBodyOf (decl)) {
					treenode *const field_nptr = DNameOf (decl);
					debuginfo_index++;
					b = buffer;
					*b++ = RTL_RECORDMEMBER;
					b = addbuf_3L_num (b, debuginfo_index);
					b = addbuf_3L_num (b, vartypeof (NTypeOf (field_nptr)));
					b = addbuf_3L_str (b, WNameOf (NNameOf (field_nptr)));
					b = addbuf_3L_num (b, NVOffsetOf (field_nptr) * 8);
					b = addbuf_3L_num (b, bytesin (NTypeOf (field_nptr)) * 8);
					add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
				}
			}
			/*}}} */
		}
	}
	/*}}} */
	debuginfo_index++;
}
#endif
/*}}}*/
/*{{{  PRIVATE treenode *gendebugspecs(tptr)*/
PRIVATE treenode *
gendebugspecs (treenode * tptr)
{
	while (isspecification (tptr)) {
		SOURCEPOSN endlocn = NOPOSN;
		treenode *nptr = DNameOf (tptr);
		/*{{{  set line mark if required */
		if (!separatelycompiled (nptr)) {
			switch (TagOf (tptr)) {
			case S_VALABBR:
			case S_ABBR:
			case S_VALRETYPE:
			case S_RETYPE:
				if ((NUsedOf (nptr)) || (TagOf (DValOf (tptr)) != S_CONSTEXP))
					gendebuglocn (LocnOf (tptr), tptr);
				break;
			default:
				gendebuglocn (LocnOf (tptr), tptr);
				break;
			}
		}
		/*}}} */
		switch (TagOf (tptr)) {
			/*{{{  decl */
		case S_DECL:
			{
				treenode *n = nptr;
				if (TagOf (n) == S_LIST)
					for (; !EndOfList (n); n = NextItem (n)) {
						/* add_to_index_table( ThisItem(n), debuginfo_index); */
						gendebugvar (ThisItem (n));
				} else {
					/* add_to_index_table( n, debuginfo_index); */
					gendebugvar (n);
				}
				gendebug (DValOf (tptr));	/* bug TS/1263 11/05/92 */
			}
			break;
			/*}}} */
			/*{{{  abbrev retype */
		case S_VALABBR:
		case S_ABBR:
		case S_VALRETYPE:
		case S_RETYPE:
			if (	/* ((TagOf(tptr) == S_VALABBR) || (TagOf(tptr) == S_VALRETYPE)) && */
				   (TagOf (DValOf (tptr)) == S_CONSTEXP)) {
				/* add_to_index_table(nptr), debuginfo_index); */
				gendebugconstant (tptr);
			} else {
				gendebug (DValOf (tptr));
				add_to_index_table (nptr, debuginfo_index);
				gendebugvar (nptr);
			}
			break;
			/*}}} */
			/*{{{  procedure function */
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
			{
				if (!isinline (nptr)) {
					const INT32 savedoffset = enclosing_this_offset;
					const INT32 savedworkspace = workspace_adjust;
					add_to_index_table (nptr, debuginfo_index);
					gendebugproc (nptr);
					if (!separatelycompiled (nptr))
#if 0
						/* This was removed by CO'N to change the debug info */
						/* It is now done by a PROC w/s record with value 0 */
						/* followed by a SEQuential w/s record with the correct ws size */
						/* This makes it easier to set a breakpoint at procedure entry */
						/*{{{  do procedure parameters */
					{
						gendebugworkspace (RTL_SEGTYPE_PROC, NPMaxwspOf (nptr), 0);
						enclosing_this_offset = NPMaxwspOf (nptr);
						workspace_adjust = 0;
						/*{{{  generate information for the parameters */
						{
							treenode *n;
							for (n = NParamListOf (nptr); !EndOfList (n); n = NextItem (n)) {
								treenode *thisparam = ThisItem (n);
								if (!ishiddenparam (thisparam)) {
									/* add_to_index_table( thisparam,
									   debuginfo_index); */
									gendebugvar (thisparam);
								}
							}
						}
						/*}}} */
						gendebug (DValOf (tptr));	/* Do the routine body */
						gendebugendseg ();
						enclosing_this_offset = savedoffset;
						workspace_adjust = savedworkspace;
					}
					/*}}} */
#else
						/*{{{  do parameters and body */
					{
						endlocn = NPEndposnOf (nptr);
						gendebugworkspace (RTL_SEGTYPE_PROC, 0, 0);
						enclosing_this_offset = NPMaxwspOf (nptr);
						workspace_adjust = -enclosing_this_offset;
						/*{{{  generate information for the parameters */
						{
							treenode *n;
							for (n = NParamListOf (nptr); !EndOfList (n); n = NextItem (n)) {
								treenode *thisparam = ThisItem (n);
								if (!ishiddenparam (thisparam)) {
									/* add_to_index_table( thisparam,
									   debuginfo_index); */
									gendebugvar (thisparam);
								}
							}
						}
						/*}}} */
						/* Note the entry point of the proc/function */
						/* We need another hashed value to be able to retrieve this debuginfo_index */
						/* Because nptr has already been used, we use the type field */
						/*gendebuglocn(LocnOf(tptr), NTypeOf(nptr)); */
						/* bug TS/2047 18/01/93 - use the address of NTypeOf slot */
						gendebuglocn (LocnOf (tptr), (treenode *) NTypeAddr (nptr));
						if (enclosing_this_offset != 0) {
							gendebugworkspace (RTL_SEGTYPE_SEQ, enclosing_this_offset, 0);
							workspace_adjust = 0;
							/* note the entry point after the ajw */
							gendebuglocn (LocnOf (tptr), NPConstTablesOf (nptr));
						}
						gendebug (DValOf (tptr));	/* Do the routine body */
						gendebuglocn (endlocn, (treenode *) ((PTRINT) DValOf (tptr) + 1));
						if (enclosing_this_offset != 0)
							gendebugendseg ();
						enclosing_this_offset = savedoffset;
						workspace_adjust = savedworkspace;
						gendebugendseg ();	/* moved here by MDP to get RET inside */
					}
					/*}}} */
#endif
				}
			}
			break;
			/*}}} */
			/*{{{  protocol definition */
		case S_SPROTDEF:
			add_to_index_table (nptr, debuginfo_index);
			gendebugvar (nptr);
			break;
		case S_TPROTDEF:
			{
				treenode *n = nptr;
				gendebugvar (n);
#ifdef OCCAM2_5
				gendebugscope ();
#endif
				n = NTypeOf (n);
				while (!EndOfList (n)) {
					treenode *thistag = ThisItem (n);
					/* add_to_index_table( thistag, debuginfo_index); */
					gendebugvar (thistag);
					n = NextItem (n);
				}
#ifdef OCCAM2_5
				gendebugendscope ();
#endif
			}
			break;
			/*}}} */
			/*{{{  type declaration */
#ifdef OCCAM2_5
		case S_TYPEDECL:
#if MOBILES
		case S_FORWDECL:
#endif
			gendebugtypedef (nptr);
			break;
#endif
			/*}}} */
		default:
			break;
		}
		tptr = DBodyOf (tptr);
	}
	return tptr;
}

/*}}}*/
/*{{{  PRIVATE void gendebugalt (tptr, enabling, replcount)*/
/*****************************************************************************
 *
 *  gendebugalt takes a parse tree, for an ALT 'tptr' and generates the symbolic
 *           debugging information for it.
 *
 *****************************************************************************/
PRIVATE void
gendebugalt (treenode * tptr, const BOOL enabling, const int replcount)
{
	/*{{{  create linemark if required */
	/* specification nodes sometimes don't need linemarks so the specification
	   case of the switch worries about this */
	if (!isspecification (tptr))
		gendebuglocn (LocnOf (tptr), tptr);
	/*}}} */
	switch (TagOf (tptr))
		/*{{{  cases */
	{
		/*{{{  specification */
	default:		/* Specification */
		DEBUG_MSG (("gendebugalt: specification\n"));
		gendebugscope ();
		tptr = gendebugspecs (tptr);
		gendebugalt (tptr, enabling, replcount);
		gendebugendscope ();
		return;
		/*}}} */
		/*{{{  ALT PRIALT */
	case S_ALT:
	case S_PRIALT:
		DEBUG_MSG (("gendebugalt: ALT\n"));
		for (tptr = CBodyOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr))
			gendebugalt (ThisItem (tptr), enabling, replcount);
		return;
		/*}}} */
		/*{{{  REPLALT PRIREPLALT */
	case S_REPLALT:
	case S_PRIREPLALT:
		DEBUG_MSG (("gendebugalt: repl ALT\n"));
		{
			gendebug (ReplCStartExpOf (tptr));
			gendebug (ReplCLengthExpOf (tptr));
			gendebug (ReplCStepExpOf (tptr));
			/*{{{  do the replicated process */
			{
				treenode *const replicator = ReplCNameOf (tptr);
				const INT32 reploffset = NVOffsetOf (replicator);
				const INT32 base = NVOffsetOf (ReplCTempOf (tptr));
				SetNVOffset (replicator, enabling ? reploffset : (base + replcount));
				gendebugscope ();
				gendebugvar (replicator);
				gendebugalt (ReplCBodyOf (tptr), enabling, replcount + 1);
				gendebugendscope ();
				SetNVOffset (replicator, reploffset);
			}
			/*}}} */
		}
		return;
		/*}}} */
		/*{{{  ALTERNATIVE */
	case S_ALTERNATIVE:
		{
			treenode *input = AltInputOf (tptr);
			BOOL endscope = FALSE;
			DEBUG_MSG (("gendebugalt: ALTERNATIVE\n"));
			if (isspecification (input)) {
				gendebugscope ();
				endscope = TRUE;
				input = gendebugspecs (input);
			}
			if (enabling) {
				insidealtguard = TRUE;	/* bug TS/1443 05/11/92 */

				gendebug (AltGuardOf (tptr));
#if 0				/* bug 779 2/11/90 */
				switch (inputtypeof (input)) {
				default:	/* Any channel or port input */
					gendebug (LHSOf (input));	/* channel */
					break;
				case INP_SKIP:
				case INP_DELAYED_INPUT:
					break;
				}
#else
				gendebug (AltChanExpOf (tptr));	/* bug 779 2/11/90 */
				gendebug (AltTimeExpOf (tptr));	/* bug INSdi01967 05/05/93 */
#endif

				insidealtguard = FALSE;
			} else {
				gendebug (input);
				gendebug (AltBodyOf (tptr));
			}
			if (endscope)
				gendebugendscope ();
		}
		return;
		/*}}} */
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void gendebug (tptr)*/
/*****************************************************************************
 *
 *  gendebug takes a parse tree, 'tptr' and generates the symbolic
 *           debugging information for it.
 *           The file table and the code offsets are output separately.
 *
 *****************************************************************************/
PRIVATE void
gendebug (treenode * tptr)
{
	while (tptr != NULL) {
		/*{{{  create linemark if required */
		/* specification nodes sometimes don't need linemarks so the specification
		   case of the switch worries about this */
		if (need_locate (TagOf (tptr)) && !isspecification (tptr))
			gendebuglocn (LocnOf (tptr), tptr);
		/*}}} */
		switch (TagOf (tptr))
			/*{{{  cases */
		{
		default:
			return;
			/*{{{  STOP SKIP SUSPEND GUYCODE GUYSTEP */
		case S_STOP:
			/*{{{  comment */
			/* STOP creates an explicit linemark because it requires locate but is not
			   included in need_locate(...), This is because the code to produce
			   codemarks, tstop() in gen1, produces its own explicit codemarks
			   independent of need_locate to prevent duplicate codemarks being produced
			   for explicit STOP statements */
			/*}}} */
			gendebuglocn (LocnOf (tptr), tptr);
			return;
		case S_SKIP:
		case S_CONSTEXP:
		case S_SUSPEND:
			/* Case S_LABELDEF just returns */
			return;
			/*}}} */
			/*{{{  SEQ IF GUY ASM ALT PRIALT */
		case S_SEQ:
		case S_IF:
		case S_GUY:
		case S_ASM:
			tptr = CBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  REPLSEQ REPLIF */
		case S_REPLSEQ:
		case S_REPLIF:
			/* add_to_index_table( ReplCNameOf(tptr), debuginfo_index); */
			gendebugscope ();
			gendebugvar (ReplCNameOf (tptr));
			gendebug (ReplCStartExpOf (tptr));
			gendebug (ReplCLengthExpOf (tptr));
			gendebug (ReplCStepExpOf (tptr));
			gendebug (ReplCBodyOf (tptr));
			gendebugendscope ();
			gendebuglocn (LocnOf (tptr), ReplCNameOf (tptr));	/* bug TS/1434 10/03/92 */
			return;
			/*}}} */
			/*{{{  ALTs of any kind */
		case S_ALT:
		case S_PRIALT:
		case S_REPLALT:
		case S_PRIREPLALT:
			set_alt_ds (tptr);	/* bug TS/1443 05/11/92 */
			gendebugaltstart ();	/* MDP */
			gendebugalt (tptr, TRUE, 0);	/* enabling guards and channels */
			gendebugaltgo ();	/* MDP */
			gendebugalt (tptr, FALSE, 0);	/* inputs and bodies */
			return;
			/*}}} */
			/*{{{  PAR PRIPAR */
		case S_PAR:
		case S_PRIPAR:
			{
				INT32 parsize = ((TagOf (tptr) == S_PRIPAR) && (code_style_flags & CODE_STYLE_ALT_PRI_PAR)) ? DS_IO : 0;
				INT32 segment_type = (TagOf (tptr) == S_PRIPAR) ? RTL_SEGTYPE_PRIPAR : RTL_SEGTYPE_PAR;
				treenode *const debug_tptr = tptr;
				for (tptr = CBodyOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr))
					/*{{{  do a branch of the PAR */
				{
					treenode *sp = ThisItem (tptr);
					const INT32 thisbranchwsp = SpMaxwspOf (sp);
					const INT32 thisbranchsize = SpDatasizeOf (sp);
					const INT32 savedoffset = enclosing_this_offset;
					const INT32 savedworkspace = workspace_adjust;
					enclosing_this_offset += parsize + thisbranchwsp;
					workspace_adjust += parsize + thisbranchwsp;
					gendebugworkspace (segment_type, enclosing_this_offset, 0);

					/* par size: */
					gendebugconstant_sub ("$ps", RTL_INT64, thisbranchwsp, thisbranchsize, TRUE);	/* bug TS/1996 09/12/92 */

					/*{{{  make sure only first branch of PRI PAR is marked as PRI */
					segment_type = RTL_SEGTYPE_PAR;
					/*}}} */
					gendebug (SpBodyOf (sp));	/* do the PAR branch */
					gendebugendseg ();
					enclosing_this_offset = savedoffset;
					workspace_adjust = savedworkspace;
					parsize += thisbranchsize;
				}
				/*}}} */
				if (CBodyOf (debug_tptr) != NULL)	/* bug TS/2030 13/01/93 */
					gendebuglocn (LocnOf (debug_tptr), CBodyOf (debug_tptr));	/* bug TS/1434 10/03/92 */
			}
			return;
			/*}}} */
			/*{{{  REPLPAR */
		case S_REPLPAR:
			{
				treenode *const sp = ReplCBodyOf (tptr);
				treenode *const replicator = ReplCNameOf (tptr);
				const INT32 thisbranchwsp = SpMaxwspOf (sp);
				const INT32 savedoffset = enclosing_this_offset;
				const INT32 prev_offset = enclosing_this_offset;
				const INT32 savedworkspace = workspace_adjust;
				const int replparslots = (SpVSUsageOf (sp) == 0) ? MIN_REPLPAR_SPECIALS : MIN_REPLPAR_SPECIALS + 1;
				gendebugscope ();
				gendebugvar (replicator);
				gendebug (ReplCStartExpOf (tptr));
				gendebug (ReplCLengthExpOf (tptr));
				gendebug (ReplCStepExpOf (tptr));
				/* gendebugendscope(); */
				enclosing_this_offset = thisbranchwsp - replparslots + REPLPAR_STATICLINK;
				gendebugworkspace (RTL_SEGTYPE_REPL, enclosing_this_offset, prev_offset);

				/* par size: */
				gendebugconstant_sub ("$ps", RTL_INT64, enclosing_this_offset + 1, SpDatasizeOf (sp), TRUE);	/* bug TS/1996 09/12/92 */
				/* par count: */
				gendebugconstant_sub ("$pc", RTL_INTEGER, 0, LoValOf (ReplCLengthExpOf (tptr)), TRUE);	/* bug TS/1434 12/03/92 */

				/* step count: */
				gendebugconstant_sub ("$pt", RTL_INTEGER, 0, LoValOf (ReplCStepExpOf (tptr)), TRUE);

				workspace_adjust = 0;
				/*{{{  send replicator debug info. */
				{
					INT32 offset = NVOffsetOf (replicator);
					SetNVOffset (replicator, thisbranchwsp - replparslots + REPLPAR_REPLICATOR);
					/* gendebugscope(); */
					gendebugvar (replicator);
					SetNVOffset (replicator, offset);
				}
				/*}}} */
				gendebug (SpBodyOf (sp));
				/* gendebugendscope(); */
				gendebugendseg ();
				gendebugendscope ();
				enclosing_this_offset = savedoffset;
				workspace_adjust = savedworkspace;
				gendebuglocn (LocnOf (tptr), sp);	/* bug TS/1434 10/03/92 */
			}
			return;
			/*}}} */
			/*{{{  WHILE CHOICE SELECTION */
		case S_WHILE:
		case S_CHOICE:
		case S_SELECTION:
		case S_VARIANT:
			gendebug (CondGuardOf (tptr));
			tptr = CondBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  FINSTANCE PINSTANCE */
		case S_FINSTANCE:
		case S_PINSTANCE:
			if (insidealtguard) {	/* bug TS/1443 05/11/92 */
				const INT32 altadjust = get_altadjust (tptr);
				if (altadjust != 0) {
					gendebugworkspace (RTL_SEGTYPE_SEQ, altadjust, 0);
					workspace_adjust += altadjust;
					gendebuglocn (LocnOf (tptr), tptr);
					gendebug (IParamListOf (tptr));
					gendebugendseg ();
					return;
				}
			}
			if (TagOf (tptr) == S_FINSTANCE)	/* bug TS/1443 05/11/92 */
				gendebuglocn (LocnOf (tptr), tptr);
			tptr = IParamListOf (tptr);
			break;
			/*}}} */
			/*{{{  ASS OUTPUT INPUT TAGGED_INPUT DELAYED_INPUT CASE_INPUT CASE */
		case S_ASS:
		case S_OUTPUT:
		case S_INPUT:
		case S_TAGGED_INPUT:
		case S_DELAYED_INPUT:
		case S_CASE_INPUT:
		case S_CASE:
			gendebug (LHSOf (tptr));	/* Added to fix bug 778 29/10/90 */
			tptr = RHSOf (tptr);
			break;
			/*}}} */
			/*{{{  LIST */
		case S_LIST:
			gendebug (ThisItem (tptr));
			tptr = NextItem (tptr);
			break;
			/*}}} */
			/*{{{  monadic operator node */
		case S_MOSTPOS:
		case S_MOSTNEG:
		case S_NEG:
		case S_BITNOT:
		case S_UMINUS:
		case S_NOT:
		case S_SIZE:
		case S_VAL:
			/*case S_VAR: */
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			tptr = OpOf (tptr);
			break;
			/*}}} */
			/*{{{  CONSTRUCTOR */
#if 0				/* no constructors are passed to the backend any more */
		case S_CONSTRUCTOR:
			tptr = LitExpOf (tptr);
			break;
#endif
			/*}}} */
			/*{{{  dyadic operator node */
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
		case S_CSUB0:
		case S_CCNT1:
		case S_EVAL:	/* added 9/10/90 bug 1007 */
		case S_GUYCODE:	/* added 28/04/93 bug INSdi02058 */
		case S_GUYSTEP:	/* added 28/04/93 bug INSdi02058 */
			gendebug (LeftOpOf (tptr));
			tptr = RightOpOf (tptr);
			break;
		case S_AFTER:
			assert (FALSE);	/* after is processed in trans */
			break;
			/*}}} */
			/*{{{  ARRAYSUB ARRAYITEM */
		case S_ARRAYSUB:
		case S_ARRAYITEM:
		case S_RECORDSUB:
		case S_RECORDITEM:
			gendebug (ASExpOf (tptr));
			tptr = ASBaseOf (tptr);
			break;
			/*}}} */
			/*{{{  valof node */
		case S_VALOF:
			gendebug (VLBodyOf (tptr));
			tptr = VLResultListOf (tptr);
			gendebuglocn (LocnOf (tptr), tptr);	/* linemark for RESULT line */
			break;
			/*}}} */
			/*{{{  specification node */
		case S_DECL:
		case S_VALABBR:
		case S_ABBR:
		case S_VALRETYPE:
		case S_RETYPE:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_SPROTDEF:
		case S_TPROTDEF:
		case S_PRAGMA:	/* bug 829 19/9/91 */
		case S_TYPEDECL:
#if MOBILES
		case S_FORWDECL:
#endif
			gendebugscope ();
			tptr = gendebugspecs (tptr);
			gendebug (tptr);
			gendebugendscope ();
			return;
			/*}}} */
			/*{{{  PLACE WSPLACE VSPLACE */
		case S_PLACE:
		case S_WSPLACE:
		case S_VSPLACE:
			tptr = DBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  segment node */
		case S_SEGMENTITEM:
		case S_SEGMENT:
			/* debuginfo_index++; */
			gendebug (SNameOf (tptr));
			gendebug (SStartExpOf (tptr));
			gendebug (SLengthExpOf (tptr));
			gendebug (SCheckExpOf (tptr));	/* added 9/10/90 bug 1007 */
			if (TagOf (tptr) == S_SEGMENT)
				return;
			tptr = SSubscriptExpOf (tptr);
			break;
			/*}}} */
			/*{{{  temp node */
		case T_TEMP:
		case T_REGTEMP:
		case T_PREEVALTEMP:
			tptr = NDeclOf (tptr);
			break;
			/*}}} */
			/*{{{  space usage node */
		case S_SPACEUSAGE:
			tptr = SpBodyOf (tptr);
			break;
			/*}}} */
			/* namenodes just return */
		}
		/*}}} */
	}
	return;
}

/*}}}*/
/*{{{  PUBLIC void debug_gencodemark (address, offset)*/
/*****************************************************************************
 *
 * debug_gencodemark generates a codemark record to the debug file
 *
 * codemark     index ptr offset
 *
 *****************************************************************************/
PUBLIC void
debug_gencodemark (const treenode * const address, const BIT32 offset)
/* address is NULL iff this marks end of PROC body (MDP) */
{
	const int line = (address == NULL) ? 0 : get_from_index_table (address);

	/*printf("debug_gencodemark: offset is %d\n", offset); */

	if ((line == last_codemark_line) && (offset <= last_codemark_offset))
		return;		/* bug TS/1870 27/10/92 */

	if (debug_diagnostics)
		fprintf (outfile, "%4d Codemark : %d %ld\n", debuginfo_index, line, (long) offset);
	else if (debug_to_file)
		/*{{{  write a record to the object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_CODEMARK;
		b = addbuf_3L_num (b, debuginfo_index);
		b = addbuf_3L_num (b, line);
		b = addbuf_3L_num (b, offset);
		add_to_debug_buffer (buffer, b - buffer, BUF_KEEP);
	}
	/*}}} */
	debuginfo_index++;

	last_codemark_line = line;
	last_codemark_offset = offset;
}

/*}}}*/
/*{{{  PUBLIC void debug_genlibrpatch (line, offset)*/
/*****************************************************************************
 *
 * debug_genlibrpatch  generates a libpatch record to the debug file
 *
 * libpatch     index address
 *
 * N.B. The 'line' parameter of this procedure is not required by the
 *      debugger and so is not included in the libpatch record. It is left
 *      in the parameter list to accomodate future changes
 *      Andy Whitlow - 8.9.88
 *
 *****************************************************************************/
PUBLIC void
debug_genlibrpatch (const treenode * address, const BIT32 offset)
{
	USE_VAR (address);	/* stop unused variable warning */

	if (debug_diagnostics)
		fprintf (outfile, "%4d Libpatch : %ld\n", debuginfo_index, (long) offset);
	else if (debug_to_file)
		/*{{{  write a record to the object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_LIBPATCH;
		b = addbuf_3L_num (b, debuginfo_index);
		/* b = addbuf_3L_num(b, line);  don't need line for current debugger */
		b = addbuf_3L_num (b, offset);
		add_to_debug_buffer (buffer, b - buffer, BUF_KEEP);
	}
	/*}}} */
	debuginfo_index++;
}

/*}}}*/
/*{{{  PUBLIC void debug_genaddrfix (address, offset)*/
/*****************************************************************************
 *
 * debug_genaddrfix generates a addressfix record to the debug file
 * 
 * newlibpatch     index ptr offset
 *
 *****************************************************************************/
PUBLIC void
debug_genaddrfix (const treenode * const address, const BIT32 offset)
{
	int line;
	if (			/*minimal_debugoutput || *//* bug TS/1434 15/09/92 */
		   !symbolic_debugoutput)	/* bug 1160 - 18/2/91 */
		return;
	line = get_from_index_table (address);
	if (debug_diagnostics)
		fprintf (outfile, "%4d Addressfix : %d %ld\n", debuginfo_index, line, (long) offset);
	else if (debug_to_file)
		/*{{{  write a record to the object file */
	{
		char buffer[MAX_DEBUG_STRING_SIZE];
		char *b = buffer;

		*b++ = RTL_ADDRESS_FIX;
		b = addbuf_3L_num (b, debuginfo_index);
		b = addbuf_3L_num (b, line);
		b = addbuf_3L_num (b, offset);
		add_to_debug_buffer (buffer, b - buffer, BUF_THROW);
	}
	/*}}} */
	debuginfo_index++;
}

/*}}}*/

/*{{{  source code output   UNUSED*/
#if 0				/* This is only used by source_output */

/*{{{  linemark list public routine*/
/*{{{  PUBLIC int get_from_linemark_list(index)*/
/*{{{  comment*/
/******************************************************************************
*
*    get_from_linemark_list(index)
*
*    index = index of codemark record.
*    Finds the linemark record corresponding to the given index and returns
*    the line number.
*    Andy Whitlow 15.11.88
*
******************************************************************************/
/*}}}*/
PUBLIC int
get_from_linemark_list (int index)
{
	linemark_cell_t *lptr;

	for (lptr = linemark_list; lptr != NULL; lptr = lptr->next)
		if (lptr->index == index)
			return (lptr->linenumber);
	geninternal (GEN_LINEMARK_LIST_ERR);
	return (0);		/* not reached */
}

/*}}}*/
/*}}}*/
/*{{{  file list public routine*/
/*{{{  PUBLIC int get_file_number(index)*/
/*{{{  comment*/
/******************************************************************************
*
*    get_file_number(this_index)
*
*    this_index = index of codemark record.
*    Finds the filenumber of the file which contains the linemark with index
*    of 'this_index'.
*    Andy Whitlow 15.12.88
*
******************************************************************************/
/*}}}*/
PUBLIC int
get_file_number (int this_index)
{
	file_cell_t *fptr = file_list;
	int curr_fileid, prev_fileid;

	/* printf("\n@@@ searching for filenumber for index %d", this_index); */
	curr_fileid = fptr->fileid;
	/* this_index = fptr->index; */
	fptr = fptr->next;
	while (this_index > fptr->index) {
		/*{{{  check next file list cell */
		if (fptr->tag == FILE_START) {
			/*{{{  found start file marker */
			prev_fileid = curr_fileid;
			curr_fileid = fptr->fileid;
			/*}}} */
		} else
			/*{{{  found end file marker */
			curr_fileid = prev_fileid;
		/*}}} */
		fptr = fptr->next;
		/*}}} */
	}
	return (curr_fileid);
}

/*}}}*/
/*}}}*/
#endif
/*}}}*/

/*{{{  PUBLIC BOOL need_locate(tag)*/
/*{{{  comment*/
/******************************************************************************
*
*  need_locate(tag)
*  tag = tag field of tree node being examined.
*  returns true if run time locate information is required for that node.
*  Andy Whitlow 20.10.88
*
******************************************************************************/
/*}}}*/
PUBLIC BOOL
need_locate (int tag)
{
	switch (tag)
		/*{{{  cases */
	{
		/*{{{  don't need locate info */
	default:
		return (FALSE);
		/*}}} */
		/*{{{  do need locate info */
		/* case S_STOP : STOP does need locate info but this is provided for explicitly
		   in tstop(), (gen1), if we were to include STOP here we would
		   obtain two codemarks for an explicit STOP statement */
	case S_SKIP:
/*case S_SEQ : *//* no code generated */
	case S_REPLSEQ:
	case S_IF:
	case S_REPLIF:
	case S_ALT:
	case S_REPLALT:
	case S_PRIALT:
	case S_PRIREPLALT:
	case S_WHILE:
	case S_PINSTANCE:
/*case S_FINSTANCE : *//* removed for bug TS/1443 05/11/92 */
	case S_ASS:
	case S_OUTPUT:
	case S_INPUT:
	case S_TAGGED_INPUT:
	case S_DELAYED_INPUT:
	case S_CASE:
	case S_CASE_INPUT:
	case S_VALABBR:
	case S_VALRETYPE:
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_DECL:
	case S_SFUNCDEF:
	case S_ABBR:
	case S_RETYPE:
	case S_SEGMENT:
	case S_SEGMENTITEM:
	case S_VALOF:
	case S_CHOICE:
	case S_ALTERNATIVE:
	case S_SELECTION:
	case S_VARIANT:
		/*case S_GUY : */
		/*case S_ASM : */
	case S_GUYCODE:
	case S_GUYSTEP:
		/*case S_LABELDEF :  This never actually exists! */
	case S_PAR:
	case S_REPLPAR:
	case S_PRIPAR:
		/* case S_CONSTEXP : */
		return (TRUE);
		/*}}} */
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void flush_debug_buffers()*/
/*{{{  comment*/
/******************************************************************************
*
*  flushes debug buffers to output after the code generation phase.
*  called by harness.tsr after the call to tmain.
*
******************************************************************************/
/*}}}*/
PUBLIC void
flush_debug_buffers (void)
{
	if (kp != 0) {
		write_debug_string (keep, kp);
		kp = 0;
	}
#ifndef MERGE_OUTPUT_BUFFERS
	if (tp != 0) {
		write_debug_string (throw, tp);
		tp = 0;
	}
#endif
}

/*}}}*/
/*{{{  PUBLIC void debugmain (tptr)*/
/*****************************************************************************
 *
 * debugmain writes out all the symbolic debugging information for tree tptr.
 *
 *****************************************************************************/
PUBLIC void
debugmain (treenode * tptr)
{
	int i;
	debuginfo_index = 0;
	last_codemark_line = -1;

	/*{{{  (re)initialise index hash table(s) */
	if (configuring) {
		/* If configuring, this is done more than once, so we have to keep
		   re-initialising the hash table
		 */
		for (i = 0; i < INDEX_HASH_TABLE_SIZE; i++) {
			index_hash_t *hptr = index_hash_table[i];
			while (hptr != NULL) {
				index_hash_t *temp = hptr->next;
				freevec (hptr, sizeof (*hptr));
				hptr = temp;
			}
			index_hash_table[i] = NULL;
		}
#if 0				/* These are only used by the source_output */
		while (linemark_list != NULL) {
			linemark_cell_t *lptr = linemark_list;
			linemark_list = linemark_list->next;
			freevec (lptr, sizeof (*lptr));
		}
		while (file_list != NULL) {
			file_cell_t *fptr = file_list;
			file_list = file_list->next;
			freevec (fptr, sizeof (*fptr));
		}
#endif
	}
	/*}}} */

#if 0
	debug_buffer_size = /*tcoff_obj_format ? */ TCOFF_MAX_DEBUG_STRING_SIZE
		/*: LFF_MAX_DEBUG_STRING_SIZE */ ;
#endif

	if (keep == NULL)
		keep = memalloc (debug_buffer_size);
	kp = 0;			/* initialise keep pointer */

#ifndef MERGE_OUTPUT_BUFFERS
	if (throw == NULL)
		throw = memalloc (debug_buffer_size);
	tp = 0;			/* initialise throw pointer */
#endif

	debug_to_file = tcoff_without_code || (!assembly_output && !disassemble);	/* MDP */
	enclosing_this_offset = 0;
	workspace_adjust = 0;
	filenum = 0;
	nextfile = 1;
#ifdef OCCAM2_5
	debug_typedef_number = RTL_MINUSERTYPE;
#endif
	alt_starting = FALSE;

	DEBUG_MSG (("debugmain: start of debug info\n"));
	gendebugversion ();
#ifndef MERGE_OUTPUT_BUFFERS
	flush_debug_buffers ();	/* ensure that the version marker is first */
#endif
	gendebugstartfold (0);
	gendebug (tptr);
	i = numberoffiles ();
	while (nextfile < i)	/* Finish any blank files */
		if (fileissource (nextfile)) {
			DEBUG_MSG (("debugmain: tidying up blank file %d\n", nextfile));
			gendebugfoldmarks (filenum, nextfile);
		} else {
			DEBUG_MSG (("debugmain: ignoring non-source file %d\n", nextfile));
			nextfile++;
		}
	DEBUG_MSG (("debugmain: tidying up the endfolds\n"));
	gendebugfoldmarks (filenum, 0);	/* Back to the start */
	gendebugendfold ();
}

/*}}}*/
/*{{{  PUBLIC void debugfree*/
PUBLIC void
debugfree (void)
{
	if (keep != NULL) {
		memfree (keep);
		keep = NULL;
	}
#ifndef MERGE_OUTPUT_BUFFERS
	if (throw != NULL) {
		memfree (throw);
		throw = NULL;
	}
#endif
}

/*}}}*/

/*{{{  mapfile writing*/
/*{{{  data declarations*/
PRIVATE const char *const resptr_string = "<result_pointer>";
PRIVATE const char *const hidden_string = "<hidden_dimension>";
PRIVATE const char *const sl_string = "<static_link>";
PRIVATE const char *const vsp_string = "<vectorspace_pointer>";
#ifdef MOBILES
PRIVATE const char *const msp_string = "<mobilespace_pointer>";
#endif

/*}}}*/
/*{{{  forward declarations*/
PRIVATEPARAM int look_for_pars (treenode * const tptr, void *const voidptr);

/*}}}*/
/*{{{  PRIVATE void write_var_mapfile*/
PRIVATE void
write_var_mapfile (treenode * const nptr)
{
	int dims;
	INT32 vartype;
	INT32 access;
	BIT32 output_offset;

	calc_var_details (nptr, &dims, &vartype, &access, &output_offset);

	switch (access) {
	case RTL_BYVAL:
	case RTL_BYREF:
		map_add_entry (mapfile_handle, NULL, WNameOf (NNameOf (nptr)), NULL, output_offset /* * bytesperword */ , NULL);
		break;
	default:
		break;
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM int look_for_vars*/
PRIVATEPARAM int
look_for_vars (treenode * const tptr, void *const voidptr)
{
	switch (TagOf (tptr)) {
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
		prewalktree (DBodyOf (tptr), look_for_vars, voidptr);	/* bug TS/1803 11/08/92 */
		return STOP_WALK;
	case S_PAR:
	case S_PRIPAR:
		return STOP_WALK;
	case S_REPLPAR:
	case S_PRIREPLPAR:
		write_var_mapfile (ReplCNameOf (tptr));
		return STOP_WALK;
	case S_REPLSEQ:
	case S_REPLIF:
	case S_REPLALT:
		write_var_mapfile (ReplCNameOf (tptr));
		break;
	case S_DECL:
	case S_ABBR:
	case S_VALABBR:
	case S_RETYPE:
	case S_VALRETYPE:
		{
			treenode *n = DNameOf (tptr);
			if (TagOf (n) == S_LIST)
				for (; !EndOfList (n); n = NextItem (n))
					write_var_mapfile (ThisItem (n));
			else
				write_var_mapfile (n);
		}
		break;
	default:
		break;
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATE void write_param_mapfile*/
PRIVATE void write_param_mapfile (treenode * const nptr)
{
	treenode *params;
	BIT32 posn = NPMaxwspOf (nptr) + INS_EXTRA;
	for (params = NParamListOf (nptr); !EndOfList (params); params = NextItem (params)) {
		treenode *const param = ThisItem (params);
		const char *s = NULL;
		switch (TagOf (param)) {
		default:
			badtag (LocnOf (nptr), TagOf (param), "write_param_mapfile");
			break;
		case N_PARAM:
		case N_VALPARAM:
		case N_RESULTPARAM:
			write_var_mapfile (param);
			if (basetype (gettype (param)) != S_TIMER)
				posn++;
			break;
		case S_FNFORMALRESULT:
			s = resptr_string;
			break;
		case S_HIDDEN_PARAM:
			s = hidden_string;
			break;
		case S_PARAM_STATICLINK:
			s = sl_string;
			break;
		case S_PARAM_VSP:
			s = vsp_string;
			break;
#ifdef MOBILES
		case S_PARAM_MSP:
			s = msp_string;
			break;
#endif
		}
		if (s != NULL) {
			map_add_entry (mapfile_handle, NULL, s, NULL, posn /* * bytesperword */ , NULL);
			posn++;
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void write_par_mapfile*/
PRIVATE void
write_par_mapfile (treenode * const tptr, treenode * const proc_nptr)
{
	const char *const filename = lookupfilename (FileNumOf (LocnOf (tptr)));
	const int linenumber = FileLineOf (LocnOf (tptr));
	INT32 parsize = ((TagOf (tptr) == S_PRIPAR) && (code_style_flags & CODE_STYLE_ALT_PRI_PAR)) ? DS_IO : 0;

	treenode *list;
	int branch;

	for (list = CBodyOf (tptr), branch = 1; !EndOfList (list); list = NextItem (list), branch++) {
		treenode *const sp = ThisItem (list);
		const INT32 thisbranchwsp = SpMaxwspOf (sp);
		const INT32 thisbranchsize = SpDatasizeOf (sp);
		const INT32 savedoffset = enclosing_this_offset;
		const INT32 savedworkspace = workspace_adjust;
		enclosing_this_offset += parsize + thisbranchwsp;
		workspace_adjust += parsize + thisbranchwsp;

		(void) map_open_section (mapfile_handle, map_sectiontype_par,
					 WNameOf (NNameOf (proc_nptr)), branch, filename, linenumber, -enclosing_this_offset, 0, FALSE);
		prewalktree (SpBodyOf (sp), look_for_vars, NULL);
		map_close_section (mapfile_handle, NULL);

		enclosing_this_offset = 0;
		prewalktree (SpBodyOf (sp), look_for_pars, proc_nptr);

		enclosing_this_offset = savedoffset;
		workspace_adjust = savedworkspace;
		parsize += thisbranchsize;

	}
}

/*}}}*/
/*{{{  PRIVATE void write_repl_mapfile*/
PRIVATE void write_repl_mapfile (treenode * const tptr, treenode * const proc_nptr)
{
	const char *const filename = lookupfilename (FileNumOf (LocnOf (tptr)));
	const int linenumber = FileLineOf (LocnOf (tptr));

	treenode *const sp = ReplCBodyOf (tptr);
	treenode *const replicator = ReplCNameOf (tptr);
	const INT32 thisbranchwsp = SpMaxwspOf (sp);
	const INT32 savedoffset = enclosing_this_offset;
	const INT32 savedworkspace = workspace_adjust;
	const int replparslots = (SpVSUsageOf (sp) == 0) ? MIN_REPLPAR_SPECIALS : MIN_REPLPAR_SPECIALS + 1;

	enclosing_this_offset = thisbranchwsp - replparslots + REPLPAR_STATICLINK;
	workspace_adjust = 0;

	(void) map_open_section (mapfile_handle, map_sectiontype_repl,
				 WNameOf (NNameOf (proc_nptr)),
				 (int) LoValOf (ReplCLengthExpOf (tptr)), filename, linenumber, -(thisbranchwsp + DS_MIN), SpDatasizeOf (sp), FALSE);

	map_add_entry (mapfile_handle, NULL, WNameOf (NNameOf (replicator)),
		       NULL, (thisbranchwsp - replparslots + REPLPAR_REPLICATOR) /* * bytesperword */ ,
		       NULL);
	map_add_entry (mapfile_handle, NULL, sl_string, NULL, (thisbranchwsp - replparslots + REPLPAR_STATICLINK) /* * bytesperword */ ,
		       NULL);
	if (SpVSUsageOf (sp) != 0)
		map_add_entry (mapfile_handle, NULL, vsp_string, NULL, (thisbranchwsp - replparslots + REPLPAR_VSP) /* * bytesperword */ ,
			       NULL);

	prewalktree (ReplCBodyOf (tptr), look_for_vars, NULL);
	map_close_section (mapfile_handle, NULL);

	prewalktree (ReplCBodyOf (tptr), look_for_pars, proc_nptr);

	enclosing_this_offset = savedoffset;
	workspace_adjust = savedworkspace;

}

/*}}}*/

/*{{{  PRIVATEPARAM int look_for_pars*/
PRIVATEPARAM int
look_for_pars (treenode * const tptr, void *const voidptr)
{
	switch (TagOf (tptr)) {
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
		return STOP_WALK;
	case S_PAR:
	case S_PRIPAR:
		write_par_mapfile (tptr, voidptr);
		return STOP_WALK;
	case S_REPLPAR:
	case S_PRIREPLPAR:
		write_repl_mapfile (tptr, voidptr);
		return STOP_WALK;
	default:
		break;
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATE void write_proc_mapfile*/
PRIVATE void
write_proc_mapfile (treenode * const tptr)
{
	treenode *const nptr = DNameOf (tptr);
	INT32 wsusage, vsusage;

	getprocwsandvs (nptr, &wsusage, &vsusage);

	(void) map_open_section (mapfile_handle, map_sectiontype_proc, WNameOf (NNameOf (nptr)), 0, NULL, 0, 0, 0, FALSE);
	prewalktree (DValOf (tptr), look_for_vars, NULL);
	map_close_section (mapfile_handle, NULL);

	(void) map_open_section (mapfile_handle, map_sectiontype_formal, NULL, 0, NULL, 0, 0, 0, FALSE);
	write_param_mapfile (nptr);
	map_set_size (mapfile_handle, NULL, wsusage, vsusage);
	map_close_section (mapfile_handle, NULL);

	prewalktree (DValOf (tptr), look_for_pars, nptr);
}

/*}}}*/
/*{{{  PRIVATEPARAM int look_for_procs*/
PRIVATEPARAM int
look_for_procs (treenode * const tptr, void *const voidptr)
{
	switch (TagOf (tptr)) {
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
		{
			treenode *const nptr = DNameOf (tptr);
			if (!isinline (nptr) && !separatelycompiled (nptr)) {
				prewalktree (DValOf (tptr), look_for_procs, voidptr);
				write_proc_mapfile (tptr);
				prewalktree (DBodyOf (tptr), look_for_procs, voidptr);
				return STOP_WALK;
			}
		}
		break;
	default:
		break;
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PUBLIC void debug_write_mapfile*/
PUBLIC void
debug_write_mapfile (treenode * const tptr)
{
	workspace_adjust = 0;
	prewalktree (tptr, look_for_procs, NULL);
}

/*}}}*/
/*}}}*/
