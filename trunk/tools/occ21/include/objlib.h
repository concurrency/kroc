/* $Id: objlib.h,v 1.1 1996/04/15 10:52:15 djb1 Exp $ */

/*
 *	library object file handling
 *	Copyright (C) 1991 Inmos Limited
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


/*{{{  type declarations */
/* Each library or separately compiled unit which is #USEd is turned into
   a sequence or S_PROCDEF or S_LFUNCDEF nodes, each of which points,
   as per usual, to a namenode (N_LIBPROCDEF or N_LIBFUNCDEF).
   Where the same routine appears in one library but compiled for multiple
   different processor types and/or modes, only one PROCDEF record is
   created.
   If the same name appears in _different_ libraries, they are treated
   as being entirely separate, and are scoped according to the normal
   occam rules.
*/
/* Each library procedure or function declaration has an NLExternal field,
   which holds a pointer to a libentry_t record.
   This, in turn, holds a list of procdef_t records.
   There will be one procdef_t record for each routine of the same name but
   different processor type and/or mode in that same library.

   (Note - as an optimisation, when compiling, rather than configuring,
   only one procdef_t record is kept; this being the `most suitable' one.
   The ordering by the librarian means that the first callable one
   will be the `most suitable'. Search for "SINGLE_PROCDEF".
   This saves a little bit of space, but otherwise is harmless.)

   Each procdef_t record points to a module_t record. If many entrypoints
   are declared in the same module of a library, they will all point
   to the same module_t record.
   The module_t record holds the identity of that module's `origin'
   symbol, which is used in a linker directive to ensure that the same
   object file is used to link as was to compile.
   The module_t records are chained together, but are otherwise not
   directly accessable.

   CON - 3/4/91
*/

/*
 * modified frmb December 2004: now we have an N_LIBMPROCDECL drifting around
 * too, for marking separately compiled mobile process implementations.
 */

/*{{{  typedef module_t */
typedef struct module_s  /* used for matching symbols to their origin modules */
  {
    struct module_s       *m_next;     /* next on chain for this file */
    struct wordnodestruct *m_name;     /* ptr to origin symbol name */
    BIT32         m_instr, m_attr;     /* processor type and attributes */
    long int               m_seek_ptr; /* position in file */
    int                    m_id_val;   /* id of origin, or special value */
    int                    m_filenum;  /* Number of file in filetable */
    INT32                  m_size;     /* module code size */
  #if 1 /*def CONFIG*/
    void                  *m_config;   /* Used by the configurer */
    void                  *m_filestr;  /* Used by the configurer to cache a filename string */
  #endif
  } module_t;
#define ORIGIN_NOT_FOUND (-1)          /* m_id_val special value */
#define ORIGIN_FOUND     (-2)          /* m_id_val special value */
/*}}}*/
/*{{{  typedef procdef_t */
typedef struct procdef_s /* One definition of a PROC from a library */
  {
    struct procdef_s *p_next;
    module_t         *p_module;        /* module which this declaration is in */
    INT32             p_ws;            /* ws usage */
    INT32             p_vs;            /* vs usage */
  #ifdef MOBILES
    INT32             p_ms;            /* ms usage */
  #endif
  #if 1 /*def CONFIG*/
    INT32             p_offset;        /* entry offset in that module */
  #endif
  } procdef_t;
#define ORIGIN_WS (-1)  /* ws value used in a descriptor to indicate
                           that this symbol is an ORIGIN */

/*}}}*/
/*{{{  typedef libentry_t */
/* Each N_LIBPROCDEF etc record's NLExternal field has a pointer
   to one of these:
*/
typedef struct libentry_s /* A name defined in a library */
  {
    struct libentry_s     *l_next;     /* Next entry in chain */
    wordnode              *l_name;     /* Name of entrypoint */
    procdef_t             *l_procdefs; /* list of individual definitions */
    unsigned int           l_bits;     /* various bit patterns */
    BIT32                  l_hash;     /* Hash function of descriptor */
  } libentry_t;
#define LIBENTRY_BIT_DESC_SENT      0x01
#define LIBENTRY_BIT_COMPATIBLE     0x02
#define LIBENTRY_BIT_COMPATIBLE_ERR 0x04
#define LIBENTRY_BIT_NESTED_TIMER   0x08
#define LIBENTRY_BIT_NESTED_PLACE   0x10
#define LIBENTRY_BIT_NESTED_PORT    0x20

/*}}}*/

/*}}}*/

/*{{{  function prototypes */
BOOL compatible_call (BIT32 callee_instr, BIT32 callee_attr);

libentry_t *search_libentries(libentry_t *ptr, wordnode *name);
module_t *search_modulechain(module_t *ptr, long int seek_val);

procdef_t *get_procdef(treenode *nptr, SOURCEPOSN locn, BOOL check, BOOL allow_fpu_calls);
void checklibproctype (treenode *nptr, SOURCEPOSN locn, BOOL allow_fpu_calls);
BOOL compiledforcorrectproc (treenode *nptr, BOOL allow_fpu_calls);
void getlibwsandvs (treenode *nptr, SOURCEPOSN locn, BOOL allow_fpu_calls, INT32 *ws, INT32 *vs);
#ifdef MOBILES
void getlibwsandvsandms (treenode *nptr, SOURCEPOSN locn, BOOL allow_fpu_calls, INT32 *ws, INT32 *vs, INT32 *ms);
#endif

const char *create_descriptor_string(fe_handle_t *fe_handle,
   treenode *nptr, const wordnode *nameptr,
   BOOL short_names, BOOL merge_types, BOOL add_spaces);

BIT32 origin_num_from_origin_str(const wordnode *string);

/*}}}*/

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (compatible_call)
#pragma IMS_nosideeffects (search_libentries)
#pragma IMS_nosideeffects (search_modulechain)
#endif
/*}}}*/


