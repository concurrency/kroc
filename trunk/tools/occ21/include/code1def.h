/* $Id: code1def.h,v 1.9 1998/01/12 14:10:04 mdp2 Exp $ */

/*
 *	code1 declarations
 *	Copyright (C) 1987 Inmos Limited
 *	Modifications (C) ~1996-1998 Michael Poole
 *	Modifications (C) 2002 Fred Barnes <frmb2@ukc.ac.uk>
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

/*{{{  defines*/
#define COMMENT_INTRO "--"

#define SEMOP_CLAIM	1
#define SEMOP_RELEASE	2
#define SEMOP_ISWAITING	3
#define SEMOP_INIT	4

#define INDIR_AREG	0
#define INDIR_BREG	1
#define INDIR_CREG	2
/*}}}*/

/*{{{  variables*/
extern int req_code_size;

/* These are used by the configurer to get the coder to do some
   extra work */
extern void (*code_pseudo_link_fn)(treenode *);
extern void (*code_save_entrydata_fn)(const treenode *const nptr,
                              const INT32 offset, const INT32 code_size);
extern void (*code_link_map_fn)(void);
/*}}}*/

/*{{{  routines*/
void compress_code (void);
void genprimary (int instruction, INT32 operand);
void genbranch (int instruction, int label);
void genjump (int label);
void genlabeldiff (int instruction, int label1, int label2);
void gensecondary (int instruction);
void checkerror_controlled (int mode);
void checkerror (void);
void markdeadcode (void);
void mark_flag_clean (int fpu_not_int);
void genstartblock (void);
void coder_return_from_call(BOOL clean_fpu);
void setlab (int label);
void setsectionlab (int label);
void commentexp (treenode *tptr);
void gencomment0 (const char *c);
void gencomment1 (const char *c, BIT32 p1);
void gencomments (const char *c, const char *p1);
#ifdef HAVE_STDARG_H
void gencommentv (const char *fmt, ...);
#endif
void genstartconstblock (const char *c);
void genendconstblock (void);
void notefpresult (const char *type);
void coder_add_entry (treenode *nptr);
void coder_finish_entry (treenode *nptr);
void coder_add_mapfile_entry (treenode *nptr);
void add_const_block (const int l, const BYTE *const p, treenode *type_tptr, const char *type_string, const int swap);
int genconstant (treenode *cptr);
void genchecknotnull (void);
void gensemop (int op);
void genreschedule (void);
void genindirect (int reg);
void genstartjumptable (void);
void genjumptableentry (int instruction, int label);
void genendjumptable (void);
void genfpuentry (int instruction);
void genextfpop (int op);
/*void declarelabel (int label, BIT32 offset);*/ /* unused */
void add_to_libentries (treenode *nptr);
void genlibstubs (void);
int genproftable(INT32 proftsize);
/*void coder_genlocate (treenode *address, BOOL write_linenumber); */
void new_occam_line (treenode *address, BOOL write_linenumber, BOOL need_locate, BOOL clear_tstack);
void coder_genaddrfix (treenode *address, int label);
void throw_the_result_away (void);

void coder_jumptoentrypoint(const treenode *const nptr);
void coder_mark_alignment(void);
int  endofsection_padlen (int alignment);
void alignendofsection (int alignment);
void alignwholemodule (void);

void initcode (void);
void freecode (void);

FILE *open_object_file (const char *filename);
void write_object_file (void);
void close_object_file (FILE *fptr, const char *filename);
/*{{{  extra routines only strictly needed if generating target code directly*/
/* included here so we only need one version of this header file */
void genwidenshort (void);
void noteprocentry (void);
void genprocentry (const int tag, const int ws);
void genprocreturn (const int tag, SOURCEPOSN elocn, const int nregs, const int ws);
void throw_nested_result_away (void);
void gen_func_results (const int regresults);
void gencondbranch (int jumpcondop, int label);
void genloadnamedlabptr (const char *name);
void genloadlabptr (const int thatlab, const int thislab, const char *text);
void genboolinvert (void);
void genloopheadtop (const int wsoffset);
void genloopheadbottom (void);
void genloopend (const int wsoffset, const int endlabel, const int looplabel);
void genloopendb (const int wsoffset, const int endlabel, const int looplabel);
void genloopend3 (const int wsoffset, const int endlabel, const int looplabel);
void genaltend (const int altcount, const int altendlab);
void genloadconstant(INT32 c);
void genshiftimmediate (const int op, const int shiftcount);
void gennotprocess (void);
void geni64tor (const int sourcemode, treenode *const source, const int desttype,
                const int roundmode, const int simple);
void gen_init_chanarray (const int pwidth, const int cwidth, const int elems);
BOOL genreplicatorcheck (const BOOL must_check, const int poslab, const int zerolab);
void gencompare (const int relop, const BOOL flags);
void gentstop ( treenode *const address );
void endsectionalign (int wlen);
void gengcall (const BOOL savedreturn);
void gencontrolsplit (const int label);
void gencontroljoin (const int label);
void genfppop (void);
void genloadwsmap (const int mpp_offset, const int maplab);
void genunloadwsmap (const int mpp_offset, const int maplab);
void genrmwsmap (void);
void genmppclone (void);
void genmppserialise (void);
void genmppdeserialise (void);
void genloadcodemap (treenode *const nptr);
/*}}}*/
#if 0
void genfbarinit (void);
void genfbarsync (void);
void genfbarresign (void);
void genfbarenroll (void);
#endif
void genmobiletypedescription (treenode *const mtype);
void gendtrace (void);
void genkillcall (void);
void genwaitint (void);
/*}}}*/

/*{{{  aditional MOBILE stuff */
#ifdef MOBILES
void genmobileinit (const int msp_offset, treenode *nptr);
void genmobileinitdynpar (const int msp_offset, treenode *sptr);
#endif
/*}}}  */

/*{{{  debugging */
#ifdef BACKENDTRACE
	extern char betrace_buf[512];	/* hiding in code1k.c */

	#define BETRACE(args...) { \
		sprintf (betrace_buf, ##args); \
		gencomment0 (betrace_buf); \
		}
#else
	#define BETRACE(args...)
#endif
/*}}}  */

/*{{{  side-effects pragmas*/
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (get_padlen)
#endif
/*}}}*/



