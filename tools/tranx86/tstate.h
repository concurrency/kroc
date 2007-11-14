/*
 *	tstate.h -- transputer state definition
 *	Copyright (C) 2000-2004 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __TSTATE_H
#define __TSTATE_H

struct TAG_procinf;	/* forward */
struct TAG_csinfo;	/* forward */
struct TAG_ins_chain;
struct TAG_tdfixup;	/* forward */

typedef struct {
	int cond;
	int numfuncresults;
	int incasetable;
	int casetable_label;
	int libout_pending;
	char *libout_name;
	int line_pending;
	int file_pending;
	int proc_pending;
	int fp_line_pending;
	int fp_file_pending;
	int fp_proc_pending;
	char **file_list;
	int file_cur, file_max;
	char **proc_list;
	int proc_cur, proc_max;
	int magic_pending;	/* TS_MAGIC_... */
#if 0
	int indirectchan;	/* used for mobile processes, affects input/output instructions */
#endif
	int ws_size, ws_adjust, vs_size, ms_size;
	int end_of_module;
	int stack_drift;			/* when we use the C stack temporarily */
	tstack *stack;
	struct TAG_csinfo *csinfo;		/* for control split/join handling */
	struct TAG_ins_chain *zeropoint;	/* last instruction that started with tsdepth of zero */
	int last_lab;
	int had_jentry;
	char *jentry_name;	/* used for checking usage with TCOFF descriptor */
	int flushscreenpoint;	/* label at which the screen-flush code goes */
	/* various labels for debugging */
	int overflow_label;
	int filename_label;
	int procedure_label;
	int floaterr_label;
	int procfile_setup_label;
	int insert_setup_label;
	int range_entry_label;
	int zerodiv_label;
	int floatrange_label[4];
	int floatconv_label;
	int floatspace_label;
	int supress_debug_insert;
	char **glob_names;
	int glob_names_cur, glob_names_max;
	void *ustate;		/* used for run-time code-gen */
	struct TAG_procinf *cpinfo;
	struct TAG_tdfixup *fixups;
} tstate;

#define TS_MAGIC_NONE		0x00
#define TS_MAGIC_IOSPACE	0x01	/* used to indicate an array access is PLACED in IOSPACE (PORT) */
#define TS_MAGIC_UNUSED_LOOPVAR	0x02	/* used to indicate a loopend variable is unused */
#define TS_MAGIC_PREENABLE	0x04	/* used to indicate a pre-enabling sequence (ENB.3) */
#define TS_MAGIC_UNCHECKED	0x08	/* used to indicate that an operation does not need to check for overflow */
#define TS_MAGIC_EXTENDS_RESIGN	0x10	/* used to indicate barrier resignation from a PAR EXTENDS, so cannot complete the barrier */
#define TS_MAGIC_JOINLAB	0x20	/* used to indicate setlab belonging to a PAR join-label */
#define TS_MAGIC_DOSUBCODEMAP	0x40	/* used before a CALL/GCALL to indicate what's being called may be a SUSPEND path */
#define TS_MAGIC_CODEMAP	0x80	/* used before an instruction that sets a label (and others, e.g. STOPP) to tag for code-mapping */
#define TS_MAGIC_TYPEDESC	0x100	/* used before DATABYTES to indicate that it's a type description */

extern tstate *new_tstate (void);
extern void tstate_initialise (tstate *state, etc_chain *etc_code);
extern void tstate_ctofp (tstate *ts);
extern int tstate_is_glob_name (tstate *state, char *name, int len);
extern void tstate_add_glob_name (tstate *state, char *name, int len);
extern void tstate_addfixup (tstate *state, int thislab, int offs, int otherlab);
extern void tstate_clear_fixups (tstate *state);
extern struct TAG_tdfixup *tstate_getfixups (tstate *state);

extern void ustate_clear (tstate *state);
extern int ustate_newreg (tstate *state, int ureg);
extern int ustate_regof (tstate *state, int ureg);
extern int ustate_newlab (tstate *state, int ulab);
extern int ustate_labof (tstate *state, int ulab);

/* this structure is used to fixup type-descriptors -- records offsets and labels which need adjusting */

typedef struct TAG_tdfixup {
	struct TAG_tdfixup *next;
	int thislab, offset, otherlab;
} tdfixup_t;

/* stuff below keeps track of procedures (for mobile processes) */

typedef struct TAG_procinf {
	char *name;			/* unconverted name */
	int namelen;			/* unconverted name length */
	char *iname;			/* internal name (for subordinates) */
	int inamelen;			/* internal name length */
	int is_proc;			/* non-zero if a PROC */
	int is_local;			/* non-zero if local */
	int is_internal;		/* non-zero if an internal label */
	int eplab;			/* entry-point label */
	int maplab;			/* PROC code-map label */
	int namelab;			/* label set for PROC name string */
	int inamelab;			/* label set for internal-to-PROC name string */
	int written_out;
	
	struct TAG_procinf **refs;	/* other names referenced */
	int refs_cur, refs_max;
} procinf;

extern procinf *procinf_declare (char *name, int len);
extern procinf *procinf_internallab (procinf *current, int lab);
extern procinf *procinf_lookup (char *name, int len);
extern procinf *procinf_findbylab (int lab);
extern procinf *procinf_findbyname (char *name, int len);
extern void procinf_addref (procinf *current, procinf *ref);
extern void procinf_dumpentry (FILE *stream, procinf *pinfo);
extern void procinf_dumptab (FILE *stream);
extern void procinf_iterate (void (*func)(procinf *, void *), void *param);


typedef struct TAG_cssplitinfo {
	struct TAG_cssplitinfo *next;	/* next split for a particular label */
	struct TAG_ins_chain *startins;
	int depth;
	int vregs[3];
} cssplitinfo;

typedef struct TAG_csinfo {
	struct TAG_csinfo *next;	/* next set of splits/join */
	int label;			/* associated with this set of splits/join */

	cssplitinfo *splits;
	int nsplits;
} csinfo;

extern void control_set_split (tstate *ts, int cslab, struct TAG_ins_chain *lastins);
extern void control_set_join (tstate *ts, int cslab, struct TAG_ins_chain *lastins);
extern int control_in_split (tstate *ts);


#endif	/* !__TSTATE_H */

