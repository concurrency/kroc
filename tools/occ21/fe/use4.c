/*
 *	use4.c -- undefined variable usage checking
 *	Copyright (C) 2001-2002 Fred Barnes <frmb@kent.ac.uk>
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

/*{{{  includes*/
#ifdef HAVE_CONFIG_H
	#include <config.h>
#endif
#include <stdio.h>
#include "feinc.h"
#include "useerror.h"
#include "usehdr.h"
#include "usedef.h"
#include "use1def.h"
#include "use2def.h"
#include "syndef.h"		/* syn_lexlevel */
#include "chkdef.h"		/* current_fe_handle etc */
#include "predefhd.h"		/* pragmas */
#include "lexdef.h"		/* for BOOL strict_checking */
/*}}}*/
/*{{{  constants*/
#define UDV_UNDEFINED 0
#define UDV_DEFINED 1		/* for variables */
#define UDV_UNKNOWN 2		/* for variables */
#define UDV_INPUTTED 3		/* for channels */
#define UDV_OUTPUTTED 4		/* for channels */
#define UDV_XINPUTTED 5		/* for channels */
#define UDV_PARTIAL 6		/* mixed defined/undefined (for records) */
#define UDV_INVALID 7
static char *udvstatestrings[] = {"UDV_UNDEFINED", "UDV_DEFINED", "UDV_UNKNOWN", "UDV_INPUTTED", "UDV_OUTPUTTED", "UDV_XINPUTTED", "UDV_PARTIAL", "UDV_INVALID"};
static char *udvnstatestrings[] = {"UDV_UNDEFINED+", "UDV_DEFINED+", "UDV_UNKNOWN+", "UDV_INPUTTED+", "UDV_OUTPUTTED+", "UDV_XINPUTTED+", "UDV_PARTIAL+", "UDV_INVALID+"};
static char *udvsstatestrings[] = {"UNDEF", "DEF", "UNKNOWN", "INPUT", "OUTPUT", "XINPUT", "PARTIAL", "INVALID"};
/* static char *udvnsstatestrings[] = {"UNDEF+", "DEF+", "UNKNOWN+", "INPUT+", "OUTPUT+", "XINPUT+", "PARTIAL+", "INVALID+"}; */
static char *udvminstatestrings[] = {"UND", "DEF", "UNK", "INP", "OUT", "XIN", "PTL", "INV"};

#define MAX_NESTING 1024	/* used for handling nested structures (IF, WHILE, ALT, CASE, PAR, replicators) */
#define UDV_ITRACE_MAX 20	/* used for tracing PROC calls */
/*}}}*/
/*{{{  private types*/

typedef enum {
	UDVT_INVALID = 0,
	UDVT_VAR = 1,		/* regular variable */
	UDVT_CHAN = 2,		/* regular channel */
	UDVT_MOB_DA = 3,	/* dynamic mobile array */
	UDVT_MOB_CT = 4		/* dynamic mobile channel-type */
} udv_type_t;

/* attached to the namenode of something to indicate current state */
typedef struct TAG_udv_t {
	struct TAG_udv_t *next;		/* next in in-scope stack */
	treenode *nameof;		/* namenode for this variable */
	udv_type_t type;		/* UDVT_... constant for the type */
	char state[MAX_NESTING];	/* UDV_... */
	int did_warn;			/* non-zero if a warning has been generated about this var already */
	struct TAG_udv_t **nested;	/* nested state (of record fields, etc.) */
} udv_t;

typedef struct TAG_udv_warning_t {
	struct TAG_udv_warning_t *next;	/* next warning */
	SOURCEPOSN locn;		/* location of generated warning */
	int err;			/* USE_... code for error */
	char *str;			/* string to pass (variable name) */
	int itrace_len;
	treenode **itrace;		/* call trace if (itrace_len > 0) */
} udv_warning_t;

typedef struct TAG_udv_udef_t {
	int n_undefined;		/* number of undefined things */
	int n_unknown;			/* number of unknown things */
	int n_partial;			/* number of partially defined things */
	int supress_warn;		/* non-zero to supress warning/error generation */
} udv_udef_t;

/* CLAIM stack -- needed for handling subscripted stuff sensibly.. */
typedef struct TAG_udv_cstk {
	struct TAG_udv_cstk *next;	/* next CLAIM up */
	treenode *claimed;		/* what was CLAIMed -- might be a subscription */
} udv_cstk;
/*}}}*/
/*{{{  forwards*/
PRIVATE int udv_nesting_maxdepth (udv_t *udv);
PRIVATE int udv_nesting_maxwidth (udv_t *udv);
PRIVATEPARAM int do_undefinedcheck (treenode *n, void *const voidptr);
PRIVATE void undefinedcheck_set_decl (treenode *namelist, treenode **revdecllist);
PRIVATE void undefinedcheck_end_decl (treenode *rdecllist);
PRIVATE treenode *undefinedcheck_set_spec (treenode *n, treenode **revdecllist, udv_t **otherp);
PRIVATE void undefinedcheck_end_spec (treenode *n, udv_t *other);
PRIVATE udv_t *udv_makeudv (treenode *tptr, udv_t *next);
PRIVATE void udv_freeudv (udv_t *udv);

PUBLIC char *use_exprstring (treenode *exp);

/*}}}*/
/*{{{  private variables*/
#define SINGLE_WARNING 1

PRIVATE int udv_vstacklevel = 0;
PRIVATE udv_t *udv_vstack = NULL;
PRIVATE udv_t *udv_xcvar = NULL;	/* nasty, indicates variable used during extended CASE input */
PRIVATE udv_warning_t *udv_wstack = NULL;
PRIVATE use_mode_t udv_defaultmode;
#ifdef MOBILES
PRIVATE BOOL udv_ismobile = FALSE;	/* set TRUE when handling something MOBILEs */
#endif
PRIVATE SOURCEPOSN udv_locn;
PRIVATE int udv_skip_channels = 0;
PRIVATE treenode *udv_itrace[UDV_ITRACE_MAX];
PRIVATE int udv_itraceptr = 0;
PRIVATE udv_cstk *udv_cstack = NULL;

/* constants for ASM handling */
PRIVATE wordnode *op_ld = NULL;
PRIVATE wordnode *op_ldab = NULL;
PRIVATE wordnode *op_ldabc = NULL;
PRIVATE wordnode *op_ldl = NULL;
PRIVATE wordnode *op_st = NULL;
PRIVATE wordnode *op_stab = NULL;
PRIVATE wordnode *op_stabc = NULL;
PRIVATE wordnode *op_stl = NULL;
PRIVATE wordnode *op_savel = NULL;
/*}}}*/
/*{{{  PRIVATE void udv_dump_stack_nested_bit (FILE *stream, int botlvl, int toplvl, int indent, udv_t *udv)*/
/*
 *	prints out some nested state in the undefinedness stack.  used by udv_dump_stack_bit() below
 */
PRIVATE void udv_dump_stack_nested_bit (FILE *stream, int botlvl, int toplvl, int indent, udv_t *udv)
{
	int i;
	
	fprintf (stream, "%-1d+%-12s", indent, udv->nameof ? WNameOf (NNameOf (udv->nameof)) : "(null)");
	for (i = botlvl; i <= toplvl; i++) {
		fprintf (stream, "%-3s ", ((udv->state[i] >= 0) && (udv->state[i] <= UDV_INVALID)) ? udvminstatestrings[(int)(udv->state[i])] : "");
	}
	fprintf (stream, "\n");
	/* and the nested state of this */
	for (i=0; udv->nested && udv->nested[i]; i++) {
		udv_dump_stack_nested_bit (stream, botlvl, toplvl, indent + 1, udv->nested[i]);
	}
	return;
}
/*}}}*/
/*{{{  void udv_dump_stack_bit (FILE *stream, int botlvl, int toplvl, BOOL show_substate)*/
/*
 *	prints out a chunk of the undefinedness stack (debugging)
 *	this one prints out the list downwards
 */
void udv_dump_stack_bit (FILE *stream, int botlvl, int toplvl, BOOL show_substate)
{
	int i;
	udv_t *tmp;

	fprintf (stream, "udv_dump_stack_bit: botlvl = %d, toplvl = %d, show_substate = %d\n", botlvl, toplvl, show_substate);
	fprintf (stream, "              ");
	for (i=botlvl; i<=toplvl; i++) {
		fprintf (stream, " %-2d ", i);
	}
	fprintf (stream, "\n");

	for (tmp = udv_vstack; tmp; tmp = tmp->next) {
		fprintf (stream, "%-14s", WNameOf (NNameOf (tmp->nameof)));
		for (i = botlvl; i <= toplvl; i++) {
			fprintf (stream, "%-3s ", ((tmp->state[i] >= 0) && (tmp->state[i] <= UDV_INVALID)) ? udvminstatestrings[(int)(tmp->state[i])] : "");
		}
		fprintf (stream, "\n");
		if (tmp->nested && show_substate) {
			for (i=0; tmp->nested[i]; i++) {
				udv_dump_stack_nested_bit (stream, botlvl, toplvl, 1, tmp->nested[i]);
			}
		}
	}

	return;
}
/*}}}*/
/*{{{  void udv_dump_stack (FILE *stream, int indent, int slevel)*/
/*
 *	prints the current undefinedness stack (debugging)
 */
void udv_dump_stack (FILE *stream, int indent, int slevel)
{
	int i;
	udv_t *tmp;

	fprintf (stream, "udv_dump_stack: level = %d\n\t", udv_vstacklevel);
	for (tmp=udv_vstack; tmp; tmp=tmp->next) {
		fprintf (stream, "%-8s", WNameOf (NNameOf (tmp->nameof)));
	}
	fprintf (stream, "\n");
	for (i = slevel; i <= udv_vstacklevel; i++) {
		fprintf (stream, "%-3d:\t", i);
		for (tmp=udv_vstack; tmp; tmp=tmp->next) {
			fprintf (stream, "%-8s", ((tmp->state[i] >= 0) && (tmp->state[i] <= UDV_INVALID)) ? udvsstatestrings[(int)(tmp->state[i])] : "");
		}
		fprintf (stream, "\n");
	}
	return;
}
/*}}}*/
/*{{{  void udv_dump_udvt (FILE *stream, udv_t *var, int slevel)*/
/*
 *	prints (debugging) info about a udv_t tree -- specifically for nested state
 */
void udv_dump_udvt (FILE *stream, udv_t *var, int slevel)
{
	int i, j;

	fprintf (stream, "udv_dump_udvt: nameof = ");
	printtreenl (stream, 4, var->nameof);
	fprintf (stream, "udv_dump_udvt: state:   ");
	for (i=slevel; i<=udv_vstacklevel; i++) {
		fprintf (stream, "%d\t", i);
	}
	fprintf (stream, "\n                        ");
	for (i=slevel; i<=udv_vstacklevel; i++) {
		fprintf (stream, "%s\t", ((var->state[i] >= 0) && (var->state[i] <= UDV_INVALID)) ? udvsstatestrings[(int)(var->state[i])] : "");
	}
	fprintf (stream, "\n");
	if (var->nested) {
		fprintf (stream, "udv_dump_udvt: nested state:\n");
		fprintf (stream, "  FIELD                 ");
		for (j=slevel; j<=udv_vstacklevel; j++) {
			fprintf (stream, "%d\t", j);
		}
		fprintf (stream, "\n");
		for (i=0; var->nested[i]; i++) {
			udv_t *nested = var->nested[i];

			if (nested->nameof) {
				fprintf (stream, "%-24s", WNameOf (NNameOf (nested->nameof)));
			} else {
				fprintf (stream, "%-24s", "<invalid?>");
			}
			for (j=slevel; j<=udv_vstacklevel; j++) {
				fprintf (stream, "%s\t", ((nested->state[j] >= 0) && (nested->state[j] < UDV_INVALID)) ? udvsstatestrings[(int)(nested->state[j])] : "");
			}
			fprintf (stream, "\n");
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE int udv_nesting_maxdepth (udv_t *udv)*/
/*
 *	returns the maximum nesting depth of a udv_t structure
 */
PRIVATE int udv_nesting_maxdepth (udv_t *udv)
{
	int maxdepth = 0;
	int i, j;

	if (udv->nested && udv->nested[0]) {
		for (i=0; udv->nested[i]; i++) {
			j = udv_nesting_maxdepth (udv->nested[i]);
			if (j > maxdepth) {
				maxdepth = j;
			}
		}
		return maxdepth + 1;
	}
	return maxdepth;
}
/*}}}*/
/*{{{  PRIVATE int udv_nesting_maxwidth (udv_t *udv)*/
/*
 *	returns the maximum nesting "width" of a udv_t structure
 */
PRIVATE int udv_nesting_maxwidth (udv_t *udv)
{
	int maxwidth = 0;
	int i, j;

	if (udv->nested && udv->nested[0]) {
		for (i=0; udv->nested[i]; i++) {
			j = udv_nesting_maxwidth (udv->nested[i]);
			if (j > maxwidth) {
				maxwidth = j;
			}
		}
		if (i > maxwidth) {
			maxwidth = i;
		}
	}
	return maxwidth;
}
/*}}}*/
/*{{{  PRIVATE int udv_strofexp (char *buf, int buflen, treenode *exp)*/
/*
 *	fills a string representing this expression
 *	returns the number of bytes set
 */
PRIVATE int udv_strofexp (char *buf, int buflen, treenode *exp)
{
	int left = buflen;
	char *ch = buf;

	switch (nodetypeoftag (TagOf (exp))) {
	case NAMENODE:
		{
			int touse = WLengthOf (NNameOf (exp));
			char *tch;

			if (touse >= buflen) {
				touse = buflen - 1;
			}
			strncpy (ch, WNameOf (NNameOf (exp)), touse);
			ch[touse] = '\0';
			tch = strchr (ch, '$');
			if (tch) {
				/* anon chan-type fiddle */
				if (tch[2] == 's') {
					*tch = '?';
				} else {
					*tch = '!';
				}
				tch[1] = '\0';
				touse = (int)(tch - ch) + 1;
			}
#if 0
fprintf (stderr, "udv_strofexp: returning %d \"%s\" for: ", touse, buf);
printtreenl (stderr, 4, exp);
#endif
			return touse;
		}
	case ARRAYSUBNODE:
		{
			int set = udv_strofexp (buf, buflen, ASBaseOf (exp));

			left -= set;
			ch += set;
			if (TagOf (ASIndexOf (exp)) == N_FIELD) {
				int touse = WLengthOf (NNameOf (ASIndexOf (exp)));

				if ((touse + 2) > left) {
					touse = left - 2;
				}
				*ch = '[';
				ch++;
				strncpy (ch, WNameOf (NNameOf (ASIndexOf (exp))), touse);
				ch += touse;
				*ch = ']';
				ch++;
				*ch = '\0';
				set += (touse + 2);
			} else {
				if (left < 5) {
					strncpy (ch, "[]", 2);
					ch += 2;
					set += 2;
				} else {
					strncpy (ch, "[..]", 4);
					ch += 4;
					set += 4;
				}
			}
			buf[set] = '\0';
#if 0
fprintf (stderr, "udv_strofexp: returning %d \"%s\" for: ", set, buf);
printtreenl (stderr, 4, exp);
#endif
			return set;
		}
	case SEGMENTNODE:
		{
			int set = udv_strofexp (ch, left, SNameOf (exp));

			left -= set;
			ch += set;
			buf[set] = '\0';

			return set;
		}
	default:
		return 0;
	}
}
/*}}}*/
/*{{{  PRIVATE int udv_getclaimname (char *buf, int buflen, treenode *claimvar)*/
/*
 *	fills a string for something CLAIMed -- sorting anonymous channel-types automagically
 *	returns the number of bytes written
 */
PRIVATE int udv_getclaimname (char *buf, int buflen, treenode *claimvar)
{
	char *ch = buf;
	int left = buflen;

	switch (nodetypeoftag (TagOf (claimvar))) {
	case NAMENODE:
		{
			int touse = WLengthOf (NNameOf (claimvar));
			char *tch;

			if (touse > buflen) {
				touse = buflen;
			}
			strncpy (ch, WNameOf (NNameOf (claimvar)), touse);
			tch = strchr (ch, '$');
			if (tch) {
				/* anon chan-type fiddle */
				if (tch[2] == 's') {
					*tch = '?';
				} else {
					*tch = '!';
				}
				tch[1] = '\0';
				touse = (int)(tch - ch) + 1;
			}
			return touse;
		}
	case ARRAYSUBNODE:
		{
			int set = udv_getclaimname (ch, buflen, ASBaseOf (claimvar));

			left -= set;
			ch += set;
			if (left > 8) {
				strcpy (ch, " element");
				ch += 8;
				left -= 8;
				set += 8;
			} else if (left > 2) {
				strcpy (ch, "[]");
				ch += 2;
				left -= 2;
				set += 2;
			}
			return set;
		}
	default:
		strcpy (ch, "(unknown)");
		return 9;
	}
	return 0;
}
/*}}}*/
/*{{{  PRIVATE void udv_error_now (int code, SOURCEPOSN locn, const char *string)*/
/*
 *	generates an error now about something
 */
PRIVATE void udv_error_now (int code, SOURCEPOSN locn, const char *string)
{
	msg_out_s (SEV_ERR_JMP, USE, code, locn, string);
	return;
}
/*}}}*/
/*{{{  PRIVATE void udv_error_nowi (int code, SOURCEPOSN locn, int v)*/
/*
 *	generates an error now about something (with an integer argument)
 */
PRIVATE void udv_error_nowi (int code, SOURCEPOSN locn, int v)
{
	msg_out_i (SEV_ERR_JMP, USE, code, locn, v);
	return;
}
/*}}}*/
/*{{{  PRIVATE void udv_new_warning (int code, SOURCEPOSN locn, const char *string)*/
/*
 *	queues a new warning about something
 */
PRIVATE void udv_new_warning (int code, SOURCEPOSN locn, const char *string)
{
	udv_warning_t *after, *tmp;

#if 0
fprintf (stderr, "udv_new_warning (): code = %d, locn = %d, string = %s\n", code, (int)locn, string);
#endif
	after = NULL;
	if (strict_checking) {
		/* set with -strict compiler option, generate error now */
		msg_out_s (SEV_ERR, USE, code, locn, string);
		return;
	}
	for (tmp = udv_wstack; tmp && (tmp->locn < locn); after = tmp, tmp = tmp->next);
	for (; tmp && (tmp->locn == locn) && (strcmp (tmp->str, string) < 0); after = tmp, tmp = tmp->next);
	if (tmp && (tmp->locn == locn) && (tmp->str && string && !strcmp (tmp->str, string)) && (tmp->err == code)) {
		return;
	}
	tmp = (udv_warning_t *)newvec (sizeof (udv_warning_t));
	tmp->locn = locn;
	tmp->str = (char *)newvec (strlen (string) + 1);
	strcpy (tmp->str, string);
	tmp->err = code;
	if (!after) {
		tmp->next = udv_wstack;
		udv_wstack = tmp;
	} else {
		tmp->next = after->next;
		after->next = tmp;
	}
	tmp->itrace_len = udv_itraceptr;
	if (udv_itraceptr) {
		int i, amount;

		amount = (udv_itraceptr >= UDV_ITRACE_MAX) ? UDV_ITRACE_MAX : udv_itraceptr;
		tmp->itrace = (treenode **)newvec (amount * sizeof (treenode *));
		for (i=0; i<amount; i++) {
			tmp->itrace[i] = udv_itrace[i];
		}
	} else {
		tmp->itrace = NULL;
	}
#if 0
fprintf (stderr, "udv_new_warning(): added something to the warnings list.  now:\n");
for (tmp = udv_wstack; tmp; tmp = tmp->next) {
fprintf (stderr, "    ->code = %d, ->locn = %d, ->str = %s\n", tmp->code, (int)(tmp->locn), tmp->str);
#endif
	return;
}
/*}}}*/
/*{{{  PRIVATE void udv_gen_warnings (void)*/
/*
 *	generates warnings
 */
PRIVATE void udv_gen_warnings (void)
{
	udv_warning_t *tmp;
	int i;

	while (udv_wstack) {
		tmp = udv_wstack;
		udv_wstack = udv_wstack->next;
		if (tmp->itrace_len) {
			for (i=0; i<tmp->itrace_len; i++) {
				msg_out_s (SEV_WARN, USE, USE_INCALLOF, LocnOf (tmp->itrace[i]), WNameOf (NNameOf (INameOf (tmp->itrace[i]))));
			}
			freevec (tmp->itrace, tmp->itrace_len * sizeof (treenode *));
		}
		msg_out_s (SEV_WARN, USE, tmp->err, tmp->locn, tmp->str);
		freevec (tmp->str, strlen (tmp->str) + 1);
		freevec (tmp, sizeof (udv_warning_t));
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void freeudv (udv_t *udv)*/
/*
 *	frees a udv_t structure
 */
PRIVATE void freeudv (udv_t *udv)
{
	freevec (udv, sizeof (udv_t));
	return;
}
/*}}}*/
/*{{{  PRIVATE udv_t *newudv (void)*/
/*
 *	allocates a new udv_t structure
 */
PRIVATE udv_t *newudv (void)
{
	udv_t *tmp;
	int i;

	tmp = (udv_t *)newvec (sizeof (udv_t));
	tmp->next = NULL;
	tmp->nameof = NULL;
	tmp->nested = NULL;
	tmp->did_warn = 0;
	tmp->type = UDVT_INVALID;
	
	for (i = 0; i < MAX_NESTING; ++i) {
		tmp->state[i] = UDV_UNDEFINED;
	}

	return tmp;
}
/*}}}*/
/*{{{  PRIVATE BOOL udv_cmatch (treenode *v1, treenode *v2)*/
/*
 *	checks to see if two things (involved with CLAIM) are the same -- ie, if something is CLAIMed
 *	when using, or that something isn't obviously CLAIMed twice
 */
PRIVATE BOOL udv_cmatch (treenode *v1, treenode *v2)
{
	for (;;) {
#if 0
fprintf (stderr, "udv_cmatch: v1 (%p) = ", v1);
printtreenl (stderr, 4, v1);
fprintf (stderr, "udv_cmatch: v2 (%p) = ", v2);
printtreenl (stderr, 4, v2);
#endif
		if (TagOf (v1) != TagOf (v2)) {
			/* can't possibly be the same thing! */
			return FALSE;
		}
		switch (TagOf (v1)) {
		case N_DECL:
		case N_PARAM:
		case N_VALPARAM:
		case N_ABBR:
		case N_VALABBR:
		case N_RESULTPARAM:
		case N_REPL:
			if (v1 == v2) {
				return TRUE;
			}
			return FALSE;
		case S_ARRAYSUB:
		case S_ARRAYITEM:
		case S_RECORDSUB:
		case S_RECORDITEM:
			if (!udv_cmatch (ASBaseOf (v1), ASBaseOf (v2))) {
				return FALSE;
			}
			v1 = ASIndexOf (v1);
			v2 = ASIndexOf (v2);
			break;
		default:
			switch (nodetypeoftag (TagOf (v1))) {
			case DOPNODE:
				if (!udv_cmatch (LeftOpOf (v1), LeftOpOf (v2))) {
					return FALSE;
				}
				v1 = RightOpOf (v1);
				v2 = RightOpOf (v2);
				break;
			case MOPNODE:
				v1 = OpOf (v1);
				v2 = OpOf (v2);
				break;
			case CONSTEXPNODE:
				if (LoValOf (v1) == LoValOf (v2)) {
					return TRUE;
				}
				return FALSE;
			default:
				return FALSE;
			}
			break;
		}
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE BOOL udv_isclaimed (treenode *var)*/
PRIVATE BOOL udv_isclaimed (treenode *var)
{
	udv_cstk *tmp;
	
#if 0
fprintf (stderr, "udv_isclaimed: var = ");
printtreenl (stderr, 4, var);
#endif
	for (tmp = udv_cstack; tmp; tmp = tmp->next) {
		if (udv_cmatch (tmp->claimed, var)) {
			return TRUE;
		}
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE BOOL udv_newclaim (treenode *var)*/
/*
 *	this adds a CLAIM to the list, but checks that it's not already there
 *	first.  If OK, returns TRUE, otherwise returns FALSE and doesn't change anything
 */
PRIVATE BOOL udv_newclaim (treenode *var)
{
	udv_cstk *tmp;

#if 0
fprintf (stderr, "udv_newclaim: var = ");
printtreenl (stderr, 4, var);
#endif
	if (udv_isclaimed (var)) {
		return FALSE;
	}
	/* make a new one and add it */
	tmp = (udv_cstk *)newvec (sizeof (udv_cstk));
	tmp->next = udv_cstack;
	tmp->claimed = var;
	udv_cstack = tmp;
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE void udv_freeclaim (treenode *var)*/
/*
 *	frees the head of the claim list -- must match!
 */
PRIVATE void udv_freeclaim (treenode *var)
{
	udv_cstk *tmp;

	if (!udv_cstack) {
		msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, udv_locn, "udv_freeclaim: nothing CLAIMed");
		return;
	}
	if (!udv_cmatch (udv_cstack->claimed, var)) {
		msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, udv_locn, "udv_freeclaim: this is not CLAIMed");
		return;
	}
	tmp = udv_cstack;
	udv_cstack = udv_cstack->next;
	freevec (tmp, sizeof (udv_cstk));
	return;
}
/*}}}*/
/*{{{  PRIVATE BOOL udv_isfixed (treenode *var)*/
/*
 *	tests whether the specified name is FIXED.  this information is left
 *	by an earlier stage in the usage-checker
 */
PRIVATE BOOL udv_isfixed (treenode *var)
{
	switch (TagOf (var)) {
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		if (NTypeAttrOf (var) & TypeAttr_fixed) {
			return TRUE;
		}
		break;
	default:
		break;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE void udv_pushstack (void)*/
/*
 *	pushes the stack down
 */
PRIVATE void udv_pushstack (void)
{
	udv_vstacklevel++;
	if (udv_vstacklevel == MAX_NESTING) {
		msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, udv_locn, "udv_pushstack (MAX_NESTING reached)");
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_copy_nested (int old_level, int new_level, udv_t *udv, BOOL copy_chan_state)*/
/*
 *	copies nested state from state[old_level] to state[new_level].
 *	calls itself recursively for deep structures.
 */
PRIVATE void vstack_copy_nested (int old_level, int new_level, udv_t *udv, BOOL copy_chan_state)
{
	if (copy_chan_state) {
		udv->state[new_level] = udv->state[old_level];
	} else {
		switch (udv->state[old_level]) {
		case UDV_INPUTTED:
		case UDV_OUTPUTTED:
			udv->state[new_level] = UDV_UNDEFINED;
			break;
		default:
			udv->state[new_level] = udv->state[old_level];
			break;
		}
	}
	if (udv->nested) {
		int i;

		for (i=0; udv->nested[i]; i++) {
			vstack_copy_nested (old_level, new_level, udv->nested[i], copy_chan_state);
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_copy (int old_level, int new_level, BOOL copy_chan_state)*/
/*
 *	copies vstack info from state[old_level] to state[new_level]
 */
PRIVATE void vstack_copy (int old_level, int new_level, BOOL copy_chan_state)
{
	udv_t *tmp;

#if 0
fprintf (stderr, "vstack_copy from %d -> %d (copy_chan_state = %d)\n", old_level, new_level, (int)copy_chan_state);
#endif
	if (old_level != new_level) {
		for (tmp = udv_vstack; tmp; tmp = tmp->next) {
			vstack_copy_nested (old_level, new_level, tmp, copy_chan_state);
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_submerge_replseq (int old_level, int new_level, udv_t *udv, int count_known, int count_val)*/
/*
 *	performs a substate merge for replicated SEQs
 */
PRIVATE void vstack_submerge_replseq (int old_level, int new_level, udv_t *udv, int count_known, int count_val)
{
	int i;

	if (count_known && (count_val > 0)) {
		udv->state[new_level] = udv->state[old_level];
	} else {
		switch (udv->state[old_level]) {
		case UDV_UNDEFINED:
		case UDV_DEFINED:
			if (udv->state[old_level] != udv->state[new_level]) {
				udv->state[new_level] = UDV_UNKNOWN;
			} /* else unchanged */
			break;
		case UDV_UNKNOWN:
			/* the loop contents may not execute at all, so leave as unknown */
			udv->state[new_level] = UDV_UNKNOWN;
			break;
		}
	}
	if (udv->nested) {
		for (i=0; udv->nested[i]; i++) {
			vstack_submerge_replseq (old_level, new_level, udv->nested[i], count_known, count_val);
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_merge_while (int old_level, int new_level)*/
/*
 *	merges two vstack levels together, updating `new_level' with info from `old_level'
 *	this handles WHILE loops..
 */
PRIVATE void vstack_merge_while (int old_level, int new_level)
{
	udv_t *tmp;

	if (old_level == new_level) {
		return;
	}
#if 0
fprintf (stderr, "vstack_merge_while: merging from %d -> %d\n", old_level, new_level);
fprintf (stderr, "\t");
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", WNameOf (NNameOf (tmp->nameof)));
}
fprintf (stderr, "\n");

fprintf (stderr, "%-3d:\t", new_level);
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", udvsstatestrings[tmp->state[new_level]]);
}
fprintf (stderr, "\n");
fprintf (stderr, "%-3d:\t", old_level);
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", udvsstatestrings[tmp->state[old_level]]);
}
fprintf (stderr, "\n");

#endif
	for (tmp = udv_vstack; tmp; tmp = tmp->next) {
		switch (tmp->state[old_level]) {
		case UDV_UNDEFINED:
		case UDV_DEFINED:
			if (tmp->state[old_level] != tmp->state[new_level]) {
				tmp->state[new_level] = UDV_UNKNOWN;
			} /* else unchanged */
			break;
		case UDV_UNKNOWN:
			/* the loop contents may not execute at all, so leave as unknown */
			tmp->state[new_level] = UDV_UNKNOWN;
			break;
		case UDV_PARTIAL:
			tmp->state[new_level] = UDV_PARTIAL;
			break;
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_merge_if (int high_level, int new_level)*/
/*
 *	merges stack levels from [new_level + 1 .. high_level] down into new_level
 */
PRIVATE void vstack_merge_if (int high_level, int new_level)
{
	int n_levels = high_level - new_level;
	int n_start = new_level + 1;
	int i;
	udv_t *tmp;

	if (!n_levels) {
		/* IF with no conditions ?? */
		return;
	}
#if 0
fprintf (stderr, "vstack_merge_if: merging from [%d .. %d] -> %d\n", n_start, high_level, new_level);
fprintf (stderr, "\t");
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", WNameOf (NNameOf (tmp->nameof)));
}
fprintf (stderr, "\n");
for (i=new_level; i <= high_level; i++) {
fprintf (stderr, "%-3d:\t", i);
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", (tmp->state[i] == UDV_DEFINED) ? "DEF" : ((tmp->state[i] == UDV_UNDEFINED) ? "UNDEF" : ((tmp->state[i] == UDV_UNKNOWN) ? "UNKNOWN" : "<chan>")));
}
fprintf (stderr, "\n");
}
#endif
	for (tmp = udv_vstack; tmp; tmp = tmp->next) {
		int n_defining, n_undefining, n_unknowing;

		switch (tmp->state[new_level]) {
		case UDV_UNDEFINED:
		case UDV_UNKNOWN:
			/* if all branches define this, set as defined,
			 * if some define it, set as unknown,
			 * if non define it, leave as undefined/unknown
			 */
			n_defining = 0;
			for (i = 0; i < n_levels; i++) {
				if (tmp->state[n_start + i] == UDV_DEFINED) {
					n_defining++;
				}
			}
			if (n_defining == n_levels) {
				tmp->state[new_level] = UDV_DEFINED;
			} else if (n_defining > 0) {
				tmp->state[new_level] = UDV_UNKNOWN;
			}
			break;
		case UDV_DEFINED:
			/* if all branches undefine this, set as undefined,
			 * if some undefine it or unknown it, set as unknown
			 * otherwise leave defined
			 */
			n_undefining = 0;
			n_unknowing = 0;
			for (i=0; i<n_levels; i++) {
				switch (tmp->state[n_start + i]) {
				case UDV_UNDEFINED:
					n_undefining++;
					break;
				case UDV_UNKNOWN:
					n_unknowing++;
					break;
				}
			}
			if (n_undefining == n_levels) {
				tmp->state[new_level] = UDV_UNDEFINED;
			} else if ((n_undefining > 0) || (n_unknowing > 0)) {
				tmp->state[new_level] = UDV_UNKNOWN;
			}
			break;
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_merge_alt (int high_level, int new_level)*/
/*
 *	merges stack levels from [new_level + 1 .. high_level] down into new_level for an ALT
 */
PRIVATE void vstack_merge_alt (int high_level, int new_level)
{
	vstack_merge_if (high_level, new_level);
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_update_substate (udv_t *udv, int slevel)*/
/*
 *	updates state(s) to reflect contents of sub-states (at the given stack level)
 *	typically called after some merge
 */
PRIVATE void vstack_update_substate (udv_t *udv, int slevel)
{
	int n_undefined = 0;
	int n_unknown = 0;
	int n_partial = 0;
	int n_io = 0;
	int n_defined = 0;
	int i;

	if (!udv || !udv->nested) {
		return;
	}
	/* if there is nested sub-state, update that first */
	for (i=0; udv->nested[i]; i++) {
		if (udv->nested[i]->nested) {
			vstack_update_substate (udv->nested[i], slevel);
		}
	}
	/* count up for this one */
	for (i=0; udv->nested[i]; i++) {
		switch (udv->nested[i]->state[slevel]) {
		case UDV_UNDEFINED:
			n_undefined++;
			break;
		case UDV_UNKNOWN:
			n_unknown++;
			break;
		case UDV_PARTIAL:
			n_partial++;
			break;
		case UDV_INPUTTED:
		case UDV_OUTPUTTED:
		case UDV_XINPUTTED:
			n_io++;
			break;
		case UDV_DEFINED:
			n_defined++;
			break;
		}
	}
	/* update state -- if needed */
	switch (udv->state[slevel]) {
	case UDV_UNDEFINED:
		/* if we're looking at an ordinary array or record, it might become partially defined depending on the substate */
		if ((udv->type == UDVT_VAR) && udv->nameof) {
			treenode *var = udv->nameof;
			treenode *type = NULL;

			switch (TagOf (var)) {
			case N_DECL:
			case N_ABBR:
			case N_PARAM:
			case N_RESULTPARAM:
				type = NTypeOf (var);
				type = follow_user_type (type);
				break;
			}
			if (type && (TagOf (type) == S_ARRAY)) {
				/* move from undefined state, possibly -- FIXME: we don't know about internal definedness states for arrays */
				if (!n_undefined && !n_unknown && !n_partial) {
					/* all defined, but we can't tell really */
					udv->state[slevel] = UDV_UNKNOWN;
				} else if (n_undefined) {
					/* undefined, assume worst case */
					udv->state[slevel] = UDV_UNDEFINED;
				} else if (n_partial) {
					/* propagate partial definedness */
					udv->state[slevel] = UDV_PARTIAL;
				} else {
					/* anything else is unknown */
					udv->state[slevel] = UDV_UNKNOWN;
				}
			} else if (type && (TagOf (type) == S_RECORD)) {
				if (!n_undefined && !n_unknown && !n_partial) {
					/* fully defined */
					udv->state[slevel] = UDV_DEFINED;
				} else if (n_undefined == i) {
					/* fully undefined */
					udv->state[slevel] = UDV_UNDEFINED;
				} else if (n_defined && !n_unknown) {
					/* partially defined */
					udv->state[slevel] = UDV_PARTIAL;
				} else if (n_unknown) {
					/* some unknown state */
					udv->state[slevel] = UDV_UNKNOWN;
				} else if (n_partial) {
					/* some partially defined substate */
					udv->state[slevel] = UDV_PARTIAL;
				} else {
					/* mixed -- consider partial */
					udv->state[slevel] = UDV_PARTIAL;
				}
			}
		}
		break;
	case UDV_INPUTTED:
	case UDV_OUTPUTTED:
	case UDV_XINPUTTED:
		break;		/* FIXME: leave this alone ? */
	case UDV_DEFINED:
	case UDV_UNKNOWN:
	case UDV_PARTIAL:
		if (!n_undefined && !n_unknown && !n_partial) {
			/* fully defined */
			udv->state[slevel] = UDV_DEFINED;
		} else if (n_undefined == i) {
			/* fully undefined */
			udv->state[slevel] = UDV_UNDEFINED;
		} else if (n_undefined && !n_unknown) {
			/* partially defined */
			udv->state[slevel] = UDV_PARTIAL;
		} else if (n_unknown) {
			/* some unknown state */
			udv->state[slevel] = UDV_UNKNOWN;
		} else if (n_partial) {
			/* partial state */
			udv->state[slevel] = UDV_PARTIAL;
		} else {
			/* mixed, consider unknown */
			udv->state[slevel] = UDV_UNKNOWN;
		}
		break;
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_submerge_par (udv_t *udv, int high_level, int new_level)*/
/*
 *	merges nested PAR state
 */
PRIVATE void vstack_submerge_par (udv_t *udv, int high_level, int new_level)
{
	int n_levels = high_level - new_level;
	int n_start = new_level + 1;
	int i;
	const char vstate = udv->state[new_level];

	for (i=0; i<n_levels; i++) {
		if (udv->state[n_start + i] != vstate) {
			/* only one branch is ever allowed to change it */
			switch (udv->type) {
			default:
				udv->state[new_level] = udv->state[n_start + i];
				break;
			case UDVT_CHAN:
				if (udv->state[n_start + i] == UDV_UNDEFINED) {
					/* skip -- don't populate undefinedness in channels */
				} else {
					udv->state[new_level] = udv->state[n_start + i];
				}
				break;
			}
		}
	}
	/* do nested state */
	if (udv->type != UDVT_MOB_CT) {
		if (udv->nested) {
			for (i=0; udv->nested[i]; i++) {
				vstack_submerge_par (udv->nested[i], high_level, new_level);
			}
		}
		vstack_update_substate (udv, new_level);
	}

	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_merge_par (int high_level, int new_level)*/
/*
 *	merges stack levels from [new_level + 1 .. high_level] down into new_level
 */
PRIVATE void vstack_merge_par (int high_level, int new_level)
{
	udv_t *tmp;

	if (new_level == high_level) {
		/* PAR with no branches ?? */
		return;
	}
#if 0
fprintf (stderr, "vstack_merge_par: (before merging from [%d .. %d] -> %d)\n", new_level + 1, high_level, new_level);
udv_dump_stack_bit (stderr, new_level, high_level, TRUE);
#endif
	for (tmp = udv_vstack; tmp; tmp = tmp->next) {
		vstack_submerge_par (tmp, high_level, new_level);
	}
#if 0
fprintf (stderr, "vstack_merge_par: (after merging from [%d .. %d] -> %d)\n", n_start, high_level, new_level);
fprintf (stderr, "\t");
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", WNameOf (NNameOf (tmp->nameof)));
}
fprintf (stderr, "\n");
for (i=new_level; i <= high_level; i++) {
fprintf (stderr, "%-3d:\t", i);
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", (tmp->state[i] == UDV_DEFINED) ? "DEF" : ((tmp->state[i] == UDV_UNDEFINED) ? "UNDEF" : ((tmp->state[i] == UDV_UNKNOWN) ? "UNKNOWN" : "<chan>")));
}
fprintf (stderr, "\n");
}
#endif
#if 0
fprintf (stderr, "vstack_merge_par: (after merging from [%d .. %d] -> %d)\n", new_level + 1, high_level, new_level);
udv_dump_stack_bit (stderr, new_level, high_level, TRUE);
#endif
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_merge_replpar (int old_level, int new_level)*/
/*
 *	merges stack[old_level] into stack[new_level] for a replicated PAR
 */
PRIVATE void vstack_merge_replpar (int old_level, int new_level)
{
	udv_t *tmp;

	/* update new with old */
	for (tmp = udv_vstack; tmp; tmp = tmp->next) {
		tmp->state[new_level] = tmp->state[old_level];
	}
#if 0
fprintf (stderr, "vstack_merge_replpar: (after merge from %d to %d)\n", old_level, new_level);
fprintf (stderr, "\t");
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", WNameOf (NNameOf (tmp->nameof)));
}
fprintf (stderr, "\n");

fprintf (stderr, "%-3d:\t", old_level);
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", (tmp->state[old_level] == UDV_DEFINED) ? "DEF" : ((tmp->state[old_level] == UDV_UNDEFINED) ? "UNDEF" : ((tmp->state[old_level] == UDV_UNKNOWN) ? "UNKNOWN" : "<chan>")));
}
fprintf (stderr, "\n");
fprintf (stderr, "%-3d:\t", new_level);
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-8s", (tmp->state[new_level] == UDV_DEFINED) ? "DEF" : ((tmp->state[new_level] == UDV_UNDEFINED) ? "UNDEF" : ((tmp->state[new_level] == UDV_UNKNOWN) ? "UNKNOWN" : "<chan>")));
}
fprintf (stderr, "\n");
#endif
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_merge_replseq (int old_level, int new_level, treenode *countexp)*/
/*
 *	merges stack[old_level] into stack[new_level] for a replicated SEQ
 */
PRIVATE void vstack_merge_replseq (int old_level, int new_level, treenode *countexp)
{
	udv_t *tmp;
	int count_known = 0, count_val = 0;

	if (old_level == new_level) {
		return;
	}
	count_known = is_evaluable (countexp);
	if (count_known) {
		count_val = eval_const_treenode (countexp);
	}
#if 0
fprintf (stderr, "vstack_merge_replseq: (before merging from %d -> %d).  count_known = %d, count_val = %d\n", old_level, new_level, count_known, count_val);
fprintf (stderr, "\t");
udv_dump_stack_bit (stderr, new_level, old_level, 1);

#if 0
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-12s", WNameOf (NNameOf (tmp->nameof)));
}
fprintf (stderr, "\n");
{ int i, j;
for (i=new_level; i <= old_level; i++) {
fprintf (stderr, "%-3d:\t", i);
for (tmp=udv_vstack; tmp; tmp=tmp->next) {
fprintf (stderr, "%-12s", tmp->nested ? udvnsstatestrings[(int)(tmp->state[i])] : udvsstatestrings[(int)(tmp->state[i])]);
}
fprintf (stderr, "\n");
}}
#endif
#endif
	for (tmp = udv_vstack; tmp; tmp = tmp->next) {
		vstack_submerge_replseq (old_level, new_level, tmp, count_known, count_val);
		vstack_update_substate (tmp, new_level);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_merge_replif (int old_level, int new_level)*/
/*
 *	merges stack[old_level] into stack[new_level] for a replicated IF
 */
PRIVATE void vstack_merge_replif (int old_level, int new_level)
{
	if (old_level == new_level) {
		return;
	}
	/* just copy state.  either the nested stuff gets executed or STOP does */
	vstack_copy (old_level, new_level, TRUE);
	return;
}
/*}}}*/
/*{{{  PRIVATE void vstack_merge_replalt (int old_level, int new_level)*/
/*
 *	merges stack[old_level] into stack[new_level] for a replicated ALT
 */
PRIVATE void vstack_merge_replalt (int old_level, int new_level)
{
	vstack_merge_while (old_level, new_level);
	return;
}
/*}}}*/
/*{{{  PRIVATE BOOL have_undefined (udv_udef_t *udef)*/
/*
 *	checks to see if `udef' has anything in it
 */
PRIVATE BOOL have_undefined (udv_udef_t *udef)
{
	if (udef->n_undefined || udef->n_unknown || udef->n_partial) {
		return TRUE;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE void udv_copy_substate (udv_t *dest, udv_t *source)*/
/*
 *	copies nested state from one udv_t to another
 */
PRIVATE void udv_copy_substate (udv_t *dest, udv_t *source)
{
	int i;

	if (!dest || !source || !dest->nested || !source->nested) {
		return;
	}
	/* fields should be in the same order.. */
	for (i=0; dest->nested[i] && source->nested[i]; i++) {
		dest->nested[i]->state[udv_vstacklevel] = source->nested[i]->state[udv_vstacklevel];
	}
	if (dest->nested[i] || source->nested[i]) {
		/* left-overs -- serious */
		msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, udv_locn, "udv_copy_substate -- mismatch");
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void udv_set_state (udv_t *dest, int level, char state)*/
/*
 *	sets state absolutely
 */
PRIVATE void udv_set_state (udv_t *dest, int level, char state)
{
	if (!dest) {
		return;
	}
	dest->state[level] = state;
	if (dest->nested) {
		int i;

		for (i = 0; dest->nested[i]; i++) {
			udv_set_state (dest->nested[i], level, state);
		}
	}
	return;
}
/*}}}*/

#ifdef MOBILES
FORWARD PRIVATEPARAM int do_undefinedcheckexp (treenode **nptr, void *const voidptr);

/*{{{  PRIVATE BOOL udv_thingismobile (treenode *n)*/
/*
 *	determines whether something is MOBILE
 */
PRIVATE BOOL udv_thingismobile (treenode *n)
{
	switch (TagOf (n)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
		if (TagOf (follow_user_type (NTypeOf (n))) == S_MOBILE) {
			return TRUE;
		}
		break;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE BOOL udv_thingisdynmobile (treenode *n)*/
/*
 *	determines whether something is a DYNAMIC mobile array (or channel array, not channel-tye arrays though)
 */
PRIVATE BOOL udv_thingisdynmobile (treenode *n)
{
	if (!n) {
		return FALSE;
	}
	switch (TagOf (n)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		{
			treenode *type = NTypeOf (n);

			if (TagOf (type) == N_TYPEDECL) {
				type = follow_user_type (type);
			}
			if (TagOf (type) == S_MOBILE) {
				if ((TagOf (MTypeOf (type)) == S_ARRAY) && (ARDimLengthOf (MTypeOf (type)) == NULL)) {
					return TRUE;
				}
			}
		}
		break;
	case S_ARRAYSUB:
	case S_ARRAYITEM:
		{
			treenode *type = chk_gettype (n);		/* auto follow-user-type */

			if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_ARRAY) && (!(ARDimLengthOf (MTypeOf (type))) || (TagOf (ARDimLengthOf (MTypeOf (type))) == S_NTH_DIMENSION))) {
				return TRUE;
			}
		}
		break;
	case S_UNDEFINED:
		return udv_thingisdynmobile (OpOf (n));
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL udv_thingisdynmobileproctype (treenode *n)*/
/*
 *	determines whether something is a dynamic mobile-process
 */
PUBLIC BOOL udv_thingisdynmobileproctype (treenode *n)
{
	if (!n) {
		return FALSE;
	}
	switch (TagOf (n)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		{
			treenode *type = NTypeOf (n);

			if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == N_PROCTYPEDECL)) {
				return TRUE;
			}
		}
		return FALSE;
	case S_ARRAYSUB:
	case S_ARRAYITEM:
	case S_RECORDSUB:
	case S_RECORDITEM:
		{
			treenode *type = chk_gettype (n);

			if (type && (((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == N_PROCTYPEDECL)) || (TagOf (type) == N_PROCTYPEDECL))) {
				return TRUE;
			}
		}
		return FALSE;
	case S_UNDEFINED:
		return udv_thingisdynmobileproctype (OpOf (n));
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL udv_thingisdynmobilechantype (treenode *n)*/
/*
 *	determines whether something is a dynamic mobile chan-type variable
 */
PUBLIC BOOL udv_thingisdynmobilechantype (treenode *n)
{
	if (!n) {
		return FALSE;
	}
	switch (TagOf (n)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		{
			treenode *type = NTypeOf (n);

			if (TagOf (type) == N_TYPEDECL) {
				type = follow_user_type (type);
			}
			if ((TagOf (type) == S_ANYCHANTYPE) || (TagOf (type) == S_ANYPROCTYPE)) {
				return TRUE;
			}
			if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_RECORD)) {
				/* MOBILE record, check for channel presence */
				treenode *field = ARTypeOf (MTypeOf (type));
				BOOL havechans = FALSE;

				while (field && (TagOf (field) == S_DECL)) {
					treenode *item = NTypeOf (DNameOf (field));

					if (TagOf (item) == S_CHAN) {
						havechans = TRUE;
						break;	/* while () */
					}
					field = DBodyOf (field);
				}
				return havechans;
			}
		}
		break;
	case S_ARRAYSUB:
	case S_ARRAYITEM:
		{
			treenode *type = chk_gettype (n);		/* auto follow-user-type */

			if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_RECORD)) {
				/* MOBILE record, check for channel presence */
				treenode *field = ARTypeOf (MTypeOf (type));
				BOOL havechans = FALSE;

				while (field && (TagOf (field) == S_DECL)) {
					treenode *item = NTypeOf (DNameOf (field));

					if (TagOf (item) == S_CHAN) {
						havechans = TRUE;
						break;	/* while () */
					}
					field = DBodyOf (field);
				}
				return havechans;
			}
		}
		break;
	case S_UNDEFINED:
		return udv_thingisdynmobilechantype (OpOf (n));
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE BOOL udv_thingisdynmobilebarriertype (treenode *n)*/
/*
 *	determines whether something is a dynamic MOBILE BARRIER
 */
PRIVATE BOOL udv_thingisdynmobilebarriertype (treenode *n)
{
	if (!n) {
		return FALSE;
	}
	switch (TagOf (n)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		{
			treenode *type = NTypeOf (n);

			if (TagOf (type) == N_TYPEDECL) {
				type = follow_user_type (type);
			}
			if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_FULLBARRIER)) {
				return TRUE;
			}
		}
		break;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE void mchecksimpleprotocol (treenode **const pptr, treenode **const iptr, const INT32 pitem)*/
/*
 *	checks a simple protocol for MOBILE usage
 */
PRIVATE void mchecksimpleprotocol (treenode **const pptr, treenode **const iptr, const INT32 pitem)
{
	if (TagOf (follow_user_type (*pptr)) == S_MOBILE) {
		/* then iptr must be a MOBILE thing */
		if (udv_thingisdynmobilechantype (*iptr)) {
			/* check that any chan-type is not claimed */
			const int oldmode = udv_defaultmode;

			udv_defaultmode = EXP_WRITTEN;
			modprewalktree (iptr, do_undefinedcheckexp, NULL);
			udv_defaultmode = oldmode;
		}
		udv_ismobile = TRUE;
		modprewalktree (iptr, do_undefinedcheckexp, NULL);
		udv_ismobile = FALSE;
	} else {
		modprewalktree (iptr, do_undefinedcheckexp, NULL);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void mcheckseqprotocol (treenode **pptr, treenode **iptr)*/
/*
 *	checks a sequential protocol for MOBILE usage
 */
PRIVATE void mcheckseqprotocol (treenode **pptr, treenode **iptr)
{
	int pitem = 1;
	const BOOL anyprotocol = (TagOf (*pptr) == S_ANY);

	while (!EndOfList (*iptr) && (anyprotocol || !EndOfList (*pptr))) {
		if (anyprotocol) {
			mchecksimpleprotocol (pptr, ThisItemAddr (*iptr), pitem);
		} else {
			mchecksimpleprotocol (ThisItemAddr (*pptr), ThisItemAddr (*iptr), pitem);
			pptr = NextItemAddr (*pptr);
		}
		iptr = NextItemAddr (*iptr);
		pitem++;
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void mcheckprotocol (treenode **protocol, treenode **instance)*/
/*
 *	checks protocol usage for MOBILES
 */
PRIVATE void mcheckprotocol (treenode **protocol, treenode **instance)
{
#if 0
fprintf (stderr, "mcheckprotocol: *protocol is: ");
printtreenl (stderr, 4, *protocol);
fprintf (stderr, "mcheckprotocol: *instance is: ");
printtreenl (stderr, 4, *instance);
#endif
	switch (TagOf (*protocol)) {
	case N_SPROTDEF:
		mcheckseqprotocol (NTypeAddr (*protocol), instance);
		break;
	case S_ANY:
		mcheckseqprotocol (protocol, instance);
		break;
	case N_TPROTDEF:
		{
			treenode *instancetag;
			treenode *tagptr;
			treenode **sptr;
			BOOL found;

			if (TagOf (*instance) == S_LIST) {
				instancetag = ThisItem (*instance);
			} else {
				instancetag = *instance;
			}
			tagptr = NTypeOf (*protocol);
			found = FALSE;
			while (!EndOfList (tagptr) && !found) {
				if (ThisItem (tagptr) == instancetag) {
					tagptr = ThisItem (tagptr);
					found = TRUE;
				} else {
					tagptr = NextItem (tagptr);
				}
			}
			if (tagptr) {
				sptr = NTypeAddr (tagptr);
				if (!EmptyList (*sptr)) {
					mcheckseqprotocol (sptr, NextItemAddr (*instance));
				}
			}
		}
		break;
	default:
		mchecksimpleprotocol (protocol, ThisItemAddr (*instance), 1);
		break;
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void udv_dynmobile_targetcheck (treenode **nptr)*/
/*
 *	checks target and inserts/removes UNDEFINEDness as appropiate
 */
PRIVATE void udv_dynmobile_targetcheck (treenode **nptr)
{
	udv_udef_t local_undefined;

#if 0
fprintf (stderr, "udv_dynmobile_targetcheck: *nptr =");
printtreenl (stderr, 4, *nptr);
#endif
	if (TagOf (*nptr) == S_LIST) {
		treenode *walk;

		for (walk = *nptr; !EmptyList (walk); walk = NextItem (walk)) {
			if (udv_thingisdynmobile (ThisItem (walk)) || udv_thingisdynmobilechantype (ThisItem (walk)) || udv_thingisdynmobileproctype (ThisItem (walk))) {
				local_undefined.n_undefined = 0;
				local_undefined.n_unknown = 0;
				local_undefined.n_partial = 0;
				local_undefined.supress_warn = 0;
				modprewalktree (ThisItemAddr (walk), do_undefinedcheckexp, &local_undefined);

				/* always insert UNDEFINED node -- tagging the fact we got here anyway */
				if (TagOf (ThisItem (walk)) != S_UNDEFINED) {
					SetLeft (walk, newmopnode (S_UNDEFINED, LocnOf (*nptr), ThisItem (walk), 0));
				}
				/* if there is some definedness (before the input) add 1 to MOpTypeOf for UNDEFINED */
#if 0
fprintf (stderr, "traced input: n_undefined = %d, n_unknown = %d\n", local_undefined.n_undefined, local_undefined.n_unknown);
#endif

				if (local_undefined.n_undefined != 1) {
					SetMOpType (ThisItem (walk), MOpTypeOf (ThisItem (walk)) + 1);
				}
			} else {
#if 0
fprintf (stderr, "udv_dynmobile_targetcheck: about to do_undefinedcheckexp with");
printtreenl (stderr, 4, ThisItem (walk));
#endif
				modprewalktree (ThisItemAddr (walk), do_undefinedcheckexp, NULL);
			}
		}
	} else if (udv_thingisdynmobile (*nptr) || udv_thingisdynmobilechantype (*nptr) || udv_thingisdynmobileproctype (*nptr)) {
		local_undefined.n_undefined = 0;
		local_undefined.n_unknown = 0;
		local_undefined.n_partial = 0;
		local_undefined.supress_warn = 0;
		modprewalktree (nptr, do_undefinedcheckexp, &local_undefined);

		/* always insert UNDEFINED node -- tagging the fact we got here anyway */
		if (TagOf (*nptr) != S_UNDEFINED) {
			*nptr = newmopnode (S_UNDEFINED, LocnOf (*nptr), *nptr, 0);
		}
		/* if there is some definedness (before the input) add 1 to MOpTypeOf for UNDEFINED */
#if 0
fprintf (stderr, "traced input: n_undefined = %d, n_unknown = %d.  MOpTypeOf(*nptr) = %d\n", local_undefined.n_undefined, local_undefined.n_unknown, MOpTypeOf (*nptr));
#endif
		if (local_undefined.n_undefined != 1) {
			SetMOpType (*nptr, MOpTypeOf (*nptr) + 1);
		}
	} else {
		modprewalktree (nptr, do_undefinedcheckexp, NULL);
	}
#if 0
fprintf (stderr, "udv_dynmobile_targetcheck(): leaving *nptr =");
printtreenl (stderr, 4, *nptr);
#endif
	return;
}/*}}}*/
#endif

/*{{{  PRIVATE void udv_resetexp (treenode *n, const char old, const char new)*/
/*
 *	called to change the state of something without checking.
 *	currently only used for extended input checking.
 */
PRIVATE void udv_resetexp (treenode *n, const char old, const char new)
{
	udv_t *tmp = NULL;
	int stop = 0;
	treenode *tptr = n;

	while (!stop) {
		switch (TagOf (tptr)) {
		case S_ARRAYSUB:
		case S_RECORDSUB:
			tptr = ASBaseOf (tptr);
			break;
		case N_DECL:
		case N_PARAM:
		case N_RESULTPARAM:
		case N_ABBR:
		case N_RETYPE:
			tmp = NUndefOf (tptr);
			if (!tmp) {
				return;
			}
#if 0
fprintf (stderr, "udv_resetexp: extracted state from tptr = %p, tmp = %p, tptr =", tptr, tmp);
printtreenl (stderr, 4, tptr);
#endif
			stop = 1;
			break;
		default:
			return;
		}
	}
	if (tmp && (tmp->state[udv_vstacklevel] == old)) {
		tmp->state[udv_vstacklevel] = new;
	}
	if (tmp && tmp->nested) {
		/* reset any nested state */
		int i;

		for (i=0; tmp->nested[i]; i++) {
			if (tmp->nested[i]->state[udv_vstacklevel] == old) {
				tmp->nested[i]->state[udv_vstacklevel] = new;
			}
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_undefinetargets (treenode *n, void *const voidptr)*/
/*
 *	called by prewalktree to make any variables (which are targets) undefined
 *	used by do_undefinedcheck_assign to propagate undefinedness/unknownness across assignments,
 *	and also in handling of UNDEFINED parameters (to mark locals)
 */
PRIVATEPARAM int do_undefinetargets (treenode *n, void *const voidptr)
{
	udv_t *tmp;
	udv_udef_t *undef = (udv_udef_t *)voidptr;
	char target_state;

	if (!n || !undef) {
		return CONTINUE_WALK;
	}
	if (undef->n_undefined) {
		target_state = UDV_UNDEFINED;
	} else if (undef->n_unknown) {
		target_state = UDV_UNKNOWN;
	} else {
		return CONTINUE_WALK;
	}
	switch (TagOf (n)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
	case N_RETYPE:
		tmp = NUndefOf (n);
		if (!tmp) {
			return STOP_WALK;
		}
		tmp->state[udv_vstacklevel] = target_state;
		if (tmp->nested) {
			/* set any target state */
			int i;

			for (i=0; tmp->nested[i]; i++) {
				tmp->nested[i]->state[udv_vstacklevel] = target_state;
			}
		}
		return STOP_WALK;
	case S_ARRAYSUB:
	case S_ARRAYITEM:
	case S_RECORDSUB:
	case S_RECORDITEM:
		prewalktree (ASBaseOf (n), do_undefinetargets, voidptr);
		return STOP_WALK;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_redefinetargets (treenode *n, void *const voidptr)*/
/*
 *	this is called to blindly change the state of variables -- used for mangling expressions that use DEFINED..!
 */
PRIVATEPARAM int do_redefinetargets (treenode *n, void *const voidptr)
{
	udv_t *tmp;
	char target_state = *(char *)voidptr;

	if (!n) {
		return CONTINUE_WALK;
	}
	switch (TagOf (n)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		tmp = NUndefOf (n);
		if (!tmp) {
			return STOP_WALK;
		}
		tmp->state[udv_vstacklevel] = target_state;
		if (tmp->nested) {
			/* set any target state */
			int i;

			for (i=0; tmp->nested[i]; i++) {
				tmp->nested[i]->state[udv_vstacklevel] = target_state;
			}
		}
		return STOP_WALK;
	case S_ARRAYSUB:
	case S_ARRAYITEM:
	case S_RECORDSUB:
	case S_RECORDITEM:
		prewalktree (ASBaseOf (n), do_redefinetargets, voidptr);
		return STOP_WALK;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATE void udv_fixup_defined (treenode **nptr)*/
PRIVATE void udv_fixup_defined (treenode **nptr)
{
	if (TagOf (*nptr) == S_UDVCONSTEXP) {
		switch (LoValOf (*nptr)) {
		case 0:
		case 1:
			SetTag (*nptr, S_CONSTEXP);
			break;
		default:
			*nptr = CExpOf (*nptr);
			break;
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_removeundefined (treenode **nptr, void *const voidptr)*/
PRIVATEPARAM int do_removeundefined (treenode **nptr, void *const voidptr)
{
	if (!nptr) {
		return STOP_WALK;
	}
	if (TagOf (*nptr) == S_UNDEFINED) {
		*nptr = OpOf (*nptr);
		return STOP_WALK;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_undefinedcheckexp (treenode **nptr, void *const voidptr)*/
/*
 *	called by prewalktree for undefinedness checks on expressions
 *	uses `udv_defaultmode' to determine whether it's input/output
 */
PRIVATEPARAM int do_undefinedcheckexp (treenode **nptr, void *const voidptr)
{
	udv_t *tmp;
	udv_udef_t *undef = (udv_udef_t *)voidptr;
	treenode *n = *nptr;
	static char varstr[128];
	int supress_warn = 0;

	if (undef && undef->supress_warn) {
		supress_warn = 1;
	}

#if 0
fprintf (stderr, "do_undefinedcheckexp (udv_defaultmode = %d, udv_vstacklevel = %d), n =", udv_defaultmode, udv_vstacklevel);
printtreenl (stderr, 4, n);
#endif
	switch (TagOf (n)) {
		/*{{{  names*/
	case N_DECL:
	case N_ABBR:
	case N_VALABBR:
	case N_RETYPE:
	case N_VALRETYPE:
	case N_PARAM:
	case N_RESULTPARAM:
		tmp = NUndefOf (n);
		if (!tmp) {
			/* global-type vars (which can appear if #included correctly) */
			return STOP_WALK;
		}
		if (udv_skip_channels && (TagOf (NTypeOf (n)) == S_CHAN)) {
			return CONTINUE_WALK;
		}
		switch (udv_defaultmode) {
			/*{{{  EXP_READ*/
		case EXP_READ:
			switch (tmp->state[udv_vstacklevel]) {
			case UDV_UNDEFINED:
			case UDV_UNKNOWN:
			case UDV_PARTIAL:
				if (!tmp->did_warn && !supress_warn) {
					int oldptr = udv_itraceptr;
					char tstate = tmp->state[udv_vstacklevel];

					if ((TagOf (n) != N_PARAM) && (TagOf (n) != N_RESULTPARAM) && (NLexLevelOf (n) >= syn_lexlevel)) {
						udv_itraceptr = 0;
					}
					udv_new_warning (((TagOf (n) == N_PARAM) || (TagOf (n) == N_RESULTPARAM)) ?
							((tstate == UDV_UNDEFINED) ? USE_PARAM_UNDEFINED : ((tstate == UDV_UNKNOWN) ? USE_PARAM_UNKNOWN : USE_PARAM_PARTIAL)) :
							((tstate == UDV_UNDEFINED) ? USE_VAR_UNDEFINED : ((tstate == UDV_UNKNOWN) ? USE_VAR_UNKNOWN : USE_VAR_PARTIAL)),
							udv_locn, WNameOf (NNameOf (n)));
					udv_itraceptr = oldptr;
					tmp->did_warn = SINGLE_WARNING;
				}
				if (undef) {
					switch (tmp->state[udv_vstacklevel]) {
					case UDV_UNDEFINED:
						undef->n_undefined++;
						break;
					case UDV_PARTIAL:
						undef->n_partial++;
						break;
					default:
						undef->n_unknown++;
						break;
					}
				}
				break;
			}
#ifdef MOBILES
			if (udv_ismobile && (TagOf (follow_user_type (NTypeOf (n))) == S_MOBILE)) {
				/* we should only get in here for direct assignment, etc., which leave the source (READ) undefined */
				/* updated: mobile barriers do not get undefined with this, neither do shared channel-type ends */
				if (udv_thingisdynmobilechantype (n)) {
					treenode *type = chk_gettype_main (n, TRUE);

					/* if shared, not undefined */
					if (!(NTypeAttrOf (type) & TypeAttr_shared)) {
						udv_set_state (tmp, udv_vstacklevel, UDV_UNDEFINED);
					}
				} else if (udv_thingisdynmobilebarriertype (n)) {
					/* always shared */
				} else {
					udv_set_state (tmp, udv_vstacklevel, UDV_UNDEFINED);
				}
#if 0
				tmp->state[udv_vstacklevel] = UDV_UNDEFINED;
				/* update any nested state */
				if (tmp->nested) {
					int i;

					for (i=0; tmp->nested[i]; i++) {
						tmp->nested[i]->state[udv_vstacklevel] = UDV_UNDEFINED;
					}
				}
#endif
			}
#endif
			break;
			/*}}}*/
			/*{{{  EXP_WRITTEN*/
		case EXP_WRITTEN:
#ifdef MOBILES
			if (udv_thingisdynmobilechantype (n)) {
				treenode *type = chk_gettype_main (n, TRUE);

				/* since we're writing to it, it had better not be claimed if shared */
				if ((TagOf (type) == S_ANYCHANTYPE) || (TagOf (type) == S_ANYPROCTYPE)) {
					/* don't do anything.. */
				} else if ((NTypeAttrOf (type) & TypeAttr_shared) && udv_isclaimed (n)) {
					udv_error_now ((TagOf (n) == N_PARAM) || (TagOf (n) == N_RESULTPARAM) ? USE_PARAM_CLAIMED_WRITE : USE_VAR_CLAIMED_WRITE, udv_locn, WNameOf (NNameOf (n)));
				}
			}
#endif
			if (undef) {
				/* record the state if this thing is undefined */
				switch (tmp->state[udv_vstacklevel]) {
				case UDV_UNDEFINED:
					undef->n_undefined++;
					break;
				case UDV_PARTIAL:
					undef->n_partial++;
					break;
				case UDV_UNKNOWN:
					undef->n_unknown++;
					break;
				}
#if 0
fprintf (stderr, "do_undefinedcheckexp: EXP_WRITTEN, undef was set to (%d,%d) --> defining variable and getting out\n", undef->n_undefined, undef->n_unknown);
#endif
			}
			udv_set_state (tmp, udv_vstacklevel, UDV_DEFINED);
			
#if 0
			tmp->state[udv_vstacklevel] = UDV_DEFINED;
			if (tmp->nested) {
				/* this automatically defines any nested state */
				int i;

				for (i=0; tmp->nested[i]; i++) {
					tmp->nested[i]->state[udv_vstacklevel] = UDV_DEFINED;
				}
			}
#endif
#if 0
fprintf (stderr, "do_undefinedcheckexp: EXP_WRITTEN, tmp =\n");
udv_dump_udvt (stderr, tmp, 0);
#endif
			tmp->did_warn = 0;
			break;
			/*}}}*/
			/*{{{  CHAN_INPUT/XINPUT/OUTPUT*/
		case CHAN_INPUT:
		case CHAN_XINPUT:
		case CHAN_OUTPUT:
			{
				const BOOL isinput = ((udv_defaultmode == CHAN_INPUT) || (udv_defaultmode == CHAN_XINPUT)) ? TRUE : FALSE;
				const BOOL isextended = (udv_defaultmode == CHAN_XINPUT);
				BOOL docheck;

				/* don't complain about channel array SEQ I/O */
				if (TagOf (NTypeOf (n)) == S_ARRAY) {
					treenode *basetype = ARTypeOf (NTypeOf (n));

					while ((TagOf (basetype) == S_MOBILE) || (TagOf (basetype) == S_ARRAY)) {
						basetype = (TagOf (basetype) == S_MOBILE) ? MTypeOf (basetype) : ARTypeOf (basetype);
					}
					if (TagOf (basetype) == S_CHAN) {
						docheck = FALSE;
					} else {
						docheck = TRUE;
					}
#ifdef MOBILES
				} else if (NTypeAttrOf (n)) {
					/* don't mark as outputted or inputted. */
					docheck = FALSE;
#endif
				} else {
					docheck = TRUE;
				}
				if (docheck && (tmp->state[udv_vstacklevel] == UDV_XINPUTTED)) {
					/* doing something on a channel during extended portion */
					msg_out_s (SEV_ERR, USE, USE_CHAN_IN_XIN, udv_locn, WNameOf (NNameOf (n)));
				} else if (docheck && (tmp->state[udv_vstacklevel] == (isinput ? UDV_OUTPUTTED : UDV_INPUTTED)) && !tmp->did_warn) {
					/* inputting on a channel which has been used for output, or visa-versa */
					int oldptr = udv_itraceptr;

					udv_itraceptr = 0;
					udv_new_warning (USE_CHAN_IO_SEQ, udv_locn, WNameOf (NNameOf (n)));
					udv_itraceptr = oldptr;
					tmp->did_warn = SINGLE_WARNING;
				}
				tmp->state[udv_vstacklevel] = (isinput ? (isextended ? UDV_XINPUTTED : UDV_INPUTTED) : UDV_OUTPUTTED);
			}
			break;
			/*}}}*/
		}
		break;
		/*}}}*/
		/*{{{  SIZE, ADDRESSOF */
	case S_SIZE:
	case S_ADDRESSOF:
#if 0
fprintf (stderr, "do_undefinedcheckexp: S_SIZE in undefined checker.\n");
#endif
		/* not an error at the moment, I don't think.. */
		return STOP_WALK;
		/*}}}*/
#ifdef MOBILES
		/*{{{  UNDEFINED*/
	case S_UNDEFINED:
		modprewalktree (OpAddr (n), do_undefinedcheckexp, voidptr);
		return STOP_WALK;
		/*}}}*/
		/*{{{  CLONE*/
	case S_CLONE:
		/* RHS of this must be left untouched */
		{
			const BOOL prevstate = udv_ismobile;

			udv_ismobile = FALSE;
			modprewalktree (OpAddr (n), do_undefinedcheckexp, voidptr);
			udv_ismobile = prevstate;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  DEFINED*/
	case S_DEFINED:
		/* this is constructive -- always defines (since we only check after pushing vstack in IF/ALT handling) */
		{
			const int oldmode = udv_defaultmode;
			udv_udef_t local_undefined = {0, 0, 0, 1};
			int astate = UDV_DEFINED;

			if (!udv_thingisdynmobile (OpOf (n)) && !udv_thingisdynmobilechantype (OpOf (n))) {
				udv_error_now (USE_BAD_MOBILE_DEFINED, LocnOf (n), "");
				return STOP_WALK;
			}
			/* check first, with local and warnings suppressed, to see if we can tell either way */
			udv_defaultmode = EXP_READ;
			modprewalktree (OpAddr (n), do_undefinedcheckexp, &local_undefined);
			if (local_undefined.n_undefined) {
				astate = UDV_UNDEFINED;
			} else if (local_undefined.n_partial) {
				astate = UDV_PARTIAL;
			} else if (local_undefined.n_unknown) {
				astate = UDV_UNKNOWN;
			} /* else defined */
#if 0
fprintf (stderr, "do_undefinedcheckexp: checked for READ with suppressed warings: undefined = %d, unknown = %d\n", local_undefined.n_undefined, local_undefined.n_unknown);
#endif

			/* then walk anyway, generates warnings about other bits of undefinedness -- ie, "clients[i]", where "i" is undefined/unknown */
			udv_defaultmode = EXP_WRITTEN;
			modprewalktree (OpAddr (n), do_undefinedcheckexp, NULL);
			udv_defaultmode = oldmode;

			/* rightho -- reduce to constant if absolutely defined or undefined */
#if 0
			if (astate == UDV_UNDEFINED) {
				*nptr = newconstexpnode (S_UDVCONSTEXP, udv_locn, n, 0, 0);
			} else if (astate == UDV_DEFINED) {
				*nptr = newconstexpnode (S_UDVCONSTEXP, udv_locn, n, 0, 1);
			} else {
				*nptr = newconstexpnode (S_UDVCONSTEXP, udv_locn, n, 0, -1);
			}
#endif
			*nptr = newconstexpnode (S_UDVCONSTEXP, udv_locn, n, 0, -1);
		}
		return STOP_WALK;
		/*}}}*/
#endif	/* MOBILES */
		/*{{{  DECL*/
	case S_DECL:
		{
			/* we run into declarations in front of VALOF nodes, collect up decls and expect a VALOF/RESULT at the end */
			treenode *revdecllist = NULL;		/* used to temporarily store declarations (in reverse order) */

			undefinedcheck_set_decl (DNameOf (n), &revdecllist);
			/* walk body -- using do_undefinedcheckexp: capture more specs */
			modprewalktree (DBodyAddr (n), do_undefinedcheckexp, voidptr);
			undefinedcheck_end_decl (revdecllist);

		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  ABBR/VALABBR/RETYPE/VALRETYPE*/
	case S_ABBR:
	case S_VALABBR:
	case S_RETYPE:
	case S_VALRETYPE:
		{
			/* can run into these in front of VALOF nodes, collect up and process body */
			udv_t *other;

			other = NULL;
			undefinedcheck_set_spec (n, NULL, &other);
			modprewalktree (DBodyAddr (n), do_undefinedcheckexp, voidptr);
			undefinedcheck_end_spec (n, other);
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  VALOF*/
	case S_VALOF:
		{
			/* have any leading specifications in scope, check body then ensure DEFINED states on anything that is a RESULT */
			int oldmode = udv_defaultmode;
			treenode *rlist;

			prewalkproctree (VLBodyOf (n), do_undefinedcheck, NULL);
			for (rlist = VLResultListOf (n); !EmptyList (rlist); rlist = NextItem (rlist)) {
				udv_defaultmode = EXP_READ;
				udv_locn = LocnOf (n);
				modprewalktree (ThisItemAddr (rlist), do_undefinedcheckexp, voidptr);
			}
			udv_defaultmode = oldmode;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  NOT*/
	case S_NOT:
		{
			treenode *op;

			/* walk operand */
			modprewalktree (OpAddr (n), do_undefinedcheckexp, voidptr);
			op = OpOf (n);

#if 0
fprintf (stderr, "do_undefinedcheckexp: S_NOT: walked sub-expression, op =");
printtreenl (stderr, 4, op);
#endif
			if (TagOf (op) == S_UDVCONSTEXP) {
				/* move the constant expression upwards (NOT->CONSTEXP(x,y)) into (CONSTEXP(!x,NOT->y)) */
				treenode *tmp = CExpOf (op);

				SetCExp (op, *nptr);
				SetOp (n, tmp);
				*nptr = op;
				/* loval refers to the defined state for the sub-expression */
				switch (LoValOf (op)) {
				case 0:
				case 1:
					{
						const int loval = LoValOf (op);
						int target_state = (loval == 0) ? UDV_DEFINED : UDV_UNDEFINED;

						/* better undefine/redefine targets */
						prewalktree (OpOf (tmp), do_redefinetargets, &target_state);
						SetLoVal (op, (loval == 0) ? 1 : 0);
						break;
					}
				default:
					break;
				}
			}
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  RECORDSUB/ARRAYSUB/SEGMENT*/
	case S_SEGMENT:
	case S_RECORDSUB:
	case S_ARRAYSUB:
		{
			udv_t *baseudv, *fieldudv;
			treenode *base, *field;
			int max_nesting = 32;			/* the compiler puts a limit on this elsewhere too iirc */
			int cur_nesting = 0;
			int i;
			treenode *nodestack[32];
			udv_t *udvstack[32];
			udv_t *tempudv = NULL;

			/* check record or array subscription or segment */

			/*{{{  trace backwards until we find something that isn't a RECORDSUB, ARRAYSUB or SEGMENT*/
			base = n;
			while ((TagOf (base) == S_RECORDSUB) || (TagOf (base) == S_ARRAYSUB) || (TagOf (base) == S_SEGMENT)) {
				nodestack[cur_nesting] = base;
				udvstack[cur_nesting] = NULL;
				cur_nesting++;
				if (cur_nesting == max_nesting) {
					msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, udv_locn,
							"do_undefinedcheckexp -- max nested in RECORDSUB/ARRAYSUB/SEGMENT exceeded");
				}
				if (TagOf (base) == S_SEGMENT) {
					base = SNameOf (base);
				} else {
					base = ASBaseOf (base);
				}
			}
			/*}}}*/
			/*{{{  then forwards picking out the fields*/
			nodestack[cur_nesting] = base;
			if (TagOf (base) == S_FINSTANCE) {
				/* check function parameters and return -- must always be a read */
				prewalkproctree (base, do_undefinedcheck, voidptr);
				return STOP_WALK;
			}
			switch (nodetypeoftag (TagOf (base))) {
			case NAMENODE:
				udvstack[cur_nesting] = NUndefOf (base);
				break;
			case LITNODE:
			case CONSTTABLENODE:
				/* should be a CONSTRUCTOR or CONSTCONSTRUCTOR, fabricate udv entry for it */
				tempudv = udv_makeudv (base, tempudv);
				udvstack[cur_nesting] = tempudv;
				break;
			default:
				msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, udv_locn,
						"do_undefinedcheckexp -- RECORDSUB/ARRAYSUB base not namenode");
				break;
			}

			if (!udvstack[cur_nesting]) {
#if 0
fprintf (stderr, "do_undefinedcheckexp(): missing NUndefOf, base = ");
printtreenl (stderr, 4, base);
#endif
				msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, udv_locn, "do_undefinedcheckexp -- missing stack entry");
			}
			for (i=cur_nesting - 1; i>=0; i--) {
				/*{{{  find subscript field of nodestack[i]*/
				int j;

				/* find subscript field of nodestack[i], will be in the nested structure of udvstack[i+1] (or just itself, in some cases) */
				if (!udvstack[i+1]->nested) {
					msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, udv_locn, "do_undefinedcheckexp -- missing structure (1)");
				}
				if (TagOf (nodestack[i]) == S_RECORDSUB) {
					for (j=0; udvstack[i+1]->nested[j] && (udvstack[i+1]->nested[j]->nameof != ASIndexOf (nodestack[i])); j++);
					if (!udvstack[i+1]->nested || !udvstack[i+1]->nested[j]) {
						msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, udv_locn, "do_undefinedcheckexp -- missing structure (2)");
					}
					udvstack[i] = udvstack[i+1]->nested[j];
				} else if (TagOf (nodestack[i]) == S_ARRAYSUB) {
					/* FIXME: bits of arrays */
					if (!udvstack[i+1]->nested || !udvstack[i+1]->nested[0]) {
						msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, udv_locn, "do_undefinedcheckexp -- missing structure (3)");
					}
					udvstack[i] = udvstack[i+1]->nested[0];
				} else if (TagOf (nodestack[i]) == S_SEGMENT) {
					/* segments are effectively the array (for the time being) */
					udvstack[i] = udvstack[i+1];
				} else {
					msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, udv_locn, "do_undefinedcheckexp -- unknown structure");
				}
				/*}}}*/
			}
			fieldudv = udvstack[0];
			field = nodestack[0];
			/*}}}*/

			/*{{{  walk "up" the tree path checking definedness state */
			for (i=cur_nesting; i>0; i--) {
				baseudv = udvstack[i];
				base = nodestack[i];
				
#ifdef MOBILES
				if (udv_thingisdynmobilechantype (base)) {
					/*{{{  MOBILE channel-type subscription*/
					treenode *type;
					int oldmode = udv_defaultmode;

					type = chk_gettype_main (base, TRUE);
					if (NTypeAttrOf (type) & TypeAttr_shared) {
						/* better have CLAIMed this thing, or the thing above it (actually below, cos we're backwards here) */
						if (!udv_isclaimed (base) && !((i < cur_nesting) && udv_isclaimed (nodestack[i+1]))) {
#if 0
fprintf (stderr, "use4: do_undefinedcheckexp(): i=%d, base=", i);
printtreenl (stderr, 4, base);
fprintf (stderr, "use4: nodestack[i+1]=");
printtreenl (stderr, 4, nodestack[i+1]);
#endif
							udv_getclaimname (varstr, 127, base);
							udv_error_now (USE_NOT_CLAIMED, udv_locn, varstr);
						}
					}

					if ((udv_defaultmode == CHAN_INPUT) || (udv_defaultmode == CHAN_OUTPUT) || (udv_defaultmode == CHAN_XINPUT)) {
						/* check we can read the base */
						udv_defaultmode = EXP_READ;
						modprewalktree (&base, do_undefinedcheckexp, NULL);

						udv_defaultmode = oldmode;
					}
					/*}}}*/
				}
				/* if this is a dynamic mobile, it had better be defined */
				if (udv_thingisdynmobile (base)) {
					if ((baseudv->state[udv_vstacklevel] != UDV_DEFINED) && !baseudv->did_warn && !supress_warn) {
						/* generate warning */
						udv_strofexp (varstr, 127, base);
						switch (baseudv->state[udv_vstacklevel]) {
						case UDV_UNDEFINED:
							udv_new_warning ((((TagOf (base) == N_PARAM) || (TagOf (base) == N_RESULTPARAM)) ? USE_PARAM_UNDEFINED : USE_VAR_UNDEFINED),
									udv_locn, varstr);
							if (undef) {
								undef->n_undefined++;
							}
							break;
						case UDV_UNKNOWN:
							udv_new_warning ((((TagOf (base) == N_PARAM) || (TagOf (base) == N_RESULTPARAM)) ? USE_PARAM_UNKNOWN : USE_VAR_UNKNOWN),
									udv_locn, varstr);
							if (undef) {
								undef->n_unknown++;
							}
							break;
						case UDV_PARTIAL:
							udv_new_warning ((((TagOf (base) == N_PARAM) || (TagOf (base) == N_RESULTPARAM)) ? USE_PARAM_PARTIAL : USE_VAR_PARTIAL),
									udv_locn, varstr);
							if (undef) {
								undef->n_partial++;
							}
							break;
						}
						baseudv->did_warn = SINGLE_WARNING;
					}
				}
#endif
				if (TagOf (base) == S_SEGMENT) {
					/* make sure that the base and length are readable */
					int oldmode = udv_defaultmode;

					udv_defaultmode = EXP_READ;
					modprewalktree (SStartExpAddr (base), do_undefinedcheckexp, voidptr);
					modprewalktree (SLengthExpAddr (base), do_undefinedcheckexp, voidptr);

					udv_defaultmode = oldmode;
				}
			}
			/*}}}*/
			/*{{{  check the top-level */
			baseudv = udvstack[cur_nesting];
			base = nodestack[cur_nesting];

			switch (udv_defaultmode) {
				/*{{{  EXP_READ*/
			case EXP_READ:
				switch (fieldudv->state[udv_vstacklevel]) {
				case UDV_UNDEFINED:
				case UDV_UNKNOWN:
				case UDV_PARTIAL:
					if (!fieldudv->did_warn && !supress_warn) {
						/* generate a warning */
#if 0
fprintf (stderr, "here: field = ");
printtreenl (stderr, 4, field);
#endif
						udv_strofexp (varstr, 127, field);
						switch (fieldudv->state[udv_vstacklevel]) {
						case UDV_UNDEFINED:
							udv_new_warning ((((TagOf (base) == N_PARAM) || (TagOf (base) == N_RESULTPARAM)) ? USE_PARAM_UNDEFINED : USE_VAR_UNDEFINED),
									udv_locn, varstr);
							if (undef) {
								undef->n_undefined++;
							}
							break;
						case UDV_UNKNOWN:
							udv_new_warning ((((TagOf (base) == N_PARAM) || (TagOf (base) == N_RESULTPARAM)) ? USE_PARAM_UNKNOWN : USE_VAR_UNKNOWN),
									udv_locn, varstr);
							if (undef) {
								undef->n_unknown++;
							}
							break;
						case UDV_PARTIAL:
							udv_new_warning ((((TagOf (base) == N_PARAM) || (TagOf (base) == N_RESULTPARAM)) ? USE_PARAM_PARTIAL : USE_VAR_PARTIAL),
									udv_locn, varstr);
							if (undef) {
								undef->n_partial++;
							}
							break;
						}
						fieldudv->did_warn = SINGLE_WARNING;
					}
					break;
				}
				break;
				/*}}}*/
				/*{{{  EXP_WRITTEN*/
			case EXP_WRITTEN:
				if (undef) {
					/* record state if this field is undefined */
					switch (fieldudv->state[udv_vstacklevel]) {
					case UDV_UNDEFINED:
						undef->n_undefined++;
						break;
					case UDV_UNKNOWN:
						undef->n_unknown++;
						break;
					case UDV_PARTIAL:
						undef->n_partial++;
						break;
					}
				}
				/* this will update the target state */
				udv_set_state (fieldudv, udv_vstacklevel, UDV_DEFINED);
				/* then update parent fields */
				for (i=1; i<=cur_nesting; i++) {
					udv_t *tmpudv = udvstack[i];
					int n_undef, n_partial, n_unknown, n_defined;
					int j;

					n_undef = 0;
					n_partial = 0;
					n_unknown = 0;
					n_defined = 0;

					/* gather "inner" state */
					for (j=0; tmpudv->nested[j]; j++) {
						switch (tmpudv->nested[j]->state[udv_vstacklevel]) {
						case UDV_UNDEFINED:
							n_undef++;
							break;
						case UDV_PARTIAL:
							n_partial++;
							break;
						case UDV_DEFINED:
							n_defined++;
							break;
						case UDV_UNKNOWN:
						default:
							n_unknown++;
							break;
						}
					}

#if 0
fprintf (stderr, "use4: udv_doundefinedcheckexp(): subscript fixup, n_undef = %d, n_partial = %d, n_defined = %d, n_unknown = %d.  nodestack[i] =", n_undef, n_partial, n_defined, n_unknown);
printtreenl (stderr, 4, nodestack[i]);
#endif
					/* we are the parent state.. */
					if (TagOf (nodestack[i-1]) == S_RECORDSUB) {
						/* update state of tmpudv; "unknown" is most binding, then "partial", then "defined"/"undefined" */
						if (n_unknown) {
							tmpudv->state[udv_vstacklevel] = UDV_UNKNOWN;
						} else if (n_partial) {
							tmpudv->state[udv_vstacklevel] = UDV_PARTIAL;
						} else if (!n_undef) {
							/* nothing undefined */
							tmpudv->state[udv_vstacklevel] = UDV_DEFINED;
#if 0
fprintf (stderr, "use4: udv_doundefinedcheckexp(): subscript fixup, setting state at current depth to DEFINED\n");
#endif
						} else if (!n_defined) {
							/* nothing defined.. -- ought not to happen given that we set one to DEFINED above */
							tmpudv->state[udv_vstacklevel] = UDV_UNDEFINED;
						} else {
							/* introducing partial definedness */
							tmpudv->state[udv_vstacklevel] = UDV_PARTIAL;
						}
					} else if (TagOf (nodestack[i-1]) == S_ARRAYSUB) {
						/* update state fairly loosely */
						switch (tmpudv->state[udv_vstacklevel]) {
						case UDV_DEFINED:
							break;		/* won't adjust this */
						case UDV_UNDEFINED:
						case UDV_UNKNOWN:
						case UDV_PARTIAL:
							if (n_unknown) {
								tmpudv->state[udv_vstacklevel] = UDV_UNKNOWN;
							} else if (n_partial) {
								tmpudv->state[udv_vstacklevel] = UDV_PARTIAL;
							} else if (!n_undef) {
								tmpudv->state[udv_vstacklevel] = UDV_DEFINED;
							} else if (n_defined) {
								tmpudv->state[udv_vstacklevel] = UDV_PARTIAL;
							}
							break;
						}
					} else {
						/* segment -- SKIP */
					}
				}
				break;
				/*}}}*/
				/*{{{  CHAN_INPUT, CHAN_XINPUT, CHAN_OUTPUT*/
			case CHAN_INPUT:
			case CHAN_OUTPUT:
			case CHAN_XINPUT:
				if (fieldudv->state[udv_vstacklevel] == UDV_XINPUTTED) {
					/* doing something to a channel during extended input */
					udv_strofexp (varstr, 127, field);
					msg_out_s (SEV_ERR, USE, USE_CHAN_IN_XIN, udv_locn, varstr);
				}
				fieldudv->state[udv_vstacklevel] = ((udv_defaultmode == CHAN_INPUT) ? UDV_INPUTTED : ((udv_defaultmode == CHAN_XINPUT) ? UDV_XINPUTTED : UDV_OUTPUTTED));
				break;
				/*}}}*/
			}
			/*}}}*/

			if (tempudv) {
				udv_freeudv (tempudv);
			}
		}
		return STOP_WALK;
		/*}}}*/
	}
#if 0
fprintf (stderr, "do_undefinedcheckexp [returning CONTINUE_WALK] (udv_defaultmode = %d, udv_vstacklevel = %d), n =", udv_defaultmode, udv_vstacklevel);
printtreenl (stderr, 4, n);
#endif
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATE udv_t *do_getudvof (treenode *nptr)*/
/*
 *	returns the key udv_t thing for an l-value, or NULL
 *	might return a nested state
 */
PRIVATE udv_t *do_getudvof (treenode *nptr)
{
	if (!nptr) {
		return NULL;
	}
#if 0
fprintf (stderr, "do_getudvof: incoming nptr = ");
printtreenl (stderr, 4, nptr);
#endif
	switch (TagOf (nptr)) {
	default:
		return NULL;

	case N_DECL:
	case N_ABBR:
	case N_RETYPE:
	case N_PARAM:
	/* case N_VALPARAM: */
	case N_RESULTPARAM:
		return NUndefOf (nptr);

	case S_ARRAYSUB:
	case S_RECORDSUB:
		{
			udv_t *this = do_getudvof (ASBaseOf (nptr));

			if (!this) {
				return NULL;
			}
			switch (TagOf (nptr)) {
			case S_RECORDSUB:
				if (!this->nested) {
					msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (nptr), "do_getudvof(): missing sub-state (1)");
					return this;
				} else {
					int i;

					/* find the nested state */
					for (i=0; this->nested[i] && (this->nested[i]->nameof != ASIndexOf (nptr)); i++);
					if (!this->nested[i]) {
						msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (nptr), "do_getudvof(): missing sub-state (2)");
						return this;		/* no state ? */
					}
					return this->nested[i];
				}
			case S_ARRAYSUB:
				/* FIXME: definedness handling for arrays -- properly */
				if (!this->nested) {
					msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (nptr), "do_getudvof(): missing sub-state (3)");
				}
				return this->nested[0];
			default:
				return this;
			}
		}
		break;
	}
	return NULL;
}
/*}}}*/
/*{{{  PRIVATE void do_undefinedcheck_assign (treenode **lhsptr, treenode **rhsptr)*/
/*
 *	checks undefinedness in a single assign
 */
PRIVATE void do_undefinedcheck_assign (treenode **lhsptr, treenode **rhsptr)
{
	udv_udef_t undefinedcount;
	treenode *lhs = *lhsptr;
	treenode *rhs = *rhsptr;

	undefinedcount.n_undefined = 0;
	undefinedcount.n_unknown = 0;
	undefinedcount.n_partial = 0;
	undefinedcount.supress_warn = 0;
#ifdef MOBILES
	if (udv_thingismobile (lhs)) {
		/* check RHS */
		if (udv_thingisdynmobile (lhs) && (TagOf (rhs) == S_NEW_ARRAY)) {
			/* special handling for NEW_ARRAY -- only defines outermost name */
			udv_t *leftudv = do_getudvof (lhs);

#if 0
fprintf (stderr, "do_undefinedcheck_assign: lhs =");
printtreenl (stderr, 8, lhs);
fprintf (stderr, "    rhs =");
printtreenl (stderr, 8, rhs);
#endif
			leftudv->state[udv_vstacklevel] = UDV_DEFINED;
		} else {
			if (udv_thingismobile (rhs)) {
				udv_ismobile = TRUE;
				udv_defaultmode = EXP_READ;
				modprewalktree (rhsptr, do_undefinedcheckexp, &undefinedcount);
				udv_ismobile = FALSE;
			} else {
				udv_defaultmode = EXP_READ;
				modprewalktree (rhsptr, do_undefinedcheckexp, &undefinedcount);
			}
			udv_defaultmode = EXP_WRITTEN;
			if (udv_thingisdynmobile (lhs) || udv_thingisdynmobilechantype (lhs) || udv_thingisdynmobileproctype (lhs)) {
				udv_dynmobile_targetcheck (lhsptr);
			} else {
				modprewalktree (lhsptr, do_undefinedcheckexp, NULL);
			}
			/* modprewalktree (lhsptr, do_undefinedcheckexp, NULL); */
			/* if the RHS is undefined, make sure we leave the LHS undefined */
			if (have_undefined (&undefinedcount)) {
				prewalktree (lhs, do_undefinetargets, &undefinedcount);
			}
		}
	} else
#endif
	{
		udv_defaultmode = EXP_READ;
		modprewalktree (rhsptr, do_undefinedcheckexp, &undefinedcount);
		udv_defaultmode = EXP_WRITTEN;
		modprewalktree (lhsptr, do_undefinedcheckexp, NULL);
#if 0
fprintf (stderr, "*** scopein, before do_undefinedcheck_assign:\n");
udv_dump_stack_bit (stderr, udv_vstacklevel, udv_vstacklevel, TRUE);
#endif
#if 0
fprintf (stderr, "do_undefinedcheck_assign: have_undefined(&undefinedcount) = %d\n", have_undefined (&undefinedcount));
#endif
		if (have_undefined (&undefinedcount)) {
			prewalktree (lhs, do_undefinetargets, &undefinedcount);
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void init_decl_subtype (treenode *type, udv_t *udv, char defstate)*/
/*
 *	sets up any needed sub-type undefined nodes (records for now)
 */
PRIVATE void init_decl_subtype (treenode *type, udv_t *udv, char defstate)
{
	int ismobile = 0;

#if 0
fprintf (stderr, "use4.c: init_decl_subtype(): type =");
printtreenl (stderr, 4, type);
#endif
	if (TagOf (type) == S_MOBILE) {
		type = MTypeOf (type);
		ismobile = 1;
		if (TagOf (type) == N_TYPEDECL) {
			type = NTypeOf (type);
		}
	} else {
		if (TagOf (type) == N_TYPEDECL) {
			type = NTypeOf (type);
		}
		if (TagOf (type) == S_MOBILE) {
			type = MTypeOf (type);
			ismobile = 1;
		}
	}

	if (TagOf (type) == S_ARRAY) {
		/* create single-level nested state for array */
		if (ismobile) {
			udv->type = UDVT_MOB_DA;
		} else {
			udv->type = UDVT_VAR;
		}
		udv->nested = (udv_t **)newvec (2 * sizeof (udv_t *));
		udv->nested[0] = newudv ();
		udv->nested[0]->next = NULL;
		udv->nested[0]->nameof = NULL;
		udv->nested[0]->state[udv_vstacklevel] = defstate;
		udv->nested[0]->did_warn = 0;
		udv->nested[0]->nested = NULL;
		udv->nested[1] = NULL;			/* so we can see the end */
		init_decl_subtype (ARTypeOf (type), udv->nested[0], defstate);
	} else if (TagOf (type) == S_RECORD) {
		int n_fields;
		treenode *typedecl;
		treenode *firstdecl;
		int have_chan = 0;

		typedecl = ARTypeOf (type);
		firstdecl = typedecl;			/* should be pointing at a DECL node */
		if (TagOf (firstdecl) != S_DECL) {
			return;
		}
#if 0
fprintf (stderr, "init_decl_subtype: name of TYPEDECL entering scope.  firstdecl is: ");
/* printtreenl (stderr, 4, nptr); */
printtreenl (stderr, 4, firstdecl);
#endif
		n_fields = 0;
		while (typedecl && (TagOf (typedecl) == S_DECL)) {
			if (TagOf (DNameOf (typedecl)) == S_LIST) {
				treenode *list = DNameOf (typedecl);

				while (!EndOfList (list)) {
					n_fields++;
					list = NextItem (list);
				}
			} else {
				n_fields++;
			}
			typedecl = DBodyOf (typedecl);
		}
#if 0
fprintf (stderr, "init_decl_subtype: %d fields in N_TYPEDECL, defstate = %d\n", n_fields, defstate);
#endif
		if (n_fields > 0) {
			/* setup udv->nested */
			udv->nested = (udv_t **)newvec ((n_fields + 1) * sizeof (udv_t *));
			n_fields = 0;
			typedecl = firstdecl;
			while (typedecl && (TagOf (typedecl) == S_DECL)) {
				if (TagOf (DNameOf (typedecl)) == S_LIST) {
					treenode *list = DNameOf (typedecl);

					while (!EndOfList (list)) {
						treenode *dname = ThisItem (list);

						udv->nested[n_fields] = newudv ();
						udv->nested[n_fields]->next = NULL;
						udv->nested[n_fields]->nameof = dname;
						udv->nested[n_fields]->state[udv_vstacklevel] = defstate;
						udv->nested[n_fields]->did_warn = 0;
						udv->nested[n_fields]->nested = NULL;
						/* subtypes ? */
						init_decl_subtype (NTypeOf (dname), udv->nested[n_fields], defstate);
						if (TagOf (NTypeOf (dname)) == S_CHAN) {
							have_chan = 1;
							udv->nested[n_fields]->type = UDVT_CHAN;
						} else {
							udv->nested[n_fields]->type = UDVT_INVALID;
						}
						n_fields++;
						list = NextItem (list);
					}
				} else {
					treenode *dname = DNameOf (typedecl);

					udv->nested[n_fields] = newudv ();
					udv->nested[n_fields]->next = NULL;
					udv->nested[n_fields]->nameof = dname;
					udv->nested[n_fields]->state[udv_vstacklevel] = defstate;
					udv->nested[n_fields]->did_warn = 0;
					udv->nested[n_fields]->nested = NULL;
					/* subtypes ? */
					init_decl_subtype (NTypeOf (dname), udv->nested[n_fields], defstate);
					if (TagOf (NTypeOf (dname)) == S_CHAN) {
						have_chan = 1;
						udv->nested[n_fields]->type = UDVT_CHAN;
					} else {
						udv->nested[n_fields]->type = UDVT_INVALID;
					}
					n_fields++;
				}
				typedecl = DBodyOf (typedecl);
			}
			udv->nested[n_fields] = NULL;		/* mark end of list */
			if (!have_chan || !ismobile) {
				udv->type = UDVT_VAR;
			} else {
				udv->type = UDVT_MOB_CT;
			}
		}
	} else {
		udv->nested = NULL;
		if (TagOf (type) == S_CHAN) {
			udv->type = UDVT_CHAN;
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void free_decl_subtype (treenode *nptr, udv_t *udv)*/
/*
 *	frees any sub-type undefined nodes (records for now)
 */
PRIVATE void free_decl_subtype (treenode *nptr, udv_t *udv)
{
	if (udv->nested) {
		int n_fields = 0;

		while (udv->nested[n_fields] != NULL) {
			/* free any subtype info */
			free_decl_subtype (udv->nested[n_fields]->nameof, udv->nested[n_fields]);
			freeudv (udv->nested[n_fields]);
			udv->nested[n_fields] = NULL;
			n_fields++;
		}
		freevec (udv->nested, (n_fields + 1) * sizeof (udv_t *));
		udv->nested = NULL;
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE udv_t *udv_makeudv (treenode *tptr, udv_t *next)*/
/*
 *	creates a udv_t tree for some constant type
 */
PRIVATE udv_t *udv_makeudv (treenode *tptr, udv_t *next)
{
	treenode *type;
	udv_t *udv;

	switch (TagOf (tptr)) {
	case S_CONSTRUCTOR:
	case S_CONSTCONSTRUCTOR:
	case S_STRING:
		type = chk_gettype (tptr);
		break;
	default:
#if 0
fprintf (stderr, "use4: udv_makeudv: unknown tptr = ");
printtreenl (stderr, 4, tptr);
#endif
		msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, udv_locn, "udv_makeudv -- unknown type");
		return NULL;
	}
	udv = newudv ();
	udv->next = NULL;
	udv->nameof = (treenode *)lookupword ("constructor", 11);
	udv->state[udv_vstacklevel] = UDV_DEFINED;	/* for now */
	udv->did_warn = 0;
	udv->nested = NULL;

	init_decl_subtype (type, udv, UDV_DEFINED);
#if 0
fprintf (stderr, "use4: udv_makeudv: type = ");
printtreenl (stderr, 4, chk_gettype(tptr));
#endif
	return udv;
}
/*}}}*/
/*{{{  PRIVATE void udv_freeudv (udv_t *udv)*/
/*
 *	frees a udv_t tree created by udv_makeudv()
 */
PRIVATE void udv_freeudv (udv_t *udv)
{
	while (udv) {
		udv_t *next = udv->next;

		free_decl_subtype (udv->nameof, udv);
		freeudv (udv);
		udv = next;
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void do_undefinedcheck_asm (treenode *tptr)*/
/*
 *	does undefined-checking for ASM blocks
 */
PRIVATE void do_undefinedcheck_asm (treenode *tptr)
{
	treenode *tmp;
	const int old_skip_channels = udv_skip_channels;

	if (!tptr) {
		return;
	}
	udv_skip_channels = 1;		/* if people want to do funny channel stuff in ASM, their business ;) */
	if (TagOf (tptr) == S_LIST) {
		tmp = tptr;
		while (!EndOfList (tmp)) {
			treenode *thisone = ThisItem (tmp);

			if ((TagOf (thisone) == S_GUYCODE) || (TagOf (thisone) == S_GUYSTEP)) {
				wordnode *op = (wordnode *)LeftOpOf (thisone);

				if ((op == op_ld) || (op == op_ldab) || (op == op_ldabc) || (op == op_ldl)) {
					/* if we're loading an address of something, assume it's written */
					treenode **rhs = RightOpAddr (thisone);

					udv_locn = LocnOf (thisone);
					if ((TagOf (*rhs) == S_LIST) && (TagOf (ThisItem (*rhs)) == S_ADDRESSOF)) {
						udv_defaultmode = EXP_WRITTEN;
						rhs = OpAddr (ThisItem (*rhs));
					} else {
						udv_defaultmode = EXP_READ;
					}
					modprewalktree (rhs, do_undefinedcheckexp, NULL);
				} else if ((op == op_st) || (op == op_stab) || (op == op_stabc) || (op == op_stl)) {
					udv_locn = LocnOf (thisone);
					udv_defaultmode = EXP_WRITTEN;
					modprewalktree (RightOpAddr (thisone), do_undefinedcheckexp, NULL);
				}
			}
			tmp = NextItem (tmp);
		}
	}
	udv_skip_channels = old_skip_channels;
	return;
}
/*}}}*/
/*{{{  PRIVATE void undefinedcheck_set_decl (treenode *namelist, treenode **revdecllist)*/
/*
 *	adds variables to the undefinedness stack
 */
PRIVATE void undefinedcheck_set_decl (treenode *namelist, treenode **revdecllist)
{
	udv_t *tmp;

#if 0
fprintf (stderr, "use4: undefinedcheck_set_decl: setting up for namelist = ");
printtreenl (stderr, 4, namelist);
#endif
	if (TagOf (namelist) == S_LIST) {
		while (!EmptyList (namelist)) {
			tmp = newudv ();
			tmp->state[udv_vstacklevel] = UDV_UNDEFINED;
			tmp->did_warn = 0;
			SetNUndef (ThisItem (namelist), tmp);
			tmp->nameof = ThisItem (namelist);
			tmp->next = udv_vstack;
			udv_vstack = tmp;
			init_decl_subtype (NTypeOf (ThisItem (namelist)), tmp, UDV_UNDEFINED);

			if (revdecllist) {
				*revdecllist = newlistnode (S_LIST, NOPOSN, ThisItem (namelist), *revdecllist);
			}

			namelist = NextItem (namelist);
		}
	} else {
		tmp = newudv ();
		tmp->state[udv_vstacklevel] = UDV_UNDEFINED;
		tmp->did_warn = 0;
		SetNUndef (namelist, tmp);
		tmp->nameof = namelist;
		tmp->next = udv_vstack;
		udv_vstack = tmp;
		init_decl_subtype (NTypeOf (namelist), tmp, UDV_UNDEFINED);

		if (revdecllist) {
			*revdecllist = newlistnode (S_LIST, NOPOSN, namelist, *revdecllist);
		}
	}
}
/*}}}*/
/*{{{  PRIVATE treenode *undefinedcheck_make_declrlist (treenode *namelist)*/
/*
 *	makes a reversed variable list from a declaration
 */
PRIVATE treenode *undefinedcheck_make_declrlist (treenode *namelist)
{
	treenode *rlist = NULL;

	if (TagOf (namelist) == S_LIST) {
		while (!EmptyList (namelist)) {
			rlist = newlistnode (S_LIST, NOPOSN, ThisItem (namelist), rlist);
			namelist = NextItem (namelist);
		}
	} else {
		rlist = newlistnode (S_LIST, NOPOSN, namelist, rlist);
	}
	return rlist;
}
/*}}}*/
/*{{{  PRIVATE treenode *undefinedcheck_set_spec (treenode *n, treenode **revdecllist, udv_t **otherp)*/
/*
 *	adds a general specification to the undefinedness stack, returns a pointer to the
 *	body process of the specification.
 */
PRIVATE treenode *undefinedcheck_set_spec (treenode *n, treenode **revdecllist, udv_t **otherp)
{
	const int tag = TagOf (n);

	switch (TagOf (n)) {
		/*{{{  ABBR/VALABBR/RETYPE/VALRETYPE*/
	case S_ABBR:
	case S_VALABBR:
	case S_RETYPE:
	case S_VALRETYPE:
		{
			udv_t *tmp, *other;

			other = do_getudvof (DValOf (n));
			tmp = newudv ();
			if ((tag == S_VALABBR) || (tag == S_VALRETYPE)) {
				udv_udef_t local_undefined = {0,0,0,0};
				const int old_skip_channels = udv_skip_channels;

				udv_defaultmode = EXP_READ;
				udv_locn = LocnOf (n);
				udv_skip_channels = 1;
				modprewalktree (DValAddr (n), do_undefinedcheckexp, &local_undefined);
				if (have_undefined (&local_undefined)) {
					tmp->state[udv_vstacklevel] = (local_undefined.n_unknown) ? UDV_UNKNOWN : UDV_UNDEFINED;
				} else {
					tmp->state[udv_vstacklevel] = UDV_DEFINED;
				}
				udv_skip_channels = old_skip_channels;
			} else if ((tag == S_ABBR) && NVRDefinedOf (DNameOf (n))) {
				tmp->state[udv_vstacklevel] = UDV_UNDEFINED;
			} else {
				tmp->state[udv_vstacklevel] = other ? other->state[udv_vstacklevel] : UDV_DEFINED;
			}
			tmp->did_warn = 0;
#if 0
fprintf (stderr, "*** SetNUndef (\"%s\" @%p, %p)\n", WNameOf (NNameOf (DNameOf (n))), DNameOf (n), tmp);
#endif
			SetNUndef (DNameOf (n), tmp);
			tmp->nameof = DNameOf (n);
			tmp->next = udv_vstack;
			udv_vstack = tmp;
			init_decl_subtype (NTypeOf (DNameOf (n)), tmp, other ? other->state[udv_vstacklevel] : UDV_DEFINED);

			if (revdecllist) {
				*revdecllist = newlistnode (S_LIST, NOPOSN, DNameOf (n), *revdecllist);
			}
			if (otherp) {
				*otherp = other;
			}

			return DBodyOf (n);
		}
		/*}}}*/
		/*{{{  DECL*/
	case S_DECL:
		undefinedcheck_set_decl (DNameOf (n), revdecllist);
		return DBodyOf (n);
		/*}}}*/
		/*{{{  PROCDEF/SFUNCDEF/LFUNCDEF*/
	case S_PROCDEF:
	case S_MPROCDECL:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
	case S_PRAGMA:
		return DBodyOf (n);
		/*}}}*/
		/*{{{  default*/
	default:
		msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (n), "undefinedcheck_set_spec(badtag)");
		break;
		/*}}}*/
	}
	return NULL;
}
/*}}}*/
/*{{{  PRIVATE void undefinedcheck_end_decl (treenode *rdecllist)*/
/*
 *	removes declaration(s) from the undefined variable list (using a reversed list of them built earlier)
 */
PRIVATE void undefinedcheck_end_decl (treenode *rdecllist)
{
	treenode *rnext;

	for (; !EmptyList (rdecllist); rdecllist = rnext) {
		treenode *item = ThisItem (rdecllist);
		udv_t *tmp;

		rnext = NextItem (rdecllist);
		if (NUndefOf (item) && (NUndefOf (item) != udv_vstack)) {
#if 0
fprintf (stderr, "undefinedcheck_end_decl: decl not TOS.  NNameOf(item) = %s, NNameOf (udv_vstack->nameof) = %s.\n", WNameOf (NNameOf (item)), udv_vstack ? WNameOf (NNameOf (udv_vstack->nameof)) : "<none>");
fprintf (stderr, "undefinedcheck_end_decl: NUndefOf (item) = %p, udv_vstack = %p\n", NUndefOf (item), udv_vstack);
#endif
			msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (item), "undefinedcheck_end_decl(not top of stack 1)");
		} else if (!NUndefOf (item)) {
			/* make sure it's this one */
			if (NNameOf (item) != (udv_vstack ? NNameOf (udv_vstack->nameof) : NULL)) {
				msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (item), "undefinedcheck_end_decl(not top of stack 2)");
			}
		}
		SetNUndef (item, NULL);
		tmp = udv_vstack;
		udv_vstack = udv_vstack->next;
		free_decl_subtype (item, tmp);
		freeudv (tmp);
		freenode (&rdecllist);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void undefinedcheck_end_spec (treenode *n, udv_t *other)*/
/*
 *	removes a general specification from the undefined variable list
 */
PRIVATE void undefinedcheck_end_spec (treenode *n, udv_t *other)
{
	udv_t *tmp;

	switch (TagOf (n)) {
		/*{{{  ABBR/VALABBR/RETYPE/VALRETYPE*/
	case S_ABBR:
	case S_VALABBR:
	case S_RETYPE:
	case S_VALRETYPE:
		if (NVRDefinedOf (DNameOf (n))) {
			/* check that this gets left defined */
			udv_defaultmode = EXP_READ;
			udv_locn = NVEndPosnOf (DNameOf (n));
			modprewalktree (DNameAddr (n), do_undefinedcheckexp, NULL);
		}
		tmp = NUndefOf (DNameOf (n));
		if (!tmp && (DNameOf (n) != (udv_vstack ? udv_vstack->nameof : NULL))) {
			msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (n), "undefinedcheck_end_spec(not top of stack 2)");
		} else {
			tmp = udv_vstack;
		}
		SetNUndef (DNameOf (n), NULL);
		if (tmp != udv_vstack) {
			msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (n), "undefinedcheck_end_spec(not top of stack 1)");
		}
		if (other) {
			switch (TagOf (n)) {
			default:
				other->state[udv_vstacklevel] = tmp->state[udv_vstacklevel];
				udv_locn = LocnOf (n);
				udv_copy_substate (other, tmp);
				break;
				/* RETYPEs changes things somewhat.. */
			case S_RETYPE:
			case S_VALRETYPE:
				udv_set_state (other, udv_vstacklevel, tmp->state[udv_vstacklevel]);
				break;
			}
		}
		udv_vstack = udv_vstack->next;
		freeudv (tmp);
		break;
		/*}}}*/
		/*{{{  default*/
	default:
		msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (n), "undefinedcheck_end_spec(badtag)");
		break;
		/*}}}*/
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void undefinedcheck_rewind (udv_t *marker)*/
/*
 *	"rewinds" the variable stack until we reach something from earlier.
 */
PRIVATE void undefinedcheck_rewind (udv_t *marker)
{
	while (udv_vstack && (udv_vstack != marker)) {
		udv_t *this = udv_vstack;
		treenode *name = this->nameof;

		switch (TagOf (name)) {
		case N_DECL:
			/* removing a declaration */
			{
				treenode *rlist = undefinedcheck_make_declrlist (DNameOf (NDeclOf (name)));

				undefinedcheck_end_decl (rlist);
			}
			break;
		case N_ABBR:
		case N_VALABBR:
		case N_RETYPE:
		case N_VALRETYPE:
			/* removing a specification */
			{
				udv_t *other;

				other = do_getudvof (DValOf (NDeclOf (name)));
				undefinedcheck_end_spec (NDeclOf (name), other);
			}
			break;
		default:
			msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (name), "undefinedcheck_rewind(badtag)");
			return;
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_undefinedcheck (treenode *n, void *const voidptr)*/
/*
 *	performs undefinedness checking
 */
PRIVATEPARAM int do_undefinedcheck (treenode *n, void *const voidptr)
{
	const int tag = TagOf (n);

	switch (tag) {
		/*{{{  ABBR/VALABBR/RETYPE/VALRETYPE*/
	case S_ABBR:
	case S_VALABBR:
	case S_RETYPE:
	case S_VALRETYPE:
		{
			udv_t *other;

			other = NULL;
			undefinedcheck_set_spec (n, NULL, &other);
			prewalkproctree (DBodyOf (n), do_undefinedcheck, voidptr);
			undefinedcheck_end_spec (n, other);
		}
		break;
		/*}}}*/
		/*{{{  DECL*/
	case S_DECL:
		{
			treenode *revdecllist = NULL;		/* used to temporarily store declarations (in reverse order) */

			undefinedcheck_set_decl (DNameOf (n), &revdecllist);
			/* walk body */
			prewalkproctree (DBodyOf (n), do_undefinedcheck, voidptr);
			undefinedcheck_end_decl (revdecllist);
		}
		break;
		/*}}}*/
		/*{{{  CLAIM*/
	case S_CLAIM:
		{
			treenode *claimvar = CTempOf (n);

#if 0
fprintf (stderr, "do_undefinedcheck(): S_CLAIM: before body check\n");
udv_dump_stack (stderr, 4, 0);
#endif
			if (!udv_newclaim (claimvar)) {
				/* already claimed! */
				udv_error_now (USE_NESTED_CLAIM, LocnOf (n), WNameOf (NNameOf (claimvar)));
			} else {
				prewalkproctree (CBodyOf (n), do_undefinedcheck, voidptr);
				udv_freeclaim (claimvar);
			}
#if 0
fprintf (stderr, "do_undefinedcheck(): S_CLAIM: after body check\n");
udv_dump_stack (stderr, 4, 0);
#endif
		}
		break;
		/*}}}*/
		/*{{{  PRAGMA*/
	case S_PRAGMA:
		switch ((pragma_name_tag_t)NModeOf (DNameOf (n))) {
		case pragma_name_defined:
		case pragma_name_undefined:
			{
				treenode *list = DValOf (n);
				udv_t *tmp;
				pragma_name_tag_t which = (pragma_name_tag_t)NModeOf (DNameOf (n));

#if 0
fprintf (stderr, "do_undefinedcheck: got S_PRAGMA with pragma_name_{un,}defined.  DValOf is:");
printtreenl (stderr, 4, DValOf (n));
#endif
				while (!EmptyList (list)) {
					tmp = NUndefOf (ThisItem (list));
					if (tmp && (udv_vstacklevel >= 0)) {
						/* automatically deals with nested state */
						udv_set_state (tmp, udv_vstacklevel, (which == pragma_name_defined) ? UDV_DEFINED : UDV_UNDEFINED);
					}
					list = NextItem (list);
				}
			}
			break;
		case pragma_name_iospace:
			{
				treenode *list = DValOf (n);

				while (!EmptyList (list)) {
					treenode *type = NTypeOf (ThisItem (list));

#if 0
fprintf (stderr, "occ21: PRAGMA IOSPACE on node:");
printtreenl (stderr, 4, ThisItem (list));
#endif
					if (TagOf (type) != S_ARRAY) {
#if 0
fprintf (stderr, "occ21: huh, PRAGMA IOSPACE but not array..\n");
#endif
					} else {
#if 0
fprintf (stderr, "occ21: setting IOSPACE on array type.\n");
#endif
						SetTypeAttr (type, TypeAttrOf (type) | TypeAttr_iospace);
					}
					list = NextItem (list);
				}
			}
			break;
		default:
			break;
		}
		return CONTINUE_WALK;
		break;
		/*}}}*/
		/*{{{  PINSTANCE/FINSTANCE*/
	case S_PINSTANCE:
	case S_FINSTANCE:
		if ((tag == S_PINSTANCE) && !separatelycompiled (INameOf (n)) && ((TagOf (INameOf (n)) == N_PROCDEF) || (udv_thingisdynmobileproctype (INameOf (n))))) {
			/*{{{  check regular PROC */
			treenode *formallist, *actuallist, *body;
			treenode *revflist, *revalist;
			udv_t *other, *tmp;
			udv_udef_t local_undefined;
			char alt_state;
			const int forked = IForkedOf (n);
			const int recursive = IRecursiveOf (n);
			const int mprocact = udv_thingisdynmobileproctype (INameOf (n));

#if 0
fprintf (stderr, "do_undefinedcheck: INSTANCE of %s, forked = %d, recursive = %d\n", WNameOf (NNameOf (INameOf (n))), forked, recursive);
#endif
			/*{{{  walk parameters, only ever go 1 deep in here, nested PROCs are handled slightly differently */
			revflist = NULL;
			revalist = NULL;
			if (mprocact) {
				/* better check that MOBILE PROC itself is defined */
				udv_defaultmode = EXP_READ;
				udv_locn = LocnOf (n);
				modprewalktree (INameAddr (n), do_undefinedcheckexp, NULL);
			}
			formallist = NParamListOf (INameOf (n));
			actuallist = IParamListOf (n);
			while (!EmptyList (formallist) && !EmptyList (actuallist)) {
				treenode *actual = ThisItem (actuallist);
#ifdef MOBILES
				int is_cloned = 0;

				local_undefined.n_undefined = 0;
				local_undefined.n_unknown = 0;
				local_undefined.n_partial = 0;
				local_undefined.supress_warn = 0;
				/* check for chan-type parameters to PROCs, make sure they're writable */
				if (TagOf (actual) == S_CLONE) {
					is_cloned = 1;
					actual = OpOf (actual);
				}
#if 0
fprintf (stderr, "do_undefinedcheck: INSTANCE of %s, udv_thingisdynmobilechantype(ThisItem(actuallist)) = %d, ThisItem(actuallist)[actual] =", WNameOf (NNameOf (INameOf (n))), udv_thingisdynmobilechantype(ThisItem(actuallist)));
printtreenl (stderr, 4, actual);
#endif
				/* check shared channel-types for CLAIMedness -- push stack because we don't define it really */
				if (udv_thingisdynmobilechantype (actual)) {
					int cur_vstack = udv_vstacklevel;
					int claimed = udv_isclaimed (actual);
					treenode *acttype = chk_gettype_main (actual, TRUE);
					treenode *formal = ThisItem (formallist);
					treenode *fmltype = NTypeOf (formal);
					int fixed = udv_isfixed (formal);

					udv_pushstack ();
					vstack_copy (cur_vstack, udv_vstacklevel, TRUE);
					/* formal should be a chan-type too! */
#if 0
fprintf (stderr, "use4: do_undefinedcheck(): INSTANCE of %s.  (%s) actual = ", WNameOf (NNameOf (INameOf (n))), claimed ? "claimed" : "not-claimed");
printtreenl (stderr, 4, actual);
fprintf (stderr, "use4: do_undefinedcheck(): INSTANCE of %s.  acttype = ", WNameOf (NNameOf (INameOf (n))));
printtreenl (stderr, 4, acttype);
fprintf (stderr, "use4: do_undefinedcheck(): INSTANCE of %s.  (%s) formal = ", WNameOf (NNameOf (INameOf (n))), fixed ? "fixed" : "not-fixed");
printtreenl (stderr, 4, formal);
fprintf (stderr, "use4: do_undefinedcheck(): INSTANCE of %s.  fmltype = ", WNameOf (NNameOf (INameOf (n))));
printtreenl (stderr, 4, fmltype);
#endif

					/* do late checks for parameter-passing CLAIMed things */
					if ((TagOf (acttype) == N_TYPEDECL) && (TagOf (fmltype) == N_TYPEDECL)) {
						/* if shared actual and non-shared formal, actual must be CLAIMed.  Furthermore, formal must be FIXED */
						if ((NTypeAttrOf (acttype) & TypeAttr_shared) && !(NTypeAttrOf (fmltype) & TypeAttr_shared)) {
							/*{{{  shared actual, non-shared formal*/
							if (!claimed) {
								udv_error_now (USE_PARAM_MUST_CLAIM, LocnOf (n), use_exprstring (actual));
							} else if (!fixed) {
								udv_error_now (USE_PARAM_UNFIXED, LocnOf (n), use_exprstring (actual));
							} else if (forked) {
								/* always moved away with the process, so mark as written */
								udv_defaultmode = EXP_WRITTEN;
							} else {
								/* otherwise we just "read" this */
								udv_defaultmode = EXP_READ;
							}
							/*}}}*/
						} else if (!(NTypeAttrOf (acttype) & TypeAttr_shared) && (NTypeAttrOf (fmltype) & TypeAttr_shared)) {
							/*{{{  unshared actual, shared formal*/
							/* badness */
							udv_error_now (USE_PARAM_INVALID_SHARED, LocnOf (n), use_exprstring (actual));
							/*}}}*/
						} else if ((NTypeAttrOf (acttype) & TypeAttr_shared) && (NTypeAttrOf (fmltype) & TypeAttr_shared)) {
							/*{{{  shared actual, shared formal*/
							/* if both SHARED, must not be CLAIMed */
							if (claimed) {
								udv_error_now (USE_PARAM_MUST_UNCLAIM, LocnOf (n), use_exprstring (actual));
							}
							/* don't write to FIXED parameters */
							if (fixed) {
								udv_defaultmode = EXP_READ;
							} else {
								udv_defaultmode = EXP_WRITTEN;
							}
							/*}}}*/
						} else {
							/*{{{  unshared actual, unshared formal*/
							/* don't write to FIXED parameters */
							if (fixed) {
								udv_defaultmode = EXP_READ;
							} else {
								udv_defaultmode = EXP_WRITTEN;
							}
							/*}}}*/
						}
					} else {
						udv_defaultmode = EXP_WRITTEN;
					}
					udv_locn = LocnOf (n);
					modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, NULL);
					/* generates a JMP error if something if claimed */
					udv_vstacklevel = cur_vstack;
				}
#endif
				alt_state = UDV_DEFINED;
				if (forked) {
					/*{{{  FORKed params must all be defined, non-mobile VAR and RESULTs permitted, but must be #PRAGMA SHARED (fe/use) */
					/* (ordinary) forked parameters have different semantic effects -- they must ALL be defined! */
					/* non-mobile VAR and RESULT params are permitted, but specially checked by the usage checker (#PRAGMA SHARED) */
					switch (TagOf (ThisItem (formallist))) {
					case N_PARAM:
						if (!udv_thingismobile (ThisItem (formallist))) {
							/* need to be able to read this */
							udv_defaultmode = EXP_READ;
							udv_locn = LocnOf (n);
							modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, &local_undefined);
						}
						break;
					case N_RESULTPARAM:
						alt_state = UDV_UNDEFINED;
						break;
					case N_VALPARAM:
						udv_defaultmode = EXP_READ;
						udv_locn = LocnOf (n);
						modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, &local_undefined);
						break;
					}

					if (have_undefined (&local_undefined)) {
						if (local_undefined.n_undefined) {
							alt_state = UDV_UNDEFINED;
						} else if (local_undefined.n_unknown) {
							alt_state = UDV_UNKNOWN;
						} else {
							alt_state = UDV_PARTIAL;
						}
#ifdef MOBILES
						/*{{{  if it's a dynamic mobile channel-type, undefinedness is very bad -- insert UNDEFINED for tran */
						if (udv_thingisdynmobilechantype (actual)) {
#if 0
fprintf (stderr, "dynamic mobile chan-type actual to FORKED thing (is_cloned = %d), n_undef = %d, n_unk = %d, actual = ", is_cloned, local_undefined.n_undefined, local_undefined.n_unknown);
printtreenl (stderr, 4, actual);
#endif
						}
						/*}}}*/
#endif
					}

					/* add parameter to variable stack */
					other = do_getudvof (actual);
					tmp = newudv ();
					tmp->state[udv_vstacklevel] = other ? other->state[udv_vstacklevel] : alt_state;
					tmp->nameof = ThisItem (formallist);
					tmp->did_warn = 0;
					tmp->next = udv_vstack;
					init_decl_subtype (NTypeOf (ThisItem (formallist)), tmp, other ? other->state[udv_vstacklevel] : alt_state);
					if (other) {
						udv_copy_substate (tmp, other);
					}
#if 0
fprintf (stderr, "*** SetNUndef (%p, %p)\n", ThisItem (formallist), tmp);
#endif
					SetNUndef (ThisItem (formallist), tmp);
					udv_vstack = tmp;

					/* add to reverse list */
					revflist = newlistnode (S_LIST, NOPOSN, ThisItem (formallist), revflist);
					revalist = newlistnode (S_LIST, NOPOSN, ThisItem (actuallist), revalist);
					/*}}}*/
				} else if (recursive) {
					/*{{{  the state of recursive parameters might not be sensible yet.., assume the best for recursive cases (will complain later if anything undefined)*/
					switch (TagOf (ThisItem (formallist))) {
					case N_VALPARAM:
						/* this must be defined at this point */
						udv_defaultmode = EXP_READ;
						udv_locn = LocnOf (n);
						modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, NULL);
						break;
					case N_PARAM:
					case N_RESULTPARAM:
						/* actually, just ignore these... (assume written) */
						udv_defaultmode = EXP_WRITTEN;
						udv_locn = LocnOf (n);
						modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, NULL);
						break;
					}
					/*}}}*/
				} else {
					switch (TagOf (ThisItem (formallist))) {
						/*{{{  N_VALPARAM -- check readability of parameter*/
					case N_VALPARAM:
						/* check readability of parameter */
						udv_defaultmode = EXP_READ;
						udv_locn = LocnOf (n);
						modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, &local_undefined);
						if (have_undefined (&local_undefined)) {
							if (local_undefined.n_undefined) {
								alt_state = UDV_UNDEFINED;
							} else if (local_undefined.n_unknown) {
								alt_state = UDV_UNKNOWN;
							} else {
								alt_state = UDV_PARTIAL;
							}
						}
						/* create a new variable and pop it in the formal parameter -- for walking the instance.. */
						other = do_getudvof (ThisItem (actuallist));
						tmp = newudv ();
						tmp->state[udv_vstacklevel] = other ? other->state[udv_vstacklevel] : alt_state;
						tmp->nameof = ThisItem (formallist);
						tmp->did_warn = 0;
						tmp->next = udv_vstack;
						init_decl_subtype (NTypeOf (ThisItem (formallist)), tmp, other ? other->state[udv_vstacklevel] : alt_state);
						if (other) {
							udv_copy_substate (tmp, other);
						}
#if 0
fprintf (stderr, "*** SetNUndef (%p, %p)\n", ThisItem (formallist), tmp);
#endif
						SetNUndef (ThisItem (formallist), tmp);
						udv_vstack = tmp;
						break;
						/*}}}*/
						/*{{{  N_PARAM, N_RESULTPARAM -- copy existing state*/
					case N_PARAM:
					case N_RESULTPARAM:
						/* if not VAL-param, must be a variable, other parameter, etc. */
#ifdef MOBILES
						/* if the formal is a channel, check for RECORDSUB (channel-types) */
						if ((TagOf (NTypeOf (ThisItem (formallist))) == S_CHAN) && (TagOf (actual) == S_RECORDSUB)) {
							/* walk it to ensure its been CLAIMed already -- no CLONEs here */
							modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, NULL);
						}
#endif
						/* create a new variable and pop it in the formal parameter -- for walking the instance.. */
						other = do_getudvof (ThisItem (actuallist));
						tmp = newudv ();
						tmp->state[udv_vstacklevel] = other ? other->state[udv_vstacklevel] : alt_state;
						tmp->nameof = ThisItem (formallist);
						tmp->did_warn = 0;
						tmp->next = udv_vstack;
						init_decl_subtype (NTypeOf (ThisItem (formallist)), tmp, other ? other->state[udv_vstacklevel] : alt_state);
						if (other) {
							udv_copy_substate (tmp, other);
						}
#if 0
fprintf (stderr, "*** SetNUndef (%p, %p)\n", ThisItem (formallist), tmp);
#endif
						SetNUndef (ThisItem (formallist), tmp);
						udv_vstack = tmp;
						break;
						/*}}}*/
					}
					/* add to reverse list */
					revflist = newlistnode (S_LIST, NOPOSN, ThisItem (formallist), revflist);
					revalist = newlistnode (S_LIST, NOPOSN, ThisItem (actuallist), revalist);
				}

				formallist = NextItem (formallist);
				actuallist = NextItem (actuallist);
			}
			/*}}}*/
			/*{{{  walk body of instanced process -- but only if non-recursive and not a MOBILE PROC activation */
			/* if (!NPRecursiveOf (INameOf (n))) { */
			if (!IRecursiveOf (n) && !mprocact) {			/* July'02 -- i'm sure this is more correct, we want to capture the first instance */
				if (udv_itraceptr < UDV_ITRACE_MAX) {
					udv_itrace[udv_itraceptr] = n;
				}
				udv_itraceptr++;
				syn_lexlevel++;
				body = DValOf (NDeclOf (INameOf (n)));
				prewalkproctree (body, do_undefinedcheck, NULL);
				syn_lexlevel--;
				udv_itraceptr--;
			}
			/*}}}*/
			/*{{{  update state of non-value parameters */
			formallist = NParamListOf (INameOf (n));
			actuallist = IParamListOf (n);
			while (!EmptyList (formallist) && !EmptyList (actuallist)) {
				if (forked) {
					/* A FORKed PROC leaves VALs and non-VALs untouched (can't change them!), MOBILEs all go to undefined, unless CLONEd */
					/* special case: VAR and RESULT parameters need to be checked for definedness on PROC return */
					/* RESULT parameters are left untouched too, but that's a feature of FORK! */
					if ((TagOf (ThisItem (actuallist)) == S_CLONE) || (udv_thingisdynmobilebarriertype (ThisItem (actuallist)))) {
						/* dandy */
					} else if (udv_thingisdynmobilechantype (ThisItem (actuallist))) {
						other = do_getudvof (ThisItem (actuallist));
						if (other) {
							other->state[udv_vstacklevel] = UDV_UNDEFINED;
						}
					} else if (udv_thingismobile (ThisItem (actuallist))) {
						if (TagOf (ThisItem (formallist)) == N_VALPARAM) {
							/* not a problem, VAL params handled nicely */
						} else {
							other = do_getudvof (ThisItem (actuallist));
							if (other) {
								other->state[udv_vstacklevel] = UDV_UNDEFINED;
							}
						}
					} else if ((TagOf (ThisItem (formallist)) == N_PARAM) || (TagOf (ThisItem (formallist)) == N_RESULTPARAM)) {
						/* update state of any other */
						other = do_getudvof (ThisItem (actuallist));
						tmp = NUndefOf (ThisItem (formallist));
						if (tmp && other) {
							other->state[udv_vstacklevel] = tmp->state[udv_vstacklevel];
						}
					}
				} else {
					if ((TagOf (ThisItem (formallist)) == N_PARAM) || (TagOf (ThisItem (formallist)) == N_RESULTPARAM)) {
						/* update state of any other */
						other = do_getudvof (ThisItem (actuallist));
						tmp = NUndefOf (ThisItem (formallist));
						if (tmp && other) {
							other->state[udv_vstacklevel] = tmp->state[udv_vstacklevel];
						}
					}
				}
				formallist = NextItem (formallist);
				actuallist = NextItem (actuallist);
			}
			/*}}}*/
			/*{{{  remove parameters (but not if recursive...!) */
			if (forked || !recursive) {
				formallist = revflist;
				actuallist = revalist;
				while (!EmptyList (formallist) && !EmptyList (actuallist)) {
					treenode *nextf, *nexta;

					switch (TagOf (ThisItem (formallist))) {
					case N_VALPARAM:
					case N_RESULTPARAM:
					case N_PARAM:
						tmp = udv_vstack;
						udv_vstack = udv_vstack->next;
#if 0
fprintf (stderr, "*** SetNUndef (%p, NULL)\n", ThisItem (formallist));
#endif
						SetNUndef (ThisItem (formallist), NULL);
						free_decl_subtype (ThisItem (formallist), tmp);
						freeudv (tmp);
						break;
					}
					nextf = NextItem (formallist);
					nexta = NextItem (actuallist);
					freenode (&formallist);
					freenode (&actuallist);
					formallist = nextf;
					actuallist = nexta;
				}
			}
			/*}}}*/
			/*}}}*/
		#if defined(PD_ATTACH_DYNMOB) && defined(PD_DETACH_DYNMOB)
		} else if ((tag == S_PINSTANCE) && (TagOf (INameOf (n)) == N_PREDEFPROC) && ((NModeOf (INameOf (n)) == PD_ATTACH_DYNMOB) || (NModeOf (INameOf (n)) == PD_DETACH_DYNMOB))) {
			/*{{{  check special MOBILE-affecting pre-defines (ATTACH/DETACH.DYMOB) */
			treenode *actuallist = IParamListOf (n);
			udv_udef_t local_undefined = {0, 0, 0, 0};


			if (NModeOf (INameOf (n)) == PD_DETACH_DYNMOB) {
				/* read from dynamic mobile (in first parameter) */
				udv_defaultmode = EXP_READ;
				udv_locn = LocnOf (n);
				modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, (void *)&local_undefined);
				if (have_undefined (&local_undefined)) {
					/* note: although we already generated a warning, it's stuck in a queue, and we skip with error_now */
					udv_error_now (USE_PARAM_UNDEFINED, LocnOf (n), WNameOf (NNameOf (ThisItem (actuallist))));
				}
				/* and write to the others */
				actuallist = NextItem (actuallist);
				while (!EmptyList (actuallist)) {
					udv_defaultmode = EXP_WRITTEN;
					udv_locn = LocnOf (n);
					modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, voidptr ? voidptr : NULL);
					actuallist = NextItem (actuallist);
				}
			} else {
				treenode *addrparam = ThisItem (actuallist);

				/* read from first two integers */
				udv_defaultmode = EXP_READ;
				udv_locn = LocnOf (n);
				modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, (void *)&local_undefined);
				if (have_undefined (&local_undefined)) {
					if (nodetypeoftag (TagOf (ThisItem (actuallist))) == NAMENODE) {
						udv_error_now (USE_PARAM_UNDEFINED, LocnOf (n), WNameOf (NNameOf (ThisItem (actuallist))));
					} else {
						udv_error_nowi (USE_NTH_PARAM_UNDEFINED, LocnOf (n), 1);
					}
				}
				actuallist = NextItem (actuallist);
				udv_defaultmode = EXP_READ;
				udv_locn = LocnOf (n);
				modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, (void *)&local_undefined);
				if (have_undefined (&local_undefined)) {
					if (nodetypeoftag (TagOf (ThisItem (actuallist))) == NAMENODE) {
						udv_error_now (USE_PARAM_UNDEFINED, LocnOf (n), WNameOf (NNameOf (ThisItem (actuallist))));
					} else {
						udv_error_nowi (USE_NTH_PARAM_UNDEFINED, LocnOf (n), 2);
					}
				}
				actuallist = NextItem (actuallist);
				/* and write to the last one */
				udv_defaultmode = EXP_WRITTEN;
				udv_locn = LocnOf (n);
				modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, voidptr ? voidptr : NULL);
				/* make the INT address parameter undefined as well -- discourages its use after attaching */
				udv_resetexp (addrparam, UDV_DEFINED, UDV_UNDEFINED);
			}
			/*}}}*/
		#endif /* PD_ATTACH_DYNMOB && PD_DETACH_DYNMOB */
		} else if ((tag == S_PINSTANCE) && (TagOf (INameOf (n)) == N_PREDEFPROC) && ((NModeOf (INameOf (n)) == PD_DECODE_DATA))) {
			/*{{{  this defines the second two parameters.  if the 4th parameter isn't present, we add it here*/
			treenode *actuallist;
			treenode *lastactual = NULL;
			int i;

			for (i=0, actuallist = IParamListOf (n); !EndOfList (actuallist); i++, actuallist = NextItem (actuallist)) {
				switch (i) {
				case 0:
					udv_defaultmode = EXP_READ;
					udv_locn = LocnOf (n);
					modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, NULL);
					break;
				case 1:
				case 2:
					udv_defaultmode = EXP_WRITTEN;
					udv_locn = LocnOf (n);
					modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, NULL);
					break;
				}
				lastactual = actuallist;
			}
			if (i == 3) {
				/* missing 4th parameter */
				/* XXX This code is modifying the tree inside
				   the undefinedness checker; this is a bad
				   idea. */
				const int old = switch_to_real_workspace ();
				treenode *fparamcopy;
				
				/* (BYTESIN var) gets fixed for dynamic mobile arrays, etc. in tran */
				fparamcopy = copytree (ThisItem (IParamListOf (n)), syn_lexlevel);
				SetRight (lastactual, newlistnode (S_LIST, NOPOSN, newmopnode (S_BYTESIN, NOPOSN, fparamcopy, S_INT), NULL));

				switch_to_prev_workspace (old);
			}
			/*}}}*/
		} else if ((tag == S_PINSTANCE) && (TagOf (INameOf (n)) == N_PREDEFPROC) && ((NModeOf (INameOf (n)) == PD_MPPSERIALISE) || (NModeOf (INameOf (n)) == PD_MPPDESERIALISE))) {
			/*{{{  these define their second parameter*/
			treenode *actuallist;
			int i;

			for (i=0, actuallist = IParamListOf (n); !EndOfList (actuallist); i++, actuallist = NextItem (actuallist)) {
				switch (i) {
				case 0:
					udv_defaultmode = EXP_READ;
					udv_locn = LocnOf (n);
					modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, NULL);
					break;
				case 1:
					udv_defaultmode = EXP_WRITTEN;
					udv_locn = LocnOf (n);
					modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, NULL);
					break;
				}
			}
			/*}}}*/
		} else {
			/*{{{  check something else (function, external PROC, etc.) */
			/* check that VAL parameters get decent variables */
			treenode *formallist = NParamListOf (INameOf (n));
			treenode *actuallist = IParamListOf (n);

#if 0
fprintf (stderr, "something else in use4! n =");
printtreenl (stderr, 4, n);
#endif
			while (!EmptyList (formallist) && !EmptyList (actuallist)) {
				int f_undef = ((NTypeAttrOf (ThisItem (formallist))) & TypeAttr_undefined);

				switch (TagOf (ThisItem (formallist))) {
				case N_VALPARAM:
					udv_defaultmode = EXP_READ;
					udv_locn = LocnOf (n);
					modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, voidptr ? voidptr : NULL);
					break;
				case N_PARAM:
				case N_RESULTPARAM:
					/* assume it gets left defined */
					if (udv_thingisdynmobilechantype (ThisItem (actuallist))) {
						int fixed = udv_isfixed (ThisItem (formallist));

						udv_defaultmode = fixed ? EXP_READ : EXP_WRITTEN;
					} else {
						udv_defaultmode = EXP_WRITTEN;
					}
					udv_locn = LocnOf (n);
					modprewalktree (ThisItemAddr (actuallist), do_undefinedcheckexp, voidptr ? voidptr : NULL);
					break;
				}
				if (f_undef) {
					/* call leaves this undefined, better reflect that in the actual parameter.. */
					udv_udef_t local_undefined;

					local_undefined.n_undefined = 1;
					local_undefined.n_unknown = 0;
					local_undefined.n_partial = 0;
					local_undefined.supress_warn = 0;
					prewalktree (ThisItem (actuallist), do_undefinetargets, (void *)&local_undefined);
				}
				/* else { handle RESULT parameters :) } */
				formallist = NextItem (formallist);
				actuallist = NextItem (actuallist);
			}
			/*}}}*/
		}
		break;
		/*}}}*/
		/*{{{  ASS*/
	case S_ASS:
		{
			udv_udef_t local_undef;

			udv_locn = LocnOf (n);
			local_undef.n_undefined = 0;
			local_undef.n_unknown = 0;
			local_undef.n_partial = 0;
			local_undef.supress_warn = 0;
			switch (TagOf (RHSOf (n))) {
			case S_LIST:
				if (TagOf (ThisItem (RHSOf (n))) == S_FINSTANCE) {
					treenode *list;
	
					/* multi-valued FUNCTION */
					prewalkproctree (ThisItem (RHSOf (n)), do_undefinedcheck, &local_undef);
					for (list = LHSOf (n); !EmptyList (list); list = NextItem (list)) {
#ifdef MOBILES
						if (udv_thingisdynmobile (ThisItem (list))) {
							udv_defaultmode = EXP_READ;
						} else
#endif
						{
							udv_defaultmode = EXP_WRITTEN;
						}
						udv_locn = LocnOf (n);
						modprewalktree (ThisItemAddr (list), do_undefinedcheckexp, NULL);
						if (have_undefined (&local_undef)) {
							prewalktree (ThisItem (list), do_undefinetargets, &local_undef);
						}
					}
				} else {
					treenode *llist, *rlist;
	
					/* walk assignments */
					for (llist = LHSOf (n), rlist = RHSOf (n); !EmptyList (llist) && !EmptyList (rlist); llist = NextItem (llist), rlist = NextItem (rlist)) {
						do_undefinedcheck_assign (ThisItemAddr (llist), ThisItemAddr (rlist));
					}
				}
				break;
			case S_FINSTANCE:
				/* check parameters to single-result FUNCTION */
				prewalkproctree (RHSOf (n), do_undefinedcheck, &local_undef);
#ifdef MOBILES
				if (udv_thingisdynmobile (LHSOf (n))) {
					udv_defaultmode = EXP_READ;
				} else
#endif
				{
					udv_defaultmode = EXP_WRITTEN;
				}
				udv_locn = LocnOf (n);
				modprewalktree (LHSAddr (n), do_undefinedcheckexp, NULL);
				if (have_undefined (&local_undef)) {
					prewalktree (LHSOf (n), do_undefinetargets, &local_undef);
				}
				break;
			default:
				do_undefinedcheck_assign (LHSAddr (n), RHSAddr (n));
				break;
			}
		}
		break;
		/*}}}*/
		/*{{{  OUTPUT*/
	case S_OUTPUT:
		udv_locn = LocnOf (n);
		udv_defaultmode = CHAN_OUTPUT;
		modprewalktree (LHSAddr (n), do_undefinedcheckexp, NULL);
		udv_defaultmode = EXP_READ;
#ifdef MOBILES
		/* very special consideration if we're outputting something MOBILEy */
		{
			treenode *t = follow_user_type (chk_gettype (LHSOf (n)));

			if ((TagOf (t) == S_CHAN) || (TagOf (t) == S_PORT)) {
				mcheckprotocol (ProtocolAddr (t), RHSAddr (n));
			}
		}
#else
		modprewalktree (RHSAddr (n), do_undefinedcheckexp, NULL);
#endif
		break;
		/*}}}*/
		/*{{{  INPUT/TAGGED_INPUT*/
	case S_INPUT:
	case S_TAGGED_INPUT:
		udv_locn = LocnOf (n);
		udv_defaultmode = CHAN_INPUT;
		modprewalktree (LHSAddr (n), do_undefinedcheckexp, NULL);
		udv_defaultmode = EXP_WRITTEN;
#ifdef MOBILES
		udv_dynmobile_targetcheck (RHSAddr (n));
#else
		modprewalktree (RHSAddr (n), do_undefinedcheckexp, NULL);
#endif
		break;
		/*}}}*/
		/*{{{  X_INPUT/X_TAGGED_INPUT*/
	case S_X_INPUT:
	case S_X_TAGGED_INPUT:
		udv_locn = LocnOf (n);
		udv_defaultmode = CHAN_XINPUT;
		modprewalktree (LHSAddr (n), do_undefinedcheckexp, NULL);
		udv_defaultmode = EXP_WRITTEN;
#ifdef MOBILES
		udv_dynmobile_targetcheck (RHSAddr (n));
#else
		modprewalktree (RHSAddr (n), do_undefinedcheckexp, NULL);
#endif
		/* walk the nested routines */
		prewalkproctree (ActionDuringOf (n), do_undefinedcheck, voidptr);
		/* reset state to UDV_INPUTTED, from UDV_XINPUTTED */
		udv_resetexp (LHSOf (n), UDV_XINPUTTED, UDV_INPUTTED);
		prewalkproctree (ActionAfterOf (n), do_undefinedcheck, voidptr);
		break;
		/*}}}*/
		/*{{{  CASE_INPUT/X_CASE_INPUT*/
	case S_CASE_INPUT:
	case S_X_CASE_INPUT:
		/* process CASE input */
		{
			treenode *list;
			const int firstlevel = udv_vstacklevel;
			const BOOL isextended = (tag == S_X_CASE_INPUT);
			udv_t *const saved_xcvar = udv_xcvar;

#if 0
fprintf (stderr, "udv: CASE_INPUT: about to process, LHSOf (n) =");
printtreenl (stderr, 4, LHSOf (n));
#endif
			/* check what we're inputting from */
			udv_locn = LocnOf (n);
			udv_defaultmode = isextended ? CHAN_XINPUT : CHAN_INPUT;
			modprewalktree (LHSAddr (n), do_undefinedcheckexp, NULL);
			udv_xcvar = do_getudvof (LHSOf (n));

#if 0
fprintf (stderr, "udv: [X_]CASE_INPUT, before processing variants.  LHS is:");
printtreenl (stderr, 4, LHSOf (n));
#endif

			/* the RHS is a list of things, check each one (we capture variant seperately) */
			for (list = RHSOf (n); !EmptyList (list); list = NextItem (list)) {
				udv_pushstack ();
				vstack_copy (firstlevel, udv_vstacklevel, TRUE);
				prewalkproctree (ThisItem (list), do_undefinedcheck, voidptr);
#if 0
fprintf (stderr, "udv: [X_]CASE_INPUT, processed variant, stack now:\n");
udv_dump_stack (stderr, 4, firstlevel);
fprintf (stderr, "udv: again, LHSOf (n) is:");
printtreenl (stderr, 4, LHSOf (n));
#endif
			}

			/* merge branches back into current */
			vstack_merge_if (udv_vstacklevel, firstlevel);
			udv_vstacklevel = firstlevel;
			udv_xcvar = saved_xcvar;
			if (isextended) {
				udv_resetexp (LHSOf (n), UDV_XINPUTTED, UDV_INPUTTED);
			}
#if 0
fprintf (stderr, "udv: CASE_INPUT: done checking, LHSOf (n) =");
printtreenl (stderr, 4, LHSOf (n));
#endif
		}
		break;
		/*}}}*/
		/*{{{  VARIANT*/
	case S_VARIANT:
		/* for CASE inputs, variables in list are written */
		udv_locn = LocnOf (n);
		udv_defaultmode = EXP_WRITTEN;
		modprewalktree (VRTaggedListAddr (n), do_undefinedcheckexp, NULL);
		/* walk process */
		prewalkproctree (VRBodyOf (n), do_undefinedcheck, voidptr);
		break;
		/*}}}*/
		/*{{{  VARIANT*/
	case S_X_VARIANT:
		/* for extended CASE inputs, variables in list are written */
		udv_locn = LocnOf (n);
		udv_defaultmode = EXP_WRITTEN;
#if 0
fprintf (stderr, "udv: about to do_undefinedcheckexp on tagged list:");
printtreenl (stderr, 4, VRTaggedListOf (n));
#endif
		modprewalktree (VRTaggedListAddr (n), do_undefinedcheckexp, NULL);
		/* walk during process */
#if 0
fprintf (stderr, "udv: about to do_undefinedcheck on during process:");
printtreenl (stderr, 4, VRDuringOf (n));
#endif
		prewalkproctree (VRDuringOf (n), do_undefinedcheck, voidptr);
		/* reset XINPUTTED to INPUTTED, works because we've pushed the stack */
#if 0
fprintf (stderr, "udv: resetting XINPUTTED to INPUTTED in channel expression\n");
#endif
		udv_resetexp (udv_xcvar->nameof, UDV_XINPUTTED, UDV_INPUTTED);
#if 0
fprintf (stderr, "udv: X_VARIANT: done during process and reset exp, udv_xcvar->nameof =");
printtreenl (stderr, 4, udv_xcvar->nameof);
fprintf (stderr, "udv: about to do_undefinedcheck on after process:");
printtreenl (stderr, 4, VRAfterOf (n));
#endif
		prewalkproctree (VRAfterOf (n), do_undefinedcheck, voidptr);
#if 0
fprintf (stderr, "udv: oki, did all that, next variant.  udv_xcvar->nameof =");
printtreenl (stderr, 4, udv_xcvar->nameof);
#endif
		break;
		/*}}}*/
		/*{{{  DELAYED_INPUT*/
	case S_DELAYED_INPUT:
		udv_locn = LocnOf (n);
		udv_defaultmode = EXP_READ;
		modprewalktree (RHSAddr (n), do_undefinedcheckexp, NULL);
		break;
		/*}}}*/
		/*{{{  CASE*/
	case S_CASE:
		/* for CASE action-nodes (CASE x), all selection guards are constant */
		{
			treenode *list;
			const int firstlevel = udv_vstacklevel;

			/* check thing we're case-ing on */
			udv_locn = LocnOf (n);
			udv_defaultmode = EXP_READ;
			modprewalktree (LHSAddr (n), do_undefinedcheckexp, NULL);

			/* walk various branches */
			for (list = RHSOf (n); !EmptyList (list); list = NextItem (list)) {
				udv_pushstack ();
				vstack_copy (firstlevel, udv_vstacklevel, TRUE);
				prewalkproctree (ThisItem (list), do_undefinedcheck, voidptr);
			}

			/* merge back together */
			vstack_merge_if (udv_vstacklevel, firstlevel);
			udv_vstacklevel = firstlevel;
		}
		break;
		/*}}}*/
		/*{{{  SELECTION*/
	case S_SELECTION:
		/* for CASE action-nodes */
		prewalkproctree (CondBodyOf (n), do_undefinedcheck, voidptr);
		break;
		/*}}}*/
		/*{{{  WHILE*/
	case S_WHILE:
		/* check condition initially, then go inside WHILE loop */
		udv_locn = LocnOf (n);
		udv_defaultmode = EXP_READ;
		modprewalktree (CondGuardAddr (n), do_undefinedcheckexp, NULL);
		udv_pushstack ();
		vstack_copy (udv_vstacklevel - 1, udv_vstacklevel, TRUE);
		/* walk the body */
		prewalkproctree (CondBodyOf (n), do_undefinedcheck, voidptr);
		/* merge variable states together */
		vstack_merge_while (udv_vstacklevel, udv_vstacklevel - 1);
		udv_vstacklevel--;

		udv_locn = LocnOf (n);
		udv_defaultmode = EXP_READ;
		modprewalktree (CondGuardAddr (n), do_undefinedcheckexp, NULL);
		udv_pushstack ();
		vstack_copy (udv_vstacklevel - 1, udv_vstacklevel, TRUE);
		/* walk the body */
		prewalkproctree (CondBodyOf (n), do_undefinedcheck, voidptr);
		/* merge variable states together */
		vstack_merge_while (udv_vstacklevel, udv_vstacklevel - 1);
		udv_vstacklevel--;
		udv_fixup_defined (CondGuardAddr (n));				/* turns UDVCONSTEXP nodes into CONSTEXP, or removes them */
		break;
		/*}}}*/
		/*{{{  IF*/
	case S_IF:
		/* walk an IF statement */
		{
			treenode *list;
			const int firstlevel = udv_vstacklevel;

			/* modified handling -- process each guard then body as we go */
			for (list = CBodyOf (n); !EmptyList (list); list = NextItem (list)) {
				/* check the guards for undefined-ness (only reading) */
				treenode *guard = ThisItem (list);

				switch (TagOf (guard)) {
				case S_CHOICE:
					/* choice -- push vstack and check the guard (might contain DEFINED stuff) */
					udv_pushstack ();
					vstack_copy (firstlevel, udv_vstacklevel, TRUE);

					udv_locn = LocnOf (guard);
					udv_defaultmode = EXP_READ;
					modprewalktree (CondGuardAddr (guard), do_undefinedcheckexp, NULL);
					udv_fixup_defined (CondGuardAddr (guard));				/* turns UDVCONSTEXP nodes into CONSTEXP, or removes them */
#if 0
fprintf (stderr, "doundefinedcheck: IF: (before walking process) ThisItem (list) =");
printtreenl (stderr, 4, ThisItem (list));
#endif
					prewalkproctree (guard, do_undefinedcheck, voidptr);			/* doesn't check CondGuard -- see CHOICE below */
#if 0
fprintf (stderr, "doundefinedcheck: IF: (after walking process) ThisItem (list) =");
printtreenl (stderr, 4, ThisItem (list));
#endif
					break;
				default:
					prewalkproctree (guard, do_undefinedcheck, voidptr);			/* FIXME: need to handle IF/REPLIF properly */
					break;
				}
			}

			/* merge branches back into current */
			vstack_merge_if (udv_vstacklevel, firstlevel);
			udv_vstacklevel = firstlevel;
		}
		break;
		/*}}}*/
		/*{{{  CHOICE*/
	case S_CHOICE:
		/* walk inside body, but don't check guard (already done in IF/REPLIF handling) */
		prewalkproctree (CondBodyOf (n), do_undefinedcheck, voidptr);
		break;
		/*}}}*/
		/*{{{  ALT/PRIALT*/
	case S_ALT:
	case S_PRIALT:
#if 0
fprintf (stderr, "*** ALT: n =");
printtreenl (stderr, 4, n);
#endif
		/* walk an ALTernative */
		{
			treenode *list;
			const int firstlevel = udv_vstacklevel;

			/* modified code -- do it all in one go */
			for (list = CBodyOf (n); !EmptyList (list); list = NextItem (list)) {
				/* check the guard pre-conds for undefined-ness (only reading) */
				treenode *guard = ThisItem (list);
				udv_t *vsmarker = udv_vstack;

				/* add any leading specifications to the undefined variable stack */
				while (isspecification (guard)) {
					undefinedcheck_set_spec (guard, NULL, NULL);
					guard = DBodyOf (guard);
				}
				switch (TagOf (guard)) {
				case S_ALTERNATIVE:
					/* alternative -- push vstack and check the guard (might contain DEFINED stuff) */
					udv_pushstack ();
					vstack_copy (firstlevel, udv_vstacklevel, TRUE);

#if 0
fprintf (stderr, "do_undefinedcheck: ALT/PRIALT: ALTERNATIVE: guard =");
printtreenl (stderr, 4, guard);
#endif
					udv_locn = LocnOf (guard);
					udv_defaultmode = EXP_READ;
					if (AltGuardOf (guard)) {
						modprewalktree (AltGuardAddr (guard), do_undefinedcheckexp, NULL);
						udv_fixup_defined (AltGuardAddr (guard));
					}

					prewalkproctree (guard, do_undefinedcheck, voidptr);
					break;
				default:
					prewalkproctree (guard, do_undefinedcheck, voidptr);		/* FIXME: handle nested ALT/REPLALT/PRIALT/REPLPRIALT */
					break;
				}
				undefinedcheck_rewind (vsmarker);
			}

			/* merge back */
			vstack_merge_alt (udv_vstacklevel, firstlevel);
			udv_vstacklevel = firstlevel;
		}
		break;
		/*}}}*/
		/*{{{  ALTERNATIVE*/
	case S_ALTERNATIVE:
		/* walk inside, but don't check guard (pre-cond), since we already did it in the ALT above */
		prewalkproctree (AltInputOf (n), do_undefinedcheck, voidptr);
		prewalkproctree (AltBodyOf (n), do_undefinedcheck, voidptr);
		break;
		/*}}}*/
		/*{{{  PAR/PRIPAR/PLACEDPAR*/
	case S_PAR:
	case S_PRIPAR:
	case S_PLACEDPAR:
		/* walk inside PAR */
		if (!EmptyList (CBodyOf (n)) && !EmptyList (NextItem (CBodyOf (n)))) {
			treenode *list;
			const int firstlevel = udv_vstacklevel;

			for (list = CBodyOf (n); !EmptyList (list); list = NextItem (list)) {
				treenode *const process = ThisItem (list);

				udv_locn = LocnOf (process);
				udv_pushstack ();
				vstack_copy (firstlevel, udv_vstacklevel, FALSE);
				prewalkproctree (process, do_undefinedcheck, voidptr);
			}

			/* merge branches of PAR back into current */
			vstack_merge_par (udv_vstacklevel, firstlevel);
			udv_vstacklevel = firstlevel;
		}
		break;
		/*}}}*/
		/*{{{  REPLPAR/PRIREPLPAR/PLACEDREPLPAR*/
	case S_REPLPAR:
	case S_PRIREPLPAR:
	case S_PLACEDREPLPAR:
		{
			const int firstlevel = udv_vstacklevel;

			/* walk replicated PAR once, then merge down */
			udv_locn = LocnOf (n);
			udv_pushstack ();
			vstack_copy (firstlevel, udv_vstacklevel, FALSE);
			/* walk the body */
			prewalkproctree (ReplCBodyOf (n), do_undefinedcheck, voidptr);
			/* merge variable states together */
			vstack_merge_replpar (udv_vstacklevel, firstlevel);
			udv_vstacklevel = firstlevel;
		}
		break;
		/*}}}*/
		/*{{{  REPLSEQ/REPLDO*/
	case S_REPLSEQ:
	case S_REPLDO:
		/* check replicator bits */
		udv_locn = LocnOf (n);
		udv_defaultmode = EXP_READ;
		modprewalktree (ReplCStartExpAddr (n), do_undefinedcheckexp, NULL);
		modprewalktree (ReplCLengthExpAddr (n), do_undefinedcheckexp, NULL);
		if (ReplCStepExpOf (n)) {
			modprewalktree (ReplCStepExpAddr (n), do_undefinedcheckexp, NULL);
		}
		/* walk replicated SEQ twice */

		udv_pushstack ();
		vstack_copy (udv_vstacklevel - 1, udv_vstacklevel, TRUE);
		/* walk the body */
		prewalkproctree (ReplCBodyOf (n), do_undefinedcheck, voidptr);
		/* merge variable states together and copy for next */
		vstack_merge_replseq (udv_vstacklevel, udv_vstacklevel - 1, ReplCLengthExpOf (n));
		udv_vstacklevel--;

		/* need to remove any UNDEFINED nodes inserted -- will be correct the second time around */
		modprewalktree (ReplCBodyAddr (n), do_removeundefined, voidptr);

		udv_pushstack ();
		vstack_copy (udv_vstacklevel - 1, udv_vstacklevel, TRUE);
		/* walk the body */
		prewalkproctree (ReplCBodyOf (n), do_undefinedcheck, voidptr);
		/* merge variable states together and copy for next */
		vstack_merge_replseq (udv_vstacklevel, udv_vstacklevel - 1, ReplCLengthExpOf (n));
		udv_vstacklevel--;
		break;
		/*}}}*/
		/*{{{  REPLIF*/
	case S_REPLIF:
		/* check parts of the replicator and the replicated guard */
		udv_locn = LocnOf (n);
		udv_defaultmode = EXP_READ;
		modprewalktree (ReplCStartExpAddr (n), do_undefinedcheckexp, NULL);
		modprewalktree (ReplCLengthExpAddr (n), do_undefinedcheckexp, NULL);
		if (ReplCStepExpOf (n)) {
			modprewalktree (ReplCStepExpAddr (n), do_undefinedcheckexp, NULL);
		}
		if (TagOf (ReplCBodyOf (n)) == S_CHOICE) {
			treenode *guard = CondGuardOf (ReplCBodyOf (n));

			udv_locn = LocnOf (guard);
			udv_defaultmode = EXP_READ;
			modprewalktree (&guard, do_undefinedcheckexp, NULL);
		}
		/* walk the body */
		udv_pushstack ();
		vstack_copy (udv_vstacklevel - 1, udv_vstacklevel, TRUE);
		prewalkproctree (ReplCBodyOf (n), do_undefinedcheck, voidptr);
		/* merge variable states together */
		vstack_merge_replif (udv_vstacklevel, udv_vstacklevel - 1);
		udv_vstacklevel--;
		break;
		/*}}}*/
		/*{{{  REPLALT/PRIREPLALT*/
	case S_REPLALT:
	case S_PRIREPLALT:
		/* handle replicated ALT / PRI ALT */
		{
			treenode *alt;
			udv_t *vsmarker = udv_vstack;

			/* check the replicated guard */
			alt = ReplCBodyOf (n);
			while (isspecification (alt)) {
				undefinedcheck_set_spec (alt, NULL, NULL);
				alt = DBodyOf (alt);
			}
			if (TagOf (alt) == S_ALTERNATIVE) {
				udv_locn = LocnOf (alt);
				udv_defaultmode = EXP_READ;
				modprewalktree (AltGuardAddr (alt), do_undefinedcheckexp, NULL);
			}
			/* increment level and go inside */
			udv_pushstack ();
			vstack_copy (udv_vstacklevel - 1, udv_vstacklevel, TRUE);
			prewalkproctree (ReplCBodyOf (n), do_undefinedcheck, voidptr);
			/* merge states together */
			vstack_merge_replalt (udv_vstacklevel, udv_vstacklevel - 1);
			udv_vstacklevel--;
			undefinedcheck_rewind (vsmarker);
		}
		break;
		/*}}}*/
		/*{{{  FORKING*/
	case S_FORKING:
		prewalkproctree (CBodyOf (n), do_undefinedcheck, NULL);
		break;
		/*}}}*/
		/*{{{  GUY,ASM*/
	case S_GUY:
	case S_ASM:
		do_undefinedcheck_asm (CBodyOf (n));
		/*}}}*/
		/*{{{  default*/
	default:
#if 0
fprintf (stderr, "***  unhandled tag %d in do_undefinedcheck\n", tag);
#endif
		return CONTINUE_WALK;
		/*}}}*/
	}
	return STOP_WALK;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_undefinedcheck_tree (treenode *n, void *const voidptr)*/
/*
 *	performs undefined usage-cheking for PROCs and FUNCTIONs
 */
PRIVATEPARAM int do_undefinedcheck_tree (treenode *n, void *const voidptr)
{
	const int old = switch_to_real_workspace ();

	switch (TagOf (n)) {
	case S_VALABBR:
		{
			udv_t *vsmarker = udv_vstack;

			while (TagOf (n) == S_VALABBR) {
				undefinedcheck_set_spec (n, NULL, NULL);
				n = DBodyOf (n);
			}
			/* deal with the rest of it */
			prewalkproctree (n, do_undefinedcheck_tree, voidptr);
			undefinedcheck_rewind (vsmarker);
		}
		return STOP_WALK;
	case S_PROCDEF:
	case S_LFUNCDEF:
	case S_MPROCDECL:
	CASE_CONFIG_SPEC
		if (!separatelycompiled (DNameOf (n))) {
			treenode *list;
			udv_t *tmp;
			treenode *revlist = NULL;	/* used to store parameters so that we remove them in vstack order */
			treenode *revnextitem;

#if 0
fprintf (stderr, "use4: do_undefinedcheck_tree: walking [%s]\n", WNameOf (NNameOf (DNameOf (n))));
#endif
			syn_lexlevel++;
			/* only ever go 1 deep in here, nested PROCs are handled slightly differently */
			for (list = NParamListOf (DNameOf (n)); !EmptyList (list); list = NextItem (list)) {
				switch (TagOf (ThisItem (list))) {
				case N_PARAM:
				case N_VALPARAM:			/* need subtyping for these */
					/* could get undefined */
					tmp = newudv ();
					tmp->state[udv_vstacklevel] = UDV_DEFINED;
					tmp->nameof = ThisItem (list);
					tmp->did_warn = 0;
					tmp->next = udv_vstack;
#if 0
fprintf (stderr, "*** SetNUndef (%p, %p)\n", ThisItem (list), tmp);
#endif
					SetNUndef (ThisItem (list), tmp);
					udv_vstack = tmp;
					init_decl_subtype (NTypeOf (ThisItem (list)), tmp, UDV_DEFINED);
					revlist = newlistnode (S_LIST, NOPOSN, ThisItem (list), revlist);
					break;
				case N_RESULTPARAM:
					/* assume it starts off undefined */
					tmp = newudv ();
					tmp->state[udv_vstacklevel] = UDV_UNDEFINED;
					tmp->nameof = ThisItem (list);
					tmp->did_warn = 0;
					tmp->next = udv_vstack;
#if 0
fprintf (stderr, "*** SetNUndef (%p, %p)\n", ThisItem (list), tmp);
#endif
					SetNUndef (ThisItem (list), tmp);
					udv_vstack = tmp;
					init_decl_subtype (NTypeOf (ThisItem (list)), tmp, UDV_UNDEFINED);
					revlist = newlistnode (S_LIST, NOPOSN, ThisItem (list), revlist);
					break;
				}
			}
#if 0
fprintf (stderr, "*** scopein, before do_undefinedcheck:\n");
udv_dump_stack (stderr, 4, 0);
#endif
			prewalkproctree (DValOf (n), do_undefinedcheck, NULL);
#if 0
fprintf (stderr, "*** scopein, after do_undefinedcheck:\n");
udv_dump_stack (stderr, 4, 0);
#endif
			revnextitem = NULL;
			for (list = revlist; !EmptyList (list); list = revnextitem) {
				udv_udef_t local_undefined;
				revnextitem = NextItem (list);

				switch (TagOf (ThisItem (list))) {
				case N_PARAM:
				case N_VALPARAM:
					/* remove param */
					tmp = udv_vstack;
					udv_locn = NPEndposnOf (DNameOf (n));
					/* if the parameter is left undefined, make a note of this in the output by inserting an undefined node */
					/* generate warning */
					local_undefined.n_undefined = 0;
					local_undefined.n_unknown = 0;
					local_undefined.n_partial = 0;
					local_undefined.supress_warn = 1;
					modprewalktree (ThisItemAddr (list), do_undefinedcheckexp, &local_undefined);
					if (have_undefined (&local_undefined) && (TagOf (ThisItem (list)) == N_PARAM)) {
						/* we supressed warnings, if the undefined attribute is already present, and the parameter is mobile, it's user-specified */
						if (udv_thingisdynmobile (ThisItem (list)) || udv_thingisdynmobilechantype (ThisItem (list)) || udv_thingismobile (ThisItem (list))) {
							if (!(NTypeAttrOf (ThisItem (list)) & TypeAttr_undefined)) {
								modprewalktree (ThisItemAddr (list), do_undefinedcheckexp, NULL);
								SetNTypeAttr (ThisItem (list), NTypeAttrOf (ThisItem (list)) | TypeAttr_undefined);
							}
						} else {
							int btype = basetype (NTypeOf (ThisItem (list)));

							if ((btype != S_CHAN) && (btype != S_TIMER) && (btype != S_PORT)) {
								modprewalktree (ThisItemAddr (list), do_undefinedcheckexp, NULL);
							}
						}
					}
					udv_vstack = udv_vstack->next;
#if 0
fprintf (stderr, "*** SetNUndef (%p, NULL)\n", ThisItem (list));
#endif
					SetNUndef (ThisItem (list), NULL);
					free_decl_subtype (ThisItem (list), tmp);
					freeudv (tmp);
					/* free revlist node */
					freenode (&list);
					break;
				case N_RESULTPARAM:
					/* check that param is left defined and remove it */
					tmp = udv_vstack;
					udv_defaultmode = EXP_READ;
					/* want to get udv_locn = locn of the terminating: */
					udv_locn = NPEndposnOf (DNameOf (n));
					/* udv_locn = LocnOf (n); */
					modprewalktree (ThisItemAddr (list), do_undefinedcheckexp, NULL);
					udv_vstack = udv_vstack->next;
#if 0
fprintf (stderr, "*** SetNUndef (%p, NULL)\n", ThisItem (list));
#endif
					SetNUndef (ThisItem (list), NULL);
					free_decl_subtype (ThisItem (list), tmp);
					freeudv (tmp);
					/* free revlist node */
					freenode (&list);
					break;
				}
			}
			syn_lexlevel--;
		}
		break;
	}
	switch_to_prev_workspace (old);
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PUBLIC void undefinedcheck (treenode *n)*/
/*
 *	performs a check for undefinedness, calls do_undefinedcheck_tree for each PROC/FUNCTION
 */
PUBLIC void undefinedcheck (treenode *n)
{
	jmp_buf saved_env;

	memcpy ((char *)saved_env, (char *)env, sizeof (env));

	if (!op_ld) {
		op_ld = lookupword (".LD", 3);
		op_ldab = lookupword (".LDAB", 5);
		op_ldabc = lookupword (".LDABC", 6);
		op_ldl = lookupword (".LDL", 4);
		op_st = lookupword (".ST", 3);
		op_stab = lookupword (".STAB", 5);
		op_stabc = lookupword (".STABC", 6);
		op_stl = lookupword (".STL", 4);
		op_savel = lookupword (".SAVEL", 6);
	}
	if (setjmp (env) == 0) {
		prewalkproctree (n, do_undefinedcheck_tree, NULL);
		udv_gen_warnings ();
	}
	memcpy ((char *)env, (char *)saved_env, sizeof (env));
	flocn = NOPOSN;
	return;
}
/*}}}*/
/*{{{  PUBLIC char *use_udvstatestringof (treenode *const nptr)*/
/*
 *	returns a string representing the current state of `nptr'
 */
PUBLIC char *use_udvstatestringof (treenode *const nptr)
{
	udv_t *tmp = NUndefOf (nptr);

	if (!tmp || (tmp->state[udv_vstacklevel] < 0) || (tmp->state[udv_vstacklevel] >= UDV_INVALID)) {
		return udvstatestrings[UDV_INVALID];
	}
	if (tmp && tmp->nested) {
		return udvnstatestrings[(int)(tmp->state[udv_vstacklevel])];
	}
	return udvstatestrings[(int)(tmp->state[udv_vstacklevel])];
}
/*}}}*/

/*{{{  PUBLIC char *use_exprstring (treenode *exp)*/
/*
 *	public interface to treenode -> string name converter (handles fields/etc.)
 *	this returns a pointer to a local static buffer (subsequent calls will overwrite)
 */
PUBLIC char *use_exprstring (treenode *exp)
{
	static char str[128];
	int len;

	len = udv_strofexp (str, 127, exp);
	str[len] = '\0';
	return (char *)str;
}
/*}}}*/

