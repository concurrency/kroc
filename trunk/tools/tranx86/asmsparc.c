/*
 *	asmsparc.c - sparc assembly source outputter
 *	Copyright (C) 2004 Fred Barnes <frmb@kent.ac.uk>
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
#include "config.h"
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#include "postmortem.h"
#include "sparc.h"
#include "archdef.h"
#include "rtlops.h"
/*}}}*/

/*{{{  local prototypes*/
static char *modify_name (char *name);
static int drop_arg (ins_arg *arg, FILE *outstream);
static int disassemble_code (ins_chain *ins, FILE *outstream, int regtrace);
static int disassemble_data (unsigned char *bytes, int length, FILE *outstream);


#if defined(CAN_DO_DOTLABELS)
	#define LBLPFX ".L"
#else
	#define LBLPFX "L"
#endif
/*}}}*/
/*{{{  register names*/
static char *sparc_regs[] = {"%r0", "%g1", "%g2", "%g3", "%g4", "%g5", "%g6", "%g7",
				"%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%sp", "%o7",
				"%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",
				"%i0", "%i1", "%i2", "%i3", "%i4", "%i5", "%fp", "%i7", "%y"};
static char *sparc_fregs[] = {"%f0", "%f1", "%f2", "%f3", "%f4", "%f5", "%f6", "%f7"};
/*}}}*/

/*{{{  memory-debug related constants*/
#define LIVESIZE (4)

#define ADDR_INVALID	0x00
#define ADDR_TARGET	0x01
#define ADDR_SOURCE	0x02

static int dmcount;
/*}}}*/

/*{{{  static char *modify_name (char *name)*/
/*
 *	char *modify_name (char *name)
 *	modifies an outgoing name
 */
static char *modify_name (char *name)
{
	static char rbuf[128];
	int i, j, len;

	i = j = 0;
	len = strlen (name);
	for (i=0; i<len; i++) {
		switch (name[i]) {
		case '.':
			rbuf[j++] = '_';
			break;
		case '$':
			break;
		case '&':
			if (options.extref_prefix) {
				strcpy (&(rbuf[j]), options.extref_prefix);
				j += strlen (options.extref_prefix);
			}
			break;
		case '%':
			i = len;
			break;
		default:
			rbuf[j++] = name[i];
			break;
		}
	}
	rbuf[j] = '\0';
	if ((rbuf[0] != '_') && strncmp (rbuf, "O_", 2) && (name[0] != '&')) {
		/* prepend O_ */
		memmove (rbuf+2, rbuf, strlen(rbuf)+1);
		memcpy (rbuf, "O_", 2);
	}
	return rbuf;
}
/*}}}*/
/*{{{  static int drop_arg (ins_arg *arg, FILE *outstream)*/
/*
 *	int drop_arg (ins_arg *arg, FILE *outstream)
 *	disassembles a single argument
 */
static int drop_arg (ins_arg *arg, FILE *outstream)
{
	ins_sib_arg *t_sib;
	char *tptr;
	char **regset;

	if (arg->flags & ARG_IND) {
		fprintf (outstream, "*");
	}
	regset = sparc_regs;
	switch (arg->flags & ARG_MODEMASK) {
	case ARG_REG:
		fprintf (outstream, "%s", regset[arg->regconst]);
		break;
	case ARG_FREG:
		fprintf (outstream, "%s", sparc_fregs[arg->regconst]);
		break;
	case ARG_REGIND:
		fprintf (outstream, "[%s", regset[arg->regconst]);
		if (arg->flags & ARG_DISP) {
			if (arg->disp < 0) {
				fprintf (outstream, " - %d", -(arg->disp));
			} else {
				fprintf (outstream, " + %d", arg->disp);
			}
		}
		fprintf (outstream, "]");
		break;
	case ARG_CONST:
		fprintf (outstream, "%d", arg->regconst);
		break;
	case ARG_LABEL:
		fprintf (outstream, LBLPFX "%d", arg->regconst);
		if (arg->flags & ARG_DISP) {
			fprintf (outstream, " + %d", arg->disp);
		}
		break;
	case ARG_INSLABEL:
		fprintf (outstream, LBLPFX "%d", ((ins_chain *)arg->regconst)->in_args[0]->regconst);
		if (arg->flags & ARG_DISP) {
			fprintf (outstream, " + %d", arg->disp);
		}
		break;
	case ARG_FLABEL:
		fprintf (outstream, "%df", arg->regconst);
		if (arg->flags & ARG_DISP) {
			fprintf (outstream, " + %d", arg->disp);
		}
		break;
	case ARG_BLABEL:
		fprintf (outstream, "%db", arg->regconst);
		if (arg->flags & ARG_DISP) {
			fprintf (outstream, " + %d", arg->disp);
		}
		break;
	case ARG_NAMEDLABEL:
		fprintf (outstream, "%s", modify_name ((char *)arg->regconst));
		if (arg->flags & ARG_DISP) {
			fprintf (outstream, " + %d", arg->disp);
		}
		break;
	case ARG_TEXT:
		for (tptr = (char *)arg->regconst; (*tptr == ' ') || (*tptr == '\t'); tptr++);
		fprintf (outstream, "%s", tptr);
		break;
	case ARG_REGINDSIB:
		t_sib = (ins_sib_arg *)arg->regconst;
		fprintf (outstream, "(%s,%s,%d)", regset[t_sib->base], regset[t_sib->index], t_sib->scale);
		break;
	}
	return 0;
}
/*}}}*/
/*{{{  static void coder_verify_addr (ins_arg *arg, int mode, int *liveregs, FILE *outstream, int dmcount)*/
/*
 *	coder_verify_address -- called to verify an address
 */
static void coder_verify_addr (ins_arg *arg, int mode, int *liveregs, FILE *outstream, int dmcount)
{
	/* first, push all live registers */
#if 0
	fprintf (outstream, "\tpushf\n\tpushl\t%%ebp\n");
	for (i=0; i<LIVESIZE; i++) {
		if (liveregs[i] != -1024) {
			fprintf (outstream, "\tpushl\t%s\n", sparc_regs[liveregs[i]]);
		}
	}
#else
	fprintf (outstream, "\tpushf\n\tpusha\n");
#endif

	/* FIXME: Intel -> Sparc */
#if 0
	/* load effective address and call kernel -- blindly use %eax, safe.. */
	fprintf (outstream, "\tlea\t");
	drop_arg (arg, outstream);
	fprintf (outstream, ", %%eax\n");
	fprintf (outstream, "\tpushl\t%%eax\n");
	fprintf (outstream, "\tpushl\t$%d\n", (dmcount << 4) | (mode & 0x0f));
	fprintf (outstream, "\tcall\t_X_memchk\n");
#endif

	/* last, pop all live registers */
#if 0
	for (i=(LIVESIZE - 1); i >= 0; i--) {
		if (liveregs[i] != -1024) {
			fprintf (outstream, "\tpopl\t%s\n", sparc_regs[liveregs[i]]);
		}
	}
	fprintf (outstream, "\tpopl\t%%ebp\n\tpopf\n");
#else
	fprintf (outstream, "\tpopa\n\tpopf\n");
#endif
	return;
}
/*}}}*/
/*{{{  static int disassemble_code (ins_chain *ins, FILE *outstream, int regtrace)*/
/*
 *	int disassemble_code (ins_chain *ins, FILE *outstream, int regtrace)
 *	dissasembles some code
 */
static int disassemble_code (ins_chain *ins, FILE *outstream, int regtrace)
{
	static char *codes[] = {"..", "mov", "nop", "..", "set", "cmp.", "addcc.", "andcc.", "orcc.", "..", \
		"..", "..", "..", "..", "ret", "call", "..", "jmpl", "swap", "decl", "incl", "subcc.", "cwde", \
		"xorcc.", "smul", "rdtsc", "..", "cdq", "idiv", "sll", "srl", "movb", "not", "..", "..", "movzbl", \
		"rcrl", "rcll", "rorl", "roll", "addxcc.", "subxcc.", "umul.", "udiv", "shldl", "shrdl", "fstcw", "fldcw", \
		"wait", "fstp", "#", "fildl", "fxch", "fld", "fildd", "flds", "fldl", "fldt", "fadds", "faddl", \
		"fmuls", "fmull", "fstps", "fstpl", "fstpt", "fistps", "fistpl", "fld1", "fldl2t", "fldl2e", "fldpi", \
		"fldlg2", "fldln2", "fldz", "faddp", "fsubp", "fmulp", "fdivrp", "sahf", "ftst", "fxam", "fstsw", \
		"fucom", "fucomp", "fucompp", "fcom", "fcomp", "fcompp", "frndint", "fsqrt", "fabs", "fchs", "fscale", \
		"fprem1", "..", "movswl", "lahf", "..", "..", "..", "inb", "outb", "call"};
	static int code_sizes[] = {2, 3, 3, 2, 3, 4, 6, 6, 5, 2, \
		2, 2, 2, 2, 3, 4, 2, 4, 4, 4, 4, 6, 4, \
		6, 4, 5, 2, 3, 4, 3, 3, 4, 3, 2, 2, 6, \
		4, 4, 4, 4, 7, 7, 5, 4, 5, 5, 5, 5, \
		4, 4, 1, 4, 4, 3, 5, 4, 4, 4, 5, 5, \
		5, 5, 5, 5, 5, 6, 6, 4, 5, 6, 5, \
		6, 6, 4, 5, 5, 5, 6, 4, 4, 4, 5, \
		5, 6, 7, 4, 5, 6, 7, 5, 4, 4, 6, \
		6, 2, 6, 4, 2, 2, 2, 3, 4, 4};
	static char *setcc_tailcodes[] = {"vs", "vc", "cs", "cc", "e", "ne", "leu", "gu", "neg", "pos", "..", "..", "l", "ge", "le", "g", "a", "n"};
	ins_chain *tmp;
	ins_arg *arg;
	int i, tlab1, tlab2;
	char *tptr;
	char wbuf[16];

	int live_vregs[LIVESIZE] = {-1024, -1024, -1024, -1024};		/* make sure it's well out of the way.. */
	int live_rregs[LIVESIZE] = {-1024, -1024, -1024, -1024};

	if (sizeof (codes) != sizeof (code_sizes)) {
		fprintf (stderr, "%s: fatal: assertation (sizeof(codes) == sizeof(code_sizes)) (%d != %d) failed\n", progname, (int) sizeof (codes), (int) sizeof (code_sizes));
		return -1;
	}
	for (tmp=ins; tmp; tmp=tmp->next) {
		if (regtrace) {
			int j;

			switch (tmp->type) {
			case INS_START_REG:
				for (j=0; (j<LIVESIZE) && (live_vregs[j] != -1024); j++);
				if (j == LIVESIZE) {
					fprintf (stderr, "%s: fatal: register trace strangeness (1).\n", progname);
					return -1;
				}
				if (!tmp->out_args[0]) {
					fprintf (stderr, "%s: fatal: missing output register on START.\n", progname);
					return -1;
				}
				live_vregs[j] = tmp->in_args[0]->regconst;
				live_rregs[j] = tmp->out_args[0]->regconst;
				break;
			case INS_END_REG:
				for (j=0; (j<LIVESIZE) && (live_vregs[j] != tmp->in_args[0]->regconst); j++);
				if (j == LIVESIZE) {
					fprintf (stderr, "%s: fatal: register trace strangeness (2).\n", progname);
					return -1;
				}
				live_vregs[j] = -1024;
				live_rregs[j] = -1024;
				break;
			}
		}
		if ((tmp->type > INS_LAST) || (tmp->type < 0)) {
			continue;
		}
		if ((options.debug_options & DEBUG_MEMCHK)) {
			switch (tmp->etc_type) {
			case EtcPrimary (I_STNL):
				/* verify target address */
				dmcount++;
				fprintf (outstream, ";! verify target address %d\n", dmcount);
				coder_verify_addr (tmp->out_args[0], ADDR_TARGET, live_rregs, outstream, dmcount);
				break;
			case EtcPrimary (I_LDNL):
				/* verify source address */
				dmcount++;
				fprintf (outstream, ";! verify source address %d\n", dmcount);
				coder_verify_addr (tmp->in_args[0], ADDR_SOURCE, live_rregs, outstream, dmcount);
				break;
			case EtcSpecial (I_XSTL):
			case EtcSpecial (I_XSTLN):
				/* verify target address */
				dmcount++;
				fprintf (outstream, ";! verify target address %d\n", dmcount);
				coder_verify_addr (tmp->out_args[0], ADDR_TARGET | ADDR_SOURCE, live_rregs, outstream, dmcount);
				break;
			}
		}
		switch (tmp->type) {
		case INS_SETLABEL:
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ":\n");
			break;
		case INS_SETFLABEL:
			/* (very) local label */
			fprintf (outstream, "%d:\n", tmp->in_args[0]->regconst);
			break;
		case INS_LOADLABDIFF:
		case INS_CONSTLABDIFF:
			if ((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_INSLABEL) {
				tlab1 = ((ins_chain *)tmp->in_args[0]->regconst)->in_args[0]->regconst;
			} else {
				tlab1 = tmp->in_args[0]->regconst;
			}
			if ((tmp->in_args[1]->flags & ARG_MODEMASK) == ARG_INSLABEL) {
				tlab2 = ((ins_chain *)tmp->in_args[1]->regconst)->in_args[0]->regconst;
			} else {
				tlab2 = tmp->in_args[1]->regconst;
			}
			if (tmp->type == INS_LOADLABDIFF) {
				fprintf (outstream, "\tmov\t(" LBLPFX "%d - " LBLPFX "%d), %s\n", tlab1, tlab2, sparc_regs[tmp->out_args[0]->regconst]);
			} else {
				fprintf (outstream, ".long (" LBLPFX "%d - " LBLPFX "%d)\n", tlab1, tlab2);
			}
			break;
		case INS_CONSTLABADDR:
			if ((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_INSLABEL) {
				tlab1 = ((ins_chain *)tmp->in_args[0]->regconst)->in_args[0]->regconst;
			} else {
				tlab1 = tmp->in_args[0]->regconst;
			}
			fprintf (outstream, ".long " LBLPFX "%d\n", tlab1);
			break;
		case INS_CJUMP:
			fprintf (outstream, "\tb%s\t", setcc_tailcodes[tmp->in_args[0]->regconst]);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			fprintf (outstream, "\tnop\n");			/* delay slot */
			break;
#if 0
		case INS_CMOVE:
			fprintf (outstream, "\tcmov%s\t", setcc_tailcodes[tmp->in_args[0]->regconst]);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_SETCC:
			fprintf (outstream, "\t%s%s\t", codes[tmp->type], setcc_tailcodes[tmp->in_args[0]->regconst]);
			if ((tmp->out_args[0]->flags & ARG_MODEMASK) == ARG_REG) {
				if (tmp->out_args[0]->regconst >= 4) {
					fprintf (stderr, "%s: error: low 8-bit register %d unacceptable! (<4)\n", progname, tmp->out_args[0]->regconst);
				} else {
					fprintf (outstream, "%s", sparc_lregs[tmp->out_args[0]->regconst]);
				}
			} else {
				drop_arg (tmp->out_args[0], outstream);
			}
			fprintf (outstream, "\n");
			break;
#endif
		case INS_ADD:
		case INS_SUB:
		case INS_OR:
		case INS_AND:
		case INS_XOR:
		case INS_RCR:
		case INS_RCL:
		case INS_ROR:
		case INS_ROL:
		case INS_ADC:
		case INS_SBB:
		case INS_UMUL:
			/* 2-1 things */
			strcpy (wbuf, codes[tmp->type]);
			tptr = wbuf + (code_sizes[tmp->type] - 1);
#if 0
			switch (rtl_instr_width (tmp)) {
			case 8:
				*tptr = 'b';
				break;
			case 16:
				*tptr = 'h';
				break;
			case 32:
				*tptr = '\0';
				break;
			}
#else
			*tptr = '\0';		/* always 32-bit operation */
#endif
			fprintf (outstream, "\t%s\t", wbuf);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_CMP:
			/* need to handle 8/16/32 bits correctly */
			strcpy (wbuf, codes[tmp->type]);
			tptr = wbuf + (code_sizes[tmp->type] - 1);
			switch (rtl_instr_width (tmp)) {
			case 8:
				*tptr = 'b';
				break;
			case 16:
				*tptr = 'h';
				break;
			case 32:
				*tptr = '\0';
				break;
			}
			fprintf (outstream, "\t%s\t", wbuf);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_MUL:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_SHLD:
		case INS_SHRD:
			/* 3-arg encoding & 8-bit register */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			if ((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_REG) {
				if (tmp->in_args[0]->regconst != REG_L2) {
					fprintf (stderr, "%s: error: SHLD/SHRD shift register %d unacceptable (!cl)\n", progname, tmp->in_args[0]->regconst);
				} else {
					fprintf (outstream, "%s", sparc_regs[tmp->in_args[0]->regconst]);
				}
			} else {
				drop_arg (tmp->in_args[0], outstream);
			}
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[2], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_SHR:
		case INS_SHL:
			/* 8-bit register */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_INC:
		case INS_DEC:
			fprintf (stderr, "%s: error: unexpected INS/DEC for Sparc\n", progname);
			break;
		case INS_NOT:
			/* 1-1 things */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_DIV:
			/* akward one */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[2], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_UDIV:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[2], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->out_args[1], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_FSUB:
			fprintf (outstream, "\tfsubrp\t%%st, %%st(1)\n");
			break;
		case INS_MOVE:
			if (((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_REG) && ((tmp->out_args[0]->flags & ARG_MODEMASK) == ARG_REG)) {
				/* register - register move */
				fprintf (outstream, "\tmov\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "\n");
			} else if (((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_CONST) && ((tmp->out_args[0]->flags & ARG_MODEMASK) == ARG_REG)) {
				/* load constant into register */
				if ((tmp->in_args[0]->regconst < (1 << 12)) && (tmp->in_args[0]->regconst >= -(1 << 12))) {
					fprintf (outstream, "\tmov\t");
				} else {
					fprintf (outstream, "\tset\t");
				}
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "\n");
			} else if ((((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_LABEL) ||
						((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_NAMEDLABEL) ||
						((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_INSLABEL) ||
						((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_FLABEL) ||
						((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_BLABEL)
					) && (tmp->in_args[0]->flags & ARG_ISCONST) && ((tmp->out_args[0]->flags & ARG_MODEMASK) == ARG_REG)) {
				/* load label address */
				fprintf (outstream, "\tset\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "\n");
			} else if ((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_REG) {
				/* store register */
				fprintf (outstream, "\tst\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "\n");
			} else if ((tmp->out_args[0]->flags & ARG_MODEMASK) == ARG_REG) {
				/* load register */
				fprintf (outstream, "\tld\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "\n");
			} else {
				fprintf (stderr, "%s: error: unhandled move!\n", progname);
			}
			break;
		case INS_MOVEB:
			/* register source/dest is 8-bit */
			if ((ArgMode (tmp->in_args[0]) == ARG_REG) && (ArgMode (tmp->out_args[0]) == ARG_REGIND)) {
				/* store byte */
				fprintf (outstream, "\tstb\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "\n");
			} else if ((ArgMode (tmp->in_args[0]) == ARG_REGIND) && (ArgMode (tmp->out_args[0]) == ARG_REG)) {
				/* load byte */
				fprintf (outstream, "\tldub\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "\n");
			} else if ((ArgMode (tmp->in_args[0]) == ARG_REG) && (ArgMode (tmp->out_args[0]) == ARG_REG)) {
				/* move byte */
				fprintf (outstream, "\tmov\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "\n");
			} else {
				fprintf (stderr, "%s: error: unhandled moveb!\n", progname);
			}
			break;
		case INS_REPMOVEB:
		case INS_REPMOVEL:
			fprintf (stderr, "%s: error: unhandled replicated move!\n", progname);
			break;
		case INS_ANNO:
			fprintf (outstream, ";! ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_SWAP:
			/* unusual 2-2 */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_JUMP:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", %%r0\n");
			fprintf (outstream, "\tnop\n");		/* delay slot */
			break;
		case INS_FSTSW:
			/* 16-bit register.. */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			if ((tmp->out_args[0]->flags & ARG_MODEMASK) == ARG_REG) {
				if (tmp->out_args[0]->regconst != REG_L0) {
					fprintf (stderr, "%s: error: fstsw register not AX (was %d)\n", progname, tmp->out_args[0]->regconst);
				}
				fprintf (outstream, "%%ax");
			} else {
				drop_arg (tmp->out_args[0], outstream);
			}
			fprintf (outstream, "\n");
			break;
		case INS_CALL:
			fprintf (stderr, "%s: error: unexpected CALL\n", progname);
			break;
		case INS_CCALL:
			/* this call allowed */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", 0\n");
			fprintf (outstream, "\tnop\n");		/* delay slot */
			break;
		case INS_KCALL:
			/* and this one too -- special */
			fprintf (outstream, "\tcall\t");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			/* delay slot */
			if (tmp->in_args[1] && (ArgMode (tmp->in_args[1]) == ARG_CONST) && (ArgConst (tmp->in_args[1]))) {
				fprintf (outstream, "\tadd\t%%o7, ");
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", %%o7\n");
			} else {
				fprintf (outstream, "\tnop\n");
			}
			break;
		case INS_PJUMP:
			/* jump to somewhere, but poll scheduler and see if a reschedule is needed.  
			 * The synch-flags 0(sf) and Tptr 4(sf) are checked for activity.
			 * We know that (a) the tstack is empty and (b) we have two/three words below Wptr for stuff */
			if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
				fprintf (outstream, ";! generating PJUMP code\n");
			#if 0	/* CMPXCHG8B version */
				fprintf (outstream, "\tmovl\t$0, %%eax\n");
				fprintf (outstream, "\tmovl\t$0, %%ebx\n");
				fprintf (outstream, "\tmovl\t$0, %%ecx\n");
				fprintf (outstream, "\tmovl\t$0, %%edx\n");
				fprintf (outstream, "\tcmpxchg8b\tsf\n");
				fprintf (outstream, "\tjz\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				fprintf (outstream, ";! reschedule code\n");
				fprintf (outstream, "\tmovl\t$");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", -4(%%ebp)\n");
				fprintf (outstream, "\tjmp\t_X_occscheduler_q\n");
			#else	/* individual compare version */
				fprintf (outstream, "\tset\tsf, %%l1\n");
				fprintf (outstream, "\tld\t[%%fp], %%l0\n");
				fprintf (outstream, "\tld\t[%%l1], %%l1\n");
				fprintf (outstream, "\tor\t%%l0, %%l1, %%l0\n");
				if (options.pause_at_loopend == 2) {
					/* check the run-queue for activity too */
					fprintf (outstream, "\tor\t%%l0, %%l6, %%l0\n");
				}
				fprintf (outstream, "\tbe\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				fprintf (outstream, ";! reschedule code\n");
				fprintf (outstream, "\tst\t$");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", [%%fp - 4]\n");
				fprintf (outstream, "\tjmpl\t_X_occscheduler_q, %%r0\n");
			#endif
			} else {
				/* dunno for this, just spit regular jump */
				fprintf (stderr, "%s: warning: pjump stepped back to regular jump\n", progname);
				fprintf (outstream, "\t%s\t", codes[INS_JUMP]);
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
			}
			break;
		default:
			/* anything else */
			if (!strcmp (codes[tmp->type], "..")) {
				fprintf (outstream, "\t..%d..\t", tmp->type);
			} else {
				fprintf (outstream, "\t%s\t", codes[tmp->type]);
			}
			for (i=0; tmp->in_args[i]; i++) {
				arg = tmp->in_args[i];
				if (arg->flags & ARG_IMP) {
					continue;
				}
				drop_arg (arg, outstream);
				if (tmp->in_args[i+1] || tmp->out_args[0]) {
					fprintf (outstream, ", ");
				}
			}
			for (i=0; tmp->out_args[i]; i++) {
				arg = tmp->out_args[i];
				if (arg->flags & ARG_IMP) {
					continue;
				}
				drop_arg (arg, outstream);
				if (tmp->out_args[i+1]) {
					fprintf (outstream, ", ");
				}
			}
			fprintf (outstream, "\n");
			break;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static int disassemble_data (unsigned char *bytes, int length, FILE *outstream)*/
/*
 *	int disassemble_data (unsigned char *bytes, int length, FILE *outstream)
 *	disassembles some data
 */
static int disassemble_data (unsigned char *bytes, int length, FILE *outstream)
{
	int i;

	for (i=0; i<length; i++) {
		if (!(i % 8)) {
			if (i > 0) {
				fprintf (outstream, "\n");
			}
			fprintf (outstream, "\t.byte\t");
		}
		fprintf (outstream, "0x%2.2x%s", bytes[i], (((i % 8) == 7) || (i==(length-1))) ? " " : ", ");
	}
	fprintf (outstream, "\n");
	return 0;
}
/*}}}*/
/*{{{  int dump_asmsparc_stream (rtl_chain *rtl_code, FILE *stream)*/
/*
 *	int dump_asmsparc_stream (rtl_chain *rtl_code, FILE *stream)
 *	dumps sparc assembly source to a stream (here and in elf via arch)
 */
int dump_asmsparc_stream (rtl_chain *rtl_code, FILE *stream)
{
	rtl_chain *tmp;
	int seen_data;
	int errored;

	seen_data = 0;
	errored = 0;
	for (tmp=rtl_code; tmp && !errored; tmp=tmp->next) {
		switch (tmp->type) {
		case RTL_SOURCEFILE:
			fprintf (stream, "\t;! sourcefile \"%s\"\n", tmp->u.sourcefile);
			break;
		case RTL_CODE:
			errored = disassemble_code (tmp->u.code.head, stream, (options.debug_options & DEBUG_MEMCHK));
			break;
		case RTL_DATA:
			/* data can be implanted in code */
			errored = disassemble_data ((unsigned char *)tmp->u.data.bytes, tmp->u.data.length, stream);
			break;
		case RTL_RDATA:
			if (!seen_data) {
				fprintf (stream, ".data\n");
				seen_data = 1;
			}
			fprintf (stream, LBLPFX "%d:\n", tmp->u.rdata.label);
			errored = disassemble_data ((unsigned char *)tmp->u.rdata.bytes, tmp->u.rdata.length, stream);
			break;
		case RTL_XDATA:
			errored = disassemble_data ((unsigned char *)tmp->u.xdata.bytes, tmp->u.xdata.length, stream);
			break;
		case RTL_SETNAMEDLABEL:
			fprintf (stream, "%s:\n", modify_name (tmp->u.label_name));
			break;
		case RTL_STUBIMPORT:
			fprintf (stream, "\tjmp\t%s\n", modify_name (tmp->u.label_name));
			break;
		case RTL_PUBLICSETNAMEDLABEL:
			{
				const char *label = modify_name (tmp->u.label_name);

				fprintf (stream, ".globl %s\n", label);
				fprintf (stream, ".type %s, @function\n", label);
				fprintf (stream, "%s:\n", label);
			}
			break;
		case RTL_PUBLICENDNAMEDLABEL:
			{
				const char *label = modify_name (tmp->u.label_name);

				fprintf (stream, ".size %s, . - %s\n", label, label);
			}
			break;
		case RTL_ALIGN:
			fprintf (stream, "\n.align %d\n", (1 << tmp->u.alignment));
			break;
		case RTL_WSVS:
			if (!seen_data) {
				fprintf (stream, ".data\n");
				seen_data = 1;
			}
			fprintf (stream, ".globl %s_wsbytes\n", (options.extref_prefix ? options.extref_prefix : ""));
			fprintf (stream, "%s_wsbytes: .long %d\n", (options.extref_prefix ? options.extref_prefix : ""), tmp->u.wsvs.ws_bytes);
			fprintf (stream, ".globl %s_wsadjust\n", (options.extref_prefix ? options.extref_prefix : ""));
			fprintf (stream, "%s_wsadjust: .long %d\n", (options.extref_prefix ? options.extref_prefix : ""), tmp->u.wsvs.ws_adjust);
			fprintf (stream, ".globl %s_vsbytes\n", (options.extref_prefix ? options.extref_prefix : ""));
			fprintf (stream, "%s_vsbytes: .long %d\n", (options.extref_prefix ? options.extref_prefix : ""), tmp->u.wsvs.vs_bytes);
			fprintf (stream, ".globl %s_msbytes\n", (options.extref_prefix ? options.extref_prefix : ""));
			fprintf (stream, "%s_msbytes: .long %d\n", (options.extref_prefix ? options.extref_prefix : ""), tmp->u.wsvs.ms_bytes);
			break;
		case RTL_UNDEFINED:
		case RTL_COMMENT:
		case RTL_CODELINE:
		case RTL_MESSAGE:
		case RTL_DYNCODEENTRY:
			break;
		case RTL_CODEMAP:
			break;
		}
	}
	return errored;
}
/*}}}*/
/*{{{  int dump_asmsparc (rtl_chain *rtl_code, char *filename)*/
/*
 *	int dump_asmsparc (rtl_chain *rtl_code, char *filename)
 *	dumps sparc assembly source
 */
int dump_asmsparc (rtl_chain *rtl_code, char *filename)
{
	FILE *outstream;
	int result;

	dmcount = 0;
	outstream = fopen (filename, "w");
	if (!outstream) {
		fprintf (stderr, "%s: error: unable to open %s for writing\n", progname, filename);
		return -1;
	}
	result = dump_asmsparc_stream (rtl_code, outstream);
	fclose (outstream);
	return result;
}
/*}}}*/

