/*
 *	asm386.c - i386 assembly source outputter
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

/*{{{  includes*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

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
#include "intel.h"
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
static char *intel_regs[] = {"%eax", "%ecx", "%edx", "%ebx", "%esp", "%ebp", "%esi", "%edi"};
static char *intel_fregs[] = {"%st(0)", "%st(1)", "%st(2)", "%st(3)", "%st(4)", "%st(5)", "%st(6)", "%st(7)"};
static char *intel_lregs[] = {"%al", "%cl", "%dl", "%bl", "%bad_l0", "%bad_l1", "%bad_l2", "%bad_l3"};
static char *intel_hregs[] = {"%ah", "%ch", "%dh", "%bh", "%bad_h0", "%bad_h1", "%bad_h2", "%bad_h3"};
static char *intel_xregs[] = {"%ax", "%cx", "%dx", "%bx", "%sp", "%bp", "%si", "%di"};
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
 *	modifies an outgoing name
 *	NOTE: this returns a pointer into a static area.
 */
static char *modify_name (char *name)
{
#define MAXNAME 128
	static char rbuf[MAXNAME];
	int i, j, len;

	j = 0;
	len = strlen (name);
	for (i=0; i<len; i++) {
		switch (name[i]) {
		case '.':
			rbuf[j++] = '_';
			break;
		case '$':
		case '^':
		case '*':
		case '@':
		case '&':
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

	const char *prepend = NULL;
	if ((rbuf[0] == '_') || (!strncmp (rbuf, "O_", 2)) || (!strncmp (rbuf, "DCR_", 4)) || (name[0] == '&') || (name[0] == '@')) {
		/* skip */
	} else if (name[0] == '^') {
		prepend = "E_";
	} else if (name[0] == '*') {
		prepend = "M_";
	} else {
		prepend = "O_";
	}
	if (prepend) {
		int plen = strlen(prepend);
		memmove (rbuf + plen, rbuf, strlen(rbuf) + 1);
		memcpy (rbuf, prepend, plen);
	}

	if (options.extref_prefix && (prepend || rbuf[0] == '&')) {
		int plen = strlen(options.extref_prefix);
		memmove (rbuf + plen, rbuf, strlen(rbuf) + 1);
		memcpy (rbuf, options.extref_prefix, plen);
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
	if (arg->flags & ARG_DISP) {
		fprintf (outstream, "%d", arg->disp);
	}
	if (arg->flags & ARG_ISCONST) {
		fprintf (outstream, "$");
	}
	if (arg->flags & ARG_IS8BIT) {
		regset = intel_lregs;
	} else if (arg->flags & ARG_IS16BIT) {
		regset = intel_xregs;
	} else {
		regset = intel_regs;
	}
	switch (arg->flags & ARG_MODEMASK) {
	case ARG_REG:
		fprintf (outstream, "%s", (arg->regconst < 0) ? "??" : regset[arg->regconst]);
		break;
	case ARG_FREG:
		fprintf (outstream, "%s", (arg->regconst < 0) ? "??" : intel_fregs[arg->regconst]);
		break;
	case ARG_REGIND:
		fprintf (outstream, "(%s)", (arg->regconst < 0) ? "??" : regset[arg->regconst]);
		break;
	case ARG_CONST:
		fprintf (outstream, "%d", arg->regconst);
		break;
	case ARG_LABEL:
		fprintf (outstream, LBLPFX "%d", arg->regconst);
		break;
	case ARG_INSLABEL:
		fprintf (outstream, LBLPFX "%d", ((ins_chain *)arg->regconst)->in_args[0]->regconst);
		break;
	case ARG_FLABEL:
		fprintf (outstream, "%df", arg->regconst);
		break;
	case ARG_BLABEL:
		fprintf (outstream, "%db", arg->regconst);
		break;
	case ARG_NAMEDLABEL:
		fprintf (outstream, "%s", modify_name ((char *)arg->regconst));
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
	fprintf (outstream, "\tpushf\n\tpusha\n");

	/* load effective address and call kernel -- blindly use %eax, safe.. */
	fprintf (outstream, "\tlea\t");
	drop_arg (arg, outstream);
	fprintf (outstream, ", %%eax\n");
	fprintf (outstream, "\tmovl\t$%d, %%ebx\n", (dmcount << 4) | (mode & 0x0f));
	fprintf (outstream, "\tmovl\t$0f, %%ecx\n");
	fprintf (outstream, "\tjmp\t_X_memchk\n");
	fprintf (outstream, "0:\n");

	/* last, pop all live registers */
	fprintf (outstream, "\tpopa\n\tpopf\n");
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
	static char *codes[] = {"..", "movl", "nop", "lea", "set", "cmpl", "addl", "andl", "orl", "into", \
		"..", "..", "pushl", "popl", "ret", "call", "..", "jmp", "xchgl", "decl", \
		"incl", "subl", "cwde", "xorl", "imul", "rdtsc", "..", "cdq", "idiv", "shl", \
		"shr", "movb", "not", "..", "..", "movzbl", "rcrl", "rcll", "rorl", "roll", \
		"adcl", "sbbl", "mul", "div", "shldl", "shrdl", "fstcw", "fldcw", "wait", "fstp", \
		"#", "fildl", "fxch", "fld", "fildd", "flds", "fldl", "fldt", "fadds", "faddl", \
		"fmuls", "fmull", "fstps", "fstpl", "fstpt", "fistps", "fistpl", "fld1", "fldl2t", "fldl2e", \
		"fldpi", "fldlg2", "fldln2", "fldz", "faddp", "fsubp", "fmulp", "fdivrp", "sahf", "ftst", \
		"fxam", "fstsw", "fucom", "fucomp", "fucompp", "fcom", "fcomp", "fcompp", "frndint", "fsqrt", \
		"fabs", "fchs", "fscale", "fprem1", "..", "movswl", "lahf", "..", "..", "..", \
		"inb", "outb", "call", "movzwl", "movw", "..", "..", "..", "..", "fsin", \
		"fcos", "inw", "outw", "inl", "outl", "lock", "fptan", "mfence", "lfence", "sfence"};
	static int code_sizes[] = {2, 4, 3, 3, 3, 4, 4, 4, 3, 4, \
		2, 2, 5, 4, 3, 4, 2, 3, 5, 4, \
		4, 4, 4, 4, 4, 5, 2, 3, 4, 3, \
		3, 4, 3, 2, 2, 6, 4, 4, 4, 4, \
		4, 4, 3, 3, 5, 5, 5, 5, 4, 4, \
		1, 4, 4, 3, 5, 4, 4, 4, 5, 5, \
		5, 5, 5, 5, 5, 6, 6, 4, 5, 6, \
		5, 6, 6, 4, 5, 5, 5, 6, 4, 4, \
		4, 5, 5, 6, 7, 4, 5, 6, 7, 5, \
		4, 4, 6, 6, 2, 6, 4, 2, 2, 2, \
		3, 4, 4, 6, 4, 2, 2, 2, 2, 4, \
		4, 3, 4, 3, 4, 4, 5, 7, 7, 7};
	static char *setcc_tailcodes[] = {"o", "no", "b", "ae", "e", "nz", "be", "a", "s", "ns", "pe", "po", "l", "ge", "le", "g", "..", ".."};
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
				fprintf (outstream, "# verify target address %d\n", dmcount);
				coder_verify_addr (tmp->out_args[0], ADDR_TARGET, live_rregs, outstream, dmcount);
				break;
			case EtcPrimary (I_LDNL):
				/* verify source address */
				dmcount++;
				fprintf (outstream, "# verify source address %d\n", dmcount);
				coder_verify_addr (tmp->in_args[0], ADDR_SOURCE, live_rregs, outstream, dmcount);
				break;
			case EtcSpecial (I_XSTL):
			case EtcSpecial (I_XSTLN):
				/* verify target address */
				dmcount++;
				fprintf (outstream, "# verify target address %d\n", dmcount);
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
				fprintf (outstream, "\tmovl\t$(" LBLPFX "%d - " LBLPFX "%d), %s\n", tlab1, tlab2, intel_regs[tmp->out_args[0]->regconst]);
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
			fprintf (outstream, "\tj%s\t", setcc_tailcodes[tmp->in_args[0]->regconst]);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			break;
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
					fprintf (outstream, "%s", intel_lregs[tmp->out_args[0]->regconst]);
				}
			} else {
				drop_arg (tmp->out_args[0], outstream);
			}
			fprintf (outstream, "\n");
			break;
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
			/* 2-1 things */
			strcpy (wbuf, codes[tmp->type]);
			tptr = wbuf + (code_sizes[tmp->type] - 1);
			switch (rtl_instr_width (tmp)) {
			case 8:
				*tptr = 'b';
				break;
			case 16:
				*tptr = 'w';
				break;
			case 32:
				*tptr = 'l';
				break;
			}
			fprintf (outstream, "\t%s\t", wbuf);
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
				*tptr = 'w';
				break;
			case 32:
				*tptr = 'l';
				break;
			}
			fprintf (outstream, "\t%s\t", wbuf);
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_MUL:
			/* various forms of this */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			if (tmp->in_args[1] && !(tmp->in_args[1]->flags & ARG_IMP)) {
				/* two-sources */
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[1], outstream);
				if (tmp->out_args[0] && !(tmp->out_args[0]->flags & ARG_IMP)) {
					/* one-destination */
					if (tmp->in_args[1]->regconst != tmp->out_args[0]->regconst) {
						fprintf (outstream, ", ");
						drop_arg (tmp->out_args[0], outstream);
					}
				} /* else r32 destination */
			} /* else D:A = r/m32 * A */
			fprintf (outstream, "\n");
			break;
		case INS_SHLD:
		case INS_SHRD:
			/* 3-arg encoding & 8-bit register */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			if ((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_REG) {
				if (tmp->in_args[0]->regconst != REG_ECX) {
					fprintf (stderr, "%s: error: SHLD/SHRD shift register %d unacceptable (!cl)\n", progname, tmp->in_args[0]->regconst);
				} else {
					fprintf (outstream, "%s", intel_lregs[tmp->in_args[0]->regconst]);
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
			if ((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_REG) {
				if (tmp->in_args[0]->regconst != REG_ECX) {
					fprintf (stderr, "%s: error: SHL/SHR shift register %d unacceptable (!cl)\n", progname, tmp->in_args[0]->regconst);
				} else {
					fprintf (outstream, "%s", intel_lregs[tmp->in_args[0]->regconst]);
				}
			} else {
				drop_arg (tmp->in_args[0], outstream);
			}
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_INC:
		case INS_DEC:
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
		case INS_UMUL:
			switch (tmp->in_args[1]->flags & ARG_MODEMASK) {
			case ARG_NAMEDLABEL:
			case ARG_LABEL:
				fprintf (outstream, "\t%sl\t", codes[tmp->type]);
				break;
			default:
				fprintf (outstream, "\t%s\t", codes[tmp->type]);
				break;
			}
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_UDIV:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[2], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_FSUB:
			fprintf (outstream, "\tfsubrp\t%%st, %%st(1)\n");
			break;
		case INS_MOVEB:
			/* register source/dest is 8-bit */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			switch ((tmp->in_args[0]->flags & ARG_MODEMASK)) {
			case ARG_REG:
				if (tmp->in_args[0]->regconst >= 4) {
					fprintf (stderr, "%s: error: low 8-bit register %d unacceptable! (<4)\n", progname, tmp->in_args[0]->regconst);
				} else {
					if (tmp->in_args[0]->flags & ARG_IS8HIGH) {
						fprintf (outstream, "%s", intel_hregs[tmp->in_args[0]->regconst]);
					} else {
						fprintf (outstream, "%s", intel_lregs[tmp->in_args[0]->regconst]);
					}
				}
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				break;
			default:
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				/* probably register or register indirect */
				switch ((tmp->out_args[0]->flags & ARG_MODEMASK)) {
				case ARG_REG:
					if (tmp->out_args[0]->regconst >= 4) {
						fprintf (stderr, "%s: error: low 8-bit register %d unacceptable! (<4)\n", progname, tmp->out_args[0]->regconst);
					} else {
						if (tmp->out_args[0]->flags & ARG_IS8HIGH) {
							fprintf (outstream, "%s", intel_hregs[tmp->out_args[0]->regconst]);
						} else {
							fprintf (outstream, "%s", intel_lregs[tmp->out_args[0]->regconst]);
						}
					}
					break;
				default:
					drop_arg (tmp->out_args[0], outstream);
					break;
				}
				break;
			}
			fprintf (outstream, "\n");
			break;
		case INS_MOVEW:
			/* register source/dest is 16-bit */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			switch ((tmp->in_args[0]->flags & ARG_MODEMASK)) {
			case ARG_REG:
				if (tmp->in_args[0]->regconst >= 4) {
					fprintf (stderr, "%s: error: low 16-bit register %d unacceptable! (<4)\n", progname, tmp->in_args[0]->regconst);
				} else {
					fprintf (outstream, "%s", intel_xregs[tmp->in_args[0]->regconst]);
				}
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				break;
			default:
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				/* probably register or register indirect */
				switch ((tmp->out_args[0]->flags & ARG_MODEMASK)) {
				case ARG_REG:
					if (tmp->out_args[0]->regconst >= 4) {
						fprintf (stderr, "%s: error: low 16-bit register %d unacceptable! (<4)\n", progname, tmp->out_args[0]->regconst);
					} else {
						fprintf (outstream, "%s", intel_xregs[tmp->out_args[0]->regconst]);
					}
					break;
				default:
					drop_arg (tmp->out_args[0], outstream);
					break;
				}
				break;
			}
			fprintf (outstream, "\n");
			break;
		case INS_REPMOVEB:
			fprintf (outstream, "\trep\n");
			fprintf (outstream, "\tmovsb\n");
			break;
		case INS_REPMOVEL:
			fprintf (outstream, "\trep\n");
			fprintf (outstream, "\tmovsl\n");
			break;
		case INS_ANNO:
			fprintf (outstream, "# ");
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
		case INS_FSTSW:
			/* 16-bit register.. */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			if ((tmp->out_args[0]->flags & ARG_MODEMASK) == ARG_REG) {
				if (tmp->out_args[0]->regconst != REG_EAX) {
					fprintf (stderr, "%s: error: fstsw register not AX (was %d)\n", progname, tmp->out_args[0]->regconst);
				}
				fprintf (outstream, "%%ax");
			} else {
				drop_arg (tmp->out_args[0], outstream);
			}
			fprintf (outstream, "\n");
			break;
		case INS_JUMP:
		case INS_CALL:
		case INS_CCALL:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_PJUMP:
			/* jump to somewhere, but poll scheduler and see if a reschedule is needed.  
			 * The synch-flags 0(sf) and Tptr 4(sf) are checked for activity.
			 * We know that (a) the tstack is empty and (b) we have two/three words below Wptr for stuff */
			if (options.kernel_interface & KRNLIFACE_MP) {
				/* if this is set, multiprocessor kernel, so reschedule the old way */
				fprintf (outstream, "# invalid PJUMP, defaulting to regular jump\n");
				fprintf (stderr, "%s: warning: pjump stepped back to regular jump\n", progname);

				fprintf (outstream, "\t%s\t", codes[INS_JUMP]);
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
			} else if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
				fprintf (outstream, "# generating PJUMP code\n");
			#if 0	/* CMPXCHG8B version */
				fprintf (outstream, "\tmovl\t$0, %%eax\n");
				fprintf (outstream, "\tmovl\t$0, %%ebx\n");
				fprintf (outstream, "\tmovl\t$0, %%ecx\n");
				fprintf (outstream, "\tmovl\t$0, %%edx\n");
				fprintf (outstream, "\tcmpxchg8b\tsf\n");
				fprintf (outstream, "\tjz\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				fprintf (outstream, "# reschedule code\n");
				fprintf (outstream, "\tmovl\t$");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", -4(%%ebp)\n");
				fprintf (outstream, "\tjmp\t_X_occscheduler_q\n");
			#else	/* individual compare version */
				fprintf (outstream, "\tlea\tsf, %%ebx\n");
				fprintf (outstream, "\tmovl\t0(%%ebx), %%eax\n");
				fprintf (outstream, "\torl\t4(%%ebx), %%eax\n");
				if (options.pause_at_loopend == 2) {
					/* check the run-queue for activity too */
					fprintf (outstream, "\torl\t%%esi, %%eax\n");
				}
				fprintf (outstream, "\tjz\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				fprintf (outstream, "# reschedule code\n");
				fprintf (outstream, "\tmovl\t$");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", -4(%%ebp)\n");
				fprintf (outstream, "\tjmp\t_X_occscheduler_q\n");
			#endif
			} else {
				/* dunno for this, just spit regular jump */
				fprintf (stderr, "%s: warning: pjump stepped back to regular jump\n", progname);
				fprintf (outstream, "\t%s\t", codes[INS_JUMP]);
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
			}
			break;
		case INS_LOCK:
			/* special instruction doesn't start new line */
			fprintf (outstream, "\t%s;", codes[tmp->type]);
			break;
		#ifdef OLD_ASSEMBLER
			/* special help for assemblers which don't understand some mnemonics */
		case INS_FUCOM:
			fprintf (outstream, "# artificial fucom\n");
			fprintf (outstream, "\t.byte 0xdd, 0xe1\n");
			break;
		case INS_FILD64:
			fprintf (outstream, "# alternative fildd ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n\tfildll ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
		case INS_FILD32:
			fprintf (outstream, "# artificial fildl ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n\t.byte 0xdb\n");
			if (((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_REGIND) && !(tmp->in_args[0]->flags & ARG_DISP)) {
				switch (tmp->in_args[0]->regconst) {
				default:
					fprintf (stderr, "%s: warning: failed to code for FILD32 (unknown reg field).  Generating for (%%eax).\n", progname);
					/* fall through */
				case REG_EAX:
					fprintf (outstream, "\t.byte 0x00\n");
					break;
				case REG_EBX:
					fprintf (outstream, "\t.byte 0x03\n");
					break;
				case REG_ECX:
					fprintf (outstream, "\t.byte 0x01\n");
					break;
				case REG_EDX:
					fprintf (outstream, "\t.byte 0x02\n");
					break;
				case REG_ESP:
					fprintf (outstream, "\t.byte 0x04, 0x24\n");
					break;
				case REG_EBP:
					fprintf (outstream, "\t.byte 0x45, 0xf6\n");
					break;
				}
			} else {
				fprintf (stderr, "%s: warning: failed to code for FILD32 (unknown mode).  Generating for (%%eax).\n", progname);
				fprintf (outstream, "\t.byte 0x00\n");
			}
			break;
		#endif	/* OLD_ASSEMBLER */
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
 *	disassembles some data
 *	returns 0 on success, non-zero on failure
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
/*{{{  static int disassemble_xdata (unsigned char *bytes, int length, tdfixup_t *fixups, FILE *outstream)*/
/*
 *	disassembles extended data -- this might include addresses of labels elsewhere,
 *	provided in "fixups"
 *	returns 0 on success, non-zero on failure
 */
static int disassemble_xdata (unsigned char *bytes, int length, tdfixup_t *fixups, FILE *outstream)
{
	tdfixup_t *walk;
	int i;

#if 0
fprintf (stderr, "asm386: disassemble_xdata(): length = %d\n", length);
for (walk = fixups; walk; walk = walk->next) {
	fprintf (stderr, "\t\tfixup (%d,%d,%d)\n", walk->thislab, walk->offset, walk->otherlab);
}
#endif
	for (i=0, walk=fixups; i<length;) {
		int count = 0;

		if (walk && (i == walk->offset)) {
			while (walk && (i == walk->offset)) {
				fprintf (outstream, "\n\t.long\t" LBLPFX "%d", walk->otherlab);
				walk = walk->next;
				i += 4;
			}
			if (i >= length) {
				continue;
			}
		}
		fprintf (outstream, "\n\t.byte\t");
		while ((i < length) && (!walk || (i < walk->offset))) {
			if (count == 8) {
				fprintf (outstream, "\n\t.byte\t");
				count = 0;
			}
			fprintf (outstream, "0x%2.2x", bytes[i]);
			if (((count & 0x07) == 0x07) || (i == (length -1)) || (walk && (walk->offset == (i+1)))) {
				/* nothing */
			} else {
				fprintf (outstream, ", ");
			}
			i++;
			count++;
		}
		while ((i < length) && walk && (i > walk->offset)) {
			/* this is bad, because it means we missed it -- won't happen if fixups are correctly sorted */
			fprintf (stderr, "%s: badly ordered fixup for L%d:%d -> L%d (ignoring)\n", progname, walk->thislab, walk->offset, walk->otherlab);
			walk = walk->next;
		}
	}
	fprintf (outstream, "\n");

	return 0;
}
/*}}}*/
/*{{{  static void dump_codemap_stream (procinf *piinf, FILE *stream)*/
/*
 *	dumps a codemap to the output stream
 */
static void dump_codemap_stream (procinf *piinf, FILE *stream, int *seen_data, int toplvl)
{
	char namebuf[MAXNAME];
	char *ntmp = NULL;
	int i;

#if 0
	if (options.annotate_output) {
		fprintf (stream, "# begin codemap for %s\n", piinf->name);
	}
#endif
	if (!*seen_data) {
		fprintf (stream, ".data\n");
		*seen_data = 1;
	}
	if (toplvl) {
		fprintf (stream, "\n.align 4\n");
		fprintf (stream, LBLPFX "%d:\n", piinf->maplab);
	}
	/* also set a global name for it! */
	if (toplvl) {
		snprintf (namebuf, sizeof namebuf, "*%s", piinf->name);
		ntmp = modify_name (namebuf);
		fprintf (stream, ".globl %s\n", ntmp);
		if (!options.disable_symbol_ops) {
			fprintf (stream, ".type %s, @object\n", ntmp);
		}
		fprintf (stream, "%s:\n", ntmp);
	}

	/* relevant address */
	if (piinf->is_internal) {
		fprintf (stream, "\t.long\t" LBLPFX "%d\n", piinf->eplab);
	} else {
		ntmp = modify_name (piinf->name);
		fprintf (stream, "\t.long\t%s\n", ntmp);
	}

	/* size of code if top-level, address of sub-ordinate map if not internal */
	if (toplvl) {
		snprintf (namebuf, sizeof namebuf, "^%s", piinf->name);
		ntmp = modify_name (namebuf);
		fprintf (stream, "\t.long\t%s", ntmp);
		fprintf (stream, " - %s\n", modify_name (piinf->name));
	} else if (piinf->is_internal) {
		fprintf (stream, "\t.long\t-1\n");
	} else {
		snprintf (namebuf, sizeof namebuf, "*%s", piinf->name);
		ntmp = modify_name (namebuf);
		fprintf (stream, "\t.long\t%s\n", ntmp);
	}

	/* name-label pointer */
	fprintf (stream, "\t.long\t" LBLPFX "%d\n", piinf->namelab);

	/* number of subordinates */
	if (toplvl) {
		fprintf (stream, "\t.long\t%d\n", piinf->refs_cur);
	} else {
		fprintf (stream, "\t.long\t0\n");
	}

	/* drop subordinates */
	for (i=0; i<piinf->refs_cur; i++) {
		/* fixup namelabels if internal label */
		if (piinf->refs[i]->is_internal) {
			piinf->refs[i]->namelab = piinf->inamelab;
		}
		dump_codemap_stream (piinf->refs[i], stream, seen_data, 0);
	}

	return;
}
/*}}}*/
/*{{{  static void dump_codemap_namelabels_stream (procinf *piinf, FILE *stream)*/
/*
 *	dumps name-labels for a codemap.  must be done separately to
 *	stop names appearing in the middle of maps and prevent duplicate
 *	name-labels being output.
 */
static void dump_codemap_namelabels_stream (procinf *piinf, FILE *stream)
{
	int i;

	if (!piinf->written_out) {
		if (piinf->namelen) {
			fprintf (stream, LBLPFX "%d:\t.asciz\t\"%*s\"\n", piinf->namelab, piinf->namelen, piinf->name);
		}
		if (piinf->inamelen) {
			fprintf (stream, LBLPFX "%d:\t.asciz\t\"%*s\"\n", piinf->inamelab, piinf->inamelen, piinf->iname);
		}
		piinf->written_out = 1;
	}
	/* subordinates */
	for (i=0; i<piinf->refs_cur; i++) {
		dump_codemap_namelabels_stream (piinf->refs[i], stream);
	}
	return;
}
/*}}}*/
/*{{{  int dump_asm386_stream (rtl_chain *rtl_code, FILE *stream)*/
/*
 *	dumps 386 assembly source to a stream (here and in elf via arch)
 */
int dump_asm386_stream (rtl_chain *rtl_code, FILE *stream)
{
	rtl_chain *tmp;
	int seen_data;
	int errored;

	seen_data = 0;
	errored = 0;
	for (tmp=rtl_code; tmp && !errored; tmp=tmp->next) {
		switch (tmp->type) {
		case RTL_SOURCEFILE:
			fprintf (stream, "\t# sourcefile \"%s\"\n", tmp->u.sourcefile);
			break;
		case RTL_CODE:
			errored = disassemble_code (tmp->u.code.head, stream, (options.debug_options & DEBUG_MEMCHK));
			break;
		case RTL_DATA:
			/* data can be implanted in code */
			errored = disassemble_data ((unsigned char *)tmp->u.data.bytes, tmp->u.data.length, stream);
			break;
		case RTL_XDATA:
			/* xdata should go where declared */
			if (tmp->u.xdata.label >= 0) {
				/* setting label with it */
				if (!seen_data) {
					fprintf (stream, ".data\n");
					seen_data = 1;
				}
				fprintf (stream, LBLPFX "%d:\n", tmp->u.xdata.label);
			}
			errored = disassemble_xdata ((unsigned char *)tmp->u.xdata.bytes, tmp->u.xdata.length, tmp->u.xdata.fixups, stream);
			break;
		case RTL_RDATA:
			if (!seen_data) {
				fprintf (stream, ".data\n");
				seen_data = 1;
			}
			fprintf (stream, LBLPFX "%d:\n", tmp->u.rdata.label);
			errored = disassemble_data ((unsigned char *)tmp->u.rdata.bytes, tmp->u.rdata.length, stream);
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
				if (!options.disable_symbol_ops) {
					fprintf (stream, ".type %s, @function\n", label);
				}
				fprintf (stream, "%s:\n", label);
			}
			break;
		case RTL_DYNCODEENTRY:
			{
				const char *label = modify_name (tmp->u.dyncode.label_name);
				const char *flabel;

				if (!seen_data) {
					fprintf (stream, ".data\n");
					seen_data = 1;
				}

				fprintf (stream, ".globl %s\n", label);
				fprintf (stream, ".align 4\n");
				fprintf (stream, "%s:\n", label);

				flabel = modify_name (tmp->u.dyncode.fcn_name);
				fprintf (stream, "\t.long %s, %d, %d, 0x%x\n", flabel,
						tmp->u.dyncode.ws_slots, tmp->u.dyncode.vs_slots, tmp->u.dyncode.typehash);

				if (tmp->u.dyncode.rmoxmode != RM_NONE) {
					/* add some extra information with details about what we're compiling */
					fprintf (stream, ".globl rmox_modname\n");
					fprintf (stream, "rmox_modname: .asciz \"%s\"\n", tmp->u.dyncode.fcn_name ?: "");
					fprintf (stream, ".align 4\n");
					fprintf (stream, ".globl rmox_modtype\n");
					fprintf (stream, "rmox_modtype: .long %d\n", (int)tmp->u.dyncode.rmoxmode);
				}
			}
			break;
		case RTL_PUBLICENDNAMEDLABEL:
			if (!options.disable_symbol_ops) {
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
			const char *pfx = options.extref_prefix ? options.extref_prefix : "";

			if (options.rmoxmode == RM_NONE) {
				/* only generate these if not compiling for RMoX */
				fprintf (stream, ".globl %s_wsbytes\n", pfx);
				fprintf (stream, "%s_wsbytes: .long %d\n", pfx, tmp->u.wsvs.ws_bytes);
				fprintf (stream, ".globl %s_wsadjust\n", pfx);
				fprintf (stream, "%s_wsadjust: .long %d\n", pfx, tmp->u.wsvs.ws_adjust);
				fprintf (stream, ".globl %s_vsbytes\n", pfx);
				fprintf (stream, "%s_vsbytes: .long %d\n", pfx, tmp->u.wsvs.vs_bytes);
				fprintf (stream, ".globl %s_msbytes\n", pfx);
				fprintf (stream, "%s_msbytes: .long %d\n", pfx, tmp->u.wsvs.ms_bytes);
			}
			break;
		case RTL_UNDEFINED:
		case RTL_CODELINE:
		case RTL_COMMENT:
		case RTL_MESSAGE:
			break;
		case RTL_CODEMAP:
			dump_codemap_stream (tmp->u.codemap.pinf, stream, &seen_data, 1);
			dump_codemap_namelabels_stream (tmp->u.codemap.pinf, stream);
			break;
		}
	}
	/*{{{  emit .note.GNU-stack section */
#ifdef HOST_OS_IS_LINUX
	fprintf (stream, ".section .note.GNU-stack,\"\",%%progbits\n");
#endif
	/*}}}*/
	return errored;
}
/*}}}*/
/*{{{  int dump_asm386 (rtl_chain *rtl_code, char *filename)*/
/*
 *	int dump_asm386 (rtl_chain *rtl_code, char *filename)
 *	dumps 386 assembly source
 */
int dump_asm386 (rtl_chain *rtl_code, char *filename)
{
	FILE *outstream;
	int result;

	dmcount = 0;
	outstream = fopen (filename, "w");
	if (!outstream) {
		fprintf (stderr, "%s: error: unable to open %s for writing\n", progname, filename);
		return -1;
	}
	result = dump_asm386_stream (rtl_code, outstream);
	fclose (outstream);
	return result;
}
/*}}}*/

