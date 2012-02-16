/*
 *	asmppc.c - PowerPC assembly source outputter
 *	Copyright (C) 2005 Fred Barnes <frmb@kent.ac.uk>
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
#include "ppc.h"
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
static char *ppc_regs[] = {"0", "1", "2", "3", "4", "5", "6", "7",
			"8", "9", "10", "11", "12", "13", "14", "15",
			"16", "17", "18", "19", "20", "21", "22", "23",
			"24", "25", "26", "27", "28", "29", "30", "31"};
static char *ppc_fregs[] = {"0", "1", "2", "3", "4", "5", "6", "7",
				"8", "9", "10", "11", "12", "13", "14", "15",
				"16", "17", "18", "19", "20", "21", "22", "23",
				"24", "25", "26", "27", "28", "29", "30", "31"};
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
		case '^':
		case '*':
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
	if ((rbuf[0] != '_') && strncmp (rbuf, "O_", 2) && (name[0] != '&') && (name[0] != '^') && (name[0] != '*')) {
		/* prepend O_ */
		memmove (rbuf+2, rbuf, strlen(rbuf) + 1);
		memcpy (rbuf, "O_", 2);
	} else if (name[0] == '^') {
		/* prepend E_ */
		memmove (rbuf+2, rbuf, strlen(rbuf) + 1);
		memcpy (rbuf, "E_", 2);
	} else if (name[0] == '*') {
		/* prepend M_ */
		memmove (rbuf+2, rbuf, strlen(rbuf) + 1);
		memcpy (rbuf, "M_", 2);
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

	regset = ppc_regs;
	switch (arg->flags & ARG_MODEMASK) {
	case ARG_REG:
		fprintf (outstream, "%s", regset[arg->regconst]);
		break;
	case ARG_FREG:
		fprintf (outstream, "%s", ppc_fregs[arg->regconst]);
		break;
	case ARG_REGIND:
		if (arg->flags & ARG_DISP) {
			fprintf (outstream, "%d(%s)", arg->disp, regset[arg->regconst]);
		} else {
			fprintf (outstream, "0(%s)", regset[arg->regconst]);
		}
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
		/* blatently not supported here, but.. */
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
			fprintf (outstream, "\tpushl\t%s\n", ppc_regs[liveregs[i]]);
		}
	}
#else
#if 0
	fprintf (outstream, "\tpushf\n\tpusha\n");
#endif
#endif

	/* FIXME: Intel -> PPC */
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
			fprintf (outstream, "\tpopl\t%s\n", ppc_regs[liveregs[i]]);
		}
	}
	fprintf (outstream, "\tpopl\t%%ebp\n\tpopf\n");
#else
#if 0
	fprintf (outstream, "\tpopa\n\tpopf\n");
#endif
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
	static char *codes[] = { \
		"..", "mr", "nop", "..", "set", "cmp", "add", "and", "or", "..", \
		"..", "..", "..", "..", "blr", "call", "..", "b", "swap", "..", \
		"..", "subf", "extsh", "xor", "mullw", "rdtsc", "..", "cdq", "divw", "slw", \
		"srw", "movb", "not", "..", "..", "movzbl", "rcrl", "rcll", "rorl", "roll", \
		"adde", "subfe", "mullwu", "divwu", "shldl", "shrdl", "fstcw", "fldcw", "wait", "..", \
		"#", "..", "..", "..", "fcfid", "lfs", "lfd", "..", "..", "..", \
		"..", "..", "stfs", "stfd", "..", "fctiw", "fctid", "..", "..", "..", \
		"..", "..", "..", "..", "fadd", "fsub", "fmul", "fdiv", "..", "..", \
		"..", "..", "..", "..", "..", "..", "..", "..", "frsp", "..", \
		"fabs", "..", "..", "..", "..", "lha", "..", "..", "..", "..", \
		"..", "..", "bl", "..", "..", "mtfsb", "..", ".."};

	static char *ccset_codes[] = { \
		"..", "..", "..", "..", "..", "cmp.", "addo.", "and.", "or.", "..", \
		"..", "..", "..", "..", "..", "..", "..", "..", "..", "..", \
		"..", "subfo.", "extsh.", "xor.", "mullwo.", "..", "..", "..", "divwo.", "..", \
		"..", "..", "not.", "..", "..", "..", "rcr.", "rcl", "ror", "rol", \
		"addeo.", "subfeo.", "mullwu.", "udiv.", "shl.", "shr.", "..", "..", "..", "..", \
		"..", "..", "..", "..", "..", "..", "..", "..", "..", "..", \
		"..", "..", "..", "..", "..", "..", "..", "..", "..", "..", \
		"..", "..", "..", "..", "..", "..", "fctiw.", "fctid.", "..", "..", \
		"..", "..", "..", "..", "..", "..", "..", "..", "frsp.", "..", \
		"..", "..", "..", "..", "..", "..", "..", "..", "..", "..", \
		"..", "..", "..", "..", "..", ".."};
		
	/* FIXME: these need sorting properly..! */
	static char *setcc_tailcodes[] = {"so", "ns", "..", "ge", "eq", "ne", "le", "gt", "lt", "ge", "..", "..", "lt", "ge", "le", "gt", "", ".."};
	ins_chain *tmp;
	ins_arg *arg;
	int i, tlab1, tlab2;

	int live_vregs[LIVESIZE] = {-1024, -1024, -1024, -1024};		/* make sure it's well out of the way.. */
	int live_rregs[LIVESIZE] = {-1024, -1024, -1024, -1024};

	if (sizeof (codes) != sizeof (ccset_codes)) {
		fprintf (stderr, "%s: fatal: assertation (sizeof(codes) == sizeof(ccset_codes)) (%d != %d) failed\n", progname, (int) sizeof (codes), (int) sizeof (ccset_codes));
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
				fprintf (outstream, ";# verify target address %d\n", dmcount);
				coder_verify_addr (tmp->out_args[0], ADDR_TARGET, live_rregs, outstream, dmcount);
				break;
			case EtcPrimary (I_LDNL):
				/* verify source address */
				dmcount++;
				fprintf (outstream, ";# verify source address %d\n", dmcount);
				coder_verify_addr (tmp->in_args[0], ADDR_SOURCE, live_rregs, outstream, dmcount);
				break;
			case EtcSpecial (I_XSTL):
			case EtcSpecial (I_XSTLN):
				/* verify target address */
				dmcount++;
				fprintf (outstream, ";# verify target address %d\n", dmcount);
				coder_verify_addr (tmp->out_args[0], ADDR_TARGET | ADDR_SOURCE, live_rregs, outstream, dmcount);
				break;
			}
		}
		switch (tmp->type) {
			/*{{{  INS_SETLABEL -- set numeric label*/
		case INS_SETLABEL:
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ":\n");
			break;
			/*}}}*/
			/*{{{  INS_SETFLABEL -- set local label*/
		case INS_SETFLABEL:
			/* (very) local label */
			fprintf (outstream, "%d:\n", tmp->in_args[0]->regconst);
			break;
			/*}}}*/
			/*{{{  INS_LOADLABDIFF, INS_CONSTLABDIFF -- load label difference*/
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
				fprintf (outstream, "\tlis\t%s, (" LBLPFX "%d - " LBLPFX "%d)@ha\n", ppc_regs[tmp->out_args[0]->regconst], tlab1, tlab2);
				fprintf (outstream, "\tori\t%s, %s, (" LBLPFX "%d - " LBLPFX "%d)@l\n", ppc_regs[tmp->out_args[0]->regconst], ppc_regs[tmp->out_args[0]->regconst], tlab1, tlab2);
			} else {
				fprintf (outstream, ".long (" LBLPFX "%d - " LBLPFX "%d)\n", tlab1, tlab2);
			}
			break;
			/*}}}*/
			/*{{{  INS_CONSTLABADDR -- load label address*/
		case INS_CONSTLABADDR:
			if ((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_INSLABEL) {
				tlab1 = ((ins_chain *)tmp->in_args[0]->regconst)->in_args[0]->regconst;
			} else {
				tlab1 = tmp->in_args[0]->regconst;
			}
			fprintf (outstream, ".long " LBLPFX "%d\n", tlab1);
			break;
			/*}}}*/
			/*{{{  INS_CJUMP -- conditional jump*/
		case INS_CJUMP:
			fprintf (outstream, "\tb%s\t", setcc_tailcodes[tmp->in_args[0]->regconst]);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			fprintf (outstream, "\tnop\n");			/* delay slot */
			break;
			/*}}}*/
#if 0
			/*{{{  INS_CMOVE -- conditional move (unsupported)*/
		case INS_CMOVE:
			fprintf (outstream, "\tcmov%s\t", setcc_tailcodes[tmp->in_args[0]->regconst]);
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
#endif
			/*{{{  INS_SETCC -- set register from condition-code flag*/
		case INS_SETCC:
			/* do this with a set/branch combo.. */
			fprintf (outstream, "\tli\t");
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", 1\n");
			fprintf (outstream, "\tb%s\t0f\n", setcc_tailcodes[tmp->in_args[0]->regconst]);
			fprintf (outstream, "\tli\t");
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", 0\n");
			fprintf (outstream, "0:\n");
			break;
			/*}}}*/
			/*{{{  INS_ADD, INS_SUB, INS_OR, INS_AND, INS_XOR -- arithmetic instructions*/
		case INS_ADD:
		case INS_SUB:
		case INS_OR:
		case INS_AND:
		case INS_XOR:
			/* 2-1 things */

			if (ArgMode (tmp->in_args[0]) == ARG_CONST) {
				if (rtl_const_bitwidth (ArgConst (tmp->in_args[0]), 1) < 16) {
					/*{{{  small constant*/
					switch (tmp->type) {
					case INS_ADD:
						fprintf (outstream, "\taddi\t");
						break;
					case INS_OR:
						fprintf (outstream, "\tori\t");
						break;
					case INS_AND:
						fprintf (outstream, "\tandi\t");
						break;
					case INS_XOR:
						fprintf (outstream, "\txori\t");
						break;
					default:
						fprintf (stderr, "%s: error: don\'t know how to code constant instruction %d for PPC\n", progname, ins->type);
						break;
					}
					/*}}}*/
				} else {
					/*{{{  must already be broken up if can't do with shifted instruction*/
					switch (tmp->type) {
					case INS_ADD:
						fprintf (outstream, "\taddis\t");
						break;
					case INS_OR:
						fprintf (outstream, "\toris\t");
						break;
					case INS_AND:
						fprintf (outstream, "\tandis\t");
						break;
					case INS_XOR:
						fprintf (outstream, "\txoris\t");
						break;
					default:
						fprintf (stderr, "%s: error: don\'t know how to code constant shifted instruction %d for PPC\n", progname, ins->type);
						break;
					}
					/* wreck the constant here */
					tmp->in_args[0]->regconst = (int)((unsigned int)tmp->in_args[0]->regconst >> 16);
					/*}}}*/
				}
			} else if (rtl_instr_setscc (tmp)) {
				if (rtl_instr_setsca (tmp)) {
					/*{{{  deal slightly specially with CA setting (carry)*/
					switch (tmp->type) {
					case INS_ADD:
						fprintf (outstream, "\taddco.\t");
						break;
					case INS_SUB:
						fprintf (outstream, "\tsubfco.\t");
						break;
					default:
						fprintf (stderr, "%s: error: don\'t know how to code CA setting instruction %d for PPC\n", progname, ins->type);
						break;
					}
					/*}}}*/
				} else {
					fprintf (outstream, "\t%s\t", ccset_codes[tmp->type]);
				}
			} else {
				if (rtl_instr_setsca (tmp)) {
					/*{{{  deal specially with CA setting*/
					switch (tmp->type) {
					case INS_ADD:
						fprintf (outstream, "\taddo.\t");
						break;
					case INS_SUB:
						fprintf (outstream, "\tsubfo.\t");
						break;
					default:
						fprintf (stderr, "%s: error: don\'t know how to code CA setting instruction %d for PPC\n", progname, ins->type);
						break;
					}
					/*}}}*/
				} else {
					fprintf (outstream, "\t%s\t", codes[tmp->type]);
				}
			}
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", ");
			if (tmp->type == INS_SUB) {
				/* do the other way around */
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[1], outstream);
			} else {
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
			}
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_ADC, INS_SBB -- add and subtract using carry*/
		case INS_ADC:
		case INS_SBB:
			if (rtl_instr_setscc (tmp)) {
				fprintf (outstream, "\t%s\t", ccset_codes[tmp->type]);
			} else {
				fprintf (outstream, "\t%s\t", codes[tmp->type]);
			}

			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", ");
			if (tmp->type == INS_SBB) {
				/* do the other way around */
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[1], outstream);
			} else {
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
			}
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_RCR, INS_RCL, INS_ROR, INS_ROL -- more arithmetic instructions*/
		case INS_RCR:
		case INS_RCL:
		case INS_ROR:
		case INS_ROL:
			fprintf (stderr, "%s: serious: not coding RCR/RCL/ROR/ROL for PPC yet..\n", progname);
			break;
			/*}}}*/
			/*{{{  INS_CMP -- compare*/
		case INS_CMP:
			/* fprintf (outstream, "\tsubf.\t11, "); */
			fprintf (outstream, "\tcmpw\t");
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_MUL, INS_UMUL, INS_DIV, INS_UDIV -- signed/unsigned multiply and division*/
		case INS_MUL:
		case INS_UMUL:
		case INS_DIV:
		case INS_UDIV:
			if (rtl_instr_setscc (tmp)) {
				fprintf (outstream, "\t%s\t", ccset_codes[tmp->type]);
			} else {
				fprintf (outstream, "\t%s\t", codes[tmp->type]);
			}
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_SHLD, INS_SHRD -- shift*/
		case INS_SHLD:
		case INS_SHRD:
			/* 3-arg encoding & 8-bit register */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			if ((tmp->in_args[0]->flags & ARG_MODEMASK) == ARG_REG) {
				fprintf (outstream, "%s", ppc_regs[tmp->in_args[0]->regconst]);
			} else {
				drop_arg (tmp->in_args[0], outstream);
			}
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[2], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_SHR, INS_SHL -- shift*/
		case INS_SHR:
		case INS_SHL:
			switch (ArgMode (tmp->in_args[0])) {
				/*{{{  ARG_CONST -- shifting with a constant*/
			case ARG_CONST:
				/* can't do this on the PPC i'm using (32-bit mode), need to register-ify */
				fprintf (outstream, "\tli\t11, ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				fprintf (outstream, "\t%s\t", codes[tmp->type]);
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", 11\n");
				break;
				/*}}}*/
				/*{{{  ARG_REG -- shifting with a register*/
			case ARG_REG:
				fprintf (outstream, "\t%s\t", codes[tmp->type]);
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				break;
				/*}}}*/
				/*{{{  default -- error*/
			default:
				fprintf (stderr, "%s: error: unhandled shift!\n", progname);
				break;
				/*}}}*/
			}
			break;
			/*}}}*/
			/*{{{  INS_INC, INS_DEC -- increment/decrement*/
		case INS_INC:
		case INS_DEC:
			fprintf (stderr, "%s: error: unexpected INS/DEC for PPC\n", progname);
			break;
			/*}}}*/
			/*{{{  INS_NOT, INS_CWDE, INS_MOVESEXT16TO32 -- various 1-1 things*/
		case INS_NOT:
		case INS_CWDE:
		case INS_MOVESEXT16TO32:
			/* 1-1 things */
			if (rtl_instr_setscc (tmp)) {
				fprintf (outstream, "\t%s\t", ccset_codes[tmp->type]);
			} else {
				fprintf (outstream, "\t%s\t", codes[tmp->type]);
			}
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_MOVE -- word move r/r, r/m, m/r (and some others, e.g. c/m, FP)*/
		case INS_MOVE:
			switch (ArgMode (tmp->out_args[0])) {
				/*{{{  ARG_REG -- target is register*/
			case ARG_REG:
				switch (ArgMode (tmp->in_args[0])) {
					/*{{{  ARG_REG -- register - register move*/
				case ARG_REG:
					fprintf (outstream, "\tmr\t");
					drop_arg (tmp->out_args[0], outstream);
					fprintf (outstream, ", ");
					drop_arg (tmp->in_args[0], outstream);
					fprintf (outstream, "\n");
					break;
					/*}}}*/
					/*{{{  ARG_CONST -- load constant into register*/
				case ARG_CONST:
					if ((tmp->in_args[0]->regconst < (1 << 15)) && (tmp->in_args[0]->regconst >= -(1 << 15))) {
						fprintf (outstream, "\tli\t");
						drop_arg (tmp->out_args[0], outstream);
						fprintf (outstream, ", ");
						drop_arg (tmp->in_args[0], outstream);
					} else {
						fprintf (outstream, "\tlis\t");
						drop_arg (tmp->out_args[0], outstream);
						fprintf (outstream, ", %d\n", ((unsigned int)tmp->in_args[0]->regconst) >> 16);
						if ((unsigned int)tmp->in_args[0]->regconst & 0xffff) {
							fprintf (outstream, "\tori\t");
							drop_arg (tmp->out_args[0], outstream);
							fprintf (outstream, ", ");
							drop_arg (tmp->out_args[0], outstream);
							fprintf (outstream, ", %d", ((unsigned int)tmp->in_args[0]->regconst) & 0xffff);
						}
					}
					fprintf (outstream, "\n");
					break;
					/*}}}*/
					/*{{{  ARG_NAMEDLABEL, ARG_LABEL, ARG_INSLABEL, ARG_FLABEL, ARG_BLABEL -- load label address if IS_CONST or dereference*/
				case ARG_NAMEDLABEL:
				case ARG_LABEL:
				case ARG_INSLABEL:
				case ARG_FLABEL:
				case ARG_BLABEL:
					if (tmp->in_args[0]->flags & ARG_ISCONST) {
						/* load label address */
						fprintf (outstream, "\tlis\t");
						drop_arg (tmp->out_args[0], outstream);
						fprintf (outstream, ", ");
						drop_arg (tmp->in_args[0], outstream);
						fprintf (outstream, "@ha\n");
						fprintf (outstream, "\tori\t");
						drop_arg (tmp->out_args[0], outstream);
						fprintf (outstream, ", ");
						drop_arg (tmp->out_args[0], outstream);
						fprintf (outstream, ", ");
						drop_arg (tmp->in_args[0], outstream);
						fprintf (outstream, "@l\n");
					} else {
						fprintf (stderr, "%s: error: unhandled move to register!\n", progname);
					}
					break;
					/*}}}*/
					/*{{{  ARG_REGIND -- load register*/
				case ARG_REGIND:
					fprintf (outstream, "\tlwz\t");
					drop_arg (tmp->out_args[0], outstream);
					fprintf (outstream, ", ");
					drop_arg (tmp->in_args[0], outstream);
					fprintf (outstream, "\n");
					break;
					/*}}}*/
					/*{{{  default -- error*/
				default:
					fprintf (stderr, "%s: error: unhandled move to register!\n", progname);
					break;
					/*}}}*/
				}
				break;
				/*}}}*/
				/*{{{  ARG_REGIND -- target is register-indirect*/
			case ARG_REGIND:
				switch (ArgMode (tmp->in_args[0])) {
					/*{{{  ARG_REG -- store register*/
				case ARG_REG:
					fprintf (outstream, "\tstw\t");
					drop_arg (tmp->in_args[0], outstream);
					fprintf (outstream, ", ");
					drop_arg (tmp->out_args[0], outstream);
					fprintf (outstream, "\n");
					break;
					/*}}}*/
					/*{{{  default -- error*/
				default:
					fprintf (stderr, "%s: error: unhandled move to register-indirect!\n", progname);
					break;
					/*}}}*/
				}
				break;
				/*}}}*/
				/*{{{  ARG_NAMEDLABEL, ARG_LABEL, ARG_INSLABEL -- target is label-indirect*/
			case ARG_NAMEDLABEL:
			case ARG_LABEL:
			case ARG_INSLABEL:
				/* use r11 for temporary */
				fprintf (outstream, "\tlis\t11, ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "@ha\n");
				fprintf (outstream, "\tori\t11, 11, ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "@l\n");
				/* have target address in r11 */
				switch (ArgMode (tmp->in_args[0])) {
					/*{{{  ARG_REG -- store register*/
				case ARG_REG:
					fprintf (outstream, "\tstw\t");
					drop_arg (tmp->in_args[0], outstream);
					fprintf (outstream, ", 0(11)\n");
					break;
					/*}}}*/
					/*{{{  ARG_CONST -- store constant*/
				case ARG_CONST:
					/* use r12 for another temporary */
					if ((tmp->in_args[0]->regconst < (1 << 15)) && (tmp->in_args[0]->regconst >= -(1 << 15))) {
						fprintf (outstream, "\tli\t12, ");
						drop_arg (tmp->in_args[0], outstream);
						fprintf (outstream, "\n");
					} else {
						fprintf (outstream, "\tlis\t12, %d\n", ((unsigned int)tmp->in_args[0]->regconst) >> 16);
						fprintf (outstream, "\tori\t12, 12, %d\n", ((unsigned int)tmp->in_args[0]->regconst) & 0xffff);
					}
					fprintf (outstream, "\tstw\t12, 0(11)\n");
					break;
					/*}}}*/
					/*{{{  ARG_NAMEDLABEL, ARG_LABEL, ARG_INSLABEL, ARG_FLABEL, ARG_BLABEL -- label address if ISCONST*/
				case ARG_NAMEDLABEL:
				case ARG_LABEL:
				case ARG_INSLABEL:
				case ARG_FLABEL:
				case ARG_BLABEL:
					if (tmp->in_args[0]->flags & ARG_ISCONST) {
						/* load label address, use r12 for temporary */
						fprintf (outstream, "\tlis\t12, ");
						drop_arg (tmp->in_args[0], outstream);
						fprintf (outstream, "@ha\n");
						fprintf (outstream, "\tori\t12, 12, ");
						drop_arg (tmp->in_args[0], outstream);
						fprintf (outstream, "@l\n");

						fprintf (outstream, "\tstw\t12, 0(11)\n");
					} else {
						fprintf (stderr, "%s: error: unhandled move to label-indirect!\n", progname);
					}
					break;
					/*}}}*/
					/*{{{  default -- error*/
				default:
					fprintf (stderr, "%s: error: unhandled move to label-indirect!\n", progname);
					break;
					/*}}}*/
				}
				break;
				/*}}}*/
				/*{{{  ARG_FREG -- target is floating-point register*/
			case ARG_FREG:
				if (ArgMode (tmp->in_args[0]) != ARG_FREG) {
					fprintf (stderr, "%s: error: move to FP-reg from non FP-reg\n", progname);
					break;
				}
				if (rtl_instr_setscc (tmp)) {
					fprintf (outstream, "\tfmr.\t");
				} else {
					fprintf (outstream, "\tfmr\t");
				}
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, "\n");
				break;
				/*}}}*/
				/*{{{  default -- error*/
			default:
				fprintf (stderr, "%s: error: unhandled move!\n", progname);
				break;
				/*}}}*/
			}
			break;
			/*}}}*/
			/*{{{  INS_MOVEB -- move byte r/r, r/m, m/r*/
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
				fprintf (outstream, "\tlbz\t");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
			} else if ((ArgMode (tmp->in_args[0]) == ARG_REG) && (ArgMode (tmp->out_args[0]) == ARG_REG)) {
				/* move byte */
				fprintf (outstream, "\tmr\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, "\n");
			} else {
				fprintf (stderr, "%s: error: unhandled moveb!\n", progname);
			}
			break;
			/*}}}*/
			/*{{{  INS_REPMOVB, INS_REPMOVL -- replicated moves (unsupported)*/
		case INS_REPMOVEB:
		case INS_REPMOVEL:
			fprintf (stderr, "%s: error: unhandled replicated move!\n", progname);
			break;
			/*}}}*/
			/*{{{  INS_ANNO -- annotation*/
		case INS_ANNO:
			fprintf (outstream, ";# ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_SWAP -- swap*/
		case INS_SWAP:
			/* unusual 2-2 */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_JUMP -- jump*/
		case INS_JUMP:
			if (ArgMode (tmp->in_args[0]) == ARG_REG) {
				fprintf (outstream, "\tmtlr\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				fprintf (outstream, "\tblr\n");
			} else {
				fprintf (outstream, "\t%s\t", codes[tmp->type]);
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
			}
			fprintf (outstream, "\tnop\n");		/* for now, anyway.. */
			break;
			/*}}}*/
			/*{{{  INS_FSTSW -- store floating-point status word*/
		case INS_FSTSW:
			/* FIXME: for PowerPC */
			fprintf (stderr, "%s: serious: not coding FSTSW for PPC yet..\n", progname);
			break;
			/*}}}*/
			/*{{{  INS_CALL -- call*/
		case INS_CALL:
			fprintf (outstream, "\tbl\t");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_CCALL -- call*/
		case INS_CCALL:
			/* this call allowed */
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			fprintf (outstream, "\tnop\n");		/* delay slot (?) */
			break;
			/*}}}*/
			/*{{{  INS_KCALL -- delay-slot aware call (unsupported) */
		case INS_KCALL:
			/* and this one too -- special */
			fprintf (stderr, "%s: error: unexpected KCALL\n", progname);
			break;
			/*}}}*/
			/*{{{  INS_PJUMP -- scheduler-checking jump*/
		case INS_PJUMP:
			/* jump to somewhere, but poll scheduler and see if a reschedule is needed.  
			 * The synch-flags 0(sf) and Tptr 4(sf) are checked for activity.
			 * We know that (a) the tstack is empty and (b) we have two/three words below Wptr for stuff */
			if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
				fprintf (outstream, ";# generating PJUMP code\n");
			#if 0	/* CMPXCHG8B version */
				fprintf (outstream, "\tmovl\t$0, %%eax\n");
				fprintf (outstream, "\tmovl\t$0, %%ebx\n");
				fprintf (outstream, "\tmovl\t$0, %%ecx\n");
				fprintf (outstream, "\tmovl\t$0, %%edx\n");
				fprintf (outstream, "\tcmpxchg8b\tsf\n");
				fprintf (outstream, "\tjz\t");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				fprintf (outstream, ";# reschedule code\n");
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
				fprintf (outstream, ";# reschedule code\n");
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
			/*}}}*/
			/*{{{  INS_FLD32, INS_FLD64 -- load floating-point value*/
		case INS_FLD32:
		case INS_FLD64:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_FST32, INS_FST64 -- store floating-point value*/
		case INS_FST32:
		case INS_FST64:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_FILD64*/
		case INS_FILD64:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_MTFSB -- set floating-point status/control register bit*/
		case INS_MTFSB:
			fprintf (outstream, "\tmtfsb%d\t%d\n", ArgConst (tmp->in_args[1]), ArgConst (tmp->in_args[0]));
			break;
			/*}}}*/
			/*{{{  INS_MTSFI -- set floating-point status/control register nibble-immediate*/
		case INS_MTFSI:
			fprintf (outstream, "\tmtfsfi\t%d, %d\n", ArgConst (tmp->in_args[0]), ArgConst (tmp->in_args[1]));
			break;
			/*}}}*/
			/*{{{  INS_FADD, INS_FSUB, INS_FMUL, INS_FDIV -- floating-point operations (2 -> 1)*/
		case INS_FADD:
		case INS_FSUB:
		case INS_FMUL:
		case INS_FDIV:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_MFFS -- copy FPU control/status word to FP register*/
		case INS_MFFS:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->in_args[0], outstream);
			break;
			/*}}}*/
			/*{{{  INS_MTFSF -- copy FP register to FPU control/status word with mask*/
		case INS_MTFSF:
			fprintf (outstream, "\t%s\t%d, ", codes[tmp->type], ArgConst (tmp->in_args[0]));
			drop_arg (tmp->in_args[1], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  INS_FABS -- 1-1 floating-point things*/
		case INS_FABS:
			fprintf (outstream, "\t%s\t", codes[tmp->type]);
			drop_arg (tmp->out_args[0], outstream);
			fprintf (outstream, ", ");
			drop_arg (tmp->in_args[0], outstream);
			fprintf (outstream, "\n");
			break;
			/*}}}*/
			/*{{{  default*/
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
			/*}}}*/
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
/*{{{  int dump_asmppc_stream (rtl_chain *rtl_code, FILE *stream)*/
/*
 *	int dump_asmppc_stream (rtl_chain *rtl_code, FILE *stream)
 *	dumps ppc assembly source to a stream (here and in elf via arch)
 */
int dump_asmppc_stream (rtl_chain *rtl_code, FILE *stream)
{
	rtl_chain *tmp;
	int seen_data;
	int errored;


#if 0
fprintf (stderr, "asmppc.c: dump_asmppc_stream(): first rtl is %d\n", rtl_code->type);
#endif
	seen_data = 0;
	errored = 0;
	for (tmp=rtl_code; tmp && !errored; tmp=tmp->next) {
		switch (tmp->type) {
		case RTL_SOURCEFILE:
			fprintf (stream, "\t;# sourcefile \"%s\"\n", tmp->u.sourcefile);
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
/*{{{  int dump_asmppc (rtl_chain *rtl_code, char *filename)*/
/*
 *	int dump_asmppc (rtl_chain *rtl_code, char *filename)
 *	dumps ppc assembly source
 */
int dump_asmppc (rtl_chain *rtl_code, char *filename)
{
	FILE *outstream;
	int result;

	dmcount = 0;
	outstream = fopen (filename, "w");
	if (!outstream) {
		fprintf (stderr, "%s: error: unable to open %s for writing\n", progname, filename);
		return -1;
	}
	result = dump_asmppc_stream (rtl_code, outstream);
	fclose (outstream);
	return result;
}
/*}}}*/

