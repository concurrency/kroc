/*
 *	asmmips.c - mips assembly source outputter
 *	Copyright (C) 2000-2003 Fred Barnes <frmb@kent.ac.uk>,
 *	                        Christian L. Jacobsen
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
#include "mips.h"
#include "archdef.h"
#include "rtlops.h"
#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif

#if defined(CAN_DO_DOTLABELS)
	#define LBLPFX ".L"
#else
	#define LBLPFX "L"
#endif


/*{{{  static char *modify_name (char *name)*/
/*
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
	if ((rbuf[0] != '_') && strncmp (rbuf, "O_", 2) && (name[0] != '&')) {
		/* prepend O_ */
		memmove (rbuf+2, rbuf, strlen(rbuf)+1);
		memcpy (rbuf, "O_", 2);
	}
	return rbuf;
}
/*}}}*/
/*{{{  static int disassemble_data (unsigned char *bytes, int length, FILE *outstream)*/
/*
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
/*{{{  static int drop_arg (ins_arg *arg, FILE *outstream)*/
/*
 *	disassembles a single argument
 */
static int drop_arg (ins_arg *arg, FILE *outstream)
{
	char *tptr;

	/*
	if (arg->flags & ARG_IND) {
		fprintf (outstream, "*");
	}
	*/
	if (arg->flags & ARG_DISP) {
		fprintf (outstream, "%d", arg->disp);
	}
	/*
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
	*/
	switch (arg->flags & ARG_MODEMASK) {
		
	case ARG_REG:
		fprintf (outstream, "$%d", arg->regconst);
		break;
		/*
	case ARG_FREG:
		fprintf (outstream, "%s", intel_fregs[arg->regconst]);
		break;
		*/
	case ARG_REGIND:
		fprintf (outstream, "($%d)", arg->regconst);
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
		fprintf (stderr, "error: got a ARG_REGINDSIB, not valid on the mips.\n");
		abort();
		break;
		/*
	case ARG_REGINDSIB:
		t_sib = (ins_sib_arg *)arg->regconst;
		fprintf (outstream, "(%s,%s,%d)", regset[t_sib->base], regset[t_sib->index], t_sib->scale);
		break;
		*/
	/* FIXME: Debugging only */
	default:
		fprintf (outstream, ".!!!%d!!!.", arg->regconst);
	}
	return 0;
}
/*}}}*/
/*{{{  static int disassemble_code (ins_chain *ins, FILE *outstream)*/
/*
 *	dissasembles some code
 */
static int disassemble_code (ins_chain *ins, FILE *outstream)
{
	static char *codes[] = {
		"..", "move", "..",  "la",  "..",     ".." , "add", "and", "or", "..", /* 0-9 */
		"..", "..",   "..",  "..",  "jr", "jal", ".." , "j"  , "..", "..", /* 10-19 */
		"..", "sub",  "..",  "xor", "mul",    ".." , ".." , ".." , "..", "sll", /* 20-29 */
		"srl", "..",   "not", "..",  "..", "lbu", "..", "..", "..", "..", /* 30-39 */
		"..", "..", "..", "..", "..", "..", "..", "..", "..", "..", /* 40-49 */
		"..", "..", "..", "..", "..", "..", "..", "..", "..", "..", /* 50-59 */
		"..", "..", "..", "..", "..", "..", "..", "..", "..", "..", /* 60-69 */
		"..", "..", "..", "..", "..", "..", "..", "..", "..", "..", /* 70-79 */
		"..", "..", "..", "..", "..", "..", "..", "..", "..", "..", /* 80-89 */
		"..", "..", "..", "..", "..", "lh", "..", "..", "..", "..", /* 90-99 */
		"..", ".."                                                  /* 100-101 */
	};
	static int code_sizes[] = {
		2, 4, 2, 2, 2, 2, 3, 2, 2, 2, /* 0-9 */
		2, 2, 2, 2, 6, 3, 2, 1, 2, 2, /* 10-19 */
		2, 3, 2, 2, 3, 2, 2, 2, 2, 3, /* 20-29 */
		3, 2, 2, 2, 2, 3, 2, 2, 2, 2, /* 30-39 */
		2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* 40-49 */
		2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* 50-59 */
		2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* 60-69 */
		2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* 70-79 */
		2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* 80-89 */
		2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* 90-99 */
		2, 2,                         /* 100-101 */
	};
	/* static char *setcc_tailcodes[] = {"o",  "no", "b",   "ae",  "e",   "nz",  "be",  "a",   "s",  "ns", "pe", "po", "l",  "ge", "le", "g", "(always)", "(never)"}; */
	static char *setcc_tailcodes[] =    {"..", "..", "ltu", "geu", "eqz", "..", "leu", "gtu", "..", "..", "..", "..", "lt", "ge", "le", "gt", "..", ".."};
	ins_chain *tmp;
	ins_arg *arg;
	int i, tlab1, tlab2;

	if (sizeof (codes) != sizeof (code_sizes)) {
		fprintf (stderr, "%s: fatal: assertation (sizeof(codes) == sizeof(code_sizes)) (%d != %d) failed\n", progname, (int) sizeof (codes), (int) sizeof (code_sizes));
		return -1;
	}
	for (tmp=ins; tmp; tmp=tmp->next) {
		if ((tmp->type > INS_LAST) || (tmp->type < 0)) {
			continue;
		}
		switch(tmp->type){
			case INS_SETLABEL:
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ":\n");
				break;
			case INS_SETFLABEL:
				/* (very) local label */
				fprintf (outstream, "%d:\n", tmp->in_args[0]->regconst);
				break;
			case INS_MOVE: /* 1 */
				/* Deal with very special cases */
				if (ArgMode(tmp->in_args[0]) == ARG_REG && ArgReg(tmp->in_args[0]) == MIPS_REG_HI)
				{
					fprintf (outstream, "\tmfhi\t");
					drop_arg (tmp->out_args[0], outstream);
					fprintf (outstream, "\n");
					break;
				}
				if (ArgMode(tmp->in_args[0]) == ARG_REG && ArgReg(tmp->in_args[0]) == MIPS_REG_LO)
				{
					fprintf (outstream, "\tmflo\t");
					drop_arg (tmp->out_args[0], outstream);
					fprintf (outstream, "\n");
					break;
				}
				switch((tmp->out_args[0]->flags & ARG_MODEMASK)) {
				/* R <- ? instruction */
				case ARG_REG: /* out */
				switch((tmp->in_args[0]->flags & ARG_MODEMASK)) {
					case ARG_REG: /* in */
						fprintf (outstream, "\t%s\t", codes[tmp->type]);
						break;
					case ARG_CONST: /* in */
						fprintf (outstream, "\tli\t");
						break;
					case ARG_REGIND: /* in */
					case ARG_LABEL: /* in */
					case ARG_NAMEDLABEL: /* in */
					case ARG_FLABEL: /* in */
						if (ArgIsConst(tmp->in_args[0]))
							fprintf (outstream, "\tla\t");
						else
							fprintf (outstream, "\tlw\t");
						break;
					case ARG_INSLABEL: /* in */
						fprintf (outstream, "\tla\t");
						break;
					default:
						fprintf (stderr, "%s: error: unsupported argument type (%d) for in arg of Load/Move\n",
							progname, (tmp->in_args[0]->flags & ARG_MODEMASK));
				} /* Switch out args */
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				break; /* ARG_REG */
				/* R -> A instruction */
				case ARG_LABEL: /* out */
				case ARG_NAMEDLABEL: /* out */
				case ARG_REGIND: /* out */
					fprintf (outstream, "\tsw\t");
					drop_arg (tmp->in_args[0], outstream);
					fprintf (outstream, ", ");
					drop_arg (tmp->out_args[0], outstream);
					break;
				default:
					fprintf (stderr, "%s: error: unsupported argument type (%d) for out arg of L/S/M\n",
						progname, (tmp->out_args[0]->flags & ARG_MODEMASK));
				} /* Switch in args */
				fprintf (outstream, "\n");
				break; /* INS_MOVE */
			case INS_SETCC: /* 4 */
				/* FIXME: Check the order in which these are dropped is correct */
				switch(ArgCC(tmp->in_args[0]))
				{
					case CC_Z:
						fprintf (outstream, "\tseq\t");
						break;
					case CC_NZ:
						fprintf (outstream, "\tsne\t");
						break;
					default:
						fprintf (outstream, "\ts%s\t", setcc_tailcodes[ArgCC(tmp->in_args[0])]);
						break;
				}
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[2], outstream);
				fprintf (outstream, "\n");
				break; /* INS_SETCC */
			case INS_ADD: /* 6 */
				/* If I_ADD then use the MIPS add ins which traps on overflow */
				/* Also use addix if one of the args is a constant */
				if (EtcTypeGetSecondary(tmp) == I_ADD || EtcTypeGetPrimary(tmp) == I_ADC)
					fprintf (outstream, "\tadd%s\t", (ArgIsConst(tmp->in_args[1])?"i":""));
				else
					fprintf (outstream, "\tadd%su\t", (ArgIsConst(tmp->in_args[1])?"i":""));
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				/* Note args swapped cos at&t intel syntax has funny ordering here, due
				 * to ins %src, %dest ordering =(%dest=%dest-%src). This is the way we
				 * would excpect the instruction to be written on the MIPS, so the non
				 * output args must be swapped.
				 */
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				break; /* INS_ADD */
			case INS_LOADLABDIFF: /* 26 */
			case INS_CONSTLABDIFF: /* 33 */
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
					/* fprintf (outstream, "\tmovl\t$(L%d - L%d), %s\n", tlab1, tlab2, intel_regs[tmp->out_args[0]->regconst]); */
					fprintf (stderr, "error: dont know about INS_LOADLABDIFF yet");
					abort();
				} else {
					fprintf (outstream, ".long (" LBLPFX "%d - " LBLPFX "%d)\n", tlab1, tlab2);
					/* FIXME: Temporary fudge */
					if (tmp->next->type != INS_CONSTLABDIFF) {
						fprintf (outstream, ".align 8\n");
					}
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
			case INS_CJUMP: /* 10 */
				switch (ArgCC (tmp->in_args[0])) {
					case CC_Z:
						/* If we have four args, this is a beq */
						if (tmp->in_args[3]) {
							fprintf (outstream, "\tbeq\t");
						} else {
							fprintf (outstream, "\tbeqz\t");
						}
						break; /* CC_Z */
					case CC_NZ:
						/* If we have four args, this is a bne */
						if (tmp->in_args[3]) {
							fprintf (outstream, "\tbne\t");
						} else {
							fprintf (outstream, "\tbnez\t");
						}
						break; /* CC_NZ */
					case CC_LT:
						/* If we have four args, this is a blt */
						if (tmp->in_args[3]) {
							fprintf (outstream, "\tblt\t");
						} else {
							fprintf (outstream, "\tbltz\t");
						}
						break; /* CC_LT */
					default:
						fprintf (outstream, "\tb%s\t", setcc_tailcodes[ArgCC(tmp->in_args[0])]);
				}
				if (tmp->in_args[2]) {
					drop_arg (tmp->in_args[2], outstream);
					fprintf (outstream, ", ");
				}
				if (tmp->in_args[3]) {
					drop_arg (tmp->in_args[3], outstream);
					fprintf (outstream, ", ");
				}
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, "\n");
				break; /* INS_CJUMP */
			case INS_JUMP: /* 17 */
				if (ArgMode(tmp->in_args[0]) == ARG_REG) {
					fprintf (outstream, "\tjr\t");
				} else {
					fprintf (outstream, "\tj\t");
				}
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				break; /* INS_JUMP */
			case INS_DEC: /* 19 */
				fprintf (outstream, "\tsubu\t");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", 1\n");
				break; /* INS_DEC */
			case INS_INC: /* 20 */
				fprintf (outstream, "\taddiu\t");
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, ", 1\n");
				break; /* INS_INC */
			case INS_SUB: /* 21 */
				/* If I_SUB then use the MIPS sub ins which traps on overflow */
				if (EtcTypeGetSecondary(tmp) == I_SUB) {
					fprintf (outstream, "\tsub\t");
				} else {
					fprintf (outstream, "\tsubu\t");
				}
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				/* Note args swapped cos at&t intel syntax has funny ordering here, due
				 * to "ins %src, %dest" ordering =(%dest=%dest-%src). This is the way we
				 * would excpect the instruction to be written on the MIPS, so the non
				 * output args must be swapped.
				 */
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				break; /* INS_SUB */
			case INS_MUL: /* 24 */
				/* SIGNED multiply! */
				switch(EtcTypeGetSecondary(tmp)) {
				case I_MUL:
					/* If I_MUL then use the MIPS mulou ins which traps on overflow */
					fprintf (outstream, "\tmulo\t");
					break;
				default:
					fprintf (outstream, "\tmul\t");
					break;
				}
				/*
				if (EtcTypeGetSecondary(tmp) == I_MUL)
					fprintf (outstream, "\tmulo\t");
				else
					fprintf (outstream, "\tmul\t");
				*/
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				break; /* INS_MUL */
			case INS_DIV: /* 28 */
				/* This could be either a DIV or a REM */
				if (HasEtcType(tmp))
				{
					if (EtcTypeGetSecondary(tmp) == I_DIV)
						fprintf (outstream, "\tdiv\t");
					else if (EtcTypeGetSecondary(tmp) == I_REM)
						fprintf (outstream, "\trem\t");
					else
					{
						printf ("tranx86: DIV/REM instruction with invalid ETC instruction type information\n");
						abort();
					}
					drop_arg (tmp->out_args[0], outstream);
					fprintf (outstream, ", ");
					drop_arg (tmp->in_args[1], outstream);
					fprintf (outstream, ", ");
					drop_arg (tmp->in_args[0], outstream);
					fprintf (outstream, "\n");
				}
				else
				{
					printf ("tranx86: DIV/REM instruction without ETC instruction type information\n");
					abort();
				}
				break;
			case INS_MOVEB: /* 31 */
				if (ArgMode(tmp->out_args[0]) != ARG_REG)
				{
					/* Assume store */
					fprintf (outstream, "\tsb\t");
					drop_arg (tmp->in_args[0], outstream);
					fprintf (outstream, ", ");
					drop_arg (tmp->out_args[0], outstream);
					fprintf (outstream, "\n");
				}
				else
				{
					/* Assume load */
					fprintf (outstream, "\tlb\t");
					drop_arg (tmp->in_args[0], outstream);
					fprintf (outstream, ", ");
					drop_arg (tmp->out_args[0], outstream);
					fprintf (outstream, "\n");
				}
			break; /* INS_MOVEB */
			case INS_ADC: /* 40 */
				/* FIXME: We dont use this instruction as add with carry on the 
				 * mips, but for other things at the moment... like setif */
				switch(GetEtcType(tmp)) {
				case I_XDBLE:
					fprintf (outstream, "\tslt\t");
					break;
				case I_LMUL:
				case I_LSUM:
					fprintf (outstream, "\tsltu\t");
					break;
				case I_LDIFF:
					fprintf (outstream, "\tsgtu\t");
					break;
				default:
					fprintf (stderr, "warning: INS_ADC without etc instruction information");
					break;
				}
				drop_arg (tmp->out_args[0], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				break; /* INS_ADC */
			case INS_UMUL: /* 42 */
				switch(GetEtcType(tmp)) {
				case I_LMUL:
					/* If I_LMUL we need to use the hardware multiply instruction */
					fprintf (outstream, "\tmultu\t");
					break;
				default:
					fprintf (outstream, "\tmul\t");
					drop_arg (tmp->out_args[0], outstream);
					fprintf (outstream, ", ");
					break;
				}
				drop_arg (tmp->in_args[1], outstream);
				fprintf (outstream, ", ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				break; /* INS_UMUL */
			case INS_ANNO: /* 50 */
				fprintf (outstream, "# ");
				drop_arg (tmp->in_args[0], outstream);
				fprintf (outstream, "\n");
				break; /* INS_ANNO */
			default:
			/* Anything else */
			/* FIXME: Get rid of this test: */
			if (tmp->type < sizeof(code_sizes) / sizeof(int))
			if (!strcmp (codes[tmp->type], "..")) {
				fprintf (outstream, "\t..%d..\t", tmp->type);
			} else {
				fprintf (outstream, "\t%s\t", codes[tmp->type]);
			} else {
				/* If this instruction aint dealt with prints its INS_number
				   FIXME: As part of the fixme above, this needs to go when the whole
				   codes array has been populated */
				fprintf (outstream, "\t..%d..\t", tmp->type);
			}
			/* Lets assume that MIPS instructions only ever have one 
			 * non implied out arg (which I think is the case)
			 */
			if (tmp->out_args[0]) {
				if (!(tmp->out_args[0]->flags & ARG_IMP)) {
					drop_arg (tmp->out_args[0], outstream);
					if (tmp->in_args[0]) {
						fprintf (outstream, ", ");
					}
				}
			}
			/* Then "in_args" (source) 
			 * On the MIPS we need to drop them in the opposite order
			 * from what we do in the base language, intel at&t syntax
			 */
			for (i=MAX_IN_ARGS - 1; i >= 0; i--) {
				arg = tmp->in_args[i];
				if (!arg) {
					continue;
				}
				if (arg->flags & ARG_IMP) {
					continue;
				}
				drop_arg (arg, outstream);
				if (i >= 1 ) {
					if (tmp->in_args[i-1]) {
						if (!(tmp->in_args[i-1]->flags & ARG_IMP)) {
							fprintf (outstream, ", ");
						}
					}
				}
			}
			fprintf (outstream, "\n");
			break;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  int dump_asmmips_stream (rtl_chain *rtl_code, FILE *stream)*/
/*
 *	dumps RTL chain for MIPS
 */
int dump_asmmips_stream (rtl_chain *rtl_code, FILE *stream)
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
			errored = disassemble_code (tmp->u.code.head, stream);
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
			//fprintf (stream, "\tjmp\t%s\n", modify_name (tmp->u.label_name));
			fprintf (stderr, "Argh, dont know about STUBIMPORT yet...");
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
			//fprintf (stream, "\n.align %d\n", (1 << tmp->u.alignment));
			fprintf (stream, "\n.align %d\n", tmp->u.alignment);
			break;
		case RTL_WSVS:
			if (!seen_data) {
				fprintf (stream, ".data\n");
				seen_data = 1;
			}
			fprintf (stream, ".globl _wsbytes\n");
			fprintf (stream, "_wsbytes: .long %d\n", tmp->u.wsvs.ws_bytes);
			fprintf (stream, ".globl _wsadjust\n");
			fprintf (stream, "_wsadjust: .long %d\n", tmp->u.wsvs.ws_adjust);
			fprintf (stream, ".globl _vsbytes\n");
			fprintf (stream, "_vsbytes: .long %d\n", tmp->u.wsvs.vs_bytes);
			fprintf (stream, ".globl _msbytes\n");
			fprintf (stream, "_msbytes: .long %d\n", tmp->u.wsvs.ms_bytes);
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
/*{{{  int dump_asmmips (rtl_chain *rtl_code, char *filename)*/
/*
 *	dumps RTL chain to a file for MIPS
 */
int dump_asmmips (rtl_chain *rtl_code, char *filename)
{
	FILE *outstream;
	int result;

	outstream = fopen (filename, "w");
	if (!outstream) {
		fprintf (stderr, "%s: error: unable to open %s for writing\n", progname, filename);
		return -1;
	}
	result = dump_asmmips_stream (rtl_code, outstream);
	// FIXME: Just for my aesthetic needs:
	fprintf (outstream, "\n\n# vim" ":syntax=mips:\n");
	fclose (outstream);
	return result;
}
/*}}}*/


