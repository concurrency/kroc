/*
 *	rtldump.c - RTL dumper	
 *	Copyright (C) 2000 Fred Barnes <frmb@kent.ac.uk>
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

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#include "intel.h"
#include "archdef.h"

/*{{{  forwards*/
static int dump_ins (FILE *stream, ins_chain *ins, arch_t *arch);
static int dump_ins_chain (FILE *stream, ins_chain *ins_code, arch_t *arch);
static int dump_data_block (FILE *stream, char *bytes, int len);
static int dump_rtl_hunk (FILE *stream, rtl_chain *rtl, arch_t *arch);
static char *reg_string (int regnum, arch_t *arch);


/*}}}*/


/*{{{  int dump_textual_rtl (rtl_chain *rtl_code, char *outfile, arch_t *arch)*/
/*
 *	dumps some textual RTL to a file
 */
int dump_textual_rtl (rtl_chain *rtl_code, char *outfile, arch_t *arch)
{
	FILE *outstream;

	outstream = fopen (outfile, "w");
	if (!outstream) {
		fprintf (stderr, "%s: error opening %s for writing\n", progname, outfile);
		return -1;
	}
	while (rtl_code) {
		dump_rtl_hunk (outstream, rtl_code, arch);
		rtl_code = rtl_code->next;
	}
	fclose (outstream);
	return 0;
}
/*}}}*/
/*{{{  static char *reg_string (int regnum, arch_t *arch)*/
/*
 *	turns regnum into an appropriate string
 */
static char *reg_string (int regnum, arch_t *arch)
{
	static char buf[16];
	char *rv = NULL;

	switch (regnum) {
	case REG_WPTR:
		strcpy (buf, "%wptr");
		rv = buf;
		break;
	case REG_JPTR:
		strcpy (buf, "%jptr");
		rv = buf;
		break;
	case REG_LPTR:
		strcpy (buf, "%lptr");
		rv = buf;
		break;
	case REG_CC:
		strcpy (buf, "cc");
		rv = buf;
		break;
	case REG_CA:
		strcpy (buf, "ca");
		rv = buf;
		break;
	case REG_SCHED:
		strcpy (buf, "%sched");
		rv = buf;
		break;
	default:
		if (regnum >= FIRST_VIRTUAL_REG) {
			sprintf (buf, "%%%d", regnum);
			rv = buf;
		} else {
			rv = arch->get_register_name (regnum);
		}
		break;
	}
	return rv;
}
/*}}}*/
/*{{{  static int dump_ins (FILE *stream, ins_chain *ins, arch_t *arch)*/
/*
 *	dumps single instruction
 */
static int dump_ins (FILE *stream, ins_chain *ins, arch_t *arch)
{
	int i;
	static char *ins_names[] = {"..", "move", "nop", "lea", "setcc", "cmp", "add", "and", "or", \
		"into", "cjump", "setlab", "push", "pop", "ret", "call", "kcall", "jump", "swap", "dec", \
		"inc", "sub", "cwde", "xor", "imul", "laddr", "ldiff", "cdq", "idiv", "shl", "shr", "moveb", \
		"not", "ldiffsconst", "rep.moveb", "movz8to32", "rcr", "rcl", "ror", "rol", "adc", "sbb", \
		"mul", "div", "shld", "shrd", "fstcw", "fldcw", "wait", "fstp", "#", "fild", "fxch", "fld", \
		"fildl", "flds", "fldl", "fldt", "fadds", "faddl", "fmuls", "fmull", "fstps", "fstpl", "fstpt", \
		"fistps", "fistpl", "fld1", "fldl2t", "fldl2e", "fldpi", "fldlg2", "fldln2", "fldz", "faddp", \
		"fsubp", "fmulp", "fdivp", "sahf", "ftst", "fxam", "fstsw", "fucom", "fucomp", "fucompp", "fcom", \
		"fcomp", "fcompp", "frndint", "fsqrt", "fabs", "fchs", "fscale", "fprem1", "setflab", "movs16to32", \
		"lahf", "rep.movel", "cmove", "pjump", "inb", "outb", "ccall", "movz16to32", "movew", "mtfsb", \
		"mtfsi", "mffs", "mtfsf", "fsin", "fcos", "inw", "outw", "inl", "outl", "lock", "fptan"};
	static char *cc_names[] = {"<none>", "O", "NO", "B", "AE", "Z", "NZ", "BE", "A", "S", "NS", "PE", "PO", "LT", "GE", "LE", "GT"};
	static char *fregs[] = {"FP0", "FP1", "FP2", "FP3", "FP4", "FP5", "FP6", "FP7", "FP8", "FP9", "FP10", "FP11", "FP12", "FP13", "FP14", "FP15",
				"FP16", "FP17", "FP18", "FP19", "FP20", "FP21", "FP22", "FP23", "FP24", "FP25", "FP26", "FP27", "FP28", "FP29", "FP30", "FP31"};
	ins_sib_arg *t_sib;
	char *t_start;
	ins_labrefs *tmp_refs;

	if (ins->type == INS_ANNO) {
		for (t_start = (char *)ins->in_args[0]->regconst; (*t_start == ' ') || (*t_start == '\t'); t_start++);
		fprintf (stream, "# %s\n", t_start);
	} else if ((ins->type >= INS_FIRST) && (ins->type <= INS_LAST)) {
		if (ins->type == INS_SETLABEL) {
			fprintf (stream, ":");
		} else if (ins->type == INS_SETFLABEL) {
			fprintf (stream, "::");
		} else {
			fprintf (stream, "\t%s\t", ins_names[ins->type]);
		}
		for (i=0; ins->in_args[i]; i++) {
			if (ins->in_args[i]->flags & ARG_IMP) {
				fprintf (stream, "<");
			}
			if (ins->in_args[i]->flags & ARG_IND) {
				fprintf (stream, "*");
			}
			if (ins->in_args[i]->flags & ARG_DISP) {
				fprintf (stream, "%d", ins->in_args[i]->disp);
			}
			if (ins->in_args[i]->flags & ARG_ISCONST) {
				fprintf (stream, "$");
			}
			switch ((ins->in_args[i]->flags & ARG_MODEMASK)) {
			default:
			case ARG_UNDEFINED:
				fprintf (stream, "<?>");
				break;
			case ARG_REG:
				fprintf (stream, "%s", reg_string (ins->in_args[i]->regconst, arch));
				break;
			case ARG_FREG:
				fprintf (stream, "%s", fregs[ins->in_args[i]->regconst]);
				break;
			case ARG_CONST:
				fprintf (stream, "$x%x", ins->in_args[i]->regconst);
				break;
			case ARG_REGIND:
				fprintf (stream, "(%s)", reg_string (ins->in_args[i]->regconst, arch));
				break;
			case ARG_COND:
				fprintf (stream, "_%s_", cc_names[ins->in_args[i]->regconst + 1]);
				break;
			case ARG_LABEL:
				fprintf (stream, "L%d", ins->in_args[i]->regconst);
				break;
			case ARG_INSLABEL:
				fprintf (stream, "L_%d", ((ins_chain *)ins->in_args[i]->regconst)->in_args[0]->regconst);
				break;
			case ARG_NAMEDLABEL:
				fprintf (stream, "%s", (char *)ins->in_args[i]->regconst);
				break;
			case ARG_TEXT:
				fprintf (stream, "%s", (char *)ins->in_args[i]->regconst);
				break;
			case ARG_REGINDSIB:
				t_sib = (ins_sib_arg *)ins->in_args[i]->regconst;
				fprintf (stream, "(%s,", reg_string (t_sib->base, arch));
				fprintf (stream, "%s,%d)", reg_string (t_sib->index, arch), t_sib->scale);
				break;
			case ARG_FLABEL:
				fprintf (stream, "%df", ins->in_args[i]->regconst);
				break;
			case ARG_BLABEL:
				fprintf (stream, "%db", ins->in_args[i]->regconst);
				break;
			}
			if (ins->in_args[i]->flags & ARG_IMP) {
				fprintf (stream, ">");
			}
			if (ins->in_args[i+1] || ins->out_args[0]) {
				fprintf (stream, ", ");
			}
		}
		for (i=0; ins->out_args[i]; i++) {
			if (ins->out_args[i]->flags & ARG_IMP) {
				fprintf (stream, "<");
			}
			if (ins->out_args[i]->flags & ARG_IND) {
				fprintf (stream, "*");
			}
			if (ins->out_args[i]->flags & ARG_DISP) {
				fprintf (stream, "%d", ins->out_args[i]->disp);
			}
			if (ins->out_args[i]->flags & ARG_ISCONST) {
				fprintf (stream, "$");
			}
			switch ((ins->out_args[i]->flags & ARG_MODEMASK)) {
			default:
				fprintf (stream, "<?>");
				break;
			case ARG_REG:
				fprintf (stream, "%s", reg_string (ins->out_args[i]->regconst, arch));
				break;
			case ARG_FREG:
				fprintf (stream, "%s", fregs[ins->out_args[i]->regconst]);
				break;
			case ARG_REGIND:
				fprintf (stream, "(%s)", reg_string (ins->out_args[i]->regconst, arch));
				break;
			case ARG_REGINDSIB:
				t_sib = (ins_sib_arg *)ins->out_args[i]->regconst;
				fprintf (stream, "(%s,", reg_string (t_sib->base, arch));
				fprintf (stream, "%s,%d)", reg_string (t_sib->index, arch), t_sib->scale);
				break;
			case ARG_NAMEDLABEL:
				fprintf (stream, "%s", (char *)ins->out_args[i]->regconst);
				break;
			case ARG_LABREFS:
				tmp_refs = ArgLabRefs (ins->out_args[i]);
				if (tmp_refs) {
					fprintf (stream, "[%d]", tmp_refs->ref_cur);
				} else {
					fprintf (stream, "[null]");
				}
			}
			if (ins->out_args[i]->flags & ARG_IMP) {
				fprintf (stream, ">");
			}
			if (ins->out_args[i+1]) {
				fprintf (stream, ", ");
			}
		}
		fprintf (stream, "\n");
	} else {
		switch (ins->type) {
		case INS_START_REG:
			if (ins->out_args[0]) {
				fprintf (stream, "\t\t\t\t<start %s ", reg_string (ins->in_args[0]->regconst, arch));
				fprintf (stream, "%s>\n", reg_string (ins->out_args[0]->regconst, arch));
			} else {
				fprintf (stream, "\t\t\t\t<start %s>\n", reg_string (ins->in_args[0]->regconst, arch));
			}
			break;
		case INS_END_REG:
			fprintf (stream, "\t\t\t\t<end %s>\n", reg_string (ins->in_args[0]->regconst, arch));
			break;
		case INS_CONSTRAIN_REG:
			fprintf (stream, "\t\t\t\t<constrain %s ", reg_string (ins->in_args[0]->regconst, arch));
			fprintf (stream, "%s>\n", reg_string (ins->in_args[1]->regconst, arch));
			break;
		case INS_UNCONSTRAIN_REG:
			fprintf (stream, "\t\t\t\t<un-constrain %s>\n", reg_string (ins->in_args[0]->regconst, arch));
			break;
		case INS_FREE_REG:
			fprintf (stream, "\t\t\t\t<free %s>\n", reg_string (ins->in_args[0]->regconst, arch));
			break;
		case INS_SOURCELINE:
			fprintf (stream, "\t\t\t\t<sourceline %d>\n", ins->in_args[0]->regconst);
			break;
		case INS_START_CC:
			fprintf (stream, "\t\t\t\t<startcc [%8.8x]>\n", ins->in_args[0]->regconst);
			break;
		case INS_END_CC:
			fprintf (stream, "\t\t\t\t<endcc [%8.8x]>\n", ins->in_args[0]->regconst);
			break;
		case INS_CLEANUP:
			fprintf (stream, "\t\t\t\t<cleanup>\n");
			break;
		default:
		case INS_UNDEFINED:
			fprintf (stream, "\t\t\t\t<unknown instuction %d>\n", ins->type);
			break;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static int dump_ins_chain (FILE *stream, ins_chain *ins_code, arch_t *arch)*/
/*
 *	dumps instruction chain
 */
static int dump_ins_chain (FILE *stream, ins_chain *ins_code, arch_t *arch)
{
	while (ins_code) {
		dump_ins (stream, ins_code, arch);
		ins_code = ins_code->next;
	}
	return 0;
}
/*}}}*/
/*{{{  void dump_ins_chunk (FILE *stream, ins_chain *start, ins_chain *stop, arch_t *arch)*/
/*
 *	dumps chunk of an instruction chain (used for debugging mostly)
 */
void dump_ins_chunk (FILE *stream, ins_chain *start, ins_chain *stop, arch_t *arch)
{
	while (start != stop->next) {
		dump_ins (stream, start, arch);
		start = start->next;
	}
	return;
}
/*}}}*/
/*{{{  static int dump_data_block (FILE *stream, char *bytes, int len)*/
/*
 *	dumps a data block
 */
static int dump_data_block (FILE *stream, char *bytes, int len)
{
	int i;

	fprintf (stream, "\t");
	for (i=0; i<len; i++) {
		if (i && !(i % 16)) {
			fprintf (stream, "\n\t");
		}
		fprintf (stream, "%2.2x ", (unsigned char)(bytes[i]));
	}
	fprintf (stream, "\n");
	return 0;
}
/*}}}*/
/*{{{  static int dump_rtl_hunk (FILE *stream, rtl_chain *rtl, arch_t *arch)*/
/*
 *	dumps single RTL hunk
 */
static int dump_rtl_hunk (FILE *stream, rtl_chain *rtl, arch_t *arch)
{
	switch (rtl->type) {
	case RTL_SOURCEFILE:
		fprintf (stream, "<SOURCEFILE \"%s\">\n", rtl->u.sourcefile);
		break;
	case RTL_CODE:
		fprintf (stream, "<CODE START>\n");
		dump_ins_chain (stream, rtl->u.code.head, arch);
		fprintf (stream, "<CODE END>\n");
		break;
	case RTL_DATA:
		fprintf (stream, "<DATA START>\n");
		dump_data_block (stream, rtl->u.data.bytes, rtl->u.data.length);
		fprintf (stream, "<DATA END>\n");
		break;
	case RTL_XDATA:
		{
			tdfixup_t *walk;

			fprintf (stream, "<EXTENDED DATA START>\n");
			if (rtl->u.xdata.label >= 0) {
				fprintf (stream, "L%d:\n", rtl->u.xdata.label);
			}
			dump_data_block (stream, rtl->u.xdata.bytes, rtl->u.xdata.length);
			for (walk = rtl->u.xdata.fixups; walk; walk = walk->next) {
				fprintf (stream, "<FIXUP %d %d %d>\n", walk->thislab, walk->offset, walk->otherlab);
			}
			fprintf (stream, "<EXTENDED DATA END>\n");
		}
		break;
	case RTL_RDATA:
		fprintf (stream, "<RELOC DATA START>\n");
		fprintf (stream, "L%d:\n", rtl->u.rdata.label);
		dump_data_block (stream, rtl->u.rdata.bytes, rtl->u.rdata.length);
		fprintf (stream, "<RELOC DATA END>\n");
		break;
	case RTL_SETNAMEDLABEL:
		fprintf (stream, "<SETNAMEDLABEL \"%s\">\n", rtl->u.label_name);
		break;
	case RTL_STUBIMPORT:
		fprintf (stream, "<STUBIMPORT \"%s\">\n", rtl->u.label_name);
		break;
	case RTL_PUBLICSETNAMEDLABEL:
		fprintf (stream, "<PUBLICSETNAMEDLABEL \"%s\">\n", rtl->u.label_name);
		break;
	case RTL_ALIGN:
		fprintf (stream, "<ALIGN %d>\n", rtl->u.alignment);
		break;
	case RTL_COMMENT:
		fprintf (stream, "<COMMENT START>\n");
		dump_data_block (stream, rtl->u.data.bytes, rtl->u.data.length);
		fprintf (stream, "<COMMENT END>\n");
		break;
	case RTL_MESSAGE:
		fprintf (stream, "<MESSAGE START>\n");
		dump_data_block (stream, rtl->u.data.bytes, rtl->u.data.length);
		fprintf (stream, "<MESSAGE END>\n");
		break;
	case RTL_WSVS:
		fprintf (stream, "<WS=%d, ADJ=%d, VS=%d, MS=%d>\n", rtl->u.wsvs.ws_bytes, rtl->u.wsvs.ws_adjust, rtl->u.wsvs.vs_bytes, rtl->u.wsvs.ms_bytes);
		break;
	case RTL_CODEMAP:
		fprintf (stream, "<CODEMAP START>\n");
		procinf_dumpentry (stream, rtl->u.codemap.pinf);
		fprintf (stream, "<CODEMAP END>\n");
		break;
	case RTL_DYNCODEENTRY:
		fprintf (stream, "<DYNCODE ENTRY \"%s\" \"%s\" %d %d 0x%8.8x>\n", rtl->u.dyncode.label_name, rtl->u.dyncode.fcn_name,
				rtl->u.dyncode.ws_slots, rtl->u.dyncode.vs_slots, rtl->u.dyncode.typehash);
		break;
	default:
	case RTL_UNDEFINED:
		fprintf (stream, "<unknown RTL type %d>\n", rtl->type);
		break;
	}
	return 0;
}
/*}}}*/





