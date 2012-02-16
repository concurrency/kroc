/*
 *	etcdump.c - ETC dumper
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

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#define WANT_TCOFF_TAGNAMES
#include "tcoff.h"
#undef WANT_TCOFF_TAGNAMES


static int dump_primary (int prim, int operand, char *buffer);
static int dump_secondary (int esfunc, char *buffer);
static long tcoff_getl (unsigned char **s, int *len);


/*{{{  char *string_of_primary (tstack *stack, int prim, int operand)*/
/*
 *	returns string of primary instruction (for annotations)
 */
char *string_of_primary (tstack *stack, int prim, int operand)
{
	char sbuffer[128];

	dump_primary (prim, operand, sbuffer);
	sprintf (sbuffer + strlen(sbuffer), " [tsd=%d,%d]", stack->ts_depth, stack->fs_depth);
	return string_dup (sbuffer);
}
/*}}}*/
/*{{{  char *string_of_secondary (tstack *stack, int esfunc)*/
/*
 *	returns string of secondary instruction (for annotations)
 */
char *string_of_secondary (tstack *stack, int esfunc)
{
	char sbuffer[128];

	dump_secondary (esfunc, sbuffer);
	sprintf (sbuffer + strlen(sbuffer), " [tsd=%d,%d]", stack->ts_depth, stack->fs_depth);
	return string_dup (sbuffer);
}
/*}}}*/
/*{{{  int dump_textual_etc (etc_chain *chain, char *filename)*/
/*
 *	dumps ETC to text file
 */
int dump_textual_etc (etc_chain *chain, char *filename)
{
	FILE *outstream;
	etc_chain *tmp;
	int res, inproc, subop, i;
	int wsoff, loop_end;
	char dumpstr[256];

	outstream = fopen (filename, "w");
	if (!outstream) {
		fprintf (stderr, "%s: unable to open %s for writing\n", progname, filename);
		return -1;
	}
	res = 0;
	inproc = 0;
#if 0
	for (tmp=chain; tmp; tmp = tmp->next) {
		fprintf (stderr, "  0x%8.8X, 0x%8.8X\n", (unsigned int)tmp->fn, (unsigned int)tmp->opd);
	}
#endif
	for (tmp=chain; tmp && !res; tmp=tmp->next) {
		if (tmp->fn < I_OPR) {
			res = dump_primary (tmp->fn, tmp->opd, dumpstr);
			fprintf (outstream, "%s\n", dumpstr);
			continue;
		} else if (tmp->opd >= (signed int)ETC_MAX) {
			res = dump_secondary (tmp->opd, dumpstr);
			switch (tmp->opd) {
			case I_XSTL:
				fprintf (outstream, "%s %d\n", dumpstr, tmp->fn - I_OPR);
				break;
			case I_XSTLN:
				fprintf (outstream, "%s -%d\n", dumpstr, tmp->fn - I_OPR);
				break;
			case I_NCALL:
				{
					int adj = tmp->fn - I_OPR;

					tmp = tmp->next;
					if (tmp->o_bytes) {
						fprintf (outstream, "%s %s %d\n", dumpstr, tmp->o_bytes, adj);
					} else {
						fprintf (outstream, "%s L%d %d\n", dumpstr, tmp->opd, adj);
					}
				}
				break;
			case I_NWSADJ:
				{
					int adj = tmp->fn - I_OPR;

					fprintf (outstream, ".NWSADJ %d\n", adj);
				}
				break;
			case I_NRET:
				fprintf (outstream, "%s %d\n", dumpstr, tmp->fn - I_OPR);
				break;
			default:
				fprintf (outstream, "%s\n", dumpstr);
				break;
			}
		} else {
			switch (tmp->opd) {
			case ETC0:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				if (inproc) {
					fprintf (outstream, "\t");
				}
				switch (tmp->opd) {
				case FINISH_OP:
					fprintf (outstream, ".END\n");
					break;
				case I64TOREAL:
					fprintf (outstream, ".I64TOREAL\n");
					break;
				case BOOLINVERT:
					fprintf (outstream, ".BOOLINVERT\n");
					break;
				case NOTPROCESS:
					fprintf (outstream, ".NOTPROCESS\n");
					break;
				case WIDENSHORT:
					fprintf (outstream, ".WIDENSHORT\n");
					break;
				case FPPOP:
					fprintf (outstream, ".FPPOP\n");
					break;
				case STARTTABLE:
					fprintf (outstream, ".STARTTABLE\n");
					break;
				case CONTRSPLIT:
					fprintf (outstream, ".CONTRSPLIT\n");
					break;
				case CONTRJOIN:
					fprintf (outstream, ".CONTRJOIN\n");
					break;
				case CHECKNOTNULL:
					fprintf (outstream, ".CHECKNOTNULL\n");
					break;
				case SEMCLAIM:
					fprintf (outstream, ".SEMCLAIM\n");
					break;
				case SEMRELEASE:
					fprintf (outstream, ".SEMRELEASE\n");
					break;
				case SEMINIT:
					fprintf (outstream, ".SEMINIT\n");
					break;
				case RESCHEDULE:
					fprintf (outstream, ".RESCHEDULE\n");
					break;
				case INDIRECT_AREG:
					fprintf (outstream, ".INDIRECT_AREG\n");
					break;
				case INDIRECT_BREG:
					fprintf (outstream, ".INDIRECT_BREG\n");
					break;
				case INDIRECT_CREG:
					fprintf (outstream, ".INDIRECT_CREG\n");
					break;
				case RMWSMAP:
					fprintf (outstream, ".RMWSMAP\n");
					break;
				case MPPCLONE:
					fprintf (outstream, ".MPPCLONE\n");
					break;
				case MPPSERIALISE:
					fprintf (outstream, ".MPPSERIALISE\n");
					break;
				case MPPDESERIALISE:
					fprintf (outstream, ".MPPDESERIALISE\n");
					break;
				case LOADCODEMAP:
					fprintf (outstream, ".LOADCODEMAP\n");
					break;
				case FBARINIT:
					fprintf (outstream, ".FBARINIT\n");
					break;
				case FBARSYNC:
					fprintf (outstream, ".FBARSYNC\n");
					break;
				case FBARRESIGN:
					fprintf (outstream, ".FBARRESIGN\n");
					break;
				case FBARENROLL:
					fprintf (outstream, ".FBARENROLL\n");
					break;
				case R32SIN:
					fprintf (outstream, ".R32SIN\n");
					break;
				case R64SIN:
					fprintf (outstream, ".R64SIN\n");
					break;
				case R32COS:
					fprintf (outstream, ".R32COS\n");
					break;
				case R64COS:
					fprintf (outstream, ".R64COS\n");
					break;
				case DTRACE:
					fprintf (outstream, ".DTRACE\n");
					break;
				case KILLCALL:
					fprintf (outstream, ".KILLCALL\n");
					break;
				case WAIT_FOR_INTERRUPT:
					fprintf (outstream, ".WAIT_FOR_INTERRUPT\n");
					break;
				case R32TAN:
					fprintf (outstream, ".R32TAN\n");
					break;
				case R64TAN:
					fprintf (outstream, ".R64TAN\n");
					break;
				default:
					fprintf (outstream, ".SPECIAL\n");
					break;
				}
				break;
			case ETC6:
			case ETC7:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				fprintf (outstream, "\nL%d:\n", tmp->opd);
				break;
			case ETC1:
			case ETC2:
			case ETC3:
			case ETC4:
			case ETC5:
			case ETC8:
			case ETC9:
			case ETC10:
			case ETC11:
			case ETC12:
			case ETC13:
			case ETC14:
				if (inproc) {
					fprintf (outstream, "\t");
				}
				subop = tmp->opd - ETC0;
				tmp = tmp->next;
				switch (subop) {
				case LINENUM:
					fprintf (outstream, ".LINE %d\n", tmp->opd);
					break;
				case DEBUGLINE:
					fprintf (outstream, ".DEBUGLINE %d\n", tmp->opd);
					break;
				case TSDEPTH:
					fprintf (outstream, ".TSDEPTH %d\n", tmp->opd);
					break;
				case FUNCRESULTS:
					fprintf (outstream, ".FUNCRESULTS %d\n", tmp->opd);
					break;
				case FUNCRETURN:
					fprintf (outstream, ".FUNCRETURN %d\n", tmp->opd);
					break;
				case REALRESULT:
					fprintf (outstream, ".REALRESULT %d\n", tmp->opd);
					break;
				case SRLIMM:
					fprintf (outstream, ".SRLIMM %d\n", tmp->opd);
					break;
				case SLLIMM:
					fprintf (outstream, ".SLLIMM %d\n", tmp->opd);
					break;
				case SETWS:
					fprintf (outstream, ".SETWS %d\n", tmp->opd);
					break;
				case SETVS:
					fprintf (outstream, ".SETVS %d\n", tmp->opd);
					break;
				case ALIGN:
					fprintf (outstream, ".ALIGN %d\n", tmp->opd);
					break;
				default:
					fprintf (outstream, ".MYSTERY %d %d\n", subop, tmp->opd);
					break;
				}
				break;
			case ETCS1:
				fprintf (outstream, ".STUBNAME %*s\n", tmp->o_len, tmp->o_bytes);
				inproc = 0;
				break;
			case ETCS2:
				fprintf (outstream, ".GLOBAL %*s\n", tmp->o_len, tmp->o_bytes);
				inproc = 0;
				break;
			case ETCS3:
				fprintf (outstream, ".JENTRY %*s\n", tmp->o_len, tmp->o_bytes);
				inproc = 0;
				break;
			case ETCS4:
				fprintf (outstream, ".PROC %*s\n", tmp->o_len, tmp->o_bytes);
				inproc = 1;
				break;
			case ETCS5:
				fprintf (outstream, ".FILENAME %*s\n", tmp->o_len, tmp->o_bytes);
				break;
			case ETCS6:
				fprintf (outstream, ".OCCCOMMENT %*s\n", tmp->o_len, tmp->o_bytes);
				break;
			case ETCS7:
				fprintf (outstream, ".CODEMAP %*s\n", tmp->o_len, tmp->o_bytes);
				break;
			case ETCS8:
				fprintf (outstream, ".DATABYTES %d", tmp->o_len);
				for (i=0; i<tmp->o_len; i++) {
					if (!(i % 8)) {
						fprintf (outstream, "\n\t");
					}
					fprintf (outstream, "%2.2x ", (unsigned char)tmp->o_bytes[i]);
				}
				inproc = 0;
				fprintf (outstream, "\n");
				break;
			case ETCS9:
				fprintf (outstream, ".MESSAGE %*s\n", tmp->o_len, tmp->o_bytes);
				break;
			case ETCS10:
				fprintf (outstream, ".LOADLABELNAME %*s\n", tmp->o_len, tmp->o_bytes);
				break;
			case ETCS11:
				fprintf (outstream, ".LOADCODEMAPNAME %*s\n", tmp->o_len, tmp->o_bytes);
				break;
			case ETCS12:
				fprintf (outstream, ".GLOBALEND %*s\n", tmp->o_len, tmp->o_bytes);
				break;
			case ETCL0:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				switch (tmp->fn) {
				case I_J:
					fprintf (outstream, "\tJ L%d\n", tmp->opd);
					break;
				case I_CJ:
					fprintf (outstream, "\tCJ L%d\n", tmp->opd);
					break;
				case I_CALL:
					fprintf (outstream, "\tCALL L%d\n", tmp->opd);
					break;
				case I_LDC:
					i = tmp->opd;
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					if (tmp->opd < 0) {
						fprintf (outstream, "\t.LOADLABADDR L%d\n", i);
					} else {
						fprintf (outstream, "\t.LOADLABDIFF L%d L%d\n", i, tmp->opd);
					}
					break;
				default:
					fprintf (outstream, "\t.UNKNOWN\n");
					break;
				}
				break;
			case ETCL1:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				wsoff = tmp->opd;
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				loop_end = tmp->opd;
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				fprintf (outstream, "\t.LEND %d L%d L%d\n", wsoff, loop_end, tmp->opd);
				break;
			case ETCL2:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				wsoff = tmp->opd;
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				loop_end = tmp->opd;
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				fprintf (outstream, "\t.LEND3 %d L%d L%d\n", wsoff, loop_end, tmp->opd);
				break;
			case ETCL3:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				wsoff = tmp->opd;
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				loop_end = tmp->opd;
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				fprintf (outstream, "\t.LENDB %d L%d L%d\n", wsoff, loop_end, tmp->opd);
				break;
			case ETCL4:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				}
				fprintf (outstream, ".SETMS %d\n", tmp->opd);
				break;
			case ETCL5:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				} else {
					int msp_offset, count, z;

					msp_offset = tmp->opd;
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					count = tmp->opd;
					fprintf (outstream, ".MOBILEINIT %d %d\n", msp_offset, count);
					for (z=0; z<count; z++) {
						int slot_offset, data_offset;

						tmp = tmp->next;
						if (!tmp) {
							fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
							fclose (outstream);
							return -1;
						}
						slot_offset = tmp->opd;
						tmp = tmp->next;
						if (!tmp) {
							fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
							fclose (outstream);
							return -1;
						}
						data_offset = tmp->opd;
						fprintf (outstream, ".MOBILEINITPAIR %d %d\n", slot_offset, data_offset);
					}
				}
				break;
			case ETCL6:
				{
					int tcoff_tag = *(int *)tmp->o_bytes;
					int tcoff_len = tmp->o_len - sizeof (int);
					unsigned char *tcoff_data = (unsigned char *)tmp->o_bytes + sizeof (int);
					unsigned char *outbuf = smalloc ((tcoff_len * 4) + 64);
					unsigned char *sptr;
					int i;

					fprintf (outstream, ".TCOFF %d \"%s\" %d [", tcoff_tag,
						tcoff_tag_names[(tcoff_tag <= TCOFF_MAX_TAG) ? tcoff_tag : TCOFF_INVALID_TAG], tcoff_len);
					i = tcoff_len;
					sptr = tcoff_data;
					while (i) {
						if ((*sptr >= ' ') && !(*sptr & 0x80)) {
							fprintf (outstream, "%c", *sptr);
						} else {
							fprintf (outstream, "\\x%2.2x", *sptr);
						}
						sptr++, i--;
					}
					fprintf (outstream, "]\n");
					sfree (outbuf);
				}
				break;
			case ETCL7:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				} else {
					int mpp_offset = tmp->opd;
					int wsmaplab;

					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					wsmaplab = tmp->opd;
					if (mpp_offset != OCC21_NO_SLOT) {
						fprintf (outstream, ".LOADWSMAP %d L%d\n", mpp_offset, wsmaplab);
					} else {
						fprintf (outstream, ".LOADWSMAP L%d\n", wsmaplab);
					}
				}
				break;
			case ETCL8:
				tmp = tmp->next;
				if (!tmp) {
					fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
					fclose (outstream);
					return -1;
				} else {
					int mpp_offset = tmp->opd;
					int wsmaplab;

					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					wsmaplab = tmp->opd;
					if (mpp_offset != OCC21_NO_SLOT) {
						fprintf (outstream, ".UNLOADWSMAP %d L%d\n", mpp_offset, wsmaplab);
					} else {
						fprintf (outstream, ".UNLOADWSMAP L%d\n", wsmaplab);
					}
				}
				break;
			default:
				fprintf (stderr, "%s: unknown ETC code %d in input\n", progname, tmp->opd);
				res = -1;
				break;
			}
		}
	}
	fclose (outstream);
	return res;
}
/*}}}*/
/*{{{  static int dump_secondary (int esfunc, char *buffer)*/
/*
 *	dumps a secondary instruction
 */
static int dump_secondary (int esfunc, char *buffer)
{
	switch (esfunc) {
	case I_XWORD:
		sprintf (buffer, "\tXWORD");
		break;
	case I_UNPACKSN:
		sprintf (buffer, "\tUNPACKSN");
		break;
	case I_ROUNDSN:
		sprintf (buffer, "\tROUNDSN");
		break;
	case I_POSTNORMSN:
		sprintf (buffer, "\tPOSTNORMSN");
		break;
	case I_REV:
		sprintf (buffer, "\tREV");
		break;
	case I_POP:
		sprintf (buffer, "\tPOP");
		break;
	case I_RET:
		sprintf (buffer, "\tRET");
		break;
	case I_LDPI:
		sprintf (buffer, "\tLDPI");
		break;
	case I_GAJW:
		sprintf (buffer, "\tGAJW");
		break;
	case I_GCALL:
		sprintf (buffer, "\tGCALL");
		break;
	case I_MINT:
		sprintf (buffer, "\tMINT");
		break;
	case I_LEND:
		sprintf (buffer, "\tLEND");
		break;
	case I_CSUB0:
		sprintf (buffer, "\tCSUB0");
		break;
	case I_CCNT1:
		sprintf (buffer, "\tCCNT1");
		break;
	case I_SETERR:
		sprintf (buffer, "\tSETERR");
		break;
	case I_STOPERR:
		sprintf (buffer, "\tSTOPERR");
		break;
	case I_TESTERR:
		sprintf (buffer, "\tTESTERR");
		break;
	case I_BSUB:
		sprintf (buffer, "\tBSUB");
		break;
	case I_WSUB:
		sprintf (buffer, "\tWSUB");
		break;
	case I_WSUBDB:
		sprintf (buffer, "\tWSUBDB");
		break;
	case I_BCNT:
		sprintf (buffer, "\tBCNT");
		break;
	case I_WCNT:
		sprintf (buffer, "\tWCNT");
		break;
	case I_LB:
		sprintf (buffer, "\tLB");
		break;
	case I_SB:
		sprintf (buffer, "\tSB");
		break;
	case I_AND:
		sprintf (buffer, "\tAND");
		break;
	case I_OR:
		sprintf (buffer, "\tOR");
		break;
	case I_XOR:
		sprintf (buffer, "\tXOR");
		break;
	case I_ADD:
		sprintf (buffer, "\tADD");
		break;
	case I_SUB:
		sprintf (buffer, "\tSUB");
		break;
	case I_MUL:
		sprintf (buffer, "\tMUL");
		break;
	case I_DIV:
		sprintf (buffer, "\tDIV");
		break;
	case I_REM:
		sprintf (buffer, "\tREM");
		break;
	case I_DIFF:
		sprintf (buffer, "\tDIFF");
		break;
	case I_SUM:
		sprintf (buffer, "\tSUM");
		break;
	case I_PROD:
		sprintf (buffer, "\tPROD");
		break;
	case I_NOT:
		sprintf (buffer, "\tNOT");
		break;
	case I_SHL:
		sprintf (buffer, "\tSHL");
		break;
	case I_SHR:
		sprintf (buffer, "\tSHR");
		break;
	case I_GT:
		sprintf (buffer, "\tGT");
		break;
	case I_XDBLE:
		sprintf (buffer, "\tXDBLE");
		break;
	case I_CWORD:
		sprintf (buffer, "\tCWORD");
		break;
	case I_CSNGL:
		sprintf (buffer, "\tCSNGL");
		break;
	case I_LADD:
		sprintf (buffer, "\tLADD");
		break;
	case I_LSUB:
		sprintf (buffer, "\tLSUB");
		break;
	case I_LSUM:
		sprintf (buffer, "\tLSUM");
		break;
	case I_LDIFF:
		sprintf (buffer, "\tLDIFF");
		break;
	case I_LMUL:
		sprintf (buffer, "\tLMUL");
		break;
	case I_LSHL:
		sprintf (buffer, "\tLSHL");
		break;
	case I_LSHR:
		sprintf (buffer, "\tLSHR");
		break;
	case I_MOVE:
		sprintf (buffer, "\tLMOVE");
		break;
	case I_LDIV:
		sprintf (buffer, "\tLDIV");
		break;
	case I_NORM:
		sprintf (buffer, "\tNORM");
		break;
	case I_STARTP:
		sprintf (buffer, "\tSTARTP");
		break;
	case I_ENDP:
		sprintf (buffer, "\tENDP");
		break;
	case I_RUNP:
		sprintf (buffer, "\tRUNP");
		break;
	case I_STOPP:
		sprintf (buffer, "\tSTOPP");
		break;
	case I_IN:
		sprintf (buffer, "\tIN");
		break;
	case I_IN8:
		sprintf (buffer, "\tIN8");
		break;
	case I_IN32:
		sprintf (buffer, "\tIN32");
		break;
	case I_OUT:
		sprintf (buffer, "\tOUT");
		break;
	case I_OUT8:
		sprintf (buffer, "\tOUT8");
		break;
	case I_OUT32:
		sprintf (buffer, "\tOUT32");
		break;
	case I_OUTWORD:
		sprintf (buffer, "\tOUTWORD");
		break;
	case I_OUTBYTE:
		sprintf (buffer, "\tOUTBYTE");
		break;
	case I_ALTWT:
		sprintf (buffer, "\tALTWT");
		break;
	case I_DISS:
		sprintf (buffer, "\tDISS");
		break;
	case I_ENBC:
		sprintf (buffer, "\tENBC");
		break;
	case I_DISC:
		sprintf (buffer, "\tDISC");
		break;
	case I_TIN:
		sprintf (buffer, "\tTIN");
		break;
	case I_TALTWT:
		sprintf (buffer, "\tTALTWT");
		break;
	case I_ENBT:
		sprintf (buffer, "\tENBT");
		break;
	case I_DIST:
		sprintf (buffer, "\tDIST");
		break;
	case I_SAVEL:
		sprintf (buffer, "\tSAVEL");
		break;
	case I_STLB:
		sprintf (buffer, "\tSTLB");
		break;
	case I_STLF:
		sprintf (buffer, "\tSTLF");
		break;
	case I_TRAP:
		sprintf (buffer, "\tTRAP");
		break;
	case I_ALT:
		sprintf (buffer, "\tALT");
		break;
	case I_ALTEND:
		sprintf (buffer, "\tALTEND");
		break;
	case I_ENBS:
		sprintf (buffer, "\tENBS");
		break;
	case I_LDTIMER:
		sprintf (buffer, "\tLDTIMER");
		break;
	case I_TALT:
		sprintf (buffer, "\tTALT");
		break;
	case I_LDINF:
		sprintf (buffer, "\tLDINF");
		break;
	case I_CFLERR:
		sprintf (buffer, "\tCFLERR");
		break;
	case I_DUP:
		sprintf (buffer, "\tDUP");
		break;
	case I_FMUL:
		sprintf (buffer, "\tFMUL");
		break;
	case I_FPRZ:
		sprintf (buffer, "\tFPRZ");
		break;
	case I_FPRM:
		sprintf (buffer, "\tFPRM");
		break;
	case I_FPRP:
		sprintf (buffer, "\tFPRP");
		break;
	case I_FPRN:
		sprintf (buffer, "\tFPRN");
		break;
	case I_FPCHKI32:
		sprintf (buffer, "\tFPCHKI32");
		break;
	case I_FPCHKI64:
		sprintf (buffer, "\tFPCHKI64");
		break;
	case I_FPTESTERR:
		sprintf (buffer, "\tFPTESTERR");
		break;
	case I_FPCHKERR:
		sprintf (buffer, "\tFPCHKERR");
		break;
	case I_FPREV:
		sprintf (buffer, "\tFPREV");
		break;
	case I_FPDUP:
		sprintf (buffer, "\tFPDUP");
		break;
	case I_FPLDNLDB:
		sprintf (buffer, "\tFPLDNLDB");
		break;
	case I_FPLDNLSN:
		sprintf (buffer, "\tFPLDNLSN");
		break;
	case I_FPLDNLADDDB:
		sprintf (buffer, "\tFPLDNLADDDB");
		break;
	case I_FPLDNLADDSN:
		sprintf (buffer, "\tFPLDNLADDSN");
		break;
	case I_FPLDNLMULDB:
		sprintf (buffer, "\tFPLDNLMULDB");
		break;
	case I_FPLDNLMULSN:
		sprintf (buffer, "\tFPLDNLMULSN");
		break;
	case I_FPLDNLDBI:
		sprintf (buffer, "\tFPLDNLDBI");
		break;
	case I_FPLDNLSNI:
		sprintf (buffer, "\tFPLDNLSNI");
		break;
	case I_FPSTNLDB:
		sprintf (buffer, "\tFPSTNLDB");
		break;
	case I_FPSTNLSN:
		sprintf (buffer, "\tFPSTNLSN");
		break;
	case I_FPSTNLI32:
		sprintf (buffer, "\tFPSTNLI32");
		break;
	case I_FPLDZEROSN:
		sprintf (buffer, "\tFPLDZEROSN");
		break;
	case I_FPLDZERODB:
		sprintf (buffer, "\tFPLDZERODB");
		break;
	case I_FPADD:
		sprintf (buffer, "\tFPADD");
		break;
	case I_FPSUB:
		sprintf (buffer, "\tFPSUB");
		break;
	case I_FPMUL:
		sprintf (buffer, "\tFPMUL");
		break;
	case I_FPDIV:
		sprintf (buffer, "\tFPDIV");
		break;
	case I_FPNAN:
		sprintf (buffer, "\tFPNAN");
		break;
	case I_FPNOTFINITE:
		sprintf (buffer, "\tFPNOTFINITE");
		break;
	case I_FPORDERED:
		sprintf (buffer, "\tFPORDERED");
		break;
	case I_FPGT:
		sprintf (buffer, "\tFPGT");
		break;
	case I_FPEQ:
		sprintf (buffer, "\tFPEQ");
		break;
	case I_FPI32TOR64:
		sprintf (buffer, "\tFPI32TOR64");
		break;
	case I_FPI32TOR32:
		sprintf (buffer, "\tFPI32TOR32");
		break;
	case I_FPB32TOR64:
		sprintf (buffer, "\tFPB32TOR64");
		break;
	case I_FPINT:
		sprintf (buffer, "\tFPINT");
		break;
	case I_FPRTOI32:
		sprintf (buffer, "\tFPRTOI32");
		break;
	case I_FPREMSTEP:
		sprintf (buffer, "\tFPREMSTEP");
		break;
	case I_FPREM:
		sprintf (buffer, "\tFPREM");
		break;
	case I_FPREMFIRST:
		sprintf (buffer, "\tFPREMFIRST");
		break;
	case I_FPSQRT:
		sprintf (buffer, "\tFPSQRT");
		break;
	case I_FPABS:
		sprintf (buffer, "\tFPABS");
		break;
	case I_FPEXPDEC32:
		sprintf (buffer, "\tFPEXPDEC32");
		break;
	case I_FPEXPINC32:
		sprintf (buffer, "\tFPEXPINC32");
		break;
	case I_FPMULBY2:
		sprintf (buffer, "\tFPMULBY2");
		break;
	case I_FPDIVBY2:
		sprintf (buffer, "\tFPDIVBY2");
		break;
	case I_FPR32TOR64:
		sprintf (buffer, "\tFPR32TOR64");
		break;
	case I_FPR64TOR32:
		sprintf (buffer, "\tFPR64TOR32");
		break;
	case I_MALLOC:
		sprintf (buffer, "\tMALLOC");
		break;
	case I_MRELEASE:
		sprintf (buffer, "\tMRELEASE");
		break;
	case I_MRELEASEP:
		sprintf (buffer, "\tMRELEASEP");
		break;
	case I_MNEW:
		sprintf (buffer, "\tMNEW");
		break;
	case I_MFREE:
		sprintf (buffer, "\tMFREE");
		break;
	case I_NULL:
		sprintf (buffer, "\tNULL");
		break;
	case I_MIN:
		sprintf (buffer, "\tMIN");
		break;
	case I_MOUT:
		sprintf (buffer, "\tMOUT");
		break;
	case I_MIN64:
		sprintf (buffer, "\tMIN64");
		break;
	case I_MOUT64:
		sprintf (buffer, "\tMOUT64");
		break;
	case I_XABLE:
		sprintf (buffer, "\tXABLE");
		break;
	case I_XIN:
		sprintf (buffer, "\tXIN");
		break;
	case I_XMIN:
		sprintf (buffer, "\tXMIN");
		break;
	case I_XMIN64:
		sprintf (buffer, "\tXMIN64");
		break;
	case I_XEND:
		sprintf (buffer, "\tXEND");
		break;
	case I_NDISC:
		sprintf (buffer, "\tNDISC");
		break;
	case I_NDIST:
		sprintf (buffer, "\tNDIST");
		break;
	case I_NDISS:
		sprintf (buffer, "\tNDISS");
		break;
	case I_ENBC3:
		sprintf (buffer, "\tENBC3");
		break;
	case I_ENBT3:
		sprintf (buffer, "\tENBT3");
		break;
	case I_ENBS3:
		sprintf (buffer, "\tENBS3");
		break;
	case I_EXTIN:
		sprintf (buffer, "\tEXTIN");
		break;
	case I_EXTOUT:
		sprintf (buffer, "\tEXTOUT");
		break;
	case I_EXTVRFY:
		sprintf (buffer, "\tEXTVRFY");
		break;
	case I_EXTENBC:
		sprintf (buffer, "\tEXTENBC");
		break;
	case I_EXTNDISC:
		sprintf (buffer, "\tEXTNDISC");
		break;
	case I_EXTMIN:
		sprintf (buffer, "\tEXTMIN");
		break;
	case I_EXTMOUT:
		sprintf (buffer, "\tEXTMOUT");
		break;
	case I_EXTMIN64:
		sprintf (buffer, "\tEXTMIN64");
		break;
	case I_EXTMOUT64:
		sprintf (buffer, "\tEXTMOUT64");
		break;
	case I_EXTMINN:
		sprintf (buffer, "\tEXTMINN");
		break;
	case I_EXTMOUTN:
		sprintf (buffer, "\tEXTMOUTN");
		break;
	case I_GETPRI:
		sprintf (buffer, "\tGETPRI");
		break;
	case I_SETPRI:
		sprintf (buffer, "\tSETPRI");
		break;
	case I_MINN:
		sprintf (buffer, "\tMINN");
		break;
	case I_MOUTN:
		sprintf (buffer, "\tMOUTN");
		break;
	case I_XMINN:
		sprintf (buffer, "\tXMINN");
		break;
	case I_XSTL:
		sprintf (buffer, "\tXSTL");
		break;
	case I_XSTLN:
		sprintf (buffer, "\tXSTLN");
		break;
	case I_NCALL:
		sprintf (buffer, "\tNCALL");
		break;
	case I_NRET:
		sprintf (buffer, "\tNRET");
		break;
	case I_NWSADJ:
		sprintf (buffer, "\tNWSADJ");
		break;
	case I_NSTARTP:
		sprintf (buffer, "\tNSTARTP");
		break;
	case I_NNEG:
		sprintf (buffer, "\tNNEG");
		break;
	case I_NLW:
		sprintf (buffer, "\tNLW");
		break;
	case I_NSW:
		sprintf (buffer, "\tNSW");
		break;
	case I_NALTEND:
		sprintf (buffer, "\tNALTEND");
		break;
	case I_NMWENB:
		sprintf (buffer, "\tNMWENB");
		break;
	case I_NMWDIS:
		sprintf (buffer, "\tNMWDIS");
		break;
	case I_NMWALTWT:
		sprintf (buffer, "\tNMWALTWT");
		break;
	case I_NMWALT:
		sprintf (buffer, "\tNMWALT");
		break;
	case I_NMWALTEND:
		sprintf (buffer, "\tNMWALTEND");
		break;
	case I_FPPOP:
		sprintf (buffer, "\tFPPOP");
		break;
	case I_MWS_BINIT:
		sprintf (buffer, "\tMWS_BINIT");
		break;
	case I_MWS_PBRILNK:
		sprintf (buffer, "\tMWS_PBRILNK");
		break;
	case I_MWS_PBRULNK:
		sprintf (buffer, "\tMWS_PBRULNK");
		break;
	case I_MWS_PPILNK:
		sprintf (buffer, "\tMWS_PPILNK");
		break;
	case I_MWS_PBENROLL:
		sprintf (buffer, "\tMWS_PBENROLL");
		break;
	case I_MWS_PBRESIGN:
		sprintf (buffer, "\tMWS_PBRESIGN");
		break;
	case I_MWS_PBADJSYNC:
		sprintf (buffer, "\tMWS_PBADJSYNC");
		break;
	case I_MWS_SYNC:
		sprintf (buffer, "\tMWS_SYNC");
		break;
	case I_MWS_ALTLOCK:
		sprintf (buffer, "\tMWS_ALTLOCK");
		break;
	case I_MWS_ALTUNLOCK:
		sprintf (buffer, "\tMWS_ALTUNLOCK");
		break;
	case I_MWS_ALT:
		sprintf (buffer, "\tMWS_ALT");
		break;
	case I_MWS_ALTEND:
		sprintf (buffer, "\tMWS_ALTEND");
		break;
	case I_MWS_ENB:
		sprintf (buffer, "\tMWS_ENB");
		break;
	case I_MWS_DIS:
		sprintf (buffer, "\tMWS_DIS");
		break;
	case I_MWS_ALTPOSTLOCK:
		sprintf (buffer, "\tMWS_ALTPOSTLOCK");
		break;
	case I_MWS_PPBASEOF:
		sprintf (buffer, "\tMWS_PPBASEOF");
		break;
	case I_MWS_PPPAROF:
		sprintf (buffer, "\tMWS_PPPAROF");
		break;
	case I_IOR:
		sprintf (buffer, "\tIOR");
		break;
	case I_IOW:
		sprintf (buffer, "\tIOW");
		break;
	case I_IOR8:
		sprintf (buffer, "\tIOR8");
		break;
	case I_IOW8:
		sprintf (buffer, "\tIOW8");
		break;
	case I_IOR16:
		sprintf (buffer, "\tIOR16");
		break;
	case I_IOW16:
		sprintf (buffer, "\tIOW16");
		break;
	case I_IOR32:
		sprintf (buffer, "\tIOR32");
		break;
	case I_IOW32:
		sprintf (buffer, "\tIOW32");
		break;
	case I_PROC_ALLOC:
		sprintf (buffer, "\tPROC_ALLOC");
		break;
	case I_PROC_PARAM:
		sprintf (buffer, "\tPROC_PARAM");
		break;
	case I_PROC_MT_COPY:
		sprintf (buffer, "\tPROC_MT_COPY");
		break;
	case I_PROC_MT_MOVE:
		sprintf (buffer, "\tPROC_MT_MOVE");
		break;
	case I_PROC_START:
		sprintf (buffer, "\tPROC_START");
		break;
	case I_PROC_END:
		sprintf (buffer, "\tPROC_END");
		break;
	case I_GETAFF:
		sprintf (buffer, "\tGETAFF");
		break;
	case I_SETAFF:
		sprintf (buffer, "\tSETAFF");
		break;
	case I_GETPAS:
		sprintf (buffer, "\tGETPAS");
		break;
	case I_MT_ALLOC:
		sprintf (buffer, "\tMT_ALLOC");
		break;
	case I_MT_RELEASE:
		sprintf (buffer, "\tMT_RELEASE");
		break;
	case I_MT_CLONE:
		sprintf (buffer, "\tMT_CLONE");
		break;
	case I_MT_IN:
		sprintf (buffer, "\tMT_IN");
		break;
	case I_MT_OUT:
		sprintf (buffer, "\tMT_OUT");
		break;
	case I_MT_XCHG:
		sprintf (buffer, "\tMT_XCHG");
		break;
	case I_MT_LOCK:
		sprintf (buffer, "\tMT_LOCK");
		break;
	case I_MT_UNLOCK:
		sprintf (buffer, "\tMT_UNLOCK");
		break;
	case I_MT_ENROLL:
		sprintf (buffer, "\tMT_ENROLL");
		break;
	case I_MT_RESIGN:
		sprintf (buffer, "\tMT_RESIGN");
		break;
	case I_MT_SYNC:
		sprintf (buffer, "\tMT_SYNC");
		break;
	case I_MT_XIN:
		sprintf (buffer, "\tMT_XIN");
		break;
	case I_MT_XOUT:
		sprintf (buffer, "\tMT_XOUT");
		break;
	case I_MT_XXCHG:
		sprintf (buffer, "\tMT_XXCHG");
		break;
	case I_MT_DCLONE:
		sprintf (buffer, "\tMT_DCLONE");
		break;
	case I_MT_BIND:
		sprintf (buffer, "\tMT_BIND");
		break;
	case I_MB:
		sprintf (buffer, "\tMB");
		break;
	case I_RMB:
		sprintf (buffer, "\tRMB");
		break;
	case I_WMB:
		sprintf (buffer, "\tWMB");
		break;
	case I_CB:
		sprintf (buffer, "\tCB");
		break;
	case I_CBU:
		sprintf (buffer, "\tCBU");
		break;
	case I_CIR:
		sprintf (buffer, "\tCIR");
		break;
	case I_CIRU:
		sprintf (buffer, "\tCIRU");
		break;
	case I_CS:
		sprintf (buffer, "\tCS");
		break;
	case I_CSU:
		sprintf (buffer, "\tCSU");
		break;
	case I_LBX:
		sprintf (buffer, "\tLBX");
		break;
	case I_LS:
		sprintf (buffer, "\tLS");
		break;
	case I_LSX:
		sprintf (buffer, "\tLSX");
		break;
	case I_SS:
		sprintf (buffer, "\tSS");
		break;
	case I_SSUB:
		sprintf (buffer, "\tSSUB");
		break;
	case I_XBWORD:
		sprintf (buffer, "\tXBWORD");
		break;
	case I_XSWORD:
		sprintf (buffer, "\tXSWORD");
		break;
	case I_EXT_MT_IN:
		sprintf (buffer, "\tEXT_MT_IN");
		break;
	case I_EXT_MT_OUT:
		sprintf (buffer, "\tEXT_MT_OUT");
		break;
	case I_MT_RESIZE:
		sprintf (buffer, "\tMT_RESIZE");
		break;
	default:
		sprintf (buffer, "\tUnsupported %d", esfunc);
		break;
	}
	return 0;
}
/*}}}*/
/*{{{  static int dump_primary (int prim, int operand, char *buffer)*/
/*
 *	dumps a primary instruction
 */
static int dump_primary (int prim, int operand, char *buffer)
{
	switch (prim) {
	case I_LDL:
		sprintf (buffer, "\tLDL %d", operand);
		break;
	case I_LDLP:
		sprintf (buffer, "\tLDLP %d", operand);
		break;
	case I_STL:
		sprintf (buffer, "\tSTL %d", operand);
		break;
	case I_LDNL:
		sprintf (buffer, "\tLDNL %d", operand);
		break;
	case I_LDNLP:
		sprintf (buffer, "\tLDNLP %d", operand);
		break;
	case I_STNL:
		sprintf (buffer, "\tSTNL %d", operand);
		break;
	case I_EQC:
		sprintf (buffer, "\tEQC %d", operand);
		break;
	case I_LDC:
		sprintf (buffer, "\tLDC %d    -- #%8.8x", operand, (unsigned int)operand);
		break;
	case I_ADC:
		sprintf (buffer, "\tADC %d", operand);
		break;
	case I_AJW:
		sprintf (buffer, "\tAJW %d", operand);
		break;
	case I_J:
		sprintf (buffer, "\tJ %d", operand);
		break;
	case I_CJ:
		sprintf (buffer, "\tCJ %d", operand);
		break;
	case I_CALL:
		sprintf (buffer, "\tCALL %d", operand);
		break;
	default:
		sprintf (buffer, "\tUNKNOWN %d", operand);
		break;
	}
	return 0;
}
/*}}}*/
/*{{{  int dump_comments (etc_chain *chain)*/
/*
 *	dumps comments to stdout
 */
int dump_comments (etc_chain *chain)
{
	etc_chain *tmp;

	for (tmp = chain; tmp; tmp = tmp->next) {
		if (tmp->fn >= I_OPR && tmp->opd == ETCL6) {
			int tcoff_tag = *(int *)tmp->o_bytes;
			int tcoff_len = tmp->o_len - sizeof (int);
			unsigned char *p = (unsigned char *)tmp->o_bytes + sizeof (int);
			unsigned long comment_len;

			if (tcoff_tag != COMMENT_TAG)
				continue;

			/* Three longs precede a COMMENT; the last is the length. */
			tcoff_getl (&p, &tcoff_len);
			tcoff_getl (&p, &tcoff_len);
			comment_len = tcoff_getl (&p, &tcoff_len);
			if (comment_len < 0 || comment_len > tcoff_len) {
				fprintf (stderr, "%s: COMMENT has invalid length %d %d; skipping\n", progname, (int)comment_len, (int)tcoff_len);
				continue;
			}

			fwrite (p, 1, comment_len, stdout);
			printf ("\n");
		}
	}

	return 0;
}
/*}}}*/
/*{{{  static long tcoff_getl (unsigned char **s)*/
/*
 *	read an long in TCOFF format
 *
 *	This is the inverse of tcoff_putl in tcoff_io.c.
 *
 */
static long tcoff_getl (unsigned char **s, int *len)
{
	long out = 0;
	int is_negative = 0;
	unsigned char *p = *s;

	if (*len < 1)
		return 0;
	if (*p == 255) {
		is_negative = 1;
		p++;
		(*len)--;
	}

	if (*len < 1)
		return 0;
	if (*p <= 250) {
		out = *p++;
		(*len)--;
	} else {
		int bytes = 1 << (*p - 251);
		int i;
		p++;
		(*len)--;
		for (i = 0; i < bytes; i++) {
			if (*len < 1)
				return 0;
			out |= (*p++) << (i * 8);
			(*len)--;
		}
	}

	if (is_negative)
		out = -out;
	*s = p;
	return out;
}
/*}}}*/

