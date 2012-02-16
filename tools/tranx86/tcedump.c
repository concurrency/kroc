/*
 *	tcedump.c - TCE dumper
 *	Copyright (C) 2004 Christian L. Jacobsen <clj3@kent.ac.uk>, 
 *	                   Fred Barnes <frmb@kent.ac.uk>
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
//#define WANT_TCOFF_TAGNAMES
#include "tcoff.h"
//#undef WANT_TCOFF_TAGNAMES
#include "tcedump.h"

static int add_byte(char byte, sect *section) {/*{{{*/

	if(section->idx == section->alloc) {
		section->alloc += SECTION_SIZE_LEAP;
		section->ptr = realloc(section->ptr, section->alloc);
	}

	if(!section->ptr) {
		fprintf(stderr, "Could not reallocate memory (to %d bytes)\n", section->alloc);
		return -1;
	}

	section->ptr[section->idx] = byte;
	section->idx += 1;

	return 0;
}/*}}}*/

static int number_size(int num) {
	if (num < 0) {
		fprintf (stderr, "%s: FIXME: negative numbers\n", progname);
		abort ();
	}
	
	if(num <= 250) {
		return 1;
	} 
	
	if(num < (256)) {
		return 2;
	} 
	
	if(num < (65536)) {
		return 3;
	}

	//if(num < (4294967296)) {
		return 5;
	//}

	return 0;
}

static int write_number(int num, FILE *outstream) {
	if (num < 0) {
		fprintf (stderr, "%s: FIXME: negative numbers\n", progname);
		abort ();
	}

	if(num <= 250) {
		fputc(num, outstream);
		return 1;
	} 
	
	if(num < (256)) {
		fputc(PFX_BYTE, outstream);
		fputc(num, outstream);
		return 2;
	} 
	
	if(num < (65536)) {
		fputc(PFX_INT16, outstream);
		fputc((num & 0xff), outstream);
		fputc(((num >> 8) & 0xff), outstream);
		return 3;
	}

	//if(num < (4294967296)) {
		fputc(PFX_INT16, outstream);
		fputc(((num >> 24) & 0xff), outstream);
		fputc(((num >> 16) & 0xff), outstream);
		fputc(((num >> 8) & 0xff), outstream);
		fputc((num & 0xff), outstream);
		return 5;
	//}

	return 0;
}

static int write_section(sect *section, FILE *outstream) {/*{{{*/

	if(section->idx > 0) {
		fputc(LOAD_TEXT_TAG, outstream);
		write_number(section->idx + number_size(section->idx), outstream);
		write_number(section->idx, outstream);
		fwrite(section->ptr, sizeof(char), section->idx, outstream);
		section->idx = 0;
	}

	return 0;
}/*}}}*/

static int prefix(int opd, sect *section) {/*{{{*/
	int new_opd;

	if((opd < 16) && (opd >= 0)) {
		return opd;
	}
	if(opd >= 16) {
#if 0
		printf("pfix: opd: 0x%.1x (0x%.2x)\n", opd & 0xF, (I_PFIX << 4) | (opd & 0xF));
#endif
		new_opd = prefix(opd >> 4, section);
		add_byte((I_PFIX << 4) | (new_opd  & 0xf), section);
		return opd & 0xf;
	}
	if(opd < 0) {
#if 0
		printf("nfix: opd: 0x%.1x (0x%.2x)\n", opd & 0xF, (I_PFIX << 4) | (opd & 0xF));
#endif
		new_opd = prefix((~ opd) >> 4, section);
		add_byte((I_NFIX << 4) | (new_opd & 0xF), section);
		return opd & 0xf;
	}

	return opd; 
}/*}}}*/

/*
 * int dump_binary_etc (etc_chain *chain, char *filename)
 * dumps binary ETC to a file
 */
int dump_binary_etc (etc_chain *chain, char *filename)
{
	FILE *outstream;
	int i, res;
	etc_chain *tmp;
	sect section;

	/* Open the file */
	outstream = fopen (filename, "w");
	if (!outstream) {
		fprintf (stderr, "%s: unable to open %s for writing\n", progname, filename);
		return -1;
	}
	/* Allocate initial chunk of memory */
	section.alloc = SECTION_SIZE_LEAP;
	section.ptr = malloc(section.alloc);
	section.idx = 0;
	/* Start processing */
	res = 0;
	for (tmp=chain; tmp && !res; tmp=tmp->next) {
		if (tmp->fn < I_OPR) {
			add_byte ((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
		} else if (tmp->opd >= (signed int)ETC_MAX) {
			if ((tmp->opd == I_XSTL) || (tmp->opd == I_XSTLN)) {
				fprintf (stderr, "%s: XSTL/XSTLN unexpected, giving up!\n", progname);
				return -1;
			}
			add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
		} else {
			switch (tmp->opd) {
					/*{{{  ETC0*/
				case ETC0:
					if (!tmp->next) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					if(tmp->next->opd != FINISH_OP) {
						add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
					}
					tmp = tmp->next;
					switch (tmp->opd) {
						case FINISH_OP:
							/* This is not a real magic which appears in the instruction stream,
							 * it is added to the end when the instruction stream is loaded from
							 * a file by tceread.c.
							 */
							break;
						case I64TOREAL:
						case BOOLINVERT:
						case NOTPROCESS:
						case WIDENSHORT:
						case FPPOP:
						case STARTTABLE:
						case CONTRSPLIT:
						case CONTRJOIN:
						case CHECKNOTNULL:
						case SEMCLAIM:
						case SEMRELEASE:
						case SEMINIT:
						case RESCHEDULE:
						default:
							add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					}
					break;
					/*}}}*/
					/*{{{  ETC6, ETC7*/
				case ETC6:
				case ETC7:
					add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					break;
					/*}}}*/
					/*{{{  ETC1 - ETC14*/
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
					/* subop = tmp->opd - ETC0; */
					add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					break;
					/*}}}*/
					/*{{{  ETCS1 - ETCS12*/
				case ETCS1:
				case ETCS2:
				case ETCS3:
				case ETCS4:
				case ETCS5:
				case ETCS6:
				case ETCS7:
				case ETCS8:
				case ETCS9:
				case ETCS10:
				case ETCS11:
				case ETCS12:
					add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
					add_byte((I_LDC << 4) | prefix(tmp->o_len, &section), &section);
					for (i=0; i<tmp->o_len; i++) {
						add_byte((unsigned char)tmp->o_bytes[i], &section);
					}
					break;
					/*}}}*/
					/*{{{  ETCL0*/
				case ETCL0:
					add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					switch (tmp->fn) {
						case I_J:
						case I_CJ:
						case I_CALL:
							add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
							break;
						case I_LDC:
							i = tmp->opd;
							add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
							tmp = tmp->next;
							if (!tmp) {
								fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
								fclose (outstream);
								return -1;
							}
							if (tmp->opd < 0) {
								/* This does not seem to be consitent with the textual ETC dump */
								/* add_byte((I_LDC << 4) | prefix(i, &section), &section); */
								add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
							} else {
								/* This does not seem to be consitent with the textual ETC dump */
								/* add_byte((I_LDC << 4) | prefix(i, &section), &section); */
								add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
								//write_section(&section, outstream);
								//fclose (outstream);
								//abort();
							}
							break;
						default:
							fprintf (stderr, "%s: unexpected ETC code\n", progname);
							fclose (outstream);
							return -1;
					}
					break;
					/*}}}*/
					/*{{{  ETCL1*/
				case ETCL1:
					add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					//wsoff = tmp->opd;
					add_byte((I_LDLP << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					//loop_end = tmp->opd;
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					//fprintf (outstream, "\t.LEND %d L%d L%d\n", wsoff, loop_end, tmp->opd);
					break;
					/*}}}*/
					/*{{{  ETCL2*/
				case ETCL2:
					add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					//wsoff = tmp->opd;
					add_byte((I_LDLP << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					//loop_end = tmp->opd;
					add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					//fprintf (outstream, "\t.LEND3 %d L%d L%d\n", wsoff, loop_end, tmp->opd);
					break;
					/*}}}*/
					/*{{{  ETCL3*/
				case ETCL3:
					add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					//wsoff = tmp->opd;
					add_byte((I_LDLP << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					//loop_end = tmp->opd;
					add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					//fprintf (outstream, "\t.LENDB %d L%d L%d\n", wsoff, loop_end, tmp->opd);
					break;
					/*}}}*/
					/*{{{  ETCL4*/
				case ETCL4:
					add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
					tmp = tmp->next;
					if (!tmp) {
						fprintf (stderr, "%s: unexpected end of ETC chain\n", progname);
						fclose (outstream);
						return -1;
					}
					add_byte((I_LDC << 4) | prefix(tmp->opd, &section), &section);
					break;
					/*}}}*/
					/*{{{  ETCL5*/
				case ETCL5:
					add_byte((tmp->fn << 4) | prefix(tmp->opd, &section), &section);
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
					/*}}}*/
					/*{{{  ETCL6*/
				case ETCL6:
					{
						char tcoff_tag = *(int *)tmp->o_bytes;
						int tcoff_len = tmp->o_len - sizeof (int);
						unsigned char *tcoff_data = (unsigned char *)tmp->o_bytes + sizeof (int);
						/* Output any remaints of a section we have been building up */
						write_section(&section, outstream);
						/* Output the tag */
						fputc(tcoff_tag, outstream);
						/* Output the length */
						write_number(tcoff_len, outstream);
						/* Output the section from the data stored in the node */
						fwrite(tcoff_data, sizeof(char), tcoff_len, outstream);
						break;
					}
					/*}}}*/
					/*{{{  ETCL7*/
				case ETCL7:
					{
						/* FIXME! */
						break;
					}
					/*}}}*/
					/*{{{  ETCL8*/
				case ETCL8:
					{
						/* FIXME! */
						break;
					}
					/*}}}*/
					/*{{{  default -- error*/
				default:
					fprintf (stderr, "%s: (tcedump) unknown ETC code %d in input\n", progname, tmp->opd);
					res = -1;
					break;
					/*}}}*/
			}
		}
	}
	/* Write the last section if not yet written */
	write_section(&section, outstream);
	/* The TCEREAD routine does not seem to keep hold of the END_MODULE_TAGS */
	fputc(END_MODULE_TAG, outstream);
	write_number(0, outstream);
	/* Done */
	free(section.ptr);
	fclose(outstream);
	return res;
}

/*
 *	int dump_textual_etc (etc_chain *chain, char *filename)
 *	dumps ETC to text file
 */
int xxxdump_textual_etc (etc_chain *chain, char *filename)
{
	FILE *outstream;
	etc_chain *tmp;
	int res, inproc/*, subop, i*/;
	/*int wsoff, loop_end;*/
	char dumpstr[256];

	outstream = fopen (filename, "w");
	if (!outstream) {
		fprintf (stderr, "%s: unable to open %s for writing\n", progname, filename);
		return -1;
	}
	res = 0;
	inproc = 0;
	for (tmp=chain; tmp && !res; tmp=tmp->next) {
		if (tmp->fn < I_OPR) {
			//res = dump_primary (tmp->fn, tmp->opd, dumpstr);
			fprintf (outstream, "%s\n", dumpstr);
			continue;
		} else if (tmp->opd >= (signed int)ETC_MAX) {
			//res = dump_secondary (tmp->opd, dumpstr);
			switch (tmp->opd) {
			case I_XSTL:
				fprintf (outstream, "%s %d\n", dumpstr, tmp->fn - I_OPR);
				break;
			case I_XSTLN:
				fprintf (outstream, "%s %d\n", dumpstr, -(tmp->fn - I_OPR));
				break;
			default:
				fprintf (outstream, "%s\n", dumpstr);
				break;
			}
		} else {
			switch (tmp->opd) {
#if 0
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
#endif
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
#if 0
			case ETCL6:
				{
					int tcoff_tag = *(int *)tmp->o_bytes;
					int tcoff_len = tmp->o_len - sizeof (int);
					unsigned char *tcoff_data = tmp->o_bytes + sizeof (int);
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
			default:
				fprintf (stderr, "%s: unknown ETC code %d in input\n", progname, tmp->opd);
				res = -1;
				break;
#endif
			}
		}
	}
	fclose (outstream);
	return res;
}


