/*
 *	tceread.c - TCE reading stuff
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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "main.h"
#include "support.h"
#include "structs.h"
#define WANT_TCOFF_TAGNAMES
#include "tcoff.h"
#undef WANT_TCOFF_TAGNAMES
#include "transputer.h"
#include "trancomm.h"


/*}}}*/


/*{{{  private variables/forward-decls */
static char *tcefile_filename;
static char *tcefile_bytes;
static int tcefile_fd;
static size_t tcefile_length, tcefile_ptr;


static int tceread_number (void);
static void tceread_nextblocks (etc_chain **head, etc_chain **tail);
static char *tceread_barray (int amount);
static etc_chain *new_etc_chain (void);
/*}}}*/


/*{{{  static int tceread_number (void)*/
/*
 *	returns a number from the input
 */
static int tceread_number (void)
{
	unsigned int n;
	int neg;
	int len;
	char *arry;
	unsigned char ch;

	if (tcefile_ptr == tcefile_length) {
		fprintf (stderr, "%s: unexpected EOF while processing %s\n", progname, tcefile_filename);
		exit (1);
	}
	ch = tcefile_bytes[tcefile_ptr];
	n = 0;
	n += ch;
	tcefile_ptr++;
	if (n == 255) {
		neg = 1;
		n = (unsigned int)tcefile_bytes[tcefile_ptr];
		tcefile_ptr++;
	} else {
		neg = 0;
	}
	if (n <= 250) {
		/* nothing */
	} else if (n == 255) {
		fprintf (stderr, "%s: tceread_number: bad TCE file %s (%ul/%ul)\n", progname, tcefile_filename, tcefile_ptr, tcefile_length);
		exit (1);
	} else {
		unsigned char *xarry;

		len = (1 << (n - 251));
		if ((len < 0) || (len > 4)) {
			fprintf (stderr, "%s: tceread_number: length well out-of-range.  len = %d, n = %u\n", progname, len, n);
			exit (1);
		}
		n = 0;
		arry = tceread_barray (len);
		if (!arry) {
			fprintf (stderr, "%s: tceread_number: unexpected NULL\n", progname);
			exit (1);
		}
		xarry = (unsigned char *)arry;
		/* bit grotty, but solves some byteorder issues -- that i'd rather not fix in the compiler..! */
		switch (len) {
		case 1:
			n = xarry[0];
			break;
		case 2:
			n = (xarry[1] << 8) | xarry[0];
			break;
		case 4:
			n = (xarry[3] << 24) | (xarry[2] << 16) | (xarry[1] << 8) | xarry[0];
			break;
		}
		/* memcpy ((char *)&n, arry, len); */
	}
	if (neg) {
		n ^= n;
	}
	return (int)n;
}
/*}}}*/
/*{{{  static etc_chain *new_etc_chain (void)*/
/*
 *	allocates a new etc_chain structure
 */
static etc_chain *new_etc_chain (void)
{
	etc_chain *tmp;

	tmp = (etc_chain *)smalloc (sizeof (etc_chain));
	tmp->next = NULL;
	tmp->fn = 0;
	tmp->opd = 0;
	tmp->o_bytes = NULL;
	tmp->o_len = 0;
	return tmp;
}
/*}}}*/
/*{{{  etc_chain *read_etc_chain (void)*/
/*
 *	builds the ETC chain
 */
etc_chain *read_etc_chain (void)
{
	etc_chain *head, *tail;
	etc_chain *xhead, *xtail;
	int running;

	head = tail = NULL;
	running = 1;
	while (running) {
		xhead = xtail = NULL;
		tceread_nextblocks (&xhead, &xtail);

		if (!xhead && !xtail) {
			continue;
		} else if (!xhead && (xtail == (etc_chain *)-1)) {
			running = 0;
		} else {
			if (!head) {
				head = xhead;
				tail = xtail;
			} else {
				tail->next = xhead;
				tail = xtail;
			}
		}
	}
	return head;
}
/*}}}*/
/*{{{  static void dump_human_data (FILE *stream, unsigned char *ptr, int count)*/
/*
 *	dumps some human-readable stuff for diagnostics
 */
static void dump_human_data (FILE *stream, unsigned char *ptr, int count)
{
	int ilen = count;
	int idx = 0;

	while (ilen) {
		if ((ptr[idx] >= ' ') && !(ptr[idx] & 0x80)) {
			fprintf (stream, "%c", ptr[idx]);
		} else {
			fprintf (stream, "\\0x%2.2x", ptr[idx]);
		}
		idx++, ilen--;
	}
	return;
}
/*}}}*/
/*{{{  static void tceread_nextblocks (etc_chain **head, etc_chain **tail)*/
/*
 *	Returns the next bit of the TCE file
 */
static void tceread_nextblocks (etc_chain **head, etc_chain **tail)
{
	etc_chain *hblk, *tblk, *tmp;
	int tcoff_tag, rlen, textlen, nlen;
	char *code;
	unsigned char ch, lch;
	int i, fn, opd, building, lfn;

	hblk = tblk = NULL;
	/* read tcoff record */
	tcoff_tag = tceread_number ();
	rlen = tceread_number ();
	if (options.diagnostics) {
		fprintf (stderr, "nextblocks (%x,%x) --> ", (unsigned int)tcoff_tag, (unsigned int)rlen);
	}
	switch (tcoff_tag) {
	case LOAD_TEXT_TAG:
		textlen = tceread_number ();
		code = tceread_barray (textlen);
		if (options.diagnostics) {
			fprintf (stderr, "TCOFF LOAD_TEXT_TAG: %d bytes\n", textlen);
		}
		i = fn = opd = 0;
		while (i < textlen) {
			ch = (unsigned char)code[i++];
			fn = (ch >> 4);
			opd = opd | (ch & 0x0f);
			switch (fn) {
			case I_PFIX:
				opd <<= 4;
				break;
			case I_NFIX:
				opd = (int)((~(unsigned int)opd) << 4);
				break;
			default:
				tmp = new_etc_chain ();
				tmp->fn = fn;
				tmp->opd = opd;
				if (fn == I_OPR) {
					/*{{{  look for ETC specials that have some trailing bytes in them*/
					switch (opd) {
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
						nlen = 0;
						building = 1;
						while (building) {
							lch = (unsigned char)code[i++];
							lfn = ((lch >> 4) & 0x0f);
							nlen |= (lch & 0x0f);
							if (lfn == I_PFIX) {
								nlen = nlen << 4;
							} else if (lfn == I_LDC) {
								/* got it all */
								building = 0;
							} else {
								fprintf (stderr, "%s: tceread_nextblocks: bad TCE file %s (%lu/%lu)\n", progname, tcefile_filename,
										(unsigned long int)tcefile_ptr, (unsigned long int)tcefile_length);
								exit (1);
							}
						}
						tmp->o_bytes = smalloc (nlen + 4);
						memcpy (tmp->o_bytes, code + i, nlen);
						tmp->o_bytes[nlen] = '\0';
						tmp->o_bytes[nlen+1] = '\0';
						tmp->o_len = nlen;
						i += nlen;
#if 0
fprintf (stderr, "%s: tceread_nextblocks(): scooped up embedded ETC string of %d bytes [%s].  Next code-byte is 0x%2.2X\n", progname,
		nlen, tmp->o_bytes, code[i]);
#endif
						break;
					default:
						break;
					} /* switch */
					/*}}}*/
				} /* if */
				opd = 0;
				if (!hblk) {
					hblk = tblk = tmp;
				} else {
					tblk->next = tmp;
					tblk = tmp;
				}
				break;
			} /* switch */

		} /* while */

		tmp = new_etc_chain ();
		tmp->fn = I_OPR;
		tmp->opd = ETC0;
		if (!hblk) {
			hblk = tblk = tmp;
		} else {
			tblk->next = tmp;
			tblk = tmp;
		}

		tmp = new_etc_chain ();
		tmp->fn = I_LDC;
		tmp->opd = FINISH_OP;
		tblk->next = tmp;
		tblk = tmp;

		break;
	case END_MODULE_TAG:
		/* soak up tcoff */
		tceread_barray (rlen);
		if (options.diagnostics) {
			fprintf (stderr, "TCOFF END_MODULE_TAG: %d bytes\n", rlen);
		}
		*head = NULL;
		*tail = (etc_chain *)-1;
		return;
	default:
		{
			unsigned char *tcoff;

			/* soak up tcoff */
			tcoff = (unsigned char *)tceread_barray (rlen);
			if (options.diagnostics) {
				fprintf (stderr, "TCOFF: tag = %d (%s), length = %d bytes.  data = [", tcoff_tag,
					tcoff_tag_names[(tcoff_tag <= TCOFF_MAX_TAG) ? tcoff_tag : TCOFF_INVALID_TAG], rlen);
				dump_human_data (stderr, tcoff, rlen);
				fprintf (stderr, "]\n");
			}
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCL6;	/* tcoff record */
			tmp->o_len = rlen + sizeof (int);
			tmp->o_bytes = smalloc (tmp->o_len);
			memcpy (tmp->o_bytes, (char *)&tcoff_tag, sizeof (int));
			memcpy (tmp->o_bytes + sizeof (int), tcoff, rlen);
			if (!hblk) {
				hblk = tblk = tmp;
			} else {
				tblk->next = tmp;
				tblk = tmp;
			}
		}
		break;
	}
	*head = hblk;
	*tail = tblk;
	return;
}
/*}}}*/
/*{{{  static char *tceread_barray (int amount)*/
/*
 *	Returns a pointer to some bytes in the file
 */
static char *tceread_barray (int amount)
{
	char *ptr;

	if ((tcefile_ptr + amount) > tcefile_length) {
		fprintf (stderr, "%s: tceread_barray: bad TCE file %s (%lu/%lu/%d)\n", progname, tcefile_filename,
			(unsigned long int)tcefile_ptr, (unsigned long int)tcefile_length, amount);
		abort ();
		exit (1);
	}
	ptr = tcefile_bytes + tcefile_ptr;
	tcefile_ptr += amount;
	return ptr;
}
/*}}}*/
/*{{{  int open_tce_file (char *filename)*/
/*
 *	opens a TCE file
 */
int open_tce_file (char *filename)
{
	struct stat stbuf;
	int err;
	int i, j;

	tcefile_filename = filename;
#ifdef HOST_OS_IS_CYGWIN
	tcefile_fd = open (tcefile_filename, O_RDONLY | O_BINARY);
#else
	tcefile_fd = open (tcefile_filename, O_RDONLY);
#endif
	if (tcefile_fd < 0) {
		fprintf (stderr, "%s: unable to open %s: %s\n", progname, tcefile_filename, strerror (errno));
		return -1;
	}
	err = fstat (tcefile_fd, &stbuf);
	if (err < 0) {
		fprintf (stderr, "%s: unable to stat %s: %s\n", progname, tcefile_filename, strerror (errno));
		close (tcefile_fd);
		return -1;
	}
	tcefile_length = stbuf.st_size;
	tcefile_ptr = 0;
	tcefile_bytes = (char *)smalloc (tcefile_length);
	i = 0;
	while (i < tcefile_length) {
		j = read (tcefile_fd, (tcefile_bytes + i), (tcefile_length - i));
		if (j == 0) {
			fprintf (stderr, "%s: unexpected EOF in %s\n", progname, tcefile_filename);
			close (tcefile_fd);
			return -1;
		} else if (j == -1) {
			fprintf (stderr, "%s: read error while reading from %s: %s\n", progname, tcefile_filename, strerror (errno));
			close (tcefile_fd);
			return -1;
		}
		i += j;
	}
	if (i != tcefile_length) {
		fprintf (stderr, "%s: read too much from %s\n", progname, tcefile_filename);
		close (tcefile_fd);
		return -1;
	}
	return 0;
}
/*}}}*/
/*{{{  void close_tce_file (void)*/
/*
 *	closes the tce file
 */
void close_tce_file (void)
{
	free (tcefile_bytes);
	close (tcefile_fd);
	return;
}
/*}}}*/


