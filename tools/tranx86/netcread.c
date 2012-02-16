/*
 *	netcread.c - TCE reading stuff
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
static char *netcfile_filename;
static char *netcfile_bytes;
static int netcfile_fd;
static size_t netcfile_length, netcfile_ptr;


static void netcread_nextblocks (etc_chain **head, etc_chain **tail);
static etc_chain *new_etc_chain (void);
static char *netcread_nextline (void);
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
/*{{{  etc_chain *read_netc_chain (void)*/
/*
 *	builds the ETC chain
 */
etc_chain *read_netc_chain (void)
{
	etc_chain *head, *tail;
	etc_chain *xhead, *xtail;
	int running;

	head = tail = NULL;
	running = 1;
	while (running) {
		netcread_nextblocks (&xhead, &xtail);
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
/*{{{  static void add_to_chain (etc_chain **hptr, etc_chain **tptr, etc_chain *item)*/
/*
 *	adds an ETC entry to the specified ETC chain
 */
static void add_to_chain (etc_chain **hptr, etc_chain **tptr, etc_chain *item)
{
	if (!*hptr) {
		*hptr = *tptr = item;
	} else {
		(*tptr)->next = item;
		*tptr = item;
	}
	return;
}
/*}}}*/
/*{{{  static void netcread_nextblocks (etc_chain **head, etc_chain **tail)*/
/*
 *	Returns the next bit of the TCE file
 */
static void netcread_nextblocks (etc_chain **head, etc_chain **tail)
{
	etc_chain *hblk, *tblk, *tmp;
	char *line = NULL;
	char *origline;
	int goodline;
	char **bits;

	/* get next line from input */
	for (goodline = 0; !goodline; ) {
		line = netcread_nextline ();
		if (!line) {
			goodline = 1;
			break;		/* for() */
		}
		/* skip leading whitespace */
		for (; (*line == ' ') || (*line == '\t'); line++);
		if (*line == ';') {
			/* rest of the line is junk */
			continue;
		} else if (*line == '\0') {
			/* blank line */
			continue;
		}
		goodline = 1;
	}

	if (!line) {
		/* end of file */
		*head = NULL;
		*tail = (etc_chain *)-1;
		return;
	}

#if 0
fprintf (stderr, "netcread_nextblocks(): line is: %s\n", line);
#endif

	hblk = tblk = NULL;
	/* duplicate line before we pull it apart */
	origline = line;
	line = string_dup (origline);
	bits = split_stringsc (line, 0);

	if (!bits[0]) {
		fprintf (stderr, "netcread_nextblocks(): bad line: %s\n", origline);
		*head = hblk;
		*tail = tblk;
		return;
	}
	switch (bits[0][0]) {
	case '.':
		if (!strcmp (bits[0], ".setlabel")) {
			/*{{{  set label*/
			int lab;

			if (!bits[1] || (sscanf (bits[1], "%d", &lab) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC6;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = lab;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], ".byte")) {
			/*{{{  data bytes*/
			int i, blen;

			for (i=1, blen=0; bits[i]; i++) {
				char *ch;

				for (ch = bits[i]; (*ch != '\0'); ch++) {
					if ((ch[0] == '0') && (ch[1] == 'x')) {
						ch++;
						continue;
					}
					if (ch[0] == ',') {
						continue;
					}
					/* expect 2 hex characters */
					ch++;
					blen++;
				}
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCS8;
			tmp->o_len = blen;
			tmp->o_bytes = smalloc (blen);

			for (i=1, blen=0; bits[i]; i++) {
				char *ch;

				for (ch = bits[i]; (*ch != '\0'); ch++) {
					char rch = 0;

					if ((ch[0] == '0') && (ch[1] == 'x')) {
						ch++;
						continue;
					}
					if (ch[0] == ',') {
						continue;
					}
					/* expect 2 hex characters */
					if ((ch[0] >= '0') && (ch[0] <= '9')) {
						rch = ch[0] - '0';
					} else if ((ch[0] >= 'a') && (ch[0] <= 'f')) {
						rch = (ch[0] - 'a') + 10;
					} else if ((ch[0] >= 'A') && (ch[0] <= 'F')) {
						rch = (ch[0] - 'A') + 10;
					}
					rch <<= 4;
					ch++;
					if ((ch[0] >= '0') && (ch[0] <= '9')) {
						rch |= ch[0] - '0';
					} else if ((ch[0] >= 'a') && (ch[0] <= 'f')) {
						rch |= (ch[0] - 'a') + 10;
					} else if ((ch[0] >= 'A') && (ch[0] <= 'F')) {
						rch |= (ch[0] - 'A') + 10;
					}
					tmp->o_bytes[blen] = rch;
					blen++;
				}
			}

			if (!hblk) {
				hblk = tblk = tmp;
			} else {
				tblk->next = tmp;
				tblk = tmp;
			}
			/*}}}*/
		} else if (!strcmp (bits[0], ".jentry")) {
			/*{{{  jump entry (program entry-point)*/
			if (!bits[1]) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCS3;
			tmp->o_len = strlen (bits[1]);
			tmp->o_bytes = string_dup (bits[1]);
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".descriptor")) {
			/*{{{  descriptor line*/
			int v;

			if (!bits[1]) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCL6;
			tmp->o_len = strlen (bits[1]) + sizeof (int) + 10;
			tmp->o_bytes = (char *)smalloc (tmp->o_len);
			*(int *)(tmp->o_bytes) = DESCRIPTOR_TAG;
			memcpy (tmp->o_bytes + sizeof (int) + 6, bits[1] + 1, strlen (bits[1]) - 2);
			for (v=0; v<6; v++) {
				tmp->o_bytes[sizeof (int) + v] = 0x00;
			}
			memcpy (tmp->o_bytes + (tmp->o_len - 6), "\x0aSEQ\x0a:", 6);
			add_to_chain (&hblk, &tblk, tmp);
#if 0
fprintf (stderr, "netcread_nextblocks(): DESCRIPTOR [%s]\n", bits[1]);
#endif
			/*}}}*/
		} else if (!strcmp (bits[0], ".tcoff")) {
			/*{{{  arbitrary TCOFF record data*/
			int tclen, tctag;

			if (!bits[1] || !bits[2]) {
				goto bad_input_line;
			}

			/* figure out what sort of TCOFF record it is first */
			for (tctag=0; tctag<=TCOFF_MAX_TAG; tctag++) {
				if (!strcmp (bits[1], tcoff_tag_names[tctag])) {
					break;		/* this one */
				}
			}
			if (tctag > TCOFF_MAX_TAG) {
				/* try as decimal integer */
				if (sscanf (bits[1], "%d", &tctag) != 1) {
					goto bad_input_line;
				}
			}

			if (string_dequote2 (bits[2], &tclen)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCL6;
			tmp->o_len = tclen + sizeof (int);
			tmp->o_bytes = (char *)smalloc (tmp->o_len);
			*(int *)(tmp->o_bytes) = tctag;

			memcpy (tmp->o_bytes + sizeof (int), bits[2], tclen);
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".setws")) {
			/*{{{  set workspace requirements*/
			int wsb, wsadj;

#if 0
fprintf (stderr, "netcread_nextblocks(): bits[0] = [%s], bits[1] = [%s], bits[2] = [%s]\n", bits[0], bits[1] ?: "(null)", bits[2] ?: "(null)");
#endif
			if (!bits[1] || !bits[2] || (sscanf (bits[1], "%d", &wsb) != 1) || (sscanf (bits[2], "%d", &wsadj) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC11;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = wsb;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_OPR + wsadj;
			tmp->opd = I_NWSADJ;
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".setvs")) {
			/*{{{  set vectorspace requirements*/
			int vsb;

			if (!bits[1] || (sscanf (bits[1], "%d", &vsb) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC12;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = vsb;

			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], ".setms")) {
			/*{{{  set mobilespace requirements*/
			int msb;

			if (!bits[1] || (sscanf (bits[1], "%d", &msb) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCL4;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = msb;
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".mobileinit")) {
			/*{{{  mobilespace initialisation sequence*/
			int mspoffset, count;

			if (!bits[1] || !bits[2] || (sscanf (bits[1], "%d", &mspoffset) != 1) || (sscanf (bits[2], "%d", &count) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCL5;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = mspoffset >> WSH;			/* in words */
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = count;
			add_to_chain (&hblk, &tblk, tmp);

			/* should have "count" .mobileinitpairs following this */
			/*}}}*/
		} else if (!strcmp (bits[0], ".mobileinitpair")) {
			/*{{{  mobilespace pair initialisation*/
			int slotoffs, dataoffs;

			if (!bits[1] || !bits[2] || (sscanf (bits[1], "%d", &slotoffs) != 1) || (sscanf (bits[2], "%d", &dataoffs) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = slotoffs >> WSH;			/* in words */
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = dataoffs >> WSH;			/* in words */
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".setnamedlabel")) {
			/*{{{  set named label*/
			if (!bits[1] || (bits[1][0] != '\"')) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCS2;
			tmp->o_len = strlen (bits[1]) - 2;
			tmp->o_bytes = string_ndup (bits[1] + 1, tmp->o_len);
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".procentry")) {
			/*{{{  procedure entry*/
			if (!bits[1] || (bits[1][0] != '\"')) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCS4;
			tmp->o_len = strlen (bits[1]) - 2;
			tmp->o_bytes = string_ndup (bits[1] + 1, tmp->o_len);
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".tsdepth")) {
			/*{{{  set transputer stack depth*/
			int tsd;

			if (!bits[1] || (sscanf (bits[1], "%d", &tsd) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC1;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = tsd;

			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], ".loadlabaddr")) {
			/*{{{  load address of label*/
			int lab;

			if (!bits[1] || (sscanf (bits[1], "%d", &lab) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCL0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = lab;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = -1;
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".reschedule")) {
			/*{{{  reschedule instruction*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = RESCHEDULE;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], ".boolinvert")) {
			/*{{{  invert boolean*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = BOOLINVERT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], ".notprocess")) {
			/*{{{  load not-process*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = NOTPROCESS;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], ".sourcefile")) {
			/*{{{  source filename*/
			if (!bits[1]) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCS5;
			tmp->o_len = strlen (bits[1]);
			tmp->o_bytes = string_dup (bits[1]);
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".sourceline")) {
			/*{{{  source line-number*/
			int lineno;

			if (!bits[1] || (sscanf (bits[1], "%d", &lineno) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0 + LINENUM;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = lineno;
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".align")) {
			/*{{{  alignment (usually in constants)*/
			int align;

			if (!bits[1] || (sscanf (bits[1], "%d", &align) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0 + ALIGN;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = align;
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".funcresults")) {
			/*{{{  function results (called after a function to indicate stuff left on the stack)*/
			int nresults;

			if (!bits[1] || (sscanf (bits[1], "%d", &nresults) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0 + FUNCRESULTS;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = nresults;
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".funcreturn")) {
			/*{{{  function return (called before the real ret)*/
			int nresults;

			if (!bits[1] || (sscanf (bits[1], "%d", &nresults) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0 + FUNCRETURN;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = nresults;
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], ".labaddr")) {
			/*{{{  constant address of label*/
			int lab;

			if (!bits[1] || (sscanf (bits[1], "%d", &lab) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR + lab;
			tmp->opd = I_NLABADDR;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'a':
#if 0
fprintf (stderr, "case 'a': [%s]\n", bits[0]);
#endif
		if (!strcmp (bits[0], "add")) {
			/*{{{  add with overflow check*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ADD;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "adc")) {
			/*{{{  add constant (with overflow checking)*/
			int cnst;

			if (!bits[1] || (sscanf (bits[1], "%d", &cnst) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_ADC;
			tmp->opd = cnst;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ajw")) {
			/*{{{  adjust workspace*/
			int adj;

			if (!bits[1] || (sscanf (bits[1], "%d", &adj) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_AJW;
			tmp->opd = adj >> WSH;

			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "alt")) {
			/*{{{  alt start*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ALT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "altend")) {
			/*{{{  alt end (new style)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NALTEND;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "altwt")) {
			/*{{{  alt wait*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ALTWT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'c':
		if (!strcmp (bits[0], "call")) {
			/*{{{  call routine*/
			int adj;
			char *fname;

			if (!bits[1] || !bits[2] || (sscanf (bits[2], "%d", &adj) != 1)) {
				goto bad_input_line;
			}
			fname = bits[1];

			tmp = new_etc_chain ();
			tmp->fn = I_OPR + (adj >> WSH);
			tmp->opd = I_NCALL;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			if ((*fname == 'L') && (fname[1] >= '0') && (fname[1] <= '9')) {
				/* numeric label */
				if (sscanf (fname + 1, "%d", &(tmp->opd)) != 1) {
					goto bad_input_line;
				}
			} else {
				tmp->opd = -1;
				tmp->o_len = strlen (fname);
				tmp->o_bytes = string_dup (fname);
			}
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "cj")) {
			/*{{{  conditional jump*/
			if (!bits[1]) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCL0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_CJ;
			if (sscanf (bits[1], "%d", &tmp->opd) != 1) {
				goto bad_input_line;
			}
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], "csub0")) {
			/*{{{  range check (Breg must be in [0..(Areg-1)])*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_CSUB0;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ccnt1")) {
			/*{{{  range check (FIXME: exact spec?)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_CCNT1;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "csngl")) {
			/*{{{  range check (FIXME: exact spec?)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_CSNGL;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "cword")) {
			/*{{{  range check (FIXME: exact spec?)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_CWORD;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'd':
		if (!strcmp (bits[0], "diff")) {
			/*{{{  subtract without overflow checking*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_DIFF;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "div")) {
			/*{{{  divide with overflow checking*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_DIV;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "disc")) {
			/*{{{  disable channel (3->1)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NDISC;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "dist")) {
			/*{{{  disable timer guard (3->1)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NDIST;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "diss")) {
			/*{{{  disable skip guard (3->1)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NDISS;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'e':
		if (!strcmp (bits[0], "endp")) {
			/*{{{  end process*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ENDP;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "enbc")) {
			/*{{{  enable channel (3->1)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ENBC3;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "enbc2")) {
			/*{{{  enable channel (2->0)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ENBC2;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "enbs")) {
			/*{{{  enable skip guard (2->1)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ENBS3;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "enbs2")) {
			/*{{{  enable skip guard (1->0)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ENBS2;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "enbt")) {
			/*{{{  enable timer guard (3->1)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ENBT3;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "enbt2")) {
			/*{{{  enable timer guard (2->0)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_ENBT2;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'f':
		if (!strcmp (bits[0], "fbarinit")) {
			/*{{{  full barrier initialisation*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = FBARINIT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "fbarsync")) {
			/*{{{  full barrier synchronisation*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = FBARSYNC;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "fbarresign")) {
			/*{{{  full barrier resign*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = FBARRESIGN;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "fbarenroll")) {
			/*{{{  full barrier enroll*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETC0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = FBARENROLL;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		}
	case 'g':
		if (!strcmp (bits[0], "gt")) {
			/*{{{  greater-than*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_GT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "getpri")) {
			/*{{{  get priority*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_GETPRI;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "getaff")) {
			/*{{{  get affinity*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_GETAFF;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "getpas")) {
			/*{{{  get priority and affinity state*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_GETPAS;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'i':
		if (!strcmp (bits[0], "in")) {
			/*{{{  channel input*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_IN;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ior")) {
			/*{{{  I/O read*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_IOR;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "iow")) {
			/*{{{  I/O write*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_IOW;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ior8")) {
			/*{{{  I/O read*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_IOR8;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "iow8")) {
			/*{{{  I/O write*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_IOW8;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ior16")) {
			/*{{{  I/O read*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_IOR16;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "iow16")) {
			/*{{{  I/O write*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_IOW16;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ior32")) {
			/*{{{  I/O read*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_IOR32;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "iow32")) {
			/*{{{  I/O write*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_IOW32;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'j':
		if (!strcmp (bits[0], "j")) {
			/*{{{  outright jump*/
			if (!bits[1]) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = ETCL0;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_J;
			if (sscanf (bits[1], "%d", &tmp->opd) != 1) {
				goto bad_input_line;
			}
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], "jtable")) {
			/*{{{  constant address of label*/
			int lab;

			if (!bits[1] || (sscanf (bits[1], "%d", &lab) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR + lab;
			tmp->opd = I_NJTABLE;
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else if (!strcmp (bits[0], "jcsub0")) {
			/*{{{  jump if Breg outside of [0..(Areg-1)]*/
			int lab;

			if (!bits[1] || (sscanf (bits[1], "%d", &lab) != 1)) {
				goto bad_input_line;
			}

			tmp = new_etc_chain ();
			tmp->fn = I_OPR + lab;
			tmp->opd = I_NJCSUB0;
			add_to_chain (&hblk, &tblk, tmp);

			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'l':
		if (!strcmp (bits[0], "ldl")) {
			/*{{{  load local*/
			int wsoff;

			if (!bits[1] || (sscanf (bits[1], "%d", &wsoff) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_LDL;
			tmp->opd = wsoff >> WSH;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ldc")) {
			/*{{{  load constant*/
			int cnst;

			if (!bits[1] || (sscanf (bits[1], "%d", &cnst) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_LDC;
			tmp->opd = cnst;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ldnl")) {
			/*{{{  load non-local*/
			int wsoff;

			if (!bits[1] || (sscanf (bits[1], "%d", &wsoff) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_LDNL;
			tmp->opd = wsoff >> WSH;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ldlp")) {
			/*{{{  load local pointer*/
			int wsoff;

			if (!bits[1] || (sscanf (bits[1], "%d", &wsoff) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_LDLP;
			tmp->opd = wsoff >> WSH;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ldnlp")) {
			/*{{{  load non-local pointer*/
			int wsoff;

			if (!bits[1] || (sscanf (bits[1], "%d", &wsoff) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_LDNLP;
			tmp->opd = wsoff >> WSH;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "lt")) {
			/*{{{  less-than*/
			/* don't have this, of course, so reverse and greater-than */
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_REV;
			add_to_chain (&hblk, &tblk, tmp);

			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_GT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "lb")) {
			/*{{{  load byte*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_LB;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "lw")) {
			/*{{{  load word*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NLW;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "ldtimer")) {
			/*{{{  load timer*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_LDTIMER;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'm':
		if (!strcmp (bits[0], "malloc")) {
			/*{{{  allocate dynamic memory*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MALLOC;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "move")) {
			/*{{{  move memory*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MOVE;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mrelease")) {
			/*{{{  free dynamic memory*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MRELEASE;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mul")) {
			/*{{{  multiply with overflow checking*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MUL;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strncmp (bits[0], "mt_", 3)) {
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			if (!strcmp (bits[0], "mt_alloc")) {
				/*{{{  allocate and initialise mobile type*/
				tmp->opd = I_MT_ALLOC;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_release")) {
				/*{{{  free mobile type*/
				tmp->opd = I_MT_RELEASE;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_clone")) {
				/*{{{  clone mobile type*/
				tmp->opd = I_MT_CLONE;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_in")) {
				/*{{{  mobile type channel input*/
				tmp->opd = I_MT_IN;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_out")) {
				/*{{{  mobile mobile type channel output*/
				tmp->opd = I_MT_OUT;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_xchg")) {
				/*{{{  mobile mobile type channel exchange*/
				tmp->opd = I_MT_XCHG;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_lock")) {
				/*{{{  lock a mobile type*/
				tmp->opd = I_MT_LOCK;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_unlock")) {
				/*{{{  unlock a mobile type*/
				tmp->opd = I_MT_UNLOCK;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_enroll")) {
				/*{{{  increase enroll count of a mobile type*/
				tmp->opd = I_MT_ENROLL;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_resign")) {
				/*{{{  decrease enroll count of a mobile type*/
				tmp->opd = I_MT_RESIGN;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_sync")) {
				/*{{{  synchronise on a mobile type*/
				tmp->opd = I_MT_SYNC;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_xin")) {
				/*{{{  extended mobile type channel input*/
				tmp->opd = I_MT_XIN;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_xout")) {
				/*{{{  extended mobile mobile type channel output*/
				tmp->opd = I_MT_XOUT;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_xxchg")) {
				/*{{{  extended mobile mobile type channel exchange*/
				tmp->opd = I_MT_XXCHG;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_dclone")) {
				/*{{{  allocate a mobile type by cloning data*/
				tmp->opd = I_MT_DCLONE;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_bind")) {
				/*{{{  bind a mobile type*/
				tmp->opd = I_MT_BIND;
				/*}}}*/
			} else if (!strcmp (bits[0], "mt_resize")) {
				/*{{{  resize a mobile type*/
				tmp->opd = I_MT_RESIZE;
				/*}}}*/
			} else {
				goto bad_input_line;
			}
			add_to_chain (&hblk, &tblk, tmp);
		} else if (!strcmp (bits[0], "mwenb")) {
			/*{{{  enable multiway sync guard*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NMWENB;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mwdis")) {
			/*{{{  disable multiway sync guard*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NMWDIS;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mwaltwt")) {
			/*{{{  multiway sync ALT wait*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NMWALTWT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mwalt")) {
			/*{{{  multiway sync ALT start*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NMWALT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mwaltend")) {
			/*{{{  multiway sync ALT end*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NMWALTEND;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_binit")) {
			/*{{{  new mw-sync barrier init*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_BINIT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_pbrilnk")) {
			/*{{{  new mw-sync par-barrier init and link*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_PBRILNK;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_pbrulnk")) {
			/*{{{  new mw-sync par-barrier unlink*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_PBRULNK;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_ppilnk")) {
			/*{{{  new mw-sync proc-barrier init and link*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_PPILNK;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_pbenroll")) {
			/*{{{  new mw-sync par-barrier enroll*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_PBENROLL;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_pbresign")) {
			/*{{{  new mw-sync par-barrier resign*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_PBRESIGN;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_pbadjsync")) {
			/*{{{  new mw-sync par-barrier adjust sync*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_PBADJSYNC;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_sync")) {
			/*{{{  new mw-sync proc-barrier synchronise*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_SYNC;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_altlock")) {
			/*{{{  new mw-sync alt lock*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_ALTLOCK;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_altunlock")) {
			/*{{{  new mw-sync alt unlock*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_ALTUNLOCK;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_alt")) {
			/*{{{  new mw-sync alt start*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_ALT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_altend")) {
			/*{{{  new mw-sync alt end*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_ALTEND;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_enb")) {
			/*{{{  new mw-sync alt enable*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_ENB;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_dis")) {
			/*{{{  new mw-sync alt disable*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_DIS;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_altpostlock")) {
			/*{{{  new mw-sync alt lock-after-ALT*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_ALTPOSTLOCK;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_ppbaseof")) {
			/*{{{  new mw-sync barrier-base from proc-barrier*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_PPBASEOF;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "mws_ppparof")) {
			/*{{{  new mw-sync par-barrier from proc-barrier*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_MWS_PPPAROF;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'n':
		if (!strcmp (bits[0], "neg")) {
			/*{{{  invert integer*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NNEG;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "not")) {
			/*{{{  bitwise not*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NOT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "null")) {
			/*{{{  load constant null*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NULL;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'o':
		if (!strcmp (bits[0], "out")) {
			/*{{{  channel output*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_OUT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'p':
		if (!strcmp (bits[0], "prod")) {
			/*{{{  multiply without overflow checking*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_PROD;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "pop")) {
			/*{{{  pop register from stack*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_POP;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 'r':
		if (!strcmp (bits[0], "ret")) {
			/*{{{  return*/
			int adj;

			if (!bits[1] || (sscanf (bits[1], "%d", &adj) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_OPR + (adj >> WSH);
			tmp->opd = I_NRET;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "rev")) {
			/*{{{  reverse top two stack entries*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_REV;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "runp")) {
			/*{{{  run process*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_RUNP;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 's':
		if (!strcmp (bits[0], "seterr")) {
			/*{{{  set error flag (halt)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_SETERR;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "stl")) {
			/*{{{  store local*/
			int wsoff;

			if (!bits[1] || (sscanf (bits[1], "%d", &wsoff) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_STL;
			tmp->opd = wsoff >> WSH;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "stnl")) {
			/*{{{  store non-local*/
			int off;

			if (!bits[1] || (sscanf (bits[1], "%d", &off) != 1)) {
				goto bad_input_line;
			}
			tmp = new_etc_chain ();
			tmp->fn = I_STNL;
			tmp->opd = off >> WSH;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "startp")) {
			/*{{{  start process*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NSTARTP;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "stopp")) {
			/*{{{  stop process*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_STOPP;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "sub")) {
			/*{{{  subtract with overflow checking*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_SUB;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "sb")) {
			/*{{{  store byte*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_SB;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "sw")) {
			/*{{{  store word*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_NSW;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "sum")) {
			/*{{{  add without overflow check*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_SUM;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "setpri")) {
			/*{{{  set priority*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_SETPRI;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "setaff")) {
			/*{{{  set affinity*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_SETAFF;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "shr")) {
			/*{{{  shift right*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_SHR;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "shl")) {
			/*{{{  shift left*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_SHL;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	case 't':
		if (!strcmp (bits[0], "trap")) {
			/*{{{  trap (debugging)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_TRAP;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "taltwt")) {
			/*{{{  timer alt wait*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_TALTWT;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else if (!strcmp (bits[0], "tin")) {
			/*{{{  timer wait (timeout)*/
			tmp = new_etc_chain ();
			tmp->fn = I_OPR;
			tmp->opd = I_TIN;
			add_to_chain (&hblk, &tblk, tmp);
			/*}}}*/
		} else {
			goto bad_input_line;
		}
		break;
	default:
		goto bad_input_line;
	}

	*head = hblk;
	*tail = tblk;

	sfree (line);
	return;

bad_input_line:
	fprintf (stderr, "netcread_nextblocks(): badly formed directive: %s\n", origline);
	*head = hblk;
	*tail = tblk;
	sfree (line);
	return;
}
/*}}}*/
/*{{{  static char *netcread_nextline (void)*/
/*
 *	returns a pointer to the next line of input (textual)
 */
static char *netcread_nextline (void)
{
	char *ptr, *ch;

	if (netcfile_ptr >= netcfile_length) {
		return NULL;		/* end of file */
	}
	ptr = netcfile_bytes + netcfile_ptr;
	for (ch=ptr; (netcfile_ptr < netcfile_length) && (*ch != '\r') && (*ch != '\n'); netcfile_ptr++, ch++);
	while ((netcfile_ptr < netcfile_length) && ((*ch == '\r') || (*ch == '\n'))) {
		*ch = '\0';
		ch++;
		netcfile_ptr++;
	}

	return ptr;
}
/*}}}*/
/*{{{  int open_netc_file (char *filename)*/
/*
 *	opens a TCE file
 */
int open_netc_file (char *filename)
{
	struct stat stbuf;
	int err;
	int i, j;

	netcfile_filename = filename;
#ifdef HOST_OS_IS_CYGWIN
	netcfile_fd = open (netcfile_filename, O_RDONLY | O_BINARY);
#else
	netcfile_fd = open (netcfile_filename, O_RDONLY);
#endif
	if (netcfile_fd < 0) {
		fprintf (stderr, "%s: unable to open %s: %s\n", progname, netcfile_filename, strerror (errno));
		return -1;
	}
	err = fstat (netcfile_fd, &stbuf);
	if (err < 0) {
		fprintf (stderr, "%s: unable to stat %s: %s\n", progname, netcfile_filename, strerror (errno));
		close (netcfile_fd);
		return -1;
	}
	netcfile_length = stbuf.st_size;
	netcfile_ptr = 0;
	netcfile_bytes = (char *)smalloc (netcfile_length);
	i = 0;
	while (i < netcfile_length) {
		j = read (netcfile_fd, (netcfile_bytes + i), (netcfile_length - i));
		if (j == 0) {
			fprintf (stderr, "%s: unexpected EOF in %s\n", progname, netcfile_filename);
			close (netcfile_fd);
			return -1;
		} else if (j == -1) {
			fprintf (stderr, "%s: read error while reading from %s: %s\n", progname, netcfile_filename, strerror (errno));
			close (netcfile_fd);
			return -1;
		}
		i += j;
	}
	if (i != netcfile_length) {
		fprintf (stderr, "%s: read too much from %s\n", progname, netcfile_filename);
		close (netcfile_fd);
		return -1;
	}
	return 0;
}
/*}}}*/
/*{{{  void close_netc_file (void)*/
/*
 *	closes the netc file
 */
void close_netc_file (void)
{
	free (netcfile_bytes);
	close (netcfile_fd);
	return;
}
/*}}}*/


