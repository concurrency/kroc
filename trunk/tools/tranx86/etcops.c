/*
 *	etcops.h - ETC operations
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
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "support.h"


/*{{{  etc_chain *etc_preoptimise (etc_chain *etc_code)*/
/*
 *	general ETC optimisations to give better code:
 *
 *	turns "LDC {1,4}\n {IN|OUT}" into "{IN|OUT}{8|32}"
 *	      "STL x\n LDL x"             "XSTL x"
 */
etc_chain *etc_preoptimise (etc_chain *etc_code)
{
	etc_chain *tmp, *head, *last, *prevlast;

	head = etc_code;
	last = prevlast = NULL;
	for (tmp=head; tmp; prevlast = last, last=tmp, tmp=tmp->next) {
		if (tmp->fn < I_OPR) {
			if ((tmp->fn == I_LDL) && last && (last->fn == I_STL) && (tmp->opd == last->opd)) {
				if (tmp->opd >= 0) {
					tmp->fn = I_OPR + tmp->opd;
					tmp->opd = I_XSTL;
				} else {
					/* keep this, but adjust */
					tmp->fn = I_OPR - tmp->opd;
					tmp->opd = I_XSTLN;
				}
				/* remove the STL instruction in `last' */
				if (head == last) {
					head = tmp;
					sfree (last);
					last = NULL;
				} else {
					prevlast->next = tmp;
					sfree (last);
					last = prevlast;
					prevlast = NULL;
				}
			}
		} else if (tmp->opd >= (signed int)ETC_MAX) {
			if ((options.kernel_interface & KRNLIFACE_NEWCCSP) && ((tmp->opd == I_IN) || (tmp->opd == I_OUT)) && last && (last->fn == I_LDC)) {
				if ((last->opd == 1) || (last->opd == 4)) {
					if (tmp->opd == I_IN) {
						tmp->opd = (last->opd == 1) ? I_IN8 : I_IN32;
					} else {
						tmp->opd = (last->opd == 1) ? I_OUT8 : I_OUT32;
					}
					/* remove the LDC instruction in `last' */
					if (head == last) {
						head = tmp;
						sfree (last);
						last = NULL;
					} else {
						prevlast->next = tmp;
						sfree (last);
						last = prevlast;
						prevlast = NULL;
					}
				}
			} else if ((options.kernel_interface & KRNLIFACE_NEWCCSP) && ((tmp->opd == I_IOR) || (tmp->opd == I_IOW)) && last && (last->fn == I_LDC)) {
				/* optimise constant-width I/O read and write instructions */
				switch (last->opd) {
				case 1:
				case 2:
				case 4:
					if (tmp->opd == I_IOR) {
						tmp->opd = (last->opd == 1) ? I_IOR8 : ((last->opd == 2) ? I_IOR16 : I_IOR32);
					} else {
						tmp->opd = (last->opd == 1) ? I_IOW8 : ((last->opd == 2) ? I_IOW16 : I_IOW32);
					}

					/* remove the LDC instruction in `last' */
					if (head == last) {
						head = tmp;
						sfree (last);
						last = NULL;
					} else {
						prevlast->next = tmp;
						sfree (last);
						last = prevlast;
						prevlast = NULL;
					}
					break;
				default:
					break;
				}
			}
		} else {
			/* just soak up ETC */
			switch (tmp->opd) {
			case ETC0:
				tmp = tmp->next;
				break;
			case ETC6:
			case ETC7:
				tmp = tmp->next;
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
				tmp = tmp->next;
				break;
			case ETCS8:
				/* DATABYTES -- big lumps probably get broken up, need to stitch back together */
				{
					etc_chain *walk;
					unsigned char *tmpbuf;
					int tmplen = tmp->o_len;

					for (walk=tmp->next; (tmp->fn >= I_OPR) && (tmp->opd < (signed int)ETC_MAX) && (walk->opd == ETCS8); walk = walk->next) {
						tmplen += walk->o_len;
					}
					if (tmplen > tmp->o_len) {
						/* got more than 1, bunch them together */
						int offs = 0;
						tmpbuf = (unsigned char *)smalloc (tmplen);

						for (walk=tmp; (tmp->fn >= I_OPR) && (tmp->opd < (signed int)ETC_MAX) && (walk->opd == ETCS8); walk = walk->next) {
							memcpy (tmpbuf + offs, walk->o_bytes, walk->o_len);
							offs += walk->o_len;

							sfree (walk->o_bytes);
							walk->o_bytes = NULL;
							walk->o_len = 0;
						}
						/* walk is pointing at the next */
						tmp->next = walk;
						tmp->o_bytes = (char *)tmpbuf;
						tmp->o_len = tmplen;
					}
				}
				break;
			case ETCL0:
				tmp = tmp->next;
				if (tmp->fn == I_LDC) {
					tmp = tmp->next;
				}
				break;
			case ETCL1:
			case ETCL2:
			case ETCL3:
				tmp = tmp->next;
				tmp = tmp->next;
				tmp = tmp->next;
				break;
			case ETCL4:
				tmp = tmp->next;
				break;
			case ETCL5:
				tmp = tmp->next;
				tmp = tmp->next;
				{
					int count = tmp->opd;

					while (count) {
						tmp = tmp->next;
						tmp = tmp->next;
						count--;
					}
				}
				break;
			case ETCL7:
			case ETCL8:
				tmp = tmp->next;
				tmp = tmp->next;
				break;
			}
		}
	}
	return head;
}
/*}}}*/
/*{{{  etc_chain *sub_fpremfirststep (etc_chain *etc_code)*/
/*
 *	turns FPREMFIRST/FPREMSTEP into FPREM/FPCHKERR
 */
etc_chain *sub_fpremfirststep (etc_chain *etc_code)
{
	etc_chain *tmp, *head, *anchor, *last;
	int in_conversion;

	head = etc_code;
	in_conversion = 0;
	last = anchor = NULL;
	for (tmp=head; tmp; last=tmp, tmp=tmp->next) {
		if (tmp->fn < I_OPR) {
			/* ignore primaries */
			continue;
		} else if (tmp->opd >= (signed int)ETC_MAX) {
			switch (tmp->opd) {
			case I_FPREMFIRST:
				in_conversion = 1;
				anchor = tmp;
				break;
			case I_FPREMSTEP:
				if (!in_conversion) {
					fprintf (stderr, "%s: warning: FPREMSTEP without FPREMFIRST\n", progname);
				} else {
					/* stop at next ETC6/7 */
					in_conversion = 2;
				}
				break;
			}
			continue;
		} else {
			switch (tmp->opd) {
			case ETC0:
				tmp = tmp->next;
				break;
			case ETC6:
			case ETC7:
				tmp = tmp->next;
				if (in_conversion == 2) {
					/* sort out (anchor --> tmp) */
					anchor->opd = I_FPREM;
					anchor->next = tmp;
					tmp->fn = I_OPR;
					tmp->opd = I_FPCHKERR;
					in_conversion = 0;
				}
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
				tmp = tmp->next;
				break;
			case ETCL0:
				tmp = tmp->next;
				if (tmp->fn == I_LDC) {
					tmp = tmp->next;
				}
				break;
			case ETCL1:
			case ETCL2:
			case ETCL3:
				tmp = tmp->next;
				tmp = tmp->next;
				tmp = tmp->next;
				break;
			case ETCL4:
				tmp = tmp->next;
				break;
			case ETCL5:
				tmp = tmp->next;
				tmp = tmp->next;
				{
					int count = tmp->opd;

					while (count) {
						tmp = tmp->next;
						tmp = tmp->next;
						count--;
					}
				}
				break;
			case ETCL7:
			case ETCL8:
				tmp = tmp->next;
				tmp = tmp->next;
				break;
			}
		}
	}
	return head;
}
/*}}}*/


