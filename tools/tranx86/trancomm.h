/*
 *	trancomm.h - target independant stuff
 *	Copyright (C) 2000 Fred Barnes <frmb@kent.ac.uk>
 *	Based on trancomm.inc Copyright (C) 1997 M D Poole
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

#ifndef __TRANCOMM_H
#define __TRANCOMM_H

#define ETC0 0xffffff00				/* various stuff (FINISH_OP, CONTRJOIN, etc.) */
#define ETC1 0xffffff01				/* TSDEPTH */
#define ETC2 0xffffff02				/* FUNCRESULTS */
#define ETC3 0xffffff03				/* FUNCRETURN */
#define ETC4 0xffffff04				/* ENDWS */
#define ETC5 0xffffff05				/* REALRESULT */
#define ETC6 0xffffff06				/* SETLAB */
#define ETC7 0xffffff07				/* SECTIONLAB */
#define ETC8 0xffffff08				/* ALIGN */
#define ETC9 0xffffff09				/* LINENUM */
#define ETC10 0xffffff0a			/* DEBUGLINE */
#define ETC11 0xffffff0b			/* SETWS */
#define ETC12 0xffffff0c			/* SETVS */
#define ETC13 0xffffff0d			/* SLLIMM */
#define ETC14 0xffffff0e			/* SLRIMM */
#define ETC15 0xffffff0f			/* LOOPHEADTOP */
#define ETCS1 0xffffff11			/* stub-name */
#define ETCS2 0xffffff12			/* global-name */
#define ETCS3 0xffffff13			/* j-entry */
#define ETCS4 0xffffff14			/* proc-entry */
#define ETCS5 0xffffff15			/* source filename */
#define ETCS6 0xffffff16			/* compiler comment */
#define ETCS7 0xffffff17			/* code map */
#define ETCS8 0xffffff18			/* data-bytes */
#define ETCS9 0xffffff19			/* messages-bytes */
#define ETCS10 0xffffff1a			/* load named label */
#define ETCS11 0xffffff1b			/* load named code-map */
#define ETCS12 0xffffff1c			/* global-name end */
#define ETCL0 0xffffff20			/* J, CJ, CALL, LDC  for labels */
#define ETCL1 0xffffff21			/* loopend */
#define ETCL2 0xffffff22			/* loopend3 */
#define ETCL3 0xffffff23			/* backwards loopend */
#define ETCL4 0xffffff24			/* mobilespace usage */
#define ETCL5 0xffffff25			/* mobilespace initialisation */
#define ETCL6 0xffffff26			/* TCOFF record */
#define ETCL7 0xffffff27			/* load workspace-map */
#define ETCL8 0xffffff28			/* unload workspace-map */
#define ETC_MAX 0xffffff30				/* (-208) */

#define OCC21_NO_SLOT 0x80000000

/* ETC1-ETC15 (other stuff) */
#define TSDEPTH 1
#define FUNCRESULTS 2
#define FUNCRETURN 3
#define ENDWS 4
#define REALRESULT 5
#define SETLAB 6
#define SECTIONLAB 7
#define ALIGN 8
#define LINENUM 9
#define DEBUGLINE 10
#define SETWS 11
#define SETVS 12
#define SLLIMM 13
#define SRLIMM 14
#define LOOPHEADTOP 15

#define STUBNAME 64
#define GLOBNAME 65
#define JUMPENTRY 66
#define PROCNAME 67
#define SOURCEFILE 68
#define OCCCOMMENT 69
#define CODEMAP 70
#define DATABYTES 71
#define MESSAGEBYTES 72
#define LOADLABELNAME 73
#define LOADCODEMAPNAME 74
#define GLOBNAMEEND 75

/* ETC0 stuff (special instructions) */
#define BOOLINVERT 1
#define STARTTABLE 2
#define WIDENSHORT 3
#define LOOPHEADBOT 4
#define CONTRSPLIT 5
#define CONTRJOIN 6
#define I64TOREAL 7
#define NOTPROCESS 8
#define FPPOP 9
#define CHECKNOTNULL 10
#define SEMCLAIM 11
#define SEMRELEASE 12
#define SEMINIT 13
#define RESCHEDULE 14
#define INDIRECT_AREG 15
#define INDIRECT_BREG 16
#define INDIRECT_CREG 17
#define RMWSMAP 18
#define MPPCLONE 19
#define MPPSERIALISE 20
#define MPPDESERIALISE 21
#define LOADCODEMAP 22
#define FBARINIT 23
#define FBARSYNC 24
#define FBARRESIGN 25
#define FBARENROLL 26
/* #define MTNEW 27 */
/* #define MTFREE 28 */
/* #define MTCLONE 29 */
#define R32SIN 30
#define R64SIN 31
#define R32COS 32
#define R64COS 33
#define DTRACE 34
#define KILLCALL 35
#define WAIT_FOR_INTERRUPT 36
#define R32TAN 37
#define R64TAN 38

#define FINISH_OP -1

#endif	/* !__TRANCOMM_H */

