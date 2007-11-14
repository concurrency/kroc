/*
 *	Visible entrypoints in tcofflib
 *	Copyright (C) 1990 Inmos Limited
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

/* Copyright 1990 INMOS Limited */
/* CMSIDENTIFIER
   PLAY:TCOFFLIB_H@524.AAAA-FILE;1(20-FEB-92)[FROZEN] */
/******************************************************************************
*
*  These are the entrypoints in tcofflib that other people may use
* 
*  V 2.00.02
*
* V2.00.01 -> V2.00.02 
*       Ade 22/3/93 - added const to tcoff_putrec, bug INSdi01122
******************************************************************************/

/*******************/
#ifdef __STDC__
#define PARMS(x) x
#else
#define PARMS(x)
#endif
/*******************/

extern char *str_duplicate PARMS((char const *));
extern char *str_concat PARMS((char const *, char const *));
extern FILE *pathopen PARMS((char const *, char const *, char *, char const *));
extern long int tcoff_getl_test PARMS((FILE *, int *));
extern unsigned long int tcoff_getul_test PARMS((FILE *, int *));
extern char *tcoff_gets_test PARMS((FILE *, long int *, int *));
extern void tcoff_putl PARMS((FILE *, long int));
extern void tcoff_putul PARMS((FILE *, unsigned long int));
extern void tcoff_puts PARMS((FILE *, long int, char *));
extern long int tcoff_sizel PARMS((long int));
extern void tcoff_putrec PARMS((FILE *, long int, const char *, ...));

extern char *lff_gets_test PARMS((FILE *, long int *, int *));
extern long int lff_getl_test PARMS((FILE *, int *));
