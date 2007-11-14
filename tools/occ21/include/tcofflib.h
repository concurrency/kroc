/* Copyright 1994 INMOS Limited */

/*
 *	entry points in tcofflib
 *	Copyright (C) 1993 Inmos Limited
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

/*  $Id: tcofflib.h,v 1.1 1996/04/15 10:52:22 djb1 Exp $    */

extern long int tcoff_getl_test (FILE *, int *);
extern unsigned long int tcoff_getul_test (FILE *, int *);
extern char *tcoff_gets_test (FILE *, long int *, int *);
extern void tcoff_putl (FILE *, long int);
extern void tcoff_putul (FILE *, unsigned long int);
extern void tcoff_puts (FILE *, long int, char *);
extern long int tcoff_sizel (long int);
extern void tcoff_putrec (FILE *, long int, const char *, ...);

extern char *lff_gets_test (FILE *, long int *, int *);
extern long int lff_getl_test (FILE *, int *);
