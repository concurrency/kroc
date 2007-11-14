/* $Id: popen_re.h,v 1.1 1996/04/15 10:52:18 djb1 Exp $ */

/*
 *	Library to replace existing popen_read variants lurking around the toolset
 *	Copyright (C) 1991 Inmos Limited
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



/* To fix bugs:                                                             */
/*    INSdi01275                                                            */

#ifndef POPEN_RE_H
#define POPEN_RE_H

#include <stdio.h>
#include <stdlib.h>

/* function popen_relative                                                  */

/* Attempts to open a file according to the normal INMOS search rules.      */
/* If the open is successful, the FILE handle is returned, as detemined     */
/* by fopen of the host operating system. If it is unsuccessful, NULL is    */
/* returned.                                                                */
/* N.B. NULL will also be returned if ma is passed in as NULL and there is  */
/* a memory allocation failure.                                             */

/* The parameters are:                                                      */

/* fn - the name of the file in a zero terminated C string. This must be    */
/* in the syntactic form of a valid file name for the host operating system.*/
/* fn must not be NULL.                                                     */

/* ev - the name of an environment variable containing a list of paths.     */
/* It is in the form of a C string, zero terminated, or NULL.               */
/* The list of paths must be separated by one or more spaces or semicolons. */
/* If this parameter is NULL, then no attempt is made to search for it.     */
/* If the environment variable does not exist, then it is ignored.          */

/* rn - the name of a file relative to which the open is to be attempted.   */
/* The form of this is a zero terminated C string, or NULL.                 */

/* md - the mode in which the file is to be opened. This is the second      */
/* parameter to fopen from stdio in the host operating system. It has the   */
/* form of a zero terminated C string. It must not be NULL.                 */

/* cn - a pointer to a variable to contain the full file name if the open   */
/* is successful. The space for this name is acquired dynamically. It is    */
/* the responsibility of the user to free it afterwards. If cn is NULL, no  */
/* name is returned.                                                        */

/* ma - a pointer to a substitute for malloc. If it is NULL, then malloc    */
/* is assumed. fnilib is not customised by this function.                   */

/* mf - a pointer to a substitute for free. If it is NULL, then free is     */
/* assumed. fnilib is not customised by this function.                      */

/* er - after the function is called, this contains the count of the number */
/* syntax errors found in the list of directories extracted from the        */
/* environment variable ev. If it is NULL, then no count is returned.       */

/* Semantics:                                                               */

/* An attempt is made to open the file in the following order:              */
/* 1. fn is examined to see whether it begins with an absolute directory    */
/*    specification. If it does, we will say it is an absolute name. If not,*/
/*    it is a relative name.                                                */
/* 2. If the name is absolute, an fopen is attempted on an unmodified fn.   */
/*    If this succeeds, the file handle is returned and cn is set to point  */
/*    to a copy of fn.                                                      */
/*    If it fails, NULL is returned and cn is not accessed.                 */
/* 3. For the remainder of this description, fn is known to be relative.    */
/* 4. If rn is NULL, then an attempt is made to fopen the name fn unchanged.*/
/*    If it succeeds, the file handle is returned and cn is set to point    */
/*    to a copy of fn.                                                      */
/* 5. If rn is not NULL, then the directory part of rn is extracted. Call   */
/*    this dn. A new name is formed by appending fn to dn, ie. the name of  */
/*    a file is formed relative to the directory containing file rn is      */
/*    constructed. Call this xn. An attempt is then made to open the name   */
/*    xn. If it succeeds, then the file handle is returned and cn is set to */
/*    point to a copy of the name xn. If there is no directory part of rn,  */
/*    then xn is the same as fn.                                            */
/* 6. If ev is not NULL and the environment variable exists, then its       */
/*    contents are scanned for directory names. These are treated as pure   */
/*    prefixes to fn to form new file names. Each of these is tried in turn.*/
/*    If one opens successfully, then the file handle is returned and cn is */
/*    set to point to a copy of the catenation of the path used and fn.     */
/*    The path names in ev may omit the trailing directory marker on Unix   */
/*    or MS-DOS (ie. the final / or \\). This is true as of 24/03/93, but   */
/*    fnilib is free to evolve to other systems in a similar manner.        */
/*    If there were any errors in the syntax of path names in ev detected   */
/*    during this scan, then the value er is set to the number of errors    */
/*    found. It will be zero if no errors were found.                       */
/* 7. If the file is being opened on a host other than Unix, then an attempt*/
/*    is made to interpret fn with Unix syntax of file names. However, ev   */
/*    is always assumed to contain paths in the syntax of the host.         */
/*    The filename syntax style in fnilib is restored to what it was        */
/*    before this function was called.                                      */
/* 8. If none of the paths contains the file fn, then NULL is returned and  */
/*    cn is not referenced.                                                 */

/* Comments:                                                                */

/* This function is implemented on top of the file name interpretation      */
/* library. All the fnilib functions are usable independently of popen.     */
/* If the user wishes to use their own version of storage management        */
/* functions, then non-NULL values should be passed to ma and mf.           */
/* If the user does not wish to use her own version of storage managament   */
/* functions, then NULL should be passed to ma and mf.  In this case, the   */
/* system functions malloc() and free() will be used.  popen_relative()     */
/* will return NULL if a call to malloc() fails.                            */

FILE * popen_relative
         (char const *   const            /* fn - name of file to open      */
        , char const *   const            /* ev - name of env var with path */
        , char const *   const            /* rn - name of file relative to  */
                                          /* which the open is to be made   */
        , char const *   const            /* md - open mode string          */
        , char const * * const            /* cn - full file name returned   */
        , void * (* const)(size_t)        /* ma - substitute for malloc     */
        , void (* const)(void *)          /* mf - substitute for free       */
        , int *          const            /* er - count of errors in list   */
         );


#endif
