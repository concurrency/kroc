/* $Id: fnilib.h,v 1.1 1996/04/15 10:52:04 djb1 Exp $ */

/*
 *	Define structures for filename interpretation
 *	Copyright (C) 1991, 1993 Inmos Limited
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

/* History */
/* ahp: First version written */
/* stevec: 19930811:                                                      */
/*         Modified to add fnilib access handles, for multiple, disjoint  */
/*         accesses to fnilib.                                            */

/* Include definitions of size_t and FILE                                 */

#include <stdio.h>
#include <stddef.h>

/* To discover what version of the libary is in use, a function is        */
/* provided that obtains it.                                              */
char const * fni_version (void);

/* All calls to fnilib functions must use an "access handle".  An access  */
/* handle is obtained by a call to fni_open_access(), after all access    */
/* has completed, a call should be made to fni_close_access().  Note that */
/* any values returned from calls to fnilib functions with a given handle */
/* may only be subsequently used in calls to fnilib functions with the    */
/* same access handle.                                                    */
typedef struct fni_access_info *fni_access;

/* fni_open_access initiates access to fnilib through the returned access */
/*                 handle.                                                */
fni_access fni_open_access(void * (*) (size_t),
                                            /* memory allocation function */
                           void (*)(void *) /* memory free function       */
                          );

/* fni_close_access terminates access to fnilib through the access handle */
/*                  'access'.                                             */
void fni_close_access(fni_access * /* access */);

/* Calling this function with a TRUE argument enables internal fnilib     */
/* debugging diagnostics to be written to stderr.  A FALSE argument       */
/* disables the diagnostics.  The value returned is the previous state of */
/* the debugging diagnostics.                                             */
int fni_debug(fni_access, int);

/* Define the style of the filename syntax. This is set up at compile time*/
/* where possible. In the case of the transputer, it is found by the      */
/* server.                                                                */

typedef enum
{
  host_style,             /* use the style of the host file system        */
  unix_style,             /* use the Unix filesystem conventions          */
  msdos_style,            /* use the MS-DOS style                         */
  vms_style,              /* use the VAX/VMS file system style            */
  other_style             /* if this is present, default to Unix          */
} fni_style;


/* A flag is required for some functions.                                 */

typedef enum {
  fni_none,           /* file has no type -- normally erroneous */
  fni_user,           /* file is user type                      */
  fni_system          /* file is system type                    */
} fni_file_type;

/* When a file is opened, a structure is returned that contains the       */
/* system handle and the canonical form of the directory which it         */
/* contains. These are recorded in a local structure.                     */

typedef struct fni_file_data * fni_handle;

/* The canonical file name is an internal format that encapsulates the    */
/* filename in a way which is independent of the host which is being used */

typedef char const * fni_canonical;

/* These can be combined into lists, represented by a list structure.     */

typedef struct fni_list_entry * fni_list;

/* fni_set_style:                                                         */
/* This function changes the style for scanning the external file names.  */
/* It is provided to allow the C compiler to try the Unix file type if    */
/* the normal host type fails. It is the user's responsibility to reset   */
/* the style after trying another.                                        */

fni_style fni_set_style (fni_access,          /* access handle            */
                         fni_style const);    /* style of filename syntax */

/* fni_internalise:                                                       */
/* Converts a filename in the host's format into a canonical form.        */
/* It returns a fni_canonical with the converted form, or NULL.           */
/* If NULL is returned, it means that the filename was not recognised     */
/* as valid for the current host.                                         */
/* The parameter is the host file name in its external form.              */
/* It is the user's responsibility to free the space obtained for the     */
/* result.                                                                */

fni_canonical fni_internalise (fni_access,    /* access handle            */
                               char const * const,
                                              /* file name                */
                               fni_file_type const);
                                              /* user / system / none     */

/* fni_externalise:                                                       */
/* Converts a canonical filename into the right form for the current host.*/
/* If it cannot be converted, NULL is returned.                           */
/* Note that it can fail to convert because the host does not support     */
/* some attribute of the name (eg. device names are meaningless on Unix   */
/* systems).                                                              */
/* It is the user's responsibility to free the space obtained for the     */
/* result.                                                                */

char const * fni_externalise (fni_access,     /* access handle            */
                              fni_canonical const);
                                              /* internal form of name    */

/* fni_basename:                                                          */
/* Extracts the name of the file, without directory paths or extension.   */
/* It operates on the internal form only.                                 */
/* If there is no filename, then a pointer to an empty string is returned.*/
/* This function returns NULL only if the input is badly formed.          */
/* It is the user's responsibility to free the space obtained for the     */
/* result.                                                                */

char const * fni_basename (fni_access,        /* access handle            */
                           fni_canonical const);
                                              /* internal form of name    */

/* fni_extension:                                                         */
/* Extracts the extension from a canonical filename and returns it in a   */
/* character string that has been obtained for the purpose.               */
/* If there is no extension, then NULL is returned.                       */
/* If the extension is nempty (ie the filename ends with a '.', then an   */
/* empty string ("") is returned.                                         */

char const * fni_extension (fni_access,       /* access handle            */
                            fni_canonical const);
                                              /* internal form of name    */

/* fni_switch_extension:                                                  */
/* Change the extension on a canonical filename. It will always succeed   */
/* and will return a new string with the new name.                        */
/* An empty extension string ('') is acceptable and will result in a name */
/* ending with a '.'. If the extension is NULL, then the extension will   */
/* be removed.                                                            */

fni_canonical fni_switch_extension (fni_access, /* access handle          */
                                    fni_canonical const,
                                              /* filename                 */
                                    char const * const);
                                              /* new extension            */

/* fni_add_extension:                                                     */
/* Add an extension if there is no extension already on the filename.     */
/* The extension can be empty (""). If it is NULL, then the name returned */
/* will be the same. In all cases, new memory is allocated.               */

fni_canonical fni_add_extension (fni_access,  /* access handle            */
                                 fni_canonical const,
                                              /* filename                 */
                                 char const * const);
                                              /* extension to add         */

/* fni_add_dirs_to_list:                                                  */
/* Receives a character string containing a list of names of directories. */
/* These are examined for syntactic accuracy for the correct host and     */
/* then appended to the list of canonical file names provided.            */
/* The filenames are separated by blanks in the input string.             */
/* For each file name which is syntactically invalid, a function parameter*/
/* is invoked passing the erroneous string.                               */
/* NULL represents an empty list.                                         */

fni_list fni_add_dirs_to_list (fni_access,    /* access handle            */
                               fni_list const,/* list of directories      */
                               char const * const,
                                             /* string to examine         */
                               void (* const)(char const * const));
                                             /* error function            */

/* fni_free_list:                                                         */
/* Recovers the space used by a list of canonical file names.             */
/* Note that if any of these are to be saved, it is the user's            */
/* responsibility to do so before freeing the space.                      */

void fni_free_list (fni_access,              /*  access handle            */
                    fni_list const);         /* list to be freed          */

/* fni_system_handle:                                                     */
/* Extracts the system file handle from the fni_handle.                   */

FILE * fni_system_handle (fni_access,        /* fnilib access handle      */
                          fni_handle const); /* file handle to access     */

/* fni_position:                                                          */
/* Extracts the position of the file in the filesystem from the           */
/* fni_handle.                                                            */
/* It is the user's responsibility to free the space obtained for the     */
/* result.                                                                */

fni_canonical fni_position (fni_access,      /*  access handle            */
                            fni_handle const);
                                             /* handle to access          */
/* fni_open:                                                              */
/* When a file is opened, this function looks through the current         */
/* location and lists of canonical names as defined in SW-260.            */
/* If the file cannot be opened, then NULL is returned.                   */

fni_handle fni_open (fni_access,            /* access handle              */
                     fni_canonical const,   /* file to be opened          */
                     char const * const,    /* access method to use       */
                     fni_canonical const,   /* position of source         */
                     fni_list const,        /* user list                  */
                     fni_list const,        /* system list                */
                     fni_list const);       /* default list (env.)        */

/* fni_close:                                                             */
/* Closing a file will also free the fni_handle. The user should not      */
/* do so.                                                                 */
/* The returned value is the contents of errno after the close was        */
/* performed. The user should check the result to see if errors occurred. */

int fni_close (fni_access,                  /* access handle              */
               fni_handle const);           /* file to be closed          */

/* fni_free:                                                              */
/* Free the space used by the fni handle. This function does NOT close    */
/* the file. fni_close should be used if the file is to be closed.        */

void fni_free (fni_access,                    /* access handle            */
               fni_handle const);             /* handle to free           */

/* For the curious, the format of the canonical file name is as follows:  */
/* (^X means a <control-X> character)                                     */
/*    ^A        marker to indicate it is canonical, not external          */
/*  'U' | 'S'   indicates user or system file                             */
/*    ^B        separator                                                 */
/*  device      device name for the filesystem, includes node names       */
/*    ^C        separator                                                 */
/*  dir-list    list of directory names, separated by ^C (not / or \ )    */
/*    ^D        separator                                                 */
/*  filename    name of the file, without extension                       */
/*    ^E        separator                                                 */
/*  extension   file extension                                            */
/*    ^F        terminator                                                */
/*    NUL       end of a C string                                         */

/* In the directory list, the following conventions are used:             */
/*    ^H        means current directory                                   */
/*    ^I        means move up a level                                     */

/* Note that each part is separated from the next part by a different     */
/* separator. When converting from the external form, the following rules */
/* are assumed:                                                           */
/* Any part of the file name may be enmpty.                               */
/* Blanks and control characters are not valid characters in any file name*/
/* Version numbers (VAX/VMS) are stripped off and ignored.                */
/* If the device name is present, then the name will contain a root.      */
/* The extension is separated from the filename by the last dot.          */
