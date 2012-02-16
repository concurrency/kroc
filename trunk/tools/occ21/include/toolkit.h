/* Copyright 1994 INMOS Limited */

/*
 *	generic toolkit header file
 *	Copyright (C) 1993, 1994 Inmos Limited
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

/*  $Id: toolkit.h,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/* ahp : 930125 : Removed redefinitions of tolower and toupper */
/* ade : 10/11/93 : redefine VOID to avoid clash with imstype.h */
/* ade 17/12/93 - remove old style prototypes (the PARMS define) */
/*      -  str_duplicate() and str_concat() moved here from tcofflib.h */
/* ade 22/12/93 - check if TARG defined */
/* ade 26/1/94 - removed SEEK_SET, added PARMS and VOID */
/* ade 17/3/94 - removed popen(), unfold_arg_list() and get_indcom() */
/* ade 17/3/94 - Pauls command line stuff reinstated */
/* Ade 24/6/94 : added CVS */
/* Ade 17/8/94 : remove include tcofflib.h */
/* Ade 14/11/94 - removed alt_free() and alt_malloc() */
/*
 * Below are source files in trial that include this header, as of 8/9/94.
 *
 * $trial/ilibr/src/ilibr.c
 * $trial/ilink/src/\*.c
 * $trial/common/toolkit/src/\*.c
 *
 */

#define  PUBLIC
#define  PRIVATE        static


/* Only tcofflib uses this and defines TARG */
/*#if defined(TARG) */

#define TARG something

/*
 * Anything within this TARG define is expected to be removed from 
 * toolkit.h
 *
 */


/* 
 * Care with PARMS because there is no source file that includes toolkit.h
 * that needs PARMS. Perhaps the source that has PARMS only includes 
 * tcofflib.h (from which I've removed the other defn. of PARMS)
 */


#ifdef STD_C
#define PARMS(x) x
#else
#define PARMS(x)
#endif

#define VOID void       /* needed by expr.c and pagedmem.c in ilink */



/*
 * Careful about these host defines. For ilist I use the host defines that are
 * in imstype.h. I've left these here for the other tools written by
 * Paul Sidnell.
 * ilibr.c, ilink.c and ilist.c need the following machine defines
 */

/* #define TRANSPUTER     0 */
/* #define PC             1 */
/* #define VAX            2 */
/* #define SUN            3 */



/* IMS say: */
/* no ansi realloc on vax or sun */
/* I would like to use #ifdef STD_C but life is not like that */

#if defined(HOST_OS_IS_VMS) || !defined(HAVE_ANSI_REALLOC)
#define NO_ANSI_REALLOC
#endif

#if defined(HOST_OS_IS_MSDOS)
#define NO_STDERR_STREAM
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/* 
 * ilibr.c, ilink.c and ilist.c reference 
 * EXIT_FAILURE/SUCCESS but dont include imsstd.h
 */

#ifndef _IMSSTDH
#if defined(HOST_OS_IS_VMS)
#include <ssdef.h>
#define EXIT_FAILURE   SS$_ABORT
#define EXIT_SUCCESS   SS$_NORMAL
#endif  /* TARG == VAX */
#endif /*_IMSSTDH */

/* Should get TRUE/FALSE from imstype.h */
#define  TRUE           1
#define  FALSE          0

#define  COMMENT_STRING "--"    /* only used by toolkit/command7.c */
#define  COMMSWITCHES   "#"     /* only used by ilink.c & toolkit/command7.c */
#if 0
typedef unsigned char uchar;    /* only used by ilink.c */
#endif

/* #endif */ /* TARG */

#ifndef _IMSSTDH
#ifndef  EXIT_FAILURE                   /* Should be using imsstd.h */
#define  EXIT_FAILURE   1
#define  EXIT_SUCCESS   0
#endif
#endif  /* _IMSSTDH */ 


#define ERR_WARNING         0
#define ERR_ERROR           1
#define ERR_SERIOUS         2
#define ERR_FATAL           3




extern char *str_duplicate (char const *);
extern char *str_concat (char const *, char const *);

extern int str_semicmp (char const *, char const *);
extern int str_semicmp_lc (char const *, char const *);
extern int strcmp_lc (char const *, char const *);
extern int strcmp_lc (char const *str1, char const *str2);


#ifdef _ICC
#pragma IMS_on(printf_checking)
#endif
extern int error(int level, char const *progname, char const *filename, char const *fmt, ...);
#ifdef _ICC
#pragma IMS_off(printf_checking)
#endif

extern char *strip_path (char const *);
extern char *derive_string (char const *, int, char const *);
extern void extract_bool_opts (int *, char ***, char *, char **, int **);
extern void extract_string_opts (int *, char ***, char *, char **, char ***);
extern char **extract_multi_string_opts (int *, char ***, char *, char **);
extern void env_to_option (int *, char const ***, char *);
/*
  char *switchars = "+-/\"
  char opts[] =          { "opt1", "opt2", "opt3"... "optn", NULL }
  int boolopts[] =       { &var1,  &var2,  &var3 ... &varn }
  char *stringopts[] =   { &svar1, &svar2, &svar3... &svarn }

  no = unfold_arg_list (&argc, &argv, switchars, switchopts, "ISEARCH")
  extract_bool_opts (&argc, &argv, switchars, boolopts, boolargs);
  extract_string_opts (&argc, &argv, switchars, stropts, strargs);
  ..multi.. as above but multiple occurrences of single option.
  str = derive_string (path, ".ext");
*/

extern int dis_get_char (void);
extern void hexdump (const long int, FILE *);
extern void run_disassemble (const long int, FILE *, int);
/* hexdump/disassemble (start, outfile, print_addr)   */
/* uses: extern int diss_get_char (void);   */

extern void check_ptr (void *);
extern void *malloc_debug (size_t);
extern void *calloc_debug (size_t, size_t);
extern void *realloc_debug (void *, size_t);
extern void free_debug (void *);

/* #define DEBUG_POINTERS */

#ifdef DEBUG_POINTERS
#define malloc_chk malloc_debug
#define realloc_chk realloc_debug
#define calloc_chk calloc_debug
#define free_chk free_debug
#else
extern void *malloc_chk (size_t);
extern void *calloc_chk (size_t, size_t);
extern void *realloc_chk (void *, size_t);
extern void free_chk (void *);
#endif

/*{{{  Pauls command line stuff, used by ilink.c and ilibr.c */
/*{{{   typedef struct command_token   */
typedef struct s_command_token    /* token for command and args           */
{
  char *string;             /* actual token                         */
  struct s_command_token *next;   /* next in list                         */
}
command_token;
/*}}}  */
/*{{{   typedef struct command_line   */
typedef struct s_command_line   /* command line                         */
{
  char *file_name;              /* name of file                         */
  int line_number;              /* line number                          */
  command_token *tokens;        /* tokens for line                      */
  struct s_command_line *next;  /* next line                            */
}
command_line;
/*}}}  */
/*{{{   typedef enum command_control   */
typedef enum
{
  com_continue  = 1,        /* continue after processing            */
  com_terminate = 2         /* terminate immediately                */
}
command_control;
/*}}}  */
/*{{{   typedef enum command_options   */
typedef enum
{
  com_line      = 1,      /* command */
  com_token     = 2,      /* normal command line (only one of these) */
  com_error     = 3,      /* error routine */
  com_end       = 4       /* last entry in table */
} command_options;
/*}}}  */
/*{{{   typedef struct command_descriptor   */
typedef struct 
{
    char const *command;        /* string for command                   */
    command_options com_type;   /* type of the argument                 */
    command_control (*com_fn)(command_line *);
				/* function to call for command.        */
				/* if com_type is a token, the whole    */
				/* line is passed. If its a command,    */
				/* only the arguments */
} command_descriptor;
/*}}}  */

/* Command file parsing functions: similar 'feel' to ahp's routines.
 */ 
extern command_line *mk_command (char const *fname);
/*
 * Returns a single 'command' with empty file name and line number of 0.
 * Used for making 'command' out of file name from command line.
 */
extern command_line *add_command (command_line *command, command_line *list);
/* 
 * Adds 'command' to end of command list.
 */
extern command_line *read_indirect_file (char const *fname, 
					 char const *search_path, 
					 char const *progname,
					 char const *include,
					 command_descriptor const desc[],
					 int *ok);
/* Calls functions described in desc, passing the line to which they apply.
 * Finds files on search path.
 * Returns all lines containing no commands on success.
 * Returns non zero in ok on failure (Null is valid return)
 */ 
extern void print_command_structure (command_line const *);
/*
 * prints out command structure.
 */
/*}}}  */

