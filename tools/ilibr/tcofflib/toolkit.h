/*
 *	Standard toolkit header file
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
   PLAY:TOOLKIT_H@525.AAAA-FILE;2(25-JAN-93)[FROZEN] */
/* ahp : 930125 : Removed redefinitions of tolower and toupper */


#define  TRUE           1
#define  FALSE          0
#define  PUBLIC
#define  PRIVATE        static
#define  COMMSWITCHES   "#"
#define  COMMENT_STRING "--"

#ifndef  EXIT_FAILURE
#define  EXIT_FAILURE   1
#define  EXIT_SUCCESS   0
#endif

#ifndef  SEEK_SET
#define  SEEK_SET       0
#define  SEEK_CUR       1
#define  SEEK_END       2
#endif

#define ERR_WARNING         0
#define ERR_ERROR           1
#define ERR_SERIOUS         2
#define ERR_FATAL           3


#ifndef __STDC__
#define VOID ()
typedef unsigned int size_t;
#else
#define VOID (void)
#endif

#ifndef __alpha
typedef unsigned char uchar;
#endif

/*{{{  heap structure   */
struct s_free_chain
{
  void *mem;
  struct s_free_chain *next;
};

struct s_heap
{
  size_t obj_size;
  int n_obj;
  int next_free_mem;
  int next_block;
  void *cblock;
  void **blocks;
  struct s_free_chain *free_chain;
};
/*}}}  */

#ifdef __STDC__
#define PARMS(x) x
#else
#define PARMS(x)
#endif

#include "tcofflib.h" /* standard functions */

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
extern command_line *mk_command PARMS((char const *fname));
/*
 * Returns a single 'command' with empty file name and line number of 0.
 * Used for making 'command' out of file name from command line.
 */
extern command_line *add_command PARMS((command_line *command, command_line *list));
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

extern struct s_heap *start_heap PARMS((size_t, int));
extern void *alt_malloc PARMS((struct s_heap *));
extern void alt_free PARMS((struct s_heap *, void *));
extern void print_heap_info PARMS((struct s_heap *));

extern int str_semicmp PARMS((char const *, char const *));
extern int str_semicmp_lc PARMS((char const *, char const *));
extern int strcmp_lc PARMS((char const *, char const *));
extern int strcmp_lc PARMS((char const *str1, char const *str2));

#ifdef _ICC
#pragma IMS_on(printf_checking)
#endif
extern int error PARMS ((int level, char const *progname, char const *filename, char const *fmt, ...));
#ifdef _ICC
#pragma IMS_off(printf_checking)
#endif

extern char *strip_path PARMS((char const *));
extern char *derive_string PARMS((char const *, int, char const *));
extern void extract_bool_opts PARMS((int *, char ***, char *, char **, int **));
extern void extract_string_opts PARMS((int *, char ***, char *, char **, char ***));
extern char **extract_multi_string_opts PARMS((int *, char ***, char *, char **));
extern int unfold_arg_list PARMS((int *, char ***, char *, char **, char *));
extern void env_to_option PARMS((int *, char const ***, char *));
extern int get_indcom PARMS((char*, int *, char ***, char *, char **, char *));
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

extern FILE *pathopen PARMS((char const *, char const *, char *, const char *));
/* fs = pathopen PARMS((fname, environ_var, full_path_name, mode)); */

extern int dis_get_char PARMS((void));
extern void hexdump PARMS((const long int, FILE *));
extern void disassemble PARMS((const long int, FILE *, int));
/* hexdump/disassemble (start, outfile, print_addr)   */
/* uses: extern int diss_get_char (void);   */

extern void check_ptr PARMS((void *));
extern void *malloc_debug PARMS((size_t));
extern void *calloc_debug PARMS((size_t, size_t));
extern void *realloc_debug PARMS((void *, size_t));
extern void free_debug PARMS((void *));

/* #define DEBUG_POINTERS */

#ifdef DEBUG_POINTERS
#define malloc_chk malloc_debug
#define realloc_chk realloc_debug
#define calloc_chk calloc_debug
#define free_chk free_debug
#else
extern void *malloc_chk PARMS((size_t));
extern void *calloc_chk PARMS((size_t, size_t));
extern void *realloc_chk PARMS((void *, size_t));
extern void free_chk PARMS ((void *));
#endif
