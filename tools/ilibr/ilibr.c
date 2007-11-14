/*
 *	Inmos Librarian (ILIBR)
 *	Copyright (C) 1990, 1991 Inmos Limited
 *	Portions (C) 1995 Michael Poole
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

/* Copyright 1990, 1991 INMOS Limited */
/* CMSIDENTIFIER
   PLAY:ILIBR_C@267.AAAA-FILE;3(03-JUL-92)[FROZEN] */

/******************************************************************************
*
* 30 JAN 90: pauls: introduced check for mixing linked and linkable modules.
* 10 MAR 90: pauls: in qsort, also compare by origin.
* 14 MAR 90: pauls: put standard error stuff into code.
* 16 MAR 90: pauls: put standard indirect file reader into code.
* 30 MAR 90: pauls: added specific_module_tag
* 14 APR 90: pauls: fixed 'ignore nonexistand lbb file' bug. (4.800)
* 26 JUN 90: pauls: xm/xo/l now conforms. (B 1.00)
* 6  JUL 90: pauls: cast some constants to long for PC ! (B 1.00)
* 29 JAN 91: agw  : modified for H1, modified comparison, added cmp2
* 27 MAY 92: ahp  : removed references to H1
*  3 JUL 92: ahp  : made help page standard, new version number
* 13 Apr 93: ade  : T9 now has Y/non-Y attribute in cmp(), new version no.
* 19 May 93: ade  : remove #define const
* 17 Jun 93: ade  : added fn_file_err() for options file error
* 14 Jun 95: mdp  : change use of popen to pathopen
* 14 Jun 95: mdp  : get compilation date into version name
* 13 Oct 02: frmb : removed old transputer stuff
*
******************************************************************************/


/*{{{  includes   */
#include <stdio.h>
#include <string.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <ctype.h>
#include "arg.h"
#include "tcoff.h"
#include "toolkit.h"
/*}}}  */
#define PROG            "ilibr"
#define ENV_SEARCH_VAR  "ISEARCH"
#define ENV_COMMAND_VAR "ILIBRARG"
#define VERSION         "2.01.03"
#define FULL            "F"
#define INFO            "I"
#define INDIRECT        "F"
#define OUTPUT          "O"
#define LOAD_WAIT       "L"
#define EXECUTE_ONCE    "XO"
#define EXECUTE_MANY    "XM"
#define DEBUG           "Z"
#define DEBUG_PATH      "ZI"
#define NO_DESC         "ZND"
#define EXTENSION       ".lib"
#define ORIGIN_LEN      80
#define INCLUDECOMMAND "INCLUDE"

#define MAX_PATH_SIZE  100
#define print_info   printf ("%s Version %s, (%s)\nCompiled from SGS-THOMSON sources with permission\n", MACHINE, VERSION, __DATE__)

#ifdef _MSC
#define switch(x) switch ((int)(x))
#endif
/*{{{  comment   */
/*
This program prepends an index onto the front of a tcoff file. Each entry
in the index contains information enabling the linker to selectively pick
the modules it needs to resolve any outstanding references.

The first phase is to search through each module looking for relevant
symbols (ie EXTERNAL but not UNINDEXED, WEAK or CONDITIONAL and not
sections) and build up a local index. Each entry contains the symbol and
the position in the file of the module that contains it.

Each time a symbol is defined, mark its entry in the index. At the end of
each module, delete from the local index any undefined symbols and add
this local index to a global index.

When all the modules have been searched, the size of the index is known and
the index is output During output, the index size is added to the module
positions in each entry.

The second phase is to append the input file to the index in the output file.

NOTES:
  setvbuf upsets the hosted vax fseek operation and so was removed, still
  dont know why.
*/
/*}}}  */

/*{{{  machine dependancies   */
#define IO_BUFF_SIZE   10240
#define MACHINE        "PC"
#define exit_repeat    exit

/*}}}  */
/*{{{  errors  */
#define SER_BAD_OPEN_INPUT(FILE_NAME) \
    error (ERR_SERIOUS, PROG, FILE_NAME, "could not open for reading")

#define SER_COMMAND_LINE(STR) \
    error (ERR_SERIOUS, PROG, "", "command line error '%s'", STR)

#define SER_BAD_OPEN_OUTPUT(FILE_NAME) \
    error (ERR_SERIOUS, PROG, FILE_NAME, "could not open for writing")

#define SER_END_OF_FILE(FILE_NAME) \
    error (ERR_SERIOUS, PROG, FILE_NAME, "bad format: unexpected end of file")

#define SER_BAD_FORMAT(FILE_NAME, REASON) \
    error (ERR_SERIOUS, PROG, FILE_NAME, "bad format: %s", REASON)

#define SER_MIX(FILE_NAME) \
    error (ERR_SERIOUS, PROG, FILE_NAME, "must not mix linked and linkable files")

#define SER_BAD_CMD(CMD) \
    error (ERR_SERIOUS, PROG, "", "indirect file: unknown command '#%s'", CMD)

#define WRN_MULT_EXP_SAME(FILE_NAME, SYM) \
    error (ERR_WARNING, PROG, FILE_NAME, "bad format: symbol %s multiply exported", SYM)

#define WRN_MULT_EXP_DIFF(FILE_NAME, FILE_NAME2, SYM) \
    error (ERR_WARNING, PROG, FILE_NAME, "symbol %s also exported by %s", SYM, FILE_NAME2)

#define SER_NO_INPUT\
    error (ERR_SERIOUS, PROG, "", "no files supplied")

#define SER_NO_ENTRIES\
    error (ERR_SERIOUS, PROG, "", "no entries for index")

#define SER_BAD_FILE_IN_IND(INDFILE, N, FILE)\
    error (ERR_SERIOUS, PROG, INDFILE, "%d-could not open %s for reading", N, FILE);

#define SER_BAD_COMMAND(INDFILE, N, STR)\
    error (ERR_SERIOUS, PROG, INDFILE, "%d-unrecognised directive %s", N, STR)

#define SER_BAD_COM_TOK(INDFILE, N)\
    error (ERR_SERIOUS, PROG, INDFILE, "%d-only one file name per line", N)

#define FTL_INTERNAL(ERRNUM) \
    error (ERR_FATAL, PROG, "", "internal error %d please report", ERRNUM)

#define ERR_STUFFED_BUFFER     1
#define ERR_NO_PARSE_ARG       2

/*}}}  */
/*{{{  vars  */
PRIVATE long int start_tag;
PRIVATE int modules = 0;
PRIVATE int no_desc = FALSE;
PRIVATE int information = FALSE;
PRIVATE int debug = FALSE;
PRIVATE int execute_once = FALSE;
PRIVATE int execute_many = FALSE;
PRIVATE int load_wait = FALSE;
PRIVATE int tokens = 0;
PRIVATE char *search_path = "";
PRIVATE char *outfile = "";
PRIVATE char *thisfile = "";
PRIVATE command_line *file_list = NULL;
/*}}}  */

/*{{{  command file parsing  */
/*{{{  PRIVATE command_control fn_com_token (line)  */
PRIVATE command_control fn_com_token (line)
command_line *line;
{
  command_token *tok;
  tok = line->tokens;
  if (tok->next != NULL) SER_BAD_COM_TOK (line->file_name, line->line_number);
  return (com_continue);
}
/*}}}  */
/*{{{  PRIVATE command_control fn_com_error (line)  */
PRIVATE command_control fn_com_error (line)
command_line *line;
{
  SER_BAD_COMMAND (line->file_name, line->line_number, line->tokens->string);
  return (com_continue);
}
/*}}}  */
/*{{{  PRIVATE command_control fn_com_end (line)  */
PRIVATE command_control fn_com_end (line)
command_line *line;
{
  line = line;
  return (com_continue);
}
/*}}}  */

/* ok, I admit it, there arn't any commands here */

PRIVATE const command_descriptor commands[] =
{
  { "",            com_token,    fn_com_token },
  { "",            com_error,    fn_com_error },
  { "",            com_end,      fn_com_end }
};
/*}}}  */
/*{{{  argument parsing  */
/*{{{  arg_control fn_output (str)   */
arg_control fn_output (str)
char *str;
{
  tokens++;
  outfile = str;
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_debug_path (str)   */
arg_control fn_debug_path (str)
char *str;
{
  tokens++;
  search_path = str;
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_debug (str)   */
arg_control fn_debug (str)
char *str;
{
  tokens++;
  str = str;
  debug = TRUE;
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_info (str)   */
arg_control fn_info (str)
char *str;
{
  str = str;
  tokens++;
  information = TRUE;
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_execute_once (str)   */
arg_control fn_execute_once (str)
char *str;
{
  tokens++;
  str = str;
  execute_once = TRUE;
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_execute_many (str)   */
arg_control fn_execute_many (str)
char *str;
{
  tokens++;
  str = str;
  execute_many = TRUE;
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_load_wait (str)   */
arg_control fn_load_wait (str)
char *str;
{
  tokens++;
  str = str;
  load_wait = TRUE;
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_no_desc (str)   */
arg_control fn_no_desc (str)
char *str;
{
  tokens++;
  str = str;
  no_desc = TRUE;
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_end (str)   */
arg_control fn_end (str)
char *str;
{
  str = str;
  /* do nothing */
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_err (str)   */
arg_control fn_err (str)
char *str;
{
  SER_COMMAND_LINE (str);
  return (arg_terminate);
}
/*}}}  */
/*{{{  arg_control fn_file_err (str)   */
arg_control fn_file_err (str)
char *str;
{
  SER_BAD_OPEN_INPUT(str);
  return (arg_terminate);
}
/*}}}  */
/*{{{  arg_control fn_help (str)   */
arg_control fn_help (str)
char *str;
{
  str = str;
  printf ("%s : INMOS toolset librarian tool\n", PROG);
  print_info;
  printf ("(c) Copyright INMOS Limited 1990, 1991, 1992, 1993.\n");
  printf ("\n");
  printf ("Usage: %s {option} filename\n\nOptions:\n\n", PROG);
  printf ("   %s{filename}\tspecify an indirect file.\n", INDIRECT);
  printf ("   %s\t\tprint information.\n", INFO);
  printf ("   %s{filename}\tspecify an output file.\n", OUTPUT);
  printf ("\n");
  printf ("   Default options may be placed in the environment variable %s\n", ENV_COMMAND_VAR);
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_input (str)   */
arg_control fn_input (str)
char *str;
{
  command_line *new_file;
  tokens++;
  new_file = mk_command (str);
  file_list = add_command (new_file, file_list);
  return (arg_continue);
}
/*}}}  */
/*{{{  arg_control fn_indirect (str)   */
arg_control fn_indirect (str)
char *str;
{
  int ok;
  command_line *new_file;
  tokens++;
  new_file = read_indirect_file (str, search_path, PROG, "INCLUDE", commands, &ok);
  if (!ok) SER_BAD_OPEN_INPUT (str);

  file_list = add_command (new_file, file_list);
  return (arg_continue);
}
/*}}}  */

PRIVATE const arg_descriptor argd [] =
{
  { OUTPUT,        arg_operand,  fn_output },
  { DEBUG_PATH,    arg_operand,  fn_debug_path },
  { INDIRECT,      arg_operand,  fn_indirect },
  { INFO,          arg_single,   fn_info },
  { LOAD_WAIT,     arg_single,   fn_load_wait },
  { EXECUTE_MANY,  arg_single,   fn_execute_many },
  { EXECUTE_ONCE,  arg_single,   fn_execute_once },
  { NO_DESC,       arg_single,   fn_no_desc },
  { DEBUG,         arg_single,   fn_debug },
  { "",            arg_token,    fn_input },
  { "",            arg_help,     fn_help },
  { "",            arg_error,    fn_err },
/*  { "",            arg_file_err, fn_file_err},  */
  { "",            arg_end,      fn_end }
};
/*}}}  */




/*{{{  structures   */
struct s_id_stack
{
  long int id;
  struct s_id_stack *next;
};

struct s_opt_list
{
  long int arg;
  struct s_opt_list *next;
};

struct s_index
{
  /* these elements represent the actual record */
  long int rec_len;
  unsigned long int pos;
  long int trans_func;
  long int err_mode;
  long int lang;
  long int desclen;
  char *desc;
  long int strlen;
  int is_origin;
  char *string;
  /* these elements are needed during index construction */
  char *origin;
  char *fname;
  int error_printed;
  long int total_size;
  long int id;
  int defined;
  struct s_index *next;
};
/*}}}  */
/*{{{  PRIVATE long int tcoff_getl (fs)   */
PRIVATE long int tcoff_getl (fs)
FILE *fs;
{
  int ok;
  long int n;
  n = tcoff_getl_test (fs, &ok);
  if (!ok) SER_END_OF_FILE (thisfile);
  return (n);
}
/*}}}  */
/*{{{  PRIVATE char *tcoff_gets (fs, len)   */
PRIVATE char *tcoff_gets (fs, len)
FILE *fs;
long int *len;
{
  int ok;
  char *str;
  str = tcoff_gets_test (fs, len, &ok);
  if (!ok) SER_END_OF_FILE (thisfile);
  return (str);
}
/*}}}  */
/*{{{  PRIVATE void tcoff_throw_record (fs)   */
PRIVATE void tcoff_throw_record (fs)
FILE *fs;
{
  long int l, i;
  l = tcoff_getl (fs);
  if (l >= IO_BUFF_SIZE) 
  {
    if (fseek (fs, l, SEEK_CUR) != 0) SER_END_OF_FILE (thisfile);

  }
  else for (i = 0; i < l; i++) 
  {
    if (fgetc (fs) == EOF) SER_END_OF_FILE (thisfile);
  }
}
/*}}}  */
/*{{{  PRIVATE void throw_value (fs)   */
PRIVATE void throw_value (fs)
FILE *fs;
{
  long int n;
  n = (int) tcoff_getl (fs);
  switch (n)
  {
    case CO_VALUE_TAG:
    case SS_VALUE_TAG:
    case SV_VALUE_TAG: (void) tcoff_getl (fs); break;
    case PLUS_OP:
    case MINUS_OP:
    case TIMES_OP:
    case DIVIDE_OP:
    case REM_OP:
    case MAX_OP:
    case MIN_OP:       throw_value (fs); throw_value (fs); break;
    case LP_VALUE_TAG:
    case WL_VALUE_TAG: break;
    default:           SER_BAD_FORMAT (thisfile, "unknown expression type");
  }
}
/*}}}  */
/*{{{  PRIVATE void fcopy (infs, outfs)   */
PRIVATE void fcopy (infs, outfs)
FILE *infs, *outfs;
{
  int c;
  long int x, tmppos;
  tmppos = ftell (infs);
  x = tcoff_getl (infs); /* format tag */
  tcoff_throw_record (infs);

  x = tcoff_getl (infs);
  if (x == LIB_INDEX_START_TAG)
  {
    tcoff_throw_record (infs);
    do
    {
      x = tcoff_getl (infs);
      tcoff_throw_record (infs);
    }
    while (x != LIB_INDEX_END_TAG);
  }
  else fseek (infs, tmppos, SEEK_SET);

  while ((c = fgetc (infs)) != EOF) fputc (c, outfs);
}
/*}}}  */
/*{{{  PRIVATE struct s_index *strip_index (index, total)   */
PRIVATE struct s_index *strip_index (index, total)
struct s_index *index;
long int *total;
{
  /* weed out index entries where the symbol was not defined
  within its module. This is done prior to the outputting so that the
  final index size is known */
  struct s_index *new_index, *tmp;
  new_index = NULL;
  while (index != NULL)
  {
    if (index->defined) /* ok, leave in index */
    {
      tmp = malloc_chk (sizeof (struct s_index));
      memcpy (tmp, index , sizeof (struct s_index));

      tmp->next = new_index;
      new_index = tmp;
    }
    else /* never defined, so remove from index */
    {
      free (index->string);
      *total -= index->total_size;
    }
    tmp = index;
    index = index->next;
    free (tmp);
  }
  return (new_index);
}
/*}}}  */
/*{{{  PRIVATE void write_index (fs, index, total)   */
PRIVATE void write_index (fs, index, total)
FILE *fs;
struct s_index *index;
long int total;
{
  /* output index, adding the index size to the all file positions */
  unsigned long int index_size;
  index_size = (unsigned long int) (tcoff_sizel (start_tag) + tcoff_sizel (0L) +
                tcoff_sizel ((long int)LIB_INDEX_START_TAG) + tcoff_sizel (0L) +
                total +
                tcoff_sizel ((long int) LIB_INDEX_END_TAG) + tcoff_sizel (0L));
  if (information) printf ("writing %lu byte index to %s\n", index_size, outfile);
  tcoff_putrec (fs, start_tag, "");
  tcoff_putrec (fs, (long int) LIB_INDEX_START_TAG, "");
  while (index != NULL)
  {
    tcoff_putl (fs, (long)INDEX_ENTRY_TAG);
    tcoff_putl (fs, index->rec_len);
    tcoff_putul (fs, index->pos + index_size);
    tcoff_putl (fs, index->trans_func);
    tcoff_putl (fs, index->err_mode);
    tcoff_putl (fs, index->lang);
    tcoff_putl (fs, index->desclen);
    fwrite (index->desc, sizeof (char), (size_t) index->desclen, fs);
    tcoff_putl (fs, index->strlen);
    fwrite (index->string, sizeof (char), (size_t) index->strlen, fs);
    index = index->next;
  }
  tcoff_putrec (fs, (long int)LIB_INDEX_END_TAG, "");
}
/*}}}  */
/*{{{  PRIVATE struct s_index *index_module (infs, index, total, fstart, fname)   */
PRIVATE struct s_index *index_module (infs, index, total, fstart, fname)
FILE *infs;
struct s_index *index;
long int *total;
unsigned long int fstart;
char *fname;
{
  /*{{{  boring   */
  long int x, type, scope, err_mode, trans_func, str_len, id;
  long int mod_lang, desc_len, desc_lang;
  struct s_id_stack *stk, *stack;
  unsigned long int pos, start_pos;
  char *str, *desc, *origin;
  int ok, level;
  struct s_index *sub_index, *entry;
  int n;
  n = 0;
  stack = NULL;
  sub_index = NULL;
  level = 0;
  /*}}}  */
  pos = (unsigned long int) ftell (infs);
  x = tcoff_getl_test (infs, &ok);
  while (ok)
  {
    switch (x)
    {
      /*{{{  case SECTION_TAG:   */
      case SECTION_TAG:
        if (level <= 0) SER_BAD_FORMAT (thisfile, "directive outside module");

        (void) tcoff_getl (infs);
        type = tcoff_getl (infs);
        scope = tcoff_getl (infs);
        str = tcoff_gets (infs, &str_len);
        if ((type == 0L) && ((EXPORT_USAGE & scope) != 0L) &&
           (((UNINDEXED_USAGE | CONDITIONAL_USAGE | WEAK_USAGE) & scope) == 0L))
        {
          entry = malloc_chk (sizeof (struct s_index));
          entry->rec_len = 4L +                              /* position */
                           tcoff_sizel (trans_func) +        /* trans func */
                           tcoff_sizel (err_mode) +          /* error mode */
                           tcoff_sizel (mod_lang) +          /* language */
                           tcoff_sizel (0L) +                /* descriptor (null) */
                           tcoff_sizel (str_len) + str_len;  /* symbol name */

          entry->total_size = tcoff_sizel ((long int) INDEX_ENTRY_TAG) +/* tag */
                              tcoff_sizel (entry->rec_len) + /* record */
                              entry->rec_len;                /* record length */

          *total += entry->total_size;

          entry->pos = start_pos;
          entry->trans_func = trans_func;
          entry->err_mode = err_mode;
          entry->lang = mod_lang;
          entry->desclen = 0L;
          entry->desc = "";
          entry->strlen = str_len;
          entry->string = str;
          entry->is_origin = FALSE;
          entry->id = stack->id;
          entry->defined = FALSE;
          entry->fname = fname;
          entry->origin = origin;
          entry->error_printed = FALSE;
          entry->next = sub_index;
          sub_index = entry;
        }
        else free (str);
        stack->id++;
        break;
      /*}}}  */
      /*{{{  case SPECIFIC_SYMBOL_TAG and SYMBOL_TAG:   */
      case SPECIFIC_SYMBOL_TAG:
      case SYMBOL_TAG:
        if (level <= 0) SER_BAD_FORMAT (thisfile, "directive outside module");

        (void) tcoff_getl (infs);
        scope = tcoff_getl (infs);
        str = tcoff_gets (infs, &str_len);
        if (x == SPECIFIC_SYMBOL_TAG) (void) tcoff_getl (infs);
        if (((EXPORT_USAGE & scope) != 0L) &&
           (((UNINDEXED_USAGE | CONDITIONAL_USAGE | WEAK_USAGE) & scope) == 0L))
        {
          entry = malloc_chk (sizeof (struct s_index));
          entry->rec_len = 4L +                              /* position */
                           tcoff_sizel (trans_func) +        /* trans func */
                           tcoff_sizel (err_mode) +          /* error mode */
                           tcoff_sizel (mod_lang) +          /* language */
                           tcoff_sizel (0L) +                /* descriptor (null) */
                           tcoff_sizel (str_len) + str_len;  /* symbol name */

          entry->total_size = tcoff_sizel ((long int)INDEX_ENTRY_TAG) +/* tag */
                              tcoff_sizel (entry->rec_len) + /* record */
                              entry->rec_len;                /* record length */

          *total += entry->total_size;

          entry->pos = start_pos;
          entry->trans_func = trans_func;
          entry->err_mode = err_mode;
          entry->lang = mod_lang;
          entry->desclen = 0L;
          entry->desc = "";
          entry->strlen = str_len;
          entry->string = str;
          entry->is_origin = ((scope & ORIGIN_USAGE) != 0L);
          entry->id = stack->id;
          if ((scope & ORIGIN_USAGE) != 0L) entry->defined = TRUE;
          else entry->defined = FALSE;
          entry->origin = origin;
          entry->fname = fname;
          entry->error_printed = FALSE;
          entry->next = sub_index;
          sub_index = entry;
        }
        else free (str);
        stack->id++;
        break;
      /*}}}  */
      /*{{{  case DESCRIPTOR_TAG: */
            case DESCRIPTOR_TAG:
              if (level <= 0) SER_BAD_FORMAT (thisfile, "directive outside module");
              if (no_desc) tcoff_throw_record (infs);
              else
              {
                (void) tcoff_getl (infs);
                id = tcoff_getl (infs);
                desc_lang = tcoff_getl (infs);
                desc = tcoff_gets (infs, &desc_len);

                entry = sub_index;
                while ((entry != NULL) && (entry->id != id)) entry = entry->next;
                if (entry == NULL) free (desc);
                else
                {
                  if (entry->desc[0] != '\0') SER_BAD_FORMAT (thisfile, "multiple descriptors for symbol");

                  /* replace descriptor and language in index entry */
                  /* fix up count of bytes used etc. by calulating the difference */

                  *total -= entry->total_size;

                  entry->rec_len -= (tcoff_sizel (entry->lang)    +
                                     tcoff_sizel (entry->desclen) + entry->desclen);
                  entry->rec_len += (tcoff_sizel (desc_lang)      +
                                     tcoff_sizel (desc_len)       + desc_len);

                  entry->total_size = tcoff_sizel ((long int)INDEX_ENTRY_TAG) +/* tag */
                                      tcoff_sizel (entry->rec_len) + /* record */
                                      entry->rec_len;                /* record length */

                  *total += entry->total_size;

                  entry->lang = desc_lang;
                  entry->desclen = desc_len;
                  entry->desc = desc;
                }
              }
              break;
      /*}}}  */
      /*{{{  case DEFINE_LABEL_TAG:   */
      case DEFINE_LABEL_TAG:
        if (level <= 0) SER_BAD_FORMAT (thisfile, "directive outside module");
        (void) tcoff_getl (infs);
        id = tcoff_getl (infs);
        entry = sub_index;
        while ((entry != NULL) && (entry->id != id)) entry = entry->next;
        if (entry != NULL) entry->defined = TRUE;
        break;
      /*}}}  */
      /*{{{  case DEFINE_SYMBOL_TAG:   */
      case DEFINE_SYMBOL_TAG:
        if (level <= 0) SER_BAD_FORMAT (thisfile, "directive outside module");
        (void) tcoff_getl (infs);
        id = tcoff_getl (infs);
        throw_value (infs);
        entry = sub_index;
        while ((entry != NULL) && (entry->id != id)) entry = entry->next;
        if (entry != NULL) entry->defined = TRUE;
        break;
      /*}}}  */
      /*{{{  case START_MODULE_TAG:   */
      case START_MODULE_TAG:
        if (level == 0)
        {
          origin = malloc_chk (ORIGIN_LEN * sizeof (char));
          *origin = '\0';
          start_pos = fstart + pos;
          (void) tcoff_getl (infs); /*length */
          trans_func = tcoff_getl (infs);
          err_mode = tcoff_getl (infs);
          mod_lang = tcoff_getl (infs);
          /* if we say NO DESCRIPTORS! in the index we should say that an entry
          is NOT occam, since if it were, a descriptor would be expected */
          if (no_desc && 
             ((mod_lang == LANG_OCCAM) || 
              (mod_lang == LANG_OCCAM_HARNESS))) mod_lang = LANG_NOT_KNOWN;
          tcoff_throw_record (infs); /* string */
          n++;
        }
        else tcoff_throw_record (infs);
        stk = malloc_chk (sizeof (struct s_id_stack));
        stk->id = 0L;
        stk->next = stack;
        stack = stk;
        level++;
        break;
      /*}}}  */
      /*{{{  case END_MODULE_TAG:   */
      case END_MODULE_TAG:
        if (stack == NULL) SER_BAD_FORMAT (thisfile, "unmatched end module");
        if (--level == 0) /* join sub_index to index */
        {
          entry = sub_index;
          if (entry != NULL)
          {
            while (entry->next != NULL) entry = entry->next;
            entry->next = index;
            index = sub_index;
            sub_index = NULL;
          }
        }
        stk = stack;
        stack = stack->next;
        free (stk);
        tcoff_throw_record (infs);
        break;
      /*}}}  */
      /*{{{  case LINKABLE_TAG:   */
      case LINKABLE_TAG:
         if (start_tag != LINKABLE_TAG) SER_MIX (thisfile);
        /* valid outside module */
        tcoff_throw_record (infs);
        break;
      /*}}}  */
      /*{{{  case LINKED_UNIT_TAG:   */
      case LINKED_UNIT_TAG:
         if (start_tag != LINKED_UNIT_TAG) SER_MIX (thisfile);
        /* valid outside module */
        tcoff_throw_record (infs);
        break;
      /*}}}  */
      /*{{{  case VERSION_TAG:   */
      case VERSION_TAG:
        (void) tcoff_getl (infs); /*length */  
        tcoff_throw_record (infs); /* tool id */
        str = tcoff_gets (infs, &str_len);
        strncpy (origin, str, ORIGIN_LEN);
        free (str);
        break;
      /*}}}  */
      /*{{{  default */
      default:
        tcoff_throw_record (infs);
        break;

      /*}}}  */
    }
    pos = (unsigned long int) ftell (infs);
    x = tcoff_getl_test (infs, &ok);
  }
  if (information) printf ("Modules: %d\n", n);
  modules += n;
  return (index);
}
/*}}}  */
/*{{{  PRIVATE int n_bits (n)   */
PRIVATE int n_bits (n)
long int n;
{
  int total;
  total = 0;
  while (n != 0L)
  {
    total += (int) (n & 1);
    n = n >> 1;
  }
  return (total);
}
/*}}}  */
/*{{{  PRIVATE int cmp2 (a, b)   */
PRIVATE int cmp2 (a, b)
struct s_index *a, *b;
{
  int res, ac, bc;

  ac = n_bits (a->trans_func) + n_bits (a->err_mode);
  bc = n_bits (b->trans_func) + n_bits (b->err_mode);
  if (ac > bc) res = -1;
  else if (ac < bc) res = 1;
  else 
  {
    if (a->trans_func > b->trans_func) res = -1;
    else if (a->trans_func < b->trans_func) res = 1;
    else 
    {
      if (a->err_mode > b->err_mode) res = -1;
      else if (a->err_mode < b->err_mode) res = 1;
      else
      { 
        res = 0;
        if ((a != b) && !a->error_printed)
        {
          a->error_printed = TRUE;
          b->error_printed = TRUE;
          if (strcmp (a->fname, b->fname) == 0)
            WRN_MULT_EXP_SAME (a->fname, a->string);
          else 
            WRN_MULT_EXP_DIFF (a->fname, b->fname, a->string);
        }
      }
    }
  }
  return (res);
}
/*}}}  */
/*{{{  PRIVATE int cmp (a, b)   */
PRIVATE int cmp (x, y)
struct s_index **x, **y;
{
  struct s_index *a, *b;
  int res, ay, by;
  a = *x; b = *y;
  if (a->is_origin && !b->is_origin) res = -1;
  else if (b->is_origin && !a->is_origin) res = 1;
  else
  {
    res = strcmp (a->string, b->string);
    if (res == 0)
    {
        ay = ((a->err_mode & ATTRIB_INSTR_IO) != 0L);
        by = ((b->err_mode & ATTRIB_INSTR_IO) != 0L);
        if (ay && !by) res = -1;
        else if (!ay && by) res = 1;
        else res = cmp2(a, b);
    }
  }
  return (res);
}
/*}}}  */
/*{{{  PRIVATE struct s_index *sort_index (old_index)   */
PRIVATE struct s_index *sort_index (old_index)
struct s_index *old_index;
{
  struct s_index *ptr, **array, *new_index;
  int i, entries;
  entries = 0;
  if (information) printf ("sorting index\n");
  ptr = old_index;
  while (ptr != NULL) {entries++; ptr = ptr->next;}

  if (entries == 0) SER_NO_ENTRIES;
  array = malloc_chk (entries * sizeof (struct s_index *));

  ptr = old_index;
  for (i = 0; i < entries; i++)
  {
    array[i] = ptr;
    ptr = ptr->next;
  }

  qsort ((void *) array, entries, sizeof (struct s_index *),
         (int (*)(const void *, const void *)) cmp);

  for (i = 0; i < entries - 1; i++) array[i]->next = array[i+1];
  new_index = array[0];
  array[entries - 1]->next = NULL;
  free (array);  

  return (new_index);
}
/*}}}  */
/*{{{  PRIVATE void mk_index (file_list)   */
PRIVATE void mk_index (file_list)
command_line *file_list;
{
  /*
  char inbuff[IO_BUFF_SIZE];
  char outbuff[IO_BUFF_SIZE];
  */
  command_line *files;
  char *outbuff;
  char *inbuff;
  char full_name[MAX_PATH_SIZE];
  long int total, x;
  unsigned long int fstart;
  int ok;
  long tmppos;
  FILE *infs, *outfs;
  struct s_index *index;
  inbuff = malloc_chk (IO_BUFF_SIZE * sizeof (char));
  outbuff = malloc_chk (IO_BUFF_SIZE * sizeof (char));
  index = NULL;
  total = 0L;
  fstart = 0L;

  files = file_list;
  while (files != NULL)
  {
    thisfile = files->tokens->string;
    infs = pathopen (thisfile, search_path, full_name, "rb");
    if (infs == NULL) 
    {
      if (files->line_number == 0) SER_BAD_OPEN_INPUT (thisfile);
      else SER_BAD_FILE_IN_IND (files->file_name, files->line_number, thisfile);
    }
    ok = setvbuf (infs, inbuff, _IOFBF, IO_BUFF_SIZE);
    if (ok != 0) FTL_INTERNAL (ERR_STUFFED_BUFFER);
    if (information)
    {
      printf ("Reading %s ", thisfile);
      fflush (stdout);
    }
    x = tcoff_getl_test (infs, &ok);
    if (!ok || !((x == LINKED_UNIT_TAG) || (x == LINKABLE_TAG)))
      SER_BAD_FORMAT (thisfile, "not a TCOFF file");
    if (start_tag == 0L) start_tag = x;
    else if (x != start_tag) SER_MIX (thisfile);
    tcoff_throw_record (infs);
    tmppos = ftell (infs);
    x = tcoff_getl (infs);
    if (x == LIB_INDEX_START_TAG)
    {
      tcoff_throw_record (infs);
      if (information) printf ("(already a library) ");
      fflush (stdout);
      do
      {
        x = tcoff_getl (infs);
        tcoff_throw_record (infs);
      }
      while (x != LIB_INDEX_END_TAG);
      fstart -= ftell (infs); /* subtract index size */
    }
    else fseek (infs, tmppos, SEEK_SET);
    index = index_module (infs, index, &total, fstart, thisfile);
    fstart += ftell (infs);
    fclose (infs);
    files->tokens->string = str_duplicate (full_name);
    files = files->next;
  }
  index = strip_index (index, &total);
  index = sort_index (index);

  if (information) printf ("creating %s\n", outfile);
  outfs = fopen (outfile, "wb");
  if (outfs == NULL) SER_BAD_OPEN_OUTPUT (outfile);
  ok = setvbuf (outfs, outbuff, _IOFBF, IO_BUFF_SIZE);
  if (ok != 0) FTL_INTERNAL (ERR_STUFFED_BUFFER);
  write_index (outfs, index, total);

  files = file_list;
  while (files != NULL)
  {
    thisfile = files->tokens->string;
    infs = fopen (thisfile, "rb"); /* this is now the explicit path */
    if (infs == NULL) SER_BAD_OPEN_INPUT (thisfile);
    if (information) printf ("Copying %s to %s\n", thisfile, outfile);
    fcopy (infs, outfs);
    fclose (infs);
    files = files->next;
  }
  fclose (outfs);
  if (information) printf ("Total: %d modules\n", modules);
}
/*}}}  */

/*{{{  int main (argc, argv)   */
int main (argc, argv)
int argc;
const char *argv[];
{
  arg_parse_result result;

  search_path = ENV_SEARCH_VAR;

  env_to_option (&argc, &argv, ENV_COMMAND_VAR);
  result = arg_parse (argc -1 , &(argv[1]), argd);
  if (result == arg_parse_error) SER_COMMAND_LINE ("");
  /* if (result == arg_parse_help) SKIP (); */
  if (result == arg_parse_ok)
  {
    if (file_list == NULL)
    {
      if (information) print_info;
      else if (debug)
      {
        fn_help ("");
        printf ("\n");
        printf ("   %s\t\tdebug\n", DEBUG);
        printf ("   %s\t\tno descriptors in index.\n", NO_DESC);
        printf ("   %s{env var}\tspecify search path.\n", DEBUG_PATH);
        printf ("\n  Options beginning with Z are unsupported and for INMOS' use only\n");
      }
      else fn_help ("");
    }
    else /* run the tool */
    {
      if (information) print_info;
      if (file_list == NULL) SER_NO_INPUT;
      if (*outfile == '\0') outfile = derive_string (file_list->tokens->string, FALSE, EXTENSION);
      if (*outfile == '\0') outfile = derive_string (argv[1], FALSE, EXTENSION);
      if (debug) print_command_structure (file_list);
      start_tag = 0L;
      mk_index (file_list);
    }
  }

  if (execute_many) exit_repeat (EXIT_SUCCESS);
  return (EXIT_SUCCESS);
}
/*}}}  */
