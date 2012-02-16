/*  $Id: com.c,v 1.3 1997/01/23 09:47:03 mdp2 Exp $    */

/*
 *	Command line parsing
 *	Copyright (C) 1994 Inmos Limited
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


/* Ade 31/1/94 - init local in 'result'  read_a_token() */
/* Ade 17/3/94 - use popen_relative() and not popen() */
/* Ade 24/6/94 : added CVS */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "toolkit.h"
#include "popen_re.h"

#define MAX_STRING     256
#define MAX_PATH       1000
#define COMMENT_CHAR   '-'
#define DASHES         "--------------------------------------------------------------------------------\n"

PRIVATE command_token *look_a_head;
PRIVATE char const *this_file, *this_prog, *this_path;
PRIVATE int line_number;

/*
  parse_token (fs):= TOKEN = NULL -> END OF NON EMPTY LINE
  parse_token (fs):= TOKEN->string = NULL -> END OF FILE
*/




/*{{{   built in errors  */
#define SER_NON_ASCII \
    error (ERR_SERIOUS, this_prog, this_file, "(%d)-bad format: non ASCII character in indirect file", line_number)

#define SER_TOO_LONG \
    error (ERR_SERIOUS, this_prog, this_file, "(%d)-bad format: excessively long line in indirect file", line_number)

#define SER_NO_FILE(LINE, INC) \
    error (ERR_SERIOUS, this_prog, this_file, "(%d)-bad format: file name missing after %s", LINE, INC)

#define SER_EXTRA_GARBAGE(FNAME, LINE, INC) \
    error (ERR_SERIOUS, this_prog, FNAME, "(%d)-bad format: only single parameter for %s", LINE, INC)

#define SER_COULD_NOT_OPEN(FNAME, LINE, FILE) \
    error (ERR_SERIOUS, this_prog, FNAME, "(%d)-could not open %s for reading", LINE, FILE)

#define SER_EMPTY_FILE(FNAME) \
    error (ERR_SERIOUS, this_prog, FNAME, "nothing of importance in file")

#define FATAL_ERR(NUMBER) \
    error (ERR_FATAL, this_prog, "", "internal command file error %d: please report", NUMBER)

#define ERR_NULL_TOK_1    1
#define ERR_NULL_TOK_2    2
#define ERR_NULL_COMMAND  3
/*}}}  */

/*{{{   PRIVATE command_token *read_a_token (fs)  */
PRIVATE command_token *read_a_token (fs)
FILE *fs;
{
  command_token *result = NULL; /* init. to avoid gcc warning */
  char string[256];
  int c, len, end_of_token, end_of_file, end_of_line;

  len = 0;
  end_of_token = FALSE;
  end_of_line = FALSE;
  end_of_file = FALSE;
  if (!feof (fs))
  {
    /*{{{   throw white space  */
    c = fgetc (fs); 
    while (isspace (c) && (c != '\n')) c = fgetc (fs);
    if (feof(fs))
    {
      (void) fgetc (fs); /* set feof */
      end_of_line = TRUE;
    }
    else if (c == '\n') end_of_line = TRUE;
    else ungetc (c, fs);

    /*}}}  */
    /*{{{   read token  */
    while (!(end_of_token || end_of_line || end_of_file))
    {
      c = fgetc (fs);
      string[len] = c;
      /*{{{   newline  */
      if (string[len] == '\n')
      {
        if (len == 0) end_of_line = TRUE;
        else 
        {
          end_of_token = TRUE;
          ungetc ('\n', fs); /* put newline back */
        }
        len--;
      }
      /*}}}  */
      /*{{{   end of file  */
      else if (feof(fs))
      {
        if (len == 0) end_of_line = TRUE;
        else end_of_token = TRUE;
        (void) fgetc (fs); /* set feof */
        len--;
      }
      /*}}}  */
      /*{{{   space  */
      else if (isspace (c))
      {
        end_of_token = TRUE;
        len--;
      }
      /*}}}  */
      /*{{{   comment  */
      else if ((len >= 1) && (string[len - 1] == COMMENT_CHAR) && 
                             (string[len] == COMMENT_CHAR))
      {
        c = fgetc (fs); 
        while ((c != '\n') && (!feof(fs))) c = fgetc (fs);
        if (len == 1) end_of_line = TRUE; /* line is only a comment */
        else 
        {
          end_of_token = TRUE;
          if (feof(fs)) (void) fgetc (fs); /* set feof */
          else ungetc ('\n', fs); /* put newline back */
        }
        len -= 2;
      }
      /*}}}  */
      else if (!isgraph (c)) SER_NON_ASCII; 
      len ++;
      if (len >= MAX_STRING) SER_TOO_LONG;
    }
    /*}}}  */
    string[len] = (char) '\0';
  }
  else end_of_file = TRUE;

  /*{{{   sort out result  */
  if (end_of_line) result = NULL;
  else if (end_of_token)
  {
    result = malloc_chk (sizeof (command_token));
    result->string = str_duplicate (string);
    result->next = NULL;
  }
  else if (end_of_file)
  {
    result = malloc_chk (sizeof (command_token));
    result->string = NULL;
    result->next = NULL;
  }
  /*}}}  */

  return (result);
}
/*}}}  */
/*{{{   PRIVATE command_token *parse_token (fs)  */
PRIVATE command_token *parse_token (fs)
FILE *fs;
{
  command_token *result;
  result = look_a_head; /* we know this isn't NULL when we initialise */
  if (look_a_head == NULL) /* END OF LINE, get next token */
  {
    while (look_a_head == NULL)
    {
      line_number ++;
      look_a_head = read_a_token (fs);
    }
  }

  else if (look_a_head->string != NULL) /* not eof,  get next token */
    look_a_head = read_a_token (fs);


  if ((result != NULL) && (result->string != NULL) &&
      (look_a_head != NULL) && (look_a_head->string == NULL))
  {
    free_chk (look_a_head); /* glue on an end of line if it's missing */
    look_a_head = NULL;
  }

  return (result);
}
/*}}}  */
/*{{{   PRIVATE command_line *parse_line (fs)  */
PRIVATE command_line *parse_line (fs)
FILE *fs;
{
  command_line *result;
  command_token *tok;
  tok = parse_token (fs);
  if (tok == NULL) FATAL_ERR (ERR_NULL_TOK_1);
  if (tok->string == NULL) /* end of file */
  {
    free_chk (tok);
    result = NULL;
  }
  else
  {
    result = malloc_chk (sizeof (command_line));
    result->tokens = tok;
    result->line_number = line_number;
    result->file_name = str_duplicate (this_file);
    result->next = NULL;
    while (tok != NULL)
    {
      tok->next = parse_token (fs);
      tok = tok->next;
    }
  }  
  return (result);
}
/*}}}  */
/*{{{   PRIVATE command_line *parse_file (filename, fullname)  */
PRIVATE command_line *parse_file (filename, fullname)
char const *filename;
char *fullname;
{
  command_line *result, *line;
  FILE *fs;

  line_number = 1;
  this_file = filename;
  fs = popen_relative (filename, this_path, NULL, "r", (char const * * const)&fullname, NULL, NULL, NULL);
  if (fs != NULL)
  {
    /*{{{   get look ahead token  */
    look_a_head = read_a_token (fs);
    while (look_a_head == NULL)
    {
      line_number ++;
      look_a_head = read_a_token (fs);
    }
    /*}}}  */

    result = parse_line (fs);
    line = result;
    while (line != NULL)
    {
      line->next = parse_line (fs);
      line = line->next;
    }
    if (result == NULL) SER_EMPTY_FILE (filename);
    fclose (fs);
  }
  else result = NULL;
  return (result); 
}
/*}}}  */
/*{{{   PRIVATE void free_tokens (tok, del_string)  */
PRIVATE void free_tokens (tok, del_string)
command_token *tok;
int del_string;
{
  command_token *tmp;
  while (tok != NULL)
  {
    tmp = tok->next;
    if (del_string) free_chk (tok->string);
    free_chk (tok);
    tok = tmp;
  }
}
/*}}}  */
/*{{{   PRIVATE void replace_commands (first, old)  */
PRIVATE void replace_commands (first, old)
command_line *old, *first; /* must be non NULL */
/* insert list 'first' where 'old' used to be */
{
  command_line *last;

  last = first;
  while (last->next != NULL) last = last->next;
  last->next = old->next;

  free_chk (old->file_name);
  old->file_name = first->file_name;
  old->line_number = first->line_number;
  free_tokens (old->tokens, TRUE);
  old->tokens = first->tokens;
  old->next = first->next;

  free_chk (first);
}
/*}}}  */
/*{{{   PRIVATE command_line strip_commands (com)  */
PRIVATE command_line *strip_commands (list)
command_line *list;
{
  command_line *tmp, *result;

  if (list == NULL) result = NULL;

  else if (list->tokens->string[0] == '#')
  {
    free_tokens (list->tokens, FALSE);
    tmp = list->next;
    free_chk (list);
    result = strip_commands (tmp);
  }

  else
  {
    list->next = strip_commands (list->next);
    result = list;
  }

  return (result);
}
/*}}}  */
/*{{{   PRIVATE command_line *include_file (filename, pathname, progname, include)  */
PRIVATE command_line *include_file (filename, pathname, progname, include)
char const *pathname, *filename, *progname, *include;
{
  char fullname[MAX_PATH];
  char const *fname;
  command_line *tmp, *nextfile, *result;
  command_token *tok;
  this_prog = progname;
  this_path = pathname;

  result = parse_file (filename, fullname);
  if ((result != NULL) && (include != NULL))
  {
    nextfile = result;
    while (nextfile != NULL)
    {
      nextfile = result;
       /* find #INCLUDE */
      while  ((nextfile != NULL) && 
              !((nextfile->tokens->string[0] == '#') &&
                strcmp_lc (include, &(nextfile->tokens->string[1]))))
                     nextfile = nextfile->next;
      if (nextfile != NULL) 
      {
        tok = nextfile->tokens->next;
        if (tok == NULL) SER_NO_FILE (nextfile->line_number, include);
        fname = tok->string;
        if (tok->next != NULL) SER_EXTRA_GARBAGE (nextfile->file_name, nextfile->line_number, include);
        tmp = nextfile;
        nextfile = parse_file (fname, fullname);
        if (nextfile == NULL) SER_COULD_NOT_OPEN(tmp->file_name, tmp->line_number, fname);
        replace_commands (nextfile, tmp);
      }      
    }
  }
  return (result); 
}
/*}}}  */
/*{{{   PRIVATE int command_parse(file_list, desc)  */
PRIVATE int command_parse(file_list, desc)
command_line *file_list;
command_descriptor const desc[];
{
  command_token *tok;
  command_descriptor const *dsc;
  char *command;
  command_control (*fn_end)(command_line *);
  command_control (*fn_token)(command_line *);
  command_control (*fn_error)(command_line *);
  int res;
  /*{{{   find end routine  */
  dsc = desc;
  while (dsc->com_type != com_end) dsc++;
  fn_end = dsc->com_fn;
  /*}}}  */
  /*{{{   find token routine  */
  dsc = desc;
  while ((dsc->com_type != com_end) &&
         (dsc->com_type != com_token)) dsc++;
  if (dsc->com_fn != NULL) fn_token = dsc->com_fn;
  else fn_token = NULL;
  /*}}}  */
  /*{{{   find error routine  */
  dsc = desc;
  while ((dsc->com_type != com_end) &&
         (dsc->com_type != com_error)) dsc++;
  if (dsc->com_fn != NULL) fn_error = dsc->com_fn;
  else fn_error = NULL;
  /*}}}  */

  if (file_list != NULL) res = com_continue;
  else res = com_terminate;

  while ((file_list != NULL) && (res == com_continue))
  {
    /*{{{   silly errors  */
    tok = file_list->tokens;
    if (tok == NULL) FATAL_ERR (ERR_NULL_TOK_2);
    command = tok->string;
    if (command == NULL) FATAL_ERR (ERR_NULL_COMMAND);
    /*}}}  */
    if (tok->string[0] == '#')
    {
      /*{{{   its a command  */
      dsc = desc;
      command = &(tok->string[1]);
      while (!((dsc->com_type == com_end) ||
               ((dsc->com_type == com_line) &&
                (strcmp_lc (dsc->command, command))))) dsc++;
        if (dsc->com_type != com_end) 
        {
        if (dsc->com_fn != NULL) 
          {
            tok = file_list->tokens;
            file_list->tokens = file_list->tokens->next;
            res = (*dsc->com_fn) (file_list);
            file_list->tokens = tok;
          }
        }
        else if (fn_error != NULL) (*fn_error) (file_list);
      /*}}}  */
    }
    else if (fn_token != NULL) res = (*fn_token) (file_list);
    file_list = file_list->next;
  }

  if ((res == com_continue) && (fn_end != NULL)) res = (*fn_end) (NULL);

  if (res == com_continue)  return (TRUE);
  else return (FALSE);
}
/*}}}  */

/*{{{   PUBLIC void print_command_structure (commands)  */
PUBLIC void print_command_structure (commands)
command_line const *commands;
{
  command_token *tok;
  while (commands != NULL)
  {
    printf ("%-25s %-4d ", commands->file_name, commands->line_number);
    tok = commands->tokens;
    while (tok != NULL)
    {
      printf ("'%s' ", tok->string);
      tok = tok->next;
    }
    printf ("\n");
    commands = commands->next;
  }
  printf (DASHES);
}
/*}}}  */
/*{{{   PUBLIC command_line *mk_command (fname)  */
PUBLIC command_line *mk_command (fname)
char const *fname;
{
  command_line *result;
  result = malloc_chk (sizeof (command_line));
  result->line_number = 0;
  result->file_name = str_duplicate ("");
  result->next = NULL;
  result->tokens = malloc_chk (sizeof (command_token));
  result->tokens->next = NULL;
  result->tokens->string = str_duplicate (fname);
  return (result);
}
/*}}}  */
/*{{{   PUBLIC command_line *add_command (command, list)  */
PUBLIC command_line *add_command (command, list)
command_line *command, *list;
{
  command_line *result;
  if (list == NULL) result = command;
  else 
  {
    result = list;
    while (list->next != NULL) list = list->next;
    list->next = command;
  }
  return (result);
}
/*}}}  */
/*{{{   PUBLIC command_line *read_indirect_file (fname, search_path, progname, include, desc, ok)  */
PUBLIC command_line *read_indirect_file (fname, search_path, progname, include, desc, ok)
char const *fname, *search_path, *progname, *include;
command_descriptor const desc[];
int *ok;
{
  command_line *new_file, *tmp;

  new_file = include_file (fname, search_path, progname, include);
  *ok = command_parse (new_file, desc);
  if (*ok)
  { /* all ok */
    new_file = strip_commands (new_file);
    return (new_file);
  }
  else
  { /* failed */
    while (new_file != NULL)
    {
      free_chk (new_file->file_name);
      free_tokens (new_file->tokens, TRUE);
      tmp = new_file->next;
      free_chk (new_file);
      new_file = tmp;
    }
    return (NULL);
  }
}
/*}}}  */

