/* $Id: arg.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	command line parsing
 *	Copyright (C) 1989, 1990, 1994 Inmos Limited
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
/* ahp: Converted for CMS 19910823 */
/* ahp: Removed host dependency for compiler problems - 930311 */
/* ahp: separating out the access to the host switch character - 930311 */
/* ahp: enables strings and indirect files to be parsed - 930426 */
/* bas: Fixed bug numbers INSdi02094, INSdi02099, INSdi02235 - 940726 */
/* bas: Added malloc checking as per INSdi03133 */

/*{{{  Includes */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include "arg.h"
#include "arg0.h"

#include <ctype.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
/*}}}*/
/*{{{  Prototypes */
static arg_control do_argument (
  int const,
  char const * [],
  arg_descriptor const [],
  int *
);

static arg_control do_read_file (char const *, arg_descriptor const []);
static arg_control do_token (char const *, arg_descriptor const []);
static arg_control do_help (arg_descriptor const []);
static arg_control do_end (arg_descriptor const []);
static arg_control do_error (char const * const, arg_descriptor const []);
static arg_control do_mem_error (arg_descriptor const []);
static arg_control do_syntax (char const * const, arg_descriptor const []);
static arg_control do_env_var (char const * const, arg_descriptor const []);
static arg_control do_file_err (char const * const, arg_descriptor const []);

static arg_control parse_string (
  char const * const,
  int * const,
  char const *** const,
  arg_descriptor const []
);

static arg_control get_line (
  FILE * const,
  char const * const,
  arg_descriptor const [],
  char * * const
);

static arg_parse_result arg_sub_parse (
  int const,
  char const * [],
  arg_descriptor const []
);
/*}}}*/
/*{{{  Functions */
/*{{{  arg_parse */
/* bas - 940726 - moved most of this function out into arg_sub_parse */
/* in order to stop the arg_end function being called at the end of  */
/* each line of an indirect file.                                    */

arg_parse_result arg_parse
  (
    int const argc,           /* number of tokens on command line     */
    char const * argv[],      /* token strings                        */
    arg_descriptor const argd[] /* description of expected arguments  */
  )
{
  arg_parse_result ret;

  ret = arg_sub_parse (argc, argv, argd);

  if (ret != arg_parse_ok) return (ret);
                                 /* If we've got an error, return */

  if (argc != 0)             /* else if there were tokens are in the command */
  {
    switch (do_end (argd))          /* do the normal exit */
    {
      case arg_continue:
        return (arg_parse_ok);
      case arg_terminate:
        return (arg_parse_error);
    }
  }

  return (ret);
}
/*}}}*/
/*{{{  arg_sub_parse */
static arg_parse_result arg_sub_parse
  (
    int const argc,           /* number of tokens on command line     */
    char const * argv[],      /* token strings                        */
    arg_descriptor const argd[] /* description of expected arguments  */
  )
{
  char const * token;
  char prefix_character;
  char alt_prefix_character;
  int tk_index;

  prefix_character = arg_host_switch_char ();
  alt_prefix_character = arg_host_alternate_switch_char ();
  if (argc == 0)                    /* if no tokens are in the command  */
  {                                 /* then help is requested.          */
    do_help (argd);
    return (arg_parse_help);        /* tell user that it was help       */
  }
  else                              /* there are some tokens to look at */
  {
    tk_index = 0;
    while (tk_index < argc)         /* examine each token in turn       */
    {
      token = argv[tk_index];
      if ((token[0] == prefix_character) || /* look for argument in user list*/
          (token[0] == alt_prefix_character)) {
        if (do_argument (argc, argv, argd, &tk_index) ==
                         arg_terminate) {
          return (arg_parse_error);
				}
      } else if ((token[0] == FILE_MARKER) && (token[1] != NUL)) {
        if (do_read_file (&(token[1]), argd) == arg_terminate) {
          return (arg_parse_error);
        }
      } else {                      /* no prefix switch character       */
        if (do_token (token, argd) == arg_terminate) {
          return (arg_parse_error);
        }
      }
      tk_index += 1;                /* now look at next token on line   */
    }
  }
  return (arg_parse_ok);
}
/*}}}*/
/*{{{  arg_sub_string_parse */
static arg_parse_result arg_sub_string_parse
  (
    char const * str,            /* string to parse                      */
    arg_descriptor const argd[], /* description of expected arguments    */
    int * argcount               /* argument count returned by reference */
  )
{
  int argc, i;
  arg_parse_result res;
  arg_control ctl;
  char const * * argv;

  argc = 0;
  res = arg_parse_ok;
  ctl = arg_continue;
  argv = NULL;
  if (str != NULL)
    ctl = parse_string (str, &argc, &argv, argd);
  if (ctl == arg_terminate)
    res = arg_parse_error;
  else if (argc != 0)
    res = arg_sub_parse (argc, argv, argd);
  if (argc > 0)
  {
    for (i = 0; i < argc; ++i)
      if (argv[i] != NULL)
        free ((void *)argv[i]);
  }
  *argcount = argc;
  return (res);
}
/*}}}*/
/*{{{  arg_string_parse */
arg_parse_result arg_string_parse
  (
    char const * str,           /* string to parse                      */
    arg_descriptor const argd[] /* description of expected arguments    */
  )
{
  int discarded_value;  

  return arg_sub_string_parse(str, argd, &discarded_value);
}
/*}}}*/
/*{{{  arg_env_var_parse */
arg_parse_result arg_env_var_parse
  (
    char const * env,             /* name of environment variable         */
    arg_descriptor const argd[]   /* description of expected arguments    */
  )
{
  arg_parse_result res;
  char const * env_str;
  int argc = 0;

  res = arg_parse_ok;
  if (env != NULL)
  {
    env_str = getenv (env);
    if (env_str == NULL)
    {
      if (do_env_var (env, argd) == arg_terminate)
        res = arg_parse_error;
    }
    else
      res = arg_sub_string_parse (env_str, argd, &argc);
  }

  if (res != arg_parse_ok) return (res);
                                 /* If we've got an error, return */

  if (argc != 0)                 /* else if there were tokens are in the command */
  {
    switch (do_end (argd))          /* do the normal exit */
    {
      case arg_continue:
        return (arg_parse_ok);
      case arg_terminate:
        return (arg_parse_error);
    }
  }

  return (res);
}
/*}}}*/

/*{{{  scan_token */
/* This function scans a string and picks off the first token */

static arg_parse_result scan_token (char const * const st,
                                    char const * * const en,
                                    arg_descriptor const argd[]
                                   )
{
  arg_parse_result res;
  char * new;

  res = arg_continue;
  if (*st == QUOTE)
  {
    for (*en = st+1; **en != NUL; ++*en)
    {
      if (**en == QUOTE)
      {
        *en += 1;
        if (**en != QUOTE)
        {
          if (isspace (**en) || (**en == NUL))
            return (res);
          *en = NULL;     /* indicate to caller token is in error */
          new = arg2_new_string (st);
          if (new == NULL) {
						(void) do_mem_error (argd);
						return (arg_parse_error);
					}
          return (do_syntax ((char const *) new, argd));
        }
      }
    }
    if (**en == NUL)
    {
      *en = NULL;
      new = arg2_new_string (st);
      if (new == NULL) {
				(void) do_mem_error (argd);
				return (arg_parse_error);
			}
      return (do_syntax ((char const *) new, argd));
    }
  }
  else
  {
    for (*en = st; ! isspace (**en) && (**en != NUL); ++*en)
      ;
  }
  return (res);
}
/*}}}*/
/*{{{  parse_string */
/* This function strips the first token off a string and then recurses  */
/* to find all the other tokens. These are built up into an array of    */
/* char pointers. The return value indicates whether to continue the    */
/* parsing process.                                                     */
/* Double quotes delimit a single tokens. Two adjacent double quotes    */
/* within a string represent a single one.                              */

static arg_control parse_string (char const * const str,
                                      int * const argc,
                                      char const * * * const argv,
                                      arg_descriptor const argd[]
                                     )
{
  arg_parse_result res;
  int n;
  char const * st, * en;

  res = arg_continue;
  n = *argc;                        /* index into table for this one    */
  if (str != NULL)
    for (st = str; isspace (*st); ++st)
      ;
  else
    st = NULL;
  if ((st == NULL) || (*st == NUL))
  {
    if (n != 0)
    {
      *argv = (char const * *) malloc (n * sizeof(char const *));
      if (*argv == NULL) return (do_mem_error (argd));
    }
  }
  else
  {
    res = scan_token (st, &en, argd);
    if (res == arg_terminate)
      *argc = 0;
    else
    {
      *argc += 1;
      res = parse_string (en, argc, argv, argd);
      if (res != arg_terminate) {
        (*argv)[n] = arg2_copy_token (st, en);
        if ((*argv)[n] == NULL) return (do_mem_error (argd));
      }
      else if (*argv != NULL)
        (*argv)[n] = NULL;
    }
  }
  return (res);
}
/*}}}*/
/*{{{  do_argument */
/* This function checks out an argument that starts with the prefix     */
/* character. It searches the descriptor table to find the matching     */
/* string and calls the processing routine to perform the job.          */

static arg_control do_argument (int const argc, char const * argv[],
          arg_descriptor const argd[], int *tk_index)
{
  arg_descriptor const * desc;      /* desc. currently examined         */
  char const * token;
  char * new;

  desc = argd;
  token = argv[*tk_index];          /* token in command line            */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_single:              /* stand alone -- see if same       */
      {
        switch (arg2_compare (desc->arg_string, token+sizeof(char)))
        {
          case ident:
            return ((*(desc->arg_fn))(NULL)); /* process and return     */

          case prefix:
            return (do_error (token, argd)); /* get user to do error    */

          case noteq:
            break;                  /* no action to take                */
        }
        break;
      }

      case arg_operand:             /* argument needs operand           */
      {
        switch (arg2_compare (desc->arg_string, token+sizeof(char)))
        {
          case ident:               /* operand in next token (if one)   */
          {
            *tk_index += 1;
            if (*tk_index >= argc)
              return (do_error (token, argd)); /* last on command line  */
            else
            {
              token = argv[*tk_index]; /* user processing and return    */
              new = arg2_new_string (token);
              if (new == NULL) return (do_mem_error (argd));
              return ((*(desc->arg_fn)) (new));
            }
          }

          case prefix:              /* followed in same token           */
            new = arg2_new_string (token + sizeof(char) + strlen(desc->arg_string));
            if (new == NULL) return (do_mem_error (argd));
            return ((*(desc->arg_fn)) (new)); /* user processes and return */

          case noteq:
            break;                  /* nothing doing this time          */
        }
        break;
      }

      case arg_end:                 /* no match found in user's list    */
        return (do_error (token, argd));

      case arg_token:
      case arg_help:
      case arg_error:
        break;                      /* not relevant this time round     */
    }
    desc += 1;
  }
  return (arg_continue);
}
/*}}}*/
/*{{{  do_read_file */
/* This function reads an indirect file and parses each line in turn     */

static arg_control do_read_file (char const * fn, arg_descriptor const argd[])
{
  FILE * f;
  char * line;
  arg_control res;

  f = fopen (fn, "r");
  if (f == NULL)
    res = do_file_err (fn, argd);
  else
  {
    res = get_line (f, fn, argd, &line);
    while ((res == arg_continue) && (line != NULL))
    {
      if (line[0] != COMMENT)
      {
        if (arg_string_parse (line, argd) == arg_parse_error)
        {
          res = arg_terminate;
          break;
        }
      }
      free (line);
      res = get_line (f, fn, argd, &line);
    }
    fclose (f);
  }
  return (res);
}
/*}}}*/
/*{{{  do_token */
/* This function processes a token which does not start with a prefix */
/* character. It searches the descriptor table for the relevant       */
/* function and invokes it.                                           */

static arg_control do_token (char const * token, arg_descriptor const argd[])
{
  arg_descriptor const * desc;
  char * new;

  desc = argd;                      /* descriptor currently examined    */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_single:
      case arg_operand:
      case arg_help:
      case arg_error:
        break;                      /* nothing relevant this time       */

      case arg_token:
        new = arg2_new_string (token);
        if (new == NULL) return (do_mem_error (argd));
        return ((*(desc->arg_fn)) (new));            /* process by user */

      case arg_end:
        return (do_error (token, argd)); /* could not find argument     */

    }
    desc += 1;
  }
}
/*}}}*/
/*{{{  do_help */
/* This function invokes the user's help function and returns what    */
/* the user says should be returned.                                  */

static arg_control do_help (arg_descriptor const argd[])
{
  arg_descriptor const * desc;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_single:
      case arg_operand:
      case arg_token:
      case arg_error:
        break;                      /* not relevant                     */

      case arg_help:
        return ((*(desc->arg_fn))(NULL)); /* process and return         */

      case arg_end:
        return (arg_continue);      /* none found - ignore it           */

    }
    desc += 1;
  }
}
/*}}}*/
/*{{{  do_end */
/* This function invokes the user's end function, if it exists.       */
/* It searches the descriptor table to find the last entry and        */
/* invokes it if it not null.                                         */

static arg_control do_end (arg_descriptor const argd[])
{
  arg_descriptor const * desc;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_single:
      case arg_operand:
      case arg_token:
      case arg_error:
      case arg_help:
        break;                      /* not relevant                     */

      case arg_end:
        if (desc->arg_fn != NULL)   /* process final call (if one)      */
          return ((*(desc->arg_fn))(NULL));
        else
          return (arg_continue);

    }
    desc += 1;
  }
}
/*}}}*/
/*{{{  do_error */
/* This function invokes the user's error function, if it exists.     */
/* It searches the descriptor table to find the error entry; if it    */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control do_error (char const * const token,
                            arg_descriptor const argd[])
{
  arg_descriptor const * desc;
  char *new;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_error:
        if (desc->arg_fn != NULL) { /* user processing                  */
          new = arg2_new_string (token);
          if (new == NULL) return (do_mem_error (argd));
          return ((*(desc->arg_fn)) (new));
        } else
          return (arg_terminate);   /* none given - take default action */

      case arg_end:
        return (arg_terminate);     /* none specified -- default action */

      default:
        break;
    }
    desc += 1;
  }
}
/*}}}*/
/*{{{  do_mem_error */
/* This function invokes the user's memory allocation error function, */
/* if it exists.  It searches the descriptor table to find the        */
/* arg_mem_err entry; if it does not exist or is NULL, then           */
/* arg_terminate is returned instead.                                 */

static arg_control do_mem_error (arg_descriptor const argd[])
{
  arg_descriptor const * desc;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_mem_err:
        if (desc->arg_fn != NULL)   /* user processing                  */
          return ((*(desc->arg_fn))(NULL));
        else
          return (arg_terminate);   /* none given - take default action */

      case arg_end:
        return (arg_terminate);     /* none specified -- default action */

      default:
        break;

    }
    desc += 1;
  }
}
/*}}}*/
/*{{{  do_syntax */
/* This function invokes the user's syntax error function, if it exists.*/
/* It searches the descriptor table to find the error entry; if it    */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control do_syntax (char const * const token,
                                    arg_descriptor const argd[])
{
  arg_descriptor const * desc;
  char * new;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_syntax:
        if (desc->arg_fn != NULL) { /* user processing                  */
          new = arg2_new_string (token);
          if (new == NULL) return (do_mem_error (argd));
          return ((*(desc->arg_fn)) (new));
        } else
          return (arg_terminate);   /* none given - take default action */

      case arg_end:
        return (arg_terminate);     /* none specified -- default action */

      default:
        break;

    }
    desc += 1;
  }
}
/*}}}*/
/*{{{  do_env_var */
/* This function invokes the user's environment variable error        */
/* function, if it exists.                                            */
/* It searches the descriptor table to find the error entry; if it    */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control do_env_var (char const * const token,
                              arg_descriptor const argd[])
{
  arg_descriptor const * desc;
  char * new;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_env_var:
        new = arg2_new_string (token);
        if (new == NULL) return (do_mem_error (argd));
        if (desc->arg_fn != NULL)   /* user processing                  */
          return ((*(desc->arg_fn)) (new));
        else
          return (arg_terminate);   /* none given - take default action */

      case arg_end:
        return (arg_terminate);     /* none specified -- default action */

      default:
        break;
    }
    desc += 1;
  }
}
/*}}}*/
/*{{{  do_file_err */
/* This function invokes the user's file reading error                */
/* function, if it exists.                                            */
/* It searches the descriptor table to find the error entry; if it    */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control do_file_err (char const * const token,
                                arg_descriptor const argd[])
{
  arg_descriptor const * desc;
  char * new;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_file_err:
        if (desc->arg_fn != NULL) { /* user processing                  */
          new = arg2_new_string (token);
          if (new == NULL) return (do_mem_error (argd));
          return ((*(desc->arg_fn)) (new));
        } else
          return (arg_terminate);   /* none given - take default action */

      case arg_end:
        return (arg_terminate);     /* none specified -- default action */

      default:
        break;
    }
    desc += 1;
  }
}
/*}}}*/
/*{{{  get_line */
/* This function obtains a line from a file. It will be terminated by a */
/* a newline character and then a null character. It will ensure that   */
/* a new area is obtained for each line. The caller must free this area */
/* after is used. If there is nothing left in the file, NULL it         */
/* returns NULL.                                                        */

#define step 1000

static arg_control get_line (FILE * const f, char const * const fn,
                             arg_descriptor const argd[],
                             char * * const line)
{
  char * buf, * part;
  size_t buflen, len;
  arg_control res;

  res = arg_continue;
  buf = NULL;
  buflen = 0;
  while (! feof (f))
  {
    buflen += step;
    part = buf;
    buf = (char *) malloc (buflen * sizeof(char));
    if (buf == NULL) return (do_mem_error (argd));
    if (part != NULL)
    {
      strcpy (buf, part);
      free (part);
      part = buf + strlen (buf);
    }
    else
      part = buf;
    part[0] = NUL;
    part = fgets (part, step, f);
    if (feof (f))
    {
      if (part == NULL)
      {
/*      free (part);     -- This should not be here -- BAS 22/07/94 */
        buf = NULL;
        break;
      }
    }
    if (ferror (f))
    {
      res = do_file_err (fn, argd);
      free (buf);
      buf = NULL;
      break;
    }
    if (part != NULL)
    {
      len = strlen (part);
      if ((len > 0) && (part[len-1] == NL))
        break;
    }
  }
  *line = buf;
  return (res);
}
/*}}}*/
/*}}}*/
