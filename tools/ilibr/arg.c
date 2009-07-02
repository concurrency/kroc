/*
 *	user command line parsing
 *	Copyright (C) 1989, 1990 Inmos Limited
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

/* arg.c -- Implement the user command line argument parsing */
/* Copyright Inmos 1989, 1990 */
/* CMSIDENTIFIER */
/* ARG_AHP:ARG_C@000.S4-FILE;0(26-APR-93)[UNDER-DEVELOPMENT] */
/* Version 1.0 -- 19900129 */
/* History */
/* ahp: First version written */
/* ahp: Converted for CMS 19910823 */
/* ahp: Removed host dependency for compiler problems - 930311 */
/* ahp: separating out the access to the host switch character - 930311 */
/* ahp: enables strings and indirect files to be parsed - 930426 */

#include "arg.h"

#ifdef HAVE_CONFIG_H
#include "config.h"	/* will defined HOST_OS_IS_UNIX or similar */
#endif

#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#ifndef COMPONENT_ID
/* CMSIDENTIFIER */
#define COMPONENT_ID "[UNDER-DEVELOPMENT]"
#endif

#include <stdlib.h>
#include <string.h>

static char host_switch_char (void)
{
  char prefix_character;

#if defined(HOST_OS_IS_MSDOS) || defined(HOST_OS_IS_VMS)
  prefix_character = '/';
#elif defined(HOST_OS_IS_HELIOS) || defined(HOST_OS_IS_UNIX)		/* SUN_OS evaporated */
  prefix_character = '-';
#elif defined(HOST_OS_IS_SERVER)
  {
    int host, os, board;

    host_info (&host, &os, &board);
    switch (os)
    {
      case _IMS_OS_DOS:
      case _IMS_OS_VMS:
      {
        prefix_character = '/';
        break;
      }

      case _IMS_OS_HELIOS:
      case _IMS_OS_SUNOS:
      {
        prefix_character = '-';
        break;
      }
    }
  }
#else
#include <assert.h>
  assert(0);
#endif

  return (prefix_character);
}

#define NUL         '\0'
#define QUOTE       '"'
#define FILE_MARKER '@'
#define COMMENT     '#'
#define nl          '\n'

/* To prevent problems with some implementations that do not behave like    */
/* as ANSI defines.   */
static int to_lower (int c)
{
  if ((c >= 'A') && (c <= 'Z'))
    return (tolower(c));
  return (c);
}

typedef enum {false = 0, true = 1} bool;

typedef enum {ident = 0, prefix = 1, noteq = 2} str_compare;

static arg_control do_argument (int const, char const * [],
          arg_descriptor const [], int *);
static arg_control do_read_file (char const *, arg_descriptor const []);
static arg_control do_token (char const *, arg_descriptor const []);
static arg_control do_help (arg_descriptor const []);
static arg_control do_end (arg_descriptor const []);
static arg_control do_error (char const * const, arg_descriptor const []);
static arg_control do_syntax (char const * const, arg_descriptor const []);
static arg_control do_env_var (char const * const, arg_descriptor const []);
static arg_control do_file_err (char const * const, arg_descriptor const []);

static arg_control parse_string (char const * const,
                                 int * const,
                                 char const * * * const,
                                 arg_descriptor const []
                                );

static str_compare compare (char const * const, char const * const);
static char * new_string (char const * const);
static arg_control get_line (FILE * const, char const * const,
                             arg_descriptor const [], char * * const);

arg_parse_result arg_parse
  (
    int const argc,           /* number of tokens on command line     */
    char const * argv[],      /* token strings                        */
    arg_descriptor const argd[] /* description of expected arguments  */
  )
{
  char const * token;
  char prefix_character;
  int tk_index;

  prefix_character = host_switch_char ();
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
      if (token[0] == prefix_character) /* look for argument in user list*/
        if (do_argument (argc, argv, argd, &tk_index) ==
                         arg_terminate)
          return (arg_parse_error);
        else
          ;
      else if ((token[0] == FILE_MARKER) && (token[1] != NUL))
        if (do_read_file (&(token[1]), argd) == arg_terminate)
          return (arg_parse_error);
        else
          ;
      else                          /* no prefix switch character       */
        if (do_token (token, argd) == arg_terminate)
          return (arg_parse_error);
        else
          ;
      tk_index += 1;                /* now look at next token on line   */
    }
    switch (do_end (argd))          /* normal exit                      */
    {
      case arg_continue:
        return (arg_parse_ok);
      case arg_terminate:
        return (arg_parse_error);
    }
  }
  return (arg_parse_ok);
}

arg_parse_result arg_string_parse
  (
    char const * str,           /* string to parse                      */
    arg_descriptor const argd[] /* description of expected arguments    */
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
    res = arg_parse (argc, argv, argd);
  if (argc > 0)
  {
    for (i = 0; i < argc; ++i)
      if (argv[i] != NULL)
        free ((void *)argv[i]);
  }
  return (res);
}

arg_parse_result arg_env_var_parse
  (
    char const * env,             /* name of environment variable         */
    arg_descriptor const argd[]   /* description of expected arguments    */
  )
{
  arg_parse_result res;
  char const * env_str;

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
      res = arg_string_parse (env_str, argd);
  }
  return (res);
}

/* This function scans a string and picks off the first token */
static arg_parse_result scan_token (char const * const st,
                                    char const * * const en,
                                    arg_descriptor const argd[]
                                   )
{
  arg_parse_result res;

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
          return (do_syntax ((char const *) new_string (st), argd));
        }
      }
    }
    if (**en == NUL)
    {
      *en = NULL;
      return (do_syntax ((char const *) new_string (st), argd));
    }
  }
  else
  {
    for (*en = st; ! isspace (**en) && (**en != NUL); ++*en)
      ;
  }
  return (res);
}

/* This function copies a token from a string into allocated storage  */
/* If the token starts with a quote, then the token is a string -     */
/* quotes are stripped off and doubled quotes are reduced to single.  */
/* If any errors were found, we might still have to copy it.          */
static char const * copy_token (char const * const st, char const * const en)
{
  char * buf, * dst;
  char const * src;

  if (en == NULL)
  {
    buf = (char *) malloc (sizeof(char));
    dst = buf;
  }
  else
  {
    buf = (char *) malloc ((en - st + 1) * sizeof(char));
    dst = buf;
    for (src = (*st == QUOTE ? st+1 : st); src < en; ++src)
    {
      if ((*src == QUOTE) && (*st == QUOTE))
      {
        src += 1;
        if (src == en)
          break;
      }
      *dst = *src;
      dst += 1;
    }
  }
  *dst = NUL;
  return ((char const *)buf);
}    

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
      if (res != arg_terminate)
        (*argv)[n] = copy_token (st, en);
      else if (*argv != NULL)
        (*argv)[n] = NULL;
    }
  }
  return (res);
}

/* This function checks out an argument that start with the prefix      */
/* character. It searches the descriptor table to find the matching     */
/* string and calls the processing routine to perform the job.          */

static arg_control do_argument (int const argc, char const * argv[],
          arg_descriptor const argd[], int *tk_index)
{
  arg_descriptor const * desc;      /* desc. currently examined         */
  char const * token;

  desc = argd;
  token = argv[*tk_index];          /* token in command line            */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_single:              /* stand alone -- see if same       */
      {
        switch (compare (desc->arg_string, token+sizeof(char)))
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
        switch (compare (desc->arg_string, token+sizeof(char)))
        {
          case ident:               /* operand in next token (if one)   */
          {
            *tk_index += 1;
            if (*tk_index >= argc)
              return (do_error (token, argd)); /* last on command line  */
            else
            {
              token = argv[*tk_index]; /* user processing and return    */
              return ((*(desc->arg_fn))(new_string(token)));
            }
          }

          case prefix:              /* followed in same token           */
            return ((*(desc->arg_fn)) /* user processes and return      */
                (new_string(token + sizeof(char) + strlen(desc->arg_string))));

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

/* This functionreads an indirect file and parses each line in turn     */
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

/* This function processes a token which does not start with a prefix */
/* character. It searches the descriptor table for the relevant       */
/* function and invokes it.                                           */

static arg_control do_token (char const * token, arg_descriptor const argd[])
{
  arg_descriptor const * desc;

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
        return ((*(desc->arg_fn))(new_string(token))); /* process by user*/

      case arg_end:
        return (do_error (token, argd)); /* could not find argument     */

    }
    desc += 1;
  }
}

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

/* This function invokes the user's error function, if it exists.     */
/* It searches the descriptor table to find the error entry; if it    */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control do_error (char const * const token,
                            arg_descriptor const argd[])
{
  arg_descriptor const * desc;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_error:
        if (desc->arg_fn != NULL)   /* user processing                  */
          return ((*(desc->arg_fn))(new_string(token)));
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

/* This function invokes the user's syntax error function, if it exists.*/
/* It searches the descriptor table to find the error entry; if it    */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control do_syntax (char const * const token,
                                    arg_descriptor const argd[])
{
  arg_descriptor const * desc;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_syntax:
        if (desc->arg_fn != NULL)   /* user processing                  */
          return ((*(desc->arg_fn))(new_string(token)));
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

/* This function invokes the user's environment variable error        */
/* function, if it exists.                                            */
/* It searches the descriptor table to find the error entry; if it    */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control do_env_var (char const * const token,
                              arg_descriptor const argd[])
{
  arg_descriptor const * desc;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_env_var:
        if (desc->arg_fn != NULL)   /* user processing                  */
          return ((*(desc->arg_fn))(new_string(token)));
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

/* This function invokes the user's file reading error                */
/* function, if it exists.                                            */
/* It searches the descriptor table to find the error entry; if it    */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control do_file_err (char const * const token,
                                arg_descriptor const argd[])
{
  arg_descriptor const * desc;

  desc = argd;                      /* current descriptor               */
  while (true)
  {
    switch (desc->arg_type)
    {
      case arg_file_err:
        if (desc->arg_fn != NULL)   /* user processing                  */
          return ((*(desc->arg_fn))(new_string(token)));
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

/* This function compares two strings. If they are identical, then    */
/* the result is 'ident'. If the first is a prefix substring of the   */
/* second, then the result is 'prefix'. Otherwise, the result is      */
/* 'noteq'.                                                           */

static str_compare compare (char const * const sub, char const * const str)
{
  char const * sub_ch, * str_ch;

  sub_ch = sub;                     /* expected substring               */
  str_ch = str;                     /* check start of this string       */
  while (true)
  {
    if (*sub_ch == NUL)             /* end of substring                 */
      if (*str_ch == NUL)           /* also end of the string           */
        return (ident);             /* they are identical               */
      else
        return (prefix);            /* substring is prefix of string    */
    else
      ;

    if (to_lower((int)*sub_ch) != to_lower((int)*str_ch))
      return (noteq);               /* difference found                 */

    sub_ch += 1;                    /* keep searching                   */
    str_ch += 1;
  }
}

/* This function creates a new string to be passed to the user for      */
/* processing. This ensures that the original command line tokens are   */
/* not changed by the parser -- they may of course be clobbered by      */
/* the user directly.                                                   */

static char * new_string (char const * const old_str)
{
  char * new_str;

  new_str = (char *) malloc (sizeof(char) * (strlen(old_str)+1));
  strcpy (new_str, old_str);
  return (new_str);
}

/* This function returns the configuration version string of the        */
/* library as built.                                                    */

static char const * const cms_version = COMPONENT_ID;

char const * arg_version (void)
{
  return (cms_version);
}


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
        free (part);
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
      if ((len > 0) && (part[len-1] == nl))
        break;
    }
  }
  *line = buf;
  return (res);
}

