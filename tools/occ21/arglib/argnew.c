/* $Id: argnew.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	new argument parsing functions
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

/* bas: First version written 940809 */

/*{{{  Includes */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include "arg.h"
#include "arg0.h"
#include <imsmisc.h>

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
/*}}}  */
/*{{{  Prototypes */
static arg_parse_result arg2_parse_tokens (
		int version_number,
		int const argc,
		char const *argv[],
		arg2_descriptor const argd[],  
		int do_help_if_no_args);

static arg2_options arg2_option_type (
		const char *str,
		arg2_descriptor const argd[]);

static arg_control arg2_do_help (arg2_descriptor const argd[]);

static arg_control arg2_do_end (arg2_descriptor const argd[]);

static arg_control arg2_do_error (
		char const *token,
		arg2_descriptor const argd[]);

static arg_control arg2_do_syntax (
		char const *token,
		arg2_descriptor const argd[]);

static arg_control arg2_do_env_var (
		char const *token,
		arg2_descriptor const argd[]);

static arg_control arg2_do_file_err (
		char const *token,
		arg2_descriptor const argd[]);

static arg_control arg2_do_mem_error (arg2_descriptor const argd[]);

static arg_control arg2_do_operand (
		const char *token,
		const char *next_token,
		arg2_descriptor const argd[],
		int *token_num);

static arg_control arg2_do_single (
		const char *token,
		const char *next_token,
		arg2_descriptor const argd[],
		int *token_num);

static arg_control arg2_do_both (
		const char *token,
		const char *next_token,
		arg2_descriptor const argd[],
		int *token_num);

static arg_control arg2_do_token (
		const char *token,
		const char *next_token,
		arg2_descriptor const argd[],
		int *token_num);

static arg_parse_result arg2_scan_token (
		char const *const st,
		char const **const en,
		arg2_descriptor const argd[]);

static arg_control arg2_parse_string (
		char const *const str,
		int *const argc,
		char const ***const argv,
		arg2_descriptor const argd[]);

static arg_control arg2_get_line (
		FILE * const f, char const * const fn,
		arg2_descriptor const argd[],
		char ** const line);

static arg_control arg2_read_file (
		int version_number,
		char const *fn,
		arg2_descriptor const argd[],
		int *token_num);

static void arg2_do_help_page (
		arg2_help_page_info *info,
		arg2_help_page_type type,
		arg2_descriptor const argd[]);

static void arg2_do_info_line (arg2_help_page_info *info, char *buffer);
/*}}}  */
/*{{{  Functions */
/*{{{  arg2_parse */
arg_parse_result arg2_parse (
		int version_number,                                                                     /* The version of arglib to use */
		int const argc,                                                                                 /* Number of tokens on command line */
		char const *argv[],                                                                     /* Token strings */
		arg2_descriptor const argd[]) {                 /* Description of expected arguments */

	arg_parse_result ret;

	assert (version_number == 1);

	ret = arg2_parse_tokens (version_number, argc, argv, argd, 1);

	if (ret != arg_parse_ok) return (ret);
																				/* If we received an error, return */

	if (argc != 0) {                                                                                        /* If there were tokens on the CL */
		switch (arg2_do_end (argd)) {                           /* Run the user end-of-parse routine */
			case arg_continue:                                                              /* If success returned, indicate so */
				return (arg_parse_ok);
			case arg_terminate:                                                             /* Otherwise, indicate not */
				return (arg_parse_error);
		}
	}

	/* if there were no arguments then arg2_parse_tokens should have
	   called arg2_do_help and returned arg_parse_help which is
	   caught by the test for ret above. Thus if we get this far it
	   must be an error. */

	return (arg_parse_error);
}
/*}}}  */
/*{{{  arg2_parse_tokens */
/******************************************************************************
 * arg2_parse_tokens  Parse an argv array
 *
 * Arguments:
 *   version_number     : The version of arglib to use
 *   argc               : Number of tokens on command line
 *   argv               : Token strings
 *   argd               : Description of expected arguments
 *   do_help_if_no_args : Flag which should be set to 1 if the help function
 *                        should be called if no arguments are found.
 *                        0 otherwise.
 *   
 ******************************************************************************/
static arg_parse_result arg2_parse_tokens (int version_number,
					   int const argc, char const *argv[],
					   arg2_descriptor const argd[],
					   int do_help_if_no_args)
{
	char prefix_char;
	char alt_prefix_char;
	int token;
	arg2_options opt_type;

	assert (version_number == 1);

	prefix_char = arg2_host_switch_char (version_number);
	alt_prefix_char = arg2_host_alternate_switch_char (version_number);

	/* Display the help page if no arguments are passed */
	if (do_help_if_no_args && argc == 0) {
		arg2_do_help (argd);
		return (arg_parse_help);
	}

	/* Otherwise, go through each option in turn */
	token = 0;
	while (token < argc) {
		if ((argv[token][0] == prefix_char) ||
				(argv[token][0] == alt_prefix_char)) {
			opt_type = arg2_option_type (argv[token], argd);
			if (opt_type == arg2_end) {
				if (arg2_do_error (argv[token], argd) == arg_terminate) {
					return (arg_parse_error);
				} else {
					token++;                                                                                        /* No handler found, but arg2_error */
				}                                                                                                                               /* function indicates that we should */
																				/* continue, so skip this argument */
			}
			switch (opt_type) {
				case arg2_operand :
					if (arg2_do_operand (argv[token],
							(token < argc - 1) ? argv[token+1] : NULL, argd, &token) ==
							arg_terminate) {
						return (arg_parse_error);
					}
					break;
				case arg2_single :
					if (arg2_do_single (argv[token], NULL, argd, &token) ==
							arg_terminate) {
						return (arg_parse_error);
					}
					break;
				case arg2_both :
					if (arg2_do_both (argv[token],
							(token < argc - 1) ? argv[token+1] : NULL, argd, &token) ==
							arg_terminate) {
						return (arg_parse_error);
					}
					break;
				default:
					break;
			}
		} else if ((argv[token][0] == FILE_MARKER) && (argv[token][1] != NUL)) {
			if (arg2_read_file (version_number, &(argv[token][1]), argd, &token) ==
					arg_terminate) {
				return (arg_parse_error);
			}
		} else {
			if (arg2_do_token (argv[token], NULL, argd, &token) == arg_terminate) {
				return (arg_parse_error);
			}
		}
	}

	return (arg_parse_ok);
}
/*}}}  */
/*{{{  arg2_sub_string_parse */
arg_parse_result arg2_sub_string_parse (
		int version_number,                                                                     /* The version of arglib to use */
		char const *str,                                                                                /* The string to parse */
		arg2_descriptor const argd[],  /* Description of expected arguments */
		int * argcount) 
{

	arg_parse_result res;
	arg_control ctl;
	char const **argv;
	int argc;
	int i;

	assert (version_number == 1);

	argc = 0;
	res = arg_parse_ok;
	ctl = arg_continue;
	argv = NULL;
	if (str != NULL) {
		ctl = arg2_parse_string (str, &argc, &argv, argd);
	}
	if (ctl == arg_terminate) {
		res = arg_parse_error;
	} else if (argc != 0) {
		res = arg2_parse_tokens (version_number, argc, argv, argd, 0);
	}
	if (argc > 0) {
		for (i=0; i<argc; ++i) if (argv[i] != NULL) free ((void *) argv[i]);
	}

	*argcount = argc;
	return (res);
}
/*}}}  */
/*{{{  arg2_string_parse */
arg_parse_result arg2_string_parse (
		int version_number,                                                                     /* The version of arglib to use */
		char const *str,                                                                                /* The string to parse */
		arg2_descriptor const argd[]) {                 /* Description of expected arguments */

	int discarded_value;

	return arg2_sub_string_parse(version_number, str, argd, 
				     &discarded_value);
}
/*}}}  */
/*{{{  arg2_env_var_parse */
arg_parse_result arg2_env_var_parse (
		int version_number,                                                                     /* The version of arglib to use */
		char const *env,                                                                                /* The name of the environment var */
		arg2_descriptor const argd[]) {                 /* Description of expected arguments */

	arg_parse_result res;
	char const *env_str;
	int argc = 0;

	assert (version_number == 1);

	res = arg_parse_ok;
	if (env != NULL) {
		env_str = getenv (env);
		if (env_str == NULL) {
			if (arg2_do_env_var (env, argd) == arg_terminate) res = arg_parse_error;
		} else {
			res = arg2_sub_string_parse (version_number, env_str, argd, &argc);
		}
	}

	if (res != arg_parse_ok) return (res);
																				/* If we received an error, return */

	if (argc != 0) {                                                                                        /* If there were tokens on the CL */
		switch (arg2_do_end (argd)) {                           /* Run the user end-of-parse routine */
			case arg_continue:                                                              /* If success returned, indicate so */
				return (arg_parse_ok);
			case arg_terminate:                                                             /* Otherwise, indicate not */
				return (arg_parse_error);
		}
	}

	return (res);
}
/*}}}  */
/*{{{  arg2_parse_and_env_var_parse */
/******************************************************************************
 * arg2_parse_and_env_var_parse  Parse a command line and environment
 *                               variable in one fell swoop.   
 *
 * Arguments:
 *   version_number     : The version of arglib to use
 *   argc               : Number of tokens on command line
 *   argv               : Token strings
 *   env                : The name of the environment variable
 *   argd               : Description of expected arguments
 *
 ******************************************************************************/
arg_parse_result arg2_parse_and_env_var_parse (int version_number,
		   int const argc, char const *argv[],
		   char const *env, arg2_descriptor const argd[])
{

  arg_parse_result ret;
  char const *env_str;
  int env_argc = 0;

  assert (version_number == 1);

  /* do the environment variable first - if it exists */

  ret = arg_parse_ok;

  if (env != NULL) 
  {
    env_str = getenv (env);
    if (env_str != NULL) 
    {
       ret = arg2_sub_string_parse (version_number, env_str, argd, &env_argc);
    }
  }

  if (ret != arg_parse_ok) 
    return (ret);

  /* if we got this far the environment variable was OK or its absence
     was ignored.
     Now do the command line */

  ret = arg2_parse_tokens (version_number, argc, argv, argd, (env_argc == 0));

  if (ret != arg_parse_ok) 
    return (ret);

  /* if we got this far both the env var and the command line were OK or
     there were no arguments. If there are no arguments then run the
     arg_end function */

  if (argc + env_argc != 0) 
  {
    switch (arg2_do_end (argd)) 
    {
      case arg_continue:
	return (arg_parse_ok);
      case arg_terminate:
	return (arg_parse_error);
    }
  }

  return (arg_parse_ok);
}

/*}}}  */

/*{{{  arg2_option_type */
static arg2_options arg2_option_type (
		const char *token,
		arg2_descriptor const argd[]) {

	int desc;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_string != NULL) {
			switch (arg2_compare (argd[desc].arg_string, token + 1)) {
				case ident:
				case prefix:
					return (argd[desc].arg_type);

				default:
					break;
			}
		}
		desc++;
	}

	return (arg2_end);
}
/*}}}  */

/*{{{  arg2_do_help */
/* This function invokes the user's help function, if it exists. */
/* It searches the descriptor table to find the help entry; if it */
/* does not exist or is NULL, then arg_continue is returned instead. */

static arg_control arg2_do_help (arg2_descriptor const argd[]) {
	int desc;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_type == arg2_help) {
			if (argd[desc].arg_function != NULL) {
				return ((argd[desc].arg_function) (NULL, NULL, NULL));
			} else {
				return (arg_continue);
			}
		}
		desc++;
	}

	return (arg_continue);
}
/*}}}  */
/*{{{  arg2_do_end */
static arg_control arg2_do_end (arg2_descriptor const argd[]) {
	int desc;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		desc++;
	}

	if (argd[desc].arg_function != NULL) {
		return ((argd[desc].arg_function) (NULL, NULL, NULL));
	}

	return (arg_continue);
}
/*}}}  */

/*{{{  arg2_do_error */
/* This function invokes the user's error function, if it exists. */
/* It searches the descriptor table to find the error entry; if it */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control arg2_do_error (
		char const *token,
		arg2_descriptor const argd[]) {

	int desc;
	char *new;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_type == arg2_error) {
			if (argd[desc].arg_function != NULL) {
				new = arg2_new_string (token);
				if (new == NULL) return (arg2_do_mem_error (argd));
				return ((argd[desc].arg_function) (new, NULL, NULL));
			} else {
				return (arg_terminate);
			}
		}
		desc++;
	}

	return (arg_terminate);
}
/*}}}  */
/*{{{  arg2_do_syntax */
/* This function invokes the user's syntax function, if it exists. */
/* It searches the descriptor table to find the syntax entry; if it */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control arg2_do_syntax (
		char const *token,
		arg2_descriptor const argd[]) {

	int desc;
	char *new;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_type == arg2_syntax) {
			if (argd[desc].arg_function != NULL) {
				new = arg2_new_string (token);
				if (new == NULL) return (arg2_do_mem_error (argd));
				return ((argd[desc].arg_function) (new, NULL, NULL));
			} else {
				return (arg_terminate);
			}
		}
		desc++;
	}

	return (arg_terminate);
}
/*}}}  */
/*{{{  arg2_do_env_var */
/* This function invokes the user's env_var function, if it exists. */
/* It searches the descriptor table to find the env_var entry; if it */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control arg2_do_env_var (
		char const *token,
		arg2_descriptor const argd[]) {

	int desc;
	char *new;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_type == arg2_env_var) {
			if (argd[desc].arg_function != NULL) {
				new = arg2_new_string (token);
				if (new == NULL) return (arg2_do_mem_error (argd));
				return ((argd[desc].arg_function) (new, NULL, NULL));
			} else {
				return (arg_terminate);
			}
		}
		desc++;
	}

	return (arg_terminate);
}
/*}}}  */
/*{{{  arg2_do_file_err */
/* This function invokes the user's file-error function, if it exists. */
/* It searches the descriptor table to find the file-error entry; if it */
/* does not exist or is NULL, then arg_terminate is returned instead. */

static arg_control arg2_do_file_err (
		char const *token,
		arg2_descriptor const argd[]) {

	int desc;
	char *new;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_type == arg2_file_err) {
			if (argd[desc].arg_function != NULL) {
				new = arg2_new_string (token);
				if (new == NULL) return (arg2_do_mem_error (argd));
				return ((argd[desc].arg_function) (new, NULL, NULL));
			} else {
				return (arg_terminate);
			}
		}
		desc++;
	}

	return (arg_terminate);
}
/*}}}  */
/*{{{  arg2_do_mem_error */
/* This function invokes the user's memory allocation error function, */
/* if it exists.  It searches the descriptor table to find the memory */
/* error entry; if it does not exist or is NULL, then arg_terminate is */
/* returned instead. */

static arg_control arg2_do_mem_error (arg2_descriptor const argd[]) {
	int desc;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_type == arg2_mem_err) {
			if (argd[desc].arg_function != NULL) {
				return ((argd[desc].arg_function) (NULL, NULL, NULL));
			} else {
				return (arg_terminate);
			}
		}
		desc++;
	}

	return (arg_terminate);
}
/*}}}  */

/*{{{  arg2_do_operand */
static arg_control arg2_do_operand (
		const char *token,
		const char *next_token,
		arg2_descriptor const argd[],
		int *token_num) {

	int desc;
	char *new;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_string != NULL) {
			switch (arg2_compare (argd[desc].arg_string, token + 1)) {
				case ident:
					(*token_num) += 2;
					if (next_token == NULL) return (arg2_do_error (token, argd));
					new = arg2_new_string (next_token);
					if (new == NULL) return (arg2_do_mem_error (argd));
					if (argd[desc].arg_function != NULL) {
						return ((argd[desc].arg_function) (NULL, new, argd[desc].arg_data));
					} else {
						return (arg_terminate);
					}

				case prefix:
					(*token_num) += 1;
					new = arg2_new_string (token + 1 + strlen (argd[desc].arg_string));
					if (new == NULL) return (arg2_do_mem_error (argd));
					if (argd[desc].arg_function != NULL) {
						return ((argd[desc].arg_function) (NULL, new, argd[desc].arg_data));
					} else {
						return (arg_terminate);
					}

				default:
					break;
			}
		}
		desc++;
	}

	return (arg_terminate);
}
/*}}}  */
/*{{{  arg2_do_single */
static arg_control arg2_do_single (
		const char *token,
		const char *next_token,
		arg2_descriptor const argd[],
		int *token_num) {

	int desc;
	char *new;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_string != NULL) {
			switch (arg2_compare (argd[desc].arg_string, token + 1)) {
				case ident:
					(*token_num) += 1;
					new = arg2_new_string (token + 1);
					if (new == NULL) return (arg2_do_mem_error (argd));
					if (argd[desc].arg_function != NULL) {
						return ((argd[desc].arg_function) (new, NULL, argd[desc].arg_data));
					} else {
						return (arg_terminate);
					}

				case prefix:
					return (arg2_do_error (token, argd));

				default:
					break;
			}
		}
		desc++;
	}

	return (arg_terminate);
}
/*}}}  */
/*{{{  arg2_do_both */
static arg_control arg2_do_both (
		const char *token,
		const char *next_token,
		arg2_descriptor const argd[],
		int *token_num) {

	int desc;
	char *new1, *new2;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_string != NULL) {
			switch (arg2_compare (argd[desc].arg_string, token + 1)) {
				case ident:
					(*token_num) += 2;
					if (next_token == NULL) return (arg2_do_error (token, argd));
					new1 = arg2_new_string (token);
					if (new1 == NULL) return (arg2_do_mem_error (argd));
					new2 = arg2_new_string (next_token);
					if (new2 == NULL) return (arg2_do_mem_error (argd));
					if (argd[desc].arg_function != NULL) {
						return ((argd[desc].arg_function) (new1, new2, argd[desc].arg_data));
					} else {
						return (arg_terminate);
					}

				case prefix:
					(*token_num) += 1;
					new1 = arg2_new_string (token + 1);
					if (new1 == NULL) return (arg2_do_mem_error (argd));
					*(new1 + strlen (argd[desc].arg_string)) = NUL;
					new2 = arg2_new_string (token + 1 + strlen (argd[desc].arg_string));
					if (new2 == NULL) return (arg2_do_mem_error (argd));
					if (argd[desc].arg_function != NULL) {
						return ((argd[desc].arg_function) (new1, new2, argd[desc].arg_data));
					} else {
						return (arg_terminate);
					}

				default:
					break;
			}
		}
		desc++;
	}

	return (arg_terminate);
}
/*}}}  */
/*{{{  arg2_do_token */
static arg_control arg2_do_token (
		const char *token,
		const char *next_token,
		arg2_descriptor const argd[],
		int *token_num) {

	int desc;
	char *new;

	(*token_num) += 1;

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_type == arg2_token) {
			if (argd[desc].arg_function != NULL) {
				new = arg2_new_string (token);
				if (new == NULL) return (arg2_do_mem_error (argd));
				if (argd[desc].arg_function != NULL) {
					return ((argd[desc].arg_function) (new, NULL, argd[desc].arg_data));
				} else {
					return (arg_terminate);
				}
			} else {
				return (arg2_do_error (token, argd));
			}
		}
		desc++;
	}

	return (arg2_do_error (token, argd));
}
/*}}}  */

/*{{{  arg2_scan_token */
/* This function scans a string and picks off the first token */

static arg_parse_result arg2_scan_token (
		char const *const st,
		char const **const en,
		arg2_descriptor const argd[]) {

	arg_parse_result res;
	char * new;

	res = arg_continue;
	if (*st == QUOTE) {
		for (*en = st+1; **en != NUL; ++*en) {
			if (**en == QUOTE) {
				*en += 1;
				if (**en != QUOTE) {
					if (isspace (**en) || (**en == NUL))
						return (res);
					*en = NULL;
					new = arg2_new_string (st);
					if (new == NULL) {
						(void) arg2_do_mem_error (argd);
						return (arg_parse_error);
					}
					return (arg2_do_syntax ((char const *) new, argd));
				}
			}
		}
		if (**en == NUL) {
			*en = NULL;
			new = arg2_new_string (st);
			if (new == NULL) {
				(void) arg2_do_mem_error (argd);
				return (arg_parse_error);
			}
			return (arg2_do_syntax ((char const *) new, argd));
		}
	} else {
		for (*en = st; ! isspace (**en) && (**en != NUL); ++*en) ;
	}

	return (res);
}
/*}}}  */
/*{{{  arg2_parse_string */
/* This function strips the first token off a string and then recurses */
/* to find all the other tokens. These are built up into an array of */
/* char pointers. The return value indicates whether to continue the */
/* parsing process. */

/* Double quotes delimit a single tokens. Two adjacent double quotes */
/* within a string represent a single one. */

static arg_control arg2_parse_string (
		char const *const str,
		int *const argc,
		char const ***const argv,
		arg2_descriptor const argd[]) {

	arg_parse_result res;
	int n;
	char const * st, * en;

	res = arg_continue;
	n = *argc;                                                                                                              /* Index into table for this one */
	if (str != NULL) {
		for (st = str; isspace (*st); ++st);
	} else {
		st = NULL;
	}
	if ((st == NULL) || (*st == NUL)) {
		if (n != 0) {
			*argv = (char const * *) malloc (n * sizeof(char const *));
			if (*argv == NULL) return (arg2_do_mem_error (argd));
		}
	} else {
		res = arg2_scan_token (st, &en, argd);
		if (res == arg_terminate) {
			*argc = 0;
		} else {
			*argc += 1;
			res = arg2_parse_string (en, argc, argv, argd);
			if (res != arg_terminate) {
				(*argv)[n] = arg2_copy_token (st, en);
				if ((*argv)[n] == NULL) return (arg2_do_mem_error (argd));
			}
			else if (*argv != NULL)
				(*argv)[n] = NULL;
		}
	}
	return (res);
}
/*}}}  */

/*{{{  arg2_get_line */
/* This function obtains a line from a file. It will be terminated by a */
/* a newline character and then a null character. It will ensure that */
/* a new area is obtained for each line. The caller must free this area */
/* after is used. If there is nothing left in the file, it returns NULL. */

#define STEP 1000

static arg_control arg2_get_line (
		FILE * const f, char const * const fn,
		arg2_descriptor const argd[],
		char ** const line) {

	char *buf, *part;
	size_t buflen, len;
	arg_control res;

	res = arg_continue;
	buf = NULL;
	buflen = 0;
	while (! feof (f)) {
		buflen += STEP;
		part = buf;
		buf = (char *) malloc (buflen * sizeof (char));
		if (buf == NULL) return (arg2_do_mem_error (argd));
		if (part != NULL) {
			strcpy (buf, part);
			free (part);
			part = buf + strlen (buf);
		} else {
			part = buf;
		}
		part[0] = NUL;
		part = fgets (part, STEP, f);
		if (feof (f)) {
			if (part == NULL) {
				buf = NULL;
				break;
			}
		}
		if (ferror (f)) {
			res = arg2_do_file_err (fn, argd);
			free (buf);
			buf = NULL;
			break;
		}
		if (part != NULL) {
			len = strlen (part);
			if ((len > 0) && (part[len-1] == NL)) break;
		}
	}
	*line = buf;

	return (res);
}

#undef STEP
/*}}}  */
/*{{{  arg2_read_file */
/* This function reads an indirect file and parses each line in turn */

static arg_control arg2_read_file (
		int version_number,
		char const *fn,
		arg2_descriptor const argd[],
		int *token_num) {

	FILE *f;
	char *line;
	arg_control res;

	(*token_num) += 1;

	f = fopen (fn, "r");
	if (f == NULL) {
		res = arg2_do_file_err (fn, argd);
	} else {
		res = arg2_get_line (f, fn, argd, (char ** const) &line);
		while ((res == arg_continue) && (line != NULL)) {
			if (line[0] != COMMENT) {
				if (arg2_string_parse (version_number, line, argd) == arg_parse_error) {
					res = arg_terminate;
					break;
				}
			}
			free (line);
			res = arg2_get_line (f, fn, argd, (char ** const) &line);
		}
		fclose (f);
	}

	return (res);
}
/*}}}  */

/*{{{  arg2_print_help_page */
void arg2_print_help_page (
		int version_number,
		arg2_help_page_type type,
		arg2_descriptor const argd[]) {

	int desc;

	assert (version_number == 1);

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_type == arg2_help_info) {
			if (argd[desc].arg_data != NULL) {
				arg2_do_help_page ((arg2_help_page_info *) argd[desc].arg_data,
					type, argd);
			}
		}
		desc++;
	}
}
/*}}}  */
/*{{{  arg2_print_info_line */
void arg2_print_info_line (
		int version_number,
		arg2_descriptor const argd[]) {

	int desc;

	assert (version_number == 1);

	desc = 0;
	while (argd[desc].arg_type != arg2_end) {
		if (argd[desc].arg_type == arg2_help_info) {
			if (argd[desc].arg_data != NULL) {
				arg2_do_info_line ((arg2_help_page_info *) argd[desc].arg_data, NULL);
			}
		}
		desc++;
	}
}
/*}}}  */
/*{{{  arg2_get_info_line */
void arg2_get_info_line (int version_number, arg2_descriptor const argd[],
			 char *buffer) 
{
  int desc;

  assert (version_number == 1);

  desc = 0;

  if (buffer == NULL)
    return;

  while (argd[desc].arg_type != arg2_end) 
  {
    if (argd[desc].arg_type == arg2_help_info) 
    {
      if (argd[desc].arg_data != NULL) 
      {
	arg2_do_info_line ((arg2_help_page_info *) argd[desc].arg_data, buffer);
      }
    }
    desc++; 
  }
}
/*}}}  */

/*{{{  arg2_compare_strings */
static int arg2_compare_strings (const void *a, const void *b) {
	const char *as, *bs;

	as = ((arg2_descriptor *) a) -> arg_string;
	bs = ((arg2_descriptor *) b) -> arg_string;

	if ((as == NULL) && (bs == NULL)) return (0);
	if (as == NULL) return (-1);
	if (bs == NULL) return (1);
	return (strcmp (as, bs));
}
/*}}}  */
/*{{{  arg2_do_help_page */
static void arg2_do_help_page (
		arg2_help_page_info *info,
		arg2_help_page_type type,
		arg2_descriptor const argd[]) {

	int desc;
	const char *bar;
	arg2_descriptor *argd_copy;
	int comment_left_margin, num_options, num_tail_lines, num_user_tail_lines;

	/* Find out how many entries are in the descriptor table */
	desc = 0;
	while (argd[desc].arg_type != arg2_end) desc++;
	/* Make a copy of the table so that we can sort it */
	argd_copy = malloc (sizeof (arg2_descriptor) * (desc + 1));
	if (argd_copy == NULL) {
		arg2_do_mem_error (argd);
		return;
	}
	memcpy (argd_copy, argd, sizeof (arg2_descriptor) * (desc + 1));
	/* Sort the copy of argd on option string, but leave the */
	/* arg2_end entry in the same place */
	qsort (argd_copy, desc, sizeof (arg2_descriptor), arg2_compare_strings);

	/* Print the first header line */
	printf ("%s : %s\n",
		info -> tool_name,
		info -> toolset_description
	);

	/* Print the second header line */
	arg2_do_info_line (info, NULL);

	/* Print the third header line */
	printf ("(c) Copyright SGS-THOMSON Microelectronics Limited %s\n", info -> copyright_years);
	printf ("Modifications (c) Copyright 1998, 1999, 2000, 2001 (see AUTHORS)\n\n");

	/* Print the usage line */
	printf ("Usage: %s\n\n", info -> usage_line);

	/* Print the comment preceding the option list */
	if (type & arg2_help_long_and_ext) {
		puts ("Options:");
	} else {
		puts ("Options include:");
	}

	/* Calculate the left margin position of the comments */
	comment_left_margin = 4;
	desc = 0;
	while (argd_copy[desc].arg_type != arg2_end) {
		if (argd_copy[desc].arg_help_page & type) {
			int this_margin = 0;

			bar = (argd_copy[desc].arg_help_text == NULL) ? NULL : 
				strchr (argd_copy[desc].arg_help_text, BAR);
			if (bar != NULL) {
				this_margin = 4 + strlen (argd_copy[desc].arg_string) +
					strlen (argd_copy[desc].arg_help_text) - strlen (bar);
			}
			else if (argd_copy[desc].arg_string != NULL) {
				this_margin = 3 + strlen (argd_copy[desc].arg_string);
			}
			if (this_margin > comment_left_margin) {
				comment_left_margin = this_margin;
			}
		}
		desc++;
	}

	/* Now calculate the total number of lines in the tail line comments */
	desc = 0;
	num_tail_lines = 0;
	while (argd_copy[desc].arg_type != arg2_end) {
		if ((argd_copy[desc].arg_type == arg2_tail_line) &&
				(type & argd_copy[desc].arg_help_page)) {
			char *p;
			/* If we have found the first tail line, then set the number found to 1 */
			if (num_tail_lines == 0) num_tail_lines = 1;
			/* Now look through the tail line string and count the newline chars */
			p = argd_copy[desc].arg_data;
			while (*p != NUL) {
				num_tail_lines += (*p == NL);
				p++;
			}
		}
		desc++;
	}
	num_user_tail_lines = num_tail_lines;

	/* Add in the implicit tail line comment if env_var is set */
	if (info -> env_var != NULL)
	  num_tail_lines++;

	/* Print the option/comment table */
	desc = 0;
	num_options = 0;
	while (argd_copy[desc].arg_type != arg2_end) {
		if ((argd_copy[desc].arg_help_page & type) &&
				(argd_copy[desc].arg_help_text != NULL)) {
			num_options++;
			/* Make sure the list is not too long for the help page */
			if ((type & arg2_help_short) &&
					(num_options > 15 - num_tail_lines - (num_tail_lines != 0))) {
				break;
			}
			printf ("  %s", argd_copy[desc].arg_string);
			bar = (argd_copy[desc].arg_help_text == NULL) ? NULL :
				strchr (argd_copy[desc].arg_help_text, BAR);
			if (bar != NULL) {
				int len, spaces, x;

				putchar (' ');
				len = strlen (argd_copy[desc].arg_help_text) - strlen (bar);
				for (x=0; x<len; x++) putchar (argd_copy[desc].arg_help_text[x]);
				spaces = comment_left_margin -
					(2 + strlen (argd_copy[desc].arg_string) + len);
				for (x=0; x<spaces; x++) putchar (' ');
				printf ("%s\n", bar + 1);
			} else {
				int spaces, x;

				spaces = comment_left_margin -
					(1 + strlen (argd_copy[desc].arg_string));
				for (x=0; x<spaces; x++) putchar (' ');
				printf ("%s\n", argd_copy[desc].arg_help_text);
			}
		}
		desc++;
	}

	/* Now output the tail line comments */
	if (num_user_tail_lines != 0) {
		puts ("");
		desc = 0;
		while (argd_copy[desc].arg_type != arg2_end) {
			if ((argd_copy[desc].arg_type == arg2_tail_line) &&
					(type & argd_copy[desc].arg_help_page)) {
				printf ("%s\n", (char *) argd_copy[desc].arg_data);
			}
			desc++;
		}
	}

	if ((info -> env_var != NULL) || (type & arg2_help_extended))
	{
	  /* Here we know we will be emitting default tail lines so lets
	     print a blank line if no user tail lines were emitted */
		if (num_user_tail_lines == 0) 
		{
			puts ("");
		}
	}

	/* Print the implicit tail line comment if env_var is set */
	if (info -> env_var != NULL)
		printf ("Default options may be placed in the environment variable %s\n", info -> env_var);


	/* If this is an extended help page, print the standard footer */
	if (type & arg2_help_extended)
		puts ("Options beginning with Z are unsupported and for SGS-THOMSON's use only");

	free (argd_copy);
}
/*}}}  */
/*{{{  arg2_do_info_line */
static void arg2_do_info_line (arg2_help_page_info *info, char *buffer) 
{
  char *arch_os = ARCH_OS_STRING;

  if (buffer == NULL)
    printf ("%s Version %s (%s) (%s)\n", info -> tool_description,
	    info -> version, info -> build_time, arch_os);
  else
    sprintf (buffer, "%s Version %s (%s) (%s)", info -> tool_description,
	     info -> version, info -> build_time, arch_os);    
}

/*}}}  */

/*{{{  arg2_host_switch_char */
char arg2_host_switch_char (int version_number) 
{
  assert (version_number == 1);

  return arg_host_switch_char();
}
/*}}}  */
/*{{{  arg2_host_alternate_switch_char */
char arg2_host_alternate_switch_char (int version_number) 
{
  assert (version_number == 1);

  return arg_host_alternate_switch_char();
}
/*}}}  */

/*}}}  */
