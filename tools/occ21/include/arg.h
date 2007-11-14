/*
 *	Structures for command line parsing
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

/* $Id: arg.h,v 1.1 1996/04/15 10:51:57 djb1 Exp $ */


/* History */

/* ahp: First version written */
/* ahp: Converted for CMS 19910823 */
/* ahp: Added const qualifier to arg_string 930804 */
/* ahp: Added string parsing and environment variable parsing 930421 */
/* ahp: Added indirect options files introduced by @ character 930422 */
/* bas: Added arg_mem_err to arg_options type 940726 */
/* bas: Added new arglib interface 940808 */

#ifndef __ARG_H
#define __ARG_H

/*{{{  Defines */
/* This is the define for the current version number of the */
/* new arglib routines. */

#define ARGLIB_VERSION 1
/*}}}*/
/*{{{  Typedefs */
/*{{{  arg_control */
typedef enum {
	arg_continue	= 1,	/* Continue after processing */
	arg_terminate	= 2	/* Terminate immediately */
} arg_control;
/*}}}*/
/*{{{  arg_parse_result */
typedef enum {
	arg_parse_ok	= 0,	/* Parse completed successfully */
	arg_parse_error	= 1,	/* Error found during parsing */
	arg_parse_help	= 2	/* Help page was displayed */
} arg_parse_result;
/*}}}*/

/*{{{  arg_options */
typedef enum {	/* Attributes of argument */
	arg_single	= 1,	/* Just an argument on its own */
	arg_operand	= 2,	/* Argument has an operand */
	arg_token	= 3,	/* Function called if no prefix char */
	arg_end		= 4,	/* Last entry in table */
	arg_error	= 5,	/* Error token found */
	arg_help	= 6,	/* Invoke if no arguments present */
	arg_syntax	= 7,	/* Invoke if syntax error in string */
	arg_env_var	= 8,	/* Invoke if env var not present */
	arg_file_err	= 9,	/* Invoke if error reading a file */
	arg_mem_err	= 10	/* Invoke if error allocating memory */
} arg_options;
/*}}}*/
/*{{{  arg_descriptor */
typedef struct { /* Descriptor of an argument */
	char const *arg_string;  	/* String for argument */
	arg_options	arg_type;	/* Type of the argument */
	arg_control (*arg_fn) (char *);	/* Function to call for this argument */
} arg_descriptor;
/*}}}*/

/*{{{  arg2_help_page_type */
typedef enum {
  arg2_help_na           = 0, /* Entry is not associated with a help page */
  arg2_help_short        = 1, /* Option is shown on short help page */
  arg2_help_long         = 2, /* Option is shown on long help page */
  arg2_help_extended     = 4, /* Option is shown on extended help page */
  arg2_help_long_and_ext = 6, /* Option is shown on long/extended help pages */
  arg2_help_all          = 7  /* Option is shown on all help pages */
} arg2_help_page_type;
/*}}}*/
/*{{{  arg2_options */
typedef enum {
  arg2_token,     /* Entry is the command line token handler */
  arg2_operand,   /* Entry is an operand handler */
  arg2_single,    /* Entry is a flag-only handler */
  arg2_both,      /* Entry is a flag/operand handler */
  arg2_help,      /* Entry is the help page handler */
  arg2_error,     /* Entry is the general error handler */
  arg2_syntax,    /* Entry is the syntax-error handler */
  arg2_env_var,   /* Entry is the env-var-not-found handler */
  arg2_file_err,  /* Entry is the file-error handler */
  arg2_mem_err,   /* Entry is the memory-allocation-error handler */
  arg2_help_info, /* Entry is the ptr to a help page info struct */
  arg2_tail_line, /* Entry is a string to go on end of help page */
  arg2_end        /* Entry indicates the end of descriptor table*/
} arg2_options;
/*}}}*/
/*{{{  arg2_descriptor */
typedef struct {
  char const *arg_string;            /* Flag string for the argument */  
  arg2_options arg_type;             /* Type of the argument */
  void *arg_data;                    /* Pointer to argument data */
  arg_control (*arg_function) (const char *, const char *, void *);
                                     /* Pointer to argument handling routine */
  arg2_help_page_type arg_help_page; /* Which help pages the option will appear on */
  const char *arg_help_text;         /* The help page comment for options */
} arg2_descriptor;
/*}}}*/
/*{{{  arg2_help_page_info */
typedef struct {
  const char *tool_name;            /* The command name for the tool */
  const char *toolset_description;  /* The long name for the toolset and tool */
  const char *tool_description;     /* The long name for the tool */
  const char *version;              /* The version string */
  const char *build_time;           /* When the tool was built */
  const char *copyright_years;      /* Years that the tool is copyrighted */
  const char *usage_line;           /* A short line describing overall usage */
  const char *env_var;              /* The name of the environment variable
                                       where default arguments may be found */
} arg2_help_page_info;
/*}}}*/
/*}}}*/
/*{{{  Prototypes */
/*{{{  arg_parse */
/* arg_parse is a function that scans the arguments in tokenised form */
/* and calls the appropriate functions to process them. */
/* At least one descriptor should contain the arg_error indicator */
/* to show where to call if an error occurs. */
/* The last entry in the descriptor list must contain the arg_end */
/* marker. If there is a function here, it is called at the end. */

/* The functions that it calls all receive a (char *) argument. This */
/* argument is the operand for those which require it, suitably */
/* converted in case. In the case of an error, it contains the token */
/* that could not be matched, or that was missing an operand. If no */
/* error routine is given, then it is assumed that termination is the*/
/* correct response. In other cases, the argument is a null pointer. */
/* The user must free these arguments where necessary. */

/* If any of these functions return arg_terminate, then the parser */
/* stops immediately, freeing any of its own temporaries. It will */
/* return arg_parse_error to the caller in this case. */

/* Arguments :                                */
/*   argc : Number of tokens on command line  */
/*   argv : Token strings                     */
/*   argd : Description of expected arguments */

arg_parse_result arg_parse (int const argc, char const *argv[],	
		            arg_descriptor const argd[]);
/*}}}*/
/*{{{  arg_string_parse */
/* arg_string_parse: Parse options found in a character string. */
/* A character string is scanned and a list of tokens is created. */
/* These tokens are then passed into arg_parse for the normal scanning */
/* process. */
/* When converting a string to tokens, the syntax is: */
/*   tokens are separated by one or more whitespace characters, except: */
/*   double quotes (") are used to delimit strings, but a double quote */
/*   inside a string is written as two double quotes. */
/* [Doubling quotes for inclusion within quotes has predecents in Ada, */
/* Fortran, PL/I and other languages. A backslash is dangerous when */
/* used in MS-DOS, an include directory could be written with an */
/* ending backslash at the end of a string.] */
/* Any zero character ends the string. */
/* If a syntax error is found when parsing the string, the user's */
/* error handler (given by arg_syntax) is invoked with the best */
/* approximation of the erroneous part of the string. If it returns */
/* with arg_continue, then the tokens are processed normally, but with */
/* all invalid ones replaced by an empty string. */
/* If the string argument is a NULL pointer, then the parsing is */
/* taken to be successful, no error is reported and no help is given. */
/* If the string contains no tokens, then the parse is ok, but no help */
/* is given. */

/* Arguments :                                */ 
/*   str  : String to parse                   */
/*   argd : Description of expected arguments */

arg_parse_result arg_string_parse (char const *str, 
                                   arg_descriptor const argd[]);
/*}}}*/
/*{{{  arg_env_var_parse */
/* arg_env_var_parse: Parse the contents of an environment variable. */
/* The name of an environment variable is passed. This is extracted */
/* and the string value is parsed as though it were a string with the */
/* same syntactic conventions as arg_string_parse. */
/* If the env variable is not present, then the user's error handler */
/* (given by arg_env_var) is invoked with a copy of the env var name. */
/* If the name is given as a NULL pointer, then it is assumed that a */
/* successful parse occurs and no help is invoked. */

/* Arguments :                                */ 
/*   var  : Name of environment variable      */
/*   argd : Description of expected arguments */

arg_parse_result arg_env_var_parse (char const *var, 
                                    arg_descriptor const argd[]);
/*}}}*/
/*{{{  arg_host_switch_char */
/* This function returns the host specific switch character. This switch */
/* character may differ between different hosts.                         */

char arg_host_switch_char (void);
/*}}}*/
/*{{{  arg_host_alternate_switch_char */
/* This function returns the hosts alternate switch character. This switch */
/* character in general remains the same across all hosts.                 */

char arg_host_alternate_switch_char (void);
/*}}}*/
/*{{{  arg_version */
/* This function is deprecated. It is left for backwards compatibility only */

char const *arg_version (void);
/*}}}*/

/*{{{  arg2_parse */
/* Arguments :                                            */
/*   version_number : The version number of arglib to use */
/*   argc           : Number of tokens on command line    */
/*   argv           : Token strings                       */
/*   argd           : Argument descriptor table           */

arg_parse_result arg2_parse (int version_number, int const argc,
                             char const *argv[], arg2_descriptor const argd[]);
/*}}}*/
/*{{{  arg2_string_parse */
/* Arguments :                                            */
/*   version_number : The version number of arglib to use */
/*   str            : String to parse                     */
/*   argd           : Argument descriptor table           */

arg_parse_result arg2_string_parse (int version_number,	char const *str,
                                    arg2_descriptor const argd[]);
/*}}}*/
/*{{{  arg2_env_var_parse */
/* Arguments :                                            */
/*   version_number : The version number of arglib to use */
/*   env            : Name of environment variable        */
/*   argd           : Argument descriptor table           */

arg_parse_result arg2_env_var_parse (int version_number, char const *env,
                                     arg2_descriptor const argd[]);
/*}}}*/
/*{{{  arg2_parse_and_env_var_parse */
/* arg2_parse_and_env_var_parse is a function which combines the */
/* operation of arg2_parse and arg2_env_var_parse. First the environment */
/* variable is scanned followed by the command line. */
/* The behaviour if the environment variable is NULL or does not exist is */
/* the same as for arg_env_var_parse. */

/* The function considers the argument list to be made up of the */
/* arguments from the environment variable combined with those from */
/* the command line. Only if this combined argument list is empty will */
/* the help function (if it exists) be called */

/* Arguments :                                                        */
/*   version : version number of arglib                               */
/*   argc    : Number of tokens on command line                       */
/*   argv    : Token strings                                          */
/*   env_var : environment variable where default arguments are found */
/*   argd    : Description of expected arguments                      */

arg_parse_result arg2_parse_and_env_var_parse (int version_number,
                   int const argc, char const *argv[],
                   char const *env, arg2_descriptor const argd[]);
/*}}}*/
/*{{{  arg2_print_help_page */
/* Arguments :                                            */
/*   version_number : The version number of arglib to use */
/*   type           : The type of help page to print      */
/*   argd           : Argument descriptor table           */

void arg2_print_help_page (int version_number, arg2_help_page_type type,
                           arg2_descriptor const argd[]);
/*}}}*/
/*{{{  arg2_print_info_line */
/* Arguments :                                            */
/*   version_number : The version number of arglib to use */
/*   argd           : Argument descriptor table           */

void arg2_print_info_line (int version_number, arg2_descriptor const argd[]);
/*}}}*/
/*{{{  arg2_get_info_line */
/* Arguments :                                                       */
/*   version_number : The version number of arglib to use            */
/*   argd           : Argument descriptor table                      */
/*   buffer         : The buffer where the version string is written */

void arg2_get_info_line (int version_number, arg2_descriptor const argd[],
                         char *buffer);

/*}}}*/
/*{{{  arg2_host_switch_char */
/* This function returns the host specific switch character. This switch */
/* character may differ between different hosts.                         */

/* Arguments :                                            */
/*   version_number : The version number of arglib to use */

char arg2_host_switch_char (int version_number);
/*}}}*/
/*{{{  arg2_host_alternate_switch_char */
/* This function returns the hosts alternate switch character. This switch */
/* character in general remains the same across all hosts.                 */

/* Arguments :                                            */
/*   version_number : The version number of arglib to use */

char arg2_host_alternate_switch_char (int version_number);
/*}}}*/

/*}}}*/

/*{{{  Comment on indirect file parsing */
/* 930422: Added indirect file processing */
/* If an argument is found beginning with the character @, then it is */
/* taken that the rest of the argument is a filename. This is then */
/* opened in the current directory only - no attempt is made to search*/
/* using ISEARCH. If there is nothing else in the argument but @, or */
/* the file cannot be opened, it is transferred to the error handling */
/* function as an invalid argument. */
/* The syntax of the contents of an indirect file is: */
/*   Each line is self-contained; ie. all arguments must be complete.  */
/*   A line beginning with a hash (#) character is a comment line. */
/*   Each non-comment line is treated like parsing a character string. */
/*   If there is no newline at the end of file, one is assumed. */
/*}}}*/

#endif
