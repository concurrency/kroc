/*
 *	Structures for command line parsing
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

/* arg.h -- Define the structures for command line parsing */
/* Copyright Inmos 1989, 1990 */
/* CMSIDENTIFIER */
/* ARG_AHP:ARG_H@534.S4-FILE;0(26-APR-93)[UNDER-DEVELOPMENT] */
/* Version 1.0 -- 19900129 */
/* History */
/* ahp: First version written */
/* ahp: Converted for CMS 19910823 */
/* ahp: Added const qualifier to arg_string 930804 */
/* ahp: Added string parsing and environment variable parsing 930421 */
/* ahp: Added indirect options files introduced by @ character 930422 */

typedef enum                  /* attributes of argument               */
  {
    arg_single    = 1,        /* just an argument on its own          */
    arg_operand   = 2,        /* argument has an operand              */
    arg_token     = 3,        /* function called if no prefix char    */
    arg_end       = 4,        /* last entry in table                  */
    arg_error     = 5,        /* error token found                    */
    arg_help      = 6,        /* invoke if no arguments present       */
    arg_syntax    = 7,        /* invoke if syntax error in string     */
    arg_env_var   = 8,        /* invoke if env var not present        */
    arg_file_err  = 9         /* invoke if error reading a file       */
  }
   arg_options;
   
typedef enum
  {
    arg_continue  = 1,        /* continue after processing            */
    arg_terminate = 2         /* terminate immediately                */
  }
   arg_control;

typedef struct                /* descriptor of an argument            */
  {
    char const * arg_string;  /* string for argument                  */
    arg_options  arg_type;    /* type of the argument                 */
    arg_control (*arg_fn)(char *);/* function to call for this argument   */
  }
   arg_descriptor;

/* arg_parse is a function that scans the arguments in tokenised form */
/* and calls the appropriate functions to process them.               */
/* At least one descriptor should contain the arg_error indicator     */
/* to show where to call if an error occurs.                          */
/* The last entry in the descriptor list must contain the arg_end     */
/* marker. If there is a function here, it is called at the end.      */

/* The functions that it calls all receive a (char *) argument. This  */
/* argument is the operand for those which require it, suitably       */
/* converted in case. In the case of an error, it contains the token  */
/* that could not be matched, or that was missing an operand. If no   */
/* error routine is given, then it is assumed that termination is the */
/* correct response. In other cases, the argument is a null pointer.  */
/* The user must free these arguments where necessary.                */

/* If any of these functions return arg_terminate, then the parser    */
/* stops immediately, freeing any of its own temporaries. It will     */
/* return arg_parse_error to the caller in this case.                 */

typedef enum
  {
    arg_parse_ok    = 0,      /* parse completed successfully         */
    arg_parse_error = 1,      /* error found during parsing           */
    arg_parse_help  = 2       /* help page was displayed              */
  }
   arg_parse_result;

arg_parse_result arg_parse
  (
    int const ,               /* number of tokens on command line     */
    char const * [],          /* token strings                        */
    arg_descriptor const []   /* description of expected arguments    */
  ) ;


/* arg_string_parse: Parse options found in a character string.       */
/* A character string is scanned and a list of tokens is created.     */
/* These tokens are then passed into arg_parse for the normal scanning*/
/* process.                                                           */
/* When converting a string to tokens, the syntax is:                 */
/*  tokens are separated by one or more whitespace characters, except:*/
/*  double quotes (") are used to delimit strings, but a double quote */
/*  inside a string is written as two double quotes.                  */
/* [Doubling quotes for inclusion within quotes has predecents in Ada,*/
/* Fortran, PL/I and other languages. A backslash is dangerous when   */
/* used in MS-DOS, an include directory could be written with an      */
/* ending backslash at the end of a string.]                          */
/* Any zero character ends the string.                                */
/* If a syntax error is found when parsing the string, the user's     */
/* error handler (given by arg_syntax) is invoked with the best       */
/* approximation of the erroneous part of the string. If it returns   */
/* with arg_continue, then the tokens are processed normally, but with*/
/* all invalid ones replaced by an empty string.                      */
/* If the string argument is a NULL pointer, then the parsing is      */
/* taken to be successful, no error is reported and no help is given. */
/* If the string contains no tokens, then the parse is ok, but no help*/
/* is given.                                                          */ 

arg_parse_result arg_string_parse
  (
    char const *,             /* string to parse                      */
    arg_descriptor const []   /* description of expected arguments    */
  ) ;

/* arg_env_var_parse: Parse the contents of an environment variable.  */
/* The name of an environment variable is passed. This is extracted   */
/* and the string value is parsed as though it were a string with the */
/* same syntactic conventions as arg_string_parse.                    */
/* If the env variable is not present, then the user's error handler  */
/* (given by arg_envvar) is invoked with a copy of the env var name.  */
/* If the name is given as a NULL pointer, then it is assumed that a  */
/* successful parse occurs and no help is invoked.                    */

arg_parse_result arg_env_var_parse
  (
    char const *,             /* name of environment variable         */
    arg_descriptor const []   /* description of expected arguments    */
  ) ;

/* 930422: Added indirect file processing */
/* If an argument is found beginning with the character @, then it is */
/* taken that the rest of the argument is a filename. This is then    */
/* opened in the current directory only - no attempt is made to search*/
/* using ISEARCH. If there is nothing else in the argument but @, or  */
/* the file cannot be opened, it is transferred to the error handling */
/* function as an invalid argument.                                   */
/* The syntax of the contents of an indirect file is:                 */
/*  Each line is self-contained; ie. all arguments must be complete.  */
/*  A line beginning with a hash (#) character is a comment line.     */
/*  Each non-comment line is treated like parsing a character string. */
/*  If there is no newline at the end of file, one is assumed.        */



/* This function returns the configuration version number of the      */
/* library as built.                                                  */

char const * arg_version (void);

