/* $Id: errhdr.h,v 1.1 1996/04/15 10:52:03 djb1 Exp $ */

/*
 *	error reporting definitions
 *	Copyright (C) 1987 Inmos Limited
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


/*{{{  err_module_t */
/* Conceptually all these could be re-ordered, however this would mess
   up the usage of the 'ZPRM' command line switch, so don't re-order them!
*/
typedef enum {
  NO_MODULE,
  ANY_MODULE,
  SYN,
  CHK,
  USE,
  VTI,
  CONF,
  GEN,
  TRANS,
  MAX_MODULES
  } err_module_t;
/*}}}*/

/*{{{  err_severity_t */
typedef enum err_severity_e {
  SEV_NONE,
  SEV_INFO_NOTERR,      /* Info ...  on stdout, or normal output file */
  SEV_INFO,             /* Info ...  on stderr */
  SEV_PEDANTIC,         /* Warning... */
  SEV_WARN,             /* Warning... */
  SEV_ERR,              /* Error..., but returns ok */
  SEV_ERR_JMP,          /* Error..., but does a longjmp to 'env' */
  SEV_SERIOUS,          /* Serious..., but does a longjmp to 'env' */
  SEV_FATAL,            /* Serious..., calls abort_fn,  used to do longjmp */
  SEV_ABORT,            /* Serious..., calls abort_fn */
  SEV_FFATAL,           /* Fatal..., calls abort_fn */
  SEV_INTERNAL,         /* Fatal..., assert(FALSE); */
  SEV_INTERNAL_BANNER
  } err_severity_t;
/*}}}*/

extern jmp_buf env; /* used to recover from errors inside the checker */

/*{{{  msg_out functions */
void msg_out    (err_severity_t severity, err_module_t module, int errnum, SOURCEPOSN locn);
/*void msg_out_c  (err_severity_t severity, err_module_t module, int errnum, SOURCEPOSN locn,
                      char c);*/ /* never used */
void msg_out_i  (err_severity_t severity, err_module_t module, int errnum, SOURCEPOSN locn,
                      BIT32 d);
void msg_out_s  (err_severity_t severity, err_module_t module, int errnum, SOURCEPOSN locn,
                      const char *s);
/*void msg_out_ii (err_severity_t severity, err_module_t module, int errnum, SOURCEPOSN locn,
                      BIT32 d1, BIT32 d2);*/ /* never used */
void msg_out_is (err_severity_t severity, err_module_t module, int errnum, SOURCEPOSN locn,
                      BIT32 d, const char *s);
void msg_out_iis(err_severity_t severity, err_module_t module, int errnum, SOURCEPOSN locn,
                 BIT32 d1, BIT32 d2, const char *s);
void msg_out_ss (err_severity_t severity, err_module_t module, int errnum, SOURCEPOSN locn,
                      const char *s1, const char *s2);
void msg_out_e  (err_severity_t severity, err_module_t module, int errnum, SOURCEPOSN locn,
                      int tag);
/*}}}*/

/*{{{  miscellanous routines */
SOURCEPOSN err_sourcefileof(SOURCEPOSN locn);

void chk_invtype (SOURCEPOSN locn, int tag);

void msg_out_synetoken (int symb, SOURCEPOSN locn, int token);

void init_lex_error(int *symb_ptr, int *symbindent_ptr, int *baseindent_ptr,
       wordnode **lexword_ptr, int *literalp_ptr, const char *literalv_ptr);
typedef const char *(*messagestring_fn_t)(const int n);
struct fe_handle_s; /* forward declaration */
void err_install_messages(struct fe_handle_s *fe_handle, err_module_t class, messagestring_fn_t fn);
BOOL err_memo_err(struct fe_handle_s *fe_handle, err_module_t class, int err, SOURCEPOSN locn);
void err_free_data(struct fe_handle_s *fe_handle);

/* print list of all messages */
void err_print_messages(FILE *fptr);
/*}}}*/

/*{{{  error stuff for all modules */
#define ALL_MAX_MESSAGE_NUMBER 1000

/* Error messages for all modules */
#define ANY_BAD_TAG                 1
#define ANY_OBJFILE_WRITE_ERROR     2
#define ANY_OUT_OF_SPACE            3
#define ANY_FATAL_ABORT             4
#define ANY_FILEINFO                5

#define badtag(L,T,R)      msg_out_ss(SEV_INTERNAL,ANY_MODULE,ANY_BAD_TAG,(L),itagstring((T)),(R))
#define err_abort(R)       msg_out_s (SEV_INTERNAL,ANY_MODULE,ANY_FATAL_ABORT,NOPOSN,(R))
/*}}}*/

#define MAX_ERR_SIZE 256    /* same as MAXSTRING_SIZE */
#define FATAL_INTERNAL_BANNER "\n\
*********************************************************************\n\
* The compiler has detected an internal inconsistency.              *\n\
* Please email kroc-bugs@kent.ac.uk with a copy of the code which   *\n\
* broke the compiler, as this is probably a compiler bug.           *\n\
* Please include the KRoC version and any other relevant details.   *\n\
*********************************************************************\n\
\n"

