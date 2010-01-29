/* $Id: err1.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	Error reporting
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

/*#define DEBUG*/

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>

#if defined(HAVE_SYS_STAT_H) && defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#include <sys/stat.h>
#endif


#include "popen_re.h"		/* IMPORTED */

#include "midinc.h"
#include "localfe.h"

#include "err2.h"

#include "chkdef.h"		/* current_fe_handle */
#include "chkerror.h"		/* CHK_INVTYPE */

#ifdef TCOV
#include "../backend/generror.h"
#endif

/*}}}  */

/*{{{  global variables */
PUBLIC jmp_buf env;
/*}}}  */
/*{{{  private variables */
PRIVATE BOOL lex_info_provided = FALSE;
PRIVATE int *symb_ptr;
PRIVATE int *symbindent_ptr;
PRIVATE int *baseindent_ptr;	/* bug TS/1437 23/10/91 */

PRIVATE wordnode **lexword_ptr = NULL;
PRIVATE int *literalp_ptr = NULL;
PRIVATE const char *literalv_ptr = NULL;

PRIVATE const char *const expectedstring = "expected %s, found %s";

PRIVATE messagestring_fn_t msgstring_fn[MAX_MODULES];	/* init to NULL */

/*}}}  */
/*{{{  constants */
#define MAX_ERRORS 10

/*This is used to scan the file for errors;
  if lines are longer than this it will display the 'window' around an
  error in the wrong place.
*/
#define ERR_LINEMAX             255
/*}}}  */

/*{{{  PRIVATE void printbuf (file, locn, indent, lines) */
PRIVATE void printbuf (FILE *const file, const SOURCEPOSN locn, const int indent)
{
	int filenum = FileNumOf (locn);
	int fileline = FileLineOf (locn);
	const char *fe_fname = fe_lookupfilename (current_fe_handle, filenum);

	char line[ERR_LINEMAX];
	FILE *const fptr = popen_relative (fe_fname, current_fe_data->fe_pathname,
					   NULL, "r", NULL,
					   memalloc, memfree, NULL);

	if (fptr != NULL) {
		int i, j;

#if defined(HAVE_SYS_STAT_H) && defined(HAVE_SYS_TYPES_H)
		struct stat st_buf;

		if (fstat (fileno (fptr), &st_buf)) {
			fprintf (file, "    *** failed to stat path \"%s\", internal error.\n", fe_fname);
			fprintf (file, "%s", FATAL_INTERNAL_BANNER);
			current_fe_data->fe_abort_fn (current_fe_handle);
			return;
		} else if (!S_ISREG (st_buf.st_mode)) {
			fprintf (file, "    *** not a regular file \"%s\", cannot display.\n", fe_fname);
			return;
		}
#if 0
fprintf (stderr, "printbuf(): filename is [%s]", fe_fname);
#endif
#endif

		for (i = 0; i < fileline - 5; i++) {
			fgets (line, ERR_LINEMAX, fptr);
		}
		j = (fileline < 5) ? fileline : 5;

		for (i = 1; i <= 9 && !feof (fptr); i++) {
			/*{{{  print a line */
			line[0] = '\0';
			fgets (line, ERR_LINEMAX, fptr);
			if (!feof (fptr)) {
				char *s = line;
				int column = 0;
				int nomoretabs = FALSE;
				fprintf (file, (i == j) ? "%4d:" : "    :", fileline);
				if (strchr (line, '\t') == NULL) {
					fputs (line, file);
				} else {
					while (*s != '\0') {
						if (*s == ' ') {
							fputc (' ', file);
						} else if (*s != '\t') {
							fputc (*s, file);
							nomoretabs = TRUE;
						} else if (nomoretabs) {
							fputc (' ', file);
						} else {
							int extra = (TABSIZE - (column % TABSIZE));
							column += (extra - 1);
							while (extra > 0) {
								fputc (' ', file);
								extra--;
							}
						}
						column++;
						s++;
					}
				}
				/* the following added for bug 1020 18/10/90 */
				if (strchr (line, '\n') == NULL) {	/* it was a very long line; keep going! */
					int c;

					while ((c = fgetc (fptr)) != EOF && c != '\n') {
						fputc (c, file);
					}
					fputc ('\n', file);
				}

				if ((i == j) && (indent >= 0)) {
					/*{{{  print pointer to lexeme giving error */
					int k;

					for (k = 0; k < indent + 5; ++k) {
						fputc ('-', file);
					}
					fputs ("^\n", file);
					/*}}}  */
				}
			}
			/*}}}  */
		}
		fclose (fptr);
	}
}

/*}}}  */
/*{{{  PUBLIC SOURCEPOSN err_sourcefileof */
PUBLIC SOURCEPOSN err_sourcefileof (const SOURCEPOSN old_locn)
{
	SOURCEPOSN locn = old_locn;
	if (locn != NOPOSN) {
		int filenum = FileNumOf (locn);
		while (!fe_fileissource (current_fe_handle, filenum)) {
			SetFileLine (locn, fe_parentposnoffile (current_fe_handle, filenum));
			filenum = fe_parentoffile (current_fe_handle, filenum);
			SetFileNum (locn, filenum);
		}
	}
	return locn;
}

/*}}}  */
/*{{{  PRIVATE const char *get_msg */
PRIVATE const char *get_msg (const err_module_t module, const int n)
{
	const char *s;
	switch (module) {
	case ANY_MODULE:
		s = anymessagestring (n);
		break;
	case SYN:
		s = synmessagestring (n);
		break;
	case CHK:
		s = chkmessagestring (n);
		break;
	case USE:
		s = usemessagestring (n);
		break;
	case VTI:
		s = vtimessagestring (n);
		break;
	default:
		if (msgstring_fn[module] == NULL) {
			abort ();
		}
		s = msgstring_fn[module] (n);
		break;
	}
	return (s == NULL) ? "" : s;
}

/*}}}  */
/*{{{  PRIVATE void process_msg */
PRIVATE void process_msg (err_severity_t severity, const err_module_t module, SOURCEPOSN locn, const char *const msg)
{
	int filenum = FileNumOf (locn);
	int fileline = FileLineOf (locn);

	/*printf("process_msg: current_fe_handle is %8X\n", current_fe_handle); */

	if ((module == SYN) && lex_info_provided && (*symb_ptr == S_ILLEGALSYMB) && (severity < SEV_ERR_JMP))
		return;

	if ((severity > SEV_WARN) && current_fe_data->fe_warn_on_all_errors)
		severity = SEV_WARN;	/* Added 14/01/93 by CON */

	if (current_fe_data->fe_outfile != NULL)
		fflush (current_fe_data->fe_outfile);	/* flush any normal output */

	/*{{{  sort out line number etc, and print a source window */
	if ((locn != NOPOSN) && (severity > SEV_WARN) && (module != GEN) && (module != ANY_MODULE)) {
		locn = err_sourcefileof (locn);
		filenum = FileNumOf (locn);
		fileline = FileLineOf (locn);

		if (!current_fe_data->fe_brieferrors)
			/*{{{  print out an extract of the offending file */
		{
			fputc ('\n', current_fe_data->fe_errfile);
			printbuf (current_fe_data->fe_errfile,
				  locn, ((module == SYN) && lex_info_provided) ? (*symbindent_ptr - *baseindent_ptr) : -1);
		}
		/*}}}  */
	}
	/*}}}  */

	/*{{{  print the error message line */
	current_fe_data->fe_error_msg_fn (current_fe_handle,
					  severity, msg, locn == NOPOSN ? NULL : fe_lookupfilename (current_fe_handle, filenum), fileline);
	/*}}}  */

	if (severity > SEV_WARN)
		/*{{{  count number of errors, etc */
	{
		if (current_fe_data->fe_error_occurred != NULL)
			*current_fe_data->fe_error_occurred = TRUE;

		/*{{{  check to see if too many errors */
		{
			const int new_errorcount = fe_get_errorcount (current_fe_handle) + 1;
			fe_set_errorcount (current_fe_handle, new_errorcount);
			if (new_errorcount >= MAX_ERRORS) {
				char string[100];
				sprintf (string, "Too many errors, maximum is %d", MAX_ERRORS);

				current_fe_data->fe_error_msg_fn (current_fe_handle, SEV_FATAL, string, NULL, 0);
				current_fe_data->fe_abort_fn (current_fe_handle);
			}
		}
		/*}}}  */
		if (module == GEN)
			current_fe_data->fe_backenderror_fn ();
	}
	/*}}}  */

	/*{{{  Finish off  */
	if (current_fe_data->fe_crasherrors && (severity >= SEV_ERR)) {
		severity = SEV_INTERNAL;	/* do a core dump (for debugging) */
	}

	if (severity <= SEV_ERR) {
		/* skip */
	} else if (severity <= SEV_SERIOUS) {
		(void) switch_to_real_workspace ();
		longjmp (env, TRUE);
	} else if (severity < SEV_INTERNAL) {
		/*printf("process_msg: current_fe_handle is %8X; jumping!\n", current_fe_handle); */
		current_fe_data->fe_abort_fn (current_fe_handle);
	} else {
		current_fe_data->fe_error_msg_fn (current_fe_handle, SEV_INTERNAL_BANNER, FATAL_INTERNAL_BANNER, NULL, 0);

		if (current_fe_data->fe_outfile != NULL)
			fclose (current_fe_data->fe_outfile);	/* Flush any diagnostics */
		current_fe_data->fe_abort_fn (current_fe_handle);
		/*}}}  */
#if 0				/* do a core dump (for debugging) */
		abort ();
#endif
	}
/*}}}  */
}


/*{{{  PRIVATE void err_print_sub_messages */
PRIVATE void err_print_sub_messages (FILE * const fptr, const err_module_t module)
{
	int i;
	for (i = 0; i < ALL_MAX_MESSAGE_NUMBER; i++) {
		const char *const s = get_msg (module, i);
		if ((s != NULL) && (s[0] != '\0'))
			fprintf (fptr, "%01d %03d %s\n", module, i, s);
	}
}

/*}}}  */
/*{{{  PUBLIC void err_print_messages */
PUBLIC void err_print_messages (FILE * const fptr)
{
	err_module_t module;
	err_print_sub_messages (fptr, ANY_MODULE);
	err_print_sub_messages (fptr, SYN);
	err_print_sub_messages (fptr, CHK);
	err_print_sub_messages (fptr, USE);
	err_print_sub_messages (fptr, VTI);

	for (module = 0; module < MAX_MODULES; module++)
		if (msgstring_fn[module] != (messagestring_fn_t) NULL)
			err_print_sub_messages (fptr, module);
}

/*}}}  */

/*{{{  PRIVATE void call_ftagstring */
PRIVATE void call_ftagstring (char *const string, const int tag)
{
	ftagstring (string, tag, *lexword_ptr, *literalp_ptr, literalv_ptr);
}

/*}}}  */
/*{{{  PUBLIC void msg_out */
PUBLIC void msg_out (const err_severity_t severity, const err_module_t module, const int n, const SOURCEPOSN locn)
{
	process_msg (severity, module, locn, get_msg (module, n));
}

/*}}}  */
/*{{{  PUBLIC void msg_out_c    NEVER USED */
#if 0				/* msg_out_c is never used */
PUBLIC void msg_out_c (const err_severity_t severity, const err_module_t module, const int n, const SOURCEPOSN locn, const char c)
{
	char msg[MAXSTRING_SIZE];
	sprintf (msg, get_msg (module, n), c);
	process_msg (severity, module, locn, msg);
}
#endif /* msg_out_c is never used */
/*}}}  */
/*{{{  PUBLIC void msg_out_i */
PUBLIC void msg_out_i (const err_severity_t severity, const err_module_t module, const int n, const SOURCEPOSN locn, const BIT32 d)
{
	char msg[MAXSTRING_SIZE];
	sprintf (msg, get_msg (module, n), d);
	process_msg (severity, module, locn, msg);
}

/*}}}  */
/*{{{  PUBLIC void msg_out_s*/
PUBLIC void msg_out_s (const err_severity_t severity, const err_module_t module, const int n, const SOURCEPOSN locn, const char *const s)
{
	char msg[MAXSTRING_SIZE];
	sprintf (msg, get_msg (module, n), s);
	process_msg (severity, module, locn, msg);
}

/*}}}  */
/*{{{  PUBLIC void msg_out_ii   NEVER USED */
#if 0				/* msg_out_ii is never used */
PUBLIC void msg_out_ii (const err_severity_t severity, const err_module_t module, const int n, const SOURCEPOSN locn, const BIT32 d1, const BIT32 d2)
{
	char msg[MAXSTRING_SIZE];
	sprintf (msg, get_msg (module, n), d1, d2);
	process_msg (severity, module, locn, msg);
}
#endif /* msg_out_ii is never used */
/*}}}  */
/*{{{  PUBLIC void msg_out_is */
PUBLIC void msg_out_is (const err_severity_t severity, const err_module_t module, const int n, const SOURCEPOSN locn, const BIT32 d, const char *const s)
{
	char msg[MAXSTRING_SIZE];
	sprintf (msg, get_msg (module, n), d, s);
	process_msg (severity, module, locn, msg);
}

/*}}}  */
/*{{{  PUBLIC void msg_out_iis */
PUBLIC void msg_out_iis (const err_severity_t severity, const err_module_t module, const int n, const SOURCEPOSN locn,
	     const BIT32 d1, const BIT32 d2, const char *const s)
{
	char msg[MAXSTRING_SIZE];
	sprintf (msg, get_msg (module, n), d1, d2, s);
	process_msg (severity, module, locn, msg);
}

/*}}}  */
/*{{{  PUBLIC void msg_out_ss */
PUBLIC void msg_out_ss (const err_severity_t severity, const err_module_t module, const int n, const SOURCEPOSN locn, const char *const s1, const char *const s2)
{
	char msg[MAXSTRING_SIZE];
	sprintf (msg, get_msg (module, n), s1, s2);
	process_msg (severity, module, locn, msg);
}

/*}}}  */
/*{{{  PUBLIC void msg_out_e */
PUBLIC void msg_out_e (const err_severity_t severity, const err_module_t module, const int n, const SOURCEPOSN locn, const int tag)
{
	char found[MAXSTRING_SIZE];
	char msg[MAXSTRING_SIZE];
	call_ftagstring (found, tag);
	sprintf (msg, expectedstring, get_msg (module, n), found);
	process_msg (severity, module, locn, msg);
}

/*}}}  */
/*{{{  PUBLIC void msg_out_synetoken */
PUBLIC void msg_out_synetoken (const int symb, const SOURCEPOSN locn, const int token)
{
	/* This performs the error for SYN_E_TOKEN */
	if (symb != S_ILLEGALSYMB) {
		char expected[MAXSTRING_SIZE], found[MAXSTRING_SIZE];
		char msg[MAXSTRING_SIZE];
		call_ftagstring (expected, token);
		call_ftagstring (found, symb);
		sprintf (msg, expectedstring, expected, found);
		process_msg (SEV_ERR, SYN, locn, msg);
	}
}

/*}}}  */
/*{{{  PUBLIC void chk_invtype */
PUBLIC void chk_invtype (const SOURCEPOSN locn, const int operator_tag)
{
	char operator[MAXSTRING_SIZE];
	call_ftagstring (operator, operator_tag);
/*chkreport_s(CHK_INVTYPE, locn, operator);*/
	msg_out_s (SEV_ERR, CHK, CHK_INVTYPE, locn, operator);	/* bug 1279 22/8/91 */
}

/*}}}  */

/*{{{  PUBLIC void init_lex_error */
PUBLIC void init_lex_error (int *const init_symb_ptr,
		int *const init_symbindent_ptr,
		int *const init_baseindent_ptr, wordnode ** const init_lexword_ptr, int *const init_literalp_ptr, const char *const init_literalv_ptr)
{
	symb_ptr = init_symb_ptr;
	symbindent_ptr = init_symbindent_ptr;
	baseindent_ptr = init_baseindent_ptr;	/* bug TS/1437 23/10/91 */

	lexword_ptr = init_lexword_ptr;
	literalp_ptr = init_literalp_ptr;
	literalv_ptr = init_literalv_ptr;

	lex_info_provided = TRUE;
}

/*}}}  */
/*{{{  PUBLIC void err_install_messages */
PUBLIC void err_install_messages (fe_handle_t * const fe_handle, const err_module_t class, messagestring_fn_t fn)
{
	USE_VAR (fe_handle);	/* avoid unused var warning */

	/* cast 'class' to int here, to prevent a gcc warning about comparison
	   of unsigned enum type < 0
	 */
	if (((int) class < 0) || (class >= MAX_MODULES)) {
		abort ();
	}

	msgstring_fn[class] = fn;
}

/*}}}  */

/*{{{  PUBLIC BOOL err_memo_err */
PUBLIC BOOL err_memo_err (fe_handle_t * const fe_handle, const err_module_t class, const int err, const SOURCEPOSN locn)
{
	/* remembers which errors have already been reported.
	   Returns TRUE if ok to generate it now, ie not already reported
	 */
	memo_t *ptr;

	for (ptr = fe_handle->memo_errs; ptr != NULL; ptr = ptr->next)
		if (ptr->class == class && ptr->err == err && ptr->locn == locn)
			return FALSE;

	ptr = memalloc (sizeof (memo_t));
	ptr->class = class;
	ptr->err = err;
	ptr->locn = locn;
	ptr->next = fe_handle->memo_errs;
	fe_handle->memo_errs = ptr;
	return TRUE;
}

/*}}}  */

/*{{{  PUBLIC void err_free_data */
PUBLIC void err_free_data (fe_handle_t * const fe_handle)
{
	/* the only data which needs freeing up is the memo error stuff */
	memo_t *ptr = fe_handle->memo_errs;
	while (ptr != NULL) {
		memo_t *temp = ptr;
		ptr = ptr->next;
		memfree (temp);
	}
	fe_handle->memo_errs = NULL;
}

/*}}}  */
