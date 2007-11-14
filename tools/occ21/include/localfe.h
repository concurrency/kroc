/* $Id: localfe.h,v 1.1 1996/04/15 10:52:13 djb1 Exp $ */

/*
 *	local frontend data
 *	Copyright (C) 1992 Inmos Limited
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

/*{{{  data structures */
/*{{{  memo_t */
/* Use to remember errors/warnings which have already been generated,
   so that we can prevent duplications.
*/
typedef struct memo_s
  {
    struct memo_s *next;
    int class;
    int err;
    SOURCEPOSN locn;
  } memo_t;
/*}}}*/

/*{{{  struct filetablestruct */
/* This is used to keep details of the files which are read as part
   of this parse.
*/

typedef struct filetablestruct
  {
    /* I've changed this to be a linked list rather than an array */
    struct filetablestruct *ftnext;
    wordnode *ftname;
    BOOL      ftissource;
    int       ftparent;
    int       ftparentline;
  } filetablestruct_t;
/*}}}*/

/*{{{  fe_handle_s data structure */
/* This structure will be initialised by default to the 'static'
   values; ie ptr to NULLs and ints to zero.
*/
struct fe_handle_s
  {
    treenode            *tree;
    int                  lines;      /* Number of lines in the source file */
    filetablestruct_t   *filenames;  /* Details of all filenames etc */
    int                  numfiles;
    fe_translate_data_t *translates; /* Details of all object file symbol translations */
    BIT32                sourcehash; /* Hash function on whole of source */
  /*treenode            *hwhostname;*//*Pointer to predefined HOST edge in configurer */
    treenode *predefname[fe_max_predefnames]; /* Pointers to predefined names */
    int                  errorcount; /* Number of errors */
    const fe_data_t     *data_ptr;   /* pointer to fe_data */
    void                *user_ptr;   /* Users can attach their own data here */
    jmp_buf             *env_addr;   /* Address of error function's jmp_buf */
    memo_t              *memo_errs;  /* List of previously generated errors */
  };
/* this is typedef-ed to 'fe_handle_t' in occamfe.h */
/*}}}*/

/*}}}*/

