/*
 *	map file writer external interface
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

/*{{{  typedefs */
typedef struct map_handle_s map_handle_t;

/* We export struct map_handle_s too, so that, for instance, another
header file can declare a handle without having to import this header file
simply by using an incomplete struct type as follows:
  extern struct map_handle_s *my_handle;

If we want to pass this as a formal parameter, we use an incomplete
struct definition:
  struct map_handle_s;
  void my_function(struct map_handle_s *handle);
*/

typedef struct map_section_s map_section_t;

typedef void *(*map_malloc_t)(map_handle_t *, size_t);
typedef void (*map_free_t)(map_handle_t *, void *, size_t);
typedef void (*map_qsort_t)(void *, size_t, size_t, int (*)(const void *, const void *));

typedef enum
  {
    map_sectiontype_proc,
    map_sectiontype_formal,
    map_sectiontype_par,
    map_sectiontype_repl,
    map_sectiontype_text
  } map_sectiontype_t;

/*}}}*/

/*{{{  definitions */
#define MAP_TYPE_STR_LEN 12 /* Length of type string in section table */
/*}}}*/

/*{{{  routines */
/* open returns NULL if it failed */
map_handle_t *map_open(void *user_data,
       map_malloc_t malloc_fn, map_free_t free_fn,
       map_qsort_t qsort_fn);

/* All other routines behave as noop if given a NULL handle */

BOOL map_close(map_handle_t ** handle_addr, const char * const endstring);
/* returns TRUE if closed ok */

void *map_get_user_data(map_handle_t * const handle);
/* returns NULL if handle is NULL */

BOOL map_open_file(map_handle_t * const handle,
     const char * const mapfilename,
     const char * const sourcefilename,
     const char * const proctype,
     const char * const errmode,
     const char * const compilername);
/* returns TRUE if opened file ok */

/* must only be called after map_open_file */
void map_blank_line(map_handle_t * const handle);

map_section_t *map_open_section(map_handle_t *const handle,
       const map_sectiontype_t type,
       const char *const name, const int sub_number,
       const char *const filename, const int linenumber,
       const INT32 offset, const INT32 span,
       const BOOL bytes);
/*
Eg: PROC,   procname, 0,      NULL,     0,    0,      0,    FALSE;
    FORMAL, NULL,     0,      NULL,     0,    0,      0,    FALSE;
    PAR,    procname, branch, filename, line, offset, 0,    FALSE;
    REPL,   procname, 0,      filename, line, offset, span, FALSE;
    TEXT,   name,     0,      NULL,     0,    0,      0,    TRUE;
*/

void map_add_entry(map_handle_t *const handle, map_section_t *section,
       const char *const name, const char *const type_str, const INT32 offset,
       const char *const comment);
/* type_str must either be NULL, or the correct length (MAP_TYPE_STR_LEN) */
/* if comment is not NULL, it is added at the end of the relevant line */


/* Section size should be set before closing the section */
void map_set_size(map_handle_t *const handle, map_section_t *section,
       const INT32 size0, const INT32 size1);
/* these values are only used for types 'formal' and 'text' */
/*
Eg: FORMAL: ws, vs;
    TEXT:   size, 0;
*/

void map_close_section(map_handle_t *const handle, map_section_t **const section_addr);
/*}}}*/

/*{{{  side-effects pragmas */
#ifdef _ICC
#pragma IMS_nosideeffects (map_get_user_data)
#endif
/*}}}*/
