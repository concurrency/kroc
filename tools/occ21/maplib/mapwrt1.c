#define DEBUG

/*
 *	map file writer
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

/*{{{  history */
/* V1.0 xx/xx/92 CON First version */
/* V1.1 03/02/93 CON Added better error checking, and uses tabs */
/* V1.2 08/03/93 CON Put in fix for broken WATCOM compiler. See TS/2089 */
/*}}}*/

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>
/*#include <assert.h>*/
#include <stdlib.h> /* abort */

#include "imstype.h" /* IMPORTED */
#include "imsmisc.h" /* IMPORTED */

#include "maplib.h"
/*}}}*/
/*{{{  structure declarations */
struct map_handle_s
  {
    FILE *         map_file;
    void *         user_data;
    map_section_t *section_list;
    map_section_t *current_section; /* points to most recently created section */
    map_malloc_t   malloc_fn;
    map_free_t     free_fn;
    map_qsort_t    qsort_fn;
  };

typedef struct map_entry_s
  {
    struct map_entry_s *next;
    const char         *name;
    const char         *type;
    const char         *comment;
    INT32               offset;
  } map_entry_t;

struct map_section_s
  {
    map_section_t    *next;
    map_sectiontype_t type;
    map_entry_t      *entries;
    const char       *name;
    int               sub;      /* sub number of name */
    const char       *filename;
    int               line;
    BOOL              bytes;    /* TRUE if bytes, FALSE if words */
    INT32             offset;
    INT32             span;
    INT32             size0;    /* Section size */   /* Also WS */
    INT32             size1;                         /* Also VS */
  };
/*}}}*/

/*{{{  PUBLIC map_handle_t *map_open */
/* open returns NULL if it failed */
PUBLIC map_handle_t *map_open(void *user_data,
       map_malloc_t malloc_fn, map_free_t free_fn, map_qsort_t qsort_fn)
{
  map_handle_t *handle = NULL;
  if (malloc_fn != NULL && free_fn != NULL && qsort_fn != NULL)
    {
      static const map_handle_t initialiser = {0}; /* Initialised to zeroes and NULLs */
      /* we insert an initialiser to make icc put this into the text section;
         otherwise it thinks that it might be a tentative declaration with a
         different initialiser later, which might consist of non-NULL function
         pointers, etc.
      */
      handle = malloc_fn(NULL, sizeof(map_handle_t));
      
      /*printf("\nIn map_open\n");*/
      
      if (handle != NULL)
        {
          *handle = initialiser;
          handle->user_data = user_data;
          handle->malloc_fn = malloc_fn;
          handle->free_fn   = free_fn;
          handle->qsort_fn  = qsort_fn;
        }
    }
  return handle;
}
/*}}}*/
/*{{{  PUBLIC void *map_get_user_data */
PUBLIC void *map_get_user_data(map_handle_t *const handle)
{
  if (handle == NULL)
    return NULL;

  /*printf("\nIn map_get_user_data\n");*/
  return handle->user_data;
}
/*}}}*/
/*{{{  PUBLIC BOOL map_close */
PUBLIC BOOL map_close(map_handle_t **handle_addr, const char *const end_string)
{
  map_handle_t *const handle = *handle_addr;
  /*printf("\nIn map_close\n");*/
  if (handle != NULL)
    {
      map_free_t free_fn = handle->free_fn;

      /*{{{  clear up section list */
      {
        while (handle->section_list != NULL)
          {
            map_section_t *section = handle->section_list;
            map_close_section(handle, &section);
          }
      }
      /*}}}*/
      /*{{{  close file */
      if (handle->map_file != NULL)
        {
          int result;
          if (end_string != NULL)
            fputs(end_string, handle->map_file);
          result = fclose(handle->map_file);
          if (result != 0)
            return FALSE;
        }
      /*}}}*/

      free_fn(NULL, handle, sizeof(map_handle_t));
      *handle_addr = NULL;
    }
  return TRUE;
}
/*}}}*/

/*{{{  PUBLIC BOOL map_open_file */
PUBLIC BOOL map_open_file(map_handle_t *const handle,
     const char *const mapfilename,
     const char *const sourcefilename,
     const char *const proctype, const char *const errmode,
     const char *const compilername)
{
  /*printf("\nIn map_open_file\n");*/
  if (handle != NULL)
    {
      int i, len;

      FILE *const fptr = fopen(mapfilename, "w");
      if (fptr == NULL)
        return FALSE;
      handle->map_file = fptr;

      fprintf(fptr, "Map of code and data for source file %s\n", sourcefilename);
      fprintf(fptr, "=====================================");
      for (i = 0, len = strlen(sourcefilename); i < len; i++)
        fputc('=', fptr);
      fprintf(fptr, "\n\nCreated by %s", compilername);
      fprintf(fptr, "\n\nTarget processor : %s\nError mode       : %s\n",
              proctype, errmode);
    }
  return TRUE;
}
/*}}}*/
/*{{{  PUBLIC void map_blank_line */
PUBLIC void map_blank_line(map_handle_t *const handle)
{
  if (handle != NULL && handle->map_file != NULL)
    fputc('\n', handle->map_file);
}
/*}}}*/

/*{{{  PUBLIC map_section_t *map_open_section */
PUBLIC map_section_t *map_open_section(map_handle_t *const handle,
       const map_sectiontype_t type,
       const char *const name, const int sub_number,
       const char *const filename, const int linenumber,
       const INT32 offset, const INT32 span,
       const BOOL bytes)
{
  map_section_t *section;

  if (handle == NULL)
    return NULL;

  section = handle->malloc_fn(handle, sizeof(map_section_t));
  if (section != NULL)
    {
      section->type           = type;
      section->name           = name;
      section->sub            = sub_number;
      section->filename       = filename;
      section->line           = linenumber;
      section->offset         = offset;
      section->span           = span;
      section->bytes          = bytes;
      section->size0          = 0;
      section->size1          = 0;
      section->entries        = NULL;
      section->next           = handle->section_list;
      handle->section_list    = section;
      handle->current_section = section;
    }
  /*printf("\nIn map_open_section\n");*/
  return section;
}
/*}}}*/
/*{{{  PUBLIC void map_add_entry */
PUBLIC void map_add_entry(map_handle_t *const handle, map_section_t *section,
       const char *const name, const char *const type_str, const INT32 offset,
       const char *const comment)
{
  /*printf("\nIn map_addtosection: %s, %s, %ld\n", name, item, offset);*/

  if (handle == NULL)
    return;

  if (section == NULL)
    section = handle->current_section;

  if (section != NULL)
    {
      map_entry_t *const entry = handle->malloc_fn(handle, sizeof(map_entry_t));
      if (entry != NULL)
        {
          entry->name    = name;
          entry->type    = type_str;
          if ((type_str != NULL) && (strlen(type_str) != MAP_TYPE_STR_LEN))
            abort();
          entry->offset  = offset;
          entry->comment = comment;
          entry->next    = section->entries;
          section->entries = entry;
        }
    }
}
/*}}}*/
/*{{{  PUBLIC void map_set_size */
PUBLIC void map_set_size(map_handle_t *const handle, map_section_t *section,
       const INT32 size0, const INT32 size1)
{
  if (handle == NULL)
    return;

  if (section == NULL)
    section = handle->current_section;

  if (section != NULL)
    {
      section->size0 = size0;
      section->size1 = size1;
    }
}
/*}}}*/

/*{{{  PRIVATE const char *unitsof */
PRIVATE const char *unitsof(const map_section_t *const section)
{
  return section->bytes ? "bytes" : "words";
}
/*}}}*/
/*{{{  PRIVATE void print_list_header */
PRIVATE void print_list_header(FILE *const fptr,
        const map_section_t *const section, const char *const title)
{
  fprintf(fptr, "%-32.32s %s (%s)\n", title, "Offset", unitsof(section));
}
/*}}}*/
/*{{{  PRIVATE void print_varlist_header */
PRIVATE void print_varlist_header(FILE *const fptr, const map_section_t *const section)
{
  print_list_header(fptr, section, "Variable name");
}
/*}}}*/
/*{{{  PRIVATE void print_section_header */
PRIVATE void print_section_header(map_handle_t *const handle,
        map_section_t *const section, const BOOL empty)
{
  FILE *const fptr = handle->map_file;
  switch(section->type)
    {
      /*{{{  proc */
      case map_sectiontype_proc:
        fprintf(fptr, "\nMap of workspace\n----------------\nRoutine : %s\n",
                section->name);
        if (empty)
          fputs("No local variables\n", fptr);
        else
          print_varlist_header(fptr, section);
        break;
      /*}}}*/
      /*{{{  formal */
      case map_sectiontype_formal:
        fputc('\n', fptr);
        if (empty)
          fputs("No formal parameters\n", fptr);
        else
          print_list_header(fptr, section, "Formal parameter name");
        break;
      /*}}}*/
      /*{{{  par */
      case map_sectiontype_par:
        if (!empty)
          {
            fprintf(fptr, "\nBranch %d of PAR in %s at line %d of %s\n",
                    section->sub, section->name, section->line, section->filename);
            fprintf(fptr, "Branch is at offset %d %s\n",
                    section->offset, unitsof(section));
            print_varlist_header(fptr, section);
          }
        break;
      /*}}}*/
      /*{{{  repl */
      case map_sectiontype_repl:
        if (!empty)
          {
            fprintf(fptr, "\nBody of replicated PAR in %s at line %d of %s\n",
                    section->name, section->line, section->filename);
            fprintf(fptr, "Each branch is %d %s, %d copies, first is at offset %d %s\n",
                    section->span, unitsof(section),
                    section->sub, section->offset, unitsof(section));
            print_varlist_header(fptr, section);
          }
        break;
      /*}}}*/
      /*{{{  text */
      case map_sectiontype_text:
        fprintf(fptr, "\nSection map\n-----------\nSection name : %s : size = %d %s\n",
                section->name, section->size0, unitsof(section));
        fprintf(fptr, "%-32.32s %-12.12s %s (%s)\n",
                "Name","Type","Offset", unitsof(section));
        break;
      /*}}}*/
    }
}
/*}}}*/
/*{{{  PRIVATE void print_section_footer */
PRIVATE void print_section_footer(map_handle_t *const handle,
        map_section_t *const section)
{
  FILE *const fptr = handle->map_file;
  switch(section->type)
    {
      case map_sectiontype_formal:
        fprintf(fptr, "\nWorkspace size = %d word%s",
                section->size0, (section->size0 == 1) ? "" : "s");
        if (section->size1 != 0)
          fprintf(fptr, ", Vectorspace size = %d word%s",
                  section->size1, (section->size1 == 1) ? "" : "s");
        fputc('\n', fptr);
        break;
      case map_sectiontype_text:
        /*fprintf(fptr, "\nSection size = %ld %s\n", section->size0,
                unitsof(section));*/
        break;
      case map_sectiontype_proc:
      case map_sectiontype_par:
      case map_sectiontype_repl:
        break;
    }
}
/*}}}*/
/*{{{  PRIVATE void print_name_and_spaces */
PRIVATE void print_name_and_spaces(map_handle_t *const handle,
             const char *const name)
{
  FILE *const fptr = handle->map_file;
#if 0
  /*{{{  use fprintf */
  fprintf(fptr, "%-32.32s ", name);
  /*}}}*/
#else
  /*{{{  use tabs */
  #define STR_WIDTH 32
  #define TAB_STOP   8
  char local_name[STR_WIDTH + 1];
  int tabs;
  int i;
  int len;
  
  strncpy(local_name, name, STR_WIDTH);
  local_name[STR_WIDTH] = '\0';
  fputs(local_name, fptr);
  
  len  = strlen(local_name);
  tabs = (STR_WIDTH / TAB_STOP) - (len / TAB_STOP);
  for (i = 0; i < tabs; i++)
    fputc('\t', fptr);
  fputc(' ', fptr);
  /*}}}*/
#endif
}
/*}}}*/
/*{{{  PRIVATE void print_entry */
PRIVATE void print_entry(map_handle_t *const handle,
        map_section_t *const section, map_entry_t *const entry)
{
  FILE *const fptr = handle->map_file;
  switch(section->type)
    {
      case map_sectiontype_proc:
      case map_sectiontype_formal:
      case map_sectiontype_par:
      case map_sectiontype_repl:
        print_name_and_spaces(handle, entry->name);
        fprintf(fptr, "%10d", entry->offset);
        break;
      case map_sectiontype_text:
        print_name_and_spaces(handle, entry->name);
        fprintf(fptr, "%-12.12s %10d", entry->type, entry->offset);
        break;
    }
  if (entry->comment != NULL)
    {
      fputc(' ', fptr);
      fputs(entry->comment, fptr);
    }
  fputc('\n', fptr);
}
/*}}}*/
/*{{{  PRIVATEPARAM int compare_entries */
PRIVATEPARAM int compare_entries(const void *const v1, const void *const v2)
{
  const map_entry_t *const *const p1 = v1;
  const map_entry_t *const *const p2 = v2;
  const map_entry_t *const e1 = *p1;
  const map_entry_t *const e2 = *p2;
  if (e1->offset < e2->offset)
    return -1;
  else if (e1->offset == e2->offset)
    return 0;
  else
    return 1;
}
/*}}}*/
/*{{{  PRIVATE void process_section */
PRIVATE void process_section(map_handle_t *const handle, map_section_t *const section)
{

  print_section_header(handle, section, section->entries == NULL);

  {
    int count;
    int i;
    map_entry_t **array;
    size_t array_size;
    map_entry_t *entry;

    /*{{{  count entries */
    for (count = 0, entry = section->entries; entry != NULL; entry = entry->next)
      count++;
    /*}}}*/
    /*{{{  sort entries */
    array_size = count * (sizeof(map_entry_t *));
    array      = handle->malloc_fn(handle, array_size);
    
    for (i = 0, entry = section->entries; entry != NULL; entry = entry->next, i++)
      array[i] = entry;
    
    handle->qsort_fn(array, count, sizeof(map_entry_t *),
                     compare_entries);
    /*}}}*/
    /*{{{  print entries */
    for(i = 0; i < count; i++)
      {
        print_entry(handle, section, array[i]);
        handle->free_fn(handle, array[i], sizeof(map_entry_t));
      }
    /*}}}*/
    /*{{{  freeup list */
    handle->free_fn(handle, array, array_size);
    /*}}}*/
  }

  print_section_footer(handle, section);

  section->entries = NULL;
}
/*}}}*/

/*{{{  PRIVATE void remove_section */
PRIVATE void remove_section(map_handle_t *const handle, map_section_t *const section)
{
  map_section_t **last = &handle->section_list;
  while (TRUE)
    {
      map_section_t *const this = *last;
      if (this == section)
        {
          *last = this->next;
          handle->free_fn(handle, this, sizeof(map_section_t));
          return;
        }
      last = &this->next;
    }
}
/*}}}*/
/*{{{  PUBLIC void map_close_section */
PUBLIC void map_close_section(map_handle_t *const handle,
       map_section_t **const section_addr)
{
  map_section_t *section;
  /*printf("\nIn map_close_section\n");*/

  if (handle == NULL)
    return;

  if (section_addr == NULL)
    section = handle->current_section;
  else
    {
      section = *section_addr;
      *section_addr = NULL;
    }

  if (section != NULL)
    {
      process_section(handle, section);
      remove_section(handle, section);
    }

  handle->current_section = NULL;
}
/*}}}*/
