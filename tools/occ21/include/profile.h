/*
 *	profiling information held in TCOFF object files
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


#ifndef __PROFFILE_H
#define __PROFFILE_H
/*{{{ profiling information held in TCOFF object files. */
/* Profiling information is held in a TCOFF object file as a separate
   section with the name PROFILE_SECTION_NAME */

#define PROFILE_SECTION_NAME "profile%1"

/* The format of the profile section within a single TCOFF object file is:

    ProfileSecion :==    Header { Entry } StringPool

   where 

     Header :== Version SectionSize Contents SectionName SectionOffset
                Priority NumEntries EntrySize

     Entry  :== PROFILE_LINE_COUNT         FileName       LineNumber     |
                PROFILE_BRANCH_COUNT       FileName       LineNumber     |
                PROFILE_ROUTINE_COUNT      RoutineAddress Padding        |
                PROFILE_CALL_COUNT         CallerAddress  CalleeAddress  |
                PROFILE_INLINED_CALL_COUNT CallerAddress  CalleeAddress  

     StringPool :== StringPoolHeader StringPoolContents

     StringPoolHeader :== StringPoolSize

  Within a linked unit, the profile sections for each of the constituent
  object files are concatenated together.  The debug comment describes
  where each object file's profile section appears in the concatenation.

  The fields within each part of the profile section are described below.

 */
                
/* Version is a 32-bit number which identifies the format of the profiling
   section.  It has the following fields ... */

#define PROFTAB_VERSION 1
#define PROFTAB_CODE_OFFSETS_REL_TO_END 0x100

/* SectionSize is the size in bytes of the profile section.
   It is calculated as
        PROFTAB_HEADER_SIZE + (NumEntries * EntrySize) +
        PROFTAB_STRPOOL_EXTRA + StringPoolSize
 */
       
/* The size, in bytes, of the profile section header */
#define PROFTAB_HEADER_SIZE 32

/* Contents is a 32-bit pattern describing the type of information in the
   profiling table. It is made up of one or more of the following: */
/* Entries for the start of each basic block */
#define PROFTAB_LINE_INFO    1
/* Entries describing which way branches goes */
#define PROFTAB_BRANCH_INFO  2
/* Entries for when a routine is entered */
#define PROFTAB_ROUTINE_INFO 4
/* Entries for each routine call */
#define PROFTAB_CALL_INFO    8

/* SectionName is a 32-bit number giving the offset into the string pool
   of the name of the section containing the run-time count table.
   SectionOffset is a 32-bit offset from the start of _this module's_
   part of that section to where the run-time count table actually is. */

/* Priority is 32-bit number describing the priority of the code which is
   being counted by this profile table.  It may take the values ... */
#define PROFTAB_PRIORITY_LOW  1
#define PROFTAB_PRIORITY_HIGH 2
#define PROFTAB_PRIORITY_ALL  (PROFTAB_PRIORITY_LOW | PROFTAB_PRIORITY_HIGH)

/* NumEntries is a 32-bit number which gives the number of entries in the
   profile table. */

/* EntrySize is a 32-bit number which gives the size, in bytes, of each
   entry in the table. It may take the value ... */
#define PROFTAB_ENTRY_SIZE 12
#define PROFTAB_INT32_SIZE 4
/* Each entry in the profile table has a 32-bit tag, which may take one of
   the following values.  The value of the tag determines which fields
   follow it. */
#define PROFILE_LINE_COUNT          1
#define PROFILE_BRANCH_COUNT        2
#define PROFILE_ROUTINE_COUNT       3
#define PROFILE_CALL_COUNT          4
#define PROFILE_INLINED_CALL_COUNT  5

/* FileName is a 32-bit number which is the offset into the string-pool of
   the name of the file which contains the line of source corresponding to
   this count. */

/* LineNumber is a 32-bit number which is the line number within the
   source file, of the line of source corresponding to this count. */
   
/* RoutineAddress is a 32-bit offset from the start of this file's
   text, to the address of the routine which is being counted. */

/* Padding is four bytes worth of padding. */

/* CallerAddress is a 32-bit offset from the start of this file's text, to
   the address of the routine performing the call. */
/* CalleeAddress is a 32-bit offset from the start of this file's text, to
   the address of the routine being called. */

/* The size, in bytes, of the string pool header */
#define PROFTAB_STRPOOL_EXTRA 4

/* StringPoolSize is a 32-bit number giving the number of bytes in the
   StringPoolImage */

/* StringPoolImage is a byte array (of length StringPoolSize), which
   represents the contents of the string pool.  Strings are represented
   as offsets into this array; within the array they are nul-terminated. */
#define pad_to_mcword(len) (((len) + ((INT32)bytesperword - 1)) & \
                                      ~((INT32)bytesperword - 1))
#define ROUTINE_INFO_HASH_TABLE_SIZE 256
/*}}}*/

#define IGNORE(v) (v=v)  /* for silencing compiler moans about unused args */

/*{{{  typedef ProfTabEntryTag */
typedef enum { ProfLineCount, ProfRoutineCount, ProfCallCount} ProfTabEntryTag;
/*}}}*/
/*{{{  typedef StrPool */
typedef INT32 StrPoolI;
typedef struct StrPool
  {
    StrPoolI image_len;
    StrPoolI image_allocated_size;
    StrPoolI amount_to_increase_image_allocation_by;
    char *image;
    void *(*alloc_fn)(size_t size, void *user_data);
    void (*free_fn)(void *block, size_t size, void *user_data);
    void *user_data;
  } StrPool;
typedef struct StrPool *StrPoolP;
/*}}}*/
/*{{{  typedef RoutineInfoEntry */
typedef struct RoutineInfoEntry
{
  treenode *nptr;
  INT32  address;
  struct RoutineInfoEntry *next;
  
}RoutineInfoEntry;
/*}}}*/
/*{{{  typedef ProfTabEntry */
typedef struct ProfTabEntry
  {
    struct ProfTabEntry *next;
    /* The entry type */
    ProfTabEntryTag tag;
    INT32 entry_number;
    treenode *tptr;
    union
      {
        /* This variant is for ProfLineCount and ProfBranchCount */
        struct
          {
            /* Source file containing the code being counted by this count;
               0 means no source file available */
            StrPoolI file_name;
              
            /* Line number containing the code being counted by this count;
               0 means no line number available */
            INT32 line_number;
          } source_pos;
        /* This variant is for ProfCallCount and ProfInlinedCallCount */
        struct
         {
	   treenode *calling_nptr;
         } call_count_info;

      } u;
  } ProfTabEntry;
/*}}}*/
#define proftab_next_(p)            ((p)->next)
#define proftab_tag_(p)             ((p)->tag)
#define proftab_entry_number_(p)    ((p)->entry_number)
#define proftab_tptr_(p)            ((p)->tptr)
#define proftab_file_name_(p)       ((p)->u.source_pos.file_name)
#define proftab_line_number_(p)     ((p)->u.source_pos.line_number)
#define proftab_calling_nptr_(p)    ((p)->u.call_count_info.calling_nptr)

/*{{{  typedef ProfTabContents */
/* ProfTabContents defines the contents of a ProfTab.  It is not really an
   enumeration, but rather a set of bits which may be or'd together. */
typedef enum
  {
    ProfLineInfo    = 1,
    ProfBranchInfo  = 2,
    ProfRoutineInfo = 4,
    ProfCallInfo    = 8
  } ProfTabContents;
/*}}}*/

/*{{{  typedef ProfTab */
typedef struct ProfTab
  {
    /* Allocation routine for ProfTab data structures */
    void *(*alloc_fn)(size_t size, void *user_data);
    void (*free_fn)(void *block, size_t size, void *user_data);

    /* General data pointer which is passed in to allocation routines */
    void *user_data;

    /* Contains a set of bits which indicate what type of profiling
         information is in the section,
         eg. ... */
    ProfTabContents contents;

    /* Number of entries in the count table */
    INT32 count_table_size;

    /* Head and tail of linked list of entries in the count table */
    ProfTabEntry *entry_list_head, *entry_list_tail;

    /* String pool: all StrPoolI variables in the ProfTab data structure
       represent positions in this pool */
    StrPoolP strpool;
    RoutineInfoEntry   **rout_hash_table;
  } ProfTab;
/*}}}*/

extern ProfTab *profile_table;
extern BOOL sampling_profiling;
extern BOOL line_profiling;
extern BOOL cgraph_profiling;
extern BOOL trace_prof_image;
extern INT32 profile_count_table_label;


extern StrPoolI strpool_add(StrPoolP pool, const char *string);
extern StrPoolI strpool_size(StrPoolP pool);
extern const char *strpool_image(StrPoolP pool);

extern BOOL valid_proftable_index(ProfTab *table, INT32 index);
extern ProfTabEntry *get_proftab_entry(ProfTab *prof_table, treenode *tptr);
extern treenode *add_profcountupd_nd(ProfTabEntry *profcount, treenode *tptr);
extern  ProfTab * proftab_create(ProfTabContents contents,
                         void *alloc_fn(size_t size, void *user_data),
                         void free_fn(void *block , size_t size,
                                      void *user_data),
                         void *user_data); 
extern  ProfTabEntry *proftab_add_call_count(ProfTab * table, treenode *tptr);
extern ProfTabEntry *proftab_add_routine_count(ProfTab * table,treenode *tptr);
extern ProfTabEntry *proftab_add_line_count(ProfTab * table, const char *name,
					    INT32 line, treenode *tptr);
extern treenode *add_profcountupd_nd(ProfTabEntry *profcount, treenode *tptr);
extern void *profi_memalloc(size_t s, void *data);
extern void profi_memfree(void *p, size_t s, void *data); 
extern void set_proftab_routine_address(ProfTab *table, treenode *nptr, 
					INT32 address);
extern INT32 get_proftab_routine_address(ProfTab *table, treenode *nptr);
extern void write_profile_section(ProfTab *prof_table);
#endif
      











