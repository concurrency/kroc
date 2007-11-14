/* $Id: proflib.h,v 1.1 1996/04/15 10:52:19 djb1 Exp $ */

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

#ifndef	__PROFLIB_H
#define	__PROFLIB_H

/* Profiling information is held in a TCOFF object file as a separate
   section with the name PROFILE_SECTION_NAME */

#define PROFILE_SECTION_NAME "profile%1"

/* The format of the profile section within a single TCOFF object file is:

    ProfileSecion :==    Header { Entry } StringPool

   where 

     Header :== Version HeaderSize SectionSize Contents SectionName
                SectionOffset Priority NumEntries EntrySize RootFileName

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

/* HeaderSize is the size, in bytes, of the profile section header */
#define PROFTAB_HEADER_SIZE 40

/* SectionSize is the size in bytes of the profile section.
   It is calculated as
        PROFTAB_HEADER_SIZE + (NumEntries * EntrySize) +
        PROFTAB_STRPOOL_EXTRA + StringPoolSize
 */
       

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

/* RootFileName is a 32-bit number giving the offset into the string pool
   of the name of the source file from which the object file containing the
   profile section was derived. */

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

#endif /* ! __PROFLIB_H */
