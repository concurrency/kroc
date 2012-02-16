/*
 *	LFF definitions
 *	Copyright (C) 1990 Inmos Limited
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

/* Copyright 1990 INMOS Limited */
/* CMSIDENTIFIER
   PLAY:LFF_H@509.AAAA-FILE;1(20-FEB-92)[FROZEN] */

/******************************************************************************
*
*  LFF header tags
*
******************************************************************************/
#define LFF_T212 2L
#define LFF_T414 4L
#define LFF_T425 9L
#define LFF_T800 8L
#define LFF_TA 10L
#define LFF_TB 11L
#define LFF_TC 12L

#define LFF_UNDEFINED      0L
#define LFF_HALT           1L
#define LFF_STOP           2L
#define LFF_UNIVERSAL      3L

#define LFF_CODEFIX        37L
#define LFF_WORKSPACEFIX   48L
#define LFF_VECTORSPACEFIX 50L
#define LFF_DATAFIX        38L
#define LFF_STATICFIX      39L
#define LFF_MODNUM         44L
#define LFF_LIMIT          40L
#define LFF_INIT           41L
#define LFF_MAININIT       43L

/* #define LFF_INSTRUCTION    0..15 */
#define LFF_LONG           16L
#define LFF_WORD           17L
#define LFF_LONGADJ        18L
#define LFF_STATIC         35L
#define LFF_COMMON         36L
#define LFF_CODESYMB       45L
#define LFF_DATASYMB       33L
#define LFF_NEWID          (-39L)
#define LFF_TOTALCODE      (-10L)
#define LFF_SC             (-14L)
#define LFF_ENTRY          (-12L)
#define LFF_ID             (-16L)
#define LFF_NEWENTRY       (-31L)
#define LFF_DESC           (-8L)
#define LFF_COMMENT        (-17L)
#define LFF_LIBRARY        (-30L)
#define LFF_ENTRYSYMB      47L
#define LFF_NEWENTRYSYMB   49L
#define LFF_REF            34L
#define LFF_NEXTMODULE     46L
#define LFF_CODE           (-11L)
#define LFF_DEBUG          (-13L)
#define LFF_ADDRESS        (-15L)

