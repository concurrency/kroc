/*
 *	tcoff.h - TCOFF constants
 *	Copyright (C) 2000 Fred Barnes <frmb@kent.ac.uk>
 *	Based on tcoff.inc Copyright (C) 1997 M D Poole
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

#ifndef __TCOFF_H
#define __TCOFF_H

#define TCOFF_IGNORE_TAG	0
#define LINKABLE_TAG		1
#define START_MODULE_TAG	2
#define END_MODULE_TAG		3
#define SET_LOAD_POINT_TAG	4
#define ADJUST_POINT_TAG	5
#define LOAD_TEXT_TAG		6
#define LOAD_PREFIX_TAG		7
#define LOAD_EXPR_TAG		8
#define LOAD_ZEROS_TAG		9
#define ALIGN_TAG		10
#define SELECTION_TAG		11
#define DEFINE_MAIN_TAG		12
#define LOCAL_SYMBOLS_TAG	13
#define DEFINE_LABEL_TAG	14
#define DEFINE_SYMBOL_TAG	15
#define KILL_ID_TAG		16
#define BYTE_PATCH_TAG		17
#define REP_START_TAG		18
#define REP_END_TAG		19
#define COMMENT_TAG		20
#define MESSAGE_TAG		21
#define LIB_INDEX_START_TAG	22
#define LIB_INDEX_END_TAG	23
#define INDEX_ENTRY_TAG		24
#define WORD_PATCH_TAG		25
#define DESCRIPTOR_TAG		26
#define VERSION_TAG		27
#define LINKED_UNIT_TAG		28
#define SPECIFY_ORIGIN_TAG	29
#define SYMBOL_TAG		30
#define SPECIFIC_SYMBOL_TAG	31

#define TCOFF_MAX_TAG		31

#define TCOFF_INVALID_TAG	32

#ifdef WANT_TCOFF_TAGNAMES
	static char *tcoff_tag_names[] = {
		"IGNORE", "LINKABLE", "START.MODULE", "END.MODULE", "SET.LOAD.POINT",
		"ADJUST.POINT", "LOAD.TEXT", "LOAD.PREFIX", "LOAD.EXPR", "LOAD.ZEROS",
		"ALIGN", "SELECTION", "DEFINE.MAIN", "LOCAL.SYMBOLS", "DEFINE.LABEL",
		"DEFINE.SYMBOL", "KILL.ID", "BYTE.PATCH", "REP.START", "REP.END",
		"COMMENT", "MESSAGE", "LIB.INDEX.START", "LIB.INDEX.END", "INDEX.ENTRY",
		"WORD.PATCH", "DESCRIPTOR", "VERSION", "LINKED.UNIT", "SPECIFY.ORIGIN",
		"SYMBOL", "SPECIFIC.SYMBOL", "<invalid>"};
#endif

#endif	/* !__TCOFF_H */

