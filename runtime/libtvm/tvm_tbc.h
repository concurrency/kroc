/*
tvm - tvm_tbc.h
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2008 Carl G. Ritson

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef TVM_TBC_H
#define TVM_TBC_H

#include "tvm.h"

/*{{{  TEncode Structures */
typedef struct _tenc_element_t {
	char		id[5];
	UWORD		length;
	union {
		WORD	s_int;
		UWORD	u_int;
		char	*str;
		BYTE	*bytes;
	} data;
	BYTE		*next;
} tenc_element_t;

typedef struct _tenc_str_t tenc_str_t;
struct _tenc_str_t {
	tenc_str_t	*next;
	char		*str;
};
/*}}}*/

/*{{{  TBC Structures */
typedef struct _tbc_tlp_t {
	char		*fmt;
	char		*desc;
} tbc_tlp_t;

typedef struct _tbc_ffi_entry_t {
	char	 	*symbol;
	char		*library;
} tbc_ffi_entry_t;

typedef struct _tbc_ffi_t {
	tenc_str_t 	*libraries;
	int		n_symbols;
	tenc_str_t	*symbols;
	tbc_ffi_entry_t	*map;
} tbc_ffi_t;

typedef struct _tbc_lni_entry_t tbc_lni_entry_t;
struct _tbc_lni_entry_t {
	unsigned int 		offset;
	unsigned int		file;
	unsigned int		line;
};

typedef struct _tbc_lni_t {
	tenc_str_t	*files;
	int		n_entries;
	tbc_lni_entry_t	*entries;
} tbc_lni_t;

typedef struct _tbc_t {
	/* Must fit in 10 words: */
	unsigned int	endian;		/* 1 */
	unsigned int	ws;		/* 2 */
	unsigned int	vs;		/* 3 */
	unsigned int	ms;		/* 4 */
	
	unsigned int	bytecode_len;	/* 5 */
	BYTE		*bytecode;	/* 6 */

	tbc_tlp_t	*tlp;		/* 7 */

	tbc_ffi_t	*ffi;		/* 8 */
	tbc_lni_t	*lni;		/* 9 */
} tbc_t;
/*}}}*/

/*{{{  TBC decoder interface */
extern int tbc_decode (BYTE *data, int length, tbc_t **ptr);
/*}}}*/

#endif /* TVM_TBC_H */

