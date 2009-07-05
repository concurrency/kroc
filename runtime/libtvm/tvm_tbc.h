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
	char		*symbol;
} tbc_tlp_t;

typedef struct _tbc_ffi_entry_t {
	char	 	*symbol;
	int		library;
} tbc_ffi_entry_t;

typedef struct _tbc_ffi_t {
	tenc_str_t 	*libraries;
	int		n_symbols;
	tenc_str_t	*symbols;
	tbc_ffi_entry_t	*map;
} tbc_ffi_t;

typedef struct _tbc_sym_t tbc_sym_t;
struct _tbc_sym_t {
	unsigned int		offset;
	unsigned int		ws;
	unsigned int		vs;
	char			*name;
	char			*definition;
	tbc_sym_t		*next;
};

typedef struct _tbc_lnd_t tbc_lnd_t;
struct _tbc_lnd_t {
	unsigned int 		offset;
	unsigned int		file;
	unsigned int		line;
};

typedef struct _tbc_dbg_t {
	tenc_str_t	*files;
	int		n_lnd;
	tbc_lnd_t	*lnd;
} tbc_dbg_t;

typedef struct _tbc_t {
	/* Must fit in 10 words: */
	unsigned int	endian;		/* 1 */
	unsigned int	ws;		/* 2 */
	unsigned int	vs;		/* 3 */
	
	unsigned int	bytecode_len;	/* 4 */
	BYTE		*bytecode;	/* 5 */

	tbc_tlp_t	*tlp;		/* 6 */
	tbc_ffi_t	*ffi;		/* 7 */
	tbc_sym_t	*symbols;	/* 8 */
	tbc_dbg_t	*debug;		/* 9 */
} tbc_t;
/*}}}*/

/*{{{  TBC decoder interface */
extern WORD tenc_decode_int (BYTE *src);
extern int tenc_decode_element (BYTE *src, unsigned int *length, tenc_element_t *element);
extern int tenc_walk_to_element (BYTE *data, unsigned int *length, const char *id, tenc_element_t *element);
extern int tbc_debug_file_and_line (BYTE *data, unsigned int length, unsigned int offset, char **file, unsigned int *line);
extern int tbc_decode (BYTE *data, unsigned int length, tbc_t **ptr);
/*}}}*/

#endif /* TVM_TBC_H */

