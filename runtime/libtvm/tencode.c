/*
tvm - tencode.c
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

#include "tvm.h"

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

static WORD decode_int (BYTE *src)
{
	WORD value = *((WORD *) src);
	#if defined (TVM_BIG_ENDIAN)
	return value;
	#elif TVM_WORD_LENGTH == 4
	return SwapFourBytes (value);
	#elif TVM_WORD_LENGTH == 2
	return SwapTwoBytes (value);
	#else
	#error "Unknown TVM_WORD_LENGTH"
	#endif
}

static int decode_element (BYTE *src, int *length, tenc_element_t *element)
{
	if (*length < (sizeof (WORD) + 4)) {
		return -1;
	}

	memcpy (element->id, src, 4);
	element->id[4] 	= '\0';
	element->length = decode_int (src + 4);
	element->next	= src + (sizeof (WORD) + 4);
	*length		-= (sizeof (WORD) + 4);

	if (element->id[3] == 'I' || element->id[3] == 'U') {
		element->data.u_int	= element->length;
		element->length		= 0;
		return 0;
	} else if (*length < element->length) {
		return -1;
	} else {
		int bytes = element->length;

		bytes += ((sizeof (WORD)) - (bytes & (sizeof (WORD) - 1)));
		
		element->data.bytes 	= element->next;
		element->next		+= bytes;
		*length			-= bytes;

		return 0;
	}
}

static int ids_match (const char *a, const char *b)
{
	if (a[0] != b[0]) return 0;
	if (a[1] != b[1]) return 0;
	if (a[2] != b[2]) return 0;
	if (a[3] != b[3]) return 0;
	return 1;
}

static int walk_to_element (BYTE *data, int *length, const char *id, tenc_element_t *element) {
	while (*length > 0) {
		int ret = decode_element (data, length, element);

		if (ret < 0) {
			return ret;
		}

		if (ids_match (element->id, id)) {
			return 0;
		}
		
		data = element->next;
	}

	return -1;
}

static int load_uint (BYTE **data, int *length, const char *id, UWORD *dst)
{
	tenc_element_t element;
	int ret;

	if ((ret = walk_to_element (*data, length, id, &element)) < 0)
		return ret;
	
	*dst 	= element.data.u_int;
	*data	= element.next;

	return 0;
}

static int load_int (BYTE **data, int *length, const char *id, WORD *dst)
{
	return load_uint (data, length, id, (UWORD *) dst);
}

static int load_str (BYTE **data, int *length, const char *id, char **dst)
{
	tenc_element_t element;
	int ret;

	if ((ret = walk_to_element (*data, length, id, &element)) < 0)
		return ret;
	
	/* Make sure the string has room for a terminator */
	if (element.length < 1)
		return -1;

	*dst 	= element.data.str;
	*data	= element.next;

	/* Make sure the string is terminated */
	(*dst)[element.length - 1] = '\0';

	return 0;
}

static tenc_str_t *decode_strs (BYTE *data, int length, const char *id)
{
	tenc_str_t	*head	= NULL;
	tenc_str_t	*tail	= NULL;

	while (length > 0) {
		tenc_str_t *curr = (tenc_str_t *) data;
		char *str;

		if (load_str (&data, &length, id, &str) < 0)
			return head;
		
		curr->next	= NULL;
		curr->str	= str;

		if (tail == NULL) {
			head = tail = curr;
		} else {
			tail->next	= curr;
			tail		= curr;
		}
	}

	return head;
}

static tbc_tlp_t *decode_tlp (BYTE *head, const tenc_element_t *tlp_element)
{
	tbc_tlp_t *tlp 	= (tbc_tlp_t *) head;
	BYTE *data	= tlp_element->data.bytes;
	int length 	= tlp_element->length;

	if (load_str (&data, &length, "fmtS", &(tlp->fmt)) < 0)
		return NULL;
	
	if (load_str (&data, &length, "tlpS", &(tlp->desc)) < 0)
		return NULL;
	
	return tlp;
}

static tbc_ffi_t *decode_ffi (BYTE *head, const tenc_element_t *ffi_element)
{
	tenc_element_t 	element;
	tenc_str_t	*sym;
	tbc_ffi_t	*ffi	= (tbc_ffi_t *) head;
	BYTE 		*data	= ffi_element->data.bytes;
	int 		length	= ffi_element->length;

	if (walk_to_element (data, &length, "libL", &element) < 0)
		return NULL;
	
	ffi->libraries	= decode_strs (element.data.bytes, element.length, "libS");
	data		= element.next;
	
	if (walk_to_element (data, &length, "symL", &element) < 0)
		return NULL;

	ffi->symbols	= decode_strs (element.data.bytes, element.length, "symS");
	data		= element.next;

	if (walk_to_element (data, &length, "mapL", &element) < 0)
		return NULL;
	
	ffi->n_symbols	= 0;
	ffi->map 	= (tbc_ffi_entry_t *) (element.data.bytes - (4 + sizeof (WORD)));
	length		= element.length;
	data		= element.data.bytes;
	sym		= ffi->symbols;
	while ((sym != NULL) && (length > 0)) {
		WORD lib_idx;

		if (load_int (&data, &length, "idxI", &lib_idx) < 0)
			return NULL;

		ffi->map[ffi->n_symbols].symbol = sym->str;

		if (lib_idx < 0) {
			ffi->map[ffi->n_symbols].library = NULL;
		} else {
			tenc_str_t *lib = ffi->libraries;
			
			while (lib != NULL && lib_idx > 0) {
				lib_idx--;
				lib = lib->next;
			}

			if (lib == NULL)
				return NULL;

			ffi->map[ffi->n_symbols].library = lib->str;
		}

		ffi->n_symbols++;
		sym = sym->next;
	}
	
	if (sym != NULL)
		return NULL;

	return ffi;
}

static tbc_lni_t *decode_lni (BYTE *head, const tenc_element_t *lni_element)
{
	tenc_element_t 	element;
	tbc_lni_t	*lni	= (tbc_lni_t *) head;
	BYTE 		*data	= lni_element->data.bytes;
	int 		length	= lni_element->length;
	int		idx	= 0;

	if (walk_to_element (data, &length, "fn L", &element) < 0)
		return NULL;
	
	lni->files 	= decode_strs (element.data.bytes, element.length, "fn S");
	lni->entries 	= (tbc_lni_entry_t *) element.next;
	data		= element.next;
	
	while (length > 0) {
		tbc_lni_entry_t *ent 		= lni->entries + idx;
		BYTE 		*lnd_data;
		int 		lnd_length;

		if (walk_to_element (data, &length, "lndL", &element) < 0)
			break;

		lnd_length 	= element.length;
		lnd_data 	= element.data.bytes;
		if (load_uint (&lnd_data, &lnd_length, "offU", &(ent->offset)) < 0)
			break;
		if (load_uint (&lnd_data, &lnd_length, "fniU", &(ent->file)) < 0)
			break;
		if (load_uint (&lnd_data, &lnd_length, "ln U", &(ent->line)) < 0)
			break;

		data = element.next;
		idx++;
	}

	lni->n_entries = idx;

	return lni;
}

int tencode_tbc_decode (BYTE *data, int length, tbc_t **ptr)
{
	tenc_element_t 	element;
	tbc_t 		*tbc	= (tbc_t *) data;
	int		ret;

	/* Decode the required elements */

	if ((ret = load_uint (&data, &length, "endU", &(tbc->endian))) < 0)
		return ret;
	if ((ret = load_uint (&data, &length, "ws U", &(tbc->ws))) < 0)
		return ret;
	if ((ret = load_uint (&data, &length, "vs U", &(tbc->vs))) < 0)
		return ret;
	if ((ret = load_uint (&data, &length, "ms U", &(tbc->ms))) < 0)
		return ret;

	if ((ret = walk_to_element (data, &length, "bc B", &element)) < 0)
		return ret;
	
	tbc->bytecode_len	= element.length;
	tbc->bytecode		= element.data.bytes;
	data			= element.next;

	/* Decode optional elements */

	tbc->tlp = NULL;
	tbc->ffi = NULL;
	tbc->lni = NULL;

	while (length > 0) {
		if (decode_element (data, &length, &element) < 0)
			return 0; /* ignore errors */

		if (ids_match (element.id, "tlpL")) {
			tbc->tlp = decode_tlp (data, &element); 
		} else if (ids_match (element.id, "ffiL")) {
			tbc->ffi = decode_ffi (data, &element);
		} else if (ids_match (element.id, "lniL")) {
			tbc->lni = decode_lni (data, &element);
		}

		data = element.next;
	}

	return 0;
}

