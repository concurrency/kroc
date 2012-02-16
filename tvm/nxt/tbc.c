/*
 * tbc.c - TVM bytecode support functions
 *
 * Copyright (C) 2008  Carl G. Ritson
 *
 */

#include "tvm-nxt.h"

UWORD valid_tbc_header (BYTE *data)
{
	if (data[0] == 'T' && data[1] == 'E' && data[2] == 'n' && data[3] == 'c') {
		return tenc_decode_int (&(data[4]));
	}
	return 0;
}

static tbc_t *decode_tbc (BYTE *data, unsigned int length)
{
	tenc_element_t element;
	tbc_t *tbc = NULL;

	/* CGR FIXME: this function should move to libtvm. */

	if (tenc_decode_element (data, &length, &element))
		return NULL;

	if ((element.id[0] != 'T') || (element.id[1] != 'E') 
		|| (element.id[2] != 'n') || (element.id[3] != 'c'))
	{
		return NULL;
	}

	data	= element.data.bytes;
	length	= element.length;

	if (tenc_walk_to_element (data, &length, "tbcL", &element) < 0)
		return NULL;

	if (tbc_decode (element.data.bytes, element.length, &tbc))
		return NULL;

	return tbc;
}

tbc_t *load_context_with_tbc (ECTX ectx, tbc_t *tbc, BYTE *data, UWORD length)
{
	WORDPTR mem, vs, ws;
	WORD mem_len;
	int i;

	if (tbc == NULL && data != NULL) {
		tbc = decode_tbc (data, length);
	}

	if (tbc == NULL) {
		return NULL;
	}

	mem_len = tvm_ectx_memory_size (
		ectx,
		"", 0, 
		tbc->ws, tbc->vs
	);

	mem = (WORDPTR) tvm_malloc (ectx, sizeof (WORD) * mem_len);
	if (mem == NULL) {
		return NULL;
	}
	for (i = 0 ; i < mem_len; ++i) {
		write_word (wordptr_plus (mem, i), MIN_INT);
	}

	tvm_ectx_layout (
		ectx, mem,
		"", 0, 
		tbc->ws, tbc->vs, 
		&ws, &vs
	);

	if (tvm_ectx_install_tlp (
		ectx,
		tbc->bytecode, ws, vs,
		"", 0, NULL
	)) {
		tvm_free (ectx, mem);
		return NULL;
	}

	ectx->priv.memory		= mem;
	ectx->priv.memory_length	= mem_len;

	return tbc;
}
