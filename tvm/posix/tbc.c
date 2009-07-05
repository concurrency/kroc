/*
 * tbc.c - TVM bytecode support functions
 *
 * Copyright (C) 2008  Carl G. Ritson
 *
 */

#include "tvm_posix.h"
#include <tvm_tbc.h>

int read_tbc_file (const char *fn, BYTE **data_p, int *length_p)
{
	FILE *fh = fopen (fn, "rb");
	BYTE *data;
	int ret, length;

	if (fh == NULL)
		return -1;

	if (fseek (fh, 0, SEEK_END) < 0)
		return -1;

	length	= (int) ftell (fh);
	data 	= (BYTEPTR) malloc (length);
	
	if (data == NULL)
		return -1;
	
	if (fseek (fh, 0, SEEK_SET) < 0) {
		free (data);
		fclose (fh);
		return -1;
	}

	if ((ret = fread (data, 1, length, fh)) != length) {
		free (data);
		fclose (fh);
		return -1;
	}

	fclose (fh);

	*data_p		= data;
	*length_p	= length;

	return 0;
}

tbc_t *decode_tbc (BYTE *data, int length)
{
	tenc_element_t element;
	tbc_t *tbc = NULL;

	/* CGR FIXME: this function should move to libtvm. */

	if (tenc_decode_element (data, &length, &element))
		return NULL;

	#if TVM_WORD_LENGTH == 2
	if (memcmp (element.id, "tenc", 4) != 0)
	#else
	if (memcmp (element.id, "TEnc", 4) != 0)
	#endif
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

void free_bytecode (bytecode_t *bc)
{
	if (--bc->refcount)
		return;
	
	if (bc->ffi_table != NULL)
		release_ffi_table (bc);
	if (bc->source != NULL)
		free (bc->source);
	if (bc->data != NULL)
		free (bc->data);
	
	free (bc);
}

bytecode_t *load_bytecode (const char *file)
{
	bytecode_t *bc	= (bytecode_t *) malloc (sizeof (bytecode_t));

	bc->refcount	= 1;
	bc->source 	= strdup (file);
	bc->data	= NULL;
	bc->ffi_table	= NULL;

	if (read_tbc_file (file, &(bc->data), &(bc->length)))
		goto errout;

	if ((bc->tbc = decode_tbc (bc->data, bc->length)) == NULL)
		goto errout;

	if (!build_ffi_table (bc))
		goto errout;
	
	return bc;
errout:
	free_bytecode (bc);
	return NULL;
}

