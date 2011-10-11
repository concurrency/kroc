#include "tvm_nacl.h"

static tbc_t *decode_tbc (BYTE *data, unsigned int length)
{
	tenc_element_t element;
	tbc_t *tbc = NULL;

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

void tvm_free_bytecode (bytecode_t *bc)
{
	if (--bc->refcount)
		return;
	
	/*
	if (bc->ffi_table != NULL)
		release_ffi_table (bc);
	*/
	if (bc->source != NULL)
		free (bc->source);
	if (bc->data != NULL)
		free (bc->data);
	
	free (bc);
}

bytecode_t *tvm_alloc_bytecode (uint8_t *tbc, size_t tbc_len)
{
	bytecode_t *bc	= (bytecode_t *) malloc (sizeof (bytecode_t));

	bc->refcount	= 1;
	bc->source 	= strdup("unknown");
	bc->data	= malloc(tbc_len);
	bc->length	= tbc_len;
	bc->ffi_table	= NULL;

	memcpy(bc->data, tbc, bc->length);

	if ((bc->tbc = decode_tbc (bc->data, bc->length)) == NULL)
		goto errout;

	/*
	if (!build_ffi_table (bc))
		goto errout;
	*/

	return bc;
errout:
	tvm_free_bytecode (bc);
	return NULL;
}

