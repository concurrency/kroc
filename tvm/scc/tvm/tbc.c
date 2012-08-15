#include "tvm-scc.h"
#include <tvm_tbc.h>

static int load_uint (BYTE **data, unsigned int *length, const char *id, UWORD *dst)
{
	tenc_element_t element;
	int ret;

	if ((ret = tenc_walk_to_element (*data, length, id, &element)) < 0)
		return ret;
	
	*dst 	= element.data.u_int;
	*data	= element.next;

	return 0;
}

static int tmp_tbc_decode (BYTE *data, UWORD length, ECTX context, WORDPTR memory, UWORD memory_size)
{
	UWORD ws_size, vs_size;
	BYTE *bytecode;

	WORDPTR ws, vs;

	tenc_element_t element;
	int ret, memory_used;

	/* Decode the required elements */
	if ((ret = load_uint (&data, &length, "ws U", &ws_size)) < 0)
		return ret;
	if ((ret = load_uint (&data, &length, "vs U", &vs_size)) < 0)
		return ret;

	if ((ret = tenc_walk_to_element (data, &length, "bc B", &element)) < 0)
		return ret;
	
	bytecode = element.data.bytes;
	data = element.next;

	/* FIXME: check TLP is empty */
#if 0
	/* Decode optional elements */
	tbc->tlp = NULL;

	while (length > 0) {
		if (tenc_decode_element (data, &length, &element) < 0)
			return 0; /* ignore errors */

		if (ids_match (element.id, "tlpL")) {
			tbc->tlp = decode_tlp (data, tbc->tlp, &element); 
		}

		data = element.next;
	}
#endif

	memory_used = tvm_ectx_layout (
		context, memory,
		"", 0,
		ws_size, vs_size,
		&ws, &vs
	);

	printf ("INFO: Loaded program with ws_size=%d, vs_size=%d, memory_size=%d, used=%d\n", ws_size, vs_size, memory_size, memory_used);

	/* Check we haven't exhausted memory. */
	if (memory_used > memory_size) {
		printf("ERRO: not enough RAM for program.\n");
	}

	ret = tvm_ectx_install_tlp (
		context, bytecode,
		ws, vs,
		"", 0, NULL
	);

	return ret;
}
/*}}}*/

static int load_tbc (BYTE *data, tenc_element_t *element) {
	UWORD length;

	if (memcmp("TEnc", data, 4) != 0) {
		printf("ERRO: magic was not 'TEnc'.\n");
		return -1;
	}
	length = tenc_decode_int (data + 4);
	printf("INFO: Length stated in tbc was %d.\n", length);

	/* Skip over the first header. */
	data += 8;

	if (tenc_walk_to_element (data, &length, "tbcL", element) < 0) {
		printf("ERRO: tenc_walk_to_element() failed!.\n");
		return -1;
	}

	return 0;
}

/* Fetch file and line number information for a given iptr offset.
   Returns 0 on success, -1 on failure. */
int tbc_file_and_line (BYTE *data, UWORD offset, BYTE **file, UWORD *line) {
	tenc_element_t element;

	if (load_tbc (data, &element) != 0) {
		return -1;
	}

	return tbc_debug_file_and_line (element.data.bytes, element.length, offset, (char**)file, line);
}

/* Initialise a Transputer context from a TBC file in program memory.
   Returns 0 on success, -1 on failure. */
int init_context_from_tbc (ECTX context, BYTE *data, WORDPTR memory, UWORD memory_size) {
	tenc_element_t element;

	if (load_tbc (data, &element) != 0) {
		printf("ERRO: load_tbc() failed!\n");
		return -1;
	}

	return tmp_tbc_decode (element.data.bytes, element.length, context, memory, memory_size);
}
