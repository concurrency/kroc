#include "tvm_nacl.h"

/* PROC read.char (RESULT INT ch) */
static int _read_char (ECTX ectx, WORD args[])
{
	WORDPTR ch = (WORDPTR) args[0];
	WORD val = ectx->priv.instance->read_char(ectx->priv.instance);
	
	write_word (ch, val);
	
	return SFFI_OK;
}

/* PROC write.screen (VAL []BYTE buffer) */
static int write_screen (ECTX ectx, WORD args[])
{
	char *buffer	= (char *) wordptr_real_address ((WORDPTR) args[0]);
	WORD length	= args[1];

	ectx->priv.instance->write_screen(ectx->priv.instance, buffer, length);
	
	return SFFI_OK;
}

/* PROC write.error (VAL BYTE ch) */
static int write_error (ECTX ectx, WORD args[])
{
	ectx->priv.instance->write_error(ectx->priv.instance, args[0]);
	return SFFI_OK;
}

/* PROC get_argv (INT argc, INT argv) */
static int get_argv (ECTX ectx, WORD args[])
{
	WORDPTR	argc	= (WORDPTR) args[0];
	WORDPTR	argv	= (WORDPTR) args[1];

	#if 0
	write_word (argc, tvm_argc - 1);
	write_word (argv, (WORD) &(tvm_argv[1]));
	#endif
	write_word (argc, (WORD) 0);
	write_word (argv, (WORD) 0);
	
	return SFFI_OK;
}

/* PROC get.version ([]BYTE str) */
static int get_version (ECTX ectx, WORD args[])
{
	BYTEPTR	str	= (BYTEPTR) args[0];
	WORD	str_len	= args[1];

	strncpy ((char *) wordptr_real_address ((WORDPTR) str), VERSION, str_len);
	
	return SFFI_OK;
}

static SFFI_FUNCTION sffi_table[] = {
	NULL,
	_read_char,
	write_screen,
	write_error,
	get_argv,
	get_version
};

void tvm_install_sffi (ECTX ectx)
{
	ectx->sffi_table	= sffi_table;
	ectx->sffi_table_length	= sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
}

