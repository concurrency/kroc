
/*{{{  Includes */
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>

#include <errno.h>
#include <signal.h>
#include <string.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <time.h>
#include <sys/nacl_syscalls.h>
/*}}}*/

#ifndef VERSION
#define VERSION "1.0.0"
#endif

#ifdef __cplusplus
extern "C" {
#endif


/*{{{  tvm_ectx_priv_t - ectx private data */
#define TVM_ECTX_PRIVATE_DATA 	tvm_ectx_priv_t

typedef struct _bytecode_t bytecode_t;
typedef struct _tvm_instance_t tvm_instance_t;
typedef struct _tvm_ectx_priv_t {
	bytecode_t      *bytecode;
	void            *memory;
	int             memory_length;
	tvm_instance_t	*instance;
} tvm_ectx_priv_t;
/*}}}*/

#include <tvm.h>
#include <tvm_tbc.h>

/*{{{  bc_t - bytecode data */
struct _bytecode_t {
	int		refcount;
	char 		*source;
	BYTE		*data;
	int		length;
	tbc_t		*tbc;
	FFI_TABLE_ENTRY	*ffi_table;
	int		ffi_table_length;
	void		*dll_handles;
};
/*}}}*/

/*{{{  tvm_instance_t - TVM instance data structure */
struct _tvm_instance_t {
	tvm_t		tvm;
	volatile int	stop;
	char		*last_error;

	ECTX		firmware, user;
	bytecode_t	*fw_bc, *us_bc;
	
	void		*memory;
	int		memory_length;

	WORD		kyb_channel;
	WORD		scr_channel;
	WORD 		err_channel;
	WORD 		tlp_argv[3];

	void		*handle;
	int		(*read_char)(tvm_instance_t *);
	void		(*write_screen)(tvm_instance_t *, const char *, int);
	void		(*write_error)(tvm_instance_t *, char);
};
/*}}}*/

/*{{{  base64.c */
int tvm_base64_decode (const char *src, uint8_t *dst);
/*}}}*/

/*{{{  firmware.c */
const uint8_t *tvm_nacl_firmware(size_t *length);
/*}}}*/

/*{{{  instance.c */
tvm_instance_t *tvm_alloc_instance(void);
void tvm_free_instance(tvm_instance_t *tvm);
int tvm_load_bytecode(tvm_instance_t *tvm, uint8_t *tbc, size_t tbc_len);
int tvm_run_instance(tvm_instance_t *tvm);
/*}}}*/

/*{{{  sffi.c */
void tvm_install_sffi (ECTX ectx);
/*}}}*/

/*{{{  tbc.c */
void tvm_free_bytecode (bytecode_t *bc);
bytecode_t *tvm_alloc_bytecode (const uint8_t *tbc, size_t tbc_len);
/*}}}*/

/*{{{  vm.c */
ECTX tvm_allocate_ectx (tvm_instance_t *tvm, bytecode_t *bc, const char *tlp, WORD *argv);
void tvm_free_ectx (ECTX vm);
/*}}}*/

#ifdef __cplusplus
}
#endif
