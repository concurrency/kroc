
/*{{{  Includes */
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


#ifdef __cplusplus
extern "C" {
#endif


/*{{{  tvm_ectx_priv_t - ectx private data */
#define TVM_ECTX_PRIVATE_DATA 	tvm_ectx_priv_t

typedef struct _bytecode_t bytecode_t;
typedef struct _tvm_instance_t tvm_instance_t;
typedef void *tvm_ectx_priv_t;
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

	ECTX		firmware, user;
	bytecode_t	*fw_bc, *us_bc;
	
	void		*memory;
	int		memory_length;

	WORD		kyb_channel;
	WORD		scr_channel;
	WORD 		err_channel;
	WORD 		tlp_argv[3];
};
/*}}}*/

/*{{{  base64.c */
int tvm_base64_decode (const char *src, uint8_t *dst);
/*}}}*/

/*{{{  instance.c */
tvm_instance_t *alloc_tvm_instance(void);
void free_tvm_instance(tvm_instance_t *tvm);
/*}}}*/


#ifdef __cplusplus
}
#endif
