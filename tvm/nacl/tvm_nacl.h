
/*{{{  Includes */
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif

/*{{{  tvm_ectx_priv_t - ectx private data */
#define TVM_ECTX_PRIVATE_DATA 	tvm_ectx_priv_t

typedef struct _bytecode_t bytecode_t;
typedef struct _tvm_ectx_priv_t {
	bytecode_t	*bytecode;
	void		*memory;
	int		memory_length;
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

