
/*{{{  Includes */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WIN32
#define BYTE WIN_BYTE
#define WORD WIN_WORD
#define UWORD WIN_UWORD
#define interface WIN_interface
#include <windows.h>
#undef BYTE
#undef WORD
#undef UWORD
#undef interface
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

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

#if defined(HAVE_TVM_TVM_H)
#include <tvm/tvm.h>
#include <tvm/tvm_tbc.h>
#elif defined(HAVE_KROC_TVM_H)
#include <kroc/tvm.h>
#include <kroc/tvm_tbc.h>
#else
#include <tvm.h>
#include <tvm_tbc.h>
#endif
/*}}}*/

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

/*{{{  ffi.c */
extern int build_ffi_table (bytecode_t *bc);
extern void release_ffi_table (bytecode_t *bc);
/*}}}*/

/*{{{  introspect.c */
extern int vc0_mt_in (ECTX ectx, WORDPTR address);
/*}}}*/

/*{{{  main.c */
extern char **tvm_argv;
extern int tvm_argc;
/*}}}*/

/*{{{  sffi.c */
extern void install_sffi (ECTX ectx);
/*}}}*/

/*{{{  tbc.c */
extern int read_tbc_file (const char *fn, BYTE **data, int *length);
extern tbc_t *decode_tbc (BYTE *data, int length);
extern void free_bytecode (bytecode_t *bc);
extern bytecode_t *load_bytecode (const char *file);
/*}}}*/

/*{{{  unix_io.c / win32_io.c */
extern void init_terminal (void);
extern void restore_terminal (void);
extern int char_available (void);
extern BYTE read_char (void);
/*}}}*/

/*{{{  vm.c */
extern void init_vm (void);
extern ECTX allocate_ectx (bytecode_t *bc, const char *tlp, WORD *argv);
extern void free_ectx (ECTX vm);
/*}}}*/

