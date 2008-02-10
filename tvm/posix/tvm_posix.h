
/*{{{  Includes */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WIN32
#define BYTE WIN_BYTE
#define WORD WIN_WORD
#define UWORD WIN_UWORD
#include <windows.h>
#undef BYTE
#undef WORD
#undef UWORD 
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

#if defined(HAVE_TVM_TVM_H)
#include <tvm/tvm.h>
#elif defined(HAVE_KROC_TVM_H)
#include <kroc/tvm.h>
#else
#include <tvm.h>
#endif
/*}}}*/

/*{{{  main.c */
extern char **tvm_argv;
extern int tvm_argc;
/*}}}*/

/*{{{  sffi.c */
extern void install_sffi (ECTX ectx);
/*}}}*/

/*{{{  unix_io.c / win32_io.c */
extern void init_terminal (void);
extern void restore_terminal (void);
extern int char_available (void);
extern BYTE read_char (void);
/*}}}*/

