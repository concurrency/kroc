
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
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/timeb.h>
#include <time.h>
#include <limits.h>

#ifndef WIN32
#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <sys/time.h>
#endif /* !WIN32 */

#if defined(HAVE_TVM_TVM_H)
#include <tvm/tvm.h>
#elif defined(HAVE_KROC_TVM_H)
#include <kroc/tvm.h>
#else
#include <tvm.h>
#endif
/*}}}*/

extern char **tvm_argv;
extern int tvm_argc;

extern void init_terminal (void);
extern void restore_terminal (void);
extern int char_available (void);
extern char read_char (void);

extern void install_sffi (ECTX ectx);

