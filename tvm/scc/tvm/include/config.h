/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define to 1 if you have the <inttypes.h> header file. */
//#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <math.h> header file. */
//#define HAVE_MATH_H 1

/* Define to 1 if you have the <memory.h> header file. */
/* #undef HAVE_MEMORY_H */

/* Define to 1 if you have the <stdint.h> header file. */
//#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdio.h> header file. */
#define HAVE_STDIO_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
/* #undef HAVE_STRINGS_H */

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
/* #undef HAVE_SYS_STAT_H */

/* Define to 1 if you have the <sys/types.h> header file. */
/* #undef HAVE_SYS_TYPES_H */

/* Define to 1 if you have the <unistd.h> header file. */
/* #undef HAVE_UNISTD_H */

/* Name of package */
#define PACKAGE "libtvm"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "kroc-bugs@kent.ac.uk"

/* Define to the full name of this package. */
#define PACKAGE_NAME "libtvm"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "libtvm 1.0.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "libtvm"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.0.0"

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* define for big-endian TVM */
/* #undef TVM_BIG_ENDIAN */

/* define to enable memcpy/memset overriding */
/* #undef TVM_CUSTOM_MEM_OPS */

/* define to dispatch instructions using switch rather than a jump table */
#define TVM_DISPATCH_SWITCH 1

/* define to alloc dynamic memory allocation */
#define TVM_DYNAMIC_MEMORY 1

/* define to emulate a T2 processor */
#define TVM_EMULATE_T2 1

/* define to emulate a T4 processor */
/* #undef TVM_EMULATE_T4 */

/* define to emulate a T8 processor */
/* #undef TVM_EMULATE_T8 */

/* define for external channel bundles */
/* #undef TVM_EXTERNAL_CHANNEL_BUNDLES */

/* define if C compiler supports maths on double words */
//#define TVM_HAVE_TWOWORD 1

/* define for little-endian TVM */
#define TVM_LITTLE_ENDIAN 1

/* define to use the AVR memory interface */
//#define TVM_MEM_INTF_AVR 1

/* define to enable occam-pi support */
#define TVM_OCCAM_PI 1

/* define for Darwin TVM */
/* #undef TVM_OS_DARWIN */

/* define for FreeBSD TVM */
/* #undef TVM_OS_FREEBSD */

/* define for Linux TVM */
/* #undef TVM_OS_LINUX */

/* define for OS independent TVM */
#define TVM_OS_NONE 1

/* define for Solaris TVM */
/* #undef TVM_OS_SOLARIS */

/* define for Windows TVM */
/* #undef TVM_OS_WINDOWS */

/* define to enable packed execution context */
#define TVM_PACKED_ECTX 1

/* define to enable profiling */
/* #undef TVM_PROFILING */

/* define to T9000 short ops */
/* #undef TVM_SHORT_OPS */

/* define for type shadowing */
/* #undef TVM_TYPE_SHADOW */

/* define to allow FPU usage */
//#define TVM_USE_FPU 1

/* define to allow inline keyword usage */
#define TVM_USE_INLINE 1

/* define to allow malloc usage */
//#define TVM_USE_MALLOC 1

/* define to use memcpy */
#define TVM_USE_MEMCPY 1

/* define to use memset */
#define TVM_USE_MEMSET 1

/* define to use TLSF dynamic memory allocator */
//#define TVM_USE_TLSF 1

/* define emulated word length */
#define TVM_WORD_LENGTH 4

/* Version number of package */
#define VERSION "1.0.0"

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif
