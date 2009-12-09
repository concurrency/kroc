AC_INIT([ccsp], [1.6.3], [kroc-bugs@kent.ac.uk])
AM_CONFIG_HEADER(config.h)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

m4_pattern_allow([^AS_DEFS$])

dnl Maintainer mode not used
dnl jm_MAINTAINER_MODE

dnl Checks for programs.
AM_PROG_CC_STDC
AM_PROG_AS
AC_PROG_RANLIB
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_CHECK_TOOL(AS,as)
AC_CHECK_TOOL(LD,ld)

KROC_CCSP_FLAGS

AC_CHECK_HEADERS(stdlib.h stdarg.h malloc.h sys/param.h sys/time.h string.h strings.h sys/ioctl.h time.h)
AC_CHECK_LIB(m, sin)

#{{{  options parsing

dnl AC_ARG_ENABLE([cpu-timers], AS_HELP_STRING([--enable-cpu-timers], [enable CPU timers (default disabled)]), [cpu_timers=$enableval],[cpu_timers=no])

dnl AC_ARG_ENABLE([debug-debug], AS_HELP_STRING([--enable-debug-debug], [enable occam debugging debug messages (default disabled)]), [debug_debug=$enableval], [debug_debug=no])
dnl AC_ARG_ENABLE([dm-debug], AS_HELP_STRING([--enable-dm-debug], [enable dynamic memory debugging (default disabled)]), [enable_dm_debug=$enableval], [enable_dm_debug=no])
enable_dm_debug=no
AC_ARG_ENABLE([generate-cores], AS_HELP_STRING([--enable-generate-cores],[enable core dump generation on errornous exit (default disabled)]), [generate_cores=$enableval], [generate_cores=no])
AC_ARG_ENABLE([usegv], AS_HELP_STRING([--enable-usegv],[enable useful information on SEGV (default disabled)]), [useful_segv=$enableval], [useful_segv=no])
AC_ARG_ENABLE([ufpe], AS_HELP_STRING([--enable-ufpe],[enable useful FPE information (default disabled)]), [useful_fpe=$enableval], [useful_fpe=no])

AC_ARG_ENABLE([blocking-syscalls], AS_HELP_STRING([--disable-blocking-syscalls],[disable blocking system calls (default enabled)]), [blocking_syscalls=$enableval], [blocking_syscalls=yes])
dnl AC_ARG_ENABLE([blocking-profile], AS_HELP_STRING([--enable-blocking-profile],[enable profiling of blocking system calls (default disabled)]), [blocking_profile=$enableval], [blocking_profile=no])
dnl AC_ARG_ENABLE([blocking-debug], AS_HELP_STRING([--enable-blocking-debug],[enable debugging of blocking system calls (default disabled)]), [blocking_debug=$enableval], [blocking_debug=no])
dnl AC_ARG_ENABLE([blocking-stop], AS_HELP_STRING([--enable-blocking-stop],[enable stopping of clone when it gets SIGSEGV]), [blocking_stop=$enableval], [blocking_stop=no])

AC_ARG_ENABLE([deadlock-terse], AS_HELP_STRING([--enable-deadlock-terse],[enable terse output of deadlock debug info (default disabled)]), [deadlock_terse=$enableval], [deadlock_terse=no])
AC_ARG_ENABLE([deadlock-brief], AS_HELP_STRING([--enable-deadlock-brief],[enable brief output of deadlock debug info (default disabled)]), [deadlock_brief=$enableval], [deadlock_brief=no])

dnl AC_ARG_ENABLE([ktraces], AS_HELP_STRING([--enable-ktraces],[enable kernel-entry traces (default disabled)]), [enable_ktraces=$enableval], [enable_ktraces=no])
dnl AC_ARG_ENABLE([dtraces], AS_HELP_STRING([--enable-dtraces],[enable debugging traces (default disabled)]), [enable_dtraces=$enableval], [enable_dtraces=no])
AC_ARG_ENABLE([debug-dynproc], AS_HELP_STRING([--enable-debug-dynproc],[enable dynamic process debugging (default disabled)]), [enable_debug_dynproc=$enableval], [enable_debug_dynproc=no])
AC_ARG_ENABLE([mmap], AS_HELP_STRING([--disable-mmap],[disable mmap() based allocation (default enabled)]), [enable_mmap=$enableval], [enable_mmap=yes])
AC_ARG_ENABLE([tls], AS_HELP_STRING([--enable-tls],[use thread-local-storage for scheduler pointer]), [enable_tls=$enableval], [enable_tls=yes])
dnl AC_ARG_ENABLE([sse2], AS_HELP_STRING([--disable-sse2],[do not use SSE2 even if supported on build system]), [enable_sse2=$enableval], [enable_sse2=yes])
AC_ARG_ENABLE([tuning], AS_HELP_STRING([--enable-tuning=...],[set tuning for speed or size (default speed)]), [tuning=$enableval], [tuning=default])
AC_ARG_WITH([alloc], AS_HELP_STRING([--with-alloc=...],[allocator to use (malloc,block,sblock)]), [alloc=$withval], [alloc=])

#}}}

#{{{  import shared config

if test "x$KROC_RMOX" != "x"; then
  # other settings because we configure this out-of-tree
  KROC_CCSP_ENABLE_MP=yes
  KROC_CCSP_ENABLE_PTHREADS=no
  KROC_CCSP_ENABLE_DYNPROC=no
fi

EXTRA_CFLAGS="$EXTRA_CFLAGS $KROC_CCSP_CFLAGS"
EXTRA_LDFLAGS="$EXTRA_LDFLAGS $KROC_CCSP_LDFLAGS"
LIBS="$LIBS $KROC_CCSP_LIBS"

if test $KROC_CCSP_ENABLE_PTHREADS = yes; then
  AC_DEFINE([USE_PTHREADS],1,[define to use POSIX threads for blocking syscalls and multi-processor])
fi

if test $KROC_CCSP_ENABLE_MP = yes; then
  AC_DEFINE([ENABLE_MP],1,[define to enable multi-processor support])
  AC_DEFINE([MAX_RUNTIME_THREADS],32,[define number of runtime threads])
else
  AC_DEFINE([MAX_RUNTIME_THREADS],1,[define number of runtime threads])
fi

if test $KROC_CCSP_ENABLE_DYNPROC = yes; then
  AC_DEFINE([DYNAMIC_PROCS],1,[define to enable dynamic processes])
fi

#}}}
#{{{  bsyscall reports
AC_MSG_CHECKING(whether blocking system calls are supported)
if test "$KROC_CCSP_ENABLE_PTHREADS" = no; then
  AC_MSG_RESULT(no)
  blocking_syscalls=no
else
  AC_MSG_RESULT(yes)
fi

AC_MSG_CHECKING(whether to enable blocking system calls)
AC_MSG_RESULT($blocking_syscalls)
if test "$blocking_syscalls" = yes; then
 AC_DEFINE([BLOCKING_SYSCALLS],1,[define to support blocking syscalls])
else
 AC_DEFINE([BLOCKING_SYSCALLS_UNSUPPORTED],1,[define if blocking syscalls are unsupported])
fi

AC_MSG_CHECKING(whether to enable blocking system call profiling)
AC_MSG_RESULT($blocking_profile)
if test "$blocking_profile" = yes; then
 AC_DEFINE([BLOCKING_PROFILE],1,[define to enable blocking syscall profiling])
fi

AC_MSG_CHECKING(whether to enable blocking system call debugging)
AC_MSG_RESULT($blocking_debug)
if test "$blocking_debug" = yes; then
 AC_DEFINE([BLOCKING_DEBUG],1,[define to enable blocking syscall debugging])
fi

AC_MSG_CHECKING(whether to enable clone SIGSTOP on startup)
AC_MSG_RESULT($blocking_stop)
if test "$blocking_stop" = yes; then
 AC_DEFINE([BLOCKING_STOP],1,[define to allow blocking calls to stop when they receive SIGSEGV])
fi
#}}}
#{{{  check for rdtsc instruction
dnl Do checks to make sure CPU timers can be supported (has rdtsc instruction)

AC_TRY_RUN([
int main (void)
{
 unsigned int hi, lo;
 __asm__ __volatile__ ("rdtsc\n\t" : "=a" (hi), "=d" (lo));
 return 0;
}], [cpu_timers_supported=yes], [cpu_timers_supported=no], [cpu_timers_supported=no])

#}}}
#{{{  check for SSE2 instructions

AC_MSG_CHECKING([for SSE2 support])
AC_TRY_RUN([
int main (void)
{
 __asm__ __volatile__ ("mfence\nlfence\nsfence\n" : : : "memory" );
 return 0;
}], [have_ia32_sse2=yes], [have_ia32_sse2=no], [have_ia32_sse2=no])
if test "$have_ia32_sse2" = yes; then
 if test "$KROC_CCSP_ENABLE_SSE2" = yes; then
  AC_DEFINE([HAVE_IA32_SSE2], 1, [CPU supports SSE2 instructions])
  AC_MSG_RESULT(yes)
 else
  AC_MSG_RESULT([yes, but disabled])
 fi
else
 AC_MSG_RESULT([no; this may break multiprocessor support])
fi

#}}}
#{{{  tests for anonymous memory-mapping
dnl 
dnl tests for anonymous mmap()
dnl 

case "$host_os" in
solaris* | sunos*)
	map_membase="0x40000000"
	;;
linux* | *bsd* | *BSD*)
	map_membase="0x80000000"
	;;
cygwin*)
	map_membase="0x40000000"
	;;
*)
	map_membase="0x80000000"
	;;
esac

#{{{  test for mmap() of /dev/zero
AC_RUN_IFELSE([
#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>

int main (void)
{
 int fd;
 void *addr;

 fd = open ("/dev/zero", O_RDWR);
 if (fd < 0) {
  return 1;
 }
 addr = mmap ((void *)0, 1048576, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, fd, 0);
 if (addr == ((void *)-1)) {
  return 1;
 }
 munmap (addr, 1048576);
 close (fd);
 return 0; 
}],[can_mmap_zero=yes],[can_mmap_zero=no],[can_mmap_zero=no])
#}}}

AC_MSG_CHECKING([whether to enable mmap() based allocation])
AC_MSG_RESULT($enable_mmap)

AC_MSG_CHECKING([whether MAP_ANON on /dev/zero is supported])
AC_MSG_RESULT($can_mmap_zero)

#{{{  test for mmap() of invalid descriptor
AC_RUN_IFELSE([
#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>

int main (void)
{
 void *addr;

 addr = mmap ((void *)0, 1048576, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
 if (addr == ((void *)-1)) {
  return 1;
 }
 munmap (addr, 1048576);
 return 0; 
}],[can_mmap_anon=yes],[can_mmap_anon=no],[can_mmap_anon=no])
#}}}

AC_MSG_CHECKING([whether MAP_ANON on an invalid descriptor is supported])
AC_MSG_RESULT($can_mmap_anon)

use_mmap=no

if test [ "$can_mmap_zero" = yes ]; then
 use_mmap=yes
fi
if test [ "$can_mmap_anon" = yes ]; then
 use_mmap=yes
fi

if test [ "$use_mmap" = yes ]; then
 SAVED_CFLAGS="$CFLAGS"
 if test [ "$can_mmap_anon" = yes ]; then
  CFLAGS="$CFLAGS -DCAN_MMAP_ANON -DMEMBASE=$map_membase"
 else
  CFLAGS="$CFLAGS -DCAN_MMAP_ZERO -DMEMBASE=$map_membase"
 fi

 #{{{  test for mmap() requiring MAP_FIXED for fixed allocation
 AC_RUN_IFELSE([
 #include <sys/types.h>
 #include <sys/mman.h>
 #include <unistd.h>
 #include <fcntl.h>

 int main (void)
 {
  void *addr;
  #ifdef CAN_MMAP_ZERO
  int fd = open ("/dev/zero", O_RDRW);

  if (fd < 0) {
   return 1;
  }
  #else
  int fd = -1;
  #endif

  addr = (void *)mmap ((void *)MEMBASE, 1 << 21, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, fd, 0);
  if ((addr != (void *)-1) && (addr != (void *)MEMBASE)) {
   munmap (addr, 1 << 21);
   /* try remap with fixed */
   addr = (void *)mmap ((void *)MEMBASE, 1 << 21, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON|MAP_FIXED, fd, 0);
   if (addr != (void *)MEMBASE) {
    return 1;
   }
   return 0;
  }
  return 1;
 }],[mmap_fixed=yes],[mmap_fixed=no],[mmap_fixed=no])
 #}}}
 #{{{  test for mmap() at a fixed address
 AC_RUN_IFELSE([
 #include <sys/types.h>
 #include <sys/mman.h>
 #include <unistd.h>
 #include <fcntl.h>

 int main (void)
 {
  void *addr;
  #ifdef CAN_MMAP_ZERO
  int fd = open ("/dev/zero", O_RDRW);

  if (fd < 0) {
   return 1;
  }
  #else
  int fd = -1;
  #endif

  addr = (void *)mmap ((void *)MEMBASE, 1 << 21, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, fd, 0);
  if ((addr != (void *)-1) && (addr != (void *)MEMBASE)) {
   munmap (addr, 1 << 21);
   /* try remap with fixed */
   addr = (void *)mmap ((void *)MEMBASE, 1 << 21, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON|MAP_FIXED, fd, 0);
   if (addr != (void *)MEMBASE) {
    return 1;
   }
   return 0;
  } else if (addr == (void *)MEMBASE) {
   return 0;
  }
  return 1;
 }],[can_mmap_fixed=yes],[can_mmap_fixed=no],[can_mmap_fixed=no])
 #}}}

 CFLAGS="$SAVED_CFLAGS"

 AC_MSG_CHECKING([whether MAP_FIXED must be specified for fixed mappings])
 AC_MSG_RESULT($mmap_fixed)

 AC_MSG_CHECKING([whether fixed mapping are supported])
 AC_MSG_RESULT($can_mmap_fixed)

 if test [ "$can_mmap_fixed" = no ]; then
  use_mmap=no
 fi

else
 mmap_fixed=no
fi

if test [ "$enable_mmap" = yes ]; then
 if test [ "$use_mmap" = yes ]; then
  AC_DEFINE([DMEM_USE_MMAP],1,[define this if fixed anonymous memory-mapping is supported])
  AC_DEFINE_UNQUOTED([DMEM_MEMBASE],$map_membase,[defines the default memory base for fixed mappings])
 fi
 if test [ "$can_mmap_anon" = yes ]; then
  AC_DEFINE([DMEM_MMAP_INVFD],1,[define this if anonymous memory-mapping is done on an invalid descriptor])
 fi
 if test [ "$mmap_fixed" = yes ]; then
  AC_DEFINE([DMEM_MMAP_FIXED],1,[define this if MAP_FIXED must be given for fixed mappings])
 fi
fi

#}}}
#{{{  decide CPU support

AC_MSG_CHECKING(whether CPU timers are supported)
AC_MSG_RESULT($cpu_timers_supported)
if test "$cpu_timers_supported" = no; then
 KROC_CCSP_ENABLE_CPUTIMERS=no
fi

AC_MSG_CHECKING(whether to enable CPU timers)
AC_MSG_RESULT($cpu_timers)
if test "$KROC_CCSP_ENABLE_CPUTIMERS" = yes; then
 AC_DEFINE([ENABLE_CPU_TIMERS],1,[define to support CPU timers])
fi

#}}}
#{{{  debugging options and support

AC_MSG_CHECKING(whether to enable debug debugging messages)
AC_MSG_RESULT($debug_debug)
if test "$debug_debug" = yes; then
 AC_DEFINE([DEBUG_DEBUG],1,[define to enable debug debugging])
fi

AC_MSG_CHECKING(whether to enable dynamic memory debugging)
AC_MSG_RESULT($enable_dm_debug)

if test "$enable_dm_debug" = yes; then
 AC_CHECK_HEADER(gelf.h, [have_gelf_h=yes], [have_gelf_h=no])
 AC_MSG_CHECKING(whether gelf.h is present)
 AC_MSG_RESULT($have_gelf_h)

 AC_CHECK_HEADER(libelf.h, [have_libelf_h=yes], [have_libelf_h=no])
 AC_MSG_CHECKING(whether libelf.h is present)
 AC_MSG_RESULT($have_libelf_h)

 AC_CHECK_LIB(elf, elf_begin, have_libelf=yes, have_libelf=no)
 AC_MSG_CHECKING(whether libelf is installed and has elf_begin)
 AC_MSG_RESULT($have_libelf)

 AC_DEFINE([DM_DEBUG],1,[define to enable dynamic memory debugging])
 if test "$have_libelf_h" = yes; then
  if test "$have_libelf" = yes; then
   if test "$have_gelf_h" = yes; then
    AC_DEFINE([DM_DEBUG_LIBELF],1,[define to use libelf with dynamic memory debugging])
   fi
  fi
 fi
fi

AC_MSG_CHECKING(whether to enable core dumps)
AC_MSG_RESULT($generate_cores)
if test "$generate_cores" = yes; then
 AC_DEFINE([GENERATE_CORES],1,[define to let CCSP drop core files])
fi

AC_MSG_CHECKING(whether to enable terse deadlock debugging)
AC_MSG_RESULT($deadlock_terse)
if test "$deadlock_terse" = yes; then
 AC_DEFINE([DEADLOCK_TERSE],1,[define to enable terse deadlock debugging])
fi

AC_MSG_CHECKING(whether to enable brief deadlock debugging)
AC_MSG_RESULT($deadlock_brief)
if test "$deadlock_brief" = yes; then
 AC_DEFINE([DEADLOCK_BRIEF],1,[define to enable brief deadlock debugging])
fi

AC_MSG_CHECKING(whether to enable verbose segfault)
AC_MSG_RESULT($useful_segv)
if test "$useful_segv" = yes; then
 AC_DEFINE([USEFUL_SEGFAULT],1,[define to enable a verbose segfault])
fi

AC_MSG_CHECKING(whether to enable verbose FPE)
AC_MSG_RESULT($useful_fpe)
if test "$useful_fpe" = yes; then
 AC_DEFINE([USEFUL_FPEDEBUG],1,[define to enable verbose FPE messages])
fi

if test $KROC_CCSP_ENABLE_DYNPROC = yes; then
 AC_MSG_CHECKING(whether to enable dynamic process debugging)
 AC_MSG_RESULT($enable_debug_dynproc)
 if test "$enable_debug_dynproc" = yes; then
  AC_DEFINE([DEBUG_DYNPROC],1,[define to enable dynamic process debugging])
 fi
fi

AC_MSG_CHECKING(whether to enable kernel traces)
AC_MSG_RESULT($enable_ktraces)
if test "$enable_ktraces" = yes; then
 AC_DEFINE([ENABLE_KTRACES],1,[define to enable kernel traces])
fi

#AC_MSG_CHECKING(whether to enable debugging traces)
#AC_MSG_RESULT($enable_dtraces)
#if test "$enable_dtraces" = yes; then
# AC_DEFINE([ENABLE_DTRACES],1,[define to enable debugging traces])
# LIBCCSP=libccsptrace.so
#else
# LIBCCSP=libccsp.so
#fi

#}}}
#{{{  check for ualarm

dnl Check for presence of ualarm
AC_CHECK_FUNC(ualarm,exist_ualarm=yes,exist_ualarm=no)

# This is a shell script, so put ""s around values with spaces
if test "$exist_ualarm" = yes; then
  AC_DEFINE([HAVE_UALARM],1,[define if this system has ualarm()])
fi

#}}}
#{{{  setup RMoX build

AC_MSG_CHECKING(whether to build the special RMoX version of CCSP)
if test "x$KROC_RMOX" = "x"; then
 AC_MSG_RESULT(no)
 RMOX_RTLIB=
 RMOX_CFLAGS=
else
 AC_MSG_RESULT(yes)
 AC_MSG_CHECKING(location of RMoX include files)
 AC_MSG_RESULT($KROC_RMOX)
 AC_CHECK_FILE($KROC_RMOX/include/rmox_if.h, [], AC_MSG_ERROR([RMoX include files not found]))

 AC_DEFINE([RMOX_BUILD],1,[define if building for RMoX])
 RMOX_RTLIB=libccsp_rmox.a
 RMOX_CFLAGS="-I$KROC_RMOX/include -fno-common -nostdlib -nostartfiles -nodefaultlibs"
 enable_tls=no
fi
AC_SUBST(RMOX_RTLIB)

#}}}
#{{{  check which memory allocator to use

AC_MSG_CHECKING(which memory allocator to use)
if test "$alloc" = ""; then
 alloc=malloc
fi
if test "$alloc" = "malloc"; then
 AC_MSG_RESULT([malloc])
 AC_DEFINE([ALLOC_MALLOC],1,[define to use system malloc])
else
 if test "$alloc" = "block"; then
  AC_MSG_RESULT([per CPU block])
  AC_DEFINE([ALLOC_BLOCK],1,[define to use per-CPU block allocator])
 else
  if test "$alloc" = "sblock"; then
   AC_MSG_RESULT([shared block])
   AC_DEFINE([ALLOC_SBLOCK],1,[define to use shared block allocator])
  else
   AC_MSG_RESULT([unknown])
   AC_MSG_ERROR([no allocator specified])
  fi
 fi
fi
#}}}
#{{{  check for thread-local storage 

AC_MSG_CHECKING(whether to use thread-local-storage)
if test "$enable_tls" = no || test "$KROC_CCSP_ENABLE_MP" = no || test "x$KROC_RMOX" != "x"; then
 AC_MSG_RESULT(no)
else
 AC_TRY_RUN([
  __thread int bar;

  void foo (void)
  {
   bar = 42;
  }

  int main (void)
  {
   foo (); 
   return bar - 42;
  }], [tls_supported=yes], [tls_supported=no], [tls_supported=no])
 if test "$tls_supported" = yes; then
  AC_MSG_RESULT(yes)
  AC_DEFINE([USE_TLS],1,[define to enable use of thread-local-storage])
 else
  AC_MSG_RESULT(no)
 fi
fi

#}}}

#{{{  library name and link flags

dnl Setup the library name and flags
case "$host_os" in
cygwin*)
 build_shared="no"
 LIBCCSP="ccsp.dll"
 ;;
*Darwin* | *darwin*)
 build_shared="no"
 LIBCCSP="libccsp.dylib"
 ;;
*)
 build_shared="yes"
 SLINKFLAGS="-shared"
 LIBCCSP="libccsp.so"
 ;;
esac

AC_SUBST(SLINKFLAGS)
AC_SUBST(LIBCCSP)
AM_CONDITIONAL(BUILD_SHARED, [test "$build_shared" = "yes"])

#}}}

#{{{  architecture configuration

FRAMEPTR_CFLAGS="-fomit-frame-pointer"
EXTRA_CFLAGS="$EXTRA_CFLAGS -fno-strict-aliasing -Wpointer-arith -DTIME_WITH_SYS_TIME"

bigendian=

case "$host_cpu" in
i386 | i486 | i586 | i686 | x86_64)
  ARCH_DIR=i386
  AC_SUBST(ARCH_DIR)
  AC_DEFINE([TARGET_CPU_386],1,[define if i386 or greater target])
  bigendian=no
  ;;
# XXX: If you're resurrecting any of the entries below, some of this will need
# to go into kroc.m4 rather than here.
#sparc)
#  ARCH_DIR=sparc
#  AC_SUBST(ARCH_DIR)
#  AC_DEFINE([TARGET_CPU_SPARC],1,[define if SPARC target])
#  bigendian=yes
#  ;;
#powerpc64)
#  ARCH_DIR=ppc64
#  AC_SUBST(ARCH_DIR)
#  AC_DEFINE([TARGET_CPU_PPC64],1,[define if PowerPC-64 target])
#  bigendian=yes
#  ;;
#mips|mipsbe|mipsle)
#  ARCH_DIR=mips
#  ARCH_CFLAGS="-mips1 -DMIPS"
#  AC_SUBST(ARCH_DIR)
#  AC_DEFINE([TARGET_CPU_MIPS],1,[define if MIPS target])
#  dnl keep frame-pointer
#  FRAMEPTR_CFLAGS=""
#  if test "$host_cpu" = "mips"; then
#   bigendian=yes
#  fi
#  if test "$host_cpu" = "mipsbe"; then
#   bigendian=yes
#  fi
#  ;;
*)
  echo "unsupported architecture $host_cpu"
  exit 1
  ;;
esac

if test "$bigendian" = yes; then
 AC_DEFINE([TARGET_BIGENDIAN],1,[define if big-endian target])
fi

#}}}
#{{{  OS specific configuration

case "$host_os" in
cygwin)
 AC_DEFINE([TARGET_OS_CYGWIN],1,[define if using cygwin])
 AC_DEFINE([NO_ASM_TYPE_DIRECTIVE],1,[don't produce ELF symbol type directives])
 AC_DEFINE([CCSP_MINIMUM_SLEEP_US],10,[default minimum sleep (microseconds)])
 ;;
*Darwin* | *darwin*)
 AC_DEFINE([TARGET_OS_DARWIN],1,[define if using Darwin])
 AC_DEFINE([NO_ASM_TYPE_DIRECTIVE],1,[don't produce ELF symbol type directives])
 AC_DEFINE([CCSP_MINIMUM_SLEEP_US],2000,[default minimum sleep (microseconds)])
 ;;
*freebsd*)
 AC_DEFINE([TARGET_OS_FREEBSD],1,[define if using FreeBSD])
 AC_DEFINE([CCSP_MINIMUM_SLEEP_US],10,[default minimum sleep (microseconds)])
 ;;
*)
 AC_DEFINE([CCSP_MINIMUM_SLEEP_US],10,[default minimum sleep (microseconds)])
 ;;
esac

#}}}
#{{{  tuning

AC_MSG_CHECKING(parameter tuning)
if test "$tuning" = "size"; then
 AC_MSG_RESULT(size)
 AC_DEFINE([OPTIMISE_SIZE],1,[define to optimise for size over speed])
else
 AC_MSG_RESULT(default)
fi

#}}}

EXTRA_DEFS="$EXTRA_CFLAGS $FRAMEPTR_CFLAGS $RMOX_CFLAGS -DSIGNAL_TYPE_SYSV"
AC_SUBST(EXTRA_DEFS)
AC_SUBST(EXTRA_LDFLAGS)

AC_CONFIG_HEADERS([include/ccsp_config.h])
AC_OUTPUT([Makefile ccsp-1.6.pc common/Makefile include/Makefile include/arch/Makefile include/i386/Makefile kernel/Makefile utils/Makefile])

