################################
#  Autoconf script for libtvm  #
################################

AC_INIT([libtvm], [1.0.0])
AM_CONFIG_HEADER(config.h)
AC_PREREQ(2.59)

AM_INIT_AUTOMAKE([libtvm], [1.0.0])
AC_CANONICAL_HOST

AC_PROG_CC
AC_PROG_RANLIB

AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h stdio.h string.h])
AC_CHECK_HEADERS([sys/types.h math.h inttypes.h setjmp.h])

#########################
#  Configuration Options
#########################

AC_ARG_ENABLE([custom-copy-data], AC_HELP_STRING([--enable-custom-copy-data], [make tvm_copy_data is an externally provided function]), [custom_copy_data=$enableval], [custom_copy_data=no])
AC_ARG_ENABLE([dispatch-switch], AS_HELP_STRING([--enable-dispatch-switch], [dispatch to instructions using switch rather than a jump table]), [dispatch_switch=$enableval], [dispatch_switch=no])
AC_ARG_WITH([endian], AC_HELP_STRING([--with-endian=...], [endian mode: big/little/host, default: host]), [endian=$withval], [endian=host])
AC_ARG_WITH([memory-allocator], AC_HELP_STRING([--with-memory-allocator=...], [select memory allocator: none/malloc, default host dependent]), [memory_allocator=$withval], [memory_allocator=auto])
AC_ARG_WITH([python], AS_HELP_STRING([--with-python=PATH], [use python, optionally specifying the path to the python executable]), [python=$withval], [python=auto])
AC_ARG_ENABLE([occam-pi], AC_HELP_STRING([--disable-occam-pi], [disable support for occam-pi, actual support level depends on memory allocator]), [enable_pi=$enableval], [enable_pi=yes])
AC_ARG_WITH([transputer], AC_HELP_STRING([--with-transputer=...],[transputer to emulate: t2/t4/t8, default is host-dependent]), [transputer=$withval], [transputer=auto])

#########################
#  Host Configuration
#########################

copy_data=memcpy
word_length=4
x_fpu=no
x_os=none
x_endian=little

# CPU configuration
case "$host_cpu" in
  arm*)
    ;;
  h8300)
    x_endian=big
    wordlength=2
    ;;
  i*86)
    x_fpu=yes
    ;;
  mips | mipsbe)
    x_endian=big
    ;;
  mipsle)
    ;;
  msp430)
    x_endian=big
    wordlength=2
    ;;
  ppc)
    x_endian=big
    x_fpu=yes
    ;;
  powerpc64)
    x_endian=big
    x_fpu=yes
    ;;
  sparc)
    x_endian=big
    x_fpu=yes
    ;;
  x86_64)
    x_fpu=yes
    ;;
  *)
    AC_MSG_WARN([Unknown CPU type $host_cpu])
    ;;
esac

# OS configuration
case "$host_os" in
  cygwin*)
    x_os=windows
    ;;
  *Darwin* | *darwin*)
    x_os=darwin
    ;;
  *freebsd*)
    x_os=freebsd
    ;;
  linux*)
    x_os=linux
    ;;
  mingw32*)
    x_os=windows
    ;;
  solaris*)
    x_os=solaris
    ;;
  *)
    copy_data=internal
    x_os=none
    ;;
esac

# Custom copy data
if "x$custom_copy_data" != "xno"; then
  copy_data=custom
fi

# Endian override
case "X$endian" in
  Xlittle | Xbig)
    ;;
  *)
    endian=$x_endian
    ;;
esac

# Memory allocator
case "X$memory_allocator" in
  Xnone | Xmalloc)
    ;;
  *)
    if test "$x_os" = "none"; then
      memory_allocator=none
    else
      memory_allocator=malloc
    fi
    ;;
esac

# Python
if test "x$python" = "xauto"; then
  AC_CHECK_PROGS([PYTHON], [python python2.5 python2.4 python2.3 python2.2 python2.1 python2], [no])
else
  AC_SUBST([PYTHON], $python)
fi
AM_CONDITIONAL(HAVE_PYTHON, test "x$PYTHON" != "xno")

# Transputer type
case "X$transputer" in
  Xt2 | Xt4 | Xt8)
    ;;
  *)
    if test "$word_length" = 2; then
      transputer=t2
    else 
      if test "$x_fpu" = "yes"; then
        transputer=t8
      else
        transputer=t4
      fi
    fi
    ;;
esac

#########################
#  Defines
#########################

TVM_CFLAGS="-Wall"

# Copy data
case "$copy_data" in
  custom)
    AC_DEFINE([TVM_CUSTOM_COPY_DATA], 1, [define to enable tvm_copy_data as an externally provided function])
    ;;
  internal)
    ;;
  memcpy)
    AC_DEFINE([TVM_USE_MEMCPY], 1, [define to use memcpy as tvm_copy_data])
    ;;
esac

# Dispatch switch
if test "x$dispatch_switch" = "xyes"; then
  AC_DEFINE([TVM_DISPATCH_SWITCH], 1, [define to dispatch instructions using switch rather than a jump table])
fi
AM_CONDITIONAL(TVM_DISPATCH_SWITCH, test "x$dispatch_switch" = "xyes")

# Endian
if test "$endian" = "big"; then
  AC_DEFINE([TVM_BIG_ENDIAN], 1, [define for big-endian TVM])
else
  AC_DEFINE([TVM_LITTLE_ENDIAN], 1, [define for little-endian TVM])
fi

# FPU
if test "$x_fpu" = "yes"; then
  AC_DEFINE([TVM_USE_FPU], 1, [define to allow FPU usage])
fi

# Memory allocator
if test "$memory_allocator" = "malloc"; then
  AC_DEFINE([TVM_USE_MALLOC], 1, [define to allow malloc usage])
fi

# occam-pi
if test "x$enable_pi" = "xyes"; then
  AC_DEFINE([TVM_OCCAM_PI], 1, [define to enable occam-pi support])
fi

# OS
case "$x_os" in
  darwin)
    AC_DEFINE([TVM_OS_DARWIN], 1, [define for Darwin TVM])
    ;;
  freebsd)
    AC_DEFINE([TVM_OS_FREEBSD], 1, [define for FreeBSD TVM])
    ;;
  linux)
    AC_DEFINE([TVM_OS_LINUX], 1, [define for Linux TVM])
    ;;
  none)
    AC_DEFINE([TVM_OS_NONE], 1, [define for OS independent TVM])
    ;;
  solaris)
    AC_DEFINE([TVM_OS_SOLARIS], 1, [define for Solaris TVM])
    TVM_CFLAGS="$TVM_CFLAGS -I/opt/SUNWspro/prod/include/cc/"
    ;;
  windows)
    AC_DEFINE([TVM_OS_WINDOWS], 1, [define for Windows TVM])
    ;;
esac

# Transputer
case "$transputer" in
  t2)
    AC_DEFINE([TVM_EMULATE_T2], 1, [define to emulate a T2 processor])
    word_length=2
    ;;
  t4)
    AC_DEFINE([TVM_EMULATE_T4], 1, [define to emulate a T4 processor])
    word_length=4
    ;;
  t8)
    AC_DEFINE([TVM_EMULATE_T8], 1, [define to emulate a T8 processor])
    word_length=4
    ;;
esac

# Word length
AC_DEFINE_UNQUOTED([TVM_WORD_LENGTH], [$word_length], [define emulated word length])

#########################
#  Output
#########################

AC_MSG_NOTICE([----------------------------------------------------])
AC_MSG_NOTICE([tvm_copy_data   : $copy_data])
AC_MSG_NOTICE([dispatch-switch : $dispatch_switch])
AC_MSG_NOTICE([endian          : $endian])
AC_MSG_NOTICE([FPU             : $x_fpu])
AC_MSG_NOTICE([memory allocator: $memory_allocator])
AC_MSG_NOTICE([occam-pi support: $enable_pi])
AC_MSG_NOTICE([OS support      : $x_os])
AC_MSG_NOTICE([transputer      : $transputer])
AC_MSG_NOTICE([word length     : $word_length])
AC_MSG_NOTICE([----------------------------------------------------])

CFLAGS="$TVM_CFLAGS $CFLAGS"

AC_SUBST(TVM_CFLAGS)

AC_CONFIG_HEADERS([tvm_config.h])
AC_OUTPUT([Makefile])
