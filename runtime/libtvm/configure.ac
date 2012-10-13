################################
#  Autoconf script for libtvm  #
################################

AC_INIT([libtvm], [1.0.0], [kroc-bugs@kent.ac.uk])
AM_CONFIG_HEADER(config.h)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

AC_PROG_CC
AC_PROG_RANLIB

AC_C_INLINE

AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h stdio.h string.h])
AC_CHECK_HEADERS([sys/types.h math.h inttypes.h])

#########################
#  Configuration Options
#########################

AC_ARG_ENABLE([custom-memory-ops], AC_HELP_STRING([--enable-custom-memory-ops], [provide for overriding memcpy/memset]), [custom_memory_ops=$enableval], [custom_memory_ops=no])
AC_ARG_ENABLE([dispatch-switch], AS_HELP_STRING([--disable-dispatch-switch], [dispatch using jump tables rather than a switch statement]), [dispatch_switch=$enableval], [dispatch_switch=yes])
AC_ARG_WITH([endian], AC_HELP_STRING([--with-endian=...], [endian mode: big/little/host, default: host]), [endian=$withval], [endian=host])
AC_ARG_ENABLE([external-channel-bundles], AC_HELP_STRING([--enable-external-channel-bundles], [enable support for externally hooked channel bundles]), [external_channel_bundles=$enableval], [external_channel_bundles=no])
AC_ARG_WITH([memory-allocator], AC_HELP_STRING([--with-memory-allocator=...], [select memory allocator: none/malloc/tlsf, default host dependent]), [memory_allocator=$withval], [memory_allocator=auto])
AC_ARG_ENABLE([packed-ectx], AS_HELP_STRING([--disable-packed-ectx], [disable execution context packing, can improve code size trading memory usage and compatibility]), [packed_ectx=$enableval], [packed_ectx=yes])
AC_ARG_ENABLE([profiling], AC_HELP_STRING([--enable-profiling], [enable support for (bytecode) profiling]), [profiling=$enableval], [profiling=no])
AC_ARG_WITH([python], AS_HELP_STRING([--with-python=PATH], [use python, optionally specifying the path to the python executable directory]), [python=$withval], [python=])
AC_ARG_ENABLE([occam-pi], AC_HELP_STRING([--disable-occam-pi], [disable support for occam-pi, actual support level depends on memory allocator]), [enable_pi=$enableval], [enable_pi=yes])
AC_ARG_ENABLE([t9000-short-ops], AC_HELP_STRING([--enable-t9000-short-ops],[enable T9000 instructions for dealing with 16bit/8bit data]), [t9000_short_ops=$enableval], [t9000_short_ops=no])
AC_ARG_WITH([transputer], AC_HELP_STRING([--with-transputer=...],[transputer to emulate: t2/t4/t8, default is host-dependent]), [transputer=$withval], [transputer=auto])
AC_ARG_ENABLE([type-shadow], AC_HELP_STRING([--enable-type-shadow], [enable shadowing of registers and memory with type information for debugging]), [type_shadow=$enableval], [type_shadow=no])

#########################
#  Host Configuration
#########################

have_twoword=no
memory_ops=native
memory_interface=native
word_length=4
x_fpu=no
x_os=none
x_endian=little

# CPU configuration
# If you add a new CPU to this list, please also update OCCAM_WORD_SIZE in
# m4/occbuild.m4.
case "$host_cpu" in
  arm*)
    x_fpu=yes
    have_twoword=yes
    ;;
  avr*)
    word_length=2
    memory_interface=avr
    have_twoword=yes
    ;;
  bfin*)
    ;;
  h8300)
    x_endian=big
    word_length=2
    ;;
  i*86)
    x_fpu=yes
    have_twoword=yes
    ;;
  mips | mipsbe)
    x_endian=big
    ;;
  mipsle)
    ;;
  msp430)
    x_endian=big
    word_length=2
    ;;
  powerpc | ppc)
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
    memory_ops=internal
    x_os=none
    ;;
esac

# Custom copy memory operations
if test "x$custom_memory_ops" != "xno"; then
  memory_ops=custom
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
  Xnone | Xmalloc | Xtlsf)
    ;;
  *)
    if test "$x_os" = "none"; then
      if test "x$enable_pi" = "xyes"; then
        memory_allocator=tlsf
      else
        memory_allocator=none
      fi
    else
      memory_allocator=malloc
    fi
    ;;
esac

# Python
PYTHON_PATH="$PATH"
if test "x$python" != "x"; then
  PYTHON_PATH="$python:$PYTHON_PATH"
fi

AC_PATH_PROGS([PYTHON], [python python2.5 python2.4 python2.3 python2.2 python2.1 python2], [python=no], $PYTHON_PATH)
if test "$PYTHON" = "no"; then
  AC_MSG_ERROR([python is required])
fi

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

TVM_CFLAGS="-DTVM_INTERNALS"
OCCAM_CHECK_CFLAGS([-Wall], [TVM_CFLAGS="$TVM_CFLAGS -Wall"])

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

# External Channel Bundles
if test "$external_channel_bundles" = "yes"; then
  if test "x$enable_pi" = "xyes"; then
    AC_DEFINE([TVM_EXTERNAL_CHANNEL_BUNDLES], 1, [define for external channel bundles])
  fi
fi

# FPU
if test "$x_fpu" = "yes"; then
  AC_DEFINE([TVM_USE_FPU], 1, [define to allow FPU usage])
fi

# Inlining
if test "$ac_cv_c_inline" != "no" ; then
  AC_DEFINE([TVM_USE_INLINE], 1, [define to allow inline keyword usage])
fi

# Memory allocator
if test "$memory_allocator" != "none"; then
  AC_DEFINE([TVM_DYNAMIC_MEMORY], 1, [define to alloc dynamic memory allocation])
  if test "$memory_allocator" = "tlsf"; then
    AC_DEFINE([TVM_USE_TLSF], 1, [define to use TLSF dynamic memory allocator])
  else
    AC_DEFINE([TVM_USE_MALLOC], 1, [define to allow malloc usage])
  fi
fi
AM_CONDITIONAL(TVM_TLSF, test "$memory_allocator" = "tlsf")

# Memory interface
case "$memory_interface" in
  avr)
    AC_DEFINE([TVM_MEM_INTF_AVR], 1, [define to use the AVR memory interface])
    ;;
  native)
    ;;
esac

# Memory Operations
case "$memory_ops" in
  custom)
    AC_DEFINE([TVM_CUSTOM_MEM_OPS], 1, [define to enable memcpy/memset overriding])
    ;;
  internal)
    ;;
  native)
    AC_DEFINE([TVM_USE_MEMCPY], 1, [define to use memcpy])
    AC_DEFINE([TVM_USE_MEMSET], 1, [define to use memset])
    ;;
esac

# packed ECTX
if test "x$packed_ectx" = "xyes"; then
  AC_DEFINE([TVM_PACKED_ECTX], 1, [define to enable packed execution context])
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

# Profiling
if test "x$profiling" = "xyes"; then
  AC_DEFINE([TVM_PROFILING], 1, [define to enable profiling])
fi

# T9000 short ops
if test "x$t9000_short_ops" = "xyes"; then
  AC_DEFINE([TVM_SHORT_OPS], 1, [define to T9000 short ops])
fi

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

# Type Shadow
if test "$type_shadow" = "yes"; then
  AC_DEFINE([TVM_TYPE_SHADOW], 1, [define for type shadowing])
fi

# Word length
AC_DEFINE_UNQUOTED([TVM_WORD_LENGTH], [$word_length], [define emulated word length])

# TWOWORDs
if test "$have_twoword" = "yes"; then
  AC_DEFINE([TVM_HAVE_TWOWORD], 1, [define if C compiler supports maths on double words])
fi

#########################
#  Output
#########################

AC_MSG_NOTICE([----------------------------------------------------])
AC_MSG_NOTICE([dispatch-switch : $dispatch_switch])
AC_MSG_NOTICE([TWOWORD         : $have_twoword])
AC_MSG_NOTICE([endian          : $endian])
AC_MSG_NOTICE([external bundles: $external_channel_bundles])
AC_MSG_NOTICE([FPU             : $x_fpu])
AC_MSG_NOTICE([memory allocator: $memory_allocator])
AC_MSG_NOTICE([memory ops      : $memory_ops])
AC_MSG_NOTICE([memory interface: $memory_interface])
AC_MSG_NOTICE([packed ectx     : $packed_ectx])
AC_MSG_NOTICE([occam-pi support: $enable_pi])
AC_MSG_NOTICE([OS support      : $x_os])
AC_MSG_NOTICE([profiling       : $profiling])
AC_MSG_NOTICE([T9000 short ops : $t9000_short_ops])
AC_MSG_NOTICE([transputer      : $transputer])
AC_MSG_NOTICE([type shadow     : $type_shadow])
AC_MSG_NOTICE([word length     : $word_length])
AC_MSG_NOTICE([----------------------------------------------------])

CFLAGS="$TVM_CFLAGS $CFLAGS"

AC_SUBST(TVM_CFLAGS)

AC_CONFIG_HEADERS([tvm_config.h])
AC_OUTPUT([Makefile])
