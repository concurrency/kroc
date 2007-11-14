dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl
dnl $Source: /proj/ofa/packages/occ21/RCS/configure.in,v $
dnl
dnl $Id: configure.in,v 1.10 1998/06/04 15:13:20 djb1 Exp $
dnl

AC_REVISION($Revision: 1.10 $)
AC_INIT(be/bind1.c)
AM_CONFIG_HEADER(config.h)
AC_PREREQ(2.12)

AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE([occ21], [1.4.0])

dnl Maintainer mode not used

dnl Checks for programs.
AC_CHECK_TOOL(CC,gcc,:)

dnl AM_PROG_CC_STDC
AC_PROG_LN_S
AC_PROG_MAKE_SET
dnl AC_PROG_RANLIB
AC_CHECK_TOOL(RANLIB,ranlib,:)


AM_C_PROTOTYPES

target_cpu_=target_cpu_`echo $target_cpu | tr -d _.`
target_vendor_=target_vendor_`echo $target_vendor | tr -d _.`
target_os_=target_os_`echo $target_os | tr -d _. | sed -e 's/-/_/g'`
ARCH_DEFS="-D${target_cpu_} -D${target_vendor_} -D${target_os_}"

AC_DEFINE_UNQUOTED([TARGET_ALIAS],	"${target_alias}", [define target alias])
AC_DEFINE_UNQUOTED([TARGET_CANONICAL],	"${target}", [define target])
AC_DEFINE_UNQUOTED([TARGET_CPU],	"${target_cpu}", [define target CPU])
AC_DEFINE_UNQUOTED([TARGET_VENDOR],	"${target_vendor}", [define target vendor])
AC_DEFINE_UNQUOTED([TARGET_OS],		"${target_os}", [define target OS])


AC_MSG_CHECKING(whether architecture needs quad alignment)
case "$target_cpu" in
  alpha | mips*)
    AC_DEFINE([NEED_QUAD_ALIGNMENT],1,[define if quad alignment needed])
    AC_MSG_RESULT(yes)
    ;;
  i386 | i486 | i586 | i686 | x86_64)
    AC_DEFINE([TARGET_CPU_IS_I386],1,[define if i386 processor family])
    AC_MSG_RESULT(no)
    ;;
  *)
    AC_MSG_RESULT(no)
    ;;
esac

dnl not really used for LD, rather gcc..
EXTRA_LDFLAGS=""

case "$target_cpu" in
 x86_64)
  ARCH_DEFS="$ARCH_DEFS -m32"
  EXTRA_LDFLAGS="-m32"
  ;;
esac

AC_SUBST(ARCH_DEFS)
AC_SUBST(EXTRA_LDFLAGS)

dnl default endianism check (can be overridden with compiler flags)

default_bigendian=

case "$target_cpu" in
  sparc | mips | mipseb | ppc | ppc64 | powerpc | powerpc64)
    default_bigendian=yes
    ;;
  i386 | i485 | i586 | i686 | x86_64 | mipsel)
    default_bigendian=no
    ;;
  *)
    default_bigendian=no
    ;;
esac

AC_MSG_CHECKING(target is big-endian)
AC_MSG_RESULT($default_bigendian)
if test "$default_bigendian" = yes; then
 AC_DEFINE([TARGET_IS_BIGENDIAN],1,[define if generating code for a big-endian machine])
fi


dnl unaligned access check (needed, perhaps, for packed records)

default_accessaligned=

case "$target_cpu" in
  sparc | mips | mipseb | mipsel)
    default_accessaligned=yes
    ;;
  i386 | i485 | i586 | i686)
    default_accessaligned=no
    ;;
  *)
    default_accessaligned=no
    ;;
esac

AC_MSG_CHECKING(whether architecture requires aligned accesses)
AC_MSG_RESULT($default_accessaligned)
if test "$default_accessaligned" = yes; then
 AC_DEFINE([TARGET_ACCESSALIGNED],1,[define if memory accesses must be aligned])
fi


AC_DEFINE([TARGET_OS_IS_UNIX],1,[define if the target OS is UNIX])
#AC_DEFINE([TARGET_OS_IS_VMS],0,[define if the target OS is VMS])
#AC_DEFINE([TARGET_OS_IS_MSDOS],0,[define if the target OS is MS-DOS])
#AC_DEFINE([TARGET_OS_IS_HELIOS],0,[define if the target OS is HELIOS])
#AC_DEFINE([TARGET_OS_IS_SERVER],0,[define if the target OS is ISERVER])

if test "X$GCC" = Xyes ; then
  AC_DEFINE([COMPILER_IS_GCC],1,[define if using gcc])
fi
#AC_DEFINE(COMPILER_IS_MSC)
#AC_DEFINE(COMPILER_IS_WATCOM)
#AC_DEFINE(COMPILER_IS_LLL)


# One code option

AC_ARG_WITH([code-gen], AS_HELP_STRING([--with-code-gen=OPTION],[kroc-asm (default)]), [with_code_gen=$withval], [with_code_gen=kroc-asm])

AC_MSG_CHECKING(code generation)
case "$with_code_gen" in
  kroc-asm | *)
    AC_DEFINE([CODE_GEN_KROC_ASM],1,[define to target KRoC ETC])
    # doing this sort of thing now upsets new automakes, and we didn't need it anyway..
    #OCC21_CG_SOURCES='code1k.c'
    #OCC21_CG_OBJECTS='code1k.o'
    AC_MSG_RESULT(KROC ETC output and TCO files)
    ;;
esac

# Building occam compiler and occam 2.1
AC_DEFINE([OC],1,[define to build occam compiler])
AC_DEFINE([OCCAM2_5],1,[define to include occam 2.1 support])

dnl
dnl check for arguments
dnl

AC_ARG_ENABLE([udo], AS_HELP_STRING([--disable-udo], [user defined operators (default enabled)]), [enable_udo=$enableval], [enable_udo=yes])
AC_ARG_ENABLE([mobiles], AS_HELP_STRING([--disable-mobiles], [MOBILE declarations (default enabled)]), [enable_mobiles=$enableval], [enable_mobiles=yes])
AC_ARG_ENABLE([init], AS_HELP_STRING([--disable-init], [INITIAL declarations (default enabled)]), [enable_init=$enableval], [enable_init=yes])
AC_ARG_ENABLE([debug], AS_HELP_STRING([--enable-debug], [enable debugging (generates lots of output)]), [enable_debug=$enableval], [enable_debug=no])
AC_ARG_ENABLE([debug-nextsymb], AS_HELP_STRING([--enable-debug-nextsymb], [enable debugging of next symbol]), [enable_debug_nextsymb=$enableval], [enable_debug_nextsymb=no])
AC_ARG_ENABLE([process-priority], AS_HELP_STRING([--disable-process-priority], [disable process priority slot (NEW)]), [enable_process_priority=$enableval], [enable_process_priority=yes])
AC_ARG_ENABLE([backendtrace], AS_HELP_STRING([--enable-backendtrace], [enable tracing of back-end in generated code]), [enable_backendtrace=$enableval], [enable_backendtrace=no])
AC_ARG_ENABLE([var-indent], AS_HELP_STRING([--disable-var-indent], [disable variable indentation support (default enabled)]), [enable_varindent=$enableval], [enable_varindent=yes])

AC_MSG_CHECKING([whether to enable variable indentation suport])
AC_MSG_RESULT($enable_varindent)
if test "$enable_varindent" = yes; then
 AC_DEFINE([SUPPORT_ARBITRARY_INDENT],1,[define to support arbitrary indentation])
fi

AC_MSG_CHECKING(whether to use user defined operators)
AC_MSG_RESULT($enable_udo)
if test "$enable_udo" = yes; then
 AC_DEFINE([USER_DEFINED_OPERATORS],1,[define to enable user-defined-operators])
fi

AC_MSG_CHECKING(whether to allow MOBILE data,channel,process types)
AC_MSG_RESULT($enable_mobiles)
if test "$enable_mobiles" = yes; then
 AC_DEFINE([MOBILES],1,[define to support MOBILEs])
fi

AC_MSG_CHECKING(whether to use INITIAL declarations)
AC_MSG_RESULT($enable_init)
if test "$enable_init" = yes; then
 AC_DEFINE([INITIAL_DECL],1,[define to support INITIAL decls])
fi

AC_MSG_CHECKING(whether to enable debugging)
AC_MSG_RESULT($enable_debug)
if test "$enable_debug" = yes; then
 AC_DEFINE([DEBUG],1,[define to enable debugging])
fi

AC_MSG_CHECKING(whether to enable next-symbol debugging)
AC_MSG_RESULT($enable_debug_nextsymb)
if test "$enable_debug_nextsymb" = yes; then
 AC_DEFINE([DEBUG_NEXTSYMB],1,[define for next-symb debugging])
fi

AC_MSG_CHECKING(whether to enable process priority)
AC_MSG_RESULT($enable_process_priority)
if test "$enable_process_priority" = yes; then
 AC_DEFINE([PROCESS_PRIORITY],1,[define if process-priority desired])
fi

AC_MSG_CHECKING(whether to enable back-end tracing)
AC_MSG_RESULT($enable_backendtrace)
if test "$enable_backendtrace" = yes; then
 AC_DEFINE([BACKENDTRACE],1,[define if back-end debugging required])
fi

dnl Cross compiling of some form ?
AC_SUBST(CC)

dnl Checks for libraries.

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(string.h stdlib.h string.h strings.h limits.h malloc.h sys/types.h stdarg.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_CHECK_SIZEOF(signed int, UNKNOWN)
AC_C_INLINE
AC_MSG_CHECKING(whether can use inline keyword)
can_use_inline=no
if test "X$ac_cv_c_inline" != X; then
  case "$ac_cv_c_inline" in
    inline | yes) can_use_inline=yes
      ;;
    no)
      ;;
    *) can_use_inline=yes
      ;;
  esac
fi
AC_MSG_RESULT($can_use_inline)
if test $can_use_inline = yes ; then
  AC_DEFINE([CAN_USE_INLINE],1,[define if can inline])
fi

AC_TYPE_SIZE_T

dnl Checks for library functions.
AC_FUNC_MEMCMP
AC_FUNC_VPRINTF
AC_CHECK_FUNCS(strspn strstr strtol strrchr)

AC_OUTPUT([Makefile arglib/Makefile extlib/Makefile \
fnilib/Makefile maplib/Makefile suplib/Makefile \
tcofflib/Makefile be/Makefile fe/Makefile misc/Makefile \
include/Makefile ], [test -z "$CONFIG_HEADERS" || echo timestamp > stamp-h])
