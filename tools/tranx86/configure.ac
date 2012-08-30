AC_INIT([tranx86], [1.4.1], [kroc-bugs@kent.ac.uk])
AC_CONFIG_SRCDIR([main.c])
AM_CONFIG_HEADER(config.h)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

AC_DEFINE_UNQUOTED([TARGET_CANONICAL],"${target}",[define canonical target])

dnl Checks for programs.
AC_PROG_CC
AM_PROG_CC_STDC
AC_PROG_MAKE_SET

KROC_CCSP_FLAGS

ARCH_DEFS=""
GNUASFLAGS=""

case "$host_cpu" in
 x86_64)
  ARCH_DEFS="-m32"
  GNUASFLAGS="--32"
  ;;
 i386)
  case "$target" in
   *Darwin* | *darwin*)
    GNUASFLAGS='-arch", "i386'
    ;;
  esac
  ;;
esac

AC_SUBST(ARCH_DEFS)

case "$host_cpu" in
 i386 | i486 | i586 | i686 | x86_64)

  saved_CFLAGS="$CFLAGS"
  CFLAGS="$ARCH_DEFS $CFLAGS"
  AC_TRY_COMPILE(,[__asm__ __volatile__ ("\n\tfucom\n\tfildl (%eax)\n\tfildd (%ebx)\n");],old_as=no,old_as=yes)
  CFLAGS="$saved_CFLAGS"
  AC_ARG_ENABLE([force-old-as], AS_HELP_STRING([--enable-force-old-as], [force generation of .byte codes for certain i386 instructions]), [old_as=$enableval], [])
  AC_MSG_CHECKING(whether assembler fails to understand some mnemonics)
  AC_MSG_RESULT($old_as)
  if test "$old_as" = yes; then
   AC_DEFINE([OLD_ASSEMBLER],1,[define if ia32 assembler does not understand fucom, fildl or fildd])
  fi

 
  ;;
esac

AC_ARG_ENABLE([mp], AS_HELP_STRING ([--disable-mp], [disable multi-processor support]), [enable_mp=$enableval], [enable_mp=yes])
AC_ARG_ENABLE([udc], AS_HELP_STRING([--enable-udc], [enable (old) user-defined channels]), [enable_udc=$enableval], [enable_udc=no])
AC_ARG_ENABLE([process-priority], AS_HELP_STRING([--disable-process-priority], [disable process priorities]), [enable_process_priority=$enableval], [enable_process_priority=yes])

case "$host_cpu" in
 i386 | i486 | i586 | i686 | x86_64)
  saved_CFLAGS="$CFLAGS"
  CFLAGS="$ARCH_DEFS $CFLAGS"

  AC_RUN_IFELSE([
  int main (void)
  {
   __asm__ __volatile__ ("		\n"
    "	xorl	%%eax, %%eax		\n"
    "	je	.LTMP1			\n"
    ".LTMP2:				\n"
    "	movl	\$1, %%eax		\n"
    ".LTMP1:				\n"
    "	nop				\n"
    ::: "cc", "memory");
    return 0;
  }], [can_do_dotlabels=yes], [can_do_dotlabels=no], [can_do_dotlabels=no])

  CFLAGS="$saved_CFLAGS"
  ;;
 mips)
  can_do_dotlabels=no
  ;;
 sparc)
  can_do_dotlabels=no
  ;;
 powerpc64)
  can_do_dotlabels=no
  ;;
esac

AC_MSG_CHECKING(whether assembler supports dot-labels)
AC_MSG_RESULT($can_do_dotlabels)
if test "$can_do_dotlabels" = yes; then
 AC_DEFINE([CAN_DO_DOTLABELS],1,[define if the assembler supports .Lnnn style local-labels])
fi

AC_MSG_CHECKING(whether to enable user-defined channels)
AC_MSG_RESULT($enable_udc)
if test "$enable_udc" = yes; then
 AC_DEFINE([USER_DEFINED_CHANNELS],1,[define to enable user-defined channels])
fi

AC_MSG_CHECKING(whether to enable process priority handling)
AC_MSG_RESULT($enable_process_priority)
if test "$enable_process_priority" = yes; then
 AC_DEFINE([PROCESS_PRIORITY],1,[define to enable process priority])
fi

AC_MSG_CHECKING([whether to include support for multi-processor code-generation])
AC_MSG_RESULT($enable_mp)
if test "$enable_mp" = yes; then
 AC_DEFINE([MP_ENABLE],1,[define to enable multi-processor code-generation support])
fi

case "$host_cpu" in
 i486 | i586 | i686 | x86_64)
  AC_DEFINE([HOST_CPU_IS_I486],1,[define for 486/586/686 and compatible CPUs])
  ;;
 i386)
  AC_DEFINE([HOST_CPU_IS_I386],1,[define for 386 CPUs])
  ;;
 mips)
  AC_DEFINE([HOST_CPU_IS_MIPS],1,[define for MIPS CPUs])
  ;;
 sparc)
  AC_DEFINE([HOST_CPU_IS_SPARC],1,[define for Sparc CPUs])
  ;;
 powerpc64)
  AC_DEFINE([HOST_CPU_IS_PPC64],1,[define for PowerPC-64 CPUs])
  ;;
esac


case "$host_os" in
 solaris*)
  AC_DEFINE([HOST_OS_IS_SOLARIS],1,[define for Sun Solaris])
  ;;
 linux*)
  AC_DEFINE([HOST_OS_IS_LINUX],1,[define for Linux])
  ;;
 cygwin*)
  AC_DEFINE([HOST_OS_IS_CYGWIN],1,[define for cygwin])
  ;;
 irix*)
  AC_DEFINE([HOST_OS_IS_IRIX],1,[define for SGI IRIX])
  ;;
esac

AC_MSG_CHECKING([extra flags for GNU assembler])
if test "$GNUASFLAGS" = ""; then
 AC_MSG_RESULT([none])
else
 AC_MSG_RESULT([$GNUASFLAGS])
fi

if test "$GNUASFLAGS" != ""; then
 AC_DEFINE_UNQUOTED([GNUASFLAGS],"$GNUASFLAGS",[extra flags for GNU assembler])
fi

dnl Checks for libraries.

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(string.h malloc.h stdarg.h stdlib.h string.h strings.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.

AC_OUTPUT([Makefile])

