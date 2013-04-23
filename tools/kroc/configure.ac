AC_INIT([kroc], [1.6.0], [kroc-bugs@kent.ac.uk])

AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
AC_CHECK_PROGS(SED, gnused gsed sed)

dnl Checks for target tools.
OCCAM_CHECK_TARGET_TOOL([TARGET_AR], [ar], [no])
OCCAM_CHECK_TARGET_TOOLS([TARGET_CC], [gcc c89 cc], [no])
OCCAM_CHECK_TARGET_TOOL([TARGET_RANLIB], [ranlib], [:])

OCCAM_TOOLCHAIN
KROC_RMOX_BUILD
AC_SUBST(OCCBUILD_TOOLCHAIN)
if test $OCCBUILD_TOOLCHAIN = kroc; then
  KROC_CCSP_FLAGS
fi

OCCAM_TARGET_PREFIX
KROC_CANONICAL_TARGET=$target
AC_SUBST(KROC_CANONICAL_TARGET)
KROC_TARGET_OS=$target_os
AC_SUBST(KROC_TARGET_OS)
KROC_TARGET_CPU=$target_cpu
AC_SUBST(KROC_TARGET_CPU)

dnl check for the dynamic linker (just so we can report it in KRoC really)
AC_CHECK_TOOL(LD,ld)

AC_ARG_ENABLE([user-install], AS_HELP_STRING([--enable-user-install], [install configuration file(s) as dotfiles]), [enable_user_install=$enableval], [enable_user_install=no])

AC_ARG_WITH([nocc], AS_HELP_STRING([--with-nocc=...], [location of NOCC (experimental)]), [nocc_dir=$withval], [nocc_dir=])

AC_MSG_CHECKING(location of NOCC (experimental))
AC_MSG_RESULT($nocc_dir)
AC_PATH_PROG([NOCC_EXE],[nocc],,[$nocc_dir:/bin:/usr/bin:/usr/local/bin])

dnl AC_ARG_ENABLE([dm-debug], AS_HELP_STRING([--enable-dm-debug], [enable dynamic memory debugging (requires run-time kernel support)]), [enable_dm_debug=$enableval], [enable_dm_debug=no])
dnl 
dnl 
dnl AC_CHECK_HEADER(gelf.h, [have_gelf_h=yes], [have_gelf_h=no])
dnl AC_MSG_CHECKING(whether gelf.h is present)
dnl AC_MSG_RESULT($have_gelf_h)
dnl 
dnl AC_CHECK_HEADER(libelf.h, [have_libelf_h=yes], [have_libelf_h=no])
dnl AC_MSG_CHECKING(whether libelf.h is present)
dnl AC_MSG_RESULT($have_libelf_h)
dnl 
dnl AC_CHECK_LIB(elf, elf_begin, have_libelf=yes, have_libelf=no)
dnl AC_MSG_CHECKING(whether libelf is installed and has elf_begin)
dnl AC_MSG_RESULT($have_libelf)
dnl 
dnl AC_MSG_CHECKING(whether to enable dynamic memory debugging)
dnl AC_MSG_RESULT($enable_dm_debug)
dnl 
dnl uselibelf=no
dnl if test "$enable_dm_debug" = yes; then
dnl  AC_DEFINE([DM_DEBUG],1,[define to enable dynamic memory debugging])
dnl   if test "$have_libelf_h" = yes; then
dnl    if test "$have_libelf" = yes; then
dnl     if test "$have_gelf_h" = yes; then
dnl      uselibelf=yes
dnl     fi
dnl    fi
dnl  fi
dnl fi

if test -n "$nocc_dir"; then
 NOCCHOME=$nocc_dir
else
 NOCCHOME=
fi

dnl AC_MSG_CHECKING(whether to link with libelf)
dnl AC_MSG_RESULT($uselibelf)
dnl 
dnl if test "$uselibelf" = yes; then
dnl  AC_DEFINE([DM_DEBUG_LIBELF],1,[define to use libelf with dynamic memory debugging])
dnl  if test [ "$DEXTLIB" = "" ]; then
dnl   DEXTLIB="-lelf"
dnl  else
dnl   DEXTLIB="$DEXTLIB -lelf"
dnl  fi
dnl  if test [ "$DSYSOPTS" = "" ]; then
dnl   DSYSOPTS="-DKROC_USES_LIBELF"
dnl  else
dnl   DSYSOPTS="$DSYSOPTS -DKROC_USES_LIBELF"
dnl  fi
dnl fi
dnl 

case "$host_os" in
*Darwin* | *darwin*)
 ENV_LIBRARY_PATH="DYLD_LIBRARY_PATH"
 ;;
*)
 ENV_LIBRARY_PATH="LD_LIBRARY_PATH"
 ;;
esac

if test "$enable_user_install" = yes; then
 CONFFILEPREFIX="."
else
 CONFFILEPREFIX=""
fi

AC_SUBST(NOCCHOME)

dnl new settings for kroc
KROC_ARCHBINDIR="$bindir"
KROC_ARCHLIBDIR="$libdir"
KROC_ARCHINCDIR="$includedir/kroc"
KROC_MANDIR="$mandir"
KROC_KROCDIR="$datadir/kroc"
KROC_CONFIGDIR="$sysconfdir/kroc"
KROC_EXAMPLESDIR="$libdir/kroc/${KROC_TARGET_PREFIX}examples"
TVM_ARCHINCDIR="$includedir/tvm"
TVM_BINDIR="$bindir"
TVM_LIBDIR="$libdir/${KROC_TARGET_PREFIX}tvm"
TVM_TVMDIR="$datadir/tvm"
TVM_EXAMPLESDIR="$TVM_LIBDIR/${KROC_TARGET_PREFIX}examples"
ACLOCALDIR="${prefix}/share/aclocal"

AC_SUBST(KROC_ARCHBINDIR)
AC_SUBST(KROC_ARCHLIBDIR)
AC_SUBST(KROC_ARCHINCDIR)
AC_SUBST(KROC_MANDIR)
AC_SUBST(KROC_KROCDIR)
AC_SUBST(KROC_CONFIGDIR)
AC_SUBST(KROC_EXAMPLESDIR)
AC_SUBST(TVM_ARCHINCDIR)
AC_SUBST(TVM_BINDIR)
AC_SUBST(TVM_LIBDIR)
AC_SUBST(TVM_TVMDIR)
AC_SUBST(TVM_EXAMPLESDIR)
AC_SUBST(ACLOCALDIR)
AC_SUBST(CONFFILEPREFIX)
AC_SUBST(ENV_LIBRARY_PATH)

AC_OUTPUT([Makefile])
