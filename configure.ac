dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl

AC_REVISION($Revision: 1.1 $)
AC_INIT([kroc.in],[1.4.1-pre5],[kroc-bugs@kent.ac.uk])
AC_PREREQ(2.12)

AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(kroc,1.4.1-pre5)

KROC_CANONICAL_TARGET=$target
AC_SUBST(KROC_CANONICAL_TARGET)
KROC_HOST_OS=$host_os
AC_SUBST(KROC_HOST_OS)
KROC_HOST_CPU=$host_cpu
AC_SUBST(KROC_HOST_CPU)

dnl Checks for programs.

AC_PROG_CC

dnl Checks for libraries.

AC_CHECK_LIB(dl, dlsym, dynprocs_supported=yes, dynprocs_supported=no)
AC_MSG_CHECKING(whether dynamic procs are supported)
AC_MSG_RESULT($dynprocs_supported)
if test "$dynprocs_supported" = yes; then
  DLOPTLIB="-ldl"
else
  DLOPTLIB=
fi
AC_SUBST(DLOPTLIB)

AC_CHECK_LIB(elf,elf_begin,dm_with_libelf=yes,dm_with_libelf=no)
AC_MSG_CHECKING(whether dynamic memory debugging uses libelf)
AC_MSG_RESULT($dm_with_libelf)
if test "$dm_with_libelf" = yes;  then
  DMOPTLIB="-lelf"
else
  DMOPTLIB=
fi

needlibrt=

case "$host_os" in
  solaris*)
    needlibrt=yes
    ;;
  *)
    needlibrt=no
    ;;
esac

dnl we need a GNU flavoured sed for the kroc script
AC_CHECK_PROG(HAVE_GNUSED,gnused,yes,no)
AC_CHECK_PROG(HAVE_GSED,gsed,yes,no)
AC_CHECK_PROG(HAVE_SED,sed,yes,no)

if test "$HAVE_GNUSED" = yes; then
 SED=gnused
else
 if test "$HAVE_GSED" = yes; then
  SED=gsed
 else
  SED=sed
 fi
fi
AC_SUBST(SED)

AC_CHECK_LIB(rt,sem_init,[have_librt=yes],[have_librt=no])

case "$host_os" in
*bsd* | *BSD*)
 SAVED_CFLAGS="$CFLAGS"
 CFLAGS="$CFLAGS -pthread"
 AC_CHECK_FUNC(pthread_create,[have_pthread_libcr=yes],[have_pthread_libcr=no])
 CFLAGS="$SAVED_CFLAGS"
 ;;
esac

AC_ARG_ENABLE([pthreads], AS_HELP_STRING([--enable-pthreads], [enable pthreads support (requires pthreads run-time kernel support) (default disabled)]), [enable_pthreads=$enableval], [enable_pthreads=no])
AC_ARG_ENABLE([cttd], AS_HELP_STRING([--enable-cttd], [enable chan-type type-description support (default disabled)]), [enable_cttd=$enableval], [enable_cttd=no])
AC_ARG_ENABLE([pony], AS_HELP_STRING([--enable-pony], [enable KRoC transparent networking (pony) support (default disabled)]), [enable_pony=$enableval], [enable_pony=no])
AC_ARG_ENABLE([user-install], AS_HELP_STRING([--enable-user-install], [install configuration file(s) as dotfiles]), [enable_user_install=$enableval], [enable_user_install=no])
AC_ARG_ENABLE([mp], AS_HELP_STRING([--enable-mp], [enable multi-processor support (default disabled)]), [enable_mp=$enableval], [enable_mp=no])

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


AC_CHECK_LIB(pthread,pthread_create,[have_libpthread=yes],[have_libpthread=no])
AC_MSG_CHECKING([whether to use POSIX threads for blocking syscalls])
if test "$enable_pthreads" = yes; then
 if test "$have_libpthread" = yes; then
  DEXTLIB="-lpthread"
  if test "$needlibrt" = yes; then
    if test "$have_librt" = yes; then
      DEXTLIB="-lrt $DEXTLIB"
    fi
  fi
  DSYSOPTS="-DKROC_USES_PTHREADS"
  AC_MSG_RESULT(yes)
 else
  if test "$have_pthread_libcr" = yes; then
   DEXTLIB="-pthread"
   DSYSOPTS="-DKROC_USES_PTHREADS"
   AC_MSG_RESULT(yes)
  else
   DEXTLIB=
   DSYSOPTS=
   AC_MSG_RESULT(no)
  fi
 fi
else
 DEXTLIB=
 DSYSOPTS=
 AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING([whether to use multi-processor support])
if test "$enable_mp" = yes; then
 dnl This ends up as an option to tranx86
 TENABLEMP="-mp"
 AC_MSG_RESULT(yes)
else
 TENABLEMP=""
 AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING([whether to enable channel-type type-descriptions by default])
AC_MSG_RESULT($enable_cttd)
if test "$enable_pony" = yes; then
 OCCEXTRAOPTS="-zctt -zctuio -zctknsf"
 TRANEXTRAOPTS="--cttd"
 EXTCCEXTRAOPTS="-fomit-frame-pointer -fno-defer-pop -DCHANTYPEDESC -DCHANTYPEUIO -DCHANTYPESTATE"
else
 if test "$enable_cttd" = yes; then
  OCCEXTRAOPTS="-zctt"
  TRANEXTRAOPTS="--cttd"
  EXTCCEXTRAOPTS="-fomit-frame-pointer -fno-defer-pop -DCHANTYPEDESC"
 else
  OCCEXTRAOPTS=
  TRANEXTRAOPTS=
  EXTCCEXTRAOPTS="-fomit-frame-pointer -fno-defer-pop"
 fi
fi

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

if test "$enable_user_install" = yes; then
 CONFFILEPREFIX="."
else
 CONFFILEPREFIX=""
fi


AC_SUBST(DMOPTLIB)
AC_SUBST(DEXTLIB)
AC_SUBST(DSYSOPTS)
AC_SUBST(TENABLEMP)
AC_SUBST(OCCEXTRAOPTS)
AC_SUBST(TRANEXTRAOPTS)
AC_SUBST(EXTCCEXTRAOPTS)
AC_SUBST(NOCCHOME)

dnl new settings for kroc.conf
KROC_ARCHBINDIR="$bindir"
KROC_ARCHLIBDIR="$libdir"
KROC_ARCHINCDIR="$includedir"
KROC_MANDIR="$mandir"
KROC_KROCDIR="$datadir"
KROC_CONFIGDIR="$sysconfdir"

AC_SUBST(KROC_ARCHBINDIR)
AC_SUBST(KROC_ARCHLIBDIR)
AC_SUBST(KROC_ARCHINCDIR)
AC_SUBST(KROC_MANDIR)
AC_SUBST(KROC_KROCDIR)
AC_SUBST(KROC_CONFIGDIR)
AC_SUBST(CONFFILEPREFIX)

AC_OUTPUT([kroc kroc.conf setup.sh setup.csh Makefile])


