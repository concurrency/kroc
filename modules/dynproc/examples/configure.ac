dnl Process this file with autoconf to produce a configure script.

AC_INIT(testdyn.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(dynproc-examples, 1.0)

dnl Checks for programs.
AC_CHECK_TOOL(KROC,kroc)
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([dynproc.module course.module file.module], ,
              [AC_MSG_ERROR(Required modules not found)])

dnl Checks for operating system features.
case "$host_os" in
  Darwin* | darwin* | cygwin)
    build_shared=no
    ;;
  *)
    build_shared=yes
    ;;
esac
AM_CONDITIONAL(BUILD_SHARED, [test "$build_shared" = "yes"])

AC_OUTPUT([Makefile])

