dnl Process this file with autoconf to produce a configure script.

AC_INIT(thread-ring.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(shootout, 0.1)

dnl Checks for programs.
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module file.module convert.module], ,
              [AC_MSG_ERROR(Required modules not found)])

AC_OUTPUT([Makefile])
