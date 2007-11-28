dnl Process this file with autoconf to produce a configure script.

AC_INIT(hereticc.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(hereticc, 1.0)

dnl Checks for programs.
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module], ,
              [AC_MSG_ERROR(Required modules were not found)])

AC_OUTPUT([Makefile])
