dnl Process this file with autoconf to produce a configure script.

AC_INIT(bounce.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(occade-examples, 0.1)

dnl Checks for programs.
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([occade.module maths.module], ,
              [AC_MSG_ERROR(Required modules not found)])

AC_OUTPUT([Makefile images/Makefile])
