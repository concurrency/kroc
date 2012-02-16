AC_INIT([trap], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_trap=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([file.module sock.module useful.module cif.module], ,
              [build_trap=no])

AC_MSG_CHECKING([whether to build trap.module])
AC_MSG_RESULT($build_trap)
AM_CONDITIONAL(BUILD_TRAP, [test $build_trap = yes])
OCCAM_PROVIDE(selector.module trap.module, , , [test $build_trap = yes])

AC_OUTPUT([Makefile])
