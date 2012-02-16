AC_INIT([trap-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_trap_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([trap.module course.module useful.module], ,
              [build_trap_examples=no])

AC_MSG_CHECKING([whether to build trap examples])
AC_MSG_RESULT($build_trap_examples)
AM_CONDITIONAL([BUILD_TRAP_EXAMPLES], [test $build_trap_examples = yes])

AC_OUTPUT([Makefile])
