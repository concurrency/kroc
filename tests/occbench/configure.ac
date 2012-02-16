AC_INIT([occbench], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_benchmarks=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module maths.module], ,
              [build_benchmarks=no])

dnl Check word size.
OCCAM_WORD_SIZE([-ge 4], , [build_benchmarks=no])

AC_MSG_CHECKING([whether to build benchmarks])
AC_MSG_RESULT($build_benchmarks)
AM_CONDITIONAL([BUILD_BENCHMARKS], [test $build_benchmarks = yes])

AC_OUTPUT([Makefile benchmarks/Makefile])
