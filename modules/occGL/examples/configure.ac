AC_INIT([occGL-examples], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_occGL_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([occGL.module occSDL.module course.module], ,
              [build_occGL_examples=no])

AC_MSG_CHECKING([whether to build occGL examples])
AC_MSG_RESULT($build_occGL_examples)
AM_CONDITIONAL([BUILD_OCCGL_EXAMPLES], [test $build_occGL_examples = yes])

AC_OUTPUT([Makefile])
