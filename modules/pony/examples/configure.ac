AC_INIT([pony-examples], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_pony_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([pony.module course.module convert.module useful.module], ,
              [build_pony_examples=no])
OCCAM_INCLUDE([sdlraster.module],
              [have_sdlraster=yes], [have_sdlraster=no])
AM_CONDITIONAL([HAVE_SDLRASTER], [test $have_sdlraster = yes])

AC_MSG_CHECKING([whether to build pony examples])
AC_MSG_RESULT($build_pony_examples)
AM_CONDITIONAL([BUILD_PONY_EXAMPLES], [test $build_pony_examples = yes])

AC_OUTPUT([Makefile])
