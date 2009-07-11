AC_INIT([dynproc-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_dynproc_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([dynproc.module course.module file.module], ,
              [build_dynproc_examples=no])

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

AC_MSG_CHECKING([whether to build dynproc examples])
AC_MSG_RESULT($build_dynproc_examples)
AM_CONDITIONAL([BUILD_DYNPROC_EXAMPLES], [test $build_dynproc_examples = yes])

AC_OUTPUT([Makefile])
