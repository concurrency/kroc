AC_INIT([course-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_MAKE_SET
OCCAM_OCCBUILD

build_course_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module time.module], ,
              [build_course_examples=no])

dnl Check word size.
OCCAM_WORD_SIZE([-ge 4], , [build_course_examples=no])

AC_MSG_CHECKING([whether to build course examples])
AC_MSG_RESULT($build_course_examples)
AM_CONDITIONAL([BUILD_COURSE_EXAMPLES], [test $build_course_examples = yes])

AC_OUTPUT([Makefile])
