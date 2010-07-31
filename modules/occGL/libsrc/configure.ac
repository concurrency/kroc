AC_INIT([occGL], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
OCCAM_SWIG

build_occGL=yes

dnl Checks for libraries.
OCCAM_HAVE_OPENGL
CPPFLAGS="$CPPFLAGS $GL_CFLAGS"
AC_CHECK_HEADERS([GL/gl.h GL/glu.h], , [build_occGL=no])
old_LIBS="$LIBS"
LIBS="$LIBS $GL_LIBS"
AC_CHECK_FUNC(glBegin, , [build_occGL=no])
AC_CHECK_FUNC(gluCylinder, , [build_occGL=no])
LIBS="$old_LIBS"

AC_MSG_CHECKING([whether to build occGL.module])
AC_MSG_RESULT($build_occGL)
AM_CONDITIONAL(BUILD_OCCGL, [test $build_occGL = yes])
OCCAM_PROVIDE(occGL.module, , , [test $build_occGL = yes])

AC_OUTPUT([Makefile])
