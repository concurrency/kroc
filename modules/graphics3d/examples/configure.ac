AC_INIT([graphics3d-examples], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_graphics3d_examples=yes

AC_PATH_PROG(GNUCHESS,gnuchess,no,[$PATH])

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module sdlraster.module graphics3d.module maths.module proc.module ttyutil.module useful.module], ,
              [build_graphics3d_examples=no])

AC_MSG_CHECKING([whether to build graphics3d examples])
AC_MSG_RESULT($build_graphics3d_examples)
AM_CONDITIONAL([BUILD_GRAPHICS3D_EXAMPLES], [test $build_graphics3d_examples = yes])

AM_CONDITIONAL([USE_GNUCHESS], [test $GNUCHESS != no])
AC_SUBST(GNUCHESS)

AC_OUTPUT([Makefile])
