dnl Process this file with autoconf to produce a configure script.

AC_INIT(g3dtest.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(graphics3d-examples, 0.1)

dnl Checks for programs.
OCCAM_OCCBUILD

build_graphics3d_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module sdlraster.module graphics3d.module maths.module], ,
              [build_graphics3d_examples=no])

AC_MSG_CHECKING([whether to build graphics3d examples])
AC_MSG_RESULT($build_graphics3d_examples)
AM_CONDITIONAL([BUILD_GRAPHICS3D_EXAMPLES], [test $build_graphics3d_examples = yes])

AC_OUTPUT([Makefile])
