dnl Process this file with autoconf to produce a configure script.

AC_INIT(robot.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(robots, 0.1)

dnl Checks for programs.
OCCAM_OCCBUILD

build_robots=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module sdlraster.module occSDL.module rastergraphics.module rasterio.module maths.module], ,
              [build_robots=no])

AC_MSG_CHECKING([whether to build robots])
AC_MSG_RESULT($build_robots)
AM_CONDITIONAL([BUILD_ROBOTS], [test $build_robots = yes])

AC_OUTPUT([Makefile])
