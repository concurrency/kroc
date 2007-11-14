dnl Process this file with autoconf to produce a configure script.

AC_INIT(gl.i)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(occGL, 0.1)

AC_CANONICAL_HOST
AC_CANONICAL_TARGET
case $host in
	*-apple-darwin*)
		
		AM_CONDITIONAL(MAC_OSX, test xyes = xyes)
	;;
	*)
		AM_CONDITIONAL(MAC_OSX, test xyes = xno)
	;;
esac

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
OCCAM_SWIG

dnl Checks for libraries.
MDL_HAVE_OPENGL
AC_CHECK_HEADERS([GL/gl.h GL/glu.h], [], [have_GL=no])
#AC_CHECK_LIB(GL, glFlush, [], [AC_MSG_ERROR(OpenGL is required)])

AC_OUTPUT([Makefile])
