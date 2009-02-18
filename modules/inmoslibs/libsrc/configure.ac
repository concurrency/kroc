dnl Process this file with autoconf to produce a configure script.

AC_INIT(README)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(inmoslibs, 1.0)

dnl Checks for programs.
OCCAM_OCCBUILD

OCCAM_PROVIDE(convert.module ioconv.module, convert, none)
OCCAM_PROVIDE(forall.module, forall, none)
OCCAM_PROVIDE(hostio.module solib.module splib.module, hostio, none)
OCCAM_PROVIDE(hostsp.module, hostsp, none)
OCCAM_PROVIDE(dblmath.module maths.module snglmath.module, maths, none)
OCCAM_PROVIDE(stream.module, streamio, none)
OCCAM_PROVIDE(string.module, string, none)

AC_OUTPUT([Makefile forall/Makefile convert/Makefile string/Makefile hostio/Makefile hostsp/Makefile streamio/Makefile maths/Makefile])
