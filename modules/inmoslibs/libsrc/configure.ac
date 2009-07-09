AC_INIT([inmoslibs], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

OCCAM_PROVIDE(convert.module ioconv.module, convert, none)
OCCAM_PROVIDE(forall.module, forall, none)
OCCAM_PROVIDE(hostio.module, hostio, convert.module)
OCCAM_PROVIDE(solib.module, hostio, convert.module splib.module)
OCCAM_PROVIDE(splib.module, hostio, convert.module)
OCCAM_PROVIDE(hostsp.module, hostsp, hostio.module)
OCCAM_PROVIDE(dblmath.module snglmath.module, maths, none)
OCCAM_PROVIDE(maths.module, maths, dblmath.module snglmath.module)
OCCAM_PROVIDE(stream.module, streamio, convert.module hostio.module)
OCCAM_PROVIDE(string.module, string, convert.module)

AC_OUTPUT([Makefile forall/Makefile convert/Makefile string/Makefile hostio/Makefile hostsp/Makefile streamio/Makefile maths/Makefile])
