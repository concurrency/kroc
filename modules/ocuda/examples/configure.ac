AC_INIT([ocuda-examples], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_ocuda_examples=yes
build_ocuda_sdl_examples=yes

OCCAM_HAVE_CUDA(4.2, sm_20, ,[build_ocuda_examples=no])

OCCAM_INCLUDE([course.module ocuda.module], ,[build_ocuda_examples=no])
OCCAM_INCLUDE([sdlraster.module rastergraphics.module rastertext.module useful.module rasterio.module], ,[build_ocuda_sdl_examples=no])

AC_MSG_CHECKING([whether to build ocuda examples])
AC_MSG_RESULT($build_ocuda_examples)
AC_MSG_CHECKING([whether to build ocuda SDL examples])
AC_MSG_RESULT($build_ocuda_sdl_examples)

AM_CONDITIONAL([BUILD_OCUDA_EXAMPLES], [test $build_ocuda_examples = yes])
AM_CONDITIONAL([BUILD_OCUDA_SDL_EXAMPLES], [test $build_ocuda_sdl_examples = yes])

AC_OUTPUT([Makefile])

