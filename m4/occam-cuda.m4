dnl @synopsis OCCAM_HAVE_CUDA
dnl
dnl Search for a CUDA compiler (nvcc) that we can use with KRoC.
dnl 
dnl @author Fred Barnes <frmb@kent.ac.uk>
dnl @license public domain


dnl OCCAM_HAVE_CUDA([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for CUDA and define CUDA_CC, CUDA_CFLAGS and CUDA_LIBS
dnl
AC_DEFUN([OCCAM_HAVE_CUDA],
[dnl
AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([OCCAM_TOOLCHAIN])
AC_REQUIRE([KROC_CCSP_FLAGS])

KROC_CCSP_FLAGS

AC_ARG_WITH(cuda-prefix,[  --with-cuda-prefix         Prefix where CUDA is installed (optional)],
	    cuda_prefix="$withval", cuda_prefix="")
AC_ARG_ENABLE(cudatest, [  --disable-cudatest         Do not try to compile and run a CUDA test program],
		, enable_cudatest=yes)

if test "x$cuda_prefix" != "x" ; then
  if test "x${NVCC+set}" != "xset" ; then
    CUDA_CC="$cuda_prefix/bin/nvcc"
  else
    CUDA_CC="$NVCC"
  fi
else
  if test -x /usr/local/bin/nvcc ; then
    cuda_prefix="/usr/local"
    CUDA_CC="/usr/local/bin/nvcc"
  elif test -x /usr/bin/nvcc; then
    cuda_prefix="/usr"
    CUDA_CC="/usr/bin/nvcc"
  elif test -x /usr/local/cuda/bin/nvcc ; then
    cuda_prefix="/usr/local/cuda"
    CUDA_CC="/usr/local/cuda/bin/nvcc"
  fi
fi

AC_PATH_PROG([CUDA_CC], [nvcc], [no])

min_cuda_version=ifelse([$1], ,4.1,$1)
AC_MSG_CHECKING([for CUDA version >= $min_cuda_version])
no_cuda=""
if test "$CUDA_CC" = "no" ; then
  no_cuda="yes"
else
  CUDA_CFLAGS="-Xcompiler $(printf '%s' "$KROC_CCSP_CFLAGS" | tr ' ' ',') -arch sm_13"
  CUDA_LIBS="-lcudart"
  cuda_major_version=$($CUDA_CC --version | \
  	grep 'release' | sed 's/.*release \([[0-9]]*\).\([[0-9]]*\).*/\1/')
  cuda_minor_version=$($CUDA_CC --version | \
  	grep 'release' | sed 's/.*release \([[0-9]]*\).\([[0-9]]*\).*/\2/')

  if test "x$enable_cudatest" = "xyes" ; then
    ac_save_CFLAGS="$CFLAGS"
    ac_save_CXXFLAG="$XXFLAGS"
    ac_save_LIBS="$LIBS"
    ac_save_CC="$CC"

    CFLAGS="$CUDA_CFLAGS"
    CC="$CUDA_CC"

    rm -f conf.cudatest
    AC_TRY_RUN([
#include <stdio.h>
#include <cuda_runtime.h>

int main (int argc, char **argv)
{
	int major, minor;
	FILE *fp;

	fp = fopen ("conf.cudatest", "a");
	if (fp) {
		fclose (fp);
	}

	/* Note: HP/UX 9 writes to sscanf strings, but not an issue for KRoC (nor probably CUDA) */
	if (sscanf ("$min_cuda_version", "%d.%d", &major, &minor) != 2) {
		printf ("%s, bad version string\n", "$min_cuda_version");
		exit (1);
	}

	if (($cuda_major_version > major) || (($cuda_major_version == major) && ($cuda_minor_version >= minor))) {
		return 0;
	} else {
		printf ("\n*** '%s --version' returned %d.%d, but the minimum version\n", "$CUDA_CC", $cuda_major_version, $cuda_minor_version);
		printf ("*** of CUDA required is %d.%d.\n", major, minor);
		printf ("*** If CUDA is installed somewhere different, use the --with-cuda-prefix=...\n");
		printf ("*** option or NVCC environment variable to specify the location of the CUDA compiler\n");
		return 1;
	}
}
],, no_cuda=yes,[echo $ac_n "cross compiling; assumed OK.. $ac_c"])

    CC="$ac_save_CC"
    CFLAGS="$ac_save_CFLAGS"
    CXXFLAGS="$ac_save_CXXFLAGS"
    LIBS="$ac_save_LIBS"

    rm -f conf.cudatest
  fi

fi


if test "x$no_cuda" = "x" ; then
  AC_MSG_RESULT(yes)
  ifelse([$2], , :, [$2])
else
  AC_MSG_RESULT(no)
  echo "*** The CUDA compiler (nvcc) could not be found."
  echo "*** If CUDA is installed, use --with-cuda-prefix=..."
  echo "*** to specify its location."
  ifelse([$3], , :, [$3])

  CUDA_CC=""
  CUDA_CFLAGS=""
  CUDA_LIBS=""
fi

AC_SUBST(CUDA_CC)
AC_SUBST(CUDA_CFLAGS)
AC_SUBST(CUDA_LIBS)

])dnl

