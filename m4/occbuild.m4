#
#	occbuild definitions for autoconf
#	Copyright (C) 2007 University of Kent
#
#	This program is free software; you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation; either version 2 of the License, or
#	(at your option) any later version.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
dnl
AC_DEFUN([OCCAM_OCCBUILD],
[dnl
AC_ARG_VAR(OCCBUILD, [Path to occbuild])
AC_CHECK_PROG(OCCBUILD, occbuild, occbuild, no)
if test $OCCBUILD = no; then
	AC_MSG_ERROR([occbuild not found; set \$OCCBUILD or \$PATH appropriately])
fi
dnl
OCCBUILD_TOOLCHAIN=kroc
AC_ARG_WITH([toolchain],
            AS_HELP_STRING([--with-toolchain=ENV],
                           [select occam toolchain to use (kroc, tvm; default kroc)]),
            [OCCBUILD_TOOLCHAIN="$withval"])
AM_CONDITIONAL(OCCBUILD_KROC, test "x$OCCBUILD_TOOLCHAIN" = "xkroc")
AM_CONDITIONAL(OCCBUILD_TVM, test "x$OCCBUILD_TOOLCHAIN" = "xtvm")
OCCBUILD="$OCCBUILD --toolchain=$OCCBUILD_TOOLCHAIN"
dnl
AC_MSG_CHECKING([for flags needed when compiling FFI objects])
OCCBUILD_CFLAGS=$($OCCBUILD --cflags)
AC_MSG_RESULT([$OCCBUILD_CFLAGS])
AC_SUBST(OCCBUILD_CFLAGS)
])dnl
dnl
dnl OCCAM_INCLUDE(FILES, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
AC_DEFUN([OCCAM_INCLUDE],
[dnl
AC_REQUIRE([OCCAM_OCCBUILD])
AC_MSG_CHECKING([for occam include files $1])
: >conftest.occ
for file in $1; do
	printf >>conftest.occ '#INCLUDE "%s"\n' "$file"
done
printf >>conftest.occ 'PROC main ()\n  SKIP\n:\n'
if AC_RUN_LOG([$OCCBUILD --object conftest.occ >/dev/null]); then
	AC_MSG_RESULT([yes])
	$2
else
	AC_MSG_RESULT([no])
	$3
fi
AC_RUN_LOG([$OCCBUILD --clean conftest.tce])
rm -f conftest.occ
])dnl
dnl
AC_DEFUN([OCCAM_SWIG],
[dnl
HAVE_SWIG_OCCAM=no
AC_CHECK_PROG(SWIG, swig, swig, no)
if test $SWIG != no; then
	SWIG_OCCAM="$SWIG -occampi"
	AC_SUBST([SWIG_OCCAM])
	dnl
	# Check if the -occampi flag is supported
	AC_MSG_CHECKING([for SWIG occam-pi support])
	touch conftest.i
	if $SWIG_OCCAM -module conftest conftest.i >/dev/null 2>&1; then
		AC_MSG_RESULT([yes])
		HAVE_SWIG_OCCAM=yes
		$1
	else
		AC_MSG_RESULT([no])
		$2
	fi
fi
AM_CONDITIONAL(HAVE_SWIG_OCCAM, test "x$HAVE_SWIG_OCCAM" = "xyes")
rm -f conftest.i
])dnl
