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
])
