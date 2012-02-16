#
#	Transterpreter definitions for autoconf
#	Copyright (C) 2009 Adam Sampson <ats@offog.org>
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
dnl Find the "libtvm" library, and set TVM_CFLAGS and TVM_LIBS.
AC_DEFUN([TVM_LIBTVM],
[dnl
AC_REQUIRE([OCCAM_IN_TREE])

TVM_CFLAGS=""
TVM_LIBS=""

if test "x$KROC_BUILD_ROOT" != "x"; then
  TVM_CFLAGS="-I$KROC_BUILD_ROOT/runtime/libtvm -I$KROC_SRC_ROOT/runtime/libtvm"
  TVM_LIBS="-L$KROC_BUILD_ROOT/runtime/libtvm -ltvm"
else
  # We're not in the KRoC source tree, so we need to get the arguments...
  # somehow. I'd suggest using pkgconfig in the future, but for now:
  AC_MSG_ERROR(FIXME: builds outside source tree not implemented)
fi

AC_SUBST(TVM_CFLAGS)
AC_SUBST(TVM_LIBS)
])dnl
dnl
dnl Find the "skroc" program, and define SKROC.
AC_DEFUN([TVM_PROG_SKROC],
[dnl
AC_REQUIRE([OCCAM_IN_TREE])
AC_ARG_VAR(SKROC, [Path to kroc])
if test "x$KROC_BUILD_ROOT" != "x"; then
  SKROC="$KROC_BUILD_ROOT/tools/skroc/skroc --in-tree $KROC_BUILD_ROOT"
else
  AC_CHECK_PROG(SKROC, skroc, skroc, no)
  if test $SKROC = no; then
    AC_MSG_ERROR([skroc not found; set \$KROC or \$PATH appropriately])
  fi
fi
])dnl
