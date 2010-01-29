#
#	Compiler flag happiness test for autoconf
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
dnl Test whether the C compiler accepts some set of flags (e.g. -Wall) when
dnl compiling source files to object files.
dnl OCCAM_CHECK_CFLAGS(FLAGS, [ACTION-IF-ACCEPTED], [ACTION-IF-NOT-ACCEPTED])
AC_DEFUN([OCCAM_CHECK_CFLAGS],
[dnl
AC_LANG_ASSERT([C])

AC_MSG_CHECKING([whether $CC accepts $1])

save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS $1"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])], [success=yes], [success=no])
CFLAGS="$save_CFLAGS"

if test $success = yes; then
  AC_MSG_RESULT([yes])
  $2
else
  AC_MSG_RESULT([no])
  $3
fi

])dnl
