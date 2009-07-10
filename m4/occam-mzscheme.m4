#
#	KRoC-specific MzScheme tests for autoconf
#	Copyright (C) 2007, 2008 University of Kent
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
dnl Look for mzc and mzscheme, and set MZC and MZSCHEME accordingly.
dnl If both are found, define the conditional HAVE_SCHEME_TOOLS.
AC_DEFUN([OCCAM_MZSCHEME],
[dnl
AC_REQUIRE([OCCAM_IN_TREE])

AC_CHECK_TOOL([MZC], [mzc], [no])
AC_CHECK_TOOL([MZSCHEME], [mzscheme], [no])

# FIXME: Test that the version is appropriate.

# Currently we can't do a separate-object-dir build of Scheme tools.
if test "x$KROC_BUILD_ROOT" != "x$KROC_SRC_ROOT"; then
  MZC=no
  MZSCHEME=no
fi

AM_CONDITIONAL([HAVE_SCHEME_TOOLS],
               [test "x$MZC" != "xno" && test "x$MZSCHEME" != "xno"])
])dnl
