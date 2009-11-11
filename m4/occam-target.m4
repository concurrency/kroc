# Checks for tools for the target platform.
# These are just AC_CHECK_TARGET_TOOL[S] from autoconf 2.63, renamed;
# in older versions of autoconf, they don't work correctly. See:
# http://www.mail-archive.com/autoconf@gnu.org/msg17471.html

# Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000, 2001,
# 2002, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.


# OCCAM_CHECK_TARGET_TOOL(VARIABLE, PROG-TO-CHECK-FOR, [VALUE-IF-NOT-FOUND], [PATH])
# -------------------------------------------------------------------------------
# (Use different variables $1 and ac_ct_$1 so that cache vars don't conflict.)
AC_DEFUN([OCCAM_CHECK_TARGET_TOOL],
[AC_REQUIRE([AC_CANONICAL_TARGET])dnl
AC_CHECK_PROG([$1], [$target_alias-$2], [$target_alias-$2], , [$4])
if test -z "$ac_cv_prog_$1"; then
  if test "$build" = "$target"; then
    ac_ct_$1=$$1
    _AC_CHECK_PROG([ac_ct_$1], [$2], [$2], [$3], [$4])
    $1=$ac_ct_$1
  else
    $1="$3"
  fi
else
  $1="$ac_cv_prog_$1"
fi
])# OCCAM_CHECK_TARGET_TOOL


# OCCAM_CHECK_TARGET_TOOLS(VARIABLE, PROGS-TO-CHECK-FOR, [VALUE-IF-NOT-FOUND],
#	                [PATH])
# -------------------------------------------------------------------------
# Check for each tool in PROGS-TO-CHECK-FOR with the cross prefix. If
# none can be found with a cross prefix, then use the first one that
# was found without the cross prefix.
AC_DEFUN([OCCAM_CHECK_TARGET_TOOLS],
[AC_REQUIRE([AC_CANONICAL_TARGET])dnl
for ac_prog in $2
do
  AC_CHECK_PROG([$1],
		[$target_alias-$ac_prog], [$target_alias-$ac_prog],,
		[$4])
  test -n "$$1" && break
done
if test -z "$$1"; then
  if test "$build" = "$target"; then
    ac_ct_$1=$$1
    AC_CHECK_PROGS([ac_ct_$1], [$2], [$3], [$4])
    $1=$ac_ct_$1
  else
    $1="$3"
  fi
fi
])# OCCAM_CHECK_TARGET_TOOLS

