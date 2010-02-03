#
#       occamdoc definitions for autoconf
#       Copyright (C) 2010 Christian Jacobsen <christian@transterpreter.org>
#
#       This program is free software; you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation; either version 2 of the License, or
#       (at your option) any later version.
#
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#       GNU General Public License for more details.
#
#       You should have received a copy of the GNU General Public License
#       along with this program; if not, write to the Free Software
#       Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#
#
dnl
dnl Check if occamdoc should be built or can be used
dnl sets the automake conditional OCCAMDOC_ENABLED
AC_DEFUN([OCCAM_ENABLE_OCCAMDOC],
[dnl
	AM_PATH_PYTHON([2.2],[],[exit 1])

	AC_CHECK_PROG(XSLTPROC, xsltproc, xsltproc, no)
	AC_SUBST(XSLTPROC)

	AC_MSG_CHECKING(whether to enable occamdoc)
	if test x"$XSLTPROC" = xxsltproc; then
		AC_MSG_RESULT(yes)
	else
		AC_MSG_RESULT(no)
	fi

	AM_CONDITIONAL(OCCAMDOC_ENABLED, test x$XSLTPROC = xxsltproc)
])dnl
