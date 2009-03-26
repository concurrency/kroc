#
#	Socket library definitions for autoconf
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
dnl Set SOCK_LIBS to any additional libraries necessary for networking.
AC_DEFUN([OCCAM_SOCK_LIBS],
[dnl
SOCK_LIBS=""

old_LIBS="$LIBS"
LIBS=""
AC_SEARCH_LIBS(gethostent, [nsl])
AC_SEARCH_LIBS(setsockopt, [socket net])
AC_SEARCH_LIBS(connect,    [inet])
SOCK_LIBS="$LIBS"
LIBS="$LIBS"

AC_SUBST(SOCK_LIBS)
])dnl
