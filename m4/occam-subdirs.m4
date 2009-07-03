#
#	Subdir support for cross-building
#	(Based on status.m4 from autoconf.)
#	Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000, 2001,
#	2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
#	Copyright (C) 2009 Adam Sampson
#
#	This program is free software; you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation; either version 2, or (at your option)
#	any later version.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
#	02110-1301, USA.

# OCCAM_CONFIG_TARGET_SUBDIRS(DIR ...)
# --------------------------
# Like AC_CONFIG_SUBDIRS, but for subdirs that should be built for the target
# system -- i.e. with the host platform set to the current directory's target
# platform.
# We define two variables:
# - _AC_LIST_SUBDIRS (also set by AC_CONFIG_SUBDIRS)
#   A statically built list, should contain *all* the arguments of
#   OCCAM_CONFIG_TARGET_SUBDIRS.  The final value is assigned to ac_subdirs_all in
#   the `default' section, and used for --help=recursive.
#   It is also used in _AC_CONFIG_UNIQUE.
#   It makes no sense for arguments which are sh variables.
# - target_subdirs
#   Shell variable built at runtime, so some of these dirs might not be
#   included, if for instance the user refused a part of the tree.
#   This is used in OCCAM_OUTPUT_TARGET_SUBDIRS.
AC_DEFUN([OCCAM_CONFIG_TARGET_SUBDIRS],
[AC_REQUIRE([AC_CONFIG_AUX_DIR_DEFAULT])dnl
m4_foreach_w([_AC_Sub], [$1],
	     [_AC_CONFIG_UNIQUE([SUBDIRS],
				m4_bpatsubst(m4_defn([_AC_Sub]), [:.*]))])dnl
m4_append([_AC_LIST_SUBDIRS], [$1], [
])dnl
AS_LITERAL_IF([$1], [],
	      [AC_DIAGNOSE([syntax], [$0: you should use literals])])dnl
AC_SUBST([target_subdirs], ["$target_subdirs m4_normalize([$1])"])dnl
])


# OCCAM_OUTPUT_TARGET_SUBDIRS
# ------------------
# This is a subroutine of AC_OUTPUT, but it does not go into
# config.status, rather, it is called after running config.status.
m4_define([OCCAM_OUTPUT_TARGET_SUBDIRS],
[
#
# CONFIG_SUBDIRS section.
#
if test "$no_recursion" != yes; then

  # Remove --cache-file and --srcdir arguments so they do not pile up.
  ac_sub_configure_args=
  ac_prev=
  eval "set x $ac_configure_args"
  shift
  for ac_arg
  do
    if test -n "$ac_prev"; then
      ac_prev=
      continue
    fi
    case $ac_arg in
    -cache-file | --cache-file | --cache-fil | --cache-fi \
    | --cache-f | --cache- | --cache | --cach | --cac | --ca | --c)
      ac_prev=cache_file ;;
    -cache-file=* | --cache-file=* | --cache-fil=* | --cache-fi=* \
    | --cache-f=* | --cache-=* | --cache=* | --cach=* | --cac=* | --ca=* \
    | --c=*)
      ;;
    --config-cache | -C)
      ;;
    -srcdir | --srcdir | --srcdi | --srcd | --src | --sr)
      ac_prev=srcdir ;;
    -srcdir=* | --srcdir=* | --srcdi=* | --srcd=* | --src=* | --sr=*)
      ;;
    -prefix | --prefix | --prefi | --pref | --pre | --pr | --p)
      ac_prev=prefix ;;
    -prefix=* | --prefix=* | --prefi=* | --pref=* | --pre=* | --pr=* | --p=*)
      ;;
    *)
      case $ac_arg in
      *\'*) ac_arg=`echo "$ac_arg" | sed "s/'/'\\\\\\\\''/g"` ;;
      esac
      ac_sub_configure_args="$ac_sub_configure_args '$ac_arg'" ;;
    esac
  done

  # Always prepend --prefix to ensure using the same prefix
  # in subdir configurations.
  ac_arg="--prefix=$prefix"
  case $ac_arg in
  *\'*) ac_arg=`echo "$ac_arg" | sed "s/'/'\\\\\\\\''/g"` ;;
  esac
  ac_sub_configure_args="'$ac_arg' $ac_sub_configure_args"

  # Pass --silent
  if test "$silent" = yes; then
    ac_sub_configure_args="--silent $ac_sub_configure_args"
  fi

  # This is where this differs from AC_CONFIG_SUBDIRS: use the current target
  # as the host in the subdir.
  ac_sub_configure_args="$ac_sub_configure_args '--host=$target_alias' 'host_alias=$target_alias'"

  ac_popdir=`pwd`
  for ac_dir in : $target_subdirs; do test "x$ac_dir" = x: && continue

    # Do not complain, so a configure script can configure whichever
    # parts of a large source tree are present.
    test -d "$srcdir/$ac_dir" || continue

    ac_msg="=== configuring in $ac_dir (`pwd`/$ac_dir)"
    _AS_ECHO_LOG([$ac_msg])
    _AS_ECHO([$ac_msg])
    AS_MKDIR_P(["$ac_dir"])
    _AC_SRCDIRS(["$ac_dir"])

    cd "$ac_dir"

    # Check for guested configure; otherwise get Cygnus style configure.
    if test -f "$ac_srcdir/configure.gnu"; then
      ac_sub_configure=$ac_srcdir/configure.gnu
    elif test -f "$ac_srcdir/configure"; then
      ac_sub_configure=$ac_srcdir/configure
    elif test -f "$ac_srcdir/configure.in"; then
      # This should be Cygnus configure.
      ac_sub_configure=$ac_aux_dir/configure
    else
      AC_MSG_WARN([no configuration information is in $ac_dir])
      ac_sub_configure=
    fi

    # The recursion is here.
    if test -n "$ac_sub_configure"; then
      # Make the cache file name correct relative to the subdirectory.
      case $cache_file in
      [[\\/]]* | ?:[[\\/]]* ) ac_sub_cache_file=$cache_file ;;
      *) # Relative name.
	ac_sub_cache_file=$ac_top_build_prefix$cache_file ;;
      esac

      AC_MSG_NOTICE([running $SHELL $ac_sub_configure $ac_sub_configure_args --cache-file=$ac_sub_cache_file --srcdir=$ac_srcdir])
      # The eval makes quoting arguments work.
      eval "\$SHELL \"\$ac_sub_configure\" $ac_sub_configure_args \
	   --cache-file=\"\$ac_sub_cache_file\" --srcdir=\"\$ac_srcdir\"" ||
	AC_MSG_ERROR([$ac_sub_configure failed for $ac_dir])
    fi

    cd "$ac_popdir"
  done
fi
])# OCCAM_OUTPUT_TARGET_SUBDIRS
