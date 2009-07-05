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

# OCCAM_CONFIG_SUBDIRS_FOR(SYSTEM, DIR ...)
# --------------------------
# Configure subdirectories for a particular system (host, target; build not yet
# supported).
#
# This is compatible with AC_CONFIG_SUBDIRS in that it defines
# _AC_LIST_SUBDIRS (so --help=recursive will work), but it defines
# occam_subdirs rather than just subdirs as a variable, and you will need to
# call OCCAM_OUTPUT_SUBDIRS explicitly.
AC_DEFUN([OCCAM_CONFIG_SUBDIRS_FOR],
[AC_REQUIRE([AC_CONFIG_AUX_DIR_DEFAULT])dnl

# Call AC_CONFIG_SUBDIRS, but undo the effect it has on subdirs. This allows
# autoreconf to detect that we're using subdirs.
ac_pop_subdirs="$subdirs"
AC_CONFIG_SUBDIRS([$2])
subdirs="$ac_pop_subdirs"

AC_SUBST([occam_subdirs], ["$occam_subdirs m4_normalize([$2])"])dnl
occam_subdirs_$1="$occam_subdirs_$1 m4_normalize([$2])"
])


# OCCAM_CONFIG_SUBDIRS(DIR ...)
# --------------------------
# Equivalent to AC_CONFIG_SUBDIRS -- i.e. configure subdirs for the host
# system.
AC_DEFUN([OCCAM_CONFIG_SUBDIRS],
[
OCCAM_CONFIG_SUBDIRS_FOR([host], [$1])dnl
])


# OCCAM_CONFIG_TARGET_SUBDIRS(DIR ...)
# --------------------------
# Configure subdirs for the target system.
AC_DEFUN([OCCAM_CONFIG_TARGET_SUBDIRS],
[
OCCAM_CONFIG_SUBDIRS_FOR([target], [$1])dnl
])


# OCCAM_OUTPUT_SUBDIRS
# ------------------
# This should be called after AC_OUTPUT.
m4_define([OCCAM_OUTPUT_SUBDIRS],
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

  # Pass KROC_BUILD_ROOT and KROC_SRC_ROOT down to subdirs.
  ac_sub_configure_args="$ac_sub_configure_args 'KROC_BUILD_ROOT=$KROC_BUILD_ROOT' 'KROC_SRC_ROOT=$KROC_SRC_ROOT'"

  ac_popdir=`pwd`
  for ac_dir in : $occam_subdirs; do test "x$ac_dir" = x: && continue

    # Do not complain, so a configure script can configure whichever
    # parts of a large source tree are present.
    test -d "$srcdir/$ac_dir" || continue

    ac_configure_for=""
    for ac_subdir in : $occam_subdirs_target; do test "x$ac_subdir" = x: && continue
      if test "x$ac_dir" = "x$ac_subdir"; then
        # This is where this differs from AC_CONFIG_SUBDIRS: use the current
        # target as the host in the subdir, if one was specified.
        if test "x$target_alias" != "x"; then
          ac_configure_for="'--host=$target_alias' 'host_alias=$target_alias'"
        fi
      fi
    done

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

      AC_MSG_NOTICE([running $SHELL $ac_sub_configure $ac_sub_configure_args $ac_configure_for --cache-file=$ac_sub_cache_file --srcdir=$ac_srcdir])
      # The eval makes quoting arguments work.
      eval "\$SHELL \"\$ac_sub_configure\" $ac_sub_configure_args $ac_configure_for \
	   --cache-file=\"\$ac_sub_cache_file\" --srcdir=\"\$ac_srcdir\"" ||
	AC_MSG_ERROR([$ac_sub_configure failed for $ac_dir])
    fi

    cd "$ac_popdir"
  done
fi
])# OCCAM_OUTPUT_SUBDIRS
