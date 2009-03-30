# Configure paths for mysql client library
# Fred Barnes 2006-08-20, based on:
# freetype2.m4 by Marcelo Magallon 2001-10-26, based on:
# gtk.m4 by Owen Taylor
#
# Copyright 2001, 2003 by
# David Turner, Robert Wilhelm, and Werner Lemberg.

# OCCAM_CHECK_MYSQLCLIENT([MINIMUM-VERSION [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
# Test for mysqlclient library and define MYSQLCLIENT_CFLAGS and MYSQLCLIENT_LIBS
# MINIMUM-VERSION is what libtool reports
#
AC_DEFUN([OCCAM_CHECK_MYSQLCLIENT],
  [# Get the cflags and libraries from the mysql_config script
   #
   AC_ARG_WITH([mysqlclient-prefix],
     dnl don't quote AS_HELP_STRING!
     AS_HELP_STRING([--with-mysqlclient-prefix=PREFIX],
                    [Prefix where mysqlclient is installed (optional)]),
     [mysqlclient_config_prefix="$withval"],
     [mysqlclient_config_prefix=""])

   AC_ARG_WITH([mysqlclient-exec-prefix],
     dnl don't quote AS_HELP_STRING!
     AS_HELP_STRING([--with-mysqlclient-exec-prefix=PREFIX],
                    [Exec prefix where mysqlclient is installed (optional)]),
     [mysqlclient_config_exec_prefix="$withval"],
     [mysqlclient_config_exec_prefix=""])

   if test x$mysqlclient_config_exec_prefix != x ; then
     mysqlclient_config_args="$mysqlclient_config_args --exec-prefix=$mysqlclient_config_exec_prefix"
     if test x${MYSQLCLIENT_CONFIG+set} != xset ; then
       MYSQLCLIENT_CONFIG=$mysqlclient_config_exec_prefix/bin/mysql_config
     fi
   fi

   if test x$mysqlclient_config_prefix != x ; then
     mysqlclient_config_args="$mysqlclient_config_args --prefix=$mysqlclient_config_prefix"
     if test x${MYSQLCLIENT_CONFIG+set} != xset ; then
       MYSQLCLIENT_CONFIG=$mysqlclient_config_prefix/bin/mysql_config
     fi
   fi

   AC_PATH_PROG([MYSQLCLIENT_CONFIG], [mysql_config], [no])

   min_mysqlclient_version=m4_if([$1], [], [5.0.1], [$1])
   AC_MSG_CHECKING([for MySQL client library -- version >= $min_mysqlclient_version])
   no_mysqlclient=""
   if test "$MYSQLCLIENT_CONFIG" = "no" ; then
     no_mysqlclient=yes
   else
     MYSQLCLIENT_CFLAGS=$($MYSQLCLIENT_CONFIG $mysqlclient_config_args --cflags)
     MYSQLCLIENT_LIBS=$($MYSQLCLIENT_CONFIG $mysqlclient_config_args --libs)
     mysqlclient_config_major_version=$($MYSQLCLIENT_CONFIG $mysqlclient_config_args --version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/')
     mysqlclient_config_minor_version=$($MYSQLCLIENT_CONFIG $mysqlclient_config_args --version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/')
     mysqlclient_config_micro_version=$($MYSQLCLIENT_CONFIG $mysqlclient_config_args --version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/')
     mysqlclient_min_major_version=$(echo $min_mysqlclient_version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/')
     mysqlclient_min_minor_version=$(echo $min_mysqlclient_version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/')
     mysqlclient_min_micro_version=$(echo $min_mysqlclient_version | \
       sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/')

   fi                 # test "$FT2_CONFIG" = "no"

   if test x$no_mysqlclient = x ; then
     AC_MSG_RESULT([yes])
     m4_if([$2], [], [:], [$2])
   else
     AC_MSG_RESULT([no])
     if test "$MYSQLCLIENT_CONFIG" = "no" ; then
       AC_MSG_WARN([

  The mysql_config installed by mysqlclient could not be found.
  If the MySQL client library was installed in PREFIX, make sure
  PREFIX/bin is in your path, or set the MYSQLCLIENT_CONFIG environment
  variable to the full path to mysql_config.
       ])
     else
       if test x$mysqlclient_config_is_lt = xyes ; then
         AC_MSG_WARN([

  Your installed version of the MySQL client library is too old.
  If you have different versions of the mysqlclient library, make
  sure that correct values for --with-mysqlclient-prefix or
  --with-mysqlclient-exec-prefix are used, or set the MYSQLCLIENT_CONFIG
  environment variable to the full path to mysql_config.
         ])
       fi
     fi

     MYSQLCLIENT_CFLAGS=""
     MYSQLCLIENT_LIBS=""
     m4_if([$3], [], [:], [$3])
   fi

   AC_SUBST([MYSQLCLIENT_CFLAGS])
   AC_SUBST([MYSQLCLIENT_LIBS])])

# end of mysqlclient.m4
