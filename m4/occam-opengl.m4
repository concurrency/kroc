dnl @synopsis OCCAM_HAVE_OPENGL
dnl
dnl Search for OpenGL. We search first for Mesa (a GPL'ed version of
dnl Mesa) before a vendor's version of OpenGL, unless we were
dnl specifically asked not to with `--with-Mesa=no' or
dnl `--without-Mesa'.
dnl
dnl The four "standard" OpenGL libraries are searched for: "-lGL",
dnl "-lGLU", "-lGLX" (or "-lMesaGL", "-lMesaGLU" as the case may be)
dnl and "-lglut".
dnl
dnl All of the libraries that are found (since "-lglut" or "-lGLX"
dnl might be missing) are added to the shell output variable "GL_LIBS",
dnl along with any other libraries that are necessary to successfully
dnl link an OpenGL application (e.g. the X11 libraries). Care has been
dnl taken to make sure that all of the libraries in "GL_LIBS" are
dnl listed in the proper order.
dnl
dnl Additionally, the shell output variable "GL_CFLAGS" is set to any
dnl flags (e.g. "-I" flags) that are necessary to successfully compile
dnl an OpenGL application.
dnl
dnl The following shell variable (which are not output variables) are
dnl also set to either "yes" or "no" (depending on which libraries were
dnl found) to help you determine exactly what was found.
dnl
dnl   have_GL
dnl   have_GLU
dnl   have_GLX
dnl   have_glut
dnl
dnl A complete little toy "Automake `make distcheck'" package of how to
dnl use this macro is available at:
dnl
dnl   ftp://ftp.slac.stanford.edu/users/langston/autoconf/ac_opengl-0.01.tar.gz
dnl
dnl Please note that as the ac_opengl macro and the toy example
dnl evolves, the version number increases, so you may have to adjust
dnl the above URL accordingly.
dnl
dnl minor bugfix by ahmet inan <auto@ainan.org>
dnl
dnl @category InstalledPackages
dnl @author Matthew D. Langston <langston@SLAC.Stanford.EDU>
dnl @version 2002-09-25
dnl @license GPLWithACException

AC_DEFUN([OCCAM_HAVE_OPENGL],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_PATH_X])
  AC_REQUIRE([AC_PATH_XTRA])
  AC_REQUIRE([OCCAM_MACOS_OPENGL])

  dnl Check for Mesa first, unless we were asked not to.
  AC_ARG_WITH([--with-Mesa],
              [Prefer the Mesa library over a vendor's native OpenGL library (default=yes)],
              with_Mesa_help_string)
  AC_ARG_ENABLE(Mesa, $with_Mesa_help_string, use_Mesa=$enableval, use_Mesa=yes)

  if test x"$use_Mesa" = xyes; then
     GL_search_list="MesaGL   GL"
    GLU_search_list="MesaGLU GLU"
    GLX_search_list="MesaGLX GLX"
  else
     GL_search_list="GL  MesaGL"
    GLU_search_list="GLU MesaGLU"
    GLX_search_list="GLX MesaGLX"
  fi

  AC_LANG_SAVE
  AC_LANG_C

  dnl If we are running under X11 then add in the appropriate libraries.
  if test x"$no_x" != xyes; then
    dnl Add everything we need to compile and link X programs to GL_X_CFLAGS
    dnl and GL_X_LIBS.
    GL_CFLAGS="$X_CFLAGS"
    GL_X_LIBS="$X_PRE_LIBS $X_LIBS -lX11 -lXext -lXmu -lXt -lXi $X_EXTRA_LIBS"
  fi

  GL_save_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="$GL_CFLAGS"

  GL_save_LIBS="$LIBS"
  LIBS="$GL_X_LIBS $OCCAM_MACOS_OPENGL_LDFLAGS"

  # Save the "AC_MSG_RESULT file descriptor" to FD 8.
  exec 8>&AC_FD_MSG

  # Temporarily turn off AC_MSG_RESULT so that the user gets pretty
  # messages.
  exec AC_FD_MSG>/dev/null

  AC_SEARCH_LIBS(glAccum,          $GL_search_list, have_GL=yes,   have_GL=no)
  AC_SEARCH_LIBS(gluBeginCurve,   $GLU_search_list, have_GLU=yes,  have_GLU=no)
  AC_SEARCH_LIBS(glXChooseVisual, $GLX_search_list, have_GLX=yes,  have_GLX=no)
  AC_SEARCH_LIBS(glutInit,        glut,             have_glut=yes, have_glut=no)

  # Restore pretty messages.
  exec AC_FD_MSG>&8

  if test -n "$LIBS"; then
    GL_LIBS="$LIBS $OCCAM_MACOS_OPENGL_LDFLAGS"
    AC_SUBST(GL_CFLAGS)
    AC_SUBST(GL_LIBS)
  else
    GL_CFLAGS=
  fi

  dnl Reset GL_X_LIBS regardless, since it was just a temporary variable
  dnl and we don't want to be global namespace polluters.
  GL_X_LIBS=

  LIBS="$GL_save_LIBS"
  CPPFLAGS="$GL_save_CPPFLAGS"

  AC_LANG_RESTORE
])

dnl Using OpenGL on some versions of MacOS requires some extra linker flags.
dnl This returns them in OCCAM_MACOS_OPENGL_LDFLAGS if necessary.
AC_DEFUN([OCCAM_MACOS_OPENGL],
[
  OCCAM_MACOS_OPENGL_LDFLAGS=""
  case "$target_os" in
    darwin*)
      # Work around a MacOS 10.5 bug:
      #   http://developer.apple.com/qa/qa2007/qa1567.html
      dylib="/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib"
      if test -f $dylib; then
        OCCAM_MACOS_OPENGL_LDFLAGS="-Wl,-dylib_file,$dylib:$dylib"
      fi
      ;; 
  esac
])
