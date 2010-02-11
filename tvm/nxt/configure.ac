AC_INIT([tvm-nxt], [1.0.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

AC_PROG_CC
AC_CHECK_TOOL([LD], [ld], [no])
AC_CHECK_TOOL([OBJCOPY], [objcopy], [no])
AC_CHECK_HEADERS([stdio.h stdint.h])


AC_ARG_WITH([nxos-path],
            AC_HELP_STRING([--with-nxos-path=...],
                           [path to NxOS sources]),
            [NXOS_PATH=$withval])
if test "x$NXOS_PATH" == "x"; then
	AC_MSG_ERROR([must specify path to NxOS sources using --with-nxos-path=])
fi
AC_SUBST(NXOS_PATH)


CFLAGS="$CFLAGS -mcpu=arm7tdmi -Os -Wextra -Wall -ffreestanding -fsigned-char -ffunction-sections -std=gnu99 -fdata-sections -fomit-frame-pointer -msoft-float -mthumb-interwork"
CPPFLAGS="$CPPFLAGS \"-I$NXOS_PATH\""
LDFLAGS="$LDFLAGS \"-L$NXOS_PATH\""

LIBGCC=`$CC $CFLAGS -print-libgcc-file-name`
AC_SUBST(LIBGCC)


AC_OUTPUT([Makefile])
