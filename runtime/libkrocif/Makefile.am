## Process this file with automake to produce Makefile.in

@SET_MAKE@

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign no-dependencies

KROC=@KROC@
ILIBR=@ILIBR@

DEFS=@DEFS@ @CCEXTRAOPTS@ @CCINCLUDES@
EXTRALDFLAGS=@LDEXTRAOPTS@ @CCLIBPATH@

SUBDIRS = include io rts support

EXTRA_LIBRARIES = libkrocif.a

libkrocif_a_SOURCES =
libkrocif_a_LIBADD = \
	io/io.o io/input.o io/output.o \
	rts/occam_entry.o rts/rtsmain.o rts/tty.o rts/userproc.o \
	support/spinterf.o support/spunix.o

all-local: libkrocif.a @LIBKROCIF@

if BUILD_SHARED
@LIBKROCIF@: libkrocif.a
	$(CC) @SLINKFLAGS@ $(CFLAGS) $(EXTRALDFLAGS) -lccsp -o @LIBKROCIF@ $(libkrocif_a_LIBADD)
endif

install-exec-local: libkrocif.a @LIBKROCIF@
	$(mkinstalldirs) "$(DESTDIR)$(libdir)"
	$(INSTALL_DATA) libkrocif.a "$(DESTDIR)$(libdir)/libkrocif.a"
if BUILD_SHARED
	$(INSTALL_DATA) @LIBKROCIF@ "$(DESTDIR)$(libdir)/@LIBKROCIF@"
else
	rm -f "$(DESTDIR)$(libdir)/@LIBKROCIF@"
endif

CLEANFILES = @LIBKROCIF@ libkrocif.a *~
