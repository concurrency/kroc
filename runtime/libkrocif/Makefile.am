## Process this file with automake to produce Makefile.in

@SET_MAKE@

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

KROC=@KROC@
ILIBR=@ILIBR@

DEFS = @DEFS@ @KROC_CCSP_CFLAGS@ @KROC_CCSP_CINCPATH@
EXTRALDFLAGS = @KROC_CCSP_LDFLAGS@ @KROC_CCSP_LIBPATH@

SUBDIRS = include io rts support

EXTRA_LIBRARIES = libkrocif.a
if BUILD_SHARED
shared_libs = @LIBKROCIF@
endif

libkrocif_a_SOURCES =
libkrocif_a_LIBADD = \
	io/io.o io/input.o io/output.o \
	rts/error.o \
	rts/occam_entry.o \
	rts/tty.o rts/userproc.o \
	support/spinterf.o support/spunix.o

all-local: libkrocif.a $(shared_libs)

if BUILD_SHARED
@LIBKROCIF@: libkrocif.a
	$(CC) @SLINKFLAGS@ $(CFLAGS) $(EXTRALDFLAGS) -lccsp -o @LIBKROCIF@ $(libkrocif_a_LIBADD)
endif

install-exec-local: libkrocif.a $(shared_libs) rts/rtsmain.o
	$(mkinstalldirs) "$(DESTDIR)$(libdir)"
	$(INSTALL_DATA) libkrocif.a "$(DESTDIR)$(libdir)/libkrocif.a"
	$(INSTALL_DATA) rts/rtsmain.o "$(DESTDIR)$(libdir)/rtsmain.o"
if BUILD_SHARED
	$(INSTALL_DATA) @LIBKROCIF@ "$(DESTDIR)$(libdir)/@LIBKROCIF@"
else
	rm -f "$(DESTDIR)$(libdir)/@LIBKROCIF@"
endif

CLEANFILES = libkrocif.a $(shared_libs) *~
