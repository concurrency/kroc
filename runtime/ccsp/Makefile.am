## -*- makefile -*-
##
## Process this file with automake to produce Makefile.in
##
## $Source:$
##
## $Id:$
##

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

SUBDIRS = include utils common kernel
EXTRALDFLAGS=@EXTRA_LDFLAGS@

# This odd workaround is necessary because we don't want to use libtool to
# build shared libraries -- we get automake to build us a static library, then
# use the same objects to build a shared library. (We used to be able to build
# shared libraries directly, but the automake maintainers appear to have
# decided that we shouldn't be allowed to do that any more.)

EXTRA_LIBRARIES = libccsp.a
if BUILD_SHARED
shared_libs = @LIBCCSP@
endif

libccsp_a_SOURCES =
libccsp_a_LIBADD = \
	common/dmem.o \
	common/dtrace.o \
	common/mobproc.o \
	common/rtsmain.o \
	common/timercal.o \
	common/userproc.o \
	kernel/bsyscalls.o \
	kernel/deadlock.o \
	kernel/dynproc.o \
	kernel/entry.o \
	kernel/sched.o \
	kernel/stats.o

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = ccsp-1.6.pc

all-local: libccsp.a $(shared_libs)

if BUILD_SHARED
@LIBCCSP@: libccsp.a
	$(CC) @SLINKFLAGS@ $(CFLAGS) $(EXTRALDFLAGS) -o @LIBCCSP@ $(libccsp_a_LIBADD) @LIBS@
endif

install-exec-local: libccsp.a $(shared_libs)
	$(mkinstalldirs) "$(DESTDIR)$(libdir)"
	$(INSTALL_DATA) libccsp.a "$(DESTDIR)$(libdir)/libccsp.a"
if BUILD_SHARED
	$(INSTALL_DATA) @LIBCCSP@ "$(DESTDIR)$(libdir)/@LIBCCSP@"
else
	rm -f "$(DESTDIR)$(libdir)/@LIBCCSP@"
endif

EXTRA_DIST = README INSTALL COPYRIGHT
DISTCLEANFILES = libccsp.a $(shared_libs)

