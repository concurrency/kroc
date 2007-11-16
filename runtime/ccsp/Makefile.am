## -*- makefile -*-
##
## Process this file with automake to produce Makefile.in
##
## $Source:$
##
## $Id:$
##

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign

SUBDIRS = include utils common kernel
EXTRALDFLAGS=@EXTRA_LDFLAGS@

# This odd workaround is necessary because we don't want to use libtool to
# build shared libraries -- we get automake to build us a static library, then
# use the same objects to build a shared library. (We used to be able to build
# shared libraries directly, but the automake maintainers appear to have
# decided that we shouldn't be allowed to do that any more.)

EXTRA_LIBRARIES = libccsp.a

libccsp_a_SOURCES =
libccsp_a_LIBADD = \
	common/dmem.o \
	common/mobproc.o \
	common/pgroup.o \
	common/rtsmain.o \
	common/userproc.o \
	common/dtrace.o \
	common/timercal.o \
	kernel/bsyscalls.o \
	kernel/cspdriver.o \
	kernel/deadlock.o \
	kernel/dynproc.o \
	kernel/entry.o \
	kernel/sched.o \
	kernel/stats.o

include_HEADERS = \
	include/ccsp.h \
	include/ccsp_config.h \
	include/ccsp_consts.h \
	include/ccsp_if.h \
	include/ccsp_cif.h \
	include/ccsp_cif_stubs.h \
	include/ccsp_stats.h \
	include/ccsp_timer.h \
	include/dmem_if.h \
	include/kiface.h \
	include/kitable.h \
	include/mobile_types.h \
	include/mobproc.h \
	include/typedesc.h \
	include/mwsync.h \
	include/ukcthreads_types.h

all-local: @LIBCCSP@

@LIBCCSP@: libccsp.a
	$(CC) @SLINKFLAGS@ $(CFLAGS) $(EXTRALDFLAGS) -o @LIBCCSP@ $(libccsp_a_LIBADD)

install-exec-local: @LIBCCSP@
	$(mkinstalldirs) "$(DESTDIR)$(libdir)"
	$(INSTALL_DATA) "@LIBCCSP@" "$(DESTDIR)$(libdir)/@LIBCCSP@"

EXTRA_DIST = README INSTALL COPYRIGHT preconfigure
DISTCLEANFILES = libccsp.a @LIBCCSP@

