AM_CFLAGS = -Wall @AM_CFLAGS@

INCLUDES  = -I$(top_builddir) -I$(top_builddir)/interpreter \
						-I$(top_srcdir) -I$(top_srcdir)/interpreter

CLEANFILES = kbhandler.h kbhandler.tce kbhandler_tmp.tvmdbg kbhandler_tmp.h kbhandler.tvmdbg \
						 scrhandler.h scrhandler.tce scrhandler_tmp.tvmdbg scrhandler_tmp.h scrhandler.tvmdbg\
						 errhandler.h errhandler.tce errhandler_tmp.tvmdbg errhandler_tmp.h errhandler.tvmdg



# FIXME: Find a better way of doing this???
OLD_PATH := $(PATH)
override PATH = @top_srcdir@/skroc:@top_srcdir@/slinker:$(OLD_PATH)

libdir = $(prefix)/lib/transterpreter

if MAKE_STIW
lib_LIBRARIES = libstiw.a 
libstiw_a_SOURCES = stiwmain.c stiw.h 
EXTRA_includedir = $(libdir)
EXTRA_include_HEADERS = tvmspecials.inc
endif

if MAKE_STIWTVM
bin_PROGRAMS = tvm
tvm_SOURCES = stiwmain.c occam_debug.c occam_debug.h scheduler_helpers.c ffi.c tvm_hooks.c tbzutil.c

if WIN32
tvm_SOURCES += win32_io.c
else
tvm_SOURCES += unix_io.c
endif

EXTRA_tvm_SOURCES = kbhandler.h scrhandler.h errhandler.h
tvm_LDADD = $(top_srcdir)/interpreter/libinterpreter.a

if LINUX 
tvm_LDADD += -ldl
endif

if SOLARIS
tvm_LDADD += -ldl -lrt -L/opt/SUNWspro/lib -lm9x -lsunmath
endif

if FPU_SUPPORT
tvm_LDADD += -lm
endif

tvm_CFLAGS = -DSTIWTVM

BUILT_SOURCES = kbhandler.h scrhandler.h errhandler.h

occ21_FLAGS = 
if PI_SUPPORT
occ21_FLAGS += OCCAMPI
endif

kbhandler.h: kbhandler.occ
	./mkhandler.sh kb

scrhandler.h: scrhandler.occ
	./mkhandler.sh scr $(occ21_FLAGS)

errhandler.h: errhandler.occ
	./mkhandler.sh err
endif

