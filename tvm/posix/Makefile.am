
AUTOMAKE_OPTIONS = foreign

SKROC = @SKROC@
SKROC_OPTS = #@SKROC_OPTS@

bin_PROGRAMS = tvm
tvm_SOURCES = \
	errhandler.h \
	ffi.c \
	handlers.h \
	kbhandler.h \
	occam_debug.c \
	occam_debug.h \
	scheduler_helpers.c \
	scrhandler.h \
	stiwmain.c \
	tbzutil.c \
	tvm_hooks.c

if OS_WINDOWS
tvm_SOURCES += win32_io.c
else
tvm_SOURCES += unix_io.c
endif

BUILT_SOURCES = kbhandler.h scrhandler.h errhandler.h

kbhandler.h: kbhandler.occ
	$(SKROC) --no-std-libs $(SKROC_OPTS) --c -f $@ $<

scrhandler.h: scrhandler.occ
	$(SKROC) --no-std-libs $(SKROC_OPTS) --c -f $@ $<

errhandler.h: errhandler.occ
	$(SKROC) --no-std-libs $(SKROC_OPTS) --c -f $@ $<

EXTRA_DIST = \
	kbhandler.occ scrhandler.occ errhandler.occ \
	tvmspecials.inc

CLEANFILES = \
	kbhandler.h kbhandler.tce kbhandler.tvmdbg \
	scrhandler.h scrhandler.tce scrhandler.tvmdbg \
	errhandler.h errhandler.tce errhandler.tvmdbg
