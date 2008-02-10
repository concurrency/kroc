
AUTOMAKE_OPTIONS = foreign

SKROC = @SKROC@
SKROC_OPTS = #@SKROC_OPTS@

bin_PROGRAMS = tvm
tvm_SOURCES = \
	firmware.h \
	main.c \
	sffi.c

if OS_WINDOWS
tvm_SOURCES += win32_io.c
else
tvm_SOURCES += unix_io.c
endif

BUILT_SOURCES = firmware.h

firmware.h: firmware.occ
	$(SKROC) --no-std-libs $(SKROC_OPTS) --c -f $@ $<

EXTRA_DIST = \
	firmware.occ \
	tvmspecials.inc

CLEANFILES = \
	firmware.h firmware.tce

