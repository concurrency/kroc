
AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

SKROC = @SKROC@
SKROC_OPTS = #@SKROC_OPTS@

bin_PROGRAMS = tvm
tvm_SOURCES = \
	firmware.h \
	introspect.c \
	main.c \
	sffi.c \
	tbc.c \
	vm.c

if OS_WINDOWS
tvm_SOURCES += win32_io.c
else
tvm_SOURCES += unix_io.c
endif

BUILT_SOURCES = firmware.h

firmware.h: firmware.occ
	$(SKROC) --no-std-libs $(SKROC_OPTS) --c -f firmware.h firmware.occ

EXTRA_DIST = \
	firmware.occ \
	tvmspecials.inc

CLEANFILES = \
	firmware.h firmware.tce

