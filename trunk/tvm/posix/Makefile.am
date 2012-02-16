@SET_MAKE@

AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4
OCCBUILD = @OCCBUILD@

bin_PROGRAMS = tvm
tvm_SOURCES = \
	ffi.c \
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

all-local: tvm-posix.tbc

tvmvtincludedir = $(datadir)/tvm/$(KROC_TARGET_PREFIX)vtinclude
tvmfirmwaredir = $(datadir)/tvm/$(KROC_TARGET_PREFIX)firmware

tvmvtinclude_HEADERS = \
	introspect.inc \
	tvmspecials.inc

tvmfirmware_DATA = \
	tvm-posix.tbc

tvm-posix.tbc: tvm-posix.occ
	$(OCCBUILD) --no-std-libs --program $(srcdir)/tvm-posix.occ

EXTRA_DIST = \
	tvm-posix.occ \
	introspect.inc \
	tvmspecials.inc

clean-local:
	$(OCCBUILD) --clean tvm-posix.tbc

