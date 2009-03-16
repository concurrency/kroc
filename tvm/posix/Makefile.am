@SET_MAKE@

AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4
OCCBUILD = @OCCBUILD@

bin_PROGRAMS = tvm
tvm_SOURCES = \
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

all-local: firmware.tbc

firmware.tbc: firmware.occ
	$(OCCBUILD) --no-std-libs --program $<

EXTRA_DIST = \
	firmware.occ \
	introspect.inc \
	tvmspecials.inc

clean-local:
	$(OCCBUILD) --clean firmware.tbc

