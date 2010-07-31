@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@ @OCCAM_DEFS@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	make-format.py \
	format.occ \
	math.occ \
	matrix.occ \
	string.occ \
	useful.inc \
	useful-c.c \
	vector.occ

useful_lib_objs = \
	format.tce \
	format-gen.tce \
	math.tce \
	matrix.tce \
	string.tce \
	vector.tce
if TRACE_VIA_FFI
useful_lib_objs += useful-c.o
endif

useful_libs = useful.lib
useful_includes = $(srcdir)/useful.inc
libs = $(useful_libs)
includes = $(useful_includes)

all-local: $(libs)

useful.lib: $(useful_lib_objs)
	$(OCCBUILD) --library useful.lib --include useful.inc --need course --need maths --need time $(useful_lib_objs)

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(useful_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

format-gen.occ: $(srcdir)/make-format.py
	$(PYTHON) $(srcdir)/make-format.py

CLEANFILES = format-gen.occ

format-gen.tce: format.tce
string.tce: math.tce
