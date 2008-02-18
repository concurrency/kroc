@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	math.occ \
	random.occ \
	string.occ \
	time.occ \
	useful.inc \
	vector.occ

useful_lib_objs = \
	math.tce \
	random.tce \
	string.tce \
	time.tce \
	vector.tce

useful_libs = useful.lib
useful_includes = useful.inc
libs = $(useful_libs)

all-local: $(libs)

useful.lib: $(useful_lib_objs)
	$(OCCBUILD) --library useful.lib --include useful.inc --need course --need maths $(useful_lib_objs)

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs)

clean-local:
	$(OCCBUILD) --clean $(libs) $(useful_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
