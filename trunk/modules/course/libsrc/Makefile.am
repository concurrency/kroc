## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	utils.doc \
	utils.occ \
	string.occ \
	stringbuf.occ \
	demo_cycles.occ \
	demo_nets.occ \
	file_in.occ \
	float_io.occ \
	random.doc \
	random.occ \
	consts.inc \
	shared_screen.occ \
	shared_screen.inc

course_lib_objs = \
	utils.tce \
	string.tce \
	stringbuf.tce \
	demo_cycles.tce \
	demo_nets.tce \
	file_in.tce \
	float_io.tce \
	random.tce

shared_screen_lib_objs = \
	shared_screen.tce

dist_man3_MANS = libcourse-cycles.3 libcourse-nets.3 libcourse-utils.3

all-local: course.lib shared_screen.lib ss.lib

course.lib: $(course_lib_objs)
	$(OCCBUILD) --library course.lib --include consts.inc $(course_lib_objs)

shared_screen.lib: $(shared_screen_lib_objs)
	$(OCCBUILD) --library shared_screen.lib --need course --include shared_screen.inc $(shared_screen_lib_objs)

# Build ss.lib too for backwards compatibility.
ss.lib: $(shared_screen_lib_objs)
	$(OCCBUILD) --library ss.lib --need course --include shared_screen.inc $(shared_screen_lib_objs)

install-exec-local: course.lib $(srcdir)/consts.inc shared_screen.lib ss.lib $(srcdir)/shared_screen.inc
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" course.lib $(srcdir)/consts.inc shared_screen.lib ss.lib $(srcdir)/shared_screen.inc

clean-local:
	$(OCCBUILD) --clean course.lib $(course_lib_objs) shared_screen.lib ss.lib $(shared_screen_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

$(shared_screen_lib_objs): course.lib
demo_nets.tce: demo_cycles.tce
string.tce: utils.tce
stringbuf.tce: float_io.tce
