@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	atov.occ \
	doprnt.occ \
	sprintf.occ

fmtout_lib_objs = \
	atov.tce \
	doprnt.tce \
	sprintf.tce

fmtout_libs = fmtout.lib
libs = $(fmtout_libs)

all-local: $(libs)

fmtout.lib: $(fmtout_lib_objs)
	$(OCCBUILD) --library fmtout.lib --need convert $(fmtout_lib_objs)

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs)

clean-local:
	$(OCCBUILD) --clean $(libs) $(fmtout_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

sprintf.tce: doprnt.tce
