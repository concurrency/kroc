@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../forall --search ../hostio

EXTRA_DIST = \
	close.occ \
	command.occ \
	exit.occ \
	flush.occ \
	getenv.occ \
	getkey.occ \
	gets.occ \
	hostunix.occ \
	open.occ \
	pollkey.occ \
	remove.occ \
	rename.occ \
	seek.occ \
	spread.occ \
	spserver.occ \
	spwrite.occ \
	system.occ \
	tell.occ \
	time.occ

hostsp_lib_objs = \
	close.tce \
	command.tce \
	exit.tce \
	flush.tce \
	getenv.tce \
	getkey.tce \
	gets.tce \
	open.tce \
	pollkey.tce \
	remove.tce \
	rename.tce \
	seek.tce \
	spread.tce \
	spserver.tce \
	spwrite.tce \
	system.tce \
	tell.tce \
	time.tce

all-local: hostsp.lib

hostsp.lib: $(hostsp_lib_objs)
	$(OCCBUILD) --library hostsp.lib --need hostio $(hostsp_lib_objs)

install-exec-local: hostsp.lib
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" hostsp.lib

clean-local:
	$(OCCBUILD) --clean hostsp.lib $(hostsp_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
spserver.tce: $(srcdir)/hostunix.occ
## end pideps dependencies
