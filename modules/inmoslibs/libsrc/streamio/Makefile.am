@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../forall --search ../convert --search ../hostio

libs = \
	stream.lib
includes = \
	$(srcdir)/streamio.inc

EXTRA_DIST = \
	keyscr.occ \
	keysink.occ \
	kfile.occ \
	kkbd.occ \
	kstdin.occ \
	scrcont.occ \
	scrcopy.occ \
	scrfan.occ \
	scrmux.occ \
	scrsink.occ \
	scrtfarr.occ \
	sfile.occ \
	srchar.occ \
	srint.occ \
	srint64.occ \
	srreal32.occ \
	srreal64.occ \
	sstdout.occ \
	streamio.inc \
	swint.occ \
	swint64.occ \
	swreal32.occ \
	swreal64.occ \
	swsimple.occ \
	toansi.occ \
	totvi.occ

stream_lib_objs = \
	keyscr.tce \
	keysink.tce \
	kfile.tce \
	kkbd.tce \
	kstdin.tce \
	scrcont.tce \
	scrcopy.tce \
	scrfan.tce \
	scrmux.tce \
	scrsink.tce \
	scrtfarr.tce \
	sfile.tce \
	srchar.tce \
	srint.tce \
	srint64.tce \
	srreal32.tce \
	srreal64.tce \
	sstdout.tce \
	swint.tce \
	swint64.tce \
	swreal32.tce \
	swreal64.tce \
	swsimple.tce \
	toansi.tce \
	totvi.tce

all-local: $(libs)

stream.lib: $(stream_lib_objs)
	$(OCCBUILD) --library stream.lib --need convert --need hostio --include streamio.inc $(stream_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(stream_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
keyscr.tce: $(srcdir)/streamio.inc
keysink.tce: $(srcdir)/streamio.inc
kfile.tce: $(srcdir)/streamio.inc
kkbd.tce: $(srcdir)/streamio.inc
kstdin.tce: $(srcdir)/streamio.inc
scrcont.tce: $(srcdir)/streamio.inc
scrcopy.tce: $(srcdir)/streamio.inc
scrfan.tce: $(srcdir)/streamio.inc
scrmux.tce: $(srcdir)/streamio.inc
scrsink.tce: $(srcdir)/streamio.inc
scrtfarr.tce: $(srcdir)/streamio.inc
sfile.tce: $(srcdir)/streamio.inc
srchar.tce: $(srcdir)/streamio.inc
srint.tce: $(srcdir)/streamio.inc
srint64.tce: $(srcdir)/streamio.inc
srreal32.tce: $(srcdir)/streamio.inc
srreal64.tce: $(srcdir)/streamio.inc
sstdout.tce: $(srcdir)/streamio.inc
swint.tce: $(srcdir)/streamio.inc
swint64.tce: $(srcdir)/streamio.inc
swreal32.tce: $(srcdir)/streamio.inc
swreal64.tce: $(srcdir)/streamio.inc
swsimple.tce: $(srcdir)/streamio.inc
toansi.tce: $(srcdir)/streamio.inc
totvi.tce: $(srcdir)/streamio.inc
## end pideps dependencies
