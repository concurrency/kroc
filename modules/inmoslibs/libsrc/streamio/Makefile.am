@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../forall --search ../convert --search ../hostio

libs = \
	stream.lib
includes = \
	streamio.inc

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
keyscr.occ: $(srcdir)/streamio.inc
keysink.occ: $(srcdir)/streamio.inc
kfile.occ: $(srcdir)/streamio.inc
kkbd.occ: $(srcdir)/streamio.inc
kstdin.occ: $(srcdir)/streamio.inc
scrcont.occ: $(srcdir)/streamio.inc
scrcopy.occ: $(srcdir)/streamio.inc
scrfan.occ: $(srcdir)/streamio.inc
scrmux.occ: $(srcdir)/streamio.inc
scrsink.occ: $(srcdir)/streamio.inc
scrtfarr.occ: $(srcdir)/streamio.inc
sfile.occ: $(srcdir)/streamio.inc
srchar.occ: $(srcdir)/streamio.inc
srint.occ: $(srcdir)/streamio.inc
srint64.occ: $(srcdir)/streamio.inc
srreal32.occ: $(srcdir)/streamio.inc
srreal64.occ: $(srcdir)/streamio.inc
sstdout.occ: $(srcdir)/streamio.inc
swint.occ: $(srcdir)/streamio.inc
swint64.occ: $(srcdir)/streamio.inc
swreal32.occ: $(srcdir)/streamio.inc
swreal64.occ: $(srcdir)/streamio.inc
swsimple.occ: $(srcdir)/streamio.inc
toansi.occ: $(srcdir)/streamio.inc
totvi.occ: $(srcdir)/streamio.inc
## end pideps dependencies
