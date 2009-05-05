@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@ --kroc-opts -d

EXTRA_DIST = \
	butterflies.occ \
	g3dchess.occ \
	g3dcog2.occ \
	g3dcogs.occ \
	g3dcube.occ \
	g3dcubes.occ \
	g3dtest.occ 

if BUILD_GRAPHICS3D_EXAMPLES
examples = \
	butterflies \
	g3dchess \
	g3dcog2 \
	g3dcogs \
	g3dcube \
	g3dcubes \
	g3dtest
endif

all-local: $(examples)

butterflies: butterflies.occ
	$(OCCBUILD) --program butterflies.occ

g3dchess: g3dchess.occ
	$(OCCBUILD) --program g3dchess.occ

g3dcog2: g3dcog2.occ
	$(OCCBUILD) --program g3dcog2.occ

g3dcogs: g3dcogs.occ
	$(OCCBUILD) --program g3dcogs.occ

g3dcube: g3dcube.occ
	$(OCCBUILD) --program g3dcube.occ

g3dcubes: g3dcubes.occ
	$(OCCBUILD) --program g3dcubes.occ

g3dtest: g3dtest.occ
	$(OCCBUILD) --program g3dtest.occ

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples graphics3d --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

