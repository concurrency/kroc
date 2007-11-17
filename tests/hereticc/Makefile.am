@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	hereticc.occ

hereticc: hereticc.occ
	$(OCCBUILD) --program $<

all-local: hereticc

install-exec-local: hereticc
	$(OCCBUILD) --install-examples --prefix "$(prefix)" --destdir "$(DESTDIR)" $(hereticc)

clean-local:
	$(OCCBUILD) --clean hereticc

SUFFIXES = .occ

%: %.occ
	$(OCCBUILD) --program $<

