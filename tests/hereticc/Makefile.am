@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
# Disable usage checking for hereticc, since it's pathologically bad with occ21
# at the moment.
OCCBUILD = @OCCBUILD@ --occ21-opts '-NU'

EXTRA_DIST = \
	hereticc.occ

tests = \
	hereticc

all-local: $(tests)

hereticc: hereticc.occ
	$(OCCBUILD) --program $<

install-exec-local: $(tests)
	$(OCCBUILD) --install-examples hereticc --prefix "$(prefix)" --destdir "$(DESTDIR)" $(tests)

clean-local:
	$(OCCBUILD) --clean $(tests)

SUFFIXES = .occ
