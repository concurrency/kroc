@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
# Disable usage checking for hereticc, since it's pathologically bad with occ21
# at the moment.
OCCBUILD = @OCCBUILD@ --occ21-opts '-NU'

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

