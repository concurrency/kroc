@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	mysqlclient.occ \
	simple.occ \
	test1.occ \
	test2.occ

examples = \
	mysqlclient \
	simple \
	test1 \
	test2

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples moa --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
