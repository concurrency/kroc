@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	button_sample.occ 

examples = \
	button_sample

all-local: $(examples)

button_sample: button_sample.occ
	$(OCCBUILD) --program $<

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples button --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) button_sample.tce

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
