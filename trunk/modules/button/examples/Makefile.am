@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	button_sample.occ 

if BUILD_BUTTON_EXAMPLES
examples = \
	button_sample
endif

all-local: $(examples)

button_sample: button_sample.occ
	$(OCCBUILD) --program $(srcdir)/button_sample.occ

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples button --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) button_sample.tce

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
