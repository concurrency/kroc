## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	bar.occ \
	casting.occ \
	commstime.occ \
	demo.occ \
	double_echo.occ \
	echoing.occ \
	hello_raw_world.occ \
	hello_seq_world.occ \
	hello_world.occ \
	sort_inside.occ \
	sort_pump.occ \
	test_utils.occ

examples = \
	bar \
	casting \
	commstime \
	demo \
	double_echo \
	echoing \
	hello_raw_world \
	hello_seq_world \
	hello_world \
	sort_inside \
	sort_pump \
	test_utils

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples course --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

%: %.occ
	$(OCCBUILD) --program $<
