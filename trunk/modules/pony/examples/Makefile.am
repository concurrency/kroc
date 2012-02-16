## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	bmbackground.occ \
	bmpingpongtime.occ \
	bmthroughput.occ \
	commstest.occ \
	commstime.occ \
	mandelbauer-c.c \
	mandelbauer.occ \
	simplecanceltest.occ \
	simpletest.occ \
	startuptest.occ

if BUILD_PONY_EXAMPLES
if HAVE_SDLRASTER
sdlraster_examples = \
	mandelbauer
endif
examples = \
	bmbackground \
	bmpingpongtime \
	bmthroughput \
	commstest \
	commstime \
	simplecanceltest \
	simpletest \
	startuptest \
	$(sdlraster_examples)
endif

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples pony --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) mandelbauer-c.o

mandelbauer: mandelbauer.occ mandelbauer-c.o
	$(OCCBUILD) --program $(srcdir)/mandelbauer.occ mandelbauer-c.o

SUFFIXES = .occ .tce

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

.occ:
	$(OCCBUILD) --program $<
