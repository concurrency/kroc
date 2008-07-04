## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

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
	startuptest.occ \
	trace.c \
	trace.inc

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

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples pony --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) mandelbauer-c.o trace.o

mandelbauer: mandelbauer.occ mandelbauer-c.o trace.o
	$(OCCBUILD) --program mandelbauer.occ mandelbauer-c.o trace.o

SUFFIXES = .occ .tce

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

.occ: trace.o
	$(OCCBUILD) --program $< trace.o

## begin pideps dependencies
bmbackground.occ: $(srcdir)/trace.inc
bmpingpongtime.occ: $(srcdir)/trace.inc
bmthroughput.occ: $(srcdir)/trace.inc
commstest.occ: $(srcdir)/trace.inc
mandelbauer.occ: $(srcdir)/trace.inc
simplecanceltest.occ: $(srcdir)/trace.inc
simpletest.occ: $(srcdir)/trace.inc
## end pideps dependencies
