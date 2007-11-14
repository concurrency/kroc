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

%: %.occ trace.o
	$(OCCBUILD) --program $< trace.o

## begin pideps dependencies
bmbackground.tce: $(srcdir)/trace.inc
bmpingpongtime.tce: $(srcdir)/trace.inc
bmthroughput.tce: $(srcdir)/trace.inc
commstest.tce: $(srcdir)/trace.inc
mandelbauer.tce: $(srcdir)/trace.inc
simplecanceltest.tce: $(srcdir)/trace.inc
simpletest.tce: $(srcdir)/trace.inc
## end pideps dependencies
