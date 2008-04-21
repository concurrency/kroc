## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

KROC = @KROC@
KROCOPTS = --nocc
CFLAGS = @CFLAGS@ @DEFS@

OBJECTS = placed-par.o

SUFFIXED=.occ .xlb

EXTRA_DIST = \
	placed-par.occ

.occ.o:
	$(KROC) $(KROCOPTS) -e -c -v $<

libs = compiler.xlb compiler.so

all-local: $(libs)

install-exec-local: $(libs)

CLEANFILES = $(libs) $(OBJECTS) *~ *.s *.etc *.xlo

compiler.xlb:  placed-par.o

compiler.so:	$(OBJECTS)
		@LD@ -r -o libcompiler.so $(OBJECTS)


