## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

KROC = @KROC@
KROCOPTS = --nocc
CFLAGS = @CFLAGS@ @DEFS@

EXTRA_DIST = \
	placed-par.occ

.occ.o:
	$(KROC) $(KROCOPTS) -e -c -v $<

libs = compiler.xlb

all-local: $(libs)

compiler.xlb:  placed-par.o


