## Process this file with automake to produce Makefile.in

@SET_MAKE@
INCLUDES=-I@top_srcdir@/include
DEFS=@DEFS@ @CCEXTRAOPTS@ @CCINCLUDES@ -Wall

noinst_LIBRARIES = libio.a

libio_a_SOURCES = io.c input.occ output.occ

SUFFIXES=.occ

.occ.o:
	$(KROC) $(KROCOPTS) -c -v $<

CLEANFILES = *~ *.t *.tco *.kt* *.tce

