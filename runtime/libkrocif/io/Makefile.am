## Process this file with automake to produce Makefile.in

@SET_MAKE@
AM_CPPFLAGS = -I@top_srcdir@/include @KROC_CCSP_CINCPATH@
DEFS = @DEFS@ @KROC_CCSP_CFLAGS@ -Wall

noinst_LIBRARIES = libio.a

libio_a_SOURCES = io.c input.occ output.occ

SUFFIXES=.occ

.occ.o:
	$(KROC) $(KROCOPTS) -c -v $<

CLEANFILES = *~ *.t *.tco *.kt* *.tce

