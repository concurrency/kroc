## Process this file with automake to produce Makefile.in

@SET_MAKE@
INCLUDES=-I@top_srcdir@/include
DEFS=@DEFS@ @CCEXTRAOPTS@ @CCINCLUDES@ -Wall

noinst_LIBRARIES = librts.a

librts_a_SOURCES = occam_entry.c rtsmain.c tty.c userproc.c

