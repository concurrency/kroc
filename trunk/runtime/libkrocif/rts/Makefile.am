## Process this file with automake to produce Makefile.in

@SET_MAKE@
INCLUDES = -I@top_srcdir@/include @KROC_CCSP_CINCPATH@
DEFS = @DEFS@ @KROC_CCSP_CFLAGS@ -Wall

noinst_LIBRARIES = librts.a

librts_a_SOURCES = occam_entry.c tty.c userproc.c rtsmain.c

