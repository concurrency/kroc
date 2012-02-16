## Process this file with automake to produce Makefile.in

@SET_MAKE@
INCLUDES=-I@srcdir@/../include

noinst_LIBRARIES=libcommon.a

AUTOMAKE_OPTIONS=foreign

DEFS=@DEFS@ @EXTRA_DEFS@ -fno-defer-pop -Wall

EXTRA_DIST = \
	nmem.c

libcommon_a_SOURCES=dmem.c userproc.c rtsmain.c mobproc.c dtrace.c timercal.c

