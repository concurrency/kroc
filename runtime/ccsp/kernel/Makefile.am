## Process this file with automake to produce Makefile.in

@SET_MAKE@
INCLUDES=-I@srcdir@/../include

noinst_LIBRARIES=libkernel.a

AUTOMAKE_OPTIONS=foreign

DEFS=@DEFS@ @EXTRA_DEFS@ -fno-defer-pop -Wall

libkernel_a_SOURCES = dynproc.c bsyscalls.c deadlock.c sched.c cspdriver.c entry.c stats.c

