## Process this file with automake to produce Makefile.in

@SET_MAKE@
AM_CPPFLAGS = -I@srcdir@/../include

noinst_LIBRARIES=libkernel.a

AUTOMAKE_OPTIONS=foreign

DEFS=@DEFS@ @EXTRA_DEFS@ -fno-defer-pop -Wall

libkernel_a_SOURCES = bsyscalls.c deadlock.c dynproc.c entry.c sched.c stats.c

