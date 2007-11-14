## Process this file with automake to produce Makefile.in

@SET_MAKE@

noinst_LIBRARIES = libsuplib.a

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign

INCLUDES=-I.. -I@srcdir@/../include
DEFS=@DEFS@ @ARCH_DEFS@

libsuplib_a_SOURCES = sort.c strlwr.c strupr.c
