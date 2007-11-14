## Process this file with automake to produce Makefile.in

@SET_MAKE@

noinst_LIBRARIES = libarglib.a

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign

INCLUDES=-I.. -I@srcdir@/../include
DEFS=@DEFS@ @ARCH_DEFS@

libarglib_a_SOURCES = arg.c argnew.c argcmn.c

noinst_HEADERS = arg0.h
