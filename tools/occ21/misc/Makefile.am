## Process this file with automake to produce Makefile.in

@SET_MAKE@

noinst_LIBRARIES = libmisc.a

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign

INCLUDES=-I.. -I@srcdir@/../include
DEFS=@DEFS@ @ARCH_DEFS@ -Wall

libmisc_a_SOURCES = harness.c inst.c occompfe.c tran1.c tran2.c tran3.c mobiles.c txlib.c
