## Process this file with automake to produce Makefile.in

@SET_MAKE@

noinst_LIBRARIES = libfe.a

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign

INCLUDES=-I.. -I@srcdir@/../include
DEFS=@DEFS@ @ARCH_DEFS@ -Wall

libfe_a_SOURCES = occamfe.c lex1.c syn1.c syn2.c syn3.c chk1.c chk2.c \
chk4.c use1.c use2.c use3.c objrdr.c objtrans.c vti1.c vti2.c vti3.c \
vti4.c vti5.c err1.c err2.c list.c const1.c strpool.c objlib.c use4.c use5.c

