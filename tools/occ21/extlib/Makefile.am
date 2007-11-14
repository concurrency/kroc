## Process this file with automake to produce Makefile.in

@SET_MAKE@

noinst_LIBRARIES = libextlib.a

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign

INCLUDES=-I.. -I@srcdir@/../include
DEFS=@DEFS@ @ARCH_DEFS@

libextlib_a_SOURCES = extconv.c exti8.c exti16.c exti32.c exti64.c \
extlong.c extr32.c extr64.c extstr.c extui8.c extui16.c extui32.c
