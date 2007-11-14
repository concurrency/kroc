## Process this file with automake to produce Makefile.in

@SET_MAKE@

noinst_LIBRARIES = libbe.a

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign

INCLUDES=-I.. -I@srcdir@/../include
DEFS=@DEFS@ @ARCH_DEFS@ -Wall

libbe_a_SOURCES = bind1.c bind2.c bind3.c debug.c gen1.c gen2.c gen3.c \
gen4.c gen5.c gen7.c gen8.c gen9.c gen10.c gen11.c gen12.c gen13.c generror.c \
objwrt.c profile.c code1k.c
libbe_a_DEPENDENCIES= $(libbe_a_LIBADD)

