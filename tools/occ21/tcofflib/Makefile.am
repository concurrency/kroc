## Process this file with automake to produce Makefile.in

@SET_MAKE@

noinst_LIBRARIES = libtcofflib.a

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign

INCLUDES=-I.. -I@srcdir@/../include
DEFS=@DEFS@ @ARCH_DEFS@

libtcofflib_a_SOURCES = alloc1.c alloc2.c disassem.c hexdump.c \
command1.c command2.c command3.c command4.c command6.c com.c error.c \
strutil3.c strutil4.c strutil5.c strutil6.c strutil7.c tcoff_io.c \
lff_io.c
