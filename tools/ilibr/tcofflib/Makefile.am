## Process this file with automake to produce Makefile.in

@SET_MAKE@

noinst_LIBRARIES = libtcofflib.a

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign
DEFS=@DEFS@ @ARCH_DEFS@

INCLUDES=-I.. -I@srcdir@/../include

## old DEFINES         = -DTARG=ALPHA -DSTD_C -DANSI -ansi -Wall

libtcofflib_a_SOURCES = \
alloc1.c alloc2.c disassem.c \
hexdump.c popen.c command1.c \
command2.c command3.c command4.c \
command5.c command6.c command7.c \
error.c tcoff_io.c lff_io.c \
heap.c \
strutil3.c strutil4.c strutil5.c \
strutil6.c strutil7.c com.c \
lff.h tcoff.h tcofflib.h toolkit.h
