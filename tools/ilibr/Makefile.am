## -*- makefile -*-
##
## Process this file with automake to produce Makefile.in
##
## $Source:$
##
## $Id:$
##

VPATH = @srcdir@
INCLUDES=-I.
DEFS=@DEFS@ @ARCH_DEFS@
EXTRALDFLAGS=@ARCH_DEFS@

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign

SUBDIRS = tcofflib

bin_PROGRAMS = ilibr

# Sources are all kept in libraries in sub-directories
ilibr_SOURCES = arg.c ilibr.c arg.h host_os.h
ilibr_LDADD = tcofflib/libtcofflib.a
ilibr_LDFLAGS = $(EXTRALDFLAGS)


