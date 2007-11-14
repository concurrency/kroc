## -*- makefile -*-
##
## Process this file with automake to produce Makefile.in
##
## $Source: /u3/export/proj/ofa/packages/occ21/RCS/Makefile.am,v $
##
## $Id: Makefile.am,v 1.3 1997/11/05 17:37:54 djb1 Exp $
##

VPATH = @srcdir@:@srcdir@/fe:@srcdir@/be:@srcdir@/misc:@srcdir@/include
INCLUDES=-I. -I@srcdir@/include
DEFS=@DEFS@ @ARCH_DEFS@ -Wall
EXTRALDFLAGS=@EXTRA_LDFLAGS@

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign

SUBDIRS = fe be misc include arglib extlib fnilib maplib suplib tcofflib

bin_PROGRAMS = occ21

dist_man1_MANS = occ21.1

# Sources are all kept in libraries in sub-directories
occ21_SOURCES =
occ21_LDFLAGS = $(EXTRALDFLAGS)

# ... except for this which can't be in the above list due to the
# linking order below
EXTRA_occ21_SOURCES = version.c

# What a ***ing mess
occ21_PRE_version_OBJS = fe/vti1.o fe/vti2.o fe/vti3.o fe/vti4.o fe/vti5.o \
	fe/err1.o fe/err2.o fe/list.o fe/const1.o fe/strpool.o fe/objlib.o \
	misc/txlib.o \
	fe/occamfe.o fe/lex1.o fe/syn1.o fe/syn2.o fe/syn3.o fe/chk1.o \
	fe/chk2.o fe/chk4.o fe/use1.o fe/use2.o fe/use3.o fe/use4.o fe/objrdr.o \
	fe/objtrans.o misc/occompfe.o misc/inst.o misc/tran1.o \
	misc/tran2.o misc/tran3.o misc/mobiles.o \
	be/libbe.a

occ21_POST_version_OBJS = misc/harness.o \
	arglib/libarglib.a extlib/libextlib.a fnilib/libfnilib.a \
	maplib/libmaplib.a suplib/libsuplib.a tcofflib/libtcofflib.a

# What fun
occ21_LDADD = $(occ21_PRE_version_OBJS) version.o $(occ21_POST_version_OBJS)

version.o: $(occ21_PRE_version_OBJS) $(occ21_POST_version_OBJS)

occ21_DEPENDENCIES = $(occ21_LDADD)

EXTRA_DIST=

# automake assumes GNU tar is called 'tar'
# TAR = gnutar

