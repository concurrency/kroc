# Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

srcdir = @srcdir@
VPATH = @srcdir@

DEFS = @DEFS@
INCLUDES = -I. -I$(srcdir)/tcofflib

SUBDIRS = tcofflib

bin_PROGRAMS = ilibr

ilibr_SOURCES = \
	arg.c \
	arg.h \
	ilibr.c
ilibr_LDADD = tcofflib/libtcofflib.a
