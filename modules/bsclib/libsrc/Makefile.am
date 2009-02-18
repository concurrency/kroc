## -*- makefile -*-
##
## Process this file with automake to produce Makefile.in
##

## Cannot use ansi2knr because it is under GPL
## AUTOMAKE_OPTIONS=ansi2knr
## config.guess and config.sub have special exceptions
AUTOMAKE_OPTIONS=foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

SUBDIRS = socklib filelib proclib httplib
if BUILD_CSPDRVLIB
SUBDIRS += cspdrvlib
endif

dist_man3_MANS = libsock.3 libfile.3 libproc.3

