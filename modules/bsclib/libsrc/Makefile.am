AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

SUBDIRS =
if BUILD_FILELIB
SUBDIRS += filelib
endif
if BUILD_PROCLIB
SUBDIRS += proclib
endif
if BUILD_SOCKLIB
SUBDIRS += socklib httplib
endif
if BUILD_CSPDRVLIB
SUBDIRS += cspdrvlib
endif

dist_man3_MANS = libsock.3 libfile.3 libproc.3
