# Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

# The ordering here is significant, owing to dependencies between modules.
SUBDIRS = \
	forall \
	maths \
	convert \
	hostio \
	hostsp \
	streamio \
	string

dist_man3_MANS = \
	libconvert-inmos.3 \
	libhostio-inmos.3 \
	libmath-inmos.3 \
	libstreamio-inmos.3 \
	libstring-inmos.3

EXTRA_DIST = \
	README
