## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

EXTRA_DIST = \
	occamdoc.in \
	setup.py \
	tests/abbrev.occ \
	tests/broken.occ \
	tests/empty.occ \
	tests/extern.occ \
	tests/findall.occ \
	tests/main.occ \
	tests/merge1.occ \
	tests/merge2.occ \
	tests/modref.occ \
	tests/modrefdest.occ \
	tests/multiarg.occ \
	tests/multiargbad.occ \
	tests/noheader.occ \
	tests/spacing.occ \
	tests/tabs.occ \
	tests/vars.occ

if OCCAMDOC_ENABLED
bin_SCRIPTS = \
	occamdoc
endif

occamdocdir = $(datadir)/kroc/occamdoc
dist_occamdoc_DATA = \
	frames.html \
	occamdoc.css \
	to-html.xsl

dist_man1_MANS = occamdoc.1

if OCCAMDOC_ENABLED
all-local: occamdoc
endif

edit = sed \
	-e 's,@OCCAMDOCDATA\@,$(occamdocdir),g' \
	-e 's,@XSLTPROC\@,@XSLTPROC@,g'

occamdoc: Makefile $(srcdir)/occamdoc.in
	rm -f occamdoc occamdoc.tmp
	$(edit) $(srcdir)/occamdoc.in >occamdoc.tmp
	chmod +x occamdoc.tmp
	mv occamdoc.tmp occamdoc

CLEANFILES = occamdoc
