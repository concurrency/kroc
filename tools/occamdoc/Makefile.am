## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

EXTRA_DIST = \
	occamdoc.in

occamdocdir = $(datadir)/kroc/occamdoc
dist_occamdoc_DATA = \
	frames.html \
	occamdoc.css \
	to-html.xsl

dist_man1_MANS = occamdoc.1

all-local: occamdoc

edit = sed \
	-e 's,@OCCAMDOCDATA\@,$(occamdocdir),g' \
	-e 's,@XSLTPROC\@,@XSLTPROC@,g'

occamdoc: Makefile $(srcdir)/occamdoc.in
	rm -f occamdoc occamdoc.tmp
	$(edit) $(srcdir)/occamdoc.in >occamdoc.tmp
	chmod +x occamdoc.tmp
	mv occamdoc.tmp occamdoc

install-exec-local:	$(occamdoc_basefiles)
	$(mkinstalldirs) "$(DESTDIR)$(bindir)"
	$(INSTALL_PROGRAM) "occamdoc" "$(DESTDIR)$(bindir)/occamdoc"

CLEANFILES = occamdoc
