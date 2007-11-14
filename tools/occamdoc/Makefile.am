## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies

EXTRA_DIST=to-html.xsl frames.html occamdoc.css occamdoc.in
DISTCLEANFILES=occamdoc

occamdoc_basefiles=to-html.xsl frames.html occamdoc.css
occamdoc_programs=occamdoc

dist_man1_MANS=occamdoc.1

all-local:	$(occamdoc_basefiles) $(occamdoc_programs)

edit = sed \
	-e 's,@OCCAMDOCDATA\@,$(datadir)/occamdoc,g' \
	-e 's,@XSLTPROC\@,@XSLTPROC@,g'

occamdoc:	Makefile $(srcdir)/occamdoc.in
	rm -f occamdoc occamdoc.tmp
	$(edit) $(srcdir)/occamdoc.in >occamdoc.tmp
	chmod +x occamdoc.tmp
	mv occamdoc.tmp occamdoc

install-exec-local:	$(occamdoc_basefiles)
	$(mkinstalldirs) "$(DESTDIR)$(bindir)"
	$(INSTALL_PROGRAM) "occamdoc" "$(DESTDIR)$(bindir)/occamdoc"
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/occamdoc"
	$(INSTALL_DATA) "to-html.xsl" "$(DESTDIR)$(datadir)/occamdoc/to-html.xsl"
	$(INSTALL_DATA) "frames.html" "$(DESTDIR)$(datadir)/occamdoc/frames.html"
	$(INSTALL_DATA) "occamdoc.css" "$(DESTDIR)$(datadir)/occamdoc/occamdoc.css"


