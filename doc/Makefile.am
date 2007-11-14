## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies

SUBDIRS=sc lib

EXTRA_DIST=README \
	occ21-extensions.html kroc-faq.html udo.html \
	pragma.txt essentially-kroc.txt hostio.txt hlps.txt about-kroc.txt o2cl.txt AUTHORS OLD-AUTHORS\
	bsclib.ps udc.ps blocking-native.ps o2cl.ps udo.ps native-call.ps \
	o2cl.ms

kroc_htmldoc=occ21-extensions.html kroc-faq.html udo.html
kroc_txtdoc=pragma.txt essentially-kroc.txt hostio.txt hlps.txt about-kroc.txt o2cl.txt AUTHORS OLD-AUTHORS
kroc_psdoc=bsclib.ps udc.ps blocking-native.ps o2cl.ps udo.ps native-call.ps
kroc_msdoc=o2cl.ms

all-local:	$(kroc_htmldoc) $(kroc_txtdoc) $(kroc_psdoc)

install-exec-local:	$(kroc_htmldoc) $(kroc_txtdoc) $(kroc_psdoc)
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/doc/html"
	$(INSTALL_DATA) "occ21-extensions.html" "$(DESTDIR)$(datadir)/doc/html/occ21-extensions.html"
	$(INSTALL_DATA) "kroc-faq.html" "$(DESTDIR)$(datadir)/doc/html/kroc-faq.html"
	$(INSTALL_DATA) "udo.html" "$(DESTDIR)$(datadir)/doc/html/udo.html"
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/doc/txt"
	$(INSTALL_DATA) "pragma.txt" "$(DESTDIR)$(datadir)/doc/txt/pragma.txt"
	$(INSTALL_DATA) "essentially-kroc.txt" "$(DESTDIR)$(datadir)/doc/txt/essentially-kroc.txt"
	$(INSTALL_DATA) "hostio.txt" "$(DESTDIR)$(datadir)/doc/txt/hostio.txt"
	$(INSTALL_DATA) "hlps.txt" "$(DESTDIR)$(datadir)/doc/txt/hlps.txt"
	$(INSTALL_DATA) "about-kroc.txt" "$(DESTDIR)$(datadir)/doc/txt/about-kroc.txt"
	$(INSTALL_DATA) "o2cl.txt" "$(DESTDIR)$(datadir)/doc/txt/o2cl.txt"
	$(INSTALL_DATA) "AUTHORS" "$(DESTDIR)$(datadir)/doc/txt/AUTHORS"
	$(INSTALL_DATA) "OLD-AUTHORS" "$(DESTDIR)$(datadir)/doc/txt/OLD-AUTHORS"
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/doc/ps"
	$(INSTALL_DATA) "bsclib.ps" "$(DESTDIR)$(datadir)/doc/ps/bsclib.ps"
	$(INSTALL_DATA) "udc.ps" "$(DESTDIR)$(datadir)/doc/ps/udc.ps"
	$(INSTALL_DATA) "blocking-native.ps" "$(DESTDIR)$(datadir)/doc/ps/blocking-native.ps"
	$(INSTALL_DATA) "o2cl.ps" "$(DESTDIR)$(datadir)/doc/ps/o2cl.ps"
	$(INSTALL_DATA) "udo.ps" "$(DESTDIR)$(datadir)/doc/ps/udo.ps"
	$(INSTALL_DATA) "native-call.ps" "$(DESTDIR)$(datadir)/doc/ps/native-call.ps"
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/doc/ms"
	$(INSTALL_DATA) "o2cl.ms" "$(DESTDIR)$(datadir)/doc/ms/o2cl.ms"
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/doc/python"
	$(INSTALL_DATA) "moinmoinwiki-occampi.py" "$(DESTDIR)$(datadir)/doc/python/occampi.py"

