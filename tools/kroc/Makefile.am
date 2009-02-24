## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

EXTRA_DIST = \
	kroc-setup.sh.in \
	kroc-setup.csh.in \
	kroc.in \
	occbuild.in

if OCCBUILD_KROC
kroc_generated_scripts = \
	kroc

kroc_scripts = \
	kmakef

kroc_mans = \
	kmakef.1 \
	kroc.1
endif

generated_scripts = \
	$(kroc_generated_scripts) \
	kroc-setup.csh \
	kroc-setup.sh \
	occbuild

bin_SCRIPTS = \
	$(generated_scripts)

dist_bin_SCRIPTS = \
	$(kroc_scripts)

dist_man1_MANS = \
	$(kroc_mans)

xsltdir = $(prefix)/share/xslt
dist_xslt_DATA = \
	cspx2cspm.xsl

CLEANFILES = $(generated_scripts)

edit = sed \
	-e 's|@prefix[@]|$(prefix)|g' \
	-e 's|@CC[@]|$(CC)|g' \
	-e 's|@LD[@]|$(LD)|g' \
	-e 's|@VERSION[@]|$(VERSION)|g' \
	-e 's|@KROC_CANONICAL_TARGET[@]|$(KROC_CANONICAL_TARGET)|g' \
	-e 's|@KROC_HOST_OS[@]|$(KROC_HOST_OS)|g' \
	-e 's|@KROC_HOST_CPU[@]|$(KROC_HOST_CPU)|g' \
	-e 's|@SED[@]|$(SED)|g' \
	-e 's|@KROC_CCSP_CFLAGS[@]|$(KROC_CCSP_CFLAGS)|g' \
	-e 's|@KROC_CCSP_CINCPATH[@]|$(KROC_CCSP_CINCPATH)|g' \
	-e 's|@KROC_CCSP_ASFLAGS[@]|$(KROC_CCSP_ASFLAGS)|g' \
	-e 's|@KROC_CCSP_OCCFLAGS[@]|$(KROC_CCSP_OCCFLAGS)|g' \
	-e 's|@KROC_CCSP_TRANFLAGS[@]|$(KROC_CCSP_TRANFLAGS)|g' \
	-e 's|@KROC_CCSP_LDFLAGS[@]|$(KROC_CCSP_LDFLAGS)|g' \
	-e 's|@KROC_CCSP_LIBPATH[@]|$(KROC_CCSP_LIBPATH)|g' \
	-e 's|@KROC_CCSP_LIBS[@]|$(KROC_CCSP_LIBS)|g' \
	-e 's|@NOCCHOME[@]|$(NOCCHOME)|g' \
	-e 's|@KROC_ARCHBINDIR[@]|$(KROC_ARCHBINDIR)|g' \
	-e 's|@KROC_ARCHLIBDIR[@]|$(KROC_ARCHLIBDIR)|g' \
	-e 's|@KROC_ARCHINCDIR[@]|$(KROC_ARCHINCDIR)|g' \
	-e 's|@KROC_MANDIR[@]|$(KROC_MANDIR)|g' \
	-e 's|@KROC_KROCDIR[@]|$(KROC_KROCDIR)|g' \
	-e 's|@KROC_CONFIGDIR[@]|$(KROC_CONFIGDIR)|g' \
	-e 's|@KROC_EXAMPLESDIR[@]|$(KROC_EXAMPLESDIR)|g' \
	-e 's|@TVM_BINDIR[@]|$(TVM_BINDIR)|g' \
	-e 's|@TVM_LIBDIR[@]|$(TVM_LIBDIR)|g' \
	-e 's|@TVM_EXAMPLESDIR[@]|$(TVM_EXAMPLESDIR)|g' \
	-e 's|@ACLOCALDIR[@]|$(ACLOCALDIR)|g' \
	-e 's|@CONFFILEPREFIX[@]|$(CONFFILEPREFIX)|g' \
	-e 's|@ENV_LIBRARY_PATH[@]|$(ENV_LIBRARY_PATH)|g'

$(generated_scripts): Makefile
	rm -f $@ $@.tmp
	$(edit) '$(srcdir)/$@.in' >$@.tmp
	chmod +x $@.tmp
	chmod a-w $@.tmp
	mv $@.tmp $@

kroc-setup.csh: $(srcdir)/kroc-setup.csh.in
kroc-setup.sh: $(srcdir)/kroc-setup.sh.in
kroc: $(srcdir)/kroc.in
occbuild: $(srcdir)/occbuild.in
