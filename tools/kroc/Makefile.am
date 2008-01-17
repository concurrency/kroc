## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies

EXTRA_DIST = \
	kroc-setup.sh.in \
	kroc-setup.csh.in \
	kroc.in \
	kmakemodule.in \
	occbuild.in

generated_scripts = \
	kmakemodule \
	kroc-setup.csh \
	kroc-setup.sh \
	kroc \
	occbuild
bin_SCRIPTS = \
	$(generated_scripts) \
	kmakef

dist_man1_MANS = \
	kmakef.1 \
	kroc.1

aclocaldir = @ACLOCALDIR@
aclocal_DATA = occbuild.m4

DISTCLEANFILES = $(generated_scripts)

edit = sed \
	-e 's|@prefix[@]|$(prefix)|g' \
	-e 's|@CC[@]|$(CC)|g' \
	-e 's|@LD[@]|$(LD)|g' \
	-e 's|@VERSION[@]|$(VERSION)|g' \
	-e 's|@KROC_CANONICAL_TARGET[@]|$(KROC_CANONICAL_TARGET)|g' \
	-e 's|@KROC_HOST_OS[@]|$(KROC_HOST_OS)|g' \
	-e 's|@KROC_HOST_CPU[@]|$(KROC_HOST_CPU)|g' \
	-e 's|@DLOPTLIB[@]|$(DLOPTLIB)|g' \
	-e 's|@ARCH_DEFS[@]|$(ARCH_DEFS)|g' \
	-e 's|@ARCH_LDFLAGS[@]|$(ARCH_LDFLAGS)|g' \
	-e 's|@ARCH_ASFLAGS[@]|$(ARCH_ASFLAGS)|g' \
	-e 's|@SED[@]|$(SED)|g' \
	-e 's|@DMOPTLIB[@]|$(DMOPTLIB)|g' \
	-e 's|@DEXTLIB[@]|$(DEXTLIB)|g' \
	-e 's|@DSYSOPTS[@]|$(DSYSOPTS)|g' \
	-e 's|@TENABLEMP[@]|$(TENABLEMP)|g' \
	-e 's|@OCCEXTRAOPTS[@]|$(OCCEXTRAOPTS)|g' \
	-e 's|@TRANEXTRAOPTS[@]|$(TRANEXTRAOPTS)|g' \
	-e 's|@EXTCCEXTRAOPTS[@]|$(EXTCCEXTRAOPTS)|g' \
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

kmakemodule: $(srcdir)/kmakemodule.in
kroc-setup.csh: $(srcdir)/kroc-setup.csh.in
kroc-setup.sh: $(srcdir)/kroc-setup.sh.in
kroc: $(srcdir)/kroc.in
occbuild: $(srcdir)/occbuild.in
