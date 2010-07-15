## process this file with automake to produce Makefile.in

@SET_MAKE@
OCCAMDOC = @OCCAMDOC@

docsrc = \
	$(KROC_SRC_ROOT)/modules/*/libsrc \
	$(KROC_BUILD_ROOT)/modules/*/libsrc

if OCCAMDOC_ENABLED
all-local: output.stamp
endif

output.stamp:
	$(OCCAMDOC) -v -f html -d output `find $(docsrc) -name '*.occ' -or -name '*.inc'`
	touch output.stamp

if OCCAMDOC_ENABLED
apidir = $(prefix)/share/kroc/doc/api
install-data-local:
	@$(mkdir_p) $(DESTDIR)$(apidir)
	@cd output && for f in *; do \
		$(INSTALL_DATA) $$f $(DESTDIR)$(apidir)/$$f; \
	done
endif

clean-local:
	rm -fr output

CLEANFILES = output.stamp
