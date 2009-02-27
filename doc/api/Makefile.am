## process this file with automake to produce Makefile.in

@SET_MAKE@
OCCAMDOC = @OCCAMDOC@

docsrc = ../../modules/*/libsrc

all-local: output.stamp

output.stamp:
	$(OCCAMDOC) -v -f html -d output `find $(docsrc) -name '*.occ' -or -name '*.inc'`
	touch output.stamp

apidir = $(prefix)/share/kroc/doc/api
install-data-local:
	@$(mkdir_p) $(DESTDIR)$(apidir)
	@cd output && for f in *; do \
		$(INSTALL_DATA) $$f $(DESTDIR)$(apidir)/$$f; \
	done

clean-local:
	rm -fr output

CLEANFILES = output.stamp
