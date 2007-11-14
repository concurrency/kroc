## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies

EXTRA_DIST=blackboard.inc control_info.inc controller.occ crew_test.occ display.inc display.occ philosopher.occ timekeeper.occ

kroc_scdoc=blackboard.inc control_info.inc controller.occ crew_test.occ display.inc display.occ philosopher.occ timekeeper.occ

all-local: $(kroc_scdoc)

install-exec-local: $(kroc_scdoc)
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/doc/sc"
	$(INSTALL_DATA) "blackboard.inc" "$(DESTDIR)$(datadir)/doc/sc/blackboard.inc"
	$(INSTALL_DATA) "control_info.inc" "$(DESTDIR)$(datadir)/doc/sc/control_info.inc"
	$(INSTALL_DATA) "controller.occ" "$(DESTDIR)$(datadir)/doc/sc/controller.occ"
	$(INSTALL_DATA) "crew_test.occ" "$(DESTDIR)$(datadir)/doc/sc/crew_test.occ"
	$(INSTALL_DATA) "display.inc" "$(DESTDIR)$(datadir)/doc/sc/display.inc"
	$(INSTALL_DATA) "display.occ" "$(DESTDIR)$(datadir)/doc/sc/display.occ"
	$(INSTALL_DATA) "philosopher.occ" "$(DESTDIR)$(datadir)/doc/sc/philosopher.occ"
	$(INSTALL_DATA) "timekeeper.occ" "$(DESTDIR)$(datadir)/doc/sc/timekeeper.occ"

