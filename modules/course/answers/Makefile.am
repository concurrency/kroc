## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies

KROC=@KROC@
TRANPC=@OCTRAN@
TRANPC_OPTS=@OCTRANFLAGS@

SUFFIXES = .occ


kroc_programs=q1 q2 q3 q4 q5 q6 q6-sort q7 q7-fred q7-nick q7-richard q7-adam q7-eric q7-jonathan q7-jonathan2

all-local:	$(kroc_programs)

q1:		q1-1.o q1.occ
		$(KROC) $(KROC_OPTS) q1.occ q1-1.o -lcourse

q1-1.o:		q1-1.tce
		$(TRANPC) $(TRANPC_OPTS) q1-1.tce

q2:		q2-1.o q2.occ
		$(KROC) $(KROC_OPTS) q2.occ q2-1.o -lcourse

q2-1.o:		q2-1.tce
		$(TRANPC) $(TRANPC_OPTS) q2-1.tce

q3:		q3-1.o q3.occ
		$(KROC) $(KROC_OPTS) q3.occ q3-1.o -lcourse

q3-1.o:		q3-1.tce
		$(TRANPC) $(TRANPC_OPTS) q3-1.tce

q4:		q4-1.o q4.occ
		$(KROC) $(KROC_OPTS) q4.occ q4-1.o -lcourse

q4-1.o:		q4-1.tce
		$(TRANPC) $(TRANPC_OPTS) q4-1.tce

q5:		q5-1.o q5.occ
		$(KROC) $(KROC_OPTS) q5.occ q5-1.o -lcourse

q5-1.o:		q5-1.tce
		$(TRANPC) $(TRANPC_OPTS) q5-1.tce

q6:		q6-1.o q6.occ
		$(KROC) $(KROC_OPTS) q6.occ q6-1.o -lcourse

q6-1.o:		q6-1.tce
		$(TRANPC) $(TRANPC_OPTS) q6-1.tce

q6-sort:	q6-sort-1.o q6-sort.occ
		$(KROC) $(KROC_OPTS) q6-sort.occ q6-sort-1.o -lcourse

q6-sort-1.o:	q6-sort-1.tce
		$(TRANPC) $(TRANPC_OPTS) q6-sort-1.tce

q7:		q7-1.o q7.occ
		$(KROC) $(KROC_OPTS) q7.occ q7-1.o -lcourse

q7-1.o:		q7-1.tce
		$(TRANPC) $(TRANPC_OPTS) q7-1.tce

q7-fred:	q7-fred-1.o q7-fred.occ
		$(KROC) $(KROC_OPTS) q7-fred.occ q7-fred-1.o -lcourse

q7-fred-1.o:	q7-fred-1.tce
		$(TRANPC) $(TRANPC_OPTS) -o q7-fred-1.o q7-fred-1.tce

q7-nick:	q7-nick-1.o q7-nick-2.o q7-nick-3.o q7-nick.occ
		$(KROC) $(KROC_OPTS) q7-nick.occ q7-nick-1.o q7-nick-2.o q7-nick-3.o -lcourse -lss

q7-nick-1.o:	q7-nick-1.tce
		$(TRANPC) $(TRANPC_OPTS) q7-nick-1.tce

q7-nick-2.o:	q7-nick-2.tce
		$(TRANPC) $(TRANPC_OPTS) q7-nick-2.tce

q7-nick-3.o:	q7-nick-3.tce
		$(TRANPC) $(TRANPC_OPTS) q7-nick-3.tce

q7-richard-1.o:	q7-richard-1.tce
		$(TRANPC) $(TRANPC_OPTS) -o q7-richard-1.o q7-richard-1.tce

q7-richard-2.o:	q7-richard-2.tce
		$(TRANPC) $(TRANPC_OPTS) -o q7-richard-2.o q7-richard-2.tce

q7-richard:	q7-richard-1.o q7-richard-2.o q7-richard.occ
		$(KROC) $(KROC_OPTS) q7-richard.occ q7-richard-1.o q7-richard-2.o -lcourse

q7-adam:	q7-adam-1.o q7-adam.occ
		$(KROC) $(KROC_OPTS) q7-adam.occ q7-adam-1.o -lcourse

q7-adam-1.o:	q7-adam-1.tce
		$(TRANPC) $(TRANPC_OPTS) -o q7-adam-1.o q7-adam-1.tce

q7-eric:	q7-eric-1.o q7-eric.occ
		$(KROC) $(KROC_OPTS) q7-eric.occ q7-eric-1.o -lcourse

q7-eric-1.o:	q7-eric-1.tce
		$(TRANPC) $(TRANPC_OPTS) -o q7-eric-1.o q7-eric-1.tce

q7-jonathan:	q7-jonathan-1.o q7-jonathan.occ
		$(KROC) $(KROC_OPTS) q7-jonathan.occ q7-jonathan-1.o -lcourse

q7-jonathan-1.o:	q7-jonathan-1.tce
			$(TRANPC) $(TRANPC_OPTS) -o q7-jonathan-1.o q7-jonathan-1.tce

q7-jonathan2:	q7-jonathan2-1.o q7-jonathan2.occ
		$(KROC) $(KROC_OPTS) q7-jonathan2.occ q7-jonathan2-1.o -lcourse -lss

q7-jonathan2-1.o:	q7-jonathan2-1.tce
			$(TRANPC) $(TRANPC_OPTS) -o q7-jonathan2-1.o q7-jonathan2-1.tce

CLEANFILES=$(kroc_programs) *~ *.t *.s *.kt* *.o

#kroc_programs=q1 q2 q3 q4 q5 q6 q6-sort q7 q7-fred q7-nick q7-richard q7-adam q7-eric q7-jonathan q7-jonathan2


install-exec-local: $(kroc_programs)
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/examples/course"
	$(INSTALL_PROGRAM) "q1" "$(DESTDIR)$(datadir)/examples/course/q1"
	$(INSTALL_PROGRAM) "q2" "$(DESTDIR)$(datadir)/examples/course/q2"
	$(INSTALL_PROGRAM) "q3" "$(DESTDIR)$(datadir)/examples/course/q3"
	$(INSTALL_PROGRAM) "q4" "$(DESTDIR)$(datadir)/examples/course/q4"
	$(INSTALL_PROGRAM) "q5" "$(DESTDIR)$(datadir)/examples/course/q5"
	$(INSTALL_PROGRAM) "q6" "$(DESTDIR)$(datadir)/examples/course/q6"
	$(INSTALL_PROGRAM) "q6-sort" "$(DESTDIR)$(datadir)/examples/course/q6-sort"
	$(INSTALL_PROGRAM) "q7" "$(DESTDIR)$(datadir)/examples/course/q7"
	$(INSTALL_PROGRAM) "q7-fred" "$(DESTDIR)$(datadir)/examples/course/q7-fred"
	$(INSTALL_PROGRAM) "q7-nick" "$(DESTDIR)$(datadir)/examples/course/q7-nick"
	$(INSTALL_PROGRAM) "q7-richard" "$(DESTDIR)$(datadir)/examples/course/q7-richard"
	$(INSTALL_PROGRAM) "q7-adam" "$(DESTDIR)$(datadir)/examples/course/q7-adam"
	$(INSTALL_PROGRAM) "q7-eric" "$(DESTDIR)$(datadir)/examples/course/q7-eric"
	$(INSTALL_PROGRAM) "q7-jonathan" "$(DESTDIR)$(datadir)/examples/course/q7-jonathan"
	$(INSTALL_PROGRAM) "q7-jonathan2" "$(DESTDIR)$(datadir)/examples/course/q7-jonathan2"


