## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
KROC = @KROC@
TRANPC = `@KROC@ -c --tran` `@KROC@ -c --tranflags`

EXTRA_DIST = \
	README \
	q1.occ \
	q1-1.tce \
	q2.occ \
	q2-1.tce \
	q3.occ \
	q3-1.tce \
	q4.occ \
	q4-1.tce \
	q5.occ \
	q5-1.tce \
	q6.occ \
	q6-1.tce \
	q6-sort.occ \
	q6-sort-1.tce \
	q7.occ \
	q7-1.tce \
	q7-adam.occ \
	q7-adam-1.tce \
	q7-eric.occ \
	q7-eric-1.tce \
	q7-fred.occ \
	q7-fred-1.tce \
	q7-jonathan.occ \
	q7-jonathan-1.tce \
	q7-jonathan2.occ \
	q7-jonathan2-1.tce \
	q7-nick.occ \
	q7-nick-1.tce \
	q7-nick-2.tce \
	q7-nick-3.tce \
	q7-richard.occ \
	q7-richard-1.tce \
	q7-richard-2.tce

if BUILD_COURSE_ANSWERS
examples = \
	q1 \
	q2 \
	q3 \
	q4 \
	q5 \
	q6 \
	q6-sort \
	q7 \
	q7-adam \
	q7-eric \
	q7-fred \
	q7-jonathan \
	q7-jonathan2 \
	q7-nick \
	q7-richard
endif

all-local: $(examples)

q1: q1.occ q1-1.o
	$(OCCBUILD) --program $(srcdir)/q1.occ q1-1.o

q2: q2.occ q2-1.o
	$(OCCBUILD) --program $(srcdir)/q2.occ q2-1.o

q3: q3.occ q3-1.o
	$(OCCBUILD) --program $(srcdir)/q3.occ q3-1.o

q4: q4.occ q4-1.o
	$(OCCBUILD) --program $(srcdir)/q4.occ q4-1.o

q5: q5.occ q5-1.o
	$(OCCBUILD) --program $(srcdir)/q5.occ q5-1.o

q6: q6.occ q6-1.o
	$(OCCBUILD) --program $(srcdir)/q6.occ q6-1.o

q6-sort: q6-sort.occ q6-sort-1.o
	$(OCCBUILD) --program $(srcdir)/q6-sort.occ q6-sort-1.o

q7: q7.occ q7-1.o
	$(OCCBUILD) --program $(srcdir)/q7.occ q7-1.o

q7-fred: q7-fred.occ q7-fred-1.o
	$(OCCBUILD) --program $(srcdir)/q7-fred.occ q7-fred-1.o

q7-nick: q7-nick.occ q7-nick-1.o q7-nick-2.o q7-nick-3.o
	$(OCCBUILD) --program $(srcdir)/q7-nick.occ q7-nick-1.o q7-nick-2.o q7-nick-3.o

q7-richard: q7-richard.occ q7-richard-1.o q7-richard-2.o
	$(OCCBUILD) --program $(srcdir)/q7-richard.occ q7-richard-1.o q7-richard-2.o

q7-adam: q7-adam.occ q7-adam-1.o
	$(OCCBUILD) --program $(srcdir)/q7-adam.occ q7-adam-1.o

q7-eric: q7-eric.occ q7-eric-1.o
	$(OCCBUILD) --program $(srcdir)/q7-eric.occ q7-eric-1.o

q7-jonathan: q7-jonathan.occ q7-jonathan-1.o
	$(OCCBUILD) --program $(srcdir)/q7-jonathan.occ q7-jonathan-1.o

q7-jonathan2: q7-jonathan2.occ q7-jonathan2-1.o
	$(OCCBUILD) --program $(srcdir)/q7-jonathan2.occ q7-jonathan2-1.o

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples course --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)
	rm -f *-[0-9].o

SUFFIXES = .occ .tce

.tce.o:
	$(TRANPC) -o $@ $<
