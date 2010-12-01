@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	README \
	cif-commstime-c.c cif-commstime.occ \
	cift1-c.c cift1.occ \
	cift2-c.c cift2.occ \
	cift3-c.c cift3.occ \
	cift4-c.c cift4.occ \
	cift5-c.c cift5.occ \
	cift6-c.c cift6.occ \
	cift7-c.c cift7.occ \
	cift8-c.c cift8.occ \
	cift9-c.c cift9.occ \
	cift10-c.c cift10.occ \
	cift11-c.c cift11.occ \
	cift12-c.c cift12.occ \
	cift13-c.c cift13.occ \
	cift14-c.c cift14.occ \
	cift15.c \
	cift16-c.c cift16.occ \
	cift17-c.c cift17.occ \
	ciftutils.c ciftutils.h

if BUILD_CIF_EXAMPLES
examples = \
	cif-commstime \
	cift1 \
	cift2 \
	cift3 \
	cift4 \
	cift5 \
	cift6 \
	cift7 \
	cift8 \
	cift9 \
	cift10 \
	cift11 \
	cift12 \
	cift13 \
	cift14 \
	cift15 \
	cift16 \
	cift17
endif

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples cif --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) *.o

SUFFIXES = .occ

cif-commstime: cif-commstime.occ cif-commstime-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cif-commstime.occ cif-commstime-c.o ciftutils.o

cift1: cift1.occ cift1-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift1.occ cift1-c.o ciftutils.o

cift2: cift2.occ cift2-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift2.occ cift2-c.o ciftutils.o

cift3: cift3.occ cift3-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift3.occ cift3-c.o ciftutils.o

cift4: cift4.occ cift4-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift4.occ cift4-c.o ciftutils.o

cift5: cift5.occ cift5-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift5.occ cift5-c.o ciftutils.o

cift6: cift6.occ cift6-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift6.occ cift6-c.o ciftutils.o

cift7: cift7.occ cift7-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift7.occ cift7-c.o ciftutils.o

cift8: cift8.occ cift8-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift8.occ cift8-c.o ciftutils.o

cift9: cift9.occ cift9-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift9.occ cift9-c.o ciftutils.o

cift10: cift10.occ cift10-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift10.occ cift10-c.o ciftutils.o

cift11: cift11.occ cift11-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift11.occ cift11-c.o ciftutils.o

cift12: cift12.occ cift12-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift12.occ cift12-c.o ciftutils.o

cift13: cift13.occ cift13-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift13.occ cift13-c.o ciftutils.o

cift14: cift14.occ cift14-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift14.occ cift14-c.o ciftutils.o

cift15: cift15.o ciftutils.o
	$(CC) $(CFLAGS) -o cift15 cift15.o ciftutils.o @KROC_CCSP_LDFLAGS@ @KROC_CCSP_LIBPATH@ -lccsp -lpthread @KROC_CCSP_LIBS@

cift16: cift16.occ cift16-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift16.occ cift16-c.o ciftutils.o

cift17: cift17.occ cift17-c.o ciftutils.o
	$(OCCBUILD) --program $(srcdir)/cift17.occ cift17-c.o ciftutils.o

cift17-c.o: cift17-c.c
	$(CC) -O1 $(CFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<
