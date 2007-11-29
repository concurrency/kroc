@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
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
	cift17-c.c cift17.occ

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

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples cif --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) *.o

SUFFIXES = .occ

%: %.occ %-c.o ciftutils.o
	$(OCCBUILD) --program $^

cift15: cift15.o ciftutils.o
	$(CC) -o $@ $^ -L$(libdir) -lccsp -lpthread @LIBDL@

.c.o:
	$(CC) $(CFLAGS) -c $<
