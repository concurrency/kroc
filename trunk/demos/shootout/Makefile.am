@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	chameneos.occ \
	chameneos-redux.occ \
	cheap-concurrency.occ \
	fannkuch.occ \
	fasta.occ \
	mandelbrot.occ \
	pidigits.occ \
	random.occ \
	reverse-complement.occ \
	spectralnorm.occ \
	startup.occ \
	thread-ring.occ \
	threads.occ \
	threads-flow.occ

if BUILD_SHOOTOUT
examples = \
	chameneos \
	chameneos-redux \
	cheap-concurrency \
	fannkuch \
	fasta \
	mandelbrot \
	pidigits \
	random \
	reverse-complement \
	spectralnorm \
	startup \
	thread-ring \
	threads \
	threads-flow
endif

shootoutdir = $(prefix)/share/kroc/shootout
dist_shootout_DATA = \
	revcomp-input.txt \
	revcomp-output.txt

all-local: $(examples)

chameneos: chameneos.occ
	$(OCCBUILD) --program $(srcdir)/chameneos.occ

chameneos-redux: chameneos-redux.occ
	$(OCCBUILD) --program $(srcdir)/chameneos-redux.occ

cheap-concurrency: cheap-concurrency.occ
	$(OCCBUILD) --program $(srcdir)/cheap-concurrency.occ

fannkuch: fannkuch.occ
	$(OCCBUILD) --program $(srcdir)/fannkuch.occ

fasta: fasta.occ
	$(OCCBUILD) --program $(srcdir)/fasta.occ

mandelbrot: mandelbrot.occ
	$(OCCBUILD) --program $(srcdir)/mandelbrot.occ

pidigits: pidigits.occ
	$(OCCBUILD) --program $(srcdir)/pidigits.occ

random: random.occ
	$(OCCBUILD) --program $(srcdir)/random.occ

reverse-complement: reverse-complement.occ
	$(OCCBUILD) --program $(srcdir)/reverse-complement.occ

spectralnorm: spectralnorm.occ
	$(OCCBUILD) --program $(srcdir)/spectralnorm.occ

startup: startup.occ
	$(OCCBUILD) --program $(srcdir)/startup.occ

thread-ring: thread-ring.occ
	$(OCCBUILD) --program $(srcdir)/thread-ring.occ

threads: threads.occ
	$(OCCBUILD) --program $(srcdir)/threads.occ

threads-flow: threads-flow.occ
	$(OCCBUILD) --program $(srcdir)/threads-flow.occ

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples shootout --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
