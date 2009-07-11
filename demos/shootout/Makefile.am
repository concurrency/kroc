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
	$(OCCBUILD) --program chameneos.occ

chameneos-redux: chameneos-redux.occ
	$(OCCBUILD) --program chameneos-redux.occ

cheap-concurrency: cheap-concurrency.occ
	$(OCCBUILD) --program cheap-concurrency.occ

fannkuch: fannkuch.occ
	$(OCCBUILD) --program fannkuch.occ

fasta: fasta.occ
	$(OCCBUILD) --program fasta.occ

mandelbrot: mandelbrot.occ
	$(OCCBUILD) --program mandelbrot.occ

pidigits: pidigits.occ
	$(OCCBUILD) --program pidigits.occ

random: random.occ
	$(OCCBUILD) --program random.occ

reverse-complement: reverse-complement.occ
	$(OCCBUILD) --program reverse-complement.occ

spectralnorm: spectralnorm.occ
	$(OCCBUILD) --program spectralnorm.occ

startup: startup.occ
	$(OCCBUILD) --program startup.occ

thread-ring: thread-ring.occ
	$(OCCBUILD) --program thread-ring.occ

threads: threads.occ
	$(OCCBUILD) --program threads.occ

threads-flow: threads-flow.occ
	$(OCCBUILD) --program threads-flow.occ

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples shootout --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
