@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
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
	thread-ring.occ

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
	thread-ring

all-local: $(examples)

chameneos: chameneos.occ
	$(OCCBUILD) --program $<

chameneos-redux: chameneos-redux.occ
	$(OCCBUILD) --program $<

cheap-concurrency: cheap-concurrency.occ
	$(OCCBUILD) --program $<

fannkuch: fannkuch.occ
	$(OCCBUILD) --program $<

fasta: fasta.occ
	$(OCCBUILD) --program $<

mandelbrot: mandelbrot.occ
	$(OCCBUILD) --program $<

pidigits: pidigits.occ
	$(OCCBUILD) --program $<

random: random.occ
	$(OCCBUILD) --program $<

reverse-complement: reverse-complement.occ
	$(OCCBUILD) --program $<

spectralnorm: spectralnorm.occ
	$(OCCBUILD) --program $<

startup: startup.occ
	$(OCCBUILD) --program $<

thread-ring: thread-ring.occ
	$(OCCBUILD) --program $<

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples shootout --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
