
AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

PERL = perl
SOURCES = \
	build.pl \
	dump.pl \
	main.pl \
	ETC.pm \
	Instructions.pm \
	Linker.pm \
	PLinker.pm \
	TCOFF.pm \
	TEncode.pm \
	TVM.pm	

generated = \
	plinker.pl \
	tce-dump.pl

bin_SCRIPTS = $(generated)

all-local: $(generated)

$(generated): $(SOURCES) 
	$(PERL) build.pl

