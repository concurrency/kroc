
AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

PERL = perl
SOURCES = \
	build.pl \
	config.pl \
	main.pl \
	ETC.pm \
	Instructions.pm \
	Linker.pm \
	PLinker.pm \
	TCOFF.pm \
	TEncode.pm \
	TVM.pm	

all-local: plinker.pl

plinker.pl: $(SOURCES) 
	$(PERL) build.pl

