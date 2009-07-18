## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../../../m4

srcdir = @srcdir@
top_srcdir = @top_srcdir@
VPATH = @srcdir@
INCLUDES = -I. -I@srcdir@/include
DEFS = @DEFS@ @ARCH_DEFS@ -Wall
EXTRALDFLAGS = @EXTRA_LDFLAGS@

TESTS_ENVIRONMENT = $(srcdir)/../run_occ21_test.py $(top_srcdir)/occ21
TESTS = \
	alt-extended-rendevous.occ \
	alt-protocol-of-signal.occ \
	alt-protocol-of-signal2.occ \
	alt-protocol-of-signal3.occ \
	alt-protocol-of-signal4.occ \
	alt-protocol-of-signal5.occ \
	array-of-signals.occ \
	assignment1.occ \
	assignment2.occ \
	assignment3.occ \
	assignment4.occ \
	cast-signal.occ \
	chan-direction-annotation.occ \
	chan_array_signal_decl.occ \
	chan_signal_decl.occ \
	counted_array1.occ \
	counted_array2.occ \
	datatype-signal.occ \
	declaring-signal-variable.occ \
	extended-rendevous.occ \
	int-on-signal-channel.occ \
	nested-signals.occ \
	parallel-read.occ \
	parallel-read2.occ \
	parallel-write.occ \
	protocol-of-signal.occ \
	protocol-of-signal2.occ \
	protocol-of-signal3.occ \
	protocol-of-signal4.occ \
	protocol-of-signal5.occ \
	protocol-of-signal6.occ \
	rename-datatype.occ \
	repl-test.occ \
	signal-on-chan-int.occ \
	val-signal.occ \
	val-signal2.occ \
	variable_decl.occ

clean: 
	rm -f *.tbc
	rm -f *.tce
	rm -f ${TESTS:.occ=}
