## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS=foreign

SCHED_SOURCE = $(top_srcdir)/kernel/sched.c
MAKE_HEADER = $(top_srcdir)/utils/make-header.py

SUBDIRS = arch i386

krocincludedir = $(includedir)/kroc

# Provided public headers.
krocinclude_HEADERS = \
	ccsp.h \
	ccsp_cif.h \
	ccsp_config.h \
	ccsp_consts.h \
	ccsp_if.h \
	ccsp_pony.h \
	ccsp_stats.h \
	ccsp_timer.h \
	dmem_if.h \
	kiface.h \
	mobile_types.h \
	mobproc.h \
	mwsync.h \
	typedesc.h \
	ukcthreads_types.h

# Provided private headers.
noinst_HEADERS = \
	bsyscalls_if.h \
	compiler.h \
	deadlock.h \
	debug.h \
	debug_defs.h \
	dtrace.h \
	dynproc.h \
	inlining.h \
	kernel.h \
	mws_sync.h \
	rts.h \
	sched_consts.h \
	sched_types.h

# Generated public headers.
nodist_krocinclude_HEADERS = \
	ccsp_cif_stubs.h \
	kitable.h

# Generated private headers.
gen_headers = \
	$(nodist_krocinclude_HEADERS) \
	calltable.h

all-local: $(gen_headers)

kitable.h: $(SCHED_SOURCE) $(MAKE_HEADER)
	python $(MAKE_HEADER) --kitable kitable.h $(top_builddir)/config.h $(SCHED_SOURCE)

calltable.h: $(SCHED_SOURCE) $(MAKE_HEADER)
	python $(MAKE_HEADER) --calltable calltable.h $(top_builddir)/config.h $(SCHED_SOURCE)

ccsp_cif_stubs.h: $(SCHED_SOURCE) $(MAKE_HEADER)
	python $(MAKE_HEADER) --cif ccsp_cif_stubs.h $(top_builddir)/config.h $(SCHED_SOURCE)

CLEANFILES = $(gen_headers)
