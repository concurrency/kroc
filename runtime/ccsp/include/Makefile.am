## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS=foreign

if ARCH_DIR_I386
  USE_DIR=i386
endif
if ARCH_DIR_MIPS
  USE_DIR=mips
endif
if ARCH_DIR_SPARC
  USE_DIR=sparc
endif

SCHED_SOURCE = $(top_srcdir)/kernel/sched.c
MAKE_HEADER = $(top_srcdir)/utils/make-header.py

noinst_HEADERS = \
	dmem_if.h \
	bsyscalls_if.h \
	sched_consts.h \
	sched_types.h \
	dynproc.h \
	mobproc.h \
	rts.h \
	ukcthreads_types.h \
	debug_defs.h \
	pgroup.h \
	typedesc.h \
	mwsync.h \
	dtrace.h

all-local: kitable.h calltable.h ccsp_cif_stubs.h

kitable.h: $(SCHED_SOURCE) $(MAKE_HEADER)
	python $(MAKE_HEADER) --kitable kitable.h $(top_srcdir)/config.h $(SCHED_SOURCE)

calltable.h: $(SCHED_SOURCE) $(MAKE_HEADER)
	python $(MAKE_HEADER) --calltable calltable.h $(top_srcdir)/config.h $(SCHED_SOURCE)

ccsp_cif_stubs.h: $(SCHED_SOURCE) $(MAKE_HEADER)
	python $(MAKE_HEADER) --cif ccsp_cif_stubs.h $(top_srcdir)/config.h $(SCHED_SOURCE)

DISTCLEANFILES= kitable.h calltable.h ccsp_cif_stubs.h

