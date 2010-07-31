## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

CFLAGS=-I$(top_srcdir) @CFLAGS@

lib_LIBRARIES = libtvm.a

noinst_HEADERS = \
	instructions.h \
	ins_alt.h \
	ins_barrier.h \
	ins_chan.h \
	ins_pi.h \
	ins_mobile.h \
	ins_pri.h \
	ins_proc.h \
	ins_rmox.h \
	ins_sec.h \
	ins_t800.h \
	ins_t9000.h \
	ins_timer.h \
	interpreter.h \
	jumptbl_ex_sec.h \
	jumptbl_pri.h \
	jumptbl_sec.h \
	scheduler.h

tvmincludedir = $(includedir)/tvm

tvminclude_HEADERS = \
	mobile_types.h \
	transputer.h \
	tvm.h \
	tvm_compiler.h \
	tvm_config.h \
	tvm_mem.h \
	tvm_mem_intf.h \
	tvm_tbc.h \
	tvm_time.h \
	tvm_types.h

libtvm_a_SOURCES = \
	interpreter.c \
	mem_array.c \
	mem_avr.c \
	mem_bigendian.c \
	scheduler.c \
	tencode.c
nodist_libtvm_a_SOURCES =

if TVM_DISPATCH_SWITCH
interpreter.$(OBJEXT): $(top_builddir)/dispatch_ins.c
else
libtvm_a_SOURCES += \
	ins_alt.c \
	ins_barrier.c \
	ins_chan.c \
	ins_float.c \
	ins_pi.c \
	ins_mobile.c \
	ins_pri.c \
	ins_proc.c \
	ins_rmox.c \
	ins_sec.c \
	ins_timer.c \
	ins_t800.c \
	ins_t9000.c \
	instructions.c \
	mem.c
nodist_libtvm_a_SOURCES += \
	$(top_builddir)/jumptbl_pri.c \
	$(top_builddir)/jumptbl_sec.c \
	$(top_builddir)/jumptbl_ex_sec.c
endif

if TVM_TLSF
libtvm_a_SOURCES += tlsf.c
endif

DISPATCH_SOURCE_FILES = \
	$(top_srcdir)/ins_alt.c \
	$(top_srcdir)/ins_barrier.c \
	$(top_srcdir)/ins_chan.c \
	$(top_srcdir)/ins_float.c \
	$(top_srcdir)/ins_pi.c \
	$(top_srcdir)/ins_mobile.c \
	$(top_srcdir)/ins_pri.c \
	$(top_srcdir)/ins_proc.c \
	$(top_srcdir)/ins_rmox.c \
	$(top_srcdir)/ins_sec.c \
	$(top_srcdir)/ins_t800.c \
	$(top_srcdir)/ins_t9000.c \
	$(top_srcdir)/ins_timer.c

DISPATCH_OUTPUT_FILES = \
	$(top_builddir)/dispatch_ins.c \
	$(top_builddir)/ins_names.h \
	$(top_builddir)/instructions.inc \
	$(top_builddir)/jumptbl_pri.c \
	$(top_builddir)/jumptbl_sec.c \
	$(top_builddir)/jumptbl_ex_sec.c

make_dispatch = $(top_srcdir)/make-dispatch.py

dispatch-cookie: $(make_dispatch) $(DISPATCH_SOURCE_FILES)
	$(PYTHON) $(make_dispatch) $(DISPATCH_SOURCE_FILES)
	touch dispatch-cookie

$(DISPATCH_OUTPUT_FILES): dispatch-cookie

BUILT_SOURCES = \
	$(DISPATCH_OUTPUT_FILES)

EXTRA_DIST = \
	make-dispatch.py

CLEANFILES = $(DISPATCH_OUTPUT_FILES) dispatch-cookie
