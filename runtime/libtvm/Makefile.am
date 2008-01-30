## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign

CFLAGS=-I$(top_srcdir) @CFLAGS@

lib_LIBRARIES = libtvm.a

noinst_HEADERS = \
	ext_chan.h \
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
	ins_timer.h \
	interpreter.h \
	jumptbl_ex_sec.h \
	jumptbl_pri.h \
	jumptbl_sec.h \
	scheduler.h

include_HEADERS = \
	transputer.h \
	tvm.h \
	tvm_compiler.h \
	tvm_config.h \
	tvm_mem.h \
	tvm_mem_intf.h \
	tvm_time.h \
	tvm_types.h

libtvm_a_SOURCES = \
	interpreter.c \
	mem_array.c \
	mem_bigendian.c \
	scheduler.c

if TVM_DISPATCH_SWITCH
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
	instructions.c \
	jumptbl_pri.c \
	jumptbl_sec.c \
	jumptbl_ex_sec.c \
	mem.c \
	scheduler.c
endif

DISPATCH_SOURCE_FILES = \
	$(top_srcdir)/ext_chan.c \
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
	$(top_srcdir)/ins_timer.c

DISPATCH_OUTPUT_FILES = \
	dispatch_ins.c jumptbl_pri.c jumptbl_sec.c jumptbl_ex_sec.c

make_dispatch = $(top_srcdir)/make-dispatch.py

$(DISPATCH_OUTPUT_FILES): $(make_dispatch) $(DISPATCH_SOURCE_FILES)
	$(PYTHON) $(make_dispatch) $(DISPATCH_SOURCE_FILES)

EXTRA_DIST = \
	make-dispatch.py

splint:
	splint $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(SOURCES)
