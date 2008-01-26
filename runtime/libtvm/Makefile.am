## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign

CFLAGS=-I$(top_srcdir) @CFLAGS@

lib_LIBRARIES = libtvm.a

noinst_HEADERS = \
	dmem.h \
	ext_chan.h \
	pool_alloc.h \
	instructions.h \
	ins_alt.h \
	ins_barrier.h \
	ins_chan.h \
	ins_fred.h \
	ins_mt.h \
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
	tvm_config.h \
	tvm_mem.h \
	tvm_mem_intf.h \
	tvm_time.h \
	tvm_types.h

libtvm_a_SOURCES = \
	ext_chan.c \
	interpreter.c \
	mem_array.c \
	mem_bigendian.c \
	pool_alloc.c \
	scheduler.c \
	transputer.c

if TVM_DISPATCH_SWITCH
else
libtvm_a_SOURCES += \
	dmem.c \
	ins_alt.c \
	ins_barrier.c \
	ins_chan.c \
	ins_float.c \
	ins_fred.c \
	ins_mt.c \
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

# Rebuild the instruction dispatching code if necessary (and if we can).
DISPATCH_SOURCE_FILES = \
	dmem.c \
	ext_chan.c \
	ins_alt.c \
	ins_barrier.c \
	ins_chan.c \
	ins_float.c \
	ins_fred.c \
	ins_mt.c \
	ins_pri.c \
	ins_proc.c \
	ins_rmox.c \
	ins_sec.c \
	ins_t800.c \
	ins_timer.c
DISPATCH_OUTPUT_FILES = \
	dispatch_ins.c jumptbl_pri.c jumptbl_sec.c jumptbl_ex_sec.c

make_dispatch = make-dispatch.py

if HAVE_PYTHON
$(DISPATCH_OUTPUT_FILES): $(make_dispatch) $(DISPATCH_SOURCE_FILES)
	$(PYTHON) $(make_dispatch) $(DISPATCH_SOURCE_FILES)
endif

EXTRA_DIST = \
	dispatch_ins.c \
	make-dispatch.py

splint:
	splint $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(SOURCES)
