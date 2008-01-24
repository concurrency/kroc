## Makefile.am -- Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign

AM_CFLAGS = @AM_CFLAGS@ @TVM_CFLAGS@

lib_LIBRARIES = libtvm.a

libtvm_a_SOURCES = transputer.c transputer.h \
			   mem.h \
			   mem_bigendian.h mem_native.h \
			   scheduler.c scheduler.h \
			   interpreter.c interpreter.h \
			   hook_timer.h \
			   ext_chan.c ext_chan.h \
			   pool_alloc.c pool_alloc.h \
			   dmem.h \
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
			   ins_timer.h

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
	jumptbl_pri.c jumptbl_pri.h \
	jumptbl_sec.c jumptbl_sec.h \
	jumptbl_ex_sec.c jumptbl_ex_sec.h \
	mem.c \
	mem_native.c
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
