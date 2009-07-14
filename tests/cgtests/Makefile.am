@SET_MAKE@

# NOTE: cgtest17 gives lots of 'warnings' when translating
# NOTE: cgtest19 needs the '-s' flag to put KERNEL.RUN workspace in program data

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	README \
	cg10byte.occ cg10i16.occ cg10i32.occ cg10i64.occ cg10int.occ \
	cg10long.occ cg10misc.occ cg10r32.occ cg10r64.occ \
	cg50myi.occ cg50myr.occ cg57type.occ cg58arr.occ cg58rec.occ \
	cg60myi.occ cg60myr.occ cg66cnvt.occ cg67cplx.occ \
	cglib.occ \
	cgmain.occ \
	cgreal.occ \
	cgtest00.occ cgtest01.occ cgtest02.occ cgtest03.occ cgtest04.occ \
	cgtest05.occ cgtest06.occ cgtest07.occ cgtest08.occ cgtest09.occ \
	cgtest10.occ cgtest11.occ cgtest12.occ cgtest13.occ cgtest14.occ \
	cgtest15.occ cgtest16.occ cgtest17.occ cgtest18.occ cgtest19.occ \
	cgtest20.occ cgtest21.occ cgtest22.occ cgtest23.occ cgtest24.occ \
	cgtest25.occ cgtest26.occ cgtest27.occ \
	cgtest50.occ cgtest51.occ cgtest52.occ cgtest53.occ cgtest54.occ \
	cgtest55.occ cgtest56.occ cgtest57.occ cgtest58.occ cgtest59.occ \
	cgtest60.occ cgtest61.occ cgtest62.occ cgtest63.occ cgtest64.occ \
	cgtest65.occ cgtest66.occ cgtest67.occ \
	cgtest70.occ cgtest71.occ cgtest72.occ cgtest73.occ cgtest74.occ \
	cgtest75.occ cgtest76.occ cgtest77.occ cgtest78.occ cgtest79.occ \
	cgtest80.occ cgtest81.occ cgtest82.occ cgtest83.occ cgtest84.occ \
	cgtest85.occ cgtest86.occ cgtest87.occ cgtest88.occ cgtest89.occ \
	cgtest90.occ cgtest91.occ cgtest92.occ \
	config.inc \
	run-tests

tests = \
	cgtest00 cgtest01 cgtest02 cgtest03 cgtest04 \
	cgtest05 cgtest06 cgtest07 cgtest08 cgtest09 \
	cgtest10 cgtest11 cgtest12 cgtest13 cgtest14 \
	cgtest15 cgtest16 cgtest17 cgtest18 cgtest19 \
	cgtest20 cgtest21 cgtest22 cgtest23 cgtest24 \
	cgtest25 cgtest26 cgtest27 \
	cgtest50 cgtest51 cgtest52 cgtest53 cgtest54 \
	cgtest55 cgtest56 cgtest57 cgtest58 cgtest59 \
	cgtest60 cgtest61 cgtest62 cgtest63 cgtest64 \
	cgtest65 cgtest66 cgtest67 \
	cgtest70 cgtest71 cgtest72 cgtest73 cgtest74 \
	cgtest75 cgtest76 cgtest77 cgtest78 cgtest79 \
	cgtest80 cgtest81 cgtest82 cgtest83 cgtest84 \
	cgtest85 cgtest86 cgtest87 cgtest88 cgtest89 \
	cgtest90 cgtest91 cgtest92

TESTS_ENVIRONMENT = $(OCCBUILD) --run
TESTS = $(tests)

cglib_lib_objs = cglib.tce

libs = cglib.lib

all-local: $(libs) $(tests)

cglib.lib: $(cglib_lib_objs)
	$(OCCBUILD) --library cglib.lib $(cglib_lib_objs)

#{{{  build rules for cgtests
cgtest00: cgtest00.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest00.occ

cgtest01: cgtest01.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest01.occ

cgtest02: cgtest02.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest02.occ

cgtest03: cgtest03.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest03.occ

cgtest04: cgtest04.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest04.occ

cgtest05: cgtest05.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest05.occ

cgtest06: cgtest06.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest06.occ

cgtest07: cgtest07.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest07.occ

cgtest08: cgtest08.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest08.occ

cgtest09: cgtest09.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest09.occ

cgtest10: cgtest10.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest10.occ

cgtest11: cgtest11.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest11.occ

cgtest12: cgtest12.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest12.occ

cgtest13: cgtest13.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest13.occ

cgtest14: cgtest14.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest14.occ

cgtest15: cgtest15.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest15.occ

cgtest16: cgtest16.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest16.occ

cgtest17: cgtest17.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest17.occ

cgtest18: cgtest18.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest18.occ

cgtest19: cgtest19.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest19.occ

cgtest20: cgtest20.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest20.occ

cgtest21: cgtest21.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest21.occ

cgtest22: cgtest22.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest22.occ

cgtest23: cgtest23.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest23.occ

cgtest24: cgtest24.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest24.occ

cgtest25: cgtest25.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest25.occ

cgtest26: cgtest26.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest26.occ

cgtest27: cgtest27.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest27.occ

cgtest50: cgtest50.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest50.occ

cgtest51: cgtest51.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest51.occ

cgtest52: cgtest52.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest52.occ

cgtest53: cgtest53.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest53.occ

cgtest54: cgtest54.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest54.occ

cgtest55: cgtest55.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest55.occ

cgtest56: cgtest56.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest56.occ

cgtest57: cgtest57.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest57.occ

cgtest58: cgtest58.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest58.occ

cgtest59: cgtest59.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest59.occ

cgtest60: cgtest60.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest60.occ

cgtest61: cgtest61.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest61.occ

cgtest62: cgtest62.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest62.occ

cgtest63: cgtest63.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest63.occ

cgtest64: cgtest64.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest64.occ

cgtest65: cgtest65.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest65.occ

cgtest66: cgtest66.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest66.occ

cgtest67: cgtest67.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest67.occ

cgtest70: cgtest70.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest70.occ

cgtest71: cgtest71.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest71.occ

cgtest72: cgtest72.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest72.occ

cgtest73: cgtest73.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest73.occ

cgtest74: cgtest74.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest74.occ

cgtest75: cgtest75.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest75.occ

cgtest76: cgtest76.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest76.occ

cgtest77: cgtest77.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest77.occ

cgtest78: cgtest78.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest78.occ

cgtest79: cgtest79.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest79.occ

cgtest80: cgtest80.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest80.occ

cgtest81: cgtest81.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest81.occ

cgtest82: cgtest82.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest82.occ

cgtest83: cgtest83.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest83.occ

cgtest84: cgtest84.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest84.occ

cgtest85: cgtest85.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest85.occ

cgtest86: cgtest86.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest86.occ

cgtest87: cgtest87.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest87.occ

cgtest88: cgtest88.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest88.occ

cgtest89: cgtest89.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest89.occ

cgtest90: cgtest90.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest90.occ

cgtest91: cgtest91.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest91.occ

cgtest92: cgtest92.occ cglib.lib
	$(OCCBUILD) --program $(srcdir)/cgtest92.occ
#}}}

install-exec-local: $(libs) $(tests)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs)
	$(OCCBUILD) --install-examples cgtests --prefix "$(prefix)" --destdir "$(DESTDIR)" $(tests)

clean-local:
	$(OCCBUILD) --clean $(tests) $(libs) $(cglib_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

update-deps:
	pideps $(addprefix -b ,$(tests)) *.occ

## begin pideps dependencies
$(srcdir)/cgtest00.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest01.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest02.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest03.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest04.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest05.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest06.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest06.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest07.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest08.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest09.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest10.occ: $(srcdir)/cg10misc.occ
$(srcdir)/cgtest10.occ: $(srcdir)/cg10r32.occ
$(srcdir)/cgtest10.occ: $(srcdir)/cg10r64.occ
$(srcdir)/cgtest10.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest10.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest11.occ: $(srcdir)/cg10byte.occ
$(srcdir)/cgtest11.occ: $(srcdir)/cg10int.occ
$(srcdir)/cgtest11.occ: $(srcdir)/cg10long.occ
$(srcdir)/cgtest11.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest11.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest12.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest13.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest14.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest14.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest15.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest15.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest16.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest17.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest18.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest19.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest19.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest19.occ: $(srcdir)/config.inc
$(srcdir)/cgtest20.occ: $(srcdir)/cg10i16.occ
$(srcdir)/cgtest20.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest20.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest21.occ: $(srcdir)/cg10i32.occ
$(srcdir)/cgtest21.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest21.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest22.occ: $(srcdir)/cg10i64.occ
$(srcdir)/cgtest22.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest22.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest23.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest24.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest24.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest25.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest25.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest26.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest26.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest27.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest27.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest50.occ: $(srcdir)/cg50myi.occ
$(srcdir)/cgtest50.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest50.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest51.occ: $(srcdir)/cg50myi.occ
$(srcdir)/cgtest51.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest51.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest52.occ: $(srcdir)/cg50myi.occ
$(srcdir)/cgtest52.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest52.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest53.occ: $(srcdir)/cg50myi.occ
$(srcdir)/cgtest53.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest53.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest54.occ: $(srcdir)/cg50myr.occ
$(srcdir)/cgtest54.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest54.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest55.occ: $(srcdir)/cg50myr.occ
$(srcdir)/cgtest55.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest55.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest56.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest56.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest57.occ: $(srcdir)/cg57type.occ
$(srcdir)/cgtest57.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest57.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest58.occ: $(srcdir)/cg58arr.occ
$(srcdir)/cgtest58.occ: $(srcdir)/cg58rec.occ
$(srcdir)/cgtest58.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest58.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest59.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest59.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest60.occ: $(srcdir)/cg60myi.occ
$(srcdir)/cgtest60.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest60.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest61.occ: $(srcdir)/cg60myi.occ
$(srcdir)/cgtest61.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest61.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest62.occ: $(srcdir)/cg60myi.occ
$(srcdir)/cgtest62.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest62.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest63.occ: $(srcdir)/cg60myi.occ
$(srcdir)/cgtest63.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest63.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest64.occ: $(srcdir)/cg60myr.occ
$(srcdir)/cgtest64.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest64.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest65.occ: $(srcdir)/cg60myr.occ
$(srcdir)/cgtest65.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest65.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest66.occ: $(srcdir)/cg66cnvt.occ
$(srcdir)/cgtest66.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest66.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest67.occ: $(srcdir)/cg67cplx.occ
$(srcdir)/cgtest67.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest67.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest70.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest70.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest71.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest71.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest72.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest72.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest73.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest73.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest74.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest74.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest75.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest75.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest76.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest76.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest77.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest77.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest78.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest78.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest79.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest79.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest80.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest80.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest81.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest82.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest82.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest83.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest83.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest84.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest84.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest85.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest85.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest86.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest86.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest87.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest87.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest88.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest88.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest89.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest89.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest90.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest90.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest91.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest91.occ: $(srcdir)/cgreal.occ
$(srcdir)/cgtest92.occ: $(srcdir)/cgmain.occ
$(srcdir)/cgtest92.occ: $(srcdir)/cgreal.occ
## end pideps dependencies
