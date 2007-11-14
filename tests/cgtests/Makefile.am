@SET_MAKE@

# NOTE: cgtest17 gives lots of 'warnings' when translating
# NOTE: cgtest19 needs the '-s' flag to put KERNEL.RUN workspace in program data

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --occ21-opts '-mpt'

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
	cgtest90.occ cgtest91.occ cgtest92.occ

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

cglib_lib_objs = cglib.tce

libs = cglib.lib

all-local: $(libs) $(tests)

cglib.lib: $(cglib_lib_objs)
	$(OCCBUILD) --library cglib.lib $(cglib_lib_objs)

install-exec-local: $(libs) $(tests)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs)
	$(OCCBUILD) --install-examples cgtests --prefix "$(prefix)" --destdir "$(DESTDIR)" $(tests)

clean-local:
	$(OCCBUILD) --clean $(tests) $(libs) $(cglib_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

%: %.occ
	$(OCCBUILD) --program $<

update-deps:
	pideps $(addprefix -b ,$(tests)) *.occ

## begin pideps dependencies
$(srcdir)/cgmain.occ: cglib.lib
cgtest00: $(srcdir)/cgmain.occ
cgtest01: $(srcdir)/cgmain.occ
cgtest02: $(srcdir)/cgmain.occ
cgtest03: $(srcdir)/cgmain.occ
cgtest04: $(srcdir)/cgmain.occ
cgtest05: $(srcdir)/cgmain.occ
cgtest06: $(srcdir)/cgmain.occ
cgtest06: $(srcdir)/cgreal.occ
cgtest07: $(srcdir)/cgmain.occ
cgtest08: $(srcdir)/cgmain.occ
cgtest09: $(srcdir)/cgmain.occ
cgtest10: $(srcdir)/cg10misc.occ
cgtest10: $(srcdir)/cg10r32.occ
cgtest10: $(srcdir)/cg10r64.occ
cgtest10: $(srcdir)/cgmain.occ
cgtest10: $(srcdir)/cgreal.occ
cgtest11: $(srcdir)/cg10byte.occ
cgtest11: $(srcdir)/cg10int.occ
cgtest11: $(srcdir)/cg10long.occ
cgtest11: $(srcdir)/cgmain.occ
cgtest11: $(srcdir)/cgreal.occ
cgtest12: $(srcdir)/cgmain.occ
cgtest13: $(srcdir)/cgmain.occ
cgtest14: $(srcdir)/cgmain.occ
cgtest14: $(srcdir)/cgreal.occ
cgtest15: $(srcdir)/cgmain.occ
cgtest15: $(srcdir)/cgreal.occ
cgtest16: $(srcdir)/cgmain.occ
cgtest17: $(srcdir)/cgmain.occ
cgtest18: $(srcdir)/cgmain.occ
cgtest19: $(srcdir)/cgmain.occ
cgtest19: $(srcdir)/cgreal.occ
cgtest19: $(srcdir)/config.inc
cgtest20: $(srcdir)/cg10i16.occ
cgtest20: $(srcdir)/cgmain.occ
cgtest20: $(srcdir)/cgreal.occ
cgtest21: $(srcdir)/cg10i32.occ
cgtest21: $(srcdir)/cgmain.occ
cgtest21: $(srcdir)/cgreal.occ
cgtest22: $(srcdir)/cg10i64.occ
cgtest22: $(srcdir)/cgmain.occ
cgtest22: $(srcdir)/cgreal.occ
cgtest23: $(srcdir)/cgmain.occ
cgtest24: $(srcdir)/cgmain.occ
cgtest24: $(srcdir)/cgreal.occ
cgtest25: $(srcdir)/cgmain.occ
cgtest25: $(srcdir)/cgreal.occ
cgtest26: $(srcdir)/cgmain.occ
cgtest26: $(srcdir)/cgreal.occ
cgtest27: $(srcdir)/cgmain.occ
cgtest27: $(srcdir)/cgreal.occ
cgtest50: $(srcdir)/cg50myi.occ
cgtest50: $(srcdir)/cgmain.occ
cgtest50: $(srcdir)/cgreal.occ
cgtest51: $(srcdir)/cg50myi.occ
cgtest51: $(srcdir)/cgmain.occ
cgtest51: $(srcdir)/cgreal.occ
cgtest52: $(srcdir)/cg50myi.occ
cgtest52: $(srcdir)/cgmain.occ
cgtest52: $(srcdir)/cgreal.occ
cgtest53: $(srcdir)/cg50myi.occ
cgtest53: $(srcdir)/cgmain.occ
cgtest53: $(srcdir)/cgreal.occ
cgtest54: $(srcdir)/cg50myr.occ
cgtest54: $(srcdir)/cgmain.occ
cgtest54: $(srcdir)/cgreal.occ
cgtest55: $(srcdir)/cg50myr.occ
cgtest55: $(srcdir)/cgmain.occ
cgtest55: $(srcdir)/cgreal.occ
cgtest56: $(srcdir)/cgmain.occ
cgtest56: $(srcdir)/cgreal.occ
cgtest57: $(srcdir)/cg57type.occ
cgtest57: $(srcdir)/cgmain.occ
cgtest57: $(srcdir)/cgreal.occ
cgtest58: $(srcdir)/cg58arr.occ
cgtest58: $(srcdir)/cg58rec.occ
cgtest58: $(srcdir)/cgmain.occ
cgtest58: $(srcdir)/cgreal.occ
cgtest59: $(srcdir)/cgmain.occ
cgtest59: $(srcdir)/cgreal.occ
cgtest60: $(srcdir)/cg60myi.occ
cgtest60: $(srcdir)/cgmain.occ
cgtest60: $(srcdir)/cgreal.occ
cgtest61: $(srcdir)/cg60myi.occ
cgtest61: $(srcdir)/cgmain.occ
cgtest61: $(srcdir)/cgreal.occ
cgtest62: $(srcdir)/cg60myi.occ
cgtest62: $(srcdir)/cgmain.occ
cgtest62: $(srcdir)/cgreal.occ
cgtest63: $(srcdir)/cg60myi.occ
cgtest63: $(srcdir)/cgmain.occ
cgtest63: $(srcdir)/cgreal.occ
cgtest64: $(srcdir)/cg60myr.occ
cgtest64: $(srcdir)/cgmain.occ
cgtest64: $(srcdir)/cgreal.occ
cgtest65: $(srcdir)/cg60myr.occ
cgtest65: $(srcdir)/cgmain.occ
cgtest65: $(srcdir)/cgreal.occ
cgtest66: $(srcdir)/cg66cnvt.occ
cgtest66: $(srcdir)/cgmain.occ
cgtest66: $(srcdir)/cgreal.occ
cgtest67: $(srcdir)/cg67cplx.occ
cgtest67: $(srcdir)/cgmain.occ
cgtest67: $(srcdir)/cgreal.occ
cgtest70: $(srcdir)/cgmain.occ
cgtest70: $(srcdir)/cgreal.occ
cgtest71: $(srcdir)/cgmain.occ
cgtest71: $(srcdir)/cgreal.occ
cgtest72: $(srcdir)/cgmain.occ
cgtest72: $(srcdir)/cgreal.occ
cgtest73: $(srcdir)/cgmain.occ
cgtest73: $(srcdir)/cgreal.occ
cgtest74: $(srcdir)/cgmain.occ
cgtest74: $(srcdir)/cgreal.occ
cgtest75: $(srcdir)/cgmain.occ
cgtest75: $(srcdir)/cgreal.occ
cgtest76: $(srcdir)/cgmain.occ
cgtest76: $(srcdir)/cgreal.occ
cgtest77: $(srcdir)/cgmain.occ
cgtest77: $(srcdir)/cgreal.occ
cgtest78: $(srcdir)/cgmain.occ
cgtest78: $(srcdir)/cgreal.occ
cgtest79: $(srcdir)/cgmain.occ
cgtest79: $(srcdir)/cgreal.occ
cgtest80: $(srcdir)/cgmain.occ
cgtest80: $(srcdir)/cgreal.occ
cgtest81: $(srcdir)/cgmain.occ
cgtest82: $(srcdir)/cgmain.occ
cgtest82: $(srcdir)/cgreal.occ
cgtest83: $(srcdir)/cgmain.occ
cgtest83: $(srcdir)/cgreal.occ
cgtest84: $(srcdir)/cgmain.occ
cgtest84: $(srcdir)/cgreal.occ
cgtest85: $(srcdir)/cgmain.occ
cgtest85: $(srcdir)/cgreal.occ
cgtest86: $(srcdir)/cgmain.occ
cgtest86: $(srcdir)/cgreal.occ
cgtest87: $(srcdir)/cgmain.occ
cgtest87: $(srcdir)/cgreal.occ
cgtest88: $(srcdir)/cgmain.occ
cgtest88: $(srcdir)/cgreal.occ
cgtest89: $(srcdir)/cgmain.occ
cgtest89: $(srcdir)/cgreal.occ
cgtest90: $(srcdir)/cgmain.occ
cgtest90: $(srcdir)/cgreal.occ
cgtest91: $(srcdir)/cgmain.occ
cgtest91: $(srcdir)/cgreal.occ
cgtest92: $(srcdir)/cgmain.occ
cgtest92: $(srcdir)/cgreal.occ
## end pideps dependencies
