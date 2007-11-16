@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	imageviewer.occ \
	mrevents.occ \
	mrstar8.occ \
	sdlmatrix.occ \
	sdlsim1.occ \
	sdlsim2.occ \
	sdlstar.occ \
	sdlstar8.occ \
	sdltest.occ \
	starfield.occ \
	mrshadebob.occ \
	mrtestprog.occ

examples = \
	imageviewer \
	mrevents \
	mrstar8 \
	sdlmatrix \
	sdlsim1 \
	sdlsim2 \
	sdlstar \
	sdlstar8 \
	sdltest \
	starfield \
	mrshadebob \
	mrtestprog

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples sdlraster --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

%: %.occ
	$(OCCBUILD) --program $<
