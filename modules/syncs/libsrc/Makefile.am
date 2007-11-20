@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

libs = \
	barrier.lib \
	bucket.lib \
	crew.lib \
	semaphore.lib
includes = \
	barrier.inc \
	bucket.inc \
	crew.inc \
	semaphore.inc

EXTRA_DIST = \
	README \
	$(includes)

all-local: $(libs)

barrier.lib:
	$(OCCBUILD) --library barrier.lib --include barrier.inc

bucket.lib:
	$(OCCBUILD) --library bucket.lib --include bucket.inc

crew.lib:
	$(OCCBUILD) --library crew.lib --need semaphore --include crew.inc

semaphore.lib:
	$(OCCBUILD) --library semaphore.lib --include semaphore.inc

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs)

SUFFIXES = .lib .inc

crew.lib: semaphore.lib
