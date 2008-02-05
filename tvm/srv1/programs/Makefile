SKROC = skroc -L .. -L ../include --blackfin --no-std-libs --keep-temp-files

PROGRAMS = \
	camera-rgb.srv \
	error1.srv \
	error2.srv \
	error3.srv \
	hello.srv \
	id.srv \
	tests.srv

%.srv: %.occ
	$(SKROC) $<

all: $(PROGRAMS)

tests: *.occ

clean:
	rm -f *.srv *.tce
