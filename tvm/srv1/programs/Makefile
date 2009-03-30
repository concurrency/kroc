JAVAC = javac
SKROC = skroc -L .. -L ../firmware -L ../include --blackfin --no-std-libs --keep-temp-files

PROGRAMS = \
	dark.srv \
	dark-half.srv \
	camera-rgb.srv \
	error1.srv \
	error2.srv \
	error3.srv \
	hello.srv \
	id.srv \
	multi-dark.srv \
	srv1.srv \
	tests.srv \
	SRV.class

HEADERS = \
	../firmware/srv1.inc \
	../firmware/out.occ

%.srv: %.occ $(HEADERS)
	$(SKROC) $<

%.class: %.java
	$(JAVAC) $<

all: $(PROGRAMS)

clean:
	rm -f *.srv *.tce *.class
