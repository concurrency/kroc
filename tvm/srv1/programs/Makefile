JAVAC = javac
SKROC = skroc -L .. -L ../firmware -L ../include --blackfin --no-std-libs --keep-temp-files

PROGRAMS = \
	camera-rgb.srv \
	error1.srv \
	error2.srv \
	error3.srv \
	hello.srv \
	id.srv \
	tests.srv \
	srv1.srv

%.srv: %.occ
	$(SKROC) $<

uploader: SRV.java
	$(JAVAC) $<

all: $(PROGRAMS) uploader

clean:
	rm -f *.srv *.tce *.class
