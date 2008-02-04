all: camera-rgb.srv hello.srv id.srv init.srv lasers.srv motors.srv srv1.srv

SKROC=skroc -L .. -L ../include --blackfin --no-std-libs --keep-temp-files

camera-rgb.srv: camera-rgb.occ
	$(SKROC) camera-rgb.occ

hello.srv: hello.occ
	$(SKROC) hello.occ

id.srv: id.occ
	$(SKROC) id.occ

init.srv: init.occ
	$(SKROC) init.occ

lasers.srv: lasers.occ
	$(SKROC) lasers.occ

motors.srv: motors.occ
	$(SKROC) motors.occ

srv1.srv: srv1.occ
	$(SKROC) srv1.occ

clean:
	rm -f *.srv *.tce
