all: motors.srv camera-rgb.srv srv1.srv lasers.srv init.srv

SKROC=skroc -L .. -L ../include --blackfin --no-std-libs

motors.srv: motors.occ
	$(SKROC) motors.occ

camera-rgb.srv: camera-rgb.occ
	$(SKROC) camera-rgb.occ

srv1.srv: srv1.occ
	$(SKROC) srv1.occ

lasers.srv: lasers.occ
	$(SKROC) lasers.occ

init.srv: init.occ
	$(SKROC) init.occ

clean:
	rm -f *.srv
