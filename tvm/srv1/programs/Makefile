all: motors.srv camera-rgb.srv srv1.srv

motors.srv: motors.occ
	skroc --blackfin --no-std-libs motors.occ

camera-rgb.srv: camera-rgb.occ
	skroc --blackfin --no-std-libs camera-rgb.occ

srv1.srv: srv1.occ
	skroc --blackfin --no-std-libs srv1.occ
