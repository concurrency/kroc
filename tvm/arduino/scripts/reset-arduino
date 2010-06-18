#!/usr/bin/python
# Reset an Arduino board by toggling DTR.
# Usage: reset-arduino PORT

import sys, serial, time

ser = serial.Serial(sys.argv[1], 57600)

ser.setDTR(0)
time.sleep(0.1)
ser.setDTR(1)

ser.close()
