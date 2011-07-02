#!/bin/bash

#./checkout-source.py --path trunk/
#./checkout-source.py --path branches/avr-atmega2560
./checkout-source.py --path branches/avr-sleep

./configure-tvm-avr.py
./build-tvm-avr.py
./install-tvm-avr.py
./build-firmwares-tvm-avr.py
./copy-avr-build-tvm-avr.py
