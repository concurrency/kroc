#!/bin/bash

#./checkout-source.py --path trunk/
./checkout-source.py --path branches/avr-atmega2560

./configure-tvm-avr.py
./build-tvm-avr.py
./install-tvm-avr.py
./build-firmwares-tvm-avr.py
