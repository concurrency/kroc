AVRDUDE = avrdude
AVRDUDE_FLAGS = -V -F -p $(MCU) -P $(UPLOAD_PORT) -b $(UPLOAD_RATE) -c stk500v1
BYTECODE_ADDR = 0x5000
F_CPU = 16000000
MCU = atmega328p
UPLOAD_PORT = /dev/ttyUSB0
UPLOAD_RATE = 57600