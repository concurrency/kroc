OCCAMADDR = 0x5000
INSTALL_DIR = $(HOME)/src/arduino-0016
PORT = /dev/ttyUSB0
AVRDUDE_PROGRAMMER = stk500v1
F_CPU = 16000000

# For Arduino Duemilanove
UPLOAD_RATE = 57600
MCU = atmega328p
# For Sanguino
#UPLOAD_RATE = 38400
#MCU = atmega644p

AVRDUDE = avrdude
AVRDUDE_PORT = $(PORT)
AVRDUDE_FLAGS = -V -F -C $(INSTALL_DIR)/hardware/tools/avrdude.conf \
-p $(MCU) -P $(AVRDUDE_PORT) -c $(AVRDUDE_PROGRAMMER) \
-b $(UPLOAD_RATE)
