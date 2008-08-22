ARMGCC   ?= /Users/jadudm/svn/arm-elf
VERSION  ?= 4.2.0
PORT     ?= /dev/tty.usbserial


CROSS    = $(ARMGCC)/bin/arm-elf-
CC       = $(CROSS)gcc
LD       = $(CROSS)ld
AR       = $(CROSS)ar
AS       = $(CROSS)as
CP       = $(CROSS)objcopy
OD       = $(CROSS)objdump
RANLIB   = $(CROSS)ranlib
NATIVEGCC= gcc

LIBGCC   = -L$(ARMGCC)/lib/gcc/arm-elf/$(VERSION)/ -lgcc 

INCLUDES = \
		-I./include \
		-I./libtvm \
		-I../../runtime/libtvm \
		-I./firmware

CFLAGS   = -Wall $(INCLUDES) -c -fno-common # -O3 # For another day
AFLAGS   = -ahls -mapcs-32
LDFLAGS  = -Map main.map -Tldscript.cmd
CPFLAGS  = -O ihex
ODFLAGS  = -x --syms

OBJECTS  = crt.o irqs.o tvm.o timer.o debug.o setup.o main.o 
TVMOBJS = libtvm/libtvm.a

all: main.hex
run: download reset

reset: tools/serial_control.py
	./tools/serial_control.py --reset $(PORT)

download: tools/lpc21isp main.hex
	./tools/lpc21isp -control main.hex $(PORT) 19200 20000


main.out: $(TVMOBJS) $(OBJECTS) ldscript.cmd
	$(LD) $(LDFLAGS) -o main.out $(OBJECTS) $(TVMOBJS) $(LIBGCC) 

main.hex: main.out
	$(CP) $(CPFLAGS) main.out main.hex

libtvm/libtvm.a: libtvm.sh
	CC=$(CC) CFLAGS=-nostdlib ./libtvm.sh
	$(RANLIB) libtvm/libtvm.a

tools/lpc21isp: tools/lpc21isp-ipre.c
	$(NATIVEGCC) -o tools/lpc21isp tools/lpc21isp-ipre.c

clean:
	rm -f $(OBJECTS) main.out main.hex main.map

distclean: clean
	rm -rf libtvm
	rm -f tools/lpc21isp
