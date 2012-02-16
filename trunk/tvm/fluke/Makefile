# Hey Emacs, this is a -*- makefile -*-

# based on the WinAVR makefile written by Eric B. Weddington, Jörg Wunsch, et al.
# Released to the Public Domain
#
# On command line:
#
# make all = Make software.
#
# make clean = Clean out built project files.
#
# make program = Download the hex file to the device, using isp21lpc
#
# make filename.s = Just compile filename.c into the assembler code only
#
# To rebuild project do "make clean" then "make all".
#

NATIVEGCC = gcc
SKROC     = skroc
PORT      = /dev/tty.usbserial

TARGET_LIST = FLUKE OLIMEX

# Compiler settings
FLUKE_HOST_BAUD         = 38400
FLUKE_FOSC              = 20000000
FLUKE_PLL_MUL           = 3
FLUKE_PBSD              = 1
# Build settings
FLUKE_LPC21ISP          = tools/lpc21isp-ipre
FLUKE_LPC21ISP_BAUD     = 19200
FLUKE_LPC21ISP_XTAL     = 20000
FLUKE_LPC21ISP_CONTROL  = -control
FLUKE_MINITERM_ARGS     = --dtr=0

# Compiler settings
OLIMEX_HOST_BAUD        = 38400
OLIMEX_FOSC             = 14745600
OLIMEX_PLL_MUL          = 4
OLIMEX_PBSD             = 1
# Build settings
OLIMEX_LPC21ISP         = tools/lpc21isp
OLIMEX_LPC21ISP_BAUD    = 19200
OLIMEX_LPC21ISP_XTAL    = 14746
OLIMEX_LPC21ISP_CONTROL = -control
OLIMEX_MINITERM_ARGS    = 

BOARD_TARGET = \
 $(strip $(shell sed -n -e \
 "s/\/\* Configuration for target: *\([a-zA-Z0-9]*\) *\*/\1/p" \
 config_target.h))
LPC21ISP         = $($(BOARD_TARGET)_LPC21ISP)
LPC21ISP_BAUD    = $($(BOARD_TARGET)_LPC21ISP_BAUD)
LPC21ISP_XTAL    = $($(BOARD_TARGET)_LPC21ISP_XTAL)
LPC21ISP_CONTROL = $($(BOARD_TARGET)_LPC21ISP_CONTROL)
MINITERM_ARGS    = $($(BOARD_TARGET)_MINITERM_ARGS)
HOST_BAUD        = $($(BOARD_TARGET)_HOST_BAUD)

# MCU name and submodel
MCU = arm7tdmi
SUBMDL = LPC2106
# THUMB code is untested for the TVM build.
# THUMB    = -mthumb
# THUMB_IW = -mthumb-interwork

## Create ROM-Image (final)
RUN_MODE=ROM_RUN
## Create RAM-Image (debugging)
## On the Fluke (rev 4) board this fails (currently)
## Most likely due to a confused .ld script.
## RUN_MODE=RAM_RUN

# Output format. (can be srec, ihex, binary)
FORMAT = ihex

# Target file name (without extension).
TARGET = main

# List C source files here. (C dependencies are automatically generated.)
SRC = $(TARGET).c
SRC += sysTime.c fluke_uart.c armVIC.c fluke.c debug.c timer.c version.c

# List Assembler source files here.
# Make them always end in a capital .S.  Files ending in a lowercase .s
# will not be considered source files but generated files (assembler
# output from the compiler), and will be deleted upon "make clean"!
# Even though the DOS/Win* filesystem matches both .s and .S the same,
# it will preserve the spelling of the filenames, and gcc itself does
# care about how the name is spelled on its command-line.
ASRC = crt0.S

# Optimization level, can be [0, 1, 2, 3, s]. 
# 0 = turn off optimization. s = optimize for size.
# (Note: 3 is not always the best optimization level. See avr-libc FAQ.)
OPT = s

# Debugging format.
# Native formats for AVR-GCC's -g are stabs [default], or dwarf-2.
# AVR (extended) COFF requires stabs, plus an avr-objcopy run.
#DEBUG = stabs
DEBUG = dwarf-2

# List any extra directories to look for include files here.
#     Each directory must be seperated by a space.
EXTRAINCDIRS = 

# Compiler flag to set the C Standard level.
# c89   - "ANSI" C
# gnu89 - c89 plus GCC extensions
# c99   - ISO C99 standard (not yet fully implemented)
# gnu99 - c99 plus GCC extensions
CSTANDARD = -std=gnu99

# Place -D or -U options for C here
CDEFS =  -D$(RUN_MODE)

# Place -I options here
CINCS = \
		-I./libtvm \
		-I../../runtime/libtvm \
		-I./firmware

# Place -D or -U options for ASM here
ADEFS =  -D$(RUN_MODE)


# Compiler flags.
#  -g*:          generate debugging information
#  -O*:          optimization level
#  -f...:        tuning, see GCC manual and avr-libc documentation
#  -Wall...:     warning level
#  -Wa,...:      tell GCC to pass this to the assembler.
#    -adhlns...: create assembler listing
CFLAGS = -g$(DEBUG)
CFLAGS += $(CDEFS) $(CINCS)
CFLAGS += -O$(OPT)
CFLAGS += -Wall -Wstrict-prototypes -Wcast-align -Wcast-qual -Wimplicit 
CFLAGS += -Wmissing-declarations
CFLAGS += -Wmissing-prototypes -Wnested-externs -Wpointer-arith -Wswitch
CFLAGS += -Wredundant-decls -Wreturn-type -Wshadow -Wstrict-prototypes -Wunused
CFLAGS += -Wa,-adhlns=$(<:.c=.lst)
CFLAGS += $(patsubst %,-I%,$(EXTRAINCDIRS))
CFLAGS += $(CSTANDARD)
## NONO CFLAGS += -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums

# Assembler flags.
#  -Wa,...:   tell GCC to pass this to the assembler.
#  -ahlms:    create listing
#  -gstabs:   have the assembler create line number information; note that
#             for use in COFF files, additional information about filenames
#             and function names needs to be present in the assembler source
#             files -- see avr-libc docs [FIXME: not yet described there]
##ASFLAGS = -Wa,-adhlns=$(<:.S=.lst),-gstabs 
ASFLAGS = $(ADEFS) -Wa,-adhlns=$(<:.S=.lst),-g$(DEBUG)

#Additional libraries.

#Support for newlibc-lpc (file: libnewlibc-lpc.a)
#NEWLIBLPC = -lnewlib-lpc
NEWLIBCLPC =

MATH_LIB = -lm

# Linker flags.
#  -Wl,...:     tell GCC to pass this to linker.
#    -Map:      create map file
#    --cref:    add cross reference to  map file
LDFLAGS = -nostartfiles -Wl,-Map=$(TARGET).map,--cref
LDFLAGS += -lc
LDFLAGS += $(NEWLIBLPC) $(MATH_LIB)
LDFLAGS += -lc -lgcc
# Strips debug and symbol information
# LDFLAGS += -s

# Set Linker-Script Depending On Selected Memory
ifeq ($(RUN_MODE),RAM_RUN)
LDFLAGS +=-T$(SUBMDL)-RAM.ld
else 
LDFLAGS +=-T$(SUBMDL)-ROM.ld
endif

## -ffunction-sections in addition to the --gc-sections linker flag is useful 
## when doing bits of testing that does not involve the actual TVM runtime. 
## When using these two flags together (one CFLAG and one LDFLAG) unused
## code is removed, possibly generating a much smaller binary. If 
## ie, if -ffunction-sections is enabled, you comment out run_tvm and 
## 'make clean ;  make', you get a very small binary. Very handy.
DCE = NO
ifeq ($(DCE), YES)
CFLAGS += -ffunction-sections
LDFLAGS += -Xlinker --gc-sections
endif

# ---------------------------------------------------------------------------
# Flash-Programming support using lpc21isp by Martin Maurer 

# Settings and variables:
# LPC21ISP = lpc21isp
# LPC21ISP_PORT = com1
LPC21ISP_PORT = $(PORT)
#LPC21ISP_BAUD = 115200
LPC21ISP_BAUD = $(HOST_BAUD)
# LPC21ISP_XTAL = 14746
LPC21ISP_FLASHFILE = $(TARGET).hex
# verbose output:
## LPC21ISP_DEBUG = -debug
# enter bootloader via RS232 DTR/RTS (only if hardware supports this
# feature - see Philips AppNote):
## LPC21ISP_CONTROL = -control
#LPC21ISP_CONTROL = -control


# ---------------------------------------------------------------------------

# Define directories, if needed.
## DIRARM = c:/WinARM/
## DIRARMBIN = $(DIRAVR)/bin/
## DIRAVRUTILS = $(DIRAVR)/utils/bin/

# Define programs and commands.
SHELL = sh
CC = arm-elf-gcc
OBJCOPY = arm-elf-objcopy
OBJDUMP = arm-elf-objdump
SIZE = arm-elf-size
NM = arm-elf-nm
REMOVE = rm -f
COPY = cp
RANLIB = arm-elf-ranlib


# Define Messages
# English
MSG_ERRORS_NONE = Errors: none
MSG_BEGIN = -------- begin --------
MSG_END = --------  end  --------
MSG_SIZE_BEFORE = Size before: 
MSG_SIZE_AFTER = Size after:
MSG_FLASH = Creating load file for Flash:
MSG_EXTENDED_LISTING = Creating Extended Listing:
MSG_SYMBOL_TABLE = Creating Symbol Table:
MSG_LINKING = Linking:
MSG_COMPILING = Compiling:
MSG_ASSEMBLING = Assembling:
MSG_CLEANING = Cleaning project:
MSG_LPC21_RESETREMINDER = You may have to bring the target in bootloader-mode now.


# Define all object files.
OBJ  = $(SRC:.c=.o) $(ASRC:.S=.o) 
LIBS = libtvm/libtvm.a


# Define all listing files.
LST = $(ASRC:.S=.lst) $(SRC:.c=.lst)


# Compiler flags to generate dependency files.
### GENDEPFLAGS = -Wp,-M,-MP,-MT,$(*F).o,-MF,.dep/$(@F).d
GENDEPFLAGS = -MD -MP -MF .dep/$(@F).d

# Combine all necessary flags and optional flags.
# Add target processor to flags.
ALL_CFLAGS = -mcpu=$(MCU) $(THUMB_IW) -I. $(CFLAGS) $(GENDEPFLAGS)
ALL_ASFLAGS = -mcpu=$(MCU) $(THUMB_IW) -I. -x assembler-with-cpp $(ASFLAGS)

FIRMWARE = 	firmware/firmware.occ \
						firmware/out.occ
 

# Default target.
all: begin gccversion sizebefore build sizeafter finished end

build: elf hex lss sym

.PHONY: config
config: create-config

ifneq (config,$(findstring config,$(MAKEFLAGS)))
 ifndef target
	 $(error  Please define a target, ie 'make config target=<target>')
 endif
 ifneq ($(target),$(findstring $(target),$(TARGET_LIST)))
	 $(error Target must be one of: $(TARGET_LIST))
 endif
endif

.PHONY: create-config
create-config: TARGET=$(target)
create-config: HOST_BAUD=$($(TARGET)_HOST_BAUD)
create-config: FOSC=$($(TARGET)_FOSC)
create-config: PLL_MUL=$($(TARGET)_PLL_MUL)
create-config: PBSD=$($(TARGET)_PBSD)
create-config:
	@echo "Creating config for the $(TARGET)"
	@echo "/* Configuration for target: $(TARGET)    * " > config_target.h
	@echo " * Created automagically by the makefile, * " >>config_target.h
	@echo " * run 'make config-_____' to recreate */" >>config_target.h
	@echo "/* Baud of UART0                 */         " >>config_target.h
	@echo "#define HOST_BAUD  ($(HOST_BAUD))           " >>config_target.h
	@echo "/* Master Oscillator Freq.       */         " >>config_target.h
	@echo "#define FOSC       ($(FOSC))                " >>config_target.h
	@echo "/* PLL Multiplier                */         " >>config_target.h
	@echo "#define PLL_MUL    ($(PLL_MUL))             " >>config_target.h
	@echo "/* Pheripheral Bus Speed Divider */         " >>config_target.h
	@echo "#define PBSD       ($(PBSD))                " >>config_target.h

config_target.h:
	$(error run 'make config target=<T>', where T is in [$TARGET_LIST])

elf: $(TARGET).elf
hex: $(TARGET).hex
lss: $(TARGET).lss 
sym: $(TARGET).sym

# Eye candy.
begin:
	@echo
	@echo $(MSG_BEGIN)

finished:
	@echo $(MSG_ERRORS_NONE)

end:
	@echo $(MSG_END)
	@echo


# Display size of file.
HEXSIZE = $(SIZE) --target=$(FORMAT) $(TARGET).hex
ELFSIZE = $(SIZE) -A $(TARGET).elf
sizebefore:
	@if [ -f $(TARGET).elf ]; then echo; echo $(MSG_SIZE_BEFORE); $(ELFSIZE); echo; fi

sizeafter:
	@if [ -f $(TARGET).elf ]; then echo; echo $(MSG_SIZE_AFTER); $(ELFSIZE); echo; fi


# Display compiler version information.
gccversion : 
	@$(CC) --version


# Program the device.  
.PHONY: program
program: $(TARGET).hex $(LPC21ISP)
	@echo
	@echo $(MSG_LPC21_RESETREMINDER)
	$(LPC21ISP) $(LPC21ISP_CONTROL) $(LPC21ISP_DEBUG) $(LPC21ISP_FLASHFILE) $(LPC21ISP_PORT) $(LPC21ISP_BAUD) $(LPC21ISP_XTAL)

last-program: $(TARGET).hex $(LPC21ISP)
	@echo
	@echo $(MSG_LPC21_RESETREMINDER)
	$(LPC21ISP) $(LPC21ISP_CONTROL) $(LPC21ISP_DEBUG) $(LPC21ISP_FLASHFILE) $(LPC21ISP_PORT) $(LPC21ISP_BAUD) $(LPC21ISP_XTAL) && touch last-program

# run, reset, and reset-term interact with the 
# python tool for interacting with the Fluke.
.PHONY: run
run: last-program reset
.PHONY: run-term
run-term: last-program reset-term
.PHONY: run-iterm
run-iterm: last-program reset-iterm

.PHONY: reset
reset: tools/serial_control.py
	  ./tools/serial_control.py --reset $(HOST_BAUD) $(PORT)

.PHONY: reset-term
reset-term: tools/serial_control.py
	  ./tools/serial_control.py --reset --term $(HOST_BAUD) $(PORT)

.PHONY: reset-iterm
reset-iterm: tools/miniterm.py
	  ./tools/miniterm.py $(MINITERM_ARGS) $(PORT) $(HOST_BAUD) 


# Create final output files (.hex, .eep) from ELF output file.
# TODO: handling the .eeprom-section should be redundant
%.hex: %.elf
	@echo
	@echo $(MSG_FLASH) $@
	$(OBJCOPY) -O $(FORMAT) $< $@


# Create extended listing file from ELF output file.
%.lss: %.elf
	@echo
	@echo $(MSG_EXTENDED_LISTING) $@
	$(OBJDUMP) -h -S $< > $@


# Create a symbol table from ELF output file.
%.sym: %.elf
	@echo
	@echo $(MSG_SYMBOL_TABLE) $@
	$(NM) -n $< > $@


# Link: create ELF output file from object files.
.SECONDARY : $(TARGET).elf
.PRECIOUS : $(OBJ) 
%.elf: $(OBJ) $(LIBS)
	@echo
	@echo $(MSG_LINKING) $@
	$(CC) $(THUMB) $(ALL_CFLAGS) $(OBJ) $(LIBS) --output $@ $(LDFLAGS)


# Compile: create object files from C source files.
%.o : %.c
	@echo
	@echo $(MSG_COMPILING) $<
	$(CC) -c $(THUMB) $(ALL_CFLAGS) $< -o $@ 


# Compile: create assembler files from C source files.
%.s : %.c
	$(CC) $(THUMB) -S $(ALL_CFLAGS) $< -o $@


# Assemble: create object files from assembler source files.
%.o : %.S
	@echo
	@echo $(MSG_ASSEMBLING) $<
	$(CC) -c $(ALL_ASFLAGS) $< -o $@


# Target: clean project.
clean: begin clean_list finished end


clean_list :
	@echo
	@echo $(MSG_CLEANING)
	$(REMOVE) $(TARGET).hex
	$(REMOVE) $(TARGET).obj
	$(REMOVE) $(TARGET).elf
	$(REMOVE) $(TARGET).map
	$(REMOVE) $(TARGET).obj
	$(REMOVE) $(TARGET).a90
	$(REMOVE) $(TARGET).sym
	$(REMOVE) $(TARGET).lnk
	$(REMOVE) $(TARGET).lss
	$(REMOVE) $(OBJ)
	$(REMOVE) $(LST)
	$(REMOVE) $(SRC:.c=.s)
	$(REMOVE) $(SRC:.c=.d)
	$(REMOVE) .dep/*
	$(REMOVE) firmware/firmware.h

firmware/firmware.h: $(FIRMWARE)
	$(SKROC) --no-std-libs --c -L firmware --filename firmware/firmware.h firmware/firmware.occ

libtvm/libtvm.a: libtvm.sh
	CC=$(CC) CFLAGS=-nostdlib ./libtvm.sh
	$(RANLIB) libtvm/libtvm.a

# These are generated by other rules.
distclean: clean
	rm -rf libtvm
	rm -f tools/lpc21isp
	rm -f tools/lpc21isp
	rm -f config_target.h
	rm -f last_program

# Build the isp tool if needed
tools/lpc21isp-ipre: tools/lpc21isp-ipre.c
	  $(NATIVEGCC) -o $@ $^

tools/lpc21isp: tools/lpc21isp.c
	  $(NATIVEGCC) -o $@ $^

# Dependencies for the TVM
main.elf: libtvm/libtvm.a

main.o: libtvm/libtvm.a
main.c: firmware/firmware.h

# Include the dependency files.
-include $(shell mkdir .dep 2>/dev/null) $(wildcard .dep/*)

# Listing of phony targets.
.PHONY : all begin finish end sizebefore sizeafter gccversion \
build elf hex lss sym \
clean clean_list program

