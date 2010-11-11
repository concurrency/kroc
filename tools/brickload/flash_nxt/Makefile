
CC := $(shell which arm-elf-gcc)
AS := $(shell which arm-elf-as)
LD := $(shell which arm-elf-ld)
OBJCOPY := $(shell which arm-elf-objcopy)

OBJECTS = main.o flash.o

all: flash_driver.bin

%.o: %.c
	$(CC) -mcpu=arm7tdmi -Os -Wextra -Wall -fsigned-char -ffunction-sections -std=gnu99 -fdata-sections -fomit-frame-pointer -msoft-float -c -o $@ $<

%.o: %.s
	$(AS) --warn -mfpu=softfpa -mcpu=arm7tdmi -mapcs-32 -o $@ $<

flash_driver.elf: $(OBJECTS)
	$(LD) -Os --gc-sections --no-check-sections -o $@ $(OBJECTS)

flash_driver.bin: flash_driver.elf
	$(OBJCOPY) -O binary $< $@

clean:
	rm -f flash_driver.bin flash_driver.elf $(OBJECTS)
