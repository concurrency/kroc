#define BOOT_SECTOR	0x00000000  /* address in SPI flash of boot image */
#define FIRMWARE_SECTOR	0x00020000  /* address in SPI flash of firmware */
#define FLASH_SECTOR	0x00040000  /* address in SPI flash of user flash sector */

/* 0MiB - 32MiB SDRAM */
#define SDRAM_BOTTOM	0x00000000  /* address of the base of SDRAM */
#define SDRAM_TOP	0x02000000  /* address of the top of SDRAM */

/* >= 128 KiB dynamic memory */
#define DMEM_START	0x00020000  /* address in SDRAM of dynamic memory */

#define INVALID_ADDRESS	0xF0000000  /* reserved address to provoke errors */

