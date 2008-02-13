#define BOOT_SECTOR	0x00000000  /* address in SPI flash of boot image */
#define FIRMWARE_SECTOR	0x00020000  /* address in SPI flash of firmware */
#define FLASH_SECTOR	0x00040000  /* address in SPI flash of user flash sector */

/* 0MiB - 32MiB SDRAM */
#define SDRAM_BOTTOM	0x00000000  /* address of the base of SDRAM */
#define SDRAM_TOP	0x02000000  /* address of the top of SDRAM */

/* 1MiB - 7MiB DMA buffers */
#define DMA_BUF1	0x00100000  /* address in SDRAM for DMA transfer of frames from camera */
#define DMA_BUF2	0x00400000  /*   second DMA buffer for double buffering */

/* 7MiB - 8MiB bytecode */
#define USER_BYTECODE	0x00700000  /* address in SDRAM of user bytecode */
/* >8MiB workspace, vectorspace, etc */
#define USER_MEMORY	0x00800000  /* address in SDRAM of user memory */

