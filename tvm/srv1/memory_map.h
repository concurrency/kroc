#define FLASH_BUFFER 0x00100000  // address in SDRAM for buffering flash and xmodem 
#define BOOT_SECTOR 0x00000000   // address in SPI flash of boot image 
#define FIRMWARE_SECTOR 0x00020000  // address in SPI flash of firmware 
#define FLASH_SECTOR 0x00040000  // address in SPI flash of user flash sector  
#define DMA_BUF1     0x01000000  // address in SDRAM for DMA transfer of frames from camera
#define DMA_BUF2     0x01300000  //   second DMA buffer for double buffering
#define FRAME_BUF    0x01800000  // address in SDRAM for staging images for processing/jpeg
#define JPEG_BUF     0x00800000  // address in SDRAM for compressed image

#define FLASH_BUFFER_EXTENT  0xDECAF /* 912,559 bytes */

