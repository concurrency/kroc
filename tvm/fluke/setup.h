/* Borrowed from the Fluke firmware while testing. */

void led_on();
void led_off();

void setup_pins();

void setup_pll();

void setup_mam();

/* Constants for setup_pll */
#define FOSC            20000000
#define PLL_M		3
#define MSEL		(PLL_M-1)
#define PSEL 		0x01          
#define PLLE		0
#define PLLC		1
#define PLOCK		10
#define PLL_FEED1	0xAA
#define PLL_FEED2	0x55
#define VPBDIV_VAL	1

/* Constants for setup_mam */
#define MEMMAP_BOOT_LOADER_MODE   0      
#define MEMMAP_USER_FLASH_MODE    (1<<0)  
#define MEMMAP_USER_RAM_MODE      (1<<1)  

/* These constants are for a version 4 board. */
#define S_TXD       (1 << 0)
#define S_RXD       (1 << 1)
#define EXT_I2C_SC  (1 << 2)
#define EXT_I2C_SD  (1 << 3)
#define MCLK        (1 << 4)
#define MDOUT       (1 << 5)
#define MDIN        (1 << 6)
#define CAM_SC      (1 << 7)
#define B_TXD       (1 << 8)
#define B_RXD       (1 << 9)
#define IRIN        (1 << 10)
#define B_CTS       (1 << 11)
#define S_RST       (1 << 12)
#define IROUT1      (1 << 13)
#define BOOT        (1 << 14)
#define CAM_VSYNC   (1 << 15)

#define CAM_PCLK    (1 << 16)
#define LED         (1 << 17)
#define B_RTS       (1 << 18)
#define IROUT2      (1 << 19)
#define IROUT3      (1 << 20)
#define D2A_CS      (1 << 21)
#define A2D_CS      (1 << 22)
#define MCS         (1 << 23)
#define CAM_D0      (1 << 24)
#define CAM_D1      (1 << 25)
#define CAM_D2      (1 << 26)
#define CAM_D3      (1 << 27)
#define CAM_D4      (1 << 28)
#define CAM_D5      (1 << 29)
#define CAM_D6      (1 << 30)
#define CAM_D7      (1 << 31)
