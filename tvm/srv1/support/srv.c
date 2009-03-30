/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  srv.c - routines to interface with the SRV-1 Blackfin robot.
 *    modified from main.c - main control loop for SRV-1 robot
 *    Copyright (C) 2005-2007  Surveyor Corporation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details (www.gnu.org/licenses)
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <cdefBF537.h>
#include "bfin_config.h"
#include "uart.h"
#include "i2cwrite.h"
#include "ov9655.h"
#include "camera.h"
#include "jpeg.h"
#include "xmodem.h"
#include "stm_m25p32.h"
#include "c.h"
#include "font8x8.h"

#include "srv.h"
#include "memory_map.h"

#define QUIET(x) 0;

#define SSYNC asm("ssync;")
#define TRUE 1
#define FALSE 0

#define FOUR_TWO_ZERO 1
#define FOUR_TWO_TWO  2

/* Size of frame */
unsigned int imgWidth, imgHeight;

/* Version */
unsigned char version_string[] = "TVM SRV-1 Blackfin w/C interpreter - "  __TIME__ " - " __DATE__ ;

/* Guess: Frame count output string */
unsigned char frame[] = "frame     ";

/* Camera globals */
unsigned int quality, framecount, ix, overlay_flag;
unsigned char *output_start, *output_end; /* Framebuffer addresses */
unsigned int image_size; /* Framebuffer size */
char imgHead[7]; /* Guess: JPEG header */

/* Motor globals */
int lspeed, rspeed, base_speed, err;

/* General globals */
unsigned char *cp;
unsigned int i, j; // Loop counter.

void init_io() {
  *pPORTGIO_DIR = 0x0300;   // LEDs (PG8 and PG9)
  *pPORTH_FER = 0x0000;     // set for GPIO
  *pPORTHIO_DIR |= 0x0040;  // set PORTH6 to output for serial flow control
  *pPORTHIO = 0x0000;       // set output low 
  *pPORTHIO_INEN = 0x0001;
  *pPORTHIO_DIR |= 0x0380;  // set up lasers
}

/* clear SDRAM */
void clear_sdram() {
  for (cp=(unsigned char *)0x00000000; cp<(unsigned char *)0x02000000; cp++) {
    *cp = 0;
  }
}

/* SRV-1 Firmware Version Request
   Serial protocol char: V */
void serial_out_version () {
  uart0SendString((unsigned char *)"Version - ");
  uart0SendString((unsigned char *)version_string);
  uart0SendChar('\n');
}

/* Get frame count.
   Serial protocol char: f */
void serial_out_framecount () {
  uart0SendString((unsigned char *)"framecount:  ");
  printNumber(10, 8, 0, ' ', framecount);
  uart0SendChar('\n');
}

/* Get current time
   Serial protocol char: t */
void serial_out_time () {
  uart0SendString((unsigned char *)"time[ms]:  ");
  printNumber(10, 10, 0, ' ', readRTC());
  uart0SendChar('\n');
}

/* Dump flash buffer to serial
   Serial protocol char: z-d */
void serial_out_flashbuffer () {
  uart0SendString((unsigned char *)"##zdump: \n");
  cp = (unsigned char *)FLASH_BUFFER;
  for (i=0; i < FLASH_BUFFER_EXTENT ; i++) {
      while (*pPORTHIO & 0x0001)
          continue;
      if (*cp == 0)
          return;
      uart0SendChar(*cp++);
  }
}

/* Turn lasers on
   Serial protocol char: l */
void lasers_on () {
  *pPORTHIO |= 0x0380;
  QUIET(uart0SendString("#l"));
}

/* Turn lasers off
   Serial protocol char: L */
void lasers_off () {
  *pPORTHIO &= 0xFC7F;
  QUIET(uart0SendString("#L"));
}

void led0_on() {
	*pPORTGIO = 0x0100;  // turn on LED0
}

void led1_on() {
	*pPORTGIO = 0x0200;  // turn on LED1
}

/* Get a frame from the camera.
   Serial protocol char: I */
void get_frame () {
	unsigned char ch;
	move_image((unsigned char *)DMA_BUF1, (unsigned char *)DMA_BUF2, (unsigned char *)FRAME_BUF, imgWidth, imgHeight); 
	if (overlay_flag) {
		frame[9] = (framecount % 10) + 0x30;
		frame[8] = ((framecount/10)% 10) + 0x30;
		frame[7] = ((framecount/100)% 10) + 0x30;
		set_caption(frame, imgWidth);
	}
	output_start = (unsigned char *)JPEG_BUF;
	output_end = encode_image((unsigned char *)FRAME_BUF, 
	output_start, quality, FOUR_TWO_TWO, imgWidth, imgHeight); 
	image_size = (unsigned int)(output_end - output_start);

	led1_on();

	framecount++;
	uart0SendString(imgHead);
	uart0SendChar((unsigned char)(image_size & 0x000000FF));
	uart0SendChar((unsigned char)((image_size & 0x0000FF00) >> 8));
	uart0SendChar((unsigned char)((image_size & 0x00FF0000) >> 16));
	uart0SendChar(0x00);
	cp = (unsigned char *)JPEG_BUF;
	for (i=0; i<image_size; i++) {
		while (*pPORTHIO & 0x0001) {
			continue;
		}
		uart0SendChar(*cp++);
	}

	while (uart0GetChar(&ch)) {
		// flush input 
		continue;
	}
}

/* Turn image overlay on.
   Serial protocol char: o */
void overlay_on () {
  overlay_flag = 1;
  QUIET(uart0SendString("#o"));
}


/* Turn image overlay off.
   Serial protocol char: O */
void overlay_off () {
  overlay_flag = 0;
  QUIET(uart0SendString("#O"));
}

/* Camera initial setup */
void camera_setup () {
  imgWidth = 320;
  imgHeight = 256;
  strcpy1(imgHead, "##IMJ5");
  i2cwrite(0x30, ov9655_setup, sizeof(ov9655_setup)>>1);
  i2cwrite(0x30, ov9655_qvga, sizeof(ov9655_qvga)>>1);
  camera_init((unsigned char *)DMA_BUF1, (unsigned char *)DMA_BUF2, imgWidth, imgHeight);
  camera_start();

	/* Initialise camera-related globals */
	framecount = 0;
	overlay_flag = 1;
	quality = 3; // Default JPEG quality.
}

/* Refactored out, code to reset the camera after a frame size change. */
void camera_reset (unsigned int width) {
	imgWidth = width;
	if (width == 160) {
		imgHeight = 128;
		strcpy1(imgHead, "##IMJ3");
		camera_stop();
		i2cwrite(0x30, ov9655_qqvga, sizeof(ov9655_qqvga)>>1);
	} else if (width == 320) {
		imgHeight = 256;
		strcpy1(imgHead, "##IMJ5");
		camera_stop();
		i2cwrite(0x30, ov9655_qvga, sizeof(ov9655_qvga)>>1);
	} else if (width == 640) {
		imgHeight = 512;
		strcpy1(imgHead, "##IMJ7");
		camera_stop();
		i2cwrite(0x30, ov9655_vga, sizeof(ov9655_vga)>>1);
	} else if (width == 1280) {
		imgHeight = 1024;
		strcpy1(imgHead, "##IMJ9");
		camera_stop();
		i2cwrite(0x30, ov9655_sxga, sizeof(ov9655_sxga)>>1);
	}
	camera_init((unsigned char *)DMA_BUF1, (unsigned char *)DMA_BUF2, imgWidth, imgHeight);
	camera_start();
}

/* Set camera frame size to 160x128
   Serial protocol char: a */
void framesize_160 () {
	camera_reset(160);
	QUIET(uart0SendString("#a"));
}

/* Set camera frame size to 320x256
   Serial protocol char: b */
void framesize_320 () {
	camera_reset(320);
	QUIET(uart0SendString("#b"));
}

/* Set camera frame size to 640x512
   Serial protocol char: c */
void framesize_640 () {
	camera_reset(640);
	QUIET(uart0SendString("#c"));
}

/* Set camera frame size to 1280x1024
   Serial protocol char: A */
void framesize_1280 () {
	camera_reset(1280);
	QUIET(uart0SendString("#A"));
}

/* Change image quality.
   Serial protocol char: q */
void change_image_quality () {
	unsigned char ch;
	ch = uart0GetCh();
	quality = (unsigned int)(ch & 0x0f);
	if (quality < 1) {
		quality = 1;
	} else if (quality > 8) {
		quality = 8;
	}
	QUIET(uart0SendString((unsigned char *)"##quality - "));
	QUIET(uart0SendChar(ch));
	QUIET(uart0SendChar('\n'));
}

// write caption string of up to 40 characters to frame buffer 
void set_caption(unsigned char *str, unsigned int width) {
    unsigned char *fbuf, *fcur, *str1, cc;
    int offset, scale, len, ix, iy, iz, w2;
    
    w2 = width * 2;
    str1 = str;
    
    for (len=0; len<40 && *str1++; len++);          // find string length
    fbuf = FRAME_BUF + (unsigned char *)((width * 17) - (len * 8));  // point to 1st char
    
    for (ix=0; ix<len; ix++) {
        fcur = fbuf;
        for (iy=0; iy< 8; iy++) {
            cc = font8x8[str[ix]*8 + iy];
            for (iz=0; iz<8; iz++) {
                if (cc & fontmask[iz]) {
                    fcur[0] = 0x80;
                    fcur[1] = 0xff;
                }
                fcur += 2;
            }
            fcur += (width * 2) - 16;          // move to next line
        }    
        fbuf += 16;  // move to next char position
    }
}


void move_image(unsigned char *src1, unsigned char *src2, unsigned char *dst, unsigned int width, unsigned int height) {

    unsigned char *src;
    unsigned short *isrc, *idst;
    unsigned int ix;
        
    if (*pDMA0_CURR_ADDR < (void *)src2)
        src = src2;
    else
        src = src1;
    
    isrc = (unsigned short *)src;
    idst = (unsigned short *)dst;
    for (ix = 0; ix < (width * height); ix++)
        *idst++ = *isrc++;
    return;
}


/* XModem Receive.
   Serial protocol char: X */
int xmodem_receive () {
  int len = xmodemReceive((char *)FLASH_BUFFER, FLASH_BUFFER_EXTENT);;
  /* err = xmodemReceive((char *)FLASH_BUFFER, 131072); */
  if (len < 0) {
    uart0SendString((unsigned char *)"## Xmodem receive error:  ");
      printNumber(10, 8, 0, ' ', -len);
  } else {
  	uart0SendString((unsigned char *)"## Xmodem success. Count:  ");
      printNumber(10, 8, 0, ' ', len);
  }
  uart0SendChar('\n');
  return len;
}

/* Execute C program from flash buffer
   Serial protocol char: Q */
void start_cinterpreter () {
  c((unsigned char *)FLASH_BUFFER);
}

/* Read user flash sector into flash buffer
   Serial protocol char: z-r */
void read_user_flash () {
	for (ix = FLASH_BUFFER; ix < (FLASH_BUFFER  + 0x00010000); ix++)
      *((unsigned char *)ix) = 0;   // clear the read buffer
  ix = spi_read(FLASH_SECTOR, (unsigned char *)FLASH_BUFFER, 0x00010000);
  uart0SendString((unsigned char *)"##zread count:  ");
  printNumber(10, 8, 0, ' ', ix);
  uart0SendChar('\n');
}

/* Write user flash sector from flash buffer
   Serial protocol char: z-w */
void write_user_flash () {
  ix = spi_write(FLASH_SECTOR, (unsigned char *)FLASH_BUFFER, 
      (unsigned char *)(FLASH_BUFFER + 0x00010000), 0x00010000);
  uart0SendString((unsigned char *)"##zwrite count:  ");
  printNumber(10, 8, 0, ' ', ix);
  uart0SendChar('\n');
}

/* Write firmware flash sectors (2-3) from flash buffer
   Serial protocol char: z-z */
void write_firmware_flash () {
	cp = (unsigned char *)FLASH_BUFFER;
	if (cp[0] != 0x7F && cp[1] != 0x45 && cp[2] != 0x4C && cp[3] != 0x46) {
		uart0SendString((unsigned char *)"##zz firmware abort - invalid header\n");
		return;
	}
  ix = spi_write(FIRMWARE_SECTOR, (unsigned char *)FLASH_BUFFER, 
      (unsigned char *)(FLASH_BUFFER + 0x00020000), 0x00020000);
  uart0SendString((unsigned char *)"##zz firmware write count:  ");
  printNumber(10, 8, 0, ' ', ix);
  uart0SendChar('\n');
}

/* Write boot flash sectors (1-2) from flash buffer
   Serial protocol char: z-Z */
void write_boot_flash () {
  cp = (unsigned char *)FLASH_BUFFER;
  if (cp[1] != 0x00 && cp[2] != 0x80 && cp[3] != 0xFF) {
      uart0SendString((unsigned char *)"##zZ boot image - invalid header\n");
      return;
  }                        
  ix = spi_write(BOOT_SECTOR, (unsigned char *)FLASH_BUFFER, 
      (unsigned char *)(FLASH_BUFFER + 0x00020000), 0x00020000);
  uart0SendString((unsigned char *)"##zZ boot image write count:  ");
  printNumber(10, 8, 0, ' ', ix);
  uart0SendChar('\n');
}

/* Motor global initialisation */
void init_motors() {
	lspeed = rspeed = 0;
	base_speed = 50;
}

/* Motor command, three character command string follows.
   Serial protocol char: M */
void motor_command() {
	unsigned int mdelay;
	lspeed = (int)((signed char)uart0GetCh());
	rspeed = (int)((signed char)uart0GetCh());
	mdelay = (unsigned int)uart0GetCh();
	setPWM(lspeed, rspeed);
	if (mdelay) {
		delayMS(mdelay * 10);
		setPWM(0, 0);
		lspeed = 0;
		rspeed = 0;
	}
	QUIET(uart0SendString((unsigned char *)"#M"));
}

/* Increase motor base speed
   Serial protocol char: + */
void motor_increase_base_speed() {
  base_speed += 5;
  if (base_speed > 95) {
      base_speed = 95;
	}
  QUIET(uart0SendString((unsigned char *)"#+"));
}

/* Decrease motor base speed
   Serial protocol char: - */
void motor_decrease_base_speed() {
  base_speed -= 5;
  if (base_speed < 35) {
      base_speed = 35;
  }
  QUIET(uart0SendString((unsigned char *)"#-"));
}

/* Take motor action */
void motor_action(unsigned char ch) {
  motor_set(ch, base_speed, &lspeed, &rspeed);
  QUIET(uart0SendChar('#'));
  QUIET(uart0SendChar(ch));
}

/* General motor control code */
void motor_set(unsigned char cc, int speed, int *ls, int *rs)  {
    int left_speed, right_speed;

    left_speed = right_speed = 0;
    switch (cc) {
        case '7':     // drift left
            left_speed = speed-15;
            right_speed = speed+15;
            break;
        case '8':     // forward
            left_speed = speed; 
            right_speed = speed;
            break;
        case '9':     // drift right
            left_speed = speed+15;
            right_speed = speed-15;
            break;
        case '4':     // turn left
            left_speed = speed-30;
            right_speed = speed+30;
            break;
        case '5':        // stop
            left_speed = 0;
            right_speed = 0;
            break;
        case '6':     // turn right
            left_speed = speed+30;
            right_speed = speed-30;
            break;
        case '1':     // back left
            left_speed = -(speed-30);
            right_speed = -(speed+30);
            break;
        case '2':     // back
            left_speed = -speed;
            right_speed = -speed;
            break;
        case '3':     // back right
            left_speed = -(speed+30);
            right_speed = -(speed-30);
            break;
        case '.':     // clockwise turn
            setPWM(-speed, speed);
            delayMS(200);
            setPWM(0, 0);
            left_speed = 0;
            right_speed = 0;
            break;
        case '0':     // counter clockwise turn
            setPWM(speed, -speed);
            delayMS(200);
            setPWM(0, 0);
            left_speed = 0;
            right_speed = 0;
            break;
    }
    setPWM(left_speed, right_speed);
    *ls = left_speed;
    *rs = right_speed;
    return;
}

void initPWM() {
    // configure timers 2 and 3
    *pPORT_MUX = 0;
    *pPORTF_FER |= 0x00C0;  // configure PF6 and PF7 as timers
    *pTIMER2_CONFIG = PULSE_HI | PWM_OUT | PERIOD_CNT;
    *pTIMER3_CONFIG = PULSE_HI | PWM_OUT | PERIOD_CNT;
    *pTIMER2_PERIOD = PERIPHERAL_CLOCK / 1000;                // 1000Hz
    *pTIMER3_PERIOD = PERIPHERAL_CLOCK / 1000;                // 1000Hz
    *pTIMER2_WIDTH = ((PERIPHERAL_CLOCK / 1000) * 1) / 100; 
    *pTIMER3_WIDTH = ((PERIPHERAL_CLOCK / 1000) * 1) / 100; 
    *pTIMER_ENABLE = TIMEN2 | TIMEN3;
    *pPORTHIO_DIR |= 0x0030;  // set PORTH4 and PORTH5 to output for direction control
    *pPORTHIO &= 0xFFCF;      // set output low 
    //*pPORTHIO |= 0x0030;  
}

void setPWM (int mleft, int mright) {
    if (mleft < 0) {
        *pPORTHIO = (*pPORTHIO & 0xFFDF);  // clear left direction bit
        mleft = -mleft;
    } else {
        *pPORTHIO = (*pPORTHIO & 0xFFDF) | 0x0020;  // turn on left direction bit
    }
    if (mleft > 100)
        mleft = 100;
    if (mleft < 1)
        mleft = 1;

    if (mright < 0) {
        *pPORTHIO = (*pPORTHIO & 0xFFEF);  // clear right direction bit
        mright = -mright;
    } else {
        *pPORTHIO = (*pPORTHIO & 0xFFEF) | 0x0010;  // turn on right direction bit
    }
    if (mright > 100)
        mright = 100;
    if (mright < 1)
        mright = 1;

    *pTIMER2_WIDTH = ((PERIPHERAL_CLOCK / 1000) * mleft) / 100;
    *pTIMER3_WIDTH = ((PERIPHERAL_CLOCK / 1000) * mright) / 100;
}

/* Initialise the Real-time Clock */
void initRTC() {
    *pRTC_ICTL = 0;  // disable interrupts
    SSYNC;
    *pRTC_PREN = 0;  // disable prescaler - clock counts at 32768 Hz
    SSYNC;
    *pRTC_STAT = 0;  // clear counter
    SSYNC;
}

/* Read the RTC counter, returns number of milliseconds since reset */
int readRTC() {     
    int i1, i2;
    i1 = *pRTC_STAT;
    i2 = (i1 & 0x0000003F) + (((i1 >> 6) & 0x0000003F) * 60) +  
        (((i1 >> 12) & 0x0000001F) * 3600) + (((i1 >> 17) & 0x00007FFF) * 86400);
    return (i2 / 33);  // converts tick count to milliseconds
                       //    32,768 / 32.77 = 1,000
}

/* Clear the RTC counter value */
void clearRTC() {
  *pRTC_STAT = 0;
  SSYNC;
}

void delayMS(int delay) {  // delay up to 100000 millisecs (100 secs)
    int i0;

    if ((delay < 0) || (delay > 100000))
        return;
    i0 = readRTC();
    while (readRTC() < (i0 + delay))
        continue;
}

