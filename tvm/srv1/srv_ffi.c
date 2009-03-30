#include <stdio.h>
#include <cdefBF537.h>
#include <tvm.h>
#include "config.h"
#include "uart.h"

/*********************************************/
/* CONSOLE LOOP                              */
/*********************************************/

/* CASE: I */
void ffi_get_frame(int *w) {
  get_frame();
}

/* CASE: o */
void ffi_overlay_on (int *w) {
  overlay_on();
}

/* CASE: O */
void ffi_overlay_off (int *w) {
  overlay_off();
}

/* CASE: a, b, c, A or 160, 320, 640, 1280 */
void ffi_camera_reset (int *w) { /* VAL INT resolution */
  camera_reset(w[0]);
}

/* CASE: V */
void ffi_serial_out_version (int *w) {
  serial_out_version();
}

/* CASE: q */
void ffi_change_image_quality (int *w) {
	change_image_quality();
}

/* CASE: X */
void ffi_xmodem_receive (int *w) { /* INT ret */
  *((WORD *)w[0]) = xmodem_receive();
}

/* CASE S-d */
void ffi_serial_out_flashbuffer (int *w) {
	serial_out_flashbuffer();
}

/***
 * 20080101 MCJ
 * Skipping C interpreter functions
 */

/* CASE: f */
void ffi_serial_out_framecount (int *w) {
  serial_out_framecount();
}

/* CASE: t */
void ffi_serial_out_time (int *w) {
  serial_out_time();
}

/***
 * 20080101 MCJ
 * Skipping FLASH manipulation routines for now.
 */

/* CASE: M */
void ffi_motor_command (int *w) {
  motor_command();
}

/* CASE: + */
void ffi_motor_increase_base_speed (int *w) {
  motor_increase_base_speed();
}

/* CASE: - */
void ffi_motor_decrease_base_speed (int *w) {
  motor_decrease_base_speed();
}

/* CASE: . 0 3 2 1 6 5 4 9 8 7 */
void ffi_motor_action (int *w) { /* VAL BYTE char */
  motor_action(w[0]);
}

/* CASE: l */
void ffi_lasers_on (int *w) {
	lasers_on();
}

/* CASE: L */
void ffi_lasers_off (int *w) {
	lasers_off();
}

/*********************************************/
/* UART INTERACTION                          */
/*********************************************/
#define BYTE(w) ((char *)(w)) 
#define BOOL(w) ((char *)(w)) 
#define INT(w) ((int *)(w)) 

void ffi_uart0_char(int *w) { /* RESULT INT ready, RESULT BYTE ch */
  int ready;
  unsigned char ch;

  if (!(*pUART0_LSR & DR)) {
    ready = 0;
    ch = 0;
  } else {
    ready = 1;
    ch = *pUART0_RBR;
  }

  *INT(w[0]) = ready;
  *BYTE(w[1]) = ch;

}

void ffi_print_char (int *w) { /* VAL BYTE ch */
  uart0SendChar(w[0]);
}

int puts (const char* s) {
	int index = 0;
	
	while (s[index] != '\0') {
		uart0SendChar(s[index]);
		index++;
	}
}

/*******************************************/
/* Old stuff to be revisited ...           */
/*******************************************/

/* LED's */
void ffi_led0_on (int *w) {
	led0_on();
}

void ffi_led1_on (int *w) {
	led1_on();
}

void ffi_set_caption (int *w) {
}

void ffi_move_image (int *w) {
	// void move_image(unsigned char *src1, unsigned char *src2, unsigned char *dsrt, unsigned int width, unsigned int height)
}

void ffi_start_cinterpreter (int *w) {
	start_cinterpreter();
}

void ffi_read_user_flash (int *w) {
	read_user_flash();
}

void ffi_write_user_flash (int *w) {
	write_user_flash();
}

void ffi_write_boot_flash (int *w) {
	write_boot_flash();
}

void ffi_setPWM (int *w) { /* VAL INT left, right */
  int left = w[0];
  int right = w[1];

  setPWM(left, right);
}

