/*
 * sffi.c - Special FFI functions
 *
 * Copyright (C) 2009  Carl G. Ritson
 *
 */

#include "tvm-nxt.h"
#include <base/display.h>
#include <base/drivers/i2c_memory.h>
#include <base/drivers/motors.h>
#include <base/drivers/sensors.h>

/* PROC nx.display.clear () */
int _nx_display_clear (ECTX ectx, WORD args[]) {
	nx_display_clear ();
	return SFFI_OK;
}

/* PROC nx.display.cursor.set.pos (VAL INT x, y) */
int _nx_display_cursor_set_pos (ECTX ectx, WORD args[]) {
	nx_display_cursor_set_pos ((U8) args[0], (U8) args[1]);
	return SFFI_OK;
}

/* PROC nx.display.string (VAL []BYTE str) */
int _nx_display_string (ECTX ectx, WORD args[]) {
	BYTEPTR str 	= (BYTEPTR) args[0];
	WORD str_len	= args[1];
	WORD pos 	= 0;
	char buffer[24];

	while (pos < str_len) {
		WORD len;
		for (len = 0; len < 20 && pos < str_len; ++len, ++pos) {
			buffer[len] = read_byte (byteptr_plus (str, pos));
		}
		buffer[len] = '\0';
		nx_display_string (buffer);
	}
	
	return SFFI_OK;
}

/* PROC nx.motors.rotate (VAL INT motor, speed) */
int _nx_motors_rotate (ECTX ectx, WORD args[]) {
	nx_motors_rotate ((U8) args[0], (S8) args[1]);
	return SFFI_OK;
}

/* PROC nx.motors.rotate.angle (VAL INT motor, speed, angle, VAL BOOL brake) */
int _nx_motors_rotate_angle (ECTX ectx, WORD args[]) {
	nx_motors_rotate_angle (
		(U8) args[0], (S8) args[1], 
		(U32) args[2], args[3] ? TRUE : FALSE
	);
	return SFFI_OK;
}

/* PROC nx.motors.rotate.time (VAL INT motor, speed, ms, VAL BOOL brake) */
int _nx_motors_rotate_time (ECTX ectx, WORD args[]) {
	nx_motors_rotate_time (
		(U8) args[0], (S8) args[1], 
		(U32) args[2], args[3] ? TRUE : FALSE
	);
	return SFFI_OK;
}

/* PROC nx.motors.stop (VAL INT motor, VAL BOOL brake) */
int _nx_motors_stop (ECTX ectx, WORD args[]) {
	nx_motors_stop ((U8) args[0], args[1] ? TRUE : FALSE);
	return SFFI_OK;
}

/* PROC nx.motors.get.tach.count (VAL INT motor, RESULT INT count) */
int _nx_motors_get_tach_count (ECTX ectx, WORD args[]) {
	U32 tach = nx_motors_get_tach_count ((U8) args[0]);
	write_word ((WORDPTR) args[1], (WORD) tach);
	return SFFI_OK;
}

/* PROC nx.i2c.memory.init (VAL INT sensor, VAL BYTE address, VAL BOOL lego.compat) */
int _nx_i2c_memory_init (ECTX ectx, WORD args[]) {
	nx_i2c_memory_init ((U32) args[0], (U8) args[1], args[2] ? TRUE : FALSE);
	return SFFI_OK;
}
/* PROC nx.i2c.memory.close (VAL INT sensor) */
int _nx_i2c_memory_close (ECTX ectx, WORD args[]) {
	nx_i2c_memory_close ((U32) args[0]);
	return SFFI_OK;
}
/* PROC nx.i2c.memory.read (VAL INT sensor, VAL BYTE address, []BYTE buffer, RESULT INT result) */
int _nx_i2c_memory_read (ECTX ectx, WORD args[]) {
	S32 result = nx_i2c_memory_read (
		(U32) args[0], (U8) args[1],
		(U8 *) wordptr_real_address ((WORDPTR) args[2]),
		(U32) args[3]
	);
	write_word ((WORDPTR) args[4], (WORD) result);
	return SFFI_OK;
}
/* PROC nx.i2c.memory.write (VAL INT sensor, VAL BYTE address, VAL []BYTE buffer, RESULT INT result) */
int _nx_i2c_memory_write (ECTX ectx, WORD args[]) {
	S32 result = nx_i2c_memory_write (
		(U32) args[0], (U8) args[1],
		(U8 *) wordptr_real_address ((WORDPTR) args[2]),
		(U32) args[3]
	);
	write_word ((WORDPTR) args[4], (WORD) result);
	return SFFI_OK;
}


/* PROC nx.sensors.analog.init (VAL INT sensor) */
int _nx_sensors_analog_init (ECTX ectx, WORD args[])
{
	nx_sensors_analog_enable(args[0]);
	//TODO: Check if these lines are required
        nx_sensors_analog_digi_set(args[0] ,DIGI0);
        nx_sensors_analog_digi_clear(args[0] ,DIGI0);	
	return SFFI_OK;
}


/* PROC nx.sensors.analog.get (VAL INT sensor, RESULT INT result) */
int _nx_sensors_analog_get (ECTX ectx, WORD args[])
{
        S32 result = nx_sensors_analog_get(args[0]);
	write_word ((WORDPTR) args[1], (WORD) result);
        return SFFI_OK;
}

/* PROC nx.display.int () */
int _nx_display_int (ECTX ectx, WORD args[]) {
        nx_display_int (args[0]);
        return SFFI_OK;
}

/* PROC nx.sound () */
int _nx_sound(ECTX ectx, WORD args[])
{
        nx_sound_freq_async(args[0], args[1]);
        return SFFI_OK;
}

/* PROC nx.button () */
int _nx_button(ECTX ectx, WORD args[])
{
        S32 result = nx_avr_get_button();
        write_word ((WORDPTR) args[1], (WORD) result);
        return SFFI_OK;
}



SFFI_FUNCTION sffi_table[] = {
	_nx_display_clear,
	_nx_display_cursor_set_pos,
	_nx_display_string,
	_nx_motors_rotate,
	_nx_motors_rotate_angle,
	_nx_motors_rotate_time,
	_nx_motors_stop,
	_nx_motors_get_tach_count,
	_nx_i2c_memory_init,
	_nx_i2c_memory_close,
	_nx_i2c_memory_read,
	_nx_i2c_memory_write,
	_nx_sensors_analog_init,
	_nx_sensors_analog_get,
	_nx_display_int,
	_nx_sound,
	_nx_button

};
const int sffi_table_length = sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
