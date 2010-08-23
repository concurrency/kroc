/*
 * sffi.c - Special FFI functions
 *
 * Copyright (C) 2009  Carl G. Ritson
 *
 */

#include "tvm-nxt.h"
//#include <base/at91sam7s256.h>
//#include <base/display.h>
//#include <base/drivers/avr.h>
//#include <base/drivers/bt.h>
//#include <base/drivers/i2c_memory.h>
//#include <base/drivers/motors.h>
//#include <base/drivers/sensors.h>
//#include <base/drivers/sound.h>

#ifdef UNUSED 
#elif defined(__GNUC__) 
#define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#else
#define UNUSED(x) x 
#endif

#if 0

/* PROC nx.display.clear () */
int _nx_display_clear (ECTX UNUSED(ectx), WORD UNUSED(args[])) {
	nx_display_clear ();
	return SFFI_OK;
}

/* PROC nx.display.cursor.set.pos (VAL INT x, y) */
int _nx_display_cursor_set_pos (ECTX UNUSED(ectx), WORD args[]) {
	nx_display_cursor_set_pos ((U8) args[0], (U8) args[1]);
	return SFFI_OK;
}

/* PROC nx.display.string (VAL []BYTE str) */
int _nx_display_string (ECTX UNUSED(ectx), WORD args[]) {
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
int _nx_motors_rotate (ECTX UNUSED(ectx), WORD args[]) {
	nx_motors_rotate ((U8) args[0], (S8) args[1]);
	return SFFI_OK;
}

/* PROC nx.motors.rotate.angle (VAL INT motor, speed, angle, VAL BOOL brake) */
int _nx_motors_rotate_angle (ECTX UNUSED(ectx), WORD args[]) {
	nx_motors_rotate_angle (
		(U8) args[0], (S8) args[1], 
		(U32) args[2], args[3] ? TRUE : FALSE
	);
	return SFFI_OK;
}

/* PROC nx.motors.rotate.time (VAL INT motor, speed, ms, VAL BOOL brake) */
int _nx_motors_rotate_time (ECTX UNUSED(ectx), WORD args[]) {
	nx_motors_rotate_time (
		(U8) args[0], (S8) args[1], 
		(U32) args[2], args[3] ? TRUE : FALSE
	);
	return SFFI_OK;
}

/* PROC nx.motors.stop (VAL INT motor, VAL BOOL brake) */
int _nx_motors_stop (ECTX UNUSED(ectx), WORD args[]) {
	nx_motors_stop ((U8) args[0], args[1] ? TRUE : FALSE);
	return SFFI_OK;
}

/* PROC nx.motors.get.tach.count (VAL INT motor, RESULT INT count) */
int _nx_motors_get_tach_count (ECTX UNUSED(ectx), WORD args[]) {
	U32 tach = nx_motors_get_tach_count ((U8) args[0]);
	write_word ((WORDPTR) args[1], (WORD) tach);
	return SFFI_OK;
}

/* PROC nx.i2c.memory.init (VAL INT sensor, VAL BYTE address, VAL BOOL lego.compat) */
int _nx_i2c_memory_init (ECTX UNUSED(ectx), WORD args[]) {
	nx_i2c_memory_init ((U32) args[0], (U8) args[1], args[2] ? TRUE : FALSE);
	return SFFI_OK;
}
/* PROC nx.i2c.memory.close (VAL INT sensor) */
int _nx_i2c_memory_close (ECTX UNUSED(ectx), WORD args[]) {
	nx_i2c_memory_close ((U32) args[0]);
	return SFFI_OK;
}
/* PROC nx.i2c.memory.read (VAL INT sensor, VAL BYTE address, []BYTE buffer, RESULT INT result) */
int _nx_i2c_memory_read (ECTX UNUSED(ectx), WORD args[]) {
	S32 result = nx_i2c_memory_read (
		(U32) args[0], (U8) args[1],
		(U8 *) wordptr_real_address ((WORDPTR) args[2]),
		(U32) args[3]
	);
	write_word ((WORDPTR) args[4], (WORD) result);
	return SFFI_OK;
}
/* PROC nx.i2c.memory.write (VAL INT sensor, VAL BYTE address, VAL []BYTE buffer, RESULT INT result) */
int _nx_i2c_memory_write (ECTX UNUSED(ectx), WORD args[]) {
	S32 result = nx_i2c_memory_write (
		(U32) args[0], (U8) args[1],
		(U8 *) wordptr_real_address ((WORDPTR) args[2]),
		(U32) args[3]
	);
	write_word ((WORDPTR) args[4], (WORD) result);
	return SFFI_OK;
}


/* PROC nx.sensors.analog.init (VAL INT sensor) */
int _nx_sensors_analog_init (ECTX UNUSED(ectx), WORD args[])
{
	nx_sensors_analog_enable (args[0]);
	//TODO: Check if these lines are required
        nx_sensors_analog_digi_set (args[0], DIGI0);
        nx_sensors_analog_digi_clear (args[0], DIGI0);	
	return SFFI_OK;
}


/* PROC nx.sensors.analog.get (VAL INT sensor, RESULT INT result) */
int _nx_sensors_analog_get (ECTX UNUSED(ectx), WORD args[])
{
        S32 result = nx_sensors_analog_get (args[0]);
	write_word ((WORDPTR) args[1], (WORD) result);
        return SFFI_OK;
}

/* PROC nx.display.int (VAL INT display) */
int _nx_display_int (ECTX UNUSED(ectx), WORD args[]) {
        nx_display_int (args[0]);
        return SFFI_OK;
}

/* PROC nx.sound (VAL INT freq, VAL INT len) */
int _nx_sound (ECTX UNUSED(ectx), WORD args[])
{
        nx_sound_freq_async (args[0], args[1]);
        return SFFI_OK;
}

/* PROC nx.button (RESULT INT button) */
int _nx_button (ECTX UNUSED(ectx), WORD args[])
{
        S32 result = nx_avr_get_button ();
        write_word ((WORDPTR) args[0], (WORD) result);
        return SFFI_OK;
}

/* PROC nx.bt.init () */
int _nx_bt_init (ECTX UNUSED(ectx), WORD UNUSED(args[]))
{	
	nx_bt_reset ();

	#if 0
	{
		char buffer[20];
		int i;

		nx_display_clear ();
		nx_display_string ("bt_init");
		nx_display_end_line ();
		
		if (nx_bt_get_local_addr ((U8*) buffer)) {
			for (i = 0; i < BT_ADDR_SIZE; ++i) {
				nx_display_hex ((U8) buffer[i]);
				nx_display_string (":");
			}
		} else {
			nx_display_string ("local addr: FAIL");
		}
		nx_display_end_line ();
		
		nx_bt_get_friendly_name (buffer);
		nx_display_string (buffer);
		nx_display_end_line ();

		nx_bt_begin_known_devices_dumping ();
		while (nx_bt_get_state() == BT_STATE_KNOWN_DEVICES_DUMPING) {
			if (nx_bt_has_known_device ()) {
				bt_device_t dev;
				nx_bt_get_known_device (&dev);
				for (i = 0; i < BT_ADDR_SIZE; ++i) {
					nx_display_hex ((U8) dev.addr[i]);
					nx_display_string (":");
				}
				nx_display_end_line ();
			}
		}
		
		nx_bt_get_version ();
		
		if (nx_bt_get_brick_status_byte ((U8 *) buffer)) {
			nx_display_string ("status: ");
			nx_display_hex ((U8) buffer[0]);
			nx_display_string (" ");
			nx_display_hex ((U8) buffer[1]);
		} else {
			nx_display_string ("status: FAILED");
		}
		nx_display_end_line ();

		if (nx_bt_set_brick_status_byte (1, 0)) {
			nx_display_string ("set status: OK");
		} else {
			nx_display_string ("set status: FAIL");
		}	
		nx_display_end_line ();
		
		if (nx_bt_get_discoverable ()) {
			nx_display_string ("discoverable");
		} else {
			nx_display_string ("hidden");
		}
		nx_display_end_line ();
		nx_systick_wait_ms (10000);
	}
	#endif

        return SFFI_OK;
}

/* PROC nx.bt.set.discoverable (VAL BOOL set) */
int _nx_bt_set_discoverable (ECTX UNUSED(ectx), WORD args[])
{
        nx_bt_set_discoverable (args[0]);
        return SFFI_OK;
}


/* PROC nx.bt.open.port (RESULT INT handle) */
int _nx_bt_open_port (ECTX UNUSED(ectx), WORD args[])
{
        S32 result = nx_bt_open_port ();
        write_word ((WORDPTR) args[0], (WORD) result);
        return SFFI_OK;
}

/* PROC nx.bt.close.port (VAL INT handle, RESULT INT return) */
int _nx_bt_close_port (ECTX UNUSED(ectx), WORD args[])
{
        S32 result = nx_bt_close_port (args[0]);
        write_word ((WORDPTR) args[1], (WORD) result);
        return SFFI_OK;
}

/* PROC nx.bt.connection.pending (RESULT INT return) */
int _nx_bt_connection_pending (ECTX UNUSED(ectx), WORD args[])
{
        S32 result = nx_bt_connection_pending ();
        write_word ((WORDPTR) args[0], (WORD) result);
        return SFFI_OK;
}


/* PROC nx.bt.accept.connection (VAL BOOL accept) */
int _nx_bt_accept_connection (ECTX UNUSED(ectx), WORD args[])
{       
        nx_bt_accept_connection (args[0] ? TRUE : FALSE);
        return SFFI_OK;
}


/* PROC nx.bt.connection.established (RETURN INT handle) */
int _nx_bt_connection_established (ECTX UNUSED(ectx), WORD args[])
{       
        S32 result = nx_bt_connection_established ();
        write_word ((WORDPTR) args[0], (WORD) result);
        return SFFI_OK;
}


/* PROC nx.bt.stream.open (VAL INT handle) */
int _nx_bt_stream_open (ECTX UNUSED(ectx), WORD args[])
{
        nx_bt_stream_open (args[0]);
        return SFFI_OK;
}

/* PROC nx.bt.stream.write (VAL []BYTE data) */
int _nx_bt_stream_write (ECTX UNUSED(ectx), WORD args[])
{
        nx_bt_stream_write (
		(U8 *) wordptr_real_address ((WORDPTR) args[0]),
		args[1]
	);
        return SFFI_OK;
}

/* PROC nx.bt.stream.data.written (RESULT BOOL written) */
int _nx_bt_stream_data_written (ECTX UNUSED(ectx), WORD args[])
{
        S32 result = nx_bt_stream_data_written ();
        write_word ((WORDPTR) args[0], (WORD) result);
        return SFFI_OK;
}

/* PROC nx.bt.stream.opened (RESULT BOOL open) */
int _nx_bt_stream_opened (ECTX UNUSED(ectx), WORD args[])
{
        S32 result = nx_bt_stream_opened ();
        write_word ((WORDPTR) args[0], (WORD) result);
        return SFFI_OK;
}

/* PROC nx.bt.stream.read ([]BYTE data) */
int _nx_bt_stream_read (ECTX UNUSED(ectx), WORD args[])
{
        nx_bt_stream_read (
		(U8 *) wordptr_real_address ((WORDPTR) args[0]),
		args[1]
	);
        return SFFI_OK;
}

/* PROC nx.bt.stream.data.read (RESULT INT len) */
int _nx_bt_stream_data_read (ECTX UNUSED(ectx), WORD args[])
{       
        S32 result = nx_bt_stream_data_read ();
        write_word ((WORDPTR) args[0], (WORD) result);
        return SFFI_OK;
}

/* PROC nx.bt.stream.close () */
int _nx_bt_stream_close (ECTX UNUSED(ectx), WORD UNUSED(args[]))
{       
        nx_bt_stream_close ();
        return SFFI_OK;
}

/* PROC nx.bt.set.friendly.name (VAL []BYTE name) */
int _nx_bt_set_friendly_name (ECTX UNUSED(ectx), WORD args[])
{
	BYTEPTR str 	= (BYTEPTR) args[0];
	WORD str_len	= args[1];
	WORD pos;
	char buffer[BT_NAME_MAX_LNG + 1];

	for (pos = 0; pos < str_len && pos < BT_NAME_MAX_LNG; ++pos) {
		buffer[pos] = read_byte (byteptr_plus (str, pos));
	}
	buffer[pos] = '\0';

	nx_bt_set_friendly_name (buffer);

	return SFFI_OK;
}

/* PROC nx.bt.add.known.device (VAL [6]BYTE addr, VAL []BYTE name, VAL [4]BYTE class, RESULT INT result) */
int _nx_bt_add_known_device (ECTX UNUSED(ectx), WORD args[])
{
	BYTEPTR addr 	= (BYTEPTR) args[0];
	BYTEPTR name 	= (BYTEPTR) args[1];
	WORD name_len	= args[2];
	BYTEPTR class 	= (BYTEPTR) args[3];
	bt_device_t dev;
	int i;

	for (i = 0; i < 6; ++i) {
		dev.addr[i] = read_byte (byteptr_plus (addr, i));
	}
	dev.addr[6] = '\0'; /* FIXME: verify this */

	for (i = 0; i < name_len && i < BT_NAME_MAX_LNG; ++i) {
		dev.name[i] = read_byte (byteptr_plus (name, i));
	}
	dev.name[i] = '\0';

	for (i = 0; i < 4; ++i) {
		dev.class[i] = read_byte (byteptr_plus (class, i));
	}

	i = nx_bt_add_known_device (&dev);
	write_word ((WORDPTR) args[4], (WORD) i);

	return SFFI_OK;
}

/* PROC nx.bt.remove.known.device (VAL [6]BYTE addr, RESULT INT result) */
int _nx_bt_remove_known_device (ECTX UNUSED(ectx), WORD args[])
{
	BYTEPTR addr = (BYTEPTR) args[1];
	U8 buffer[BT_ADDR_SIZE];
	int i;

	for (i = 0; i < 6; ++i) {
		buffer[i] = read_byte (byteptr_plus (addr, i));
	}
	buffer[6] = '\0'; /* FIXME: verify this */	

	i = nx_bt_remove_known_device (buffer);
	write_word ((WORDPTR) args[1], (WORD) i);

	return SFFI_OK;
}

/* PROC nx.bt.has.dev.waiting.for.pin (RESULT BOOL result) */
int _nx_bt_has_dev_waiting_for_pin (ECTX UNUSED(ectx), WORD args[])
{
	write_word ((WORDPTR) args[0], (WORD) nx_bt_has_dev_waiting_for_pin ());
	return SFFI_OK;
}

/* PROC nx.bt.send.pin (VAL []BYTE pin) */
int _nx_bt_send_pin (ECTX UNUSED(ectx), WORD args[])
{
	BYTEPTR pin 	= (BYTEPTR) args[0];
	WORD pin_len	= args[1];
	char buffer[BT_PIN_MAX_LNG + 1];
	int i;

	for (i = 0; i < pin_len && i < BT_PIN_MAX_LNG; ++i) {
		buffer[i] = read_byte (byteptr_plus (pin, i));
	}
	buffer[i] = '\0';

	nx_bt_send_pin (buffer);

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
	_nx_button,
	_nx_bt_init,
	_nx_bt_set_discoverable,
	_nx_bt_open_port,
        _nx_bt_close_port,
        _nx_bt_connection_pending,
        _nx_bt_accept_connection, 
        _nx_bt_connection_established,
	_nx_bt_stream_open,
        _nx_bt_stream_write,
        _nx_bt_stream_data_written,
        _nx_bt_stream_opened,
	_nx_bt_stream_read,
        _nx_bt_stream_data_read,
	_nx_bt_stream_close,
	_nx_bt_set_friendly_name,
	_nx_bt_add_known_device,
	_nx_bt_remove_known_device,
	_nx_bt_has_dev_waiting_for_pin,
	_nx_bt_send_pin
};
#else /* 0 */
SFFI_FUNCTION sffi_table[] = { };
#endif /* 0 */

const int sffi_table_length = sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
