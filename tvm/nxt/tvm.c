/*
 * tvm.c - NXT TVM wrapper
 *
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

#include "tvm-nxt.h"
//#include <base/display.h>
//#include <base/drivers/_avr.h>
//#include <base/drivers/_sensors.h>
//#include <base/drivers/_usb.h>
//#include <base/drivers/bt.h>
//#include <base/drivers/motors.h>
//#include <base/drivers/sensors.h>

//static void *mem_pool = NX_USERSPACE_START; 
static tvm_t tvm;
static tvm_ectx_t context;

static WORD nxt_get_time (ECTX ectx) {
	(void) ectx;
	return 0; /* FIXME: */
	//return nx_systick_get_ms ();
}

static void nxt_modify_sync_flags (ECTX ectx, WORD set, WORD clear) {
	/* FIXME: */
	//nx_interrupts_disable ();
	ectx->sflags = (ectx->sflags & (~clear)) | set;
	//nx_interrupts_enable ();
}

void tvm_main (void) {
	UWORD tbc_length 	= 0;
	BYTE *tbc_data 		= NULL;
	tbc_t *tbc 		= NULL;
		
	tvm_init (&tvm);
	
	#if 0

	for (;;) {
		U8 buffer[NX_USB_PACKET_SIZE];
		WORD usb 	= 0;
		U32 pos		= 0;
		U32 i;
		int running 	= 1;

		nx_display_clear ();
		nx_display_string ("I am the TVM.");
		nx_display_end_line ();

		if (tbc != NULL) {
			nx_display_string ("OK to reload.");
			nx_display_end_line ();
		}

		nx_usb_read (buffer, NX_USB_PACKET_SIZE);
		while (usb == 0) {
			if ((pos = nx_usb_data_read ()) >= 8) {
				if ((tbc_length = valid_tbc_header (buffer))) {
					tbc_length 	+= 8;
					tbc_data 	= (BYTE *) mem_pool;
					tbc		= NULL;
					for (i = 0; i < pos; ++i) {
						tbc_data[i] = buffer[i];
					}
					usb = tbc_length - pos;
				}
			} else {
				switch (nx_avr_get_button ()) {
					case BUTTON_OK:
						if (tbc != NULL) {
							usb = -1;
						}
						break;
					case BUTTON_CANCEL:
						nx__avr_power_down ();
						break;
					default:
						nx_systick_wait_ms (100);
						break;
				}
			}
		}

		if (usb > 0) {
			nx_display_cursor_set_pos (0, 1);
			nx_display_string ("Got header (");
			nx_display_uint (tbc_length);
			nx_display_string (")");
			nx_display_end_line ();
			
			nx_display_cursor_set_pos (0, 2);
			nx_display_uint (usb);
			nx_display_string ("      ");
		} else {
			nx_display_string ("Reload TBC (");
			nx_display_uint (tbc_length);
			nx_display_string (")");
			nx_display_end_line ();
		}

		if (usb > 0) {
			nx_usb_write ((U8 *) &pos, 4);
		}

		while (usb > 0) {
			U32 tmp;
			
			if (usb >= NX_USB_PACKET_SIZE) {
				nx_usb_read (&(tbc_data[pos]), NX_USB_PACKET_SIZE);
			} else {
				nx_usb_read (&(tbc_data[pos]), usb);
			}

			while (!(tmp = nx_usb_data_read ()))
				continue;

			pos += tmp;
			usb -= tmp;
			
			nx_display_cursor_set_pos (0, 2);
			nx_display_uint (usb);
			nx_display_string ("      ");
			
			nx_usb_write ((U8 *) &pos, 4);
		}
		
		nx_display_cursor_set_pos (0, 2);
		nx_display_string ("Got TBC.    ");
		nx_display_end_line ();
		nx_systick_wait_ms (200);

		tvm_ectx_init (&tvm, &context);
		context.mem_pool = mem_pool + tbc_length;
		context.get_time = nxt_get_time;
		context.modify_sync_flags = nxt_modify_sync_flags;
		context.sffi_table = sffi_table;
		context.sffi_table_length = sffi_table_length;
		
		tlsf_init_memory_pool (NX_USERSPACE_SIZE - tbc_length, 
				(void *) context.mem_pool);
		
		if ((tbc = load_context_with_tbc (&context, tbc, tbc_data, tbc_length)) == NULL) {
			nx_display_string ("Decode failed!");
			nx_systick_wait_ms (3000);
			continue;
		}

		nx_display_string ("Running...");
		//nx_display_end_line ();
		nx_systick_wait_ms (1000);
		//nx_display_clear ();
		
		while (running) {
			int ret = tvm_run (&context);
			
			switch (ret) {
				case ECTX_PREEMPT:
				case ECTX_TIME_SLICE: {
					/* Safe to continue. */
					break;
				}
				case ECTX_SLEEP: {
					WORD next = context.tnext;
					WORD now = nxt_get_time (&context);
					while (TIME_AFTER (next, now)) {
						nx_systick_wait_ms (next - now);
						now = nxt_get_time (&context);
					}
					break;
				}
				case ECTX_INTERRUPT: {
					//clear_pending_interrupts ();
					break;
				}
				case ECTX_EMPTY: {
					//if (!waiting_on_interrupts ()) {
						//terminate("deadlock", NULL);
					//}
					nx_display_end_line ();
					nx_display_string ("Deadlock.");
					break;
				}
				case ECTX_SHUTDOWN: {
					nx_display_end_line ();
					nx_display_string ("End of program.");
					running = 0;
					break;
				}
				default: {
					nx_display_end_line ();
					nx_display_string ("Error = ");
					nx_display_uint (ret);
					running = 0;
					break;
				}
			}
		}
		
		for (i = 0; i < 3; ++i)
			nx_motors_stop (i, FALSE);
		for (i = 0; i < 4; ++i)
			nx__sensors_disable (i);
		nx_systick_wait_ms (3000);
	}

	#endif
	/* NOTREACHED */
}
