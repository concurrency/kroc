/* $Id: memlib.h,v 1.1 1996/04/15 10:52:13 djb1 Exp $ */

/*
 *	definitions and prototypes for the MEM package
 *	Copyright (C) 1991 Inmos Limited
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef MEMLIB
#define MEMLIB

#ifndef _IMSTYPEH
#include "imstype.h"
#endif

/* ------------------------------------------------------------------------ */

#define MEM_LIBRARY_VERSION    	0x030002
#define MEM_MAX_SEQUENCE  	8

/* ------------------------------------------------------------------------ */
/* --- General types --- */

typedef struct mem_s  *mem_t;

typedef enum { mem_err, mem_ok                               } mem_status_t;
typedef enum { mem_sev_warning, mem_sev_error, mem_sev_fatal } mem_severity_t;
typedef enum { mem_continue, mem_terminate                   } mem_continue_t;
typedef enum { mem_t9000_model, mem_t450_model 		     } mem_model_types_t;

typedef mem_continue_t (*mem_handler_fn_t)( mem_t handle,
                                            mem_severity_t sev,
                                            const char *message,
                                            const char *filename,
                                            int line_number,
                                            void *user_parameter );


/* ------------------------------------------------------------------------ */
/* --- Data structures returned from mem --- */

typedef enum
    {
        mem_inactive_bank = 0,
        mem_ram_bank      = 1,
        mem_rom_bank      = 2,
        mem_port_bank     = 3
    } mem_type_t;

typedef enum
    {
        mem_bits_8        = 1,
        mem_bits_16       = 3,
        mem_bits_32       = 7,
        mem_bits_64       = 15
    } mem_width_t;

typedef struct mem_structure_page_s
  {
    mem_type_t      type; 
    mem_width_t     width;
    INT32           base_address;
    INT32           length;
  } mem_structure_page_t;

typedef struct mem_structure_s
  {
    int                     num_pages;
    BOOL		    has_cover_all_bank;
    mem_structure_page_t   *pages;
  } mem_structure_t;

/* --- */

typedef struct mem_bank_regs_s
  {
    INT32    address_register;			/* T9000 */
    INT32    mask_register;			/* T9000 */
    INT32    ras_bits_register;			/* T9000 */
    INT32    ras_strobe_register;		/* T9000 */
    INT32    cas_strobe_register;		/* T9000 */
    INT32    prog_strobe_register;		/* T9000 */
    INT32    write_strobe_register;		/* T9000 */
    INT32    timing_control_register;		/* T9000 */
    INT32    format_control_register;		/* T9000 */
    INT32    config_data_field_0;		/* T450  */
    INT32    config_data_field_1;		/* T450  */
    INT32    config_data_field_2;		/* T450  */
    INT32    config_data_field_3;		/* T450  */
  } mem_bank_regs_t;

typedef struct mem_registers_s
  {
    BOOL              remap_boot_bank; 		/* T9000 */
    INT32             refresh_control_register;	/* T9000 */
    mem_bank_regs_t   bank[5];
  } mem_registers_t;

/* --- */

typedef struct mem_strobe_s
    {
        BOOL    enabled;
        char    title[256];
        BOOL    read;				/* T9000 */
        BOOL    write;				/* T9000 */
        BOOL    falling_read;			/* T450 */
        BOOL    falling_write;			/* T450 */
        BOOL    rising_read;			/* T450 */
        BOOL    rising_write;			/* T450 */
        int     time_to_falling_edge;
        int     time_to_rising_edge;
    } mem_strobe_t;


typedef struct mem_bank_s
    {
        BOOL    enabled;
        char    title[256];
        BOOL    is_dram;
        BOOL    refresh_disabled;
        BOOL    cacheable;			/* T9000 */
        BOOL    device_only;			/* T9000 */
	BOOL    page_mode_disabled;		/* T450 */
        INT32   base_address;
        INT32   address_mask;
        int     port_size;
        INT32   page_address_bits;
        int     page_address_shift;
        int     ras_precharge_time;
        int     ras_edge_time;
        BOOL    ras_edge_read;			/* T450 */
        BOOL    ras_edge_write;			/* T450 */
        int     ras_cycle_time;
        int     cas_cycle_time;
        int     bus_release_time;
        BOOL    wait_pin;
	int	data_drive_delay;		/* T450 */

        mem_strobe_t  prog_strobe;
        mem_strobe_t  ras_strobe;
        mem_strobe_t  cas_strobe;
        mem_strobe_t  write_strobe;
    } mem_bank_t;

typedef struct mem_data_s
    {
        int  	processor_id;
        int  	dram_refresh_interval;
        BOOL 	dram_refresh_use_memreqout;	/* T9000 */
	BOOL	signal_all_pending_cycles;	/* T450 */
        int  	dram_refresh_time;
        int  	dram_refresh_ras_time;
        BOOL 	remap_boot_bank;
	BOOL	proc_clock_out;			/* T450 */

	int	pad_strength_rcp0;		/* T450 */
	int	pad_strength_rcp1;		/* T450 */
	int	pad_strength_rcp2;		/* T450 */
	int	pad_strength_rcp3;		/* T450 */
	int	pad_strength_be1;		/* T450 */
	int	pad_strength_be2;		/* T450 */
	int	pad_strength_a2_8;		/* T450 */
	int	pad_strength_a9_12;		/* T450 */
	int	pad_strength_a13_16;		/* T450 */
	int	pad_strength_a17_20;		/* T450 */
	int	pad_strength_a21_24;		/* T450 */
	int	pad_strength_a25_31;		/* T450 */
	int	pad_strength_d0_7;		/* T450 */
	int	pad_strength_d8_15;		/* T450 */
	int	pad_strength_d16_31;		/* T450 */

        mem_bank_t bank[5];		/* Incorporate boot bank data */
    } mem_data_t;

/* --- An input structure describing a sequence of cycles --- */

typedef enum 
    {
        mem_read_cycle,
        mem_write_cycle,
        mem_refresh_cycle
    } mem_cycle_t; 

typedef struct mem_sequence_s
    {
	mem_cycle_t	cycle_type;
	int		bank;
	INT32		address;
    } mem_sequence_t;

/* --- Timing diagram data output --- */

typedef enum 
    {
	mem_bank_0_displayed    = 0x001,
	mem_bank_1_displayed    = 0x002,
	mem_bank_2_displayed    = 0x004,
	mem_bank_3_displayed    = 0x008,
	mem_refresh_displayed	= 0x100
    } mem_displayed_elements_t;

typedef enum 
    {
	mem_ras_subcycle,
	mem_cas_subcycle,
	mem_float_subcycle,
	mem_refresh_subcycle
    } mem_subcycle_t;

typedef enum
    { 
        mem_ras_strobe,
	mem_cas_strobe,
	mem_prog_strobe,
	mem_write_strobe,
	mem_address_bus,
	mem_data_bus
    } mem_entity_enum_t;

typedef enum
    {
        mem_transit_low,
	mem_transit_high,
	mem_transit_active,
	mem_transit_inactive,
	mem_transit_read
    } mem_transition_t;

/* --- */

typedef struct mem_entity_timing_s mem_entity_timing_t;

struct mem_entity_timing_s
    {
	mem_entity_timing_t  *next;
	int	    	      bank;
	mem_entity_enum_t     entity;
	char		     *title;
	int		      transition_count;
	mem_transition_t      transition[1 + MEM_MAX_SEQUENCE*4];
	int		      transition_timestamp[1 + MEM_MAX_SEQUENCE*4];
	int		      ypos;		/* User field */
    };

/* --- */

typedef struct mem_timing_s
    {
	int     displayed_mask;					/* Orred display elements 		*/

	int     pico_seconds_per_phase;

	int	clock_rise_time;	/* Accuraccy bits, these will be dummy filled. But by inserting */
	int 	clock_fall_time;	/* them now later versions will not need graphics mods  */

	int	strobe_rise_time;	/* Accuraccy bits, these will be dummy filled. But by inserting */
	int 	strobe_fall_time;	/* them now later versions will not need graphics mods  */

	int     address_delay;		/* Accuraccy bits, these will be dummy filled. But by inserting */
	int	address_settle_time;	/* them now later versions will not need graphics mods  */

	int     data_delay;		/* Accuraccy bits, these will be dummy filled. But by inserting */
	int	data_settle_time;	/* them now later versions will not need graphics mods  */

	int     data_read_start;	/* Accuraccy bits, these will be dummy filled. But by inserting */
	int	data_read_end;		/* them now later versions will not need graphics mods  */

	int     access_count;					/* Number of reads/writes/refreshes 	*/
	int     access_timestamp[MEM_MAX_SEQUENCE];		/* An array of timestamps for access ends */

	int     sub_access_count;				/* Number of ras, cas and bus releases	*/
	mem_subcycle_t  sub_access_types[MEM_MAX_SEQUENCE*3];	/* Is it ras cas or release 		*/
        int    	sub_access_timestamp[MEM_MAX_SEQUENCE*3];	/* timestamp for end of each		*/

	mem_entity_timing_t	*strobe_list;
	mem_entity_timing_t	*bus_list;

    } mem_timing_t;

/* ------------------------------------------------------------------------ */
/* --- Global mem functions --- */

INT32 mem_version( void );

mem_status_t   mem_open( mem_t             *handle, 
			 mem_model_types_t  model_type,
                         mem_handler_fn_t   error_function,
                         void              *user_handler_parameter );

mem_status_t   mem_close( mem_t  *handle );

mem_status_t   mem_set_isearch_name( mem_t              handle,
                                     const char        *isearch_name );

mem_status_t   mem_get_isearch_name( mem_t              handle,
                                     const char       **isearch_name );

mem_status_t   mem_read_mem( mem_t         handle, 
                             const char   *filename );

mem_status_t   mem_write_mem( mem_t         handle, 
                              const char   *filename );

mem_status_t   mem_check( mem_t   handle );

/* ------------------------------------------------------------------------ */
/* --- mem interrogation functions --- */

mem_status_t   mem_get_structure( mem_t handle, mem_structure_t **data );

mem_status_t   mem_get_registers( mem_t handle, mem_registers_t **data );

mem_status_t   mem_get_data( mem_t handle, mem_data_t **data );

/* ------------------------------------------------------------------------ */

mem_status_t   mem_calculate_timing( mem_t   	      handle, 
				     int 	      sequence_size,
				     mem_sequence_t  *sequence,
				     int	      clock_freq_mhz,
				     mem_timing_t   **timing );

mem_status_t   mem_free_timing( mem_timing_t   *timing );

/* ------------------------------------------------------------------------ */
/* --- Particular constant names --- */

char *mem_device_name( int id );
char *mem_strobe_name( int id );

/* ------------------------------------------------------------------------ */
/* --- workarounds --- */

mem_status_t mem_disable_emi_page_shift_workaround (void);


/* ------------------------------------------------------------------------ */
/* --- Functions to return general constants --- */
#endif
