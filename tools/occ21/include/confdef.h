/* $Id: confdef.h,v 1.1 1996/04/15 10:52:01 djb1 Exp $*/
/*
 *	configuration definitions
 *	Copyright (C) 1987 Inmos limited
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

 
/*									*/
/* Tag definitions as defined by the ILISTer				*/
/*									*/

#define USER_ROM_ROM            4
#define USER_ROM_RAM            0
#define SYSTEM_ROM_ROM          8
#define SYSTEM_ROM_RAM          0
#define ROM_BOOT          	2
#define PATCH_PARAMETER_DATA	16
#define ABSOLUTE_PLACEMENT      32

#define CFB_FILE_TAG		0x42464307
#define CFB_FILE_VERSION_TAG	0x0000000
#define CFB2_FILE_VERSION_TAG	0x0000001
#define CFB2_PROPER_FILE_VERSION_TAG 0x0010000
#define CFB_BOOT_FROM_LINK	CFB_FILE_VERSION_TAG
#define CFB_BOOT_FROM_ROM_ROM	(CFB_FILE_VERSION_TAG | USER_ROM_ROM | SYSTEM_ROM_ROM | ROM_BOOT)
#define CFB_BOOT_FROM_ROM_RAM	(CFB_FILE_VERSION_TAG | USER_ROM_RAM | SYSTEM_ROM_RAM | ROM_BOOT)

/*									*/
/* Tag definitions for records in the .cfb file				*/
/*									*/

#define NONE_TAG                0
#define END_TAG 		1
#define FILE_TAG		2
#define PROCESSOR_TAG		3
#define PARAMETER_TAG		4
#define PROCESS_TAG		5
#define LINK_TAG		6
#define CHANNEL_TAG		7
#define CODE_TAG		8
#define DATA_TAG		9
#define OPTION_TAG		10
#define BEGIN_TAG               11
#define ROUTE_TAG               12
#define CONFIGURATION_TAG       13
#define BLOCK_TAG               14

/*									*/
/* Processor type definitions for processor types in .cfb file		*/
/*									*/

#define T212_PROCESSOR_TYPE	-1
#define M212_PROCESSOR_TYPE	-2
#define T414_PROCESSOR_TYPE	-3
#define T800_PROCESSOR_TYPE	-4
#define T222_PROCESSOR_TYPE	-5
#define TA_PROCESSOR_TYPE 	-6
#define TB_PROCESSOR_TYPE	-7
#define TC_PROCESSOR_TYPE	-8
#define T425_PROCESSOR_TYPE	0
#define T805_PROCESSOR_TYPE	10
#define T801_PROCESSOR_TYPE	20
#define T426_PROCESSOR_TYPE     30
#define T225_PROCESSOR_TYPE	40
#define T400_PROCESSOR_TYPE	50
#define T9000_PROCESSOR_TYPE	300
#define T9000_GAMMA_PROCESSOR_TYPE	302
#define T9000_GAMMAE_PROCESSOR_TYPE	303
#define T450_PROCESSOR_TYPE     1024
#define ST20_PROCESSOR_TYPE     2047

/*                                                                      */
/* Begin type definitions fo rthe BEGIN statement in CFB2 file          */
/*                                                                      */
#define SEQUENCE_TYPE           1
#define GROUP_TYPE              2
/*                                                                      */
/* File type definitions for the FILE statement in CFB2 file            */
/*                                                                      */

#define TCOFF_FILE_TYPE         1
#define BINARY_FILE_TYPE        2

/*                                                                      */
/* Configuration types for the CONFIGURATION statement in .CFB2 file    */
/*                                                                      */
#define MEMORY_CONFIGURATION        1
#define VCP_CONFIGURATION           2
#define INTERNAL_RAM_CONFIGURATION  3
#define LABEL_CONFIGURATION         4
/*                                                                      */
/* Memory bank types for MEMORY_CONFIGURATION in .cf2                   */
/*                                                                      */
#define READ_WRITE_MEMORY           1
#define READ_ONLY_MEMORY            2

/*                                                                      */
/* Memory MODE bits for MEMORY_CONFIGURATION in .CFB2                   */
/*                                                                      */
#define RESERVED_MODE_BIT           2
#define SYSTEM_MODE_BIT             1
#define SYSTEM_MODE                 1
#define RESERVED_MODE               2
#define USER_MODE                   0

#define ROM_TYPE                    2
#define RAM_TYPE                    1

/*                                                                      */
/* Parameter types for the PARAMETER statement in .CFB2 file            */
/*                                                                      */
#define COMPLETE_PARAMETER      1
#define REDUCED_PARAMETER       2

/*									*/
/* Link mode definitions for the link statements in .cfb file 		*/
/*									*/

#define NOPATH_MODE		0
#define PATH_MODE		1
#define NOHOST_MODE             0
#define HOST_MODE               2
/*									*/
/* Channels type definitions for the channel statements in .cfb file 	*/
/*									*/

#define INTERNAL_CHANNEL_TYPE	1
#define EXTERNAL_CHANNEL_TYPE	2
#define VIRTUAL_CHANNEL_TYPE	3

/*									*/
/* Process mode definitions for the process statements in .cfb file	*/
/*									*/

#define LOW_PRIORITY_MODE	0
#define HIGH_PRIORITY_MODE	1
#define PARALLEL_PROCESS_MODE	0
#define SEQUENTIAL_PROCESS_MODE 0x2
#define SEPERATE_STACK_MODE	0x4
#define NO_SEPERATE_STACK_MODE	0
#define DEBUG_MODE              0x8
#define NO_DEBUG_MODE           0
#define PROFILE_MODE            0x10
#define NO_PROFILE_MODE         0
#define EXECUTE_MODE            0x20
#define NO_EXECUTE_MODE         0
#define H_ARCHITECTURE_MODE     0x40
#define NO_H_ARCHITECTURE_MODE  0
#define PROTECTED_MODE          0x80
#define NO_PROTECTED_MODE       0
/*									*/
/* Code type definitions for the code statements in .cfb file		*/
/*									*/

#define INIT_SYSTEM_TYPE		1
#define INIT_PROCESS_TYPE		2
#define INIT_PROCESS_OVERLAY_TYPE	3
#define USER_PROCESS_TYPE		4
#define USER_PROCESS_SHARED_TYPE	5
#define INIT_SYSTEM_OVERLAY_TYPE        6
#define INIT_PROCESS_SHARED_TYPE        7

/*                                                                      */
/* Channel type definitions for the CHANNEL statement in CFB2 file      */
/*                                                                      */

#define INTERNAL_CHANNEL_TYPE           1
#define EXTERNAL_CHANNEL_TYPE           2
#define VIRTUAL_CHANNEL_TYPE            3
#define PSEUDO_CHANNEL_TYPE             4

/*									*/
/* Data type definitions for the data statements in .cfb file		*/
/*									*/

#define STACK_TYPE		1
#define VECTOR_TYPE		2
#define STATIC_TYPE		3
#define HEAP_TYPE		4

/*									*/
/* Processor modes							*/
/*									*/

#define NO_HALT_MODE		0
#define HALT_MODE		1
#define CONF_MODE		2
#define NO_CONF_MODE		0
#define CFB_PATCH_BITS		12
#define CFB_PATCH_BITS_POSITION 2
#define CFB_NO_PATCHING		0 
#define CFB_DATA_PATCHING	1
#define CFB_CODE_PATCHING	2
#define CFB_CODE_AND_DATA_PATCHING 3

/*									*/
/* Bits for Origin statements in .cfb file 				*/
/*									*/
#define CODE_SEGMENT		0
#define PARAMETER_SEGMENT	1
#define STACK_SEGMENT		2
#define VECTOR_SEGMENT		3
#define STATIC_SEGMENT		4
#define HEAP_SEGMENT		5

/*									*/
/* Bit for Origin statements in .cfb file 				*/
/*									*/
#define CODE_SEGMENT_BIT	1
#define PARAMETER_SEGMENT_BIT	1<<PARAMETER_SEGMENT
#define STACK_SEGMENT_BIT	1<<STACK_SEGMENT
#define VECTOR_SEGMENT_BIT	1<<VECTOR_SEGMENT
#define STATIC_SEGMENT_BIT	1<<STATIC_SEGMENT
#define HEAP_SEGMENT_BIT	1<<HEAP_SEGMENT


