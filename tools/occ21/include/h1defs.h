/*
 *	Hardware definitions
 *	Copyright (C) 1994 Inmos Limited
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

/*#define MAX_VCP_PACKET_LENGTH	1024*/
#define MAX_VCP_PACKET_LENGTH	1020
#define VCP_PACKET_LENGTH  	MAX_VCP_PACKET_LENGTH
#define H1_WORD_SIZE            4
#define H1_DEFAULT_MEMSTART_FOR_SINGLE 0x80000088

#define H1_MAX_PACKET_SIZE                  VCP_PACKET_LENGTH 
#define PASS_ON_BUFFER_SIZE                 (H1_MAX_PACKET_SIZE + 4)
#define LABELLING_BLOCK_WORD_SIZE           (1024/4)
#define LABELLING_BLOCK_BYTE_SIZE           1024
#define VIRTUAL_NETWORK_LOADER_APPROX_SIZE  LABELLING_BLOCK_BYTE_SIZE   
#define SYSTEM_MEMORY_AREA                  (13*1024)

#define H1_FUDGED_OVERLAY_WORKSPACE_SIZE 60       /* VERY VERY TEMPORARY !!!! */
#define ALLOW_NETWORK_LOADER_SIZE           1024*4

#define H1_VCP_START_ADDRESS                0x80000040
#define H1_VCP_CHANNEL_PAIR_SIZE            8

#define CURRENT_HEADER_SIZE_LIMIT           4
#define ENCODED_VLCB_IN_BYTES               12
#define ENCODED_VLCB_IN_WORDS               3
#define ENCODED_LINK_DATA_IN_WORDS          4
#define ENCODED_LINK_DATA_IN_BYTES          8
#define ENCODED_MAX_NUMBER_OF_VLINKS_IN_BYTES 4

#define TOTAL_ENCODED_LINKS_DATA_IN_BYTES   (ENCODED_LINK_DATA_IN_BYTES * NUMBER_OF_LINKS)

#define ENCODED_VLCB_ET_AL_FOR_ONE_VLINK    ((TOTAL_ENCODED_LINKS_DATA_IN_BYTES + ENCODED_MAX_NUMBER_OF_VLINKS_IN_BYTES) + ENCODED_VLCB_IN_BYTES)


#define LINK_INDEX                          0
#define HEADER_INDEX                        1
#define BUFFER_INDEX                        2

#define H1_VLCB_SIZE_IN_WORDS               8
#define H1_BUFFER_SIZE_IN_BYTES             32
#define H1_RESOURCE_SIZE_IN_WORDS           2

#define H1_FUDGED_EDGE_LINK_MODE            0
#define H1_FUDGED_EDGE_MAX_HEADER           1 
#define H1_BOOTFILE_VERSION                 2

#define EDGE_CHANNEL                        MY_UNDEFINED_VALUE
#define EDGE_ROUTE                          MY_UNDEFINED_VALUE

#define VERSION_RECORD_SIZE                 8 /* Tag and version*/
#define VERSION_RECORD_TAG                  16

#define H1_NETWORK_LOADER_INFORMATION_RECORD_TAG    17
#define H1_CFB_FILENAME_RECORD_TAG                  18
#define HOST_INFORMATION_RECORD_TAG                 19
#define ROUTER_HEADER_RECORD_TAG                    20

#define H1_COMMENT_RECORD_TAG_SIZE          4
#define ROM_ROOT_PROCESSOR                  0
#define CFB_ROOT_PROCESSOR                  ROM_ROOT_PROCESSOR

#define H1_NETWORK_LOADER_INFORMATION_RECORD_SIZE 12 /* Tag, address, length */

#define H1_CFB_FILENAME_RECORD_TAG_SIZE     4



#define SYSTEM_MEMORY_IN_INTERNAL_MEMORY_HERE 0x2800

#define ONE_BYTE_VLCB_SELECTOR              1
#define TWO_BYTE_VLCB_SELECTOR              (ONE_BYTE_VLCB_SELECTOR+1)

/*
 * long header bit in link mode register
 */
#define LINK_MODE_REGISTER_LONG_HEADER_BIT  0x4 
