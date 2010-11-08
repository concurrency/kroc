/*
 * fat12.c - NXT FAT12 initialisation and access functions
 *
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 *
 * Testing:
 *   gcc -O2 -Wall fat12.c -o fat12 -DFAT12_DEBUG
 */

#ifdef FAT12_DEBUG
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <errno.h>
#include <sys/mman.h>
#include <string.h>
#else
#include "tvm-nxt.h"
#endif

static uint8_t data_0_to_26[] = {
	0xeb, 0x3c, 0x90, 0x6d, 0x6b, 0x64, 0x6f, 0x73, 0x66,
	0x73, 0x00, 0x00, 0x02, 0x04, 0x01, 0x00, 0x02, 0x10,
	0x00, 0x50, 0x00, 0xf8, 0x01, 0x00, 0x20, 0x00, 0x40
};
static uint8_t data_38_to_190[] = {
	0x29, 0x4a, 0xb5, 0x2d, 0x2e, 0x4e, 0x58, 0x54, 0x20,
	0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x46, 0x41,
	0x54, 0x31, 0x32, 0x20, 0x20, 0x20, 0x0e, 0x1f, 0xbe,
	0x5b, 0x7c, 0xac, 0x22, 0xc0, 0x74, 0x0b, 0x56, 0xb4,
	0x0e, 0xbb, 0x07, 0x00, 0xcd, 0x10, 0x5e, 0xeb, 0xf0,
	0x32, 0xe4, 0xcd, 0x16, 0xcd, 0x19, 0xeb, 0xfe, 0x54,
	0x68, 0x69, 0x73, 0x20, 0x69, 0x73, 0x20, 0x6e, 0x6f,
	0x74, 0x20, 0x61, 0x20, 0x62, 0x6f, 0x6f, 0x74, 0x61,
	0x62, 0x6c, 0x65, 0x20, 0x64, 0x69, 0x73, 0x6b, 0x2e,
	0x20, 0x20, 0x50, 0x6c, 0x65, 0x61, 0x73, 0x65, 0x20,
	0x69, 0x6e, 0x73, 0x65, 0x72, 0x74, 0x20, 0x61, 0x20,
	0x62, 0x6f, 0x6f, 0x74, 0x61, 0x62, 0x6c, 0x65, 0x20,
	0x66, 0x6c, 0x6f, 0x70, 0x70, 0x79, 0x20, 0x61, 0x6e,
	0x64, 0x0d, 0x0a, 0x70, 0x72, 0x65, 0x73, 0x73, 0x20,
	0x61, 0x6e, 0x79, 0x20, 0x6b, 0x65, 0x79, 0x20, 0x74,
	0x6f, 0x20, 0x74, 0x72, 0x79, 0x20, 0x61, 0x67, 0x61,
	0x69, 0x6e, 0x20, 0x2e, 0x2e, 0x2e, 0x20, 0x0d, 0x0a
};
static uint8_t data_510_to_514[] = {
	0x55, 0xaa, 0xf8, 0xff, 0xff
};
static uint8_t data_1024_to_1026[] = {
	0xf8, 0xff, 0xff
};
static uint8_t data_1536_to_1561[] = {
	0x4e, 0x58, 0x54, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
	0x20, 0x20, 0x08, 0x00, 0x00, 0xbb, 0x66, 0x68, 0x3d,
	0x68, 0x3d, 0x00, 0x00, 0xbb, 0x66, 0x68, 0x3d
};

enum {
	ENT_FREE	= 0x000,
	ENT_BAD		= 0xFF7,
	ENT_EOC		= 0xFF8,
};

/* FAT data structures */
struct _bootsec_t {
	uint8_t		jump[3];
	uint8_t		oem_name[8];
	uint16_t	bytes_per_sector;
	uint8_t		sectors_per_cluster;
	uint16_t	reserved_sectors;
	uint8_t		n_fats;
	uint16_t	root_dir_entries;
	uint16_t	total_sectors;
	uint8_t		media_descriptor;
	uint16_t	sectors_per_fat;
	uint16_t	sectors_per_track;
	uint16_t	n_heads;
	uint32_t	hidden_sectors;
	uint32_t	total_sectors_32b;
} __attribute__ ((packed));
typedef struct _bootsec_t bootsec_t;

struct _dir_ent_t {
	uint8_t		name[8];
	uint8_t		ext[3];
	uint8_t		attr;
	uint8_t		reserved;
	uint8_t		ctime_fine;
	uint16_t	ctime;
	uint16_t	cdate;
	uint16_t	adate;
	uint16_t	ea_index;
	uint16_t	mtime;
	uint16_t	mdate;
	uint16_t	start;
	uint32_t	length;
};
typedef struct _dir_ent_t dir_ent_t;

/* FileStartSector = 
 * ReservedSectors(0x0e) + (NumofFAT(0x10) * Sectors2FAT(0x16)) + 
 * (MaxRootEntry(0x11) * 32 / BytesPerSector(0x0b)) + 
 * ((X − 2) * SectorsPerCluster(0x0d)) */ 

void fat12_init (uint8_t *data, uint32_t len)
{
	memset (data, 0, len);
	/* FIXME: this is a fixed 40k FAT12 file system with 16 root dir entries */
	memcpy (data +    0,      data_0_to_26, sizeof (data_0_to_26)); 
	memcpy (data +   38,    data_38_to_190, sizeof (data_38_to_190)); 
	memcpy (data +  510,   data_510_to_514, sizeof (data_510_to_514)); 
	memcpy (data + 1024, data_1024_to_1026, sizeof (data_1024_to_1026)); 
	memcpy (data + 1536, data_1536_to_1561, sizeof (data_1536_to_1561)); 
}

int fat12_decode (uint8_t *data, uint32_t len)
{
	bootsec_t 	*bs = (bootsec_t *) data;
	uint32_t 	ds;
	uint8_t		*data_end = data + len;
	uint8_t		*fat;
	dir_ent_t	*root_dir;

	ds = 	bs->reserved_sectors 
		+ (bs->sectors_per_fat * bs->n_fats)
		+ ((bs->root_dir_entries * 32) / bs->bytes_per_sector);
	fat =	data + (bs->bytes_per_sector * bs->reserved_sectors);
	root_dir = (dir_ent_t *) (fat + (bs->bytes_per_sector * bs->n_fats * bs->sectors_per_fat));
	
	#ifdef FAT12_DEBUG
	fprintf (stderr,
		"oem_name = \"%c%c%c%c%c%c%c%c\"\n"
		"bytes_per_sector = %d, sectors_per_cluster = %d\n"
		"reserved_sectors = %d\n"
		"n_fats = %d, sectors_per_fat = %d\n"
		"root_dir_entries = %d\n",
		bs->oem_name[0], bs->oem_name[1], bs->oem_name[2], bs->oem_name[3],
		bs->oem_name[4], bs->oem_name[5], bs->oem_name[6], bs->oem_name[7],
		bs->bytes_per_sector, bs->sectors_per_cluster,
		bs->reserved_sectors,
		bs->n_fats, bs->sectors_per_fat,
		bs->root_dir_entries
	);
	#endif

	if (fat >= data_end)
		return -1;
	if ((fat + (bs->bytes_per_sector * bs->sectors_per_fat)) >= data_end)
		return -1;
	if (((uint8_t *) root_dir) >= data_end)
		return -1;
	if (((uint8_t *) (root_dir + (bs->root_dir_entries * 32))) >= data_end)
		return -1;

	return 0;
}

#ifdef FAT12_DEBUG
int main (int argc, char *argv[]) {
	uint8_t *data;
	size_t len;
	FILE *fh = NULL;

	if (argc >= 2) {
		char *file = argv[1];

		fprintf (stdout, "Using %s for testing.\n", file);
		fh = fopen (file, "r");
		if (fh == NULL) {
			fprintf (stderr, "Unable to open %s: %s\n", file, strerror (errno));
			return 1;
		}

		fseek (fh, 0, SEEK_END);
		len = (uint32_t) ftell (fh);
		fseek (fh, 0, SEEK_SET);
	
		data = (uint8_t *) mmap (0, len, PROT_READ, MAP_SHARED, fileno (fh), 0); 
		if (data == NULL) {
			fprintf (stderr, "Unable to map %s: %s\n", file, strerror (errno));
			fclose (fh);
			return 1;
		}
	} else {
		fprintf (stdout, "Using built-in file system for testing.\n");
		len = 40 * 1024;
		data = (uint8_t *) malloc (len);
		fat12_init (data, len);
	}

	fprintf (stdout, "fat12_decode (%p, %ld) = %d\n",
		data, len,
		fat12_decode (data, (uint32_t) len)
	);

	if (fh) {
		munmap (data, len);
		fclose (fh);
	} else {
		free (data);
	}

	return 0;
}
#endif
