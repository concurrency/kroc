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

static const uint8_t data_0_to_26[] = {
	0xeb, 0x3c, 0x90, 0x6d, 0x6b, 0x64, 0x6f, 0x73, 0x66,
	0x73, 0x00, 0x00, 0x02, 0x04, 0x01, 0x00, 0x02, 0x10,
	0x00, 0x50, 0x00, 0xf8, 0x01, 0x00, 0x20, 0x00, 0x40
};
static const uint8_t data_38_to_190[] = {
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
static const uint8_t data_510_to_514[] = {
	0x55, 0xaa, 0xf8, 0xff, 0xff
};
static const uint8_t data_1024_to_1026[] = {
	0xf8, 0xff, 0xff
};
static const uint8_t data_1536_to_1561[] = {
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
} __attribute__ ((packed));
typedef struct _dir_ent_t dir_ent_t;

/* FileStartSector = 
 * ReservedSectors(0x0e) + (NumofFAT(0x10) * Sectors2FAT(0x16)) + 
 * (MaxRootEntry(0x11) * 32 / BytesPerSector(0x0b)) + 
 * ((X − 2) * SectorsPerCluster(0x0d)) */ 

static uint16_t read_fat_entry (uint8_t *fat, uint16_t entry)
{
	uint32_t offset = (entry / 2) * 3;
	uint32_t value = fat[offset + 0] | (fat[offset + 1] << 8) | (fat[offset + 2] << 16);

	if ((entry % 2) == 0) {
		value &= 0xfff;
	} else {
		value >>= 12;
	}

	return value;
}

void fat12_init (uint8_t *data, uint32_t len)
{
	bootsec_t *bs = (bootsec_t *) data;
	uint32_t max_clusters;

	/* This is a fixed 40k FAT12 file system with 16 root dir entries */
	memset (data, 0, len);
	memcpy (data +    0,      data_0_to_26, sizeof (data_0_to_26)); 
	memcpy (data +   38,    data_38_to_190, sizeof (data_38_to_190)); 
	memcpy (data +  510,   data_510_to_514, sizeof (data_510_to_514)); 
	memcpy (data + 1024, data_1024_to_1026, sizeof (data_1024_to_1026)); 
	memcpy (data + 1536, data_1536_to_1561, sizeof (data_1536_to_1561));

	/* Rewrite the total sectors to match length (on a cluster boundary) */
	bs->total_sectors 	= (len / bs->bytes_per_sector) & ~(bs->sectors_per_cluster - 1);
	max_clusters 		= ((bs->sectors_per_fat * bs->bytes_per_sector) * 2) / 3;

	if ((bs->total_sectors / bs->sectors_per_cluster) > max_clusters)
		bs->total_sectors = max_clusters * bs->sectors_per_cluster;
}

int fat12_extract_file (uint8_t *data, uint32_t len, int fn, uint8_t *dst)
{
	bootsec_t 	*bs		= (bootsec_t *) data;
	uint8_t		*data_end	= data + len;
	
	uint32_t 	ds;
	uint32_t	bpc, bpf, to_read;
	uint16_t	nc;
	uint8_t		*fat, *da;
	dir_ent_t	*root_dir, *de;
	
	bpc	= bs->bytes_per_sector * bs->sectors_per_cluster;
	bpf	= bs->bytes_per_sector * bs->sectors_per_fat;
	ds	= bs->reserved_sectors 
			+ (bs->sectors_per_fat * bs->n_fats)
			+ ((bs->root_dir_entries * 32) / bs->bytes_per_sector);
	fat	= data + (bs->bytes_per_sector * bs->reserved_sectors);
	root_dir= (dir_ent_t *) (fat + (bs->n_fats * bpf));
	da	= ((uint8_t *) root_dir) + (bs->root_dir_entries * 32);
	
	if (fat >= data_end)
		return -1;
	if ((fat + bpf) >= data_end)
		return -1;
	if (((uint8_t *) root_dir) >= data_end)
		return -1;
	if (((uint8_t *) (root_dir + (bs->root_dir_entries * 32))) >= data_end)
		return -1;
	if (fn < 0 || fn >= bs->root_dir_entries)
		return -1;

	de	= &(root_dir[fn]);
	to_read = de->length;
	nc	= de->start;

	while (to_read) {
		uint32_t len = to_read > bpc ? bpc : to_read;
		
		if (((nc / 2) * 3) > bpf)
			return -1;
		
		memcpy (dst, da + (nc * bpc), len);
		
		nc 	= read_fat_entry (fat, nc);
		to_read -= len;
	}
	
	return de->length;
}

int fat12_find_file (uint8_t *data, uint32_t len, char *ext, uint32_t *file_len)
{
	bootsec_t 	*bs		= (bootsec_t *) data;
	uint8_t		*data_end	= data + len;
	uint32_t	best_mtime	= 0;
	int		best		= -1;

	uint32_t 	ds;
	uint8_t		*fat;
	dir_ent_t	*root_dir;
	int		i;

	ds	= bs->reserved_sectors 
			+ (bs->sectors_per_fat * bs->n_fats)
			+ ((bs->root_dir_entries * 32) / bs->bytes_per_sector);
	fat	= data + (bs->bytes_per_sector * bs->reserved_sectors);
	root_dir= (dir_ent_t *) (fat + (bs->bytes_per_sector * bs->n_fats * bs->sectors_per_fat));
	
	#ifdef FAT12_DEBUG
	fprintf (stderr,
		"oem_name = \"%c%c%c%c%c%c%c%c\"\n"
		"bytes_per_sector = %d, sectors_per_cluster = %d\n"
		"reserved_sectors = %d, total_sectors = %d\n"
		"n_fats = %d, sectors_per_fat = %d\n"
		"root_dir_entries = %d\n",
		bs->oem_name[0], bs->oem_name[1], bs->oem_name[2], bs->oem_name[3],
		bs->oem_name[4], bs->oem_name[5], bs->oem_name[6], bs->oem_name[7],
		bs->bytes_per_sector, bs->sectors_per_cluster,
		bs->reserved_sectors, bs->total_sectors,
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

	for (i = 0; i < bs->root_dir_entries; ++i) {
		dir_ent_t *de = &(root_dir[i]);
		if (!(de->name[0] == 0x00 || de->name[0] == 0xe5)) {
			#ifdef FAT12_DEBUG
			fprintf (stderr, 
				"root[%d] = \"%c%c%c%c%c%c%c%c\".\"%c%c%c\"\n"
				"\tstart = %d, length = %d\n"
				"\tmdate = %04x, mtime = %04x\n",
				i,
				de->name[0], de->name[1], de->name[2], de->name[3],
				de->name[4], de->name[5], de->name[6], de->name[7],
				de->ext[0], de->ext[1], de->ext[2],
				de->start, de->length,
				de->mdate, de->mtime
			);
			#endif
			
			if ((de->ext[0] == ext[0]) 
					&& (de->ext[1] == ext[1]) 
					&& (de->ext[2] == ext[2])) {
				uint32_t mtime = (de->mdate << 16) | (de->mtime);
				if (mtime > best_mtime) {
					best 		= i;
					best_mtime 	= mtime;
					if (file_len)
						*file_len = de->length;
				}
			}
		} else {
			#ifdef FAT12_DEBUG
			fprintf (stderr, "root[%d] = empty (%02x)\n", 
				i, de->name[0]
			);
			#endif
		}
	}
	
	return best;
}

#ifdef FAT12_DEBUG
int main (int argc, char *argv[]) {
	uint32_t f_len;
	uint8_t *data;
	size_t len;
	FILE *fh = NULL;
	int fn;

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
		len = 60 * 1024;
		data = (uint8_t *) malloc (len);
		fat12_init (data, len);
	}

	fn = fat12_find_file (data, (uint32_t) len, "TBC", &f_len);
	fprintf (stdout, "fat12_find_file (%p, %ld) = %d\n",
		data, len, fn
	);
	if (fn >= 0) {
		uint8_t *f_data = (uint8_t *) malloc (f_len);
		int ret = fat12_extract_file (data, (uint32_t) len, fn, f_data);
		fprintf (stdout, "fat12_decode_file (%p, %ld, %d, %p) = %d\n",
			data, len, fn, f_data,
			ret
		);
		free (f_data);
	}

	if (fh) {
		munmap (data, len);
		fclose (fh);
	} else {
		free (data);
	}

	return 0;
}
#endif
