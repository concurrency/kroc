/*
	rasterio: libpng interface
	Copyright (C) 2007  Adam Sampson <ats@offog.org>

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation, either
	version 2 of the License, or (at your option) any later version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library.  If not, see
	<http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <png.h>

/* FIXME: This code doesn't really use CIF; it's just that there isn't a
 * portable header that provides the mobile type allocation stuff. */
#include <cif.h>

/*{{{  terminate*/
static char *terminate (const char *s, int len)
{
	char *buf = malloc (len + 1);

	if (buf == NULL)
		SetErr ();

	memcpy (buf, s, len);
	buf[len] = '\0';
	
	return buf;
}
/*}}}*/

/*{{{  _read_raster_png */
/* #PRAGMA EXTERNAL "PROC C.read.raster.png (VAL []BYTE filename, RESULT RASTER raster, RESULT INT rc) = 0" */
void _read_raster_png (int *w)
{
	char *filename = terminate ((const char *) w[0], w[1]);
	mt_array_t **raster = (mt_array_t **) &w[3];
	int *rc = (int *) w[6];

	FILE *f = NULL;
	png_structp png = NULL;
	png_infop info = NULL;
	png_uint_32 width, height;
	int bit_depth, color_type, interlace_method, compression_method, filter_method;
	png_bytep *rows;
	int i;
	int *data;

	*rc = -1;

	/*{{{  open the file */
	f = fopen (filename, "rb");
	free (filename);
	if (f == NULL)
		goto out;
	/*}}}*/

	/*{{{  set up libpng */
	png = png_create_read_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png == NULL)
		goto out;

	info = png_create_info_struct (png);
	if (info == NULL)
		goto out;

	if (setjmp (png_jmpbuf (png)))
		goto out;

	png_init_io (png, f);
	/*}}}*/

	/*{{{  read header */
	png_read_info (png, info);
	png_get_IHDR (png, info, &width, &height, &bit_depth, &color_type,
	              &interlace_method, &compression_method, &filter_method);
	/*}}}*/

	/*{{{  set up transformations to #AARRGGBB */
	if (color_type == PNG_COLOR_TYPE_PALETTE)
		png_set_palette_to_rgb (png);
	if (bit_depth == 16)
		png_set_strip_16 (png);
	png_set_invert_alpha (png);
	if (bit_depth < 8)
		png_set_packing (png);
	if (color_type == PNG_COLOR_TYPE_GRAY || color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
		png_set_gray_to_rgb (png);

	i = 1;
	if (((char *) &i)[0] == 0) {
		/* Big-endian machine -- ARGB. */
		if (color_type == PNG_COLOR_TYPE_RGB)
			png_set_filler (png, 0, PNG_FILLER_BEFORE);
		if (color_type == PNG_COLOR_TYPE_RGB_ALPHA)
			png_set_swap_alpha (png);
	} else {
		/* Little-endian machine -- BGRA */
		if (color_type == PNG_COLOR_TYPE_RGB)
			png_set_filler (png, 0, PNG_FILLER_AFTER);
		if (color_type == PNG_COLOR_TYPE_RGB || color_type == PNG_COLOR_TYPE_RGBA)
			png_set_bgr (png);
	}
	/* FIXME: Do gamma correction? */
	/*}}}*/

	/*{{{  allocate raster */
	/* FIXME: This shouldn't be CCSP-specific */
	*raster = ccsp_mt_alloc (MT_MAKE_ARRAY_TYPE (2, MT_MAKE_NUM (MT_NUM_INT32)), width * height);
	(*raster)->dimensions[0] = height;
	(*raster)->dimensions[1] = width;
	data = (int *) (*raster)->data;
	/*}}}*/

	/*{{{  read image data */
	rows = malloc (height * sizeof *rows);
	for (i = 0; i < height; i++) {
		rows[i] = (png_bytep) &data[width * i];
	}
	png_read_image (png, rows);
	free (rows);
	/*}}}*/

	*rc = 0;

out:
	/*{{{  clean up */
	if (png != NULL)
		png_destroy_read_struct (&png, &info, NULL);
	if (f != NULL)
		fclose (f);
	/*}}}*/
}
/*}}}*/

/*{{{  _write_raster_png */
/* #PRAGMA EXTERNAL "PROC C.write.raster.png (VAL []BYTE filename, VAL [][]INT raster, RESULT INT rc) = 0" */
void _write_raster_png (int *w)
{
	char *filename = terminate ((const char *) w[0], w[1]);
	int *raster = (int *) w[2];
	const int height = w[3];
	const int width = w[4];
	int *rc = (int *) w[5];

	FILE *f = NULL;
	png_structp png = NULL;
	png_infop info = NULL;
	png_bytep *rows;
	int i;

	*rc = -1;

	/*{{{  open the file */
	f = fopen (filename, "wb");
	free (filename);
	if (f == NULL)
		goto out;
	/*}}}*/

	/*{{{  set up libpng */
	png = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png == NULL)
		goto out;

	info = png_create_info_struct (png);
	if (info == NULL)
		goto out;

	if (setjmp (png_jmpbuf (png)))
		goto out;

	png_init_io (png, f);
	/*}}}*/

	/*{{{  write header */
	png_set_IHDR (png, info, width, height,
	              8, PNG_COLOR_TYPE_RGB_ALPHA, PNG_INTERLACE_NONE,
	              PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

	png_write_info (png, info);
	/*}}}*/

	/*{{{  set up transformations from #AARRGGBB */
	png_set_invert_alpha (png);

	i = 1;
	if (((char *) &i)[0] == 0) {
		/* Big-endian machine -- ARGB. */
		png_set_swap_alpha (png);
	} else {
		/* Little-endian machine -- BGRA */
		png_set_bgr (png);
	}
	/*}}}*/

	/*{{{  write image data */
	rows = malloc (height * sizeof *rows);
	for (i = 0; i < height; i++) {
		rows[i] = (png_bytep) &raster[width * i];
	}
	png_write_image (png, rows);
	free (rows);

	png_write_end (png, NULL);
	/*}}}*/

	*rc = 0;

out:
	/*{{{  clean up */
	if (png != NULL)
		png_destroy_write_struct (&png, &info);
	if (f != NULL)
		fclose (f);
	/*}}}*/
}
/*}}}*/
