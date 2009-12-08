/*
	rasterio: libpng interface
	Copyright (C) 2007, 2009  Adam Sampson <ats@offog.org>

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

/*{{{  terminate*/
static char *terminate (const char *s, int len)
{
	char *buf = malloc (len + 1);

	if (buf == NULL)
		return NULL;

	memcpy (buf, s, len);
	buf[len] = '\0';
	
	return buf;
}
/*}}}*/

/*{{{  read_raster_png_state */
/* We have to split read_raster_png into two parts so we can do the mobile
   allocation from occam -- so this structure saves the internal state. */
typedef struct {
	int stage1_ok;
	FILE *f;
	png_structp png;
	png_infop info;
} read_raster_png_state;
/*}}}*/

/*{{{  _read_raster_png_1 */
/* #PRAGMA EXTERNAL "PROC C.read.raster.png.1 (VAL []BYTE filename, RESULT INT height, width, magic) = 0" */
void _read_raster_png_1 (int *w)
{
	char *filename = terminate ((const char *) w[0], w[1]);
	int *height = (int *) w[2];
	int *width = (int *) w[3];
	read_raster_png_state **magic = (read_raster_png_state **) w[4];

	read_raster_png_state *state;
	png_uint_32 uwidth, uheight;
	int bit_depth, color_type, interlace_method, compression_method, filter_method;
	int i;

	*height = 0;
	*width = 0;
	*magic = NULL;

	/*{{{  initialise state */
	state = malloc (sizeof *state);
	if (state == NULL)
		return;
	*magic = state;
	state->stage1_ok = 0;
	state->f = NULL;
	state->png = NULL;
	state->info = NULL;
	/*}}}*/

	/*{{{  open the file */
	if (filename == NULL)
		return;
	state->f = fopen (filename, "rb");
	free (filename);
	if (state->f == NULL)
		return;
	/*}}}*/

	/*{{{  set up libpng */
	state->png = png_create_read_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (state->png == NULL)
		return;

	state->info = png_create_info_struct (state->png);
	if (state->info == NULL)
		return;

	if (setjmp (png_jmpbuf (state->png)))
		return;

	png_init_io (state->png, state->f);
	/*}}}*/

	/*{{{  read header */
	png_read_info (state->png, state->info);
	png_get_IHDR (state->png, state->info, &uwidth, &uheight, &bit_depth, &color_type,
	              &interlace_method, &compression_method, &filter_method);
	*width = uwidth;
	*height = uheight;
	/*}}}*/

	/*{{{  set up transformations to #AARRGGBB */
	png_set_palette_to_rgb (state->png);
	png_set_strip_16 (state->png);
	png_set_invert_alpha (state->png);
	png_set_packing (state->png);
	png_set_gray_to_rgb (state->png);

	i = 1;
	if (((char *) &i)[0] == 0) {
		/* Big-endian machine -- ARGB. */
		png_set_filler (state->png, 0, PNG_FILLER_BEFORE);
		png_set_swap_alpha (state->png);
	} else {
		/* Little-endian machine -- BGRA. */
		png_set_filler (state->png, 0, PNG_FILLER_AFTER);
		png_set_bgr (state->png);
	}
	/* FIXME: Do gamma correction? */
	/*}}}*/

	state->stage1_ok = 1;
}
/*}}}*/

/*{{{  _read_raster_png_2 */
/* #PRAGMA EXTERNAL "PROC C.read.raster.png.2 (VAL INT magic, [][]INT raster, INT rc) = 0" */
void _read_raster_png_2 (int *w)
{
	read_raster_png_state *state = (read_raster_png_state *) w[0];
	int *raster = (int *) w[1];
	int height = w[2];
	int width = w[3];
	int *rc = (int *) w[4];

	png_bytep *rows;
	int i;

	*rc = -1;
	if (state == NULL || !state->stage1_ok)
		goto out;

	/*{{{  read image data */
	rows = malloc (height * sizeof *rows);
	for (i = 0; i < height; i++) {
		rows[i] = (png_bytep) &raster[width * i];
	}
	png_read_image (state->png, rows);
	free (rows);
	/*}}}*/

	*rc = 0;

out:
	/*{{{  clean up */
	if (state != NULL) {
		if (state->png != NULL)
			png_destroy_read_struct (&state->png, &state->info, NULL);
		if (state->f != NULL)
			fclose (state->f);
		free (state);
	}
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
	if (filename == NULL)
		goto out;
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
