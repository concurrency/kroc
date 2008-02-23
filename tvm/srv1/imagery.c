/*
 * imagery.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

#include <font8x8.h>
#include <jpeg.h>

/* PROC draw.caption.on.frame (VAL INT frame.width, VAL []BYTE caption, []BYTE frame) */
int draw_caption_on_frame (ECTX ectx, WORD args[])
{
	WORD	width		= args[0];
	BYTEPTR	caption		= (BYTEPTR) args[1];
	WORD	caption_len	= args[2];
	BYTE	*frame		= (BYTE *) wordptr_real_address ((WORDPTR) args[3]);
	/* WORD	frame_len	= args[4]; */
	int 	ix, iy, iz;
	
	/* Limit caption length */
	if (caption_len > 40) {
		caption_len = 40;
	}

	/* Move to first character position */
	frame = frame + (((width * 16) + width) - (caption_len * 8));

	for (ix = 0; ix < caption_len; ix++) {
		unsigned int c = (unsigned int) read_byte (byteptr_plus (caption, ix));
		BYTE *fcur = frame;

		for (iy = 0; iy < 8; iy++) {
			BYTE cc = font8x8[(c * 8) + iy];

			for (iz = 0; iz < 8; iz++) {
				if (cc & fontmask[iz]) {
					fcur[0] = 0x80;
					fcur[1] = 0xff;
				}
				fcur += 2;
			}
			
			/* Move to next line */
			fcur += (width * 2) - 16;
		}
		
		/* Move to next character */
		frame += 16;
	}

	return SFFI_OK;
}


/* PROC jpeg.encode.frame (VAL INT width, height, quality, 
 * 			VAL []BYTE input, []BYTE output, INT used) */
int jpeg_encode_frame (ECTX ectx, WORD args[])
{
	WORD	width		= args[0];
	WORD	height		= args[1];
	WORD	quality		= args[2];
	BYTEPTR	input		= (BYTEPTR) args[3];
	WORD	input_len	= args[4];
	BYTEPTR	output		= (BYTEPTR) args[5];
	WORD	output_len	= args[6];
	WORDPTR	used		= (WORDPTR) args[7];
	BYTEPTR end;

	if (quality < 1) {
		quality = 1;
	} else if (quality > 8) {
		quality = 8;
	}

	/* Input buffer must be big enough to be a frame */
	/* Output buffer must be at least as 1/4 of the input buffer */
	if ((((width * height) << 1) > input_len) || (output_len < (input_len >> 4))) {
		/* Bad buffer sizes, return -1 */
		write_word (used, -1);
	} else {
		input	= (BYTEPTR) wordptr_real_address ((WORDPTR) input);
		output 	= (BYTEPTR) wordptr_real_address ((WORDPTR) output);

		end = (BYTEPTR) encode_image (
			(unsigned char *) input,
			(unsigned char *) output,
			quality,
			width,
			height
		);
		/* Return output size */
		write_word (used, ((WORD) end) - ((WORD) output));
	}

	return SFFI_OK;
}

