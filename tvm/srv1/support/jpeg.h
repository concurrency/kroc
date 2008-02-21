
#define        FOUR_ZERO_ZERO	0
#define        FOUR_TWO_ZERO	1
#define        FOUR_TWO_TWO	2
#define        FOUR_FOUR_FOUR	3
#define        RGB		4

unsigned char *encode_image (unsigned char *input, unsigned char *output, unsigned int quality, unsigned int width, unsigned int height);
//output_end = encode_image((unsigned char *)0x01800000, output_start, quality, FOUR_TWO_TWO, 320, 256); 

