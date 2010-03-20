
#include "config.h"

#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#ifdef HAVE_ASSERT_H
#include <assert.h>
#else /* !HAVE_ASSERT_H */
/* FIXME: more useful definition of assert */
static void assert (int b) {
}
#endif /* !HAVE_ASSERT_H */

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

typedef enum {
	NULL_BRICK	= 0,
	LEGO_NXT	= 1,
	LEGO_RCX	= 2,
	UNKNOWN_BRICK	= 3
} brick_type_t;

typedef struct _brick_t brick_t;
struct _brick_t {
	brick_type_t	type;
	uint16_t	vendor;
	uint16_t	product;
	void		*handle;
	void		(*release)(brick_t *);
};

void *init_usb (void);
brick_t *find_usb_devices (void *usb, 
	int32_t vendor, int32_t product,
	int32_t configuration, int32_t interface,
	brick_type_t type);
void free_usb (void *usb);


