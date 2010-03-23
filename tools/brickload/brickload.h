
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


/* brick_t */
typedef enum {
	NULL_BRICK	= 0x0000,
	LEGO_NXT	= 0x0001,
	LEGO_RCX	= 0x0002,
	UNKNOWN_BRICK	= 0x0003,
	BRICK_TYPE_MASK	= 0x00ff,

	NXOS_BRICK	= 0x0100,
	SAMBA_BRICK	= 0x0200,
	BRICK_FLAG_MASK	= 0xff00
} brick_type_t;

typedef struct _brick_t brick_t;
struct _brick_t {
	brick_type_t	type;
	uint32_t	id;

	void		*handle;
	void		*state;
	int		in_ep;
	int		out_ep;

	int		(*get_config)(brick_t *);
	int		(*set_config)(brick_t *, int configuration);

	int		(*open)(brick_t *);
	int		(*close)(brick_t *);
	
	int		(*read)(brick_t *, uint8_t *data, size_t len, uint32_t timeout_ms);
	int		(*write)(brick_t *, uint8_t *data, size_t len, uint32_t timeout_ms);

	void		(*release)(brick_t *);
};


/* Brick Lists */

brick_t *merge_brick_lists (brick_t *a, brick_t *b);
void free_brick_list (brick_t *list);


/* USB defines */
#define	LEGO_VENDOR_ID 		0x0694
#define LEGO_PRODUCT_TOWER	0x0001
#define LEGO_PRODUCT_NXT	0x0002
#define LEGO_PRODUCT_NXOS	0xff00
#define ATMEL_VENDOR_ID		0x03eb
#define ATMEL_PRODUCT_SAMBA	0x6124
#define SAMBA_INTERFACE		0x1


/* USB functions */
void *init_usb (void);
brick_t *find_usb_devices (void *usb, 
	int32_t vendor, int32_t product,
	int32_t configuration, int32_t interface,
	brick_type_t type);
void free_usb (void *usb);


/* RCX functions */
void configure_rcx_towers (void *usb);


/* NXT defines */
typedef struct _nxt_firmware_t nxt_firmware_t;
struct _nxt_firmware_t {
	uint32_t	in_ram;
	uint32_t	in_rom;
	uint32_t	write_addr;
	uint32_t	boot_addr;
	size_t		len;
	uint8_t		*data;
};


/* NXT functions */
nxt_firmware_t *load_nxt_firmware (const char *fn);
int boot_nxt (brick_t *b, nxt_firmware_t *fw);

