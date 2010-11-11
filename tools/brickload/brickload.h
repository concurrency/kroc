
#include "config.h"

#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif /* HAVE_CTYPE_H */

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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* brick_t */
typedef enum _brick_type_t {
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
	int		in_ep;
	int		out_ep;
	int		ep_type;

	int		(*get_config)(brick_t *);
	int		(*set_config)(brick_t *, int configuration);

	int		(*open)(brick_t *);
	int		(*close)(brick_t *);
	
	int		(*control)(brick_t *, int req_type, int req, int value, int index, uint8_t *data, size_t len, uint32_t timeout_ms);
	int		(*read)(brick_t *, uint8_t *data, size_t len, uint32_t timeout_ms);
	int		(*write)(brick_t *, const uint8_t *data, size_t len, uint32_t timeout_ms);

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


/* TBC defines */
typedef enum _tbc_type_t {
	TBC_UNKNOWN	= 0,
	TBC_16BIT	= 1,
	TBC_32BIT	= 2
} tbc_type_t;
typedef struct _tbc_t tbc_t;
struct _tbc_t {
	tbc_type_t	type;
	uint8_t 	*data;
	size_t		len;
};


/* TBC functions */
tbc_t *load_tbc (const char *fn);


/* RCX defines */
typedef struct _rcx_firmware_t rcx_firmware_t;
struct _rcx_firmware_t {
	uint32_t	addr;
	uint32_t	start_addr;
	uint8_t		*data;
	size_t		len;
};


/* RCX functions */
void configure_rcx_towers (void *usb);
int send_tbc_to_rcx (brick_t *b, tbc_t *tbc);
int get_rcx_version_str (brick_t *b, char *str);
rcx_firmware_t *load_rcx_firmware (const char *fn);
int boot_rcx (brick_t *b, rcx_firmware_t *fw);

/* NXT defines */
typedef struct _nxt_firmware_t nxt_firmware_t;
struct _nxt_firmware_t {
	uint32_t	in_ram;
	uint32_t	in_rom;
	uint32_t	write_addr;
	uint32_t	boot_addr;
	uint8_t		*data;
	size_t		len;
};


/* NXT functions */
nxt_firmware_t *load_nxt_firmware (const char *fn);
int boot_nxt (brick_t *b, nxt_firmware_t *fw);
int flash_nxt (brick_t *b, nxt_firmware_t *fw);
int send_tbc_to_nxt (brick_t *b, tbc_t *tbc);

