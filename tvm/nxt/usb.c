/*
 * usb.c - NXT TVM USB interface
 *
 * Copyright (C) 2007  the NxOS developers (see NXOS)
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

#include "tvm-nxt.h"
#include "at91sam7s256.h"

#ifndef MIN
#define MIN(x,y)  (((x) <= (y)) ? (x) : (y))
#endif /* MIN */

/* The USB controller supports up to 4 endpoints. */
#define N_ENDPOINTS 4

/* Maximum data packet sizes. Endpoint 0 is a special case (control
 * endpoint).
 */
#define MAX_EP0_SIZE 8
#define MAX_RCV_SIZE 64
#define MAX_SND_SIZE 64


/* Various constants for the setup packets.
 */
#define USB_BMREQUEST_DIR		0x80
#define USB_BMREQUEST_H_TO_D		0x00
#define USB_BMREQUEST_D_TO_H		0x80

#define USB_BMREQUEST_RCPT		0x0F
#define USB_BMREQUEST_RCPT_DEV		0x00 /* device */
#define USB_BMREQUEST_RCPT_INT		0x01 /* interface */
#define USB_BMREQUEST_RCPT_EPT		0x02 /* endpoint */
#define USB_BMREQUEST_RCPT_OTH		0x03 /* other */

#define USB_BMREQUEST_CLASS		0x20

#define USB_BREQUEST_GET_STATUS		0x00
#define USB_BREQUEST_CLEAR_FEATURE	0x01
#define USB_BREQUEST_SET_FEATURE	0x03
#define USB_BREQUEST_SET_ADDRESS	0x05
#define USB_BREQUEST_GET_DESCRIPTOR	0x06
#define USB_BREQUEST_SET_DESCRIPTOR	0x07
#define USB_BREQUEST_GET_CONFIG		0x08
#define USB_BREQUEST_SET_CONFIG		0x09
#define USB_BREQUEST_GET_INTERFACE	0x0A
#define USB_BREQUEST_SET_INTERFACE	0x0B

#define USB_BREQUEST_BULK_RESET		0xFF
#define USB_BREQUEST_GET_MAX_LUN	0xFE

#define USB_WVALUE_TYPE			(0xFF << 8)
#define USB_DESC_TYPE_DEVICE		1
#define USB_DESC_TYPE_CONFIG		2
#define USB_DESC_TYPE_STR		3
#define USB_DESC_TYPE_INT		4
#define USB_DESC_TYPE_ENDPT		5
#define USB_DESC_TYPE_DEVICE_QUALIFIER	6

#define USB_WVALUE_INDEX		0xFF

#define USB_FEAT_ENDPOINT_HALT		0

#define MSD_DATA_EP_IN			1
#define MSD_DATA_EP_OUT			2
#define MSD_FLAG_DEV_TO_HOST		0x01
#define MSD_FLAG_READ_ONLY		0x80
#define MSD_CBW_LEN			31
#define MSD_CSW_LEN			13
#define MSD_STATUS_CMD_PASSED		0x00
#define MSD_STATUS_CMD_FAILED		0x01
#define MSD_STATUS_PHASE_ERROR		0x02
#define MSD_STATUS_DATA_PHASE		0x03
#define MSD_STATUS_CSW_SENT		0x04
#define MSD_STATUS_STALL		0x05

#define MSD_BLOCK_SIZE			256

#define SCSI_CMD_TEST_UNIT_READY	0x00
#define SCSI_CMD_REQUEST_SENSE		0x03
#define SCSI_CMD_READ6			0x08
#define SCSI_CMD_WRITE6			0x0a
#define SCSI_CMD_INQUIRY		0x12
#define SCSI_CMD_MODE_SENSE6		0x1a
#define SCSI_CMD_START_STOP_UNIT	0x1b
#define SCSI_CMD_PREVENT_REMOVAL	0x1e
#define SCSI_CMD_READ_CAPACITY10	0x25
#define SCSI_CMD_READ10			0x28
#define SCSI_CMD_WRITE10		0x2a
#define SCSI_CMD_VERIFY10		0x2f
#define SCSI_CMD_REPORT_LUNS		0xa0

#define SCSI_INQUIRY_MAX_LEN		96

#define SCSI_SENSE_KEY_NO_SENSE		0
#define SCSI_SENSE_KEY_SOFT_ERROR	1
#define SCSI_SENSE_KEY_NOT_READY	2
#define SCSI_SENSE_KEY_MEDIUM_ERROR	3
#define SCSI_SENSE_KEY_HARDWARE_ERROR	4
#define SCSI_SENSE_KEY_ILLEGAL_REQUEST	5
#define SCSI_SENSE_KEY_UNIT_ATTENTION	6
#define SCSI_SENSE_KEY_DATA_PROTECT	7
#define SCSI_SENSE_KEY_ABORTED_COMMAND	8

#define SCSI_SENSE_CODE_INVALID_FIELD_IN_CDB			0x24
#define SCSI_SENSE_CODE_LOGICAL_BLOCK_ADDRESS_OUT_OF_RANGE	0x21
#define SCSI_SENSE_CODE_WRITE_PROTECTED				0x27

/* The following definitions are 'raw' USB setup packets. They are all
 * standard responses to various setup requests by the USB host. These
 * packets are all constant, and mostly boilerplate. Don't be too
 * bothered if you skip over these to real code.
 *
 * If you want to understand the full meaning of every bit of these
 * packets, you should refer to the USB 2.0 specifications.
 *
 * One point of interest: the USB device space is partitionned by
 * vendor and product ID. As we are lacking money and real need, we
 * don't have a vendor ID to use. Therefore, we are currently
 * piggybacking on Lego's device space, using an unused product ID.
 */
static const uint8_t usb_device_descriptor[] = {
	18, USB_DESC_TYPE_DEVICE, /* Packet size and type. */
	0x00, 0x20, /* This packet is USB 2.0. */
	0, /* Class code. */
	0, /* Sub class code. */
	0, /* Device protocol. */
	MAX_EP0_SIZE, /* Maximum packet size for EP0 (control endpoint). */
	0x94, 0x06, /* Vendor ID : LEGO */
	0x00, 0xFF, /* Product ID : NXOS */
	0x00, 0x00, /* Product revision. */
	1, /* Index of the vendor string. */
	2, /* Index of the product string. */
	0, /* Index of the serial number (none for us). */
	1, /* The number of possible configurations. */
};

static const uint8_t usb_dev_qualifier_desc[] = {
	10, USB_DESC_TYPE_DEVICE_QUALIFIER, /* Packet size and type. */
	0x00, 0x20, /* This packet is USB 2.0. */
	0, /* Class code */
	0, /* Sub class code */
	0, /* Device protocol */
	MAX_EP0_SIZE, /* Maximum packet size for EP0. */
	1, /* The number of possible configurations. */
	0 /* Reserved for future use, must be zero. */
};


static const uint8_t usb_full_config[] = {
	0x09, USB_DESC_TYPE_CONFIG, /* Descriptor size and type. */
	0x20, 0x00, /* Total length of the configuration, interface
		     * description included.
		     */
	1, /* The number of interfaces declared by this configuration. */
	1, /* The ID for this configuration. */
	0, /* Index of the configuration description string (none). */

	/* Configuration attributes bitmap. Bit 7 (MSB) must be 1, bit 6 is
	 * 1 because the NXT is self-powered, bit 5 is 0 because the NXT
	 * doesn't support remote wakeup, and bits 0-4 are 0 (reserved).
	 */
	0x40,
	0, /* Device power consumption, for non self-powered devices. */


	/*
	 * This is the descriptor for the interface associated with the
	 * configuration.
	 */
	0x09, USB_DESC_TYPE_INT, /* Descriptor size and type. */
	0x00, /* Interface index. */
	0x00, /* ID for this interface configuration. */
	0x02, /* The number of endpoints defined by this interface
	       * (excluding EP0).
	       */
	0x08, /* Interface class ("MASS STORAGE"). */
	0x06, /* Interface subclass ("SCSI transparent"). */
	0x50, /* Interface protocol ("BULK-ONLY TRANSPORT"). */
	0x00, /* Index of the string descriptor for this interface (none). */


	/*
	 * Descriptor for EP1.
	 */
	7, USB_DESC_TYPE_ENDPT, /* Descriptor length and type. */
	0x00 | MSD_DATA_EP_IN, /* Endpoint number. MSB is zero, meaning this is an OUT EP. */
	0x02, /* Endpoint type (bulk). */
	MAX_RCV_SIZE, 0x00, /* Maximum packet size (64). */
	0, /* EP maximum NAK rate (device never NAKs). */


	/*
	 * Descriptor for EP2.
	 */
	7, USB_DESC_TYPE_ENDPT, /* Descriptor length and type. */
	0x80 | MSD_DATA_EP_OUT, /* Endpoint number. MSB is one, meaning this is an IN EP. */
	0x02, /* Endpoint type (bulk). */
	MAX_SND_SIZE, 0x00, /* Maximum packet size (64). */
	0  /* EP maximum NAK rate (device never NAKs). */
};


static const uint8_t usb_string_desc[] = {
	4, USB_DESC_TYPE_STR, /* Descriptor length and type. */
	0x09, 0x04, /* Supported language ID (US English). */
};

static const uint8_t usb_lego_str[] = {
	10, USB_DESC_TYPE_STR,
	'L', 0,
	'E', 0,
	'G', 0,
	'O', 0
};

static const uint8_t usb_tvm_str[] = {
	16, USB_DESC_TYPE_STR,
	'N', 0,
	'X', 0,
	'T', 0,
	' ', 0,
	'T', 0,
	'V', 0,
	'M', 0
};

/* Internal lookup table mapping string descriptors to their indices
 * in the USB string descriptor table.
 */
static const uint8_t *usb_strings[] = {
	usb_lego_str,
	usb_tvm_str
};
static const uint8_t usb_string_count = 
	(uint8_t) (sizeof (usb_strings) / sizeof (uint8_t *));


/* Precomputed SCSI response. */
static const uint8_t scsi_standard_inquiry[] = {
	0x00,	/* 000b = Peripheral Device, 00h = SBC-3 */
	0x00,	/* No Removable Media */
	0x06,	/* Supports SPC-4 */
	0x02,	/* No ACA, No HiSup, Response Format = 2 */
	32,	/* Remaining Length */
	0x00, 0x00, 0x00, 			/* No flags set */
	'L', 'E', 'G', 'O', ' ', ' ', ' ', ' ', /* Vendor ID */
	'M', 'I', 'N', 'D', 'S', 'T', 'O', 'R', /* Product ID */
	'M', 'S', ' ', 'N', 'X', 'T', ' ', ' ', 
	'T', 'V', 'M', ' '			/* Revision ID */
};

static const uint8_t scsi_evpd_page_00[] = {
	0x00,	/* 000b = Peripheral Device, 00h = SBC-3 */
	0x00,	/* Page Code 00 */
	0x00,	/* Reserved */
	2,	/* Number of Pages Supported */
	0x00,	/* Mandatory Page 00 */
	0x83	/* Mandatory Page 83 */
};

static const uint8_t scsi_evpd_page_83[] = {
	0x00,		/* 000b = Peripheral Device, 00h = SBC-3 */
	0x83,		/* Page Code 83 */
	0x00, 0x01, 	/* 1 designator follows */

	0x02,		/* Code Set ASCII */
	0x00,		/* Vendor Specific ID */
	0x00,		/* Reserved */
	0x04,		/* 4 bytes */
	'L', 'E', 'G', 'O'
};


/*
 * The USB device state. Contains the current USB state (selected
 * configuration, etc.) and transitory state for data transfers.
 */
static volatile struct {
	/* The current state of the device. */
	enum usb_status {
		USB_UNINITIALISED = 0,
		USB_READY,
		USB_BUSY,
		USB_SUSPENDED,
	} status;

	/* Holds the status the bus was in before entering suspend. */
	enum usb_status pre_suspend_status;

	/* When the host gives us an address, we must send a null ACK packet
	 * back before actually changing addresses. This field stores the
	 * address that should be set once the ACK is sent.
	 */
	uint32_t new_device_address;

	/* The currently selected USB configuration. */
	uint8_t current_config;
	
	/* Bitmap of endpoint halt bits */
	uint8_t halted;

	/* Holds the state of the data transmissions on endpoints.
	 * This only gets used if the transmission needed to be split
	 * into several USB packets.
	 * 0 = EP0
	 * 1 = EP2
	 */
	uint8_t *tx_data[2];
	uint32_t tx_len[2];

	/* Used to write the data from the EP1
	*/
	uint8_t *rx_data;
	/* size of the rx data buffer */
	uint32_t rx_size;
	/* length of data in buffer (0 if none) */
	uint32_t rx_len;
	/* length of pending data in FIFO */
	uint16_t rx_pending;

	/* The USB controller has two hardware input buffers. This remembers
	 * the one currently in use.
	 */
	uint8_t current_rx_bank;
} usb_state;

/*
 * Mass Storage Driver state. 
 */

static volatile struct {
	/* The current state of the device. */
	enum msd_status {
		MSD_UNINITIALISED 	= 0,
		MSD_WAIT_RESET		= 1,
		MSD_RESET		= 2,
		MSD_WAIT_CBW		= 3,
		MSD_DATA_RX		= 4,
		MSD_DATA_TX		= 5,
		MSD_WAIT_CLEAR_HALT	= 6
	} status;

	/* Backing store for the storage drive. */
	uint8_t *data;
	uint32_t data_len;

	/* Command state */
	uint8_t flags;
	uint8_t cmd_status;
	uint8_t scsi_sense_key;
	uint8_t scsi_sense_code;
	uint8_t scsi_sense_qualifier;
	uint8_t pad[3];
	uint32_t tag;
	uint32_t transfer_len;

	union {
		/* Command Block Wrapper (CBW) buffer. */
		uint8_t cbw[MSD_CBW_LEN];

		/* Command Status Wrapper (CSW) buffer. */
		uint8_t csw[MSD_CSW_LEN];

		/* SCSI Inquiry / Request Sense Response */
		uint8_t inquiry[SCSI_INQUIRY_MAX_LEN];
	} buf;

} msd_state;

/* The flags in the UDP_CSR register are a little strange: writing to
 * them does not instantly change their value. Their value will change
 * to reflect the write when the USB controller has taken the change
 * into account. The driver must wait until the controller
 * acknowledges changes to CSR.
 *
 * These helpers set/clear CSR flags, and then loop waiting for the
 * controller to synchronize
 */
static inline void usb_csr_clear_flag (uint8_t endpoint, uint32_t flags)
{
	AT91C_UDP_CSR[endpoint] &= ~flags;
	while (AT91C_UDP_CSR[endpoint] & flags);
}

static inline void usb_csr_set_flag (uint8_t endpoint, uint32_t flags)
{
	AT91C_UDP_CSR[endpoint] |= flags;
	while ((AT91C_UDP_CSR[endpoint] & flags) != flags);
}

static inline void usb_csr_set_value (uint8_t endpoint, uint32_t value)
{
	AT91C_UDP_CSR[endpoint] = value;
	while (AT91C_UDP_CSR[endpoint] != value);
}

/* Starts sending data to the host. If the data cannot fit into a
 * single USB packet, the data is split and scheduled to be sent in
 * several packets.
 */
static void usb_write_data (int endpoint, const uint8_t *ptr, uint32_t length)
{
	uint32_t packet_size;
	int tx;

	if (endpoint != 0 && endpoint != 2)
		return;

	tx = endpoint / 2;

	/* Acknowledge any existing transmission. */
	//usb_csr_clear_flag (endpoint, AT91C_UDP_TXCOMP);

	/* The bus is now busy. */
	usb_state.status = USB_BUSY;

	if (endpoint == 0)
		packet_size = MIN (MAX_EP0_SIZE, length);
	else
		packet_size = MIN (MAX_SND_SIZE, length);

	/* If there is more data than can fit in a single packet, queue the
	 * rest up.
	 */
	if (length > packet_size) {
		length -= packet_size;
		usb_state.tx_data[tx] = (uint8_t*)(ptr + packet_size);
		usb_state.tx_len[tx] = length;
	} else {
		usb_state.tx_data[tx] = NULL;
		usb_state.tx_len[tx] = 0;
	}
	
	/* Push a packet into the USB FIFO, and tell the controller to send. */
	while (packet_size) {
		AT91C_UDP_FDR[endpoint] = *ptr;
		ptr++;
		packet_size--;
	}
	usb_csr_set_flag (endpoint, AT91C_UDP_TXPKTRDY);
}

static inline int usb_cont_write (int endpoint)
{
	int buf_n = endpoint / 2;

	if (usb_state.tx_len[buf_n] > 0 
			&& usb_state.tx_data[buf_n] != NULL) {
		//debug_msg ("CW1 ", endpoint);
		usb_write_data (
			endpoint,
			usb_state.tx_data[buf_n],
			usb_state.tx_len[buf_n]
		);
		return 1;
	} else {
		//debug_msg ("CW0 ", endpoint);
		return 0;
	}
}

/* Setup read on USB endpoint.
 */
static void usb_read_start (int endpoint, uint8_t *buf, uint32_t len)
{
	usb_state.rx_data = buf;
	usb_state.rx_size = len;
	usb_state.rx_len = 0;
	usb_csr_set_flag (endpoint, AT91C_UDP_EPEDS);
}

/* Read data from the USB controller in buffers.
 */
static void usb_read_data (int endpoint)
{
	uint32_t len, size;
	uint16_t total;
	uint8_t *ptr;

	/* Given our configuration, we should only be getting packets on
	 * endpoint 1. Ignore data on any other endpoint.
	 * (note: data from EP0 are managed by usb_manage_setup())
	 */
	if (endpoint != 1 || (usb_state.halted & (1 << endpoint))) {
		usb_csr_clear_flag (
			endpoint, 
			AT91C_UDP_RX_DATA_BK0 | AT91C_UDP_RX_DATA_BK1
		);
		return;
	}

	total = (AT91C_UDP_CSR[1] & AT91C_UDP_RXBYTECNT) >> 16;

	/* we start reading */
	/* all the bytes will be put in rx_data */
	len = usb_state.rx_len;
	size = usb_state.rx_size;
	ptr = &(usb_state.rx_data[len]);
	do {
		if (len < size) {
			uint8_t byte = AT91C_UDP_FDR[1];
			*(ptr++) = byte;
			len++;
			total--;
		} else {
			break;
		}
	} while (total > 0);
	
	usb_state.rx_len = len;
	usb_state.rx_pending = total;

	if (len == size) {
		/* If there is no buffer space left then disable the endpoint. */
		usb_csr_clear_flag (1, AT91C_UDP_EPEDS);
	}

	if (total == 0) {
		/* if we have read all the data ... */
		/* Acknowledge reading the current RX bank, and switch to the other. */
		usb_csr_clear_flag (1, usb_state.current_rx_bank);
		
		if (usb_state.current_rx_bank == AT91C_UDP_RX_DATA_BK0)
			usb_state.current_rx_bank = AT91C_UDP_RX_DATA_BK1;
		else
			usb_state.current_rx_bank = AT91C_UDP_RX_DATA_BK0;
	}
}


/* On the endpoint 0: A stall is USB's way of sending
 * back an error (either "not understood" or "not handled
 * by this device"). The connexion will be reinitialized
 * by the host.
 * On the other endpoint : Indicates to the host that the endpoint is halted
 */
static void usb_send_stall (int endpoint)
{
	if (endpoint == 0) {
		usb_state.status = USB_UNINITIALISED;
	}
	usb_csr_set_flag (endpoint, AT91C_UDP_FORCESTALL);
}

static void usb_set_halt (int endpoint)
{
	usb_state.halted |= 1 << endpoint;
	usb_send_stall (endpoint);
}

static void usb_clear_halt (int endpoint)
{
	usb_state.halted &= ~(1 << endpoint);
	usb_csr_clear_flag (endpoint, AT91C_UDP_FORCESTALL | AT91C_UDP_RX_DATA_BK0 | AT91C_UDP_RX_DATA_BK1); 
	*AT91C_UDP_RSTEP = (1 << endpoint);
	*AT91C_UDP_RSTEP = 0;
	if (endpoint == 1) {
		usb_csr_clear_flag (endpoint, AT91C_UDP_EPEDS);
		usb_state.current_rx_bank = AT91C_UDP_RX_DATA_BK0;
	}
}

/* During setup, we need to send packets with null data. */
static void usb_send_null (void)
{
	usb_write_data (0, NULL, 0);
}

static void msd_wait_cbw (void)
{
	msd_state.status = MSD_WAIT_CBW;
	usb_read_start (MSD_DATA_EP_IN, (uint8_t *) msd_state.buf.cbw, MSD_CBW_LEN);
}

static int msd_send_csw (uint8_t status)
{
	uint8_t *buf			= (uint8_t *) msd_state.buf.csw;

	*((uint32_t *)&(buf[0]))	= 0x53425355;
	*((uint32_t *)&(buf[4]))	= msd_state.tag;
	*((uint32_t *)&(buf[8]))	= msd_state.transfer_len;
	buf[12]				= status;

	if (msd_state.transfer_len > 0) {
		msd_state.status = MSD_WAIT_CLEAR_HALT;
		if (!(msd_state.flags & MSD_FLAG_DEV_TO_HOST))
			usb_set_halt (MSD_DATA_EP_IN);
		usb_set_halt (MSD_DATA_EP_OUT);
		debug_msg ("STL ", 0);
		return MSD_STATUS_STALL;
	} else {
		usb_write_data (MSD_DATA_EP_OUT, (uint8_t *) msd_state.buf.csw, MSD_CSW_LEN);
		debug_msg ("CSW ", status);
		msd_wait_cbw ();
		return MSD_STATUS_CSW_SENT;
	}
}

static void msd_cleared_halt (void)
{
	//debug_msg ("CLR ", msd_state.status);
	switch (msd_state.status) {
		case MSD_WAIT_CLEAR_HALT:
			usb_write_data (MSD_DATA_EP_OUT, (uint8_t *) msd_state.buf.csw, MSD_CSW_LEN);
			debug_msg ("CSW ", 0xffffffff);
			msd_wait_cbw ();
			break;
		case MSD_RESET:
			msd_wait_cbw ();
			break;
		default:
			break;
	}
}

static void msd_reset (void)
{
	debug_msg ("RST ", usb_state.halted);
	if (usb_state.halted == 0) {
		msd_wait_cbw ();
	} else {
		msd_state.status = MSD_RESET;
	}
}

static int msd_send_data (const uint8_t *data, uint32_t len)
{
	if ((msd_state.flags & MSD_FLAG_DEV_TO_HOST) && (len <= msd_state.transfer_len)) {
		msd_state.status = MSD_DATA_TX;
		usb_write_data (MSD_DATA_EP_OUT, data, len);
		msd_state.transfer_len -= len;
		debug_msg ("TX  ", len);
		return MSD_STATUS_DATA_PHASE;
	} else {
		return msd_send_csw (MSD_STATUS_PHASE_ERROR);
	}
}

static int msd_recv_data (uint8_t *data, uint32_t len)
{
	if (!(msd_state.flags & MSD_FLAG_DEV_TO_HOST) && (len <= msd_state.transfer_len)) {
		msd_state.status = MSD_DATA_RX;
		usb_read_start (MSD_DATA_EP_IN, data, len);
		msd_state.transfer_len -= len;
		debug_msg ("RX  ", len);
		return MSD_STATUS_DATA_PHASE;
	} else {
		return msd_send_csw (MSD_STATUS_PHASE_ERROR);
	}
}

static inline void msd_set_sense (uint8_t key, uint8_t code, uint8_t qual)
{
	msd_state.scsi_sense_key	= key;
	msd_state.scsi_sense_code	= code;
	msd_state.scsi_sense_qualifier	= qual;
}

static int msd_send_error (uint8_t key, uint8_t code, uint8_t qual)
{
	msd_set_sense (key, code, qual);
	return msd_send_csw (MSD_STATUS_CMD_FAILED);
}

static int scsi_test_unit_ready (const uint8_t *cmd)
{
	/* 1-4	= Reserved */
	/* 5	= Control */
	if ((cmd[1] | cmd[2] | cmd[3] | cmd[4]) == 0) {
		return MSD_STATUS_CMD_PASSED;
	} else {
		return MSD_STATUS_CMD_FAILED;
	}
}

static int scsi_request_sense (const uint8_t *cmd)
{
	/* 1	= DESC(0) */
	/* 2-3	= Reserved */
	/* 4	= Allocation Length */
	/* 5	= Control */
	if (cmd[1] == 0) {
		uint8_t *desc = (uint8_t *) msd_state.buf.inquiry;
		uint8_t len = cmd[4];

		/* fill buffer */
		memset (desc, 0, SCSI_INQUIRY_MAX_LEN);
		desc[0]		= 0x80 | 0x70; /* valid | current errors */
		desc[2] 	= msd_state.scsi_sense_key;
		desc[7] 	= 18 - 8; /* additional bytes */
		desc[12]	= msd_state.scsi_sense_code;
		desc[13]	= msd_state.scsi_sense_qualifier;

		if (len > SCSI_INQUIRY_MAX_LEN)
			len = SCSI_INQUIRY_MAX_LEN;

		/* reset sense data */
		msd_set_sense (SCSI_SENSE_KEY_NO_SENSE, 0, 0);
		
		/* send fixed sense descriptor */
		return msd_send_data (desc, len);
	} else {
		return MSD_STATUS_CMD_FAILED;
	}
}

static int scsi_inquiry (const uint8_t *cmd)
{
	/* 1	= EVPD(0) */
	/* 2	= Page Code */
	/* 3-4	= Allocation Length */
	/* 5	= Control */
	
	uint16_t length = (cmd[3] << 8) | cmd[4];
	uint8_t *desc = (uint8_t *) msd_state.buf.inquiry;
	int valid = 1;
	
	debug_msg ("INQ ", (cmd[1] << 24) | (cmd[2] << 16) | length);

	if (length > SCSI_INQUIRY_MAX_LEN)
		length = SCSI_INQUIRY_MAX_LEN;
	memset (desc, 0, SCSI_INQUIRY_MAX_LEN);

	if (cmd[1] == 0) {
		/* EVPD = 0 */
		if (cmd[2] == 0) {
			/* Standard Inquiry */
			memcpy (desc, scsi_standard_inquiry, sizeof (scsi_standard_inquiry));
			desc[4] = length - 5; /* adjust the additional length field */
		} else {
			valid = 0;
		}
	} else if (cmd[1] == 1) {
		/* EVPD = 1 */
		if (cmd[2] == 0x00) {
			/* Page 00 */
			memcpy (desc, scsi_evpd_page_00, sizeof (scsi_evpd_page_00));
		} else if (cmd[2] == 0x83) {
			/* Page 83 */
			memcpy (desc, scsi_evpd_page_83, sizeof (scsi_evpd_page_83));
		} else {
			valid = 0;
		}
	}

	if (valid) {
		return msd_send_data (desc, length);
	} else {
		return MSD_STATUS_CMD_FAILED;
	}
}

static int scsi_prevent_removal (const uint8_t *cmd)
{
	/* 1-3	= Reserved */
	/* 4	= Prevent(1-0) */
	/* 5	= Control */
	if ((cmd[1] | cmd[2] | cmd[3] | (cmd[4] & 0xfb)) == 0) {
		return MSD_STATUS_CMD_PASSED;
	} else {
		return MSD_STATUS_CMD_FAILED;
	}
}

static int scsi_read_capacity (const uint8_t *cmd)
{
	/* 1	= Reserved */
	/* 2-5	= LBA */
	/* 6-7	= Reserved */
	/* 8	= PMI(0) */
	/* 9	= Control */
	if ((cmd[1] | cmd[6] | cmd[7]) == 0) {
		uint32_t block_size = MSD_BLOCK_SIZE;
		uint32_t blocks = (msd_state.data_len / block_size) - 1;
		uint32_t lba = (cmd[2] << 24) | (cmd[3] << 16) | (cmd[4] << 8) | cmd[5];
		uint8_t *buf = (uint8_t *) msd_state.buf.inquiry;
		uint8_t pmi = cmd[8] & 1;

		if (pmi && (lba > blocks)) {
			buf[0] = (lba >> 24) & 0xff;
			buf[1] = (lba >> 16) & 0xff;
			buf[2] = (lba >>  8) & 0xff;
			buf[3] = (lba >>  0) & 0xff;
		} else {
			buf[0] = (blocks >> 24) & 0xff;
			buf[1] = (blocks >> 16) & 0xff;
			buf[2] = (blocks >>  8) & 0xff;
			buf[3] = (blocks >>  0) & 0xff;
		}
		
		buf[4] = (block_size >> 24) & 0xff;
		buf[5] = (block_size >> 16) & 0xff;
		buf[6] = (block_size >>  8) & 0xff;
		buf[7] = (block_size >>  0) & 0xff;

		return msd_send_data (buf, 8);
	} else {
		return MSD_STATUS_CMD_FAILED;
	}
}

static int scsi_read (const uint8_t *cmd)
{
	uint32_t lba;
	uint32_t len;
	int valid = 1;
	
	if (cmd[0] == SCSI_CMD_READ6) {
		/* 1-3	= LBA (24 bits) */
		/* 4	= Transfer Length */
		/* 5	= Control */
		valid	&= ((cmd[1] & 0xe0) == 0);
		lba	= ((cmd[1] & 0x1f) << 16) | (cmd[2] << 8) | (cmd[3]);
		len	= cmd[4];
		if (len == 0)
			len = 256;
	} else {
		/* 1	= RDPROTECT(7-5), DPO(4), FUA(3), FUA_NV(1) */
		/* 2-5	= LBA */
		/* 6	= Group Number(4-0) */
		/* 7-8	= Transfer Length */
		/* 9	= Control */
		valid	&= ((cmd[1] & 0xf0) == 0);
		valid	&= (cmd[6] == 0);
		lba	= (cmd[2] << 24) | (cmd[3] << 16) | (cmd[4] << 8) | (cmd[5]);
		len	= (cmd[7] << 8) | (cmd[8]);
	}

	if (valid) {
		debug_msg ("LBA ", lba);
		debug_msg ("LEN ", len);
		lba *= MSD_BLOCK_SIZE;
		len *= MSD_BLOCK_SIZE;
		if ((lba < msd_state.data_len) && ((lba + len) <= msd_state.data_len)) {
			return msd_send_data (msd_state.data + lba, len);
		} else {
			return msd_send_error (
				SCSI_SENSE_KEY_ILLEGAL_REQUEST,
				SCSI_SENSE_CODE_LOGICAL_BLOCK_ADDRESS_OUT_OF_RANGE, 0
			);
		}
	} else {
		return MSD_STATUS_CMD_FAILED;
	}
}

static int scsi_write (const uint8_t *cmd)
{
	uint32_t lba;
	uint32_t len;
	int valid = 1;
	
	if (cmd[0] == SCSI_CMD_WRITE6) {
		/* 1-3	= LBA (24 bits) */
		/* 4	= Transfer Length */
		/* 5	= Control */
		valid	&= ((cmd[1] & 0xe0) == 0);
		lba	= ((cmd[1] & 0x1f) << 16) | (cmd[2] << 8) | (cmd[3]);
		len	= cmd[4];
		if (len == 0)
			len = 256;
	} else {
		/* 1	= WRPROTECT(7-5), DPO(4), FUA(3), FUA_NV(1) */
		/* 2-5	= LBA */
		/* 6	= Group Number(4-0) */
		/* 7-8	= Transfer Length */
		/* 9	= Control */
		valid	&= ((cmd[1] & 0xf0) == 0);
		valid	&= (cmd[6] == 0);
		lba	= (cmd[2] << 24) | (cmd[3] << 16) | (cmd[4] << 8) | (cmd[5]);
		len	= (cmd[7] << 8) | (cmd[8]);
	}

	if (valid) {
		lba *= MSD_BLOCK_SIZE;
		len *= MSD_BLOCK_SIZE;
		if (msd_state.flags & MSD_FLAG_READ_ONLY) {
			return msd_send_error (
				SCSI_SENSE_KEY_DATA_PROTECT,
				SCSI_SENSE_CODE_WRITE_PROTECTED, 0
			);
		} else if ((lba < msd_state.data_len) && ((lba + len) <= msd_state.data_len)) {
			return msd_recv_data (msd_state.data + lba, len);
		} else {
			return msd_send_error (
				SCSI_SENSE_KEY_ILLEGAL_REQUEST,
				SCSI_SENSE_CODE_LOGICAL_BLOCK_ADDRESS_OUT_OF_RANGE, 0
			);
		}
	} else {
		return MSD_STATUS_CMD_FAILED;
	}
}

static int scsi_report_luns (const uint8_t *cmd)
{
	/* 1	= Reserved */
	/* 2	= Select Report */
	/* 3-5	= Reserved */
	/* 6-9	= Allocation Length */
	/* 10	= Reserved */
	/* 11	= Control */
	if ((cmd[1] | cmd[3] | cmd[4] | cmd[5] | cmd[10]) == 0) {
		if (cmd[2] <= 2) {
			uint32_t len = (cmd[6] << 24) | (cmd[7] << 16) | (cmd[8] << 8) | cmd[9];
			uint8_t *buf = (uint8_t *) msd_state.buf.inquiry;

			if (len > 16)
				len = 16;

			memset (buf, 0, 16);
			buf[3] = 8; /* LUN list is 8 bytes */
			
			return msd_send_data (buf, len);
		}
	}
	return MSD_STATUS_CMD_FAILED;
}

static int scsi_mode_sense (const uint8_t *cmd)
{
	uint8_t page_code = cmd[2];

	debug_msg ("SNS ", page_code);

	if (page_code == 0) {
		uint8_t len = cmd[4];
		uint8_t *desc = (uint8_t *) msd_state.buf.inquiry;

		desc[0] = 3;	/* Mode Data Length */
		desc[1] = 0x00;	/* Medium Type */
		desc[2] = 0x00;	/* Reserved (0-3,5), DPO/FUA (4), WP (7) */
		desc[3] = 0x00;	/* Block Descriptors Length */

		if (len > 4)
			len = 4;

		return msd_send_data (desc, len);
	} else {
		return MSD_STATUS_CMD_FAILED;
	}
}

static void msd_handle_cbw (const uint8_t *cbw, const int32_t len)
{
	int ok = 1;

	/* Validate CBW */
	ok &= (len == 31);
	ok &= (*((uint32_t *) cbw) == 0x43425355); 	/* header */
	ok &= ((cbw[12] & 0x7f) == 0);			/* reserve bits */
	ok &= (cbw[13] == 0);				/* LUN = 0, reserve bits */
	ok &= (cbw[14] >= 1) && (cbw[14] <= 16);	/* cmd length */
	
	debug_msg ("CBW ", *((uint32_t *) cbw));
	// (ok << 24) | len); (cbw[12] << 16) | (cbw[13] << 8) | cbw[14]);

	if (!ok) {
		msd_state.status = MSD_WAIT_RESET;
		usb_set_halt (MSD_DATA_EP_IN);
		usb_set_halt (MSD_DATA_EP_OUT);
		debug_msg ("ERRC", 0);
	} else {
		const uint8_t *cmd;
		int cmd_len;

		/* Setup command state */
		msd_state.tag		= *((uint32_t *) &(cbw[4]));
		msd_state.transfer_len	= *((uint32_t *) &(cbw[8]));
		msd_state.flags		= (msd_state.flags & (~MSD_FLAG_DEV_TO_HOST)) | (cbw[12] >> 7);
		
		cmd_len = cbw[14];
		cmd	= &(cbw[15]);
		ok	= MSD_STATUS_CMD_FAILED;
		
		debug_msg ("CMD ", cmd[0]);

		if (cmd_len == 6) {
			switch (cmd[0]) {
				case SCSI_CMD_TEST_UNIT_READY: 	ok = scsi_test_unit_ready (cmd);break;
				case SCSI_CMD_REQUEST_SENSE:	ok = scsi_request_sense (cmd); 	break;
				case SCSI_CMD_INQUIRY:		ok = scsi_inquiry (cmd);	break;
				case SCSI_CMD_PREVENT_REMOVAL:	ok = scsi_prevent_removal (cmd);break;
				case SCSI_CMD_READ6:		ok = scsi_read (cmd);		break;
				case SCSI_CMD_WRITE6:		ok = scsi_write (cmd);		break;
				case SCSI_CMD_MODE_SENSE6:	ok = scsi_mode_sense (cmd);	break;
			}
		} else if (cmd_len == 10) {
			switch (cmd[0]) {
				case SCSI_CMD_READ_CAPACITY10:	ok = scsi_read_capacity (cmd);	break;
				case SCSI_CMD_READ10:		ok = scsi_read (cmd);		break;
				case SCSI_CMD_WRITE10:		ok = scsi_write (cmd);		break;
			}
		} else if (cmd_len == 12) {
			switch (cmd[0]) {
				case SCSI_CMD_REPORT_LUNS:	ok = scsi_report_luns (cmd);	break;
			}
		}

		switch (ok) {
			case MSD_STATUS_DATA_PHASE:
			case MSD_STATUS_CSW_SENT:
			case MSD_STATUS_STALL: 
				break;
			case MSD_STATUS_CMD_PASSED:
				msd_send_csw (MSD_STATUS_CMD_PASSED);
				break;
			default:
				msd_send_error (
					SCSI_SENSE_KEY_ILLEGAL_REQUEST,
					SCSI_SENSE_CODE_INVALID_FIELD_IN_CDB, 0
				);
				break;
		}
	}
}

static void msd_handle_rx (int endpoint)
{
	if (endpoint != MSD_DATA_EP_IN) {
		return;
	} else if (msd_state.status == MSD_WAIT_CBW) {
		debug_msg ("RXC ", msd_state.status);
		msd_handle_cbw ((uint8_t *) msd_state.buf.cbw, usb_state.rx_len);
	} else if (msd_state.status == MSD_DATA_RX) {
		debug_msg ("RXC ", msd_state.status);
		if (usb_state.rx_len == usb_state.rx_size) {
			msd_send_csw (MSD_STATUS_CMD_PASSED);
		}
	}
}

static void msd_handle_tx_complete (int endpoint)
{
	//debug_msg ("TXC ", msd_state.status);
	if (endpoint != MSD_DATA_EP_OUT) {
		return;
	} else if (msd_state.status == MSD_DATA_TX) {
		msd_send_csw (MSD_STATUS_CMD_PASSED);
	}
}


/* Handle receiving and responding to setup packets on EP0. */
static void usb_manage_setup_packet (uint8_t endpoint)
{
	/* The structure of a USB setup packet. */
	struct {
		uint8_t request_attrs; /* Request characteristics. */
		uint8_t request; /* Request type. */
		uint16_t value; /* Request-specific value. */
		uint16_t index; /* Request-specific index. */
		uint16_t length; /* The number of bytes transferred in the (optional)
			     * second phase of the control transfer. */
	} packet;
	uint16_t response = 0;
	uint8_t byte_resp;

	/* Ignore setup packets on endpoints other than control 0 */
	if (endpoint != 0) {
		usb_csr_clear_flag (endpoint, AT91C_UDP_RXSETUP);
		return;
	}

	/* Read the packet from the FIFO into the above packet struct. */
	packet.request_attrs = AT91C_UDP_FDR[0];
	packet.request       = AT91C_UDP_FDR[0];
	packet.value         = (AT91C_UDP_FDR[0] & 0xFF) | (AT91C_UDP_FDR[0] << 8);
	packet.index         = (AT91C_UDP_FDR[0] & 0xFF) | (AT91C_UDP_FDR[0] << 8);
	packet.length        = (AT91C_UDP_FDR[0] & 0xFF) | (AT91C_UDP_FDR[0] << 8);

	if ((packet.request_attrs & USB_BMREQUEST_DIR) == USB_BMREQUEST_D_TO_H) {
		usb_csr_set_flag (0, AT91C_UDP_DIR);
	}

	usb_csr_clear_flag (0, AT91C_UDP_RXSETUP);

	/* Intercept class requests */
	if (usb_state.current_config == 1 
			&& (packet.request_attrs & (USB_BMREQUEST_CLASS | USB_BMREQUEST_RCPT))
				== (USB_BMREQUEST_CLASS | USB_BMREQUEST_RCPT_INT)
			&& (packet.index == 0)) {
		switch (packet.request) {
			case USB_BREQUEST_BULK_RESET:
				/* forward reset to MSD; */
				msd_reset ();
				usb_send_null ();
				break;
			case USB_BREQUEST_GET_MAX_LUN:
				byte_resp = 0;
				usb_write_data (0, (uint8_t *)&byte_resp, 1);
				break;
			default:
				usb_send_stall (0);
				break;
		}
		
		return;
	}

	/* Respond to the control request. */
	switch (packet.request) {
		case USB_BREQUEST_GET_STATUS:
			/* The host wants to know our status.
			 *
			 * If it wants the device status, just reply that the NXT is still
			 * self-powered (as first declared by the setup packets). If it
			 * wants endpoint status, reply with that endpoints halt bit.
			 * Any other status request types are reserved, which
			 * translates to replying zero.
			 */
			switch (packet.request_attrs & USB_BMREQUEST_RCPT) {
				case USB_BMREQUEST_RCPT_DEV:
					response = 1; /* self powered */
					break;
				case USB_BMREQUEST_RCPT_EPT:
					{
						int ep = packet.index & 0x7f;
						
						if (ep < N_ENDPOINTS) {
							response = (usb_state.halted >> ep) & 1;
						} else {
							response = 0;
						}
					}
					break;
				default:
					response = 0;
					break;
			}

			usb_write_data (0, (uint8_t *)&response, 2);
			break;

		case USB_BREQUEST_SET_DESCRIPTOR:
			/* No descriptors that can be updated, so return Request Error. */
			usb_send_stall (0);
			break;

		case USB_BREQUEST_CLEAR_FEATURE:
		case USB_BREQUEST_SET_FEATURE:
			/* No features that can be set/cleared, so return Request Error. */
			if (((packet.request_attrs & USB_BMREQUEST_RCPT) == USB_BMREQUEST_RCPT_EPT)
					&& (packet.value == USB_FEAT_ENDPOINT_HALT)) {
				uint8_t ep = packet.index & 0x7f;
				
				if (ep < N_ENDPOINTS) {
					if (packet.value == USB_BREQUEST_SET_FEATURE) {
						usb_set_halt (ep);
					} else {
						usb_clear_halt (ep);
						if (usb_state.halted == 0)
							msd_cleared_halt ();
					}
					usb_send_null ();
				} else {
					usb_send_stall (0);
				}
			} else {
				usb_send_stall (0);
			}
			break;

		case USB_BREQUEST_GET_INTERFACE:
			if (usb_state.current_config == 1 && packet.index == 0) {
				byte_resp = 0;
				usb_write_data (0, (uint8_t *)&byte_resp, 1);
			} else {
				usb_send_stall (0);
			}
			break;

		case USB_BREQUEST_SET_INTERFACE:
			/* No alternate settings available, so return Request Error. */
			usb_send_stall (0);
			break;

		case USB_BREQUEST_SET_ADDRESS:
			/* The host has given the NXT a new USB address. This address
			 * must be set AFTER sending the ack packet. Therefore, we just
			 * remember the new address, and the interrupt handler will set
			 * it when the transmission completes.
			 */
			usb_state.new_device_address = packet.value;
			usb_send_null ();

			/* If the address change is to 0, do it immediately.
			 *
			 * TODO: Why? And when does this happen?
			 */
			if (usb_state.new_device_address == 0) {
				*AT91C_UDP_FADDR = AT91C_UDP_FEN;
				*AT91C_UDP_GLBSTATE = 0;
			}
			break;

		case USB_BREQUEST_GET_DESCRIPTOR:
			/* The host requested a descriptor. */
			{
				uint32_t size;
				uint8_t index = (packet.value & USB_WVALUE_INDEX);
				switch ((packet.value & USB_WVALUE_TYPE) >> 8) {
					case USB_DESC_TYPE_DEVICE: /* Device descriptor */
						size = usb_device_descriptor[0];
						usb_write_data (0, usb_device_descriptor,
								MIN(size, packet.length));
						break;

					case USB_DESC_TYPE_CONFIG: /* Configuration descriptor */
						usb_write_data (0, usb_full_config,
								MIN(usb_full_config[2], packet.length));

						/* FIXME: Why? This is not specified in the USB specs. */
						if (usb_full_config[2] < packet.length)
							usb_send_null ();
						break;

					case USB_DESC_TYPE_STR: /* String or language info. */
						if ((packet.value & USB_WVALUE_INDEX) == 0) {
							usb_write_data (0, usb_string_desc,
									MIN(usb_string_desc[0], packet.length));
						} else if (index <= usb_string_count) {
							/* The host wants a specific string. */
							usb_write_data (0, usb_strings[index-1],
									MIN(usb_strings[index-1][0],
										packet.length));
						} else {
							/* Not a string we have. */
							usb_send_stall (0);
						}
						break;

					case USB_DESC_TYPE_DEVICE_QUALIFIER: /* Device qualifier descriptor. */
						size = usb_dev_qualifier_desc[0];
						usb_write_data (0, usb_dev_qualifier_desc,
								MIN(size, packet.length));
						break;

					default: /* Unknown descriptor, tell the host by stalling. */
						usb_send_stall (0);
				}
				break;
			}

		case USB_BREQUEST_GET_CONFIG:
			/* The host wants to know the ID of the current configuration. */
			usb_write_data (0, (uint8_t *)&(usb_state.current_config), 1);
			break;

		case USB_BREQUEST_SET_CONFIG:
			/* The host selected a new configuration. */
			if (packet.value == 0 || packet.value == 1) {
				uint32_t csr[4] = { 0, 0, 0, 0 };
				int i;

				usb_state.current_config = packet.value;
				
				if (usb_state.current_config == 0) {
					*AT91C_UDP_GLBSTATE	= AT91C_UDP_FADDEN;
					usb_state.status	= USB_UNINITIALISED;
				} else {
					/* we set the register in configured mode */
					*AT91C_UDP_GLBSTATE	= AT91C_UDP_CONFG | AT91C_UDP_FADDEN;
					csr[MSD_DATA_EP_IN]	= AT91C_UDP_EPTYPE_BULK_OUT;
					csr[MSD_DATA_EP_OUT]	= AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_BULK_IN;
					usb_state.status	= USB_READY;
				}

				for (i = 1; i < 4; ++i)
					usb_csr_set_value (i, csr[i]);
				
				if (usb_state.current_config == 1) {
					usb_clear_halt (MSD_DATA_EP_IN);
					usb_clear_halt (MSD_DATA_EP_OUT);
					msd_reset ();
				}
				
				/* ack */
				usb_send_null ();
			} else {
				usb_send_stall (0);
			}
			break;
		
		default:
			usb_send_stall (0);
			break;
	}
}


/* The main USB interrupt handler. */
static void usb_isr (void)
{
	uint32_t isr = *AT91C_UDP_ISR;

	if (isr & (AT91C_UDP_ENDBUSRES 
		| AT91C_UDP_WAKEUP 
		| AT91C_UDP_SOFINT 
		| AT91C_UDP_RXSUSP 
		| AT91C_UDP_RXRSM)) { 
		
		/* End of bus reset. Starting the device setup procedure. */
		if (isr & AT91C_UDP_ENDBUSRES) {
			usb_state.status = USB_UNINITIALISED;

			/* Disable and clear all interruptions, reverting to the base
			 * state.
			 */
			*AT91C_UDP_IDR = ~0;
			*AT91C_UDP_ICR = ~0;

			/* Reset all endpoint FIFOs. */
			*AT91C_UDP_RSTEP = ~0;
			*AT91C_UDP_RSTEP = 0;

			/* Reset internal state. */
			usb_state.current_rx_bank = AT91C_UDP_RX_DATA_BK0;
			usb_state.current_config  = 0;

			/* Reset EP0 to a basic control endpoint. */
			usb_csr_set_value (0, AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_CTRL);

			/* Disable other endpoints */
			usb_state.halted = 0x00;
			usb_csr_set_value (1, 0);
			usb_csr_set_value (2, 0);
			usb_csr_set_value (3, 0);

			/* Enable interrupt handling for all three endpoints, as well as
			 * suspend/resume.
			 */
			*AT91C_UDP_IER = (AT91C_UDP_EPINT0 | AT91C_UDP_EPINT1 |
					AT91C_UDP_EPINT2 | AT91C_UDP_EPINT3 |
					AT91C_UDP_RXSUSP | AT91C_UDP_RXRSM);

			/* Enable the function endpoints, setting address 0, and return
			 * immediately. Given that we've just reset everything, there's no
			 * point in continuing.
			 */
			*AT91C_UDP_FADDR = AT91C_UDP_FEN;

			return;
		}

		if (isr & AT91C_UDP_WAKEUP) {
			*AT91C_UDP_ICR = AT91C_UDP_WAKEUP;
			isr &= ~AT91C_UDP_WAKEUP;
		}


		if (isr & AT91C_UDP_SOFINT) {
			*AT91C_UDP_ICR = AT91C_UDP_SOFINT;
			isr &= ~AT91C_UDP_SOFINT;
		}

		if (isr & AT91C_UDP_RXSUSP) {
			*AT91C_UDP_ICR = AT91C_UDP_RXSUSP;
			isr &= ~AT91C_UDP_RXSUSP;
			usb_state.pre_suspend_status = usb_state.status;
			usb_state.status = USB_SUSPENDED;
		}

		if (isr & AT91C_UDP_RXRSM) {
			*AT91C_UDP_ICR = AT91C_UDP_RXRSM;
			isr &= ~AT91C_UDP_RXRSM;
			usb_state.status = usb_state.pre_suspend_status;
		}
	}

	if (isr & (AT91C_UDP_EP0 | AT91C_UDP_EP1 | AT91C_UDP_EP2 | AT91C_UDP_EP3)) {
		uint32_t csr;
		int endpoint = 0;
		
		while (!(isr & (1 << endpoint)))
			endpoint++;

		csr = AT91C_UDP_CSR[endpoint];

		/* Acknowledge stall */
		if (csr & AT91C_UDP_ISOERROR) {
			usb_csr_clear_flag (endpoint, AT91C_UDP_FORCESTALL | AT91C_UDP_ISOERROR);
		}

		/* Setup packet */
		if (csr & AT91C_UDP_RXSETUP) {
			usb_manage_setup_packet (endpoint);
			return;
		}
		
		/* Receive complete */
		if (csr & (AT91C_UDP_RX_DATA_BK0 | AT91C_UDP_RX_DATA_BK1)) {
			usb_read_data (endpoint);
			msd_handle_rx (endpoint);
			return;
		}

		/* Transmit complete */
		if (csr & AT91C_UDP_TXCOMP) {
			/* so first we will reset this flag */
			usb_csr_clear_flag (endpoint, AT91C_UDP_TXCOMP);

			if (usb_state.new_device_address > 0) {
				/* the previous message received was SET_ADDR */
				/* now that the computer acknowledged our send_null(), 
				 * we can set this address for real */

				/* we set the specified usb address in the controller */
				*AT91C_UDP_FADDR    = AT91C_UDP_FEN | usb_state.new_device_address;
				/* and we tell the controller that we are in addressed mode now */
				*AT91C_UDP_GLBSTATE = AT91C_UDP_FADDEN;
				usb_state.new_device_address = 0;
			}

			/* and we will send the following data */
			if (!usb_cont_write (endpoint)) {
				/* then it means that we sent all the data and the host has acknowledged it */
				usb_state.status = USB_READY;
				if (endpoint > 0)
					msd_handle_tx_complete (endpoint);
			}
			
			return;
		}
	}


	/* We clear also the unused bits,
	 * just "to be sure" */
	if (isr) {
		*AT91C_UDP_ICR = 0xFFFFC4F0;
	}
}

void usb_disable (void)
{
	aic_disable (AT91C_ID_UDP);

	*AT91C_PIOA_PER = (1 << 16);
	*AT91C_PIOA_OER = (1 << 16);
	*AT91C_PIOA_SODR = (1 << 16);

	systick_wait_ms (200);
}

void usb_enable (void)
{
	/* Enable the UDP pull up by outputting a zero on PA.16 */
	/* Enabling the pull up will tell to the host (the computer) that
	 * we are ready for a communication
	 */
	*AT91C_PIOA_PER = (1 << 16);
	*AT91C_PIOA_OER = (1 << 16);
	*AT91C_PIOA_CODR = (1 << 16);

	systick_wait_ms (200);
}

void usb_set_msd (uint8_t *msd_data, uint32_t msd_len, int read_only)
{
	msd_state.flags		&= ~MSD_FLAG_READ_ONLY;
	if (read_only)
		msd_state.flags |= MSD_FLAG_READ_ONLY;
	msd_state.data		= msd_data;
	msd_state.data_len	= msd_len;
}

void usb_init (uint8_t *msd_data, uint32_t msd_len, int read_only)
{
	usb_disable ();

	memset ((void *)&usb_state, 0, sizeof (usb_state));
	memset ((void *)&msd_state, 0, sizeof (msd_state));

	nxt__interrupts_disable ();

	/* usb pll was already set in init.S */

	/* enable peripheral clock */
	*AT91C_PMC_PCER = (1 << AT91C_ID_UDP);

	/* enable system clock */
	*AT91C_PMC_SCER = AT91C_PMC_UDP;

	/* disable all the interruptions */
	*AT91C_UDP_IDR = ~0;

	/* reset all the endpoints */
	*AT91C_UDP_RSTEP = 0xF;
	*AT91C_UDP_RSTEP = 0;

	*AT91C_UDP_ICR = 0xFFFFFFFF;

	/* Install the interruption routine */

	/* the first interruption we will get is an ENDBUSRES
	 * this interruption is always emit (can't be disable with UDP_IER)
	 */
	/* other interruptions will be enabled when needed */
	aic_install_isr (AT91C_ID_UDP, AIC_PRIO_USB, AIC_TRIG_LEVEL, usb_isr);

	nxt__interrupts_enable ();

	usb_set_msd (msd_data, msd_len, read_only);
	usb_enable ();
}

