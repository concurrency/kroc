#ifndef TVM_NXT_H
#define TVM_NXT_H

/*{{{  Platform type definitions */
typedef unsigned char uint8_t; 
typedef signed char int8_t;
typedef unsigned short uint16_t;
typedef signed short int16_t;
typedef unsigned long uint32_t;
typedef signed long int32_t;

typedef uint32_t size_t;

typedef uint8_t bool;
#define FALSE (0)
#define TRUE (!FALSE)
/*}}}*/

/*{{{  Memory functions */
/* mem.c */
void memset (void *dest, int8_t val, size_t len);
/*}}}*/

/*{{{  Hardware access defines and functions */
#define NXT_CLOCK_FREQ	48000000
#define NXT_N_MOTORS	4
#define NXT_N_SENSORS	4
#define NXT_LCD_HEIGHT	64
#define NXT_LCD_WIDTH	100

/* memory map */
extern uint8_t __ram_userspace_start__;
extern uint8_t __ram_userspace_end__;
extern uint8_t __ramtext_ram_start__;
extern uint8_t __ramtext_ram_end__;
extern uint8_t __text_start__;
extern uint8_t __text_end__;
extern uint8_t __data_ram_start__;
extern uint8_t __data_ram_end__;
extern uint8_t __bss_start__;
extern uint8_t __bss_end__;
extern uint8_t __stack_start__;
extern uint8_t __stack_end__;
extern uint8_t __rom_end__;
extern uint8_t __boot_from_samba__;

#define SYMADDR(sym)		((uint8_t *)&(sym))
#define SECSIZE(start, end)	((uint32_t)(end - start))
#define NXT_FREE_MEM_START	SYMADDR(__ram_userspace_start__)
#define NXT_FREE_MEM_END	SYMADDR(__ram_userspace_end__)
#define NXT_FREE_MEM_LEN	SECSIZE(NXT_FREE_MEM_START, NXT_FREE_MEM_END)

/* buttons */
enum {
	BUTTON_NONE	= 0,
	BUTTON_OK,
	BUTTON_CANCEL,
	BUTTON_LEFT,
	BUTTON_RIGHT
};

/* interrupts.S */
void nxt__interrupts_disable (void);
void nxt__interrupts_enable (void);
void nxt__default_irq (void);
void nxt__default_fiq (void);
void nxt__spurious_irq (void);

/* lock.S */
uint32_t nxt__atomic_cas32 (uint32_t *dest, uint32_t val);
uint8_t nxt__atomic_cas8 (uint8_t *dest, uint8_t val);

/* aic.c */
enum {
	AIC_TRIG_LEVEL		= 0,
	AIC_TRIG_EDGE		= 1
};
enum {
	AIC_PRIO_LOW		= 1,
	AIC_PRIO_DRIVER		= 3,
	AIC_PRIO_USB		= 2,
	AIC_PRIO_SOFTMAC 	= 4,
	AIC_PRIO_SCHED		= 5,
	AIC_PRIO_RT 		= 6,
	AIC_PRIO_TICK		= 7
};
void aic_enable (int vector);
void aic_disable (int vector);
void aic_set (int vector);
void aic_clear (int vector);
void aic_install_isr (int vector, uint32_t prio, uint32_t trig_mode, void *isr);
void aic_init (void);

/* avr.c */
void avr_init (void);
void avr_data_init (void);
void avr_systick_update (void);
void avr_power_down (void);
int avr_get_button (void);

/* debug.c */
void debug_blink (void);
void debug_init (void);
void debug_msg (const char *msg, const uint32_t value);

/* nxt.c */
uint32_t systick_get_ms (void);
void systick_wait_ms (uint32_t ms);
void systick_wait_ns (uint32_t ns);

/* lcd.c */
void lcd_init (void);
void lcd_update (void);
void lcd_set_display (uint8_t *display);
void lcd_dirty_display (void);
void lcd_shutdown (void);
void lcd_sync_refresh (void);

/* usb.c */
void usb_disable (void);
void usb_enable (void);
void usb_set_msd (uint8_t *msd_data, uint32_t msd_len, int read_only);
void usb_init (uint8_t *msd_data, uint32_t msd_len, int read_only);


/*}}}*/


/* Define for lots of useful printed-out stuff. */
#undef DEBUG


#define TVM_ECTX_PRIVATE_DATA 	tvm_ectx_priv_t
typedef struct _tvm_ectx_priv_t {
	void            *memory;
	int             memory_length;
} tvm_ectx_priv_t;

#include <tvm.h>
#include <tvm_tbc.h>


enum {
	TVM_INTR_VIRTUAL = 1 << (SFLAG_USER_P + 0)
};
#define TVM_INTR_SFLAGS \
	(SFLAG_INTR | \
	TVM_INTR_VIRTUAL)

/*{{{  sffi.c */
extern SFFI_FUNCTION sffi_table[];
extern const int sffi_table_length;
/*}}}*/
#if 0
/*{{{  interrupts.c */
extern void init_interrupts (void);
extern void clear_pending_interrupts (void);
extern int waiting_on_interrupts (void);
extern int ffi_wait_for_interrupt (ECTX ectx, WORD args[]);
/*}}}*/
#endif
/*{{{  tbc.c */
extern UWORD valid_tbc_header (BYTE *data);
extern tbc_t *load_context_with_tbc (ECTX ectx, tbc_t *tbc, BYTE *data, UWORD length);
/*}}}*/
/*{{{  tvm.c */
extern void main (void);
/*}}}*/

#endif /* !TVM_NXT_H */
