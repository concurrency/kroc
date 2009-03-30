/*
 * introspect.c - TVM introspection layer
 *
 * Copyright (C) 2008  Carl G. Ritson
 *
 */

#include "tvm_posix.h"
#include <tvm_tbc.h>

#ifdef TVM_EXTERNAL_CHANNEL_BUNDLES

#define MT_DEFINES	1
#define MT_TVM		1
#include <mobile_types.h>

/*{{{  Very Limited Protocol Decoder */

/* Channel State Type Pre-declaration */
typedef struct c_state_t c_state_t;

/* Protocol Argument Types */
enum {
	P_A_ARRAY	= 'A',
	P_A_BYTE	= 'b',
	P_A_BYTES	= 'B',
	P_A_MT		= 'M',
	P_A_WORD	= 'w'
};

/* Protocol Argument */
typedef struct p_arg_t {
	int			type;
	int			length;
	union {
		BYTE		*array;
		BYTE		byte;
		BYTE		bytes[4];
		WORDPTR		mt;
		WORD		word;
	} data;
} TVM_PACK p_arg_t;

/* Protocol Symbol */
typedef struct p_sym_t {
	int	entry;
	char	*symbols;
	int	(*dispatch)(ECTX, c_state_t *);
} p_sym_t;

/* Protocol Case Description */
typedef struct pc_desc_t {
	p_sym_t		*symbol;
	int		argc;
	p_arg_t		argv[8];
} pc_desc_t;


/* Channel States */
enum {
	C_S_IDLE,
	C_S_ENCODE_ENTRY,
	C_S_ENCODE,
	C_S_DECODE
};

/* Channel Type */
enum {
	C_T_UNKNOWN,
	C_T_BYTECODE,
	C_T_VM,
	C_T_VM_CTL
};


/* Channel State */
struct c_state_t {
	int		state;
	WORDPTR		waiting;
	p_sym_t		*in;
	p_sym_t		*decode;
	pc_desc_t	p;
	int		type;
	union {
		bytecode_t	*bc;
		ECTX		vm;
	}		data;
	BYTE		*buffer;
};

static int handle_in (ECTX ectx,
	void *data, WORDPTR channel, BYTEPTR address, WORD count)
{
	c_state_t *c = (c_state_t *) data;

	if (c->state == C_S_ENCODE_ENTRY) {
		if (count == 1) {
			write_byte_and_type (ectx, address, (BYTE) c->p.symbol->entry, STYPE_DATA);
			
			if (c->p.argc > 0) {
				c->state = C_S_ENCODE;
			} else {
				c->state = C_S_IDLE;
				if (c->p.symbol->dispatch != NULL)
					return c->p.symbol->dispatch (ectx, c);
			}
			
			return ECTX_CONTINUE;
		} else {
			return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
		}
	} else if (c->state == C_S_ENCODE) {
		int	n	= --c->p.argc;
		p_arg_t *arg	= &(c->p.argv[n]);
		int	i;
		
		if (arg->type == P_A_WORD && count == sizeof (WORD)) {
			write_word_and_type (ectx, (WORDPTR) address, arg->data.word, STYPE_DATA);
		} else if (arg->type == P_A_BYTE && count == 1) {
			write_byte_and_type (ectx, address, arg->data.byte, STYPE_DATA);
		} else if (arg->type == P_A_BYTES && count == arg->length) {
			for (i = 0; i < arg->length; ++i) {
				write_byte_and_type (ectx, address, arg->data.bytes[i], STYPE_DATA);
				address = byteptr_plus (address, 1);
			}
		} else if (arg->type == P_A_ARRAY && count == arg->length) {
			fill_type_shadow (ectx, address, arg->length, STYPE_DATA);
			for (i = 0; i < arg->length; ++i) {
				write_byte (address, arg->data.array[i]);
				address = byteptr_plus (address, 1);
			}
			if (arg->data.array != c->buffer)
				free (arg->data.array);
		} else {
			return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
		}
		
		if (c->p.argc == 0) {
			c->state = C_S_IDLE;
			if (c->p.symbol->dispatch != NULL)
				return c->p.symbol->dispatch (ectx, c);
		}

		return ECTX_CONTINUE;
	} else {
		WORKSPACE_SET (ectx->wptr, WS_POINTER, (WORD) address);
		WORKSPACE_SET (ectx->wptr, WS_ECTX, (WORD) ectx);
		WORKSPACE_SET (ectx->wptr, WS_PENDING, count);
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);

		c->waiting = ectx->wptr;

		return _ECTX_DESCHEDULE;
	}
}

static int handle_out (ECTX ectx,
	void *data, WORDPTR channel, BYTEPTR address, WORD count)
{
	c_state_t *c = (c_state_t *) data;
	int i;

	if (c->state == C_S_IDLE && count == 1) {
		int entry = read_byte (address);
		
		for (i = 0; c->in[i].entry != entry; ++i) {
			if (c->in[i].symbols == NULL) {
				return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
			}
		}

		c->p.symbol	= &(c->in[i]);
		c->p.argc	= 0;

		if (c->in[i].symbols[0] == '\0') {
			return c->in[i].dispatch (ectx, c);
		} else {
			c->state	= C_S_DECODE;
			c->decode 	= c->p.symbol;
			return ECTX_CONTINUE;
		}
	} else if (c->state == C_S_DECODE) {
		int n		= c->p.argc++;
		p_arg_t	*arg	= &(c->p.argv[n]);
		
		arg->length	= 0;

		switch (c->decode->symbols[n]) {
			case '4': arg->length++;
			case '3': arg->length++;
			case '2': arg->length++;
			case '1': arg->length++;
			case '0': 
				arg->type = 'B';
				break;
			default:
				arg->type = c->decode->symbols[n];
				break;
		}

		if (arg->type == P_A_WORD && count == sizeof (WORD)) {
			arg->data.word = read_word ((WORDPTR) address);
		} else if (arg->type == P_A_BYTE && count == 1) {
			arg->data.byte = read_byte (address);
		} else if (arg->type == P_A_BYTES && count == arg->length) {
			for (i = 0; i < arg->length; ++i) {
				arg->data.bytes[i] = read_byte (address);
				address = byteptr_plus (address, 1);
			}
		} else if (arg->type == P_A_ARRAY) {
			arg->length	= count;
			arg->data.array = (BYTE *) malloc (count);
			for (i = 0; i < arg->length; ++i) {
				arg->data.array[i] = read_byte (address);
				address = byteptr_plus (address, 1);
			}
		} else {
			return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
		}
		
		if (c->decode->symbols[n+1] == '\0') {
			c->state = C_S_IDLE;
			return c->decode->dispatch (ectx, c);
		} else {
			return ECTX_CONTINUE;
		}
	}

	return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
}

static int handle_mt_in (ECTX ectx, 
	void *data, WORDPTR channel, WORDPTR address)
{
	c_state_t *c = (c_state_t *) data;
	
	if (c->state == C_S_ENCODE) {
		int	n	= --c->p.argc;
		p_arg_t *arg	= &(c->p.argv[n]);
		
		if (arg->type == P_A_MT) {
			write_word_and_type (ectx, address, (WORD) arg->data.mt, 
				arg->data.mt != NULL ? STYPE_MT : STYPE_NULL);

			if (c->p.argc == 0) {
				c->state = C_S_IDLE;

				if (c->p.symbol->dispatch != NULL)
					return c->p.symbol->dispatch (ectx, c);
			}
			
			return ECTX_CONTINUE;
		}
	}
	
	return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
}

static int handle_mt_out (ECTX ectx, 
	void *data, WORDPTR channel, WORDPTR address)
{
	c_state_t *c = (c_state_t *) data;

	if (c->state == C_S_DECODE) {
		int n		= c->p.argc++;
		p_arg_t	*arg	= &(c->p.argv[n]);
		
		arg->type	= c->decode->symbols[n];
		arg->length	= 0;

		if (arg->type == P_A_MT) {
			arg->data.word = read_word ((WORDPTR) address);
			/* FIXME: really we should write NULL_P back to the
			 * 	  address when the mobile type is not shared.
			 */
			if (c->decode->symbols[n+1] == '\0') {
				c->state = C_S_IDLE;
				return c->decode->dispatch (ectx, c);
			} else {
				return ECTX_CONTINUE;
			}
		}
	}

	return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
}

static void handle_free (ECTX ectx, void *data)
{
	c_state_t *c = (c_state_t *) data;
	if (c->type == C_T_BYTECODE) {
		free_bytecode (c->data.bc);
	} else if (c->type == C_T_VM_CTL) {
		free_ectx (c->data.vm);
	}
	if (c->buffer != NULL)
		free (c->buffer);
	free (c);
}

static int send_message (ECTX ectx, c_state_t *c, p_sym_t *sym, ...)
{
	va_list args;
	char	*fmt	= sym->symbols;
	int	i;

	c->state	= C_S_ENCODE_ENTRY;
	c->p.symbol	= sym;
	c->p.argc	= strlen (fmt);
	i		= c->p.argc;

	va_start (args, sym);
	
	while (i) {
		int type = (int) *fmt;

		i--;
		c->p.argv[i].type = type;

		if (type == P_A_ARRAY) {
			c->p.argv[i].data.array = va_arg (args, BYTE *); 
			c->p.argv[i].length	= va_arg (args, int);
		} else if (type == P_A_BYTES) {
			BYTE *bytes		= va_arg (args, BYTE *);
			int length		= va_arg (args, int);
			int j;
			
			c->p.argv[i].length	= length;
			
			for (j = 0; j < length; ++j) {
				c->p.argv[i].data.bytes[j] = bytes[j];
			}
		} else {
			c->p.argv[i].length	= 0;
			c->p.argv[i].data.word	= va_arg (args, WORD);
		}

		fmt++;
	}

	va_end (args);

	if (c->waiting != NOT_PROCESS_P) {
		WORDPTR wptr	= c->waiting;
		BYTEPTR address = (BYTEPTR) WORKSPACE_GET (wptr, WS_POINTER);
		WORD count	= WORKSPACE_GET (wptr, WS_PENDING);

		c->waiting = NOT_PROCESS_P;

		ectx->add_to_queue_external (ectx, ectx, wptr);

		return handle_in (ectx, c, NULL_P, address, count);
	} else {
		return ECTX_CONTINUE;
	}
}

static EXT_CB_INTERFACE cb_interface = {
	.in	= handle_in,
	.out	= handle_out,
	.swap	= NULL,
	.mt_in	= handle_mt_in,
	.mt_out	= handle_mt_out,
	.xable	= NULL,
	.xin	= NULL,
	.mt_xin	= NULL,
	.free	= handle_free
};

#define MT_CB_CONFIG (MT_SIMPLE |\
			MT_MAKE_TYPE (MT_CB) |\
			MT_CB_EXTERNAL |\
			(2 << MT_CB_CHANNELS_SHIFT))

static WORDPTR allocate_cb (ECTX ectx, p_sym_t *in, c_state_t **c_ptr)
{
	c_state_t 	*c = (c_state_t *) malloc (sizeof (c_state_t));
	WORDPTR		cb = tvm_mt_alloc (ectx, MT_CB_CONFIG, 2);
	WORDPTR		cb_base;

	if (cb == (WORDPTR) NULL_P) {
		free (c);
		return (WORDPTR) NULL_P;
	}

	c->state	= C_S_IDLE;
	c->waiting	= (WORDPTR) NOT_PROCESS_P;
	c->in		= in;
	c->decode	= NULL;
	c->p.argc	= 0;
	c->buffer	= NULL;
	*c_ptr		= c;

	cb_base = wordptr_minus (cb, MT_CB_PTR_OFFSET + (sizeof (mt_cb_ext_t) / sizeof (WORD)));

	write_offset (cb_base, mt_cb_ext_t, interface, (WORD) &cb_interface);
	write_offset (cb_base, mt_cb_ext_t, data, (WORD) c);

	return cb;
}

WORDPTR str_to_mt (ECTX ectx, const char *str)
{
	WORDPTR mt = (WORDPTR) NULL_P;
	
	if (str != NULL) {
		int length = strlen (str);
		
		/* Allocate mobile type */
		mt = tvm_mt_alloc (ectx, MT_MAKE_ARRAY_TYPE (1, MT_NUM_BYTE), length);
		/* Copy string into it */
		memcpy (
			wordptr_real_address ((WORDPTR) read_word (mt)),
			str,
			length
		);
		/* Set array dimension to string length */
		write_word (wordptr_plus (mt, 1), length);
	}
	
	return mt;
}
/*}}}*/

/*
VAL INT CLOCK.STEP IS 1:
DATA TYPE ADDR IS INT:
DATA TYPE IPTR IS INT:

DATA TYPE VM.STATE
  PACKED RECORD
    INT        state:
    [3]INT     stack:
    [4]BYTE    type:
    INT        oreg:
    ADDR       wptr:
    IPTR       iptr:
    INT        icount:
    INT        eflags:
:
*/

typedef struct vm_state_t {
	WORD	state;
	WORD	stack[3];
	BYTE	type[4];
	WORD	oreg;
	WORD	wptr;
	WORD	iptr;
	WORD	icount;
	WORD	eflags;
} TVM_PACK vm_state_t;

/*{{{  CT.VM.CTL */
/*
PROTOCOL P.VM.CTL.RQ
  CASE
    run            = 0 ; INT       -- run until for N instructions or until breakpoint
    step           = 1             -- step traced instruction
    dispatch       = 2 ; INT; INT  -- dispatch an arbitrary instruction, with argument
    set.bp         = 3 ; IPTR      -- set break point
    clear.bp       = 4 ; IPTR      -- clear break point
    get.clock      = 5             -- get clock details
    set.clock      = 6 ; INT; INT  -- set clock type and frequency
    trace          = 7 ; INT; BOOL -- enable/disable trace type (instruction)
    get.state      = 8             -- get VM state
    set.state      = 9 ; VM.STATE  -- set VM state
    read.word      = 10; ADDR      -- read word at address
    read.byte      = 11; ADDR      -- read byte at address
    read.int16     = 12; ADDR      -- read int16 at address
    read.type      = 13; ADDR      -- read type of memory at address
    return.param   = 14; INT       -- release parameter N 
    set.param.chan = 15; INT; MOBILE.CHAN
                                   -- set parameter N to channel
:
PROTOCOL P.VM.CTL.RE
  CASE
    decoded        = 0 ; IPTR; INT; INT
                                   -- new IPTR, instruction, arg
    dispatched     = 1 ; IPTR; ADDR
                                   -- new IPTR and WPTR
    bp             = 2 ; IPTR      -- break pointer IPTR reached
    clock          = 3 ; INT; INT  -- clock type and frequency
    ok             = 4
    error          = 5 ; INT
    state          = 6 ; VM.STATE
    word           = 7 ; INT
    byte           = 8 ; BYTE
    int16          = 9 ; INT16
    type           = 10; INT
    channel        = 11; MOBILE.CHAN
:
CHAN TYPE CT.VM.CTL
  MOBILE RECORD
    CHAN P.VM.CTL.RQ request?:
    CHAN P.VM.CTL.RE response!:
:
*/

static int vm_ctl_run (ECTX, c_state_t *);
static int vm_ctl_step (ECTX, c_state_t *);
static int vm_ctl_dispatch (ECTX, c_state_t *);
static int vm_ctl_set_bp (ECTX, c_state_t *);
static int vm_ctl_clear_bp (ECTX, c_state_t *);
static int vm_ctl_get_clock (ECTX, c_state_t *);
static int vm_ctl_set_clock (ECTX, c_state_t *);
static int vm_ctl_trace (ECTX, c_state_t *);
static int vm_ctl_get_state (ECTX, c_state_t *);
static int vm_ctl_set_state (ECTX, c_state_t *);
static int vm_ctl_read_word (ECTX, c_state_t *);
static int vm_ctl_read_byte (ECTX, c_state_t *);
static int vm_ctl_read_int16 (ECTX, c_state_t *);
static int vm_ctl_read_type (ECTX, c_state_t *);
static int vm_ctl_return_param (ECTX, c_state_t *);
static int vm_ctl_set_param_chan (ECTX, c_state_t *);

static p_sym_t vm_ctl_rq[] = {
	{ .entry = 0 , .symbols = "w",	.dispatch = vm_ctl_run		},
	{ .entry = 1 , .symbols = "",	.dispatch = vm_ctl_step		},
	{ .entry = 2 , .symbols = "ww",	.dispatch = vm_ctl_dispatch	},
	{ .entry = 3 , .symbols = "w",	.dispatch = vm_ctl_set_bp	},
	{ .entry = 4 , .symbols = "w",	.dispatch = vm_ctl_clear_bp	},
	{ .entry = 5 , .symbols = "",	.dispatch = vm_ctl_get_clock	},
	{ .entry = 6 , .symbols = "ww",	.dispatch = vm_ctl_set_clock	},
	{ .entry = 7 , .symbols = "wb",	.dispatch = vm_ctl_trace	},
	{ .entry = 8 , .symbols = "",	.dispatch = vm_ctl_get_state	},
	{ .entry = 9 , .symbols = "A",	.dispatch = vm_ctl_set_state	},
	{ .entry = 10, .symbols = "w",	.dispatch = vm_ctl_read_word	},
	{ .entry = 11, .symbols = "w",	.dispatch = vm_ctl_read_byte	},
	{ .entry = 12, .symbols = "w",	.dispatch = vm_ctl_read_int16	},
	{ .entry = 13, .symbols = "w",	.dispatch = vm_ctl_read_type	},
	{ .entry = 14, .symbols = "w",	.dispatch = vm_ctl_return_param },
	{ .entry = 15, .symbols = "wM",	.dispatch = vm_ctl_set_param_chan },
	{ .symbols = NULL }
};

enum {
	VM_CTL_RE_DECODED	= 0,
	VM_CTL_RE_DISPATCHED	= 1,
	VM_CTL_RE_BP		= 2,
	VM_CTL_RE_CLOCK		= 3,
	VM_CTL_RE_OK		= 4,
	VM_CTL_RE_ERROR		= 5,
	VM_CTL_RE_STATE		= 6,
	VM_CTL_RE_WORD		= 7,
	VM_CTL_RE_BYTE		= 8,
	VM_CTL_RE_INT16		= 9,
	VM_CTL_RE_TYPE		= 10,
	VM_CTL_RE_CHANNEL	= 11
};
static p_sym_t vm_ctl_re[] = {
	{ .entry = VM_CTL_RE_DECODED,	.symbols = "www",	.dispatch = NULL },
	{ .entry = VM_CTL_RE_DISPATCHED,.symbols = "ww",	.dispatch = NULL },
	{ .entry = VM_CTL_RE_BP,	.symbols = "w",		.dispatch = NULL },
	{ .entry = VM_CTL_RE_CLOCK,	.symbols = "ww",	.dispatch = NULL },
	{ .entry = VM_CTL_RE_OK,	.symbols = "",		.dispatch = NULL },
	{ .entry = VM_CTL_RE_ERROR,	.symbols = "w",		.dispatch = NULL },
	{ .entry = VM_CTL_RE_STATE,	.symbols = "A",		.dispatch = NULL },
	{ .entry = VM_CTL_RE_WORD,	.symbols = "w",		.dispatch = NULL },
	{ .entry = VM_CTL_RE_BYTE,	.symbols = "b",		.dispatch = NULL },
	{ .entry = VM_CTL_RE_INT16,	.symbols = "2",		.dispatch = NULL },
	{ .entry = VM_CTL_RE_TYPE,	.symbols = "w",		.dispatch = NULL },
	{ .entry = VM_CTL_RE_CHANNEL,	.symbols = "M",		.dispatch = NULL },
	{ .symbols = NULL }
};

static int vm_ctl_run (ECTX ectx, c_state_t *c)
{
	ECTX 	vm 	= c->data.vm;
	int 	ret 	= tvm_run_count (vm, c->p.argv[0].data.word);

	switch (ret) {
		case ECTX_TIME_SLICE:
			return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_OK]));
		default:
			return send_message (
				ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]),
				vm->state
			);
	}
}

static int vm_ctl_step (ECTX ectx, c_state_t *c)
{
	ECTX vm 	= c->data.vm;
	BYTE instr;
	
	vm->state	= ECTX_RUNNING;
	instr		= tvm_decode_instruction (vm);

	return send_message (
		ectx, c, &(vm_ctl_re[VM_CTL_RE_DECODED]),
		vm->iptr,
		instr >> 4,
		vm->oreg
	);
}

static int vm_ctl_dispatch (ECTX ectx, c_state_t *c)
{
	ECTX vm 	= c->data.vm;
	BYTE instr;

	if (c->p.argc == 2) {
		/* request ! dispatch */
		vm->state	= ECTX_RUNNING;
		vm->oreg	= c->p.argv[1].data.word;
		instr		= ((BYTE) c->p.argv[0].data.word) << 4;
	} else {
		/* continuation from response ! decoded */
		instr		= ((BYTE) c->p.argv[1].data.word) << 4;
	}

	vm->state = tvm_dispatch_instruction (vm, instr);

	/* FIXME: this needs to be different; there's no way this will be sufficient */
	switch (vm->state) {
		case ECTX_INS_INVALID:
		case ECTX_INS_UNSUPPORTED:
		case ECTX_ERROR:
		case ECTX_EMPTY:
		case ECTX_SHUTDOWN:
			return send_message (
				ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]),
				vm->state
			);
		default:
			return send_message (
				ectx, c, &(vm_ctl_re[VM_CTL_RE_DISPATCHED]),
				vm->iptr,
				vm->wptr
			);
	}
}

static int vm_ctl_set_bp (ECTX ectx, c_state_t *c)
{
	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
}

static int vm_ctl_clear_bp (ECTX ectx, c_state_t *c)
{
	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
}

static int vm_ctl_get_clock (ECTX ectx, c_state_t *c)
{
	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
}

static int vm_ctl_set_clock (ECTX ectx, c_state_t *c)
{
	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
}

static int vm_ctl_trace (ECTX ectx, c_state_t *c)
{
	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
}

static void encode_state (ECTX vm, vm_state_t *state)
{
	state->state	= vm->state;
	state->stack[0]	= vm->areg;
	state->stack[1]	= vm->breg;
	state->stack[2]	= vm->creg;
	#ifdef TVM_TYPE_SHADOW
	state->type[0]	= (BYTE) vm->aregT;
	state->type[1]	= (BYTE) vm->bregT;
	state->type[2]	= (BYTE) vm->cregT;
	#endif
	state->oreg	= vm->oreg;
	state->wptr	= (WORD) vm->wptr;
	state->iptr	= (WORD) vm->iptr; 
	state->eflags	= vm->eflags;
}

static void decode_state (ECTX vm, vm_state_t *state)
{
	vm->state	= state->state;
	vm->areg	= state->stack[0];
	vm->breg	= state->stack[1];
	vm->creg	= state->stack[2];
	#ifdef TVM_TYPE_SHADOW
	vm->aregT	= state->type[0];
	vm->bregT	= state->type[1];
	vm->cregT	= state->type[2];
	#endif
	vm->oreg	= state->oreg;
	vm->wptr	= (WORDPTR) state->wptr;
	vm->iptr	= (BYTEPTR) state->iptr;
	vm->eflags	= state->eflags;
}

static int vm_ctl_get_state (ECTX ectx, c_state_t *c)
{
	ECTX 		vm	= c->data.vm;
	vm_state_t 	*state	= (vm_state_t *) c->buffer;	

	encode_state (vm, state);

	return send_message (
		ectx, c, &(vm_ctl_re[VM_CTL_RE_STATE]),
		state, sizeof (vm_state_t)
	);
}

static int vm_ctl_set_state (ECTX ectx, c_state_t *c)
{
	ECTX 		vm	= c->data.vm;
	vm_state_t 	*state	= (vm_state_t *) c->p.argv[0].data.array;

	/* FIXME: verify state is valid */
	decode_state (vm, state);

	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_OK]));
}

static int vm_ctl_read_word (ECTX ectx, c_state_t *c)
{
	WORD w = read_word ((WORDPTR) c->p.argv[0].data.word);
	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_WORD]), w);
}

static int vm_ctl_read_byte (ECTX ectx, c_state_t *c)
{
	BYTE b = read_byte ((BYTEPTR) c->p.argv[0].data.word);
	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_BYTE]), b);
}

static int vm_ctl_read_int16 (ECTX ectx, c_state_t *c)
{
	INT16 s = read_int16 ((INT16PTR) c->p.argv[0].data.word);
	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_INT16]), &s, 2);
}

static int vm_ctl_read_type (ECTX ectx, c_state_t *c)
{
	#ifdef TVM_TYPE_SHADOW
	WORD type = read_type (ectx, (BYTEPTR) c->p.argv[0].data.word);
	#else
	WORD type = STYPE_UNDEF;
	#endif
	return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_TYPE]), type);
}

static int vm_ctl_return_param (ECTX ectx, c_state_t *c)
{
	ECTX 	vm	= c->data.vm;
	WORD 	param	= c->p.argv[0].data.word;
	WORDPTR	mt;
	
	/* FIXME: can we do a check on the VM state? */

	if (!(param >= 0 && param < vm->tlp_argc)) {
		return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
	} else {
		switch (vm->tlp_fmt[param]) {
			case '?': case '!': case 'C': case 'S':
				break; /* OK */
			default:
				return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
		}		
	}

	if ((mt = (WORDPTR) vm->tlp_argv[param]) == NULL_P) {
		return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
	} else {
		vm->tlp_argv[param] = NULL_P;
		return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_CHANNEL]), mt);
	}
}

static int vm_ctl_set_param_chan (ECTX ectx, c_state_t *c)
{
	WORDPTR mt 	= c->p.argv[1].data.mt;
	WORD 	param	= c->p.argv[0].data.word;
	ECTX 	vm	= c->data.vm;
	WORDPTR old_mt	= (WORDPTR) vm->tlp_argv[param];

	if ((vm->state != ECTX_INIT) || !(param >= 0 && param < vm->tlp_argc)) {
		/* VM is running or param invalid */
		return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
	} else {
		switch (vm->tlp_fmt[param]) {
			case '?': case '!': case 'C': case 'S':
				break; /* OK */
			default:
				return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_ERROR]), 0);
		}		
	}

	write_word (wordptr_plus (vm->wptr, param + 1), (WORD) mt);
	vm->tlp_argv[param] = (WORD) mt;
	
	if (old_mt == NULL_P) {
		return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_OK]), 0);
	} else {
		return send_message (ectx, c, &(vm_ctl_re[VM_CTL_RE_CHANNEL]), old_mt);
	}
}
/*}}}*/

/*{{{  CT.BYTECODE */
/*
PROTOCOL P.BYTECODE.RQ
  CASE
    create.vm      = 0
    get.symbol     = 1; MOBILE []BYTE   -- look up symbol name
    get.symbol.at  = 2; IPTR            -- look up symbol at bytecode offset
    get.file       = 3; INT             -- translate file number to name
    get.line.info  = 4; IPTR            -- get file/line number of address
    get.details    = 5                  -- get bytecode details
    get.tlp        = 6                  -- get top-level-process details
:
PROTOCOL P.BYTECODE.RE
  CASE
    vm             = 0; CT.VM.CTL!
    error          = 1; INT
    file           = 2; MOBILE []BYTE
    line.info      = 3; INT; INT       -- file, line
    symbol         = 4; IPTR; MOBILE []BYTE; MOBILE []BYTE; INT; INT
                                       -- offset, name, definition, ws, vs
    details        = 5; INT; INT; INT  -- ws, vs, length
    tlp            = 6; MOBILE []BYTE; MOBILE []BYTE
:
CHAN TYPE CT.BYTECODE
  MOBILE RECORD
    CHAN P.BYTECODE.RQ request?:
    CHAN P.BYTECODE.RE response!:
:
*/

static int bytecode_create_vm (ECTX, c_state_t *);
static int bytecode_get_symbol (ECTX, c_state_t *);
static int bytecode_get_file (ECTX, c_state_t *);
static int bytecode_get_line_info (ECTX, c_state_t *);
static int bytecode_get_details (ECTX, c_state_t *);
static int bytecode_get_tlp (ECTX, c_state_t *);

static p_sym_t bytecode_rq[] = {
	{ .entry	= 0, .symbols = "",	.dispatch = bytecode_create_vm },
	{ .entry	= 1, .symbols = "M",	.dispatch = bytecode_get_symbol },
	{ .entry	= 2, .symbols = "w",	.dispatch = bytecode_get_symbol },
	{ .entry	= 3, .symbols = "w",	.dispatch = bytecode_get_file },
	{ .entry	= 4, .symbols = "w",	.dispatch = bytecode_get_line_info },
	{ .entry	= 5, .symbols = "",	.dispatch = bytecode_get_details },
	{ .entry	= 6, .symbols = "",	.dispatch = bytecode_get_tlp },
	{ .symbols = NULL }
};

enum {
	BYTECODE_RE_VM		= 0,
	BYTECODE_RE_ERROR	= 1,
	BYTECODE_RE_FILE	= 2,
	BYTECODE_RE_LINE_INFO	= 3,
	BYTECODE_RE_SYMBOL	= 4,
	BYTECODE_RE_DETAILS	= 5,
	BYTECODE_RE_TLP	= 6
};
static p_sym_t bytecode_re[] = {
	{ .entry = BYTECODE_RE_VM,		.symbols = "M",		.dispatch = NULL },
	{ .entry = BYTECODE_RE_ERROR,		.symbols = "w",		.dispatch = NULL },
	{ .entry = BYTECODE_RE_FILE,		.symbols = "M",		.dispatch = NULL },
	{ .entry = BYTECODE_RE_LINE_INFO,	.symbols = "ww",	.dispatch = NULL },
	{ .entry = BYTECODE_RE_SYMBOL,		.symbols = "wMMww",	.dispatch = NULL },
	{ .entry = BYTECODE_RE_DETAILS,		.symbols = "www",	.dispatch = NULL },
	{ .entry = BYTECODE_RE_TLP,		.symbols = "MM",	.dispatch = NULL },
	{ .symbols = NULL }
};

static int bytecode_create_vm (ECTX ectx, c_state_t *c)
{
	bytecode_t	*bc = c->data.bc;
	c_state_t	*cb_c;
	WORDPTR 	cb;
	ECTX		vm = NULL;
	WORD		argv[TVM_ECTX_TLP_ARGS];
	int i;

	for (i = 0; i < TVM_ECTX_TLP_ARGS; ++i)
		argv[i] = (WORD) NULL_P;

	if (bc->tbc->tlp != NULL) {
		if (bc->tbc->tlp->fmt != NULL)
			vm = allocate_ectx (bc, bc->tbc->tlp->fmt, argv);
	} else if (vm == NULL) {
		vm = allocate_ectx (bc, "?!!", argv);
	}
	
	if (vm == NULL)
		return send_message (ectx, c, &(bytecode_re[BYTECODE_RE_ERROR]), 0);

	cb		= allocate_cb (ectx, vm_ctl_rq, &cb_c);
	cb_c->type 	= C_T_VM_CTL;
	cb_c->data.vm	= vm;
	cb_c->buffer	= (BYTE *) malloc (sizeof (vm_state_t));

	return send_message (ectx, c, &(bytecode_re[BYTECODE_RE_VM]), cb);
}

static int bytecode_get_symbol (ECTX ectx, c_state_t *c)
{
	tbc_t		*tbc = c->data.bc->tbc;
	tbc_sym_t 	*sym = tbc->symbols;

	if (c->p.argv[0].type == P_A_MT) {
		/* symbol name */
		WORDPTR mt	= c->p.argv[0].data.mt;
		char *str	= (char *) 
			wordptr_real_address ((WORDPTR) read_word (wordptr_plus (mt, 0)));
		int length	= read_word (wordptr_plus (mt, 1));

		while (sym != NULL) {
			if (memcmp (sym->name, str, length) == 0) {
				if (sym->name[length] == '\0')
					break;
			}
			sym = sym->next;
		}

		tvm_mt_release (ectx, mt);
	} else {
		/* bytecode offset */
		unsigned int iptr = (unsigned int) c->p.argv[0].data.word;

		if (iptr >= ((unsigned int) tbc->bytecode))
			iptr -= (unsigned int) tbc->bytecode;
		
		if (iptr < tbc->bytecode_len) {
			tbc_sym_t *prev = NULL;
			
			while (sym != NULL) {
				if (sym->offset > iptr)
					break;
				prev	= sym;
				sym	= sym->next;
			}
			
			sym = prev;
		}
	}

	if (sym != NULL) {
		return send_message (ectx, c, &(bytecode_re[BYTECODE_RE_SYMBOL]),
			(WORD) sym->offset,
			str_to_mt (ectx, sym->name),
			str_to_mt (ectx, sym->definition),
			(WORD) sym->ws,
			(WORD) sym->vs
		);
	} else {
		return send_message (ectx, c, &(bytecode_re[BYTECODE_RE_ERROR]), 0);
	}
}

static int bytecode_get_file (ECTX ectx, c_state_t *c)
{
	if (c->data.bc->tbc->debug != NULL) {
		tenc_str_t *file	= c->data.bc->tbc->debug->files;
		int n			= c->p.argv[0].data.word;
		
		while (file != NULL) {
			if (n == 0) {
				return 	send_message (ectx, c, 
						&(bytecode_re[BYTECODE_RE_FILE]),
						str_to_mt (ectx, file->str)
				);
			}
			file = file->next;
			n--;
		}
	}
	
	return send_message (ectx, c, &(bytecode_re[BYTECODE_RE_ERROR]), 0);
}

static int bytecode_get_line_info (ECTX ectx, c_state_t *c)
{
	tbc_t		*tbc = c->data.bc->tbc;
	tbc_dbg_t 	*dbg = tbc->debug;
	unsigned int	iptr = (unsigned int) c->p.argv[0].data.word;
	
	if (iptr >= ((unsigned int) tbc->bytecode))
		iptr -= (unsigned int) tbc->bytecode;

	if (dbg != NULL && iptr < tbc->bytecode_len) {
		int shift 	= (dbg->n_lnd >> 1);
		int i		= shift;

		while (i >= 0 && i < dbg->n_lnd) {
			if (shift > 1)
				shift >>= 1;
			
			if (dbg->lnd[i].offset <= iptr) {
				if (((i + 1) < dbg->n_lnd) && (dbg->lnd[i + 1].offset > iptr)) {
					break;
				} else {
					i += shift;
				}
			} else {
				i -= shift;
			}
		}
		
		if (i >= 0 && i < dbg->n_lnd) {
			return send_message (ectx, c, &(bytecode_re[BYTECODE_RE_LINE_INFO]),
				dbg->lnd[i].file,
				dbg->lnd[i].line
			);
		}
	}
	
	return send_message (ectx, c, &(bytecode_re[BYTECODE_RE_ERROR]), 0);
}

static int bytecode_get_details (ECTX ectx, c_state_t *c)
{
	tbc_t *tbc = c->data.bc->tbc;
	return send_message (
		ectx, c, &(bytecode_re[BYTECODE_RE_DETAILS]),
		tbc->ws, tbc->vs, tbc->bytecode_len
	);
}

static int bytecode_get_tlp (ECTX ectx, c_state_t *c)
{
	tbc_tlp_t *tlp = c->data.bc->tbc->tlp;
	
	if (tlp != NULL) {
		return send_message (
			ectx, c, &(bytecode_re[BYTECODE_RE_TLP]),
			str_to_mt (ectx, tlp->fmt),
			str_to_mt (ectx, tlp->symbol)
		);
	} else {
		return send_message (ectx, c, &(bytecode_re[BYTECODE_RE_ERROR]), 0);
	}
}
/*}}}*/

/*{{{  CT.VM */
/*
PROTOCOL P.VM.RQ
  CASE
    decode.bytecode = 0; MOBILE []BYTE -- TBC data
    load.bytecode   = 1; MOBILE []BYTE -- resource path
:
PROTOCOL P.VM.RE
  CASE
    bytecode        = 0; CT.BYTECODE!
    error           = 1; INT
:
CHAN TYPE CT.VM
  MOBILE RECORD
    CHAN P.VM.RQ request?:
    CHAN P.VM.RE response!:
:
*/

static int vm_rq_decode_bytecode (ECTX, c_state_t *);
static int vm_rq_load_bytecode (ECTX, c_state_t *);

static p_sym_t vm_rq[] = {
	{ .entry = 0, .symbols = "M", .dispatch = vm_rq_decode_bytecode },
	{ .entry = 1, .symbols = "M", .dispatch = vm_rq_load_bytecode },
	{ .symbols = NULL }
};

enum {
	VM_RE_BYTECODE	= 0,
	VM_RE_ERROR	= 1
};
static p_sym_t vm_re[] = {
	{ .entry = VM_RE_BYTECODE,	.symbols = "M",	.dispatch = NULL },
	{ .entry = VM_RE_ERROR,		.symbols = "w",	.dispatch = NULL },
	{ .symbols = NULL }
};

static int vm_rq_decode_bytecode (ECTX ectx, c_state_t *c)
{
	WORDPTR 	mt	= c->p.argv[0].data.mt;
	bytecode_t	*bc	= (bytecode_t *) malloc (sizeof (bytecode_t));
	c_state_t	*cb_c;
	WORDPTR 	cb;

	/* FIXME: verify mobile type */
	
	bc->refcount	= 1;
	bc->source 	= NULL;
	bc->length	= (int)	read_word (wordptr_plus (mt, 1));
	bc->data	= (BYTE *) malloc (bc->length);
	memcpy (bc->data, 
		(BYTE *) wordptr_real_address ((WORDPTR) read_word (wordptr_plus (mt, 0))),
		bc->length
	);

	tvm_mt_release (ectx, mt);

	if ((bc->tbc = decode_tbc (bc->data, bc->length)) == NULL) {
		free_bytecode (bc);
		return send_message (ectx, c, &(vm_re[VM_RE_ERROR]), 0);
	}

	cb		= allocate_cb (ectx, bytecode_rq, &cb_c);
	cb_c->type 	= C_T_BYTECODE;
	cb_c->data.bc	= bc;

	return send_message (ectx, c, &(vm_re[VM_RE_BYTECODE]), cb);
}

static int vm_rq_load_bytecode (ECTX ectx, c_state_t *c)
{
	WORDPTR 	mt	= c->p.argv[0].data.mt;
	bytecode_t	*bc;
	c_state_t	*cb_c;
	WORDPTR 	cb;
	char		*file;
	int		length;

	length	= (int) read_word (wordptr_plus (mt, 1));
	file	= (char *) malloc (length + 1);
	memcpy (file, 
		(BYTE *) wordptr_real_address ((WORDPTR) read_word (wordptr_plus (mt, 0))),
		length
	);
	file[length] = '\0';
	tvm_mt_release (ectx, mt);

	bc = load_bytecode (file);
	free (file);

	if (bc == NULL) {
		return send_message (ectx, c, &(vm_re[VM_RE_ERROR]), 0);
	}

	cb		= allocate_cb (ectx, bytecode_rq, &cb_c);
	cb_c->type 	= C_T_BYTECODE;
	cb_c->data.bc	= bc;

	return send_message (ectx, c, &(vm_re[VM_RE_BYTECODE]), cb);
}
/*}}}*/


/*{{{  Virtual Channel 0 - outputs VM channel bundles */
int vc0_mt_in (ECTX ectx, WORDPTR address)
{
	c_state_t 	*c;
	WORDPTR 	cb = allocate_cb (ectx, vm_rq, &c);

	if (cb == (WORDPTR) NULL_P) {
		return ectx->set_error_flag (ectx, EFLAG_MT);
	}

	/* Setup as VM type */
	c->type 	= C_T_VM;
	c->data.vm 	= ectx;

	/* Reduce reference count to 1 */
	tvm_mt_release (ectx, cb);
	
	/* Pass remaining reference to client */
	write_word (address, (WORD) cb);

	return ECTX_CONTINUE;
}
/*}}}*/

#endif /* TVM_EXTERNAL_CHANNEL_BUNDLES */
