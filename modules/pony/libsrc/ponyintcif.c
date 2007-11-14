// Copyright: Adam Sampson, Fred Barnes, Mario Schweigler (C) 2005-2006
// Institution: Computing Laboratory, University of Kent, Canterbury, UK
// Description: pony internal CIF C code file

/*
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *	MA 02110-1301, USA.
 */

/*{{{  includes*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/socket.h>

#include "cifccsp.h"
#define CIFTRACE_NAME "pony"
#include "ciftrace.h"
#include "dmem_if.h"
#include "typedesc.h"

#ifndef CHANTYPEDESC
#error missing support for channel-type descriptors
#endif
#ifndef CHANTYPEUIO
#error missing support for channel-type user operations
#endif
#ifndef CHANTYPESTATE
#error missing support for channel-type state field
#endif

/*}}}*/
/*{{{  structures*/
typedef struct chantype_BASIC {
        int refcount;
        unsigned int *typedesc;
        void *uiohook;
	Channel channels[0];
} ct_BASIC;

typedef struct chantype_BASIC_end {
	CTSem clisem;
	CTSem svrsem;
	int state;
	CTSem statesem;
} ct_BASIC_end;

#define ct_size(typedesc) (sizeof (ct_BASIC) + (sizeof (Channel) * typedesc_chantype_nchans (typedesc)) + sizeof (ct_BASIC_end))
#define ct_end(ctb) ((ct_BASIC_end *)(((char *)ctb) + sizeof (ct_BASIC) + (sizeof (Channel) * typedesc_chantype_nchans (ctb->typedesc))))
/*}}}*/
/*{{{  protocol tag values*/
enum PDI_tags {
	PDI_activate = 0,
	PDI_make_ctb_networked_confirm,
	PDI_ack,
	PDI_cancel,
	PDI_cancel_encode,
	PDI_cancel_encode_ack,
	PDI_term
};

enum PDO_tags {
	PDO_first_clc = 0,
	PDO_rest_clcs,
	PDO_data_item_nlc,
	PDO_make_ctb_networked,
	PDO_ct_end_nlc,
	PDO_cancel_confirm,
	PDO_encode_cancelled,
	PDO_encode_not_cancelled
};

enum PEI_tags {
	PEI_first_clc = 0,
	PEI_rest_clcs,
	PEI_data_item_nlc,
	PEI_increase_ref_count,
	PEI_alloc_new_ctb,
	PEI_alloc_new_ctb_confirm,
	PEI_cancel,
	PEI_term
};

enum PEO_tags {
	PEO_alloc_new_ctb_confirm = 0,
	PEO_ack
};
/*}}}*/
/*{{{ forward declarations */
static void init_ctb (ct_BASIC *ctb, unsigned int *typedesc);
static void make_ctb_networked (ct_BASIC *ctb, unsigned int *typedesc, int nct_id, ct_BASIC *nhh, ct_BASIC **dha, int dhs, ct_BASIC **eha, int ehs);
/*}}}*/

/*{{{ typedesc helper functions */
/*{{{ static inline unsigned int typedesc_id (unsigned int typedesc) */
/*
 *	returns the type ID from the start of a typedesc
 */
static inline unsigned int typedesc_id (unsigned int typedesc) {
	return typedesc >> 24;
}
/*}}}*/
/*{{{ static inline unsigned int typedesc_len (unsigned int typedesc) */
/*
 *	returns the typedesc length from the start of a typedesc
 */
static inline unsigned int typedesc_len (unsigned int typedesc) {
	return (typedesc & 0x00FFFFFF) / sizeof (unsigned int);
}
/*}}}*/
/*{{{  static int typedesc_chantype_nchans (unsigned int *tdesc)*/
/*
 *	returns the number of channels in a channel-type given its type-descriptor
 */
static int typedesc_chantype_nchans (unsigned int *tdesc)
{
	if (typedesc_id (tdesc[0]) != MTID_CHANTYPE) {
		CFATAL ("typedesc_chantype_nchans: not a channel type\n");
	}
	return (int) tdesc[1];
}
/*}}}*/
/*
 *	returns the number of server-read channels in a channel-type given its type-descriptor
 */
static int typedesc_chantype_server_read (unsigned int *tdesc)
{
	int i, nchans, count = 0;

	if (typedesc_id (tdesc[0]) != MTID_CHANTYPE) {
		CFATAL ("typedesc_chantype_server_read: not a channel type\n");
	}

	nchans = typedesc_chantype_nchans (tdesc);
	tdesc += 3;
	for (i = 0; i < nchans; i++) {
		const unsigned int id = typedesc_id (tdesc[0]);

		switch (id) {
		case MTID_CHAN_I:
			count++;
			break;
		case MTID_CHAN_O:
			break;
		default:
			CFATAL ("typedesc_chantype_server_read: expected input or output channel, found %d %08x\n", id, tdesc[0]);
		}

		tdesc += typedesc_len (tdesc[0]);
	}

	return count;
}
/*}}}*/

/*{{{ static mobile pool */
/*
 *	Static mobiles, as far as KRoC's concerned, are usually allocated by
 *	the compiler from mobilespace, and are never freed. You can request a
 *	fixed-size chunk of mobilespace from the compiler when doing a #PRAGMA
 *	EXTERN for a C process, but there isn't currently a (nice) way of doing
 *	dynamic allocation.
 *
 *	Since ponyintcif processes may be created and deleted many times during
 *	the lifetime of a program, we've got a few options for mobilespace
 *	allocation:
 *	- request a big chunk of mobilespace in the #PRAGMA EXTERN for
 *	  alloc.ctb.*, which'd result in us getting four chunks of mobilespace
 *	  (since you get one per call in the program), and do allocation from
 *	  it -- this could result in us running out of it at some later point,
 *	  which would be unpleasant;
 *	- have a fake mobilespace pool per-walker, which would mean that we'd
 *	  be allocating a chunk of memory each time we started a walker that
 *	  never got freed -- although we could guarantee we allocate precisely
 *	  the right amount of memory by walking the type definition at
 *	  allocation time;
 *	- have a single shared pool of mobilespace objects that all walkers can
 *	  draw upon -- this means that, while we're unavoidably going to leak
 *	  memory at the end of the program (since some of our pointers might be
 *	  in other processes), we should at least keep our memory use fairly
 *	  constant throughout the life of the program.
 *
 *	Carl Ritson came up with a clever hack that provides a fourth
 *	alternative: we (the tree-walkers) know that we're a leaf in the
 *	process graph. As such, whenever a process does a static mobile
 *	communication with us, we know that the object it gives us is only
 *	needed for the duration of the communication.  We can thus cheat by
 *	doing two swaps rather than one: take the object (swapping with a dummy
 *	value), read/write our data, and give it back immediately.  This means
 *	we don't have to allocate in mobilespace at all, because the compiler's
 *	done it for us! You'd need a CIF binding for extended output in order
 *	to do this.
 *
 *	For now, this implements the third option, in a rather inefficient way
 *	for simplicity's sake.
 */

typedef struct spare_mobile {
	void *ptr;
	size_t size;
	struct spare_mobile *next;
} spare_mobile;

static spare_mobile spare_mobile_head = { NULL, -1, NULL };

static void *get_static_mobile (size_t size) {
	spare_mobile *prev = &spare_mobile_head;
	spare_mobile *sm;

	for (sm = prev->next; sm != NULL; sm = sm->next) {
		if (sm->size == size) {
			void *ptr = sm->ptr;
			CTRACE ("size %d; use existing 0x%08x\n", size, (unsigned int) ptr);
			prev->next = sm->next;
			DMemFree (sm);
			return ptr;
		}
	}

	/* No existing static mobiles available; allocate a new chunk of memory. */
	CTRACE ("size %d; allocate new\n");
	return DMemAlloc (size);
}

static void return_static_mobile (void *ptr, size_t size) {
	spare_mobile *sm = DMemAlloc (sizeof *sm);

	CTRACE ("size %d; at 0x%08x\n", size, ptr);
	sm->ptr = ptr;
	sm->size = size;
	sm->next = spare_mobile_head.next;
	spare_mobile_head.next = sm;
}
/*}}}*/

/*{{{ type-walker */
/*{{{ Protocols and types */
/*
 *	The following interaction pattern is allowed along the links to the type-walker process:
 *		(
 *			( PWI_reset ; PW_count -> PWO_counts ; <result> ->
 *				PWI_reset ; PW_output -> ( PWO_more -> PWI_next )* -> ( ( PWO_more -> PWI_stop ) | PWO_done ) ->
 *				( PWI_output_ok | PWI_output_cancelled ) ) |
 *			( PWI_reset ; PW_cancel -> ( PWO_more -> PWI_stop) | PWO_done ) |
 *			( PWI_reset ; PW_input -> ( PWO_more -> PWI_next ) )* -> ( ( PWO_more -> PWI_stop ) | PWO_done )
 *		)* -> PWI_term
 */

/* Modes that the type-walker can operate in. */
typedef enum {
	PW_count,   /* Counting data and chantype items */
	PW_output,  /* Being a decode.handler in regular mode */
	PW_cancel,  /* Being a decode.handler in cancelling mode */
	PW_input    /* Being an encode.handler */
} pony_walk_mode;

/* Tagged protocol from decode/encode.handler to type-walker. */
enum PWI_tags {
	PWI_reset, /* ; pony_walk_mode mode */
	PWI_next,
	PWI_stop,
	PWI_output_ok,
	PWI_output_cancelled,
	PWI_term
};

/* Tagged protocol from type-walker to decode/encode.handler. */
enum PWO_tags {
	PWO_counts, /* ; const pony_walk_counts *counts */
	PWO_more,
	PWO_done
};

typedef struct {
	int di_first;
	int ct_first;
	int di_rest;
	int ct_rest;
} pony_walk_counts;

/* Types of items on comm_list */
typedef enum {
	CL_static_mobile,
	CL_dynamic_mobile,
	CL_ctb
} comm_list_type;

typedef struct {
	void *ptr;
	size_t size;
	comm_list_type type;
} comm_list_item;

typedef struct {
	pony_walk_mode mode;

	Channel *pwi, *pwo, *user, *to_kern, *from_kern;
	unsigned int *pdesc;

	int clc;
	int clc_total;
	pony_walk_counts counts;

	void **temp_list;
	int temp_list_used;
	int temp_list_size;

	comm_list_item *comm_list;
	int comm_list_used;
	int comm_list_size;
} pony_walk_state;

#define SCTRACE(s, format, args...) \
	CTRACE("[Chan#%08x Mode%d CLC%d] " format, (unsigned int) (s)->user, (s)->mode, (s)->clc, ##args)

/*}}}*/
/*{{{ static void *vector_add (void **list, size_t item_size, int *used, int *size, int default_size) */
/*
 *	add an item to a dynamically-sized array
 */
static void *vector_add (void **list, size_t item_size, int *used, int *size, int default_size)
{
	void *new_list, *ptr;

	if (*used == *size) {
		if (*size == 0) {
			*size = default_size;
		} else {
			*size *= 2;
		}
		CTRACE ("%d used; stretched list to %d items\n", *used, *size);
		new_list = DMemAlloc (*size * item_size);
		if (*list != NULL) {
			memcpy (new_list, *list, *used * item_size);
			DMemFree (*list);
		}
		*list = new_list;
	}

	ptr = ((char *) *list) + *used * item_size;
	*used += 1;
	return ptr;
}
/*}}}*/
/*{{{ static void temp_list_add (pony_walk_state *s, void *ptr) */
/*
 *	add a block of DMemAlloc-allocated memory to those to free at the end of this walk
 */
static void temp_list_add (pony_walk_state *s, void *ptr) {
	void **p = vector_add ((void **) &s->temp_list, sizeof *s->temp_list, &s->temp_list_used, &s->temp_list_size, 16);
	CTRACE ("adding to temp list: 0x%08x\n", (unsigned int) ptr);
	*p = ptr;
}
/*}}}*/
/*{{{ static void comm_list_add (pony_walk_state *s, void *ptr, size_t size, comm_list_type type) */
/*
 *	add a communicated item to those to free at the end of the walk, if not cancelled
 */
static void comm_list_add (pony_walk_state *s, void *ptr, size_t size, comm_list_type type)
{
	comm_list_item *item;

	if (ptr == NULL) {
		CFATAL ("comm_list_add: adding NULL pointer to comm_list\n");
	}

	item = vector_add ((void **) &s->comm_list, sizeof *s->comm_list, &s->comm_list_used, &s->comm_list_size, 16);
	item->ptr = ptr;
	item->size = size;
	item->type = type;
}
/*}}}*/
/*{{{ static void *alloc_temp_mem (pony_walk_state *s, size_t size) */
/*
 *	allocate temporary memory for the duration of this walk
 */
static void *alloc_temp_mem (pony_walk_state *s, size_t size)
{
	void *mem = DMemAlloc (size);
	CTRACE ("allocated %d bytes at %08x\n", size, (unsigned int) mem);
	temp_list_add (s, mem);
	return mem;
}
/*}}}*/
/*{{{ static void process_temp_list (pony_walk_state *s) */
/*
 *	free temporary memory
 */
static void process_temp_list (pony_walk_state *s)
{
	int i;

	CTRACE ("freeing %d temporary items\n", s->temp_list_used);
	for (i = 0; i < s->temp_list_used; i++) {
		CTRACE ("freeing item %d: 0x%08x\n", i, (unsigned int) s->temp_list[i]);
		DMemFree (s->temp_list[i]);
	}
	s->temp_list_used = 0;
}
/*}}}*/
/*{{{ static void process_comm_list (pony_walk_state *s) */
/*
 *	free communicated items
 */
static void process_comm_list (pony_walk_state *s)
{
	int i;

	CTRACE ("freeing %d items\n", s->comm_list_used);
	for (i = 0; i < s->comm_list_used; i++) {
		comm_list_item *item = &s->comm_list[i];
		CTRACE ("freeing item %d: type %d ptr 0x%08x\n", i, item->type, (unsigned int) item->ptr);

		switch (item->type) {
		case CL_dynamic_mobile:
			DMemFree ((void *) item->ptr);
			break;
		case CL_static_mobile:
			return_static_mobile (item->ptr, item->size);
			break;
		case CL_ctb:
			{
				ct_BASIC *ctb = item->ptr;
				CTRACE ("deref CTB 0x%08x\n", (unsigned int) ctb);
				ctb->refcount--;
				if (ctb->refcount == 0 && ctb->uiohook == NULL) {
					CTRACE ("freeing non-networked CTB\n");
					DMemFree (ctb);
				}
			}
			break;
		default:
			CFATAL ("process_comm_list: unknown item type %d on comm_list\n", item->type);
		}
	}
	s->comm_list_used = 0;
}
/*}}}*/
/*{{{ static void free_walk_state (pony_walk_state *s) */
/*
 *	release any memory used by the walk state
 */
static void free_walk_state (pony_walk_state *s)
{
	process_temp_list (s);
	if (s->temp_list != NULL) {
		DMemFree (s->temp_list);
	}

	process_comm_list (s);
	if (s->comm_list != NULL) {
		DMemFree (s->comm_list);
	}
}
/*}}}*/
/*{{{ static void *copy_di (pony_walk_state *s, void *data, size_t size) */
/*
 *	copy a data item into temporary memory if necessary
 */
static void *copy_di (pony_walk_state *s, void *data, size_t size)
{
	/* Since the pony kernel expects to be able to hang on to
	 * pointers it's given for the duration of the ULC (since it wants to be able
	 * to writev() them), we have to copy the data the user's given us.
	 */
	void *data_copy;
	if (s->clc == 0 || s->clc == (s->clc_total - 1)) {
		data_copy = data;
	} else {
		data_copy = alloc_temp_mem (s, size);
		memcpy (data_copy, data, size);
		CTRACE ("copied data item, size %d, from 0x%08x to 0x%08x\n", size, (unsigned int) data, (unsigned int) data_copy);
	}

	return data_copy;
}
/*}}}*/
/*{{{ static void output_di (pony_walk_state *s, void *data, size_t size) */
/*
 *	output a data item to the pony kernel
 */
static void output_di (pony_walk_state *s, void *data, size_t size)
{
	if (s->mode == PW_cancel) {
		SCTRACE (s, "cancel CLC %d\n");
	} else {
		data = copy_di (s, data, size);

		SCTRACE (s, "output CLC %d data.item.nlc; data 0x%08x; size %d\n", s->clc, (unsigned int) data, size);
		ChanOutChar (s->to_kern, PDO_data_item_nlc);
		ChanOutInt (s->to_kern, (int) data);
		ChanOutInt (s->to_kern, (int) size);
	}
}
/*}}}*/
/*{{{ static void input_di (pony_walk_state *s, void **addr, size_t size) */
/*
 *	input a data item from the pony kernel
 */
static void input_di (pony_walk_state *s, void **addr, size_t size)
{
	size_t recv_size;
	char tag;

	SCTRACE (s, "reading data.item.nlc\n");
	ChanInChar (s->from_kern, &tag);
	if (tag != PEI_data_item_nlc) {
		CFATAL ("input_di: expected data.item.nlc, got %d\n", tag);
	}
	ChanInInt (s->from_kern, (int *) addr);
	ChanInInt (s->from_kern, (int *) &recv_size);
	SCTRACE (s, "... read data.item.nlc; data %08x; size %d\n", *(unsigned int *) addr, recv_size);
	if (recv_size != size) {
		CFATAL ("input_di: expected size %d in data.item.nlc, got size %d\n", size, recv_size);
	}
}
/*}}}*/
/*{{{ static void count_di (pony_walk_state *s, int num) */
/*
 *	add data items to the count
 */
static void count_di (pony_walk_state *s, int num)
{
	if (s->clc == 0) {
		s->counts.di_first += num;
	} else {
		s->counts.di_rest += num;
	}
}
/*}}}*/
/*{{{ static void *get_data (pony_walk_state *s) */
/*
 *	get the address of the sending process's data
 */
static void *get_data (pony_walk_state *s)
{
	if (s->mode == PW_input) {
		/* There isn't a sending process. */
		return NULL;
	} else {
		Process *other = (Process *)(*s->user);
		if (other == NULL) {
			CFATAL ("get_data: no sending process for channel at 0x%08x (channel word is NULL)\n", (unsigned int) s->user);
		}
		return (void *) other[-4];
	}
}
/*}}}*/
/*{{{ static void process_di (pony_walk_state *s, size_t size, void *save_at) */
/*
 *	process a data item
 */
static void process_di (pony_walk_state *s, void **addr, size_t size)
{
	switch (s->mode) {
	case PW_count:
		count_di (s, 1);
		break;
	case PW_output:
	case PW_cancel:
		output_di (s, *addr, size);
		break;
	case PW_input:
		input_di (s, addr, size);
		break;
	}
}
/*}}}*/
/*{{{ static void release_user (pony_walk_state *s) */
/*
 *	release the user process from a communication
 */
static void release_user (pony_walk_state *s)
{
	if (s->mode == PW_output || s->mode == PW_cancel) {
		SCTRACE (s, "releasing user process\n");
		ChanXEnd (s->user);
		ChanXAble (s->user);
	}
}
/*}}}*/
/*{{{ static int new_clc (pony_walk_state *s, int inhibit_release) */
/*
 *	start a new CLC
 */
static int new_clc (pony_walk_state *s, int inhibit_release)
{
	s->clc++;
	SCTRACE (s, "new CLC\n");

	if (s->mode != PW_count) {
		char c;

		SCTRACE (s, "asking whether to continue\n");
		ChanOutChar (s->pwo, PWO_more);
		ChanInChar (s->pwi, &c);
		if (c == PWI_stop) {
			SCTRACE (s, "got PWI_stop, stopping here\n");
			return 1;
		} else if (c != PWI_next) {
			CFATAL ("new_clc: expected PWI_stop or PWI_next, got %d\n", c);
		}
		CTRACE ("got PWI_next, continuing\n");
	}

	if (!inhibit_release) {
		release_user (s);
	}

	return 0;
}
/*}}}*/
/*{{{ static inline int is_primitive (unsigned int id) */
/*
 *	say whether a typedesc id is a primitive type
 */
static inline int is_primitive (unsigned int id)
{
	return id == MTID_PRIM || id == MTID_RECORD;
}
/*}}}*/
/*{{{ static int walk_inner_primitive (pony_walk_state *s, int item_count, void **data, size_t *size) */
/*
 *	walk part of a type descriptor for a single primitive communication
 */
static int walk_inner_primitive (pony_walk_state *s, int item_count, void **data, size_t *size)
{
	const unsigned int type = typedesc_id (s->pdesc[0]);
	const unsigned int type_len = typedesc_len (s->pdesc[0]);
	unsigned int *orig_pdesc = s->pdesc;
	s->pdesc++;

	SCTRACE (s, "type is %d length %d\n", type, type_len);
	switch (type) {
	case MTID_PRIM:
	case MTID_RECORD:
		{
			long long total_size = s->pdesc[0] * item_count;
			if (s->mode == PW_output && total_size >= 0x80000000) {
				CFATAL ("walk_inner_primitive: attempting to send %lld-byte data item; this won't fit in the count\n");
			}
			*size = (size_t) total_size;

			process_di (s, data, *size);
			s->pdesc += 2;
		}
		break;
	case MTID_ARRAY:
		{
			const unsigned int nitems = s->pdesc[0];
			s->pdesc++;

			CTRACE ("array with %d items; this dimension is %d\n", nitems * item_count, nitems);
			if (walk_inner_primitive (s, nitems * item_count, data, size)) {
				return 1;
			}
		}
		break;
	default:
		CFATAL ("walk_inner_primitive: expected primitive type; this is type %d length %d\n", type, type_len);
	}

	if (s->pdesc != orig_pdesc + type_len) {
		CFATAL ("walk_inner: typedesc size mismatch: type %d length %d, measured %d\n", type, type_len, s->pdesc - orig_pdesc);
	}

	SCTRACE (s, "done\n");
	return 0;
}
/*}}}*/
/*{{{ static int walk_inner (pony_walk_state *s) */
/*
 *	walk part of a type descriptor
 */
static int walk_inner (pony_walk_state *s)
{
	const unsigned int type = typedesc_id (s->pdesc[0]);
	const unsigned int type_len = typedesc_len (s->pdesc[0]);
	unsigned int *orig_pdesc = s->pdesc;
	s->pdesc++;

	SCTRACE (s, "type is %d length %d\n", type, type_len);
	switch (type) {
	case MTID_PRIM:
	case MTID_RECORD:
	case MTID_ARRAY:
		{
			void *data = get_data (s);
			size_t size;

			s->pdesc--;
			if (walk_inner_primitive (s, 1, &data, &size)) {
				return 1;
			}
			if (s->mode == PW_input) {
				SCTRACE (s, "doing ChanOut; data 0x%08x; size %d\n", (unsigned int) data, size);
				ChanOut (s->user, data, size);
				SCTRACE (s, "ChanOut done\n");
				temp_list_add (s, data);
			}
		}
		break;
	case MTID_SEQPROTO:
		{
			unsigned int i;
			const unsigned int nitems = s->pdesc[0];
			s->pdesc++;

			for (i = 0; i < nitems; i++) {
				SCTRACE (s, "item %d of %d in protocol\n", i, nitems);
				if (i > 0) {
					if (new_clc (s, 0)) {
						return 1;
					}
				}
				if (walk_inner (s)) {
					return 1;
				}
				SCTRACE (s, "done item %d of %d\n", i, nitems);
			}
		}
		break;
	case MTID_TAGPROTO:
		{
			char tag;
			void *tag_addr = get_data (s);
			unsigned int i;
			const unsigned int ntags = s->pdesc[0];
			s->pdesc++;

			SCTRACE (s, "tagged protocol with %d possible tags\n", ntags);
			process_di (s, &tag_addr, 1);
			if (s->mode == PW_input) {
				SCTRACE (s, "doing ChanOut; data 0x%08x; size 1\n", (unsigned int) tag_addr);
				ChanOut (s->user, tag_addr, 1);
				SCTRACE (s, "ChanOut done\n");
			}
			tag = *(char *) tag_addr;
			if (s->mode == PW_input) {
				temp_list_add (s, tag_addr);
			}
			SCTRACE (s, "... and this tag is %d\n", tag);

			for (i = 0; i < ntags; i++) {
				const unsigned int tag_id = typedesc_id (s->pdesc[0]);
				const unsigned int tag_len = typedesc_len (s->pdesc[0]);

				if (tag_id != MTID_TAG) {
					CFATAL ("walk_inner: expected MTID_TAG, found type %d length %d\n", tag_id, tag_len);
				}

				if (s->pdesc[1] == tag) {
					unsigned int j;
					const unsigned int nitems = s->pdesc[2];
					s->pdesc += 3;

					CTRACE ("found tag; it's number %d\n", i);
					for (j = 0; j < nitems; j++) {
						CTRACE ("item %d of %d in tagged protocol\n", i, nitems);
						if (new_clc (s, 0)) {
							return 1;
						}
						if (walk_inner (s)) {
							return 1;
						}
					}

					/* Skip over the remaining tags. */
					s->pdesc = orig_pdesc + type_len;
					break;
				} else {
					CTRACE ("it's not tag %d\n", i);
					s->pdesc += tag_len;
				}
			}
		}
		break;
	case MTID_COUNTED:
		{
			/* This actually requires either one or two CLCs from
			 * the occam point of view (the count, and the data
			 * only if the count isn't zero), but we're only going
			 * to tell pony about one, since we can figure the
			 * count out from the data size when it's received.
			 *
			 * However, when the count is the first (occam) CLC, we
			 * have to do this as two pony CLCs, so that it can be
			 * cancelled after the first one.
			 */

			const int first_clc = (s->clc == 0);
			long long count = 0;
			const unsigned int count_id = typedesc_id (s->pdesc[0]);
			const unsigned int count_len = typedesc_len (s->pdesc[0]);
			unsigned int subtype_id, subtype_len;
			int count_size, subtype_size;

			CTRACE ("handling counted array as (probably) %d CLCs\n", first_clc ? 2 : 1);

			if (!is_primitive (count_id)) {
				CFATAL ("walk_inner: expected primitive counted array count type, found type %d length %d\n", count_id, count_len);
			}
			count_size = s->pdesc[1];
			s->pdesc += 3;

			subtype_id = typedesc_id (s->pdesc[0]);
			subtype_len = typedesc_len (s->pdesc[0]);
			if (!is_primitive (subtype_id)) {
				CFATAL ("walk_inner: expected primitive counted array data type, found type %d length %d\n", subtype_id, subtype_len);
			}
			subtype_size = s->pdesc[1];
			s->pdesc += 3;

			switch (s->mode) {
			case PW_count:
				count_di (s, 1);
				if (first_clc) {
					/* Special case: if this is the first CLC, and the count is zero,
					 * then we must only send *one* NLC, so that the pony kernel knows
					 * the communication is completed before the user process is released
					 * -- else we get deadlock inside the pony kernel. Argh! */
					/* FIXME: get rid of count mode entirely in favour of buffering */
					CTRACE ("reading count %d bytes from 0x%08x\n", count_size, (unsigned int) get_data (s));
					memcpy (&count, get_data (s), count_size);
					CTRACE ("counted array count is %lld\n", count);

					if (count != 0) {
						if (new_clc (s, 1)) {
							return 1;
						}
						count_di (s, 1);
					} else {
						CTRACE ("... so we're actually handling it as *one* CLC\n");
					}
				}
				break;
			case PW_output:
			case PW_cancel:
				{
					/* XXX: Endianness-dependent code. */
					memcpy (&count, get_data (s), count_size);
					CTRACE ("counted array count is %lld\n", count);

					if (count == 0) {
						if (first_clc) {
							/* As above: this is the first CLC, and the count is zero, so
							 * the count is the only NLC we send. */
							CTRACE ("... so we're actually handling it as *one* CLC\n");
							CTRACE ("output counted array size CLC %d\n", s->clc);
							if (s->mode == PW_output) {
								void *count_data = alloc_temp_mem (s, count_size);
								memcpy (count_data, &count, count_size);

								ChanOutChar (s->to_kern, PDO_data_item_nlc);
								ChanOutInt (s->to_kern, (int) count_data);
								ChanOutInt (s->to_kern, count_size);
							}
						} else {
							if (s->mode == PW_output) {
								CTRACE ("output fake data CLC %d (0, NULL)\n", s->clc);
								ChanOutChar (s->to_kern, PDO_data_item_nlc);
								ChanOutInt (s->to_kern, (int) NULL);
								ChanOutInt (s->to_kern, 0);
							}
						}
					} else {
						void *data;
						const long long data_size = (size_t) count * subtype_size;

						CTRACE ("array data size is %lld\n", data_size);
						if (data_size >= 0x80000000) {
							CFATAL ("walk_inner: counted array data size %lld (%d items of size %d) is too large to send\n", data_size, count, subtype_size);
						}

						if (first_clc) {
							CTRACE ("output counted array size CLC %d\n", s->clc);
							if (s->mode == PW_output) {
								void *count_data = alloc_temp_mem (s, count_size);
								memcpy (count_data, &count, count_size);

								ChanOutChar (s->to_kern, PDO_data_item_nlc);
								ChanOutInt (s->to_kern, (int) count_data);
								ChanOutInt (s->to_kern, count_size);
							}

							if (new_clc (s, 0)) {
								return 1;
							}
						} else {
							release_user (s);
						}
						if (s->mode == PW_output) {
							data = copy_di (s, get_data(s), (size_t) data_size);
							CTRACE ("output counted array data CLC %d\n", s->clc);
							ChanOutChar (s->to_kern, PDO_data_item_nlc);
							ChanOutInt (s->to_kern, (int) data);
							ChanOutInt (s->to_kern, (int) data_size);
						}
					}
				}
				break;
			case PW_input:
				{
					void *recv_data;
					size_t recv_size = 0;
					char tag;

					if (first_clc) {
						CTRACE ("input counted array size CLC %d\n", s->clc);
						ChanInChar (s->from_kern, &tag);
						if (tag != PEI_data_item_nlc) {
							CFATAL ("walk_inner: expected data.item.nlc, got %d\n", tag);
						}
						ChanInInt (s->from_kern, (int *) &recv_data);
						ChanInInt (s->from_kern, (int *) &recv_size);
						if (recv_size > sizeof count || recv_size != count_size) {
							CFATAL ("walk_inner: counted array size type is invalid; was %d, expected %d\n", recv_size, count_size);
						}
						if (count_size == sizeof (int)) {
							CTRACE ("received data %08x size %d contains %08x\n", (unsigned int) recv_data, recv_size, *(int *) recv_data);
						}
						memcpy (&count, recv_data, recv_size);
						temp_list_add (s, recv_data);

						CTRACE ("output counted array size %lld (count size %d; value %016llx)\n", count, count_size, count);
						ChanOut (s->user, &count, count_size);

						if (count != 0) {
							if (new_clc (s, 0)) {
								return 1;
							}
						} else {
							CTRACE ("... so we're actually handling it as *one* CLC\n");
						}
					}

					if (!(first_clc && count == 0)) {
						CTRACE ("input counted array data CLC %d\n", s->clc);
						ChanInChar (s->from_kern, &tag);
						if (tag != PEI_data_item_nlc) {
							CFATAL ("walk_inner: expected data.item.nlc, got %d\n", tag);
						}
						ChanInInt (s->from_kern, (int *) &recv_data);
						ChanInInt (s->from_kern, (int *) &recv_size);

						if (!first_clc) {
							CTRACE ("output counted array size; size is %d, count is %d\n", recv_size, recv_size / subtype_size);
							ChanOutInt (s->user, recv_size / subtype_size);
						}

						if (recv_size != 0) {
							CTRACE ("output counted array data at %08x\n", (unsigned int) recv_data);
							ChanOut (s->user, recv_data, recv_size);
							temp_list_add (s, recv_data);
						}
					}

					CTRACE ("output counted array done\n");
				}
				break;
			}
		}
		break;
	case MTID_MARRAY:
		{
			const unsigned int dimcount = s->pdesc[0];
			const unsigned int subtype_id = typedesc_id (s->pdesc[1]);
			const unsigned int subtype_len = typedesc_len (s->pdesc[1]);
			unsigned int subtype_size;
			CTRACE ("mobile array, dimcount %d, subtype %d length %d\n", dimcount, subtype_id, subtype_len);

			if (!is_primitive (subtype_id)) {
				CFATAL ("walk_inner: mobile array subtype must be primitive; found type %d length %d\n", subtype_id, subtype_len);
			}
			subtype_size = s->pdesc[2];
			CTRACE ("marray subtype item size is %d\n", subtype_size);
			s->pdesc += 4;

			switch (s->mode) {
			case PW_count:
				{
					count_di (s, (dimcount == 1) ? 1 : 2);
				}
				break;
			case PW_output:
			case PW_cancel:
				{
					int i;
					long long data_size;
					unsigned int *block = get_data (s);
					/* This contains the mobile pointer, followed by dimcount dimensions. */

					if (block == NULL) {
						CFATAL ("walk_inner: trying to send undefined mobile array\n");
					}

					CTRACE ("output marray, %d dimensions, block at 0x%08x\n", dimcount, block[0]);
					data_size = subtype_size;
					for (i = 0; i < dimcount; i++) {
						CTRACE ("dimension %d is %d\n", i, block[i + 1]);
						data_size *= block[i + 1];
					}
					CTRACE ("marray data size is %lld\n", data_size);

					if (data_size >= 0x80000000) {
						CFATAL ("walk_inner: mobile array data size too large to transmit\n");
					}

					if (s->mode == PW_output) {
						if (dimcount > 1) {
							CTRACE ("output CLC %d (marray dimension block at 0x%08x)\n", s->clc, (unsigned int) &block[1]);
							ChanOutChar (s->to_kern, PDO_data_item_nlc);
							ChanOutInt (s->to_kern, (int) &block[1]);
							ChanOutInt (s->to_kern, (int) dimcount * sizeof *block);
						}

						CTRACE ("output CLC %d (marray data at 0x%08x)\n", s->clc, block[0]);
						ChanOutChar (s->to_kern, PDO_data_item_nlc);
						ChanOutInt (s->to_kern, (int) block[0]);
						ChanOutInt (s->to_kern, (int) data_size);
					} else {
						CTRACE ("cancelling CLC %d marray\n", s->clc);
					}

					CTRACE ("will free marray data at 0x%08x\n", block[0]);
					comm_list_add (s, (void *) block[0], -1, CL_dynamic_mobile);
				}
				break;
			case PW_input:
				{
					int i;
					long long data_size;
					unsigned int *block = alloc_temp_mem (s, (dimcount + 1) * sizeof *block);
					unsigned int *recv_dims;
					void *recv_data;
					char tag;

					if (dimcount == 1) {
						size_t recv_size;

						CTRACE ("input CLC %d (marray data for single dimension)\n", s->clc);
						ChanInChar (s->from_kern, &tag);
						if (tag != PEI_data_item_nlc) {
							CFATAL ("walk_inner: expected data.item.nlc, got %d\n", tag);
						}
						ChanInInt (s->from_kern, (int *) &recv_data);
						ChanInInt (s->from_kern, (int *) &recv_size);

						block[0] = (unsigned int) DMemAlloc (recv_size);
						memcpy ((void *) block[0], recv_data, recv_size);
						temp_list_add (s, recv_data);
						block[1] = recv_size / subtype_size;
						CTRACE ("received %d items, total size %d\n", block[1], recv_size);
					} else {
						CTRACE ("input CLC %d (marray dimension block)\n", s->clc);
						input_di (s, (void *) &recv_dims, dimcount * sizeof *recv_dims);
						memcpy (&block[1], recv_dims, dimcount * sizeof *recv_dims);
						temp_list_add (s, (void *) recv_dims);

						data_size = subtype_size;
						for (i = 0; i < dimcount; i++) {
							CTRACE ("dimension %d is %d\n", i, block[i + 1]);
							data_size *= block[i + 1];
						}
						CTRACE ("marray data size is %lld\n", data_size);

						if (data_size >= 0x80000000) {
							CFATAL ("walk_inner: received mobile array data size too large to allocate\n");
						}

						CTRACE ("input CLC %d (marray data, size %lld)\n", s->clc, data_size);
						block[0] = (unsigned int) DMemAlloc ((size_t) data_size);
						input_di (s, &recv_data, data_size);
						memcpy ((void *) block[0], recv_data, data_size);
						temp_list_add (s, recv_data);
					}

					CTRACE ("doing ChanMOutN for marray; data 0x%08x; length %d\n", (unsigned int) block, dimcount + 1);
					ChanMOutN (s->user, block, dimcount + 1);
					CTRACE ("ChanMOutN done\n");
				}
				break;
			}
		}
		break;
	case MTID_MOBILE:
		{
			void *data = NULL;
			size_t size;

			if (s->mode == PW_output || s->mode == PW_cancel) {
				data = *(void **) get_data (s);
			}
			CTRACE ("starting static mobile, data 0x%08x\n", (unsigned int) data);
			if (walk_inner_primitive (s, 1, &data, &size)) {
				return 1;
			}
			CTRACE ("done static mobile, data 0x%08x, size %d\n", (unsigned int) data, size);
			if (s->mode == PW_input) {
				void *ptr = get_static_mobile (size);

				memcpy (ptr, data, size);

				CTRACE ("doing ChanMOut for static mobile with ptr %08x\n", (unsigned int) ptr);
				ChanMOut (s->user, &ptr);
				CTRACE ("ChanMOut done, got ptr %08x\n", (unsigned int) ptr);

				comm_list_add (s, ptr, size, CL_static_mobile);
				temp_list_add (s, data);
			}
		}
		break;
	case MTID_MCHANEND_IU:
	case MTID_MCHANEND_OU:
	case MTID_MCHANEND_IS:
	case MTID_MCHANEND_OS:
		{
			const int is_client = (type == MTID_MCHANEND_OU || type == MTID_MCHANEND_OS);
			const int is_shared = (type == MTID_MCHANEND_IS || type == MTID_MCHANEND_OS);

			CTRACE ("got mobile channel end, client %d shared %d\n", is_client, is_shared);

			switch (s->mode) {
			case PW_count:
				if (s->clc == 0) {
					s->counts.ct_first++;
				} else {
					s->counts.ct_rest++;
				}
				break;
			case PW_output:
			case PW_cancel:
				{
					ct_BASIC **data = get_data (s);
					ct_BASIC *ctb;
					ct_BASIC_end *ctb_end;

					if (data == NULL) {
						CFATAL ("walk_inner: trying to send undefined channel end\n");
					}

					ctb = *data;
					ctb_end = ct_end (ctb);

					CTRACE ("CTB at 0x%08x refcount %d typedesc 0x%08x uiohook 0x%08x\n", (unsigned int) ctb, ctb->refcount, (unsigned int) ctb->typedesc, (unsigned int) ctb->uiohook);

					if (s->mode == PW_output) {
						CTSemClaim (&ctb_end->statesem);
						if (ctb->uiohook == NULL) {
							int nct_id, dhs, ehs;
							ct_BASIC *nhh, **dha, **eha;
							char tag;
							int tmp[2];

							CTRACE ("CTB not networked; sending make.ctb.networked; state 0x%08x\n", ctb_end->state);
							ChanOutChar (s->to_kern, PDO_make_ctb_networked);
							ChanOutInt (s->to_kern, (int) ctb);
							ChanOutInt (s->to_kern, ctb_end->state & 0xFFFF);
							ChanOutInt (s->to_kern, ctb_end->state >> 16);
							ChanOutInt (s->to_kern, typedesc_chantype_nchans (ctb->typedesc));
							ChanOutInt (s->to_kern, typedesc_chantype_server_read (ctb->typedesc));

							CTRACE ("reading make.ctb.networked.confirm\n");
							ChanInInt (s->from_kern, &tag);
							if (tag != PDI_make_ctb_networked_confirm) {
								CFATAL ("walk_inner: expected make.ctb.networked.confirm; got %d\n", tag);
							}
							ChanInInt (s->from_kern, (int *) &nct_id);
							ChanInInt (s->from_kern, (int *) &nhh);
							ChanMIn64 (s->from_kern, (long long *) tmp);
							dha = (ct_BASIC **) tmp[0];
							dhs = tmp[1];
							ChanMIn64 (s->from_kern, (long long *) tmp);
							eha = (ct_BASIC **) tmp[0];
							ehs = tmp[1];

							CTRACE ("actually making the CTB networked -- nct.id now 0x%08x\n", nct_id);
							make_ctb_networked (ctb, ctb->typedesc, nct_id, nhh, dha, dhs, eha, ehs);
						}
						CTSemRelease (&ctb_end->statesem);

						CTRACE ("output CT CLC %d; nct.id 0x%08x\n", s->clc, (unsigned int) ct_end (ctb)->state);
						ChanOutChar (s->to_kern, PDO_ct_end_nlc);
						ChanOutInt (s->to_kern, ct_end (ctb)->state); /* the nct.id, now it's networked */
						ChanOutInt (s->to_kern, is_client ? 0 : 1);
						ChanOutInt (s->to_kern, is_shared ? 2 : 1);
						CTRACE ("output CT done\n");
					} else {
						CTRACE ("cancelling CTB\n");
					}

					comm_list_add (s, ctb, -1, CL_ctb);
				}
				break;
			case PW_input:
				{
					/* For channel types, the typehash is actually the typedesc pointer. */
					unsigned int *typedesc = (unsigned int *) s->pdesc[0];
					ct_BASIC *ctb;
					char tag;

					CTRACE ("reading either increase.ref.count or alloc.new.ctb\n");
					ChanInChar (s->from_kern, &tag);
					switch (tag) {
					case PEI_increase_ref_count:
						{
							ChanInInt (s->from_kern, (int *) &ctb);
							CTRACE ("increasing ref count on CTB 0x%08x\n", (unsigned int) ctb);
							ctb->refcount++;
						}
						break;
					case PEI_alloc_new_ctb:
						{
							int nct_id, dhs, ehs;
							ct_BASIC *nhh, **dha, **eha;
							int tmp[2];

							ChanInInt (s->from_kern, &nct_id);
							CTRACE ("alloc new CTB, NCT-ID 0x%08x\n", nct_id);

							ctb = DMemAlloc (ct_size (typedesc));
							init_ctb (ctb, typedesc);

							CTRACE ("sending alloc.new.ctb.confirm\n");
							ChanOutChar (s->to_kern, PEO_alloc_new_ctb_confirm);
							ChanOutInt (s->to_kern, (int) ctb);
							ChanOutInt (s->to_kern, typedesc_chantype_nchans (ctb->typedesc));
							ChanOutInt (s->to_kern, typedesc_chantype_server_read (ctb->typedesc));

							CTRACE ("reading alloc.new.ctb.confirm\n");
							ChanInInt (s->from_kern, &tag);
							if (tag != PEI_alloc_new_ctb_confirm) {
								CFATAL ("walk_inner: expected alloc.new.ctb.confirm; got %d\n", tag);
							}
							ChanInInt (s->from_kern, (int *) &nhh);
							ChanMIn64 (s->from_kern, (long long *) tmp);
							dha = (ct_BASIC **) tmp[0];
							dhs = tmp[1];
							ChanMIn64 (s->from_kern, (long long *) tmp);
							eha = (ct_BASIC **) tmp[0];
							ehs = tmp[1];

							CTRACE ("actually making the CTB networked; CTB is 0x%08x\n", ctb);
							make_ctb_networked (ctb, ctb->typedesc, nct_id, nhh, dha, dhs, eha, ehs);
						}
						break;
					default:
						CFATAL ("walk_inner: expecting increase.ref.count or alloc.new.ctb; got %d\n", tag);
					}

					CTRACE ("sending received end to user\n");
					ChanMOutN (s->user, &ctb, 1);
					CTRACE ("CTB receive done\n");
				}
				break;
			}
			s->pdesc++;
		}
		break;
	/* FIXME: These types are not yet implemented: */
	case MTID_MBARRIER:
	case MTID_MPROC:
	/* These types should never show up in this context: */
	case MTID_CHANTYPE:
	case MTID_CHAN_I:
	case MTID_CHAN_O:
	case MTID_TAG:
	case MTID_STYPE:
	case MTID_FIELD:
	case MTID_UNKNOWN:
	default:
		CFATAL ("walk_inner: unhandled type in typedesc: type %d length %d\n", type, type_len);
	}

	if (s->pdesc != orig_pdesc + type_len) {
		CFATAL ("walk_inner: typedesc size mismatch: type %d length %d, measured %d\n", type, type_len, s->pdesc - orig_pdesc);
	}

	SCTRACE (s, "done\n");
	return 0;
}
/*}}}*/
/*{{{ static void type_walker (Process *me, Channel *pwi, Channel *pwo, Channel *user, Channel *to_kern, Channel *from_kern, unsigned int *pdesc) */
/*
 *	process for walking over type descriptors, doing counting, encoding or decoding as appropriate
 */
static const int type_walker_stacksize = 16384;
static void type_walker (Process *me, Channel *pwi, Channel *pwo, Channel *user, Channel *to_kern, Channel *from_kern, unsigned int *pdesc)
{
	pony_walk_state state;

	state.mode = PW_count;
	state.pwi = pwi;
	state.pwo = pwo;
	state.user = user;
	state.to_kern = to_kern;
	state.from_kern = from_kern;
	state.temp_list = NULL;
	state.temp_list_used = 0;
	state.temp_list_size = 0;
	state.comm_list = NULL;
	state.comm_list_used = 0;
	state.comm_list_size = 0;

	CTRACE ("starting; user channel at 0x%08x; to_kern 0x%08x; from_kern 0x%08x\n", (unsigned int) user, (unsigned int) to_kern, (unsigned int) from_kern);
	while (1) {
		int stopped;
		char c;

		ChanInChar (pwi, &c);
		process_temp_list (&state);
		process_comm_list (&state);
		if (c == PWI_term) {
			break;
		} else if (c != PWI_reset) {
			CFATAL ("pony_walk_type: expected PWI_reset or PWI_term; got %d\n", c);
		}
		ChanInInt (pwi, &state.mode);

		state.pdesc = pdesc;
		state.clc = 0;
		if (state.mode == PW_count) {
			state.clc_total = 0;
		}
		state.counts.di_first = 0;
		state.counts.ct_first = 0;
		state.counts.di_rest = 0;
		state.counts.ct_rest = 0;

		SCTRACE (&state, "walk starting in mode %d\n", state.mode);
		stopped = walk_inner (&state);
		SCTRACE (&state, "walk finished: stopped = %d\n", stopped);

		if (state.mode == PW_count) {
			pony_walk_counts *counts = &state.counts;
			ChanOutChar (pwo, PWO_counts);
			ChanOut (pwo, &counts, sizeof counts);

			state.clc_total = state.clc + 1;
		} else if (!stopped) {
			ChanOutChar (pwo, PWO_done);
		}

		if (state.mode == PW_output) {
			ChanInChar (pwi, &c);
			if (c == PWI_output_ok) {
				SCTRACE (&state, "output walk completed successfully; processing comm_list\n");
				process_comm_list (&state);
			} else if (c == PWI_output_cancelled) {
				SCTRACE (&state, "output walk was cancelled; discarding comm_list\n");
				state.comm_list_used = 0;
			} else {
				CFATAL ("type_walker: expected PWI_output_ok or PWI_output_cancelled\n");
			}
		}
	}

	free_walk_state (&state);
	CTRACE ("terminating\n");
}
/*}}} */
/*}}} */

/*{{{ protocol decoder */
/*{{{ static void try_first_clc (Process *me, Channel *csync, const pony_walk_counts *counts, Channel *dh_out) */
/*
 *	try to send the first.clc message to the pony kernel to say we've got data to send
 */
static const int try_first_clc_stacksize = 4096;
static void try_first_clc (Process *me, Channel *csync, const pony_walk_counts *counts, Channel *dh_out)
{
	ChanOutChar (dh_out, PDO_first_clc);
	ChanOutInt (dh_out, counts->di_first);
	ChanOutInt (dh_out, counts->ct_first);
	ChanOutInt (dh_out, (counts->di_rest + counts->ct_rest) > 0);

	ChanOutChar (csync, 0);
}
/*}}}*/
/*{{{  static void cancel_decode_inner (Process *me, Channel *csync, Channel *dh_in, int *cflag)*/
/*
 *	cancelling helper process (inside decode)
 */
static const int cancel_decode_inner_stacksize = 4096;
static void cancel_decode_inner (Process *me, Channel *csync, Channel *dh_in, int *cflag)
{
	int idx;
	char tag;

	idx = ProcAlt (dh_in, csync, NULL);
	switch (idx) {
		/*{{{  0 -- communication from pony kernel*/
	case 0:
		ChanInChar (dh_in, &tag);
		CTRACE ("got cdi cancel\n");
		if (tag == PDI_cancel) {
			ChanInChar (csync, &tag);
			*cflag = 1;
		} else {
			CFATAL ("cancel_decode_inner: expected cancel, got %d\n", tag);
		}
		break;
		/*}}}*/
		/*{{{  1 -- cancel sync*/
	case 1:
		ChanInChar (csync, &tag);
		CTRACE ("cdi not cancelled\n");
		*cflag = 0;
		break;
		/*}}}*/
		/*{{{  default -- error*/
	default:
		CFATAL ("cancel_decode_inner: bad ProcAlt index %d\n", idx);
		/*}}}*/
	}
}
/*}}}*/
/*{{{  static void pony_protocol_decoder (Process *me, Channel *in, unsigned int *pdesc, ct_BASIC *dhan)*/
/*
 *	pony specific decoder implementation.  "in" is the application-level channel;
 *	"pdesc" points at the constant type description for this channel; "dhan" is a client-end
 *	for communication back to the pony kernel.
 */
static const int pony_protocol_decoder_stacksize = 8192;
static void pony_protocol_decoder (Process *me, Channel *in, unsigned int *pdesc, ct_BASIC *dhan)
{
	Channel pw_in, pw_out;
	Channel *dh_in = &(dhan->channels[0]);
	Channel *dh_out = &(dhan->channels[1]);
	Process *walker;
	int running = 1;

	ChanInit (&pw_in);
	ChanInit (&pw_out);
	walker = ProcAlloc (type_walker, type_walker_stacksize, 6, &pw_in, &pw_out, in, dh_out, dh_in, pdesc);
	ProcFork (walker);

	while (running) {
		char tag;
		int irun;

		CTRACE ("doing input -- managed channel at 0x%08x, control channel at 0x%08x\n", (unsigned int)in, (unsigned int)dh_in);
		ChanIn (dh_in, &tag, 1);
		switch (tag) {
			/*{{{  PDI_activate -- start decoding*/
		case PDI_activate:
			CTRACE ("activate\n");
			irun = 1;
			while (irun) {
				int idx;

				CTRACE ("alting; dh_in=0x%8.8x, in=0x%8.8x, *in=0x%8.8x\n", (unsigned int)dh_in, (unsigned int)in, (unsigned int)*in);
				idx = ProcAlt (dh_in, in, NULL);
				switch (idx) {
					/*{{{  0 -- cancel from pony kernel*/
				case 0:
					CTRACE ("cancel 1\n");
					ChanIn (dh_in, &tag, 1);
					if (tag == PDI_cancel) {
						irun = 0;
						tag = PDO_cancel_confirm;
						ChanOut (dh_out, &tag, 1);
					} else {
						CFATAL ("pony_protocol_decoder: expected cancel, got %d\n", tag);
					}
					CTRACE ("cancel 1 done\n");
					break;
					/*}}}*/
					/*{{{  1 -- from application (extended input)*/
				case 1:
					CTRACE ("input from app, channel=0x%8.8x cword=0x%8.8x (@-2=0x%8.8x, @-4=0x%8.8x, @-6=0x%8.8x)\n", (unsigned int)in, *in, ((unsigned int*)(*in))[-2], ((unsigned int *)(*in))[-4], ((unsigned int*)(*in))[-6]);
					{
						Process *i1, *i2;
						Channel csync;
						int was_cancelled = 0;
						char walk_tag;
						const pony_walk_counts *counts;
						int di_rest, ct_rest;

						CTRACE ("telling walker to count\n");
						ChanOutChar (&pw_in, PWI_reset);
						ChanOutInt (&pw_in, PW_count);
						ChanInChar (&pw_out, &walk_tag);
						if (walk_tag != PWO_counts) {
							CFATAL ("pony_protocol_decoder: expected PWO_counts, got %d\n", walk_tag);
						}
						ChanIn (&pw_out, &counts, sizeof counts);
						CTRACE ("walker count done -- received 0x%08x\n", counts);
						CTRACE ("counts are %d %d %d %d\n", counts->di_first, counts->ct_first, counts->di_rest, counts->ct_rest);
						di_rest = counts->di_rest;
						ct_rest = counts->ct_rest;

						ChanInit (&csync);
						i1 = ProcAlloc (try_first_clc, try_first_clc_stacksize, 3, &csync, counts, dh_out);
						i2 = ProcAlloc (cancel_decode_inner, cancel_decode_inner_stacksize, 3, &csync, dh_in, &was_cancelled);
						ProcPar (i1, i2, NULL);
						ProcAllocClean (i1);
						ProcAllocClean (i2);

						if (was_cancelled) {
							CTRACE ("ifa cancelled\n");
							irun = 0;
							ChanOutChar (dh_out, PDO_cancel_confirm);
							CTRACE ("ifa cancelled done\n");
						} else {
							CTRACE ("ifa not cancelled -- outputting first CLC\n");

							CTRACE ("in=0x%08x telling walker to output\n", (unsigned int) in);
							ChanOutChar (&pw_in, PWI_reset);
							ChanOutInt (&pw_in, PW_output);
							ChanInChar (&pw_out, &walk_tag);
							CTRACE ("in=0x%08x got tag %d\n", (unsigned int) in, walk_tag);

							ChanInChar (dh_in, &tag);
							switch (tag) {
							case PDI_cancel:
								CTRACE ("got cancel from kernel\n");
								irun = 0;
								ChanOutChar (dh_out, PDO_cancel_confirm);
								if (walk_tag != PWO_done) {
									ChanOutChar (&pw_in, PWI_stop);
								}

								ChanOutChar (&pw_in, PWI_output_cancelled);

								CTRACE ("cancel confirmed -- deactivating\n");
								break;
							case PDI_ack:
								/* "Reduce refcount of CTBs for first CLC" --
								 * will happen when walker comm_list is processed.
								 */
								CTRACE ("got ack from kernel\n");
								if (walk_tag != PWO_done) {
									CTRACE ("sending rest.clcs\n");
									ChanOutChar (dh_out, PDO_rest_clcs);
									ChanOutInt (dh_out, di_rest);
									ChanOutInt (dh_out, ct_rest);

									while (walk_tag != PWO_done) {
										ChanOutChar (&pw_in, PWI_next);
										ChanInChar (&pw_out, &walk_tag);
									}
									CTRACE ("in=0x%08x sending done; awaiting ack\n", (unsigned int) in);

									ChanInChar (dh_in, &tag);
									if (tag != PDI_ack) {
										CFATAL ("pony_protocol_decoder: expecting ack\n");
									}
									CTRACE ("in=0x%08x got ack\n", (unsigned int) in);
									/* "Reduce refcount of remaining CTBs" --
									 * will happen when walker comm_list is
									 * processed.
									 */
								}

								ChanOutChar (&pw_in, PWI_output_ok);

								CTRACE ("releasing user process\n");
								ChanXEnd (in);
								break;
							}
						}
					}
					CTRACE ("input from app done\n");
					break;
					/*}}}*/
					/*{{{  default -- error*/
				default:
					CFATAL ("pony_protocol_decoder: bad ProcAlt index %d\n", idx);
					/*}}}*/
				}
			}
			CTRACE ("deactivate\n");
			break;
			/*}}}*/
			/*{{{  PDI_cancel_encode -- cancel encode*/
		case PDI_cancel_encode:
			CTRACE ("cancel.encode\n");
			{
				int idx;
				char tag, walk_tag;

				idx = ProcAlt (dh_in, in, NULL);
				switch (idx) {
				/*{{{  dh_in */
				case 0:
					ChanInChar (dh_in, &tag);
					if (tag != PDI_cancel_encode_ack) {
						CFATAL ("pony_protocol_decoder: expected cancel.encode.ack, got %d\n", tag);
					}
					CTRACE ("encode.not.cancelled\n");
					ChanOutChar (dh_out, PDO_encode_not_cancelled);
					break;
				/*}}}*/
				/*{{{  in */
				case 1:
					CTRACE ("cancelling input from user\n");
					ChanOutChar (&pw_in, PWI_reset);
					ChanOutInt (&pw_in, PW_cancel);
					ChanInChar (&pw_out, &walk_tag);
					CTRACE ("cancelling, received tag %d\n", walk_tag);

					if (walk_tag != PWO_done) {
						ChanOutChar (&pw_in, PWI_stop);
					}

					CTRACE ("releasing protocol encoder after cancel\n");
					ChanXEnd (in);

					CTRACE ("awaiting ack\n");
					ChanInChar (dh_in, &tag);
					if (tag != PDI_cancel_encode_ack) {
						CFATAL ("pony_protocol_decoder: expected cancel.encode.ack, got %d\n", tag);
					}
					CTRACE ("encode.cancelled\n");
					ChanOutChar (dh_out, PDO_encode_cancelled);
					break;
				/*}}}*/
				}
				CTRACE ("done cancelling\n");
			}
			break;
			/*}}}*/
			/*{{{  PDI_term -- terminate*/
		case PDI_term:
			CTRACE ("terminate\n");
			running = 0;
			dhan->refcount--;
			if (!dhan->refcount) {
				DMemFree (dhan);
			}
			break;
			/*}}}*/
			/*{{{  default -- error*/
		default:
			CFATAL ("pony_protocol_decoder: expected PDI_active, PDI_cancel_encode or PDI_term, got %d\n", tag);
			/*}}}*/
		}
	}

	ChanOutChar (&pw_in, PWI_term);
}
/*}}}*/
/*}}}*/

/*{{{ protocol encoder */
/*{{{  static void pony_protocol_encoder (Process *me, Channel *out, unsigned int *pdesc, ct_BASIC *ehan)*/
/*
 *	pony specific encoder implementation.  "out" is the application-level output channel;
 *	"prot_desc" points at the constant type description for this channel; "enc_handle" is a client-end
 *	for communication back to the pony kernel.
 */
static const int pony_protocol_encoder_stacksize = 8192;
static void pony_protocol_encoder (Process *me, Channel *out, unsigned int *pdesc, ct_BASIC *ehan)
{
	Channel pw_in, pw_out;
	Channel *eh_in = &(ehan->channels[0]);
	Channel *eh_out = &(ehan->channels[1]);
	Process *walker;
	int running = 1;

	ChanInit (&pw_in);
	ChanInit (&pw_out);
	walker = ProcAlloc (type_walker, type_walker_stacksize, 6, &pw_in, &pw_out, out, eh_out, eh_in, pdesc);
	ProcFork (walker);

	while (running) {
		char tag;

		CTRACE ("doing input -- managed channel at 0x%08x, control channel at 0x%08x\n", (unsigned int)out, (unsigned int)eh_in);
		ChanIn (eh_in, &tag, 1);
		CTRACE ("done input\n");
		switch (tag) {
			/*{{{  PEI_first_clc -- first CLC from pony kernel*/
		case PEI_first_clc:
			{
				char walk_tag;
				int has_rest;
				const pony_walk_counts counts;

				ChanInInt (eh_in, &counts.di_first);
				ChanInInt (eh_in, &counts.ct_first);
				ChanInInt (eh_in, &has_rest);
				CTRACE ("first.clc %d %d %d\n", counts.di_first, counts.ct_first, has_rest);

				/* We can't count the tree ourself, since we can't tell the size if it's got a tagged protocol in it. */

				ChanOutChar (&pw_in, PWI_reset);
				ChanOutInt (&pw_in, PW_input);
				ChanInChar (&pw_out, &walk_tag);

				CTRACE ("sending ack, having got %d from walker\n", walk_tag);
				ChanOutChar (eh_out, PEO_ack);
				CTRACE ("ack taken\n");

				if (walk_tag != PWO_done) {
					CTRACE ("waiting for cancel/rest.clcs from kernel\n");
					ChanInChar (eh_in, &tag);
					if (tag == PEI_cancel) {
						CTRACE ("cancelled\n");
						ChanOutChar (&pw_in, PWI_stop);
					} else if (tag == PEI_rest_clcs) {
						ChanInInt (eh_in, &counts.di_rest);
						ChanInInt (eh_in, &counts.ct_rest);
						CTRACE ("rest.clcs %d %d\n", counts.di_rest, counts.ct_rest);

						CTRACE ("reading remaining NLCs\n");
						while (walk_tag != PWO_done) {
							ChanOutChar (&pw_in, PWI_next);
							ChanInChar (&pw_out, &walk_tag);
							CTRACE ("read tag %d\n", walk_tag);
						}
						CTRACE ("sending ack\n");
						ChanOutChar (eh_out, PEO_ack);
						CTRACE ("ack sent\n");
					} else {
						CFATAL ("pony_protocol_encoder: expected cancel or rest.clcs; got %d\n", tag);
					}
				}
			}
			break;
			/*}}}*/
			/*{{{  PEI_term -- terminate*/
		case PEI_term:
			CTRACE ("term\n");
			running = 0;
			ehan->refcount--;
			if (!ehan->refcount) {
				DMemFree (ehan);
			}
			break;
			/*}}}*/
			/*{{{  default -- error*/
		default:
			CFATAL ("pony_protocol_encoder: expecting PEI_first_clc or PEI_term, got %d\n", tag);
			/*}}}*/
		}
	}

	ChanOutChar (&pw_in, PWI_term);
}
/*}}}*/
/*}}} */

/*{{{  static void pony_real_get_tdesc_data (unsigned int *tdesc, int *nchans, int *nsvrread, unsigned int *typehash)*/
/*
 *	extracts data from a type-descriptor (cooked into the application binary)
 */
static void pony_real_get_tdesc_data (unsigned int *tdesc, int *nchans, int *nsvrread, unsigned int *typehash)
{
	if (typedesc_id (tdesc[0]) != MTID_CHANTYPE) {
		*nchans = 0;
		*nsvrread = 0;
		*typehash = 0;
	} else {
		*nchans = typedesc_chantype_nchans (tdesc);
		*typehash = tdesc[2];
		*nsvrread = typedesc_chantype_server_read (tdesc);
	}
}
/*}}}*/
/*{{{  static void pony_real_increase_ref_count (ct_BASIC **ctb, int ctb_ptr)*/
/*
 *	increases the ref-count of a channel-type and returns it (in "ctb" param)
 */
static void pony_real_increase_ref_count (ct_BASIC **ctb, int ctb_ptr)
{
	ct_BASIC *tmp = (ct_BASIC *)ctb_ptr;

	tmp->refcount++;
	*ctb = tmp;
}
/*}}}*/
/*{{{  static void init_ctb (ct_BASIC *ctb, unsigned int *typedesc) */
/* Warning: this function may be called in *either* context. */
static void init_ctb (ct_BASIC *ctb, unsigned int *typedesc)
{
	ct_BASIC_end *ctb_end;
	int i, nchans;

	/* setup channel-type (fully populated) */
	ctb->refcount = 1;
	ctb->typedesc = typedesc;
	ctb->uiohook = NULL;

	nchans = typedesc_chantype_nchans (typedesc);
	for (i = 0; i < nchans; i++) {
		ChanInit (&(ctb->channels[i]));
	}

	ctb_end = ct_end (ctb);
	CTSemInit (&(ctb_end->clisem));
	CTSemInit (&(ctb_end->svrsem));
	CTSemInit (&(ctb_end->statesem));
}
/*}}}*/
/*{{{  static ct_BASIC *pony_real_alloc_ctb (unsigned int *typedesc)*/
/*
 *	this actually allocates the channel-type block, used by pony_alloc_ctb() below.
 *	This is called in a C context;  pony_alloc_ctb() is called in a CIF context.
 */
static ct_BASIC *pony_real_alloc_ctb (unsigned int *typedesc)
{
	ct_BASIC *ctb;

	ctb = dmem_alloc (ct_size (typedesc));
	init_ctb (ctb, typedesc);
	return ctb;
}
/*}}}*/
/*{{{ static void make_ctb_networked (ct_BASIC *ctb, unsigned int *typedesc, int nct_id, ct_BASIC *nhh, ct_BASIC **dha, int dhs, ct_BASIC **eha, int ehs) */
/*
 * given a CTB, set up the appropriate fields in it to make it networked, and
 * fire off the protocol-encoder/decoder processes
 */
static void make_ctb_networked (ct_BASIC *ctb, unsigned int *typedesc, int nct_id, ct_BASIC *nhh, ct_BASIC **dha, int dhs, ct_BASIC **eha, int ehs)
{
	/*{{{  note from pony specification*/
/*
//--  * pony_real_alloc_ctb:
//--    Takes:
//--      * a given type-descriptor    (ws[1])
//--      * an NCT-ID                  (ws[2])
//--      * a network-hook-handle      (ws[3])
//--      * an array of decode-handles (ws[4], ws[5])
//--      * an array of encode-handles (ws[6], ws[7])
//--    Does:
//--      * allocate a new CTB according to the type-descriptor
//--      * initialise reference-count to 1
//--      * initialise type-descriptor pointer
//--      * initialise client-, server- and state-semaphores
//--      * store the NCT-ID in the state-field
//--      * set up network-hook with channel-words in the network-hook-handle
//--      * decrease reference-count of the network-hook-handle
//--      * fork off protocol-converters for each channel-word, connecting
//--        them in the correct order to the relevant channel-words (the order
//--        is the same as I described with regard to the protocol-converters
//--        earlier).
//--    IMPORTANT: the network-hook-handle and decode-handle and encode-handle
//--    variables and arrays must be set to null before returning to the
//--    occam-pi world, so that the reference-count is not accidentally
//--    changed when the (occam-pi level) allocation process finishes.
//--    Returns:
//--      * the CTB-pointer of the freshly allocated CTB (ws[0])
//--      * the integer value of that CTB-pointer        (ws[8])
*/
	/*}}}*/
	int nchans = typedesc_chantype_nchans (typedesc);

	/* store nct_id in the state-field */
	ct_end (ctb)->state = nct_id;

	/* setup uio-hook */
	ctb->uiohook = &nhh->channels[0];
	(nhh->refcount)--;

	CTRACE ("starting, ctb @ 0x%8.8x\n", (unsigned int)ctb);

	if (typedesc_id (typedesc[0]) != MTID_CHANTYPE) {
		CFATAL ("make_ctb_networked: not a channel-type!\n");
	} else {
		/*{{{  create encode/decode processes*/
		int i, coffset;
		int fi, ti;

		for (i=0, fi=0, ti=(nchans-1), coffset=3; i<nchans; i++) {
			unsigned int *chandesc = typedesc + coffset;
			Process *p = NULL;
			Process *p2 = NULL;
			CTRACE ("ctb %08x channel %d is %08x\n", ctb, i, &(ctb->channels[i]));

			switch (typedesc_id (chandesc[0])) {
				/*{{{  MTID_CHAN_I -- input channel*/
			case MTID_CHAN_I:
				p = ProcAlloc (pony_protocol_decoder, pony_protocol_decoder_stacksize, 3,
						&(ctb->channels[i]), chandesc + 1, dha[fi]);
				p2 = ProcAlloc (pony_protocol_encoder, pony_protocol_encoder_stacksize, 3,
						&(ctb->channels[i]), chandesc + 1, eha[fi]);
				fi++;
				break;
				/*}}}*/
				/*{{{  MTID_CHAN_O -- output channel*/
			case MTID_CHAN_O:
				p = ProcAlloc (pony_protocol_decoder, pony_protocol_decoder_stacksize, 3,
						&(ctb->channels[i]), chandesc + 1, dha[ti]);
				p2 = ProcAlloc (pony_protocol_encoder, pony_protocol_encoder_stacksize, 3,
						&(ctb->channels[i]), chandesc + 1, eha[ti]);
				ti--;
				break;
				/*}}}*/
			default:
				CFATAL ("make_ctb_networked: channel %d (at offset %d) not a channel ? (type = %d)\n", i, coffset, typedesc_id (chandesc[0]));
			}
			coffset += typedesc_len (chandesc[0]);

			/* start processes */
			if (p && p2) {
				ProcFork (p);
				ProcFork (p2);
			}
		}
		/*}}}*/
	}

	/* cleanup */
	DMemFree (dha);
	DMemFree (eha);

	CTRACE ("leaving, encoders and decoders started\n");
}
/*}}}*/
/*{{{  static void pony_alloc_ctb (Process *me, ct_BASIC *ctb, unsigned int *typedesc, ...)*/
/*
 *	allocates a channel-type for pony.  "ctb" holds the created channel-type;
 *	"typedesc" points at the type description; "nct_id" provides the pony
 *	'network channel-type ID'; "nhh" is the network channel-type hook
 *	for the client-end; "dha" is an array of decode-handles;
 *	"dhs" is the size of this array; "eha" is an
 *	array of encode-handles; "ehs" is the size of this array.
 *
 *	NOTE: this must be called in a CIF process context, since it forks off new processes.
 *	as such, some of the work in parameter cleaning has already been done.
 */
static const int pony_alloc_ctb_stacksize = 16384;
static void pony_alloc_ctb (Process *me, ct_BASIC *ctb, unsigned int *typedesc,
		int nct_id, ct_BASIC *nhh,
		ct_BASIC **dha, int dhs,
		ct_BASIC **eha, int ehs)
{
	make_ctb_networked (ctb, typedesc, nct_id, nhh, dha, dhs, eha, ehs);
}
/*}}}*/
/*{{{  static void pony_real_shutdown_ctbs (int *aptr, int alen)*/
/*
 *	shuts-down CTBs.  Pointers are given as integers.
 */
static void pony_real_shutdown_ctbs (int *aptr, int alen)
{
	int i;

	for (i=0; i<alen; i++) {
		ct_BASIC *ctb = (ct_BASIC *)(aptr[i]);

		if (ctb && !ctb->refcount) {
			dmem_release (ctb);
			aptr[i] = 0;
		} else if (ctb && ctb->uiohook) {
			ctb->uiohook = NULL;
		}
	}
}
/*}}}*/
/*{{{  static void pony_real_get_nct_id (int ctb_ptr, int *nct_id, int *result)*/
/*
 *	retrieves the "nct-id" of a channel-type, buried in the ops structure
 */
static void pony_real_get_nct_id (int ctb_ptr, int *nct_id, int *result)
{
	ct_BASIC *ctb = (ct_BASIC *)ctb_ptr;

	if (!ctb->uiohook) {
		*nct_id = -1;
		*result = -2;
	} else {
		*nct_id = ct_end(ctb)->state;
		*result = 0;
	}
}
/*}}}*/

/*{{{  glue for occam-pi*/
/*{{{  PROC C.pony.int.get.tdesc.data.uc (MOBILE.CHAN! cli, RESULT INT nchans, nsvrreaders, typehash)*/
void _pony_int_get_tdesc_data_uc (int *ws)
{
	pony_real_get_tdesc_data ((unsigned int*)(ws[1]), (int *)(ws[2]), (int *)(ws[3]), (unsigned int *)(ws[4]));
}
/*}}}*/
/*{{{  PROC C.pony.int.get.tdesc.data.sc (SHARED MOBILE.CHAN! cli, RESULT INT nchans, nsvrreaders, typehash)*/
void _pony_int_get_tdesc_data_sc (int *ws)
{
	pony_real_get_tdesc_data ((unsigned int *)(ws[1]), (int *)(ws[2]), (int *)(ws[3]), (unsigned int *)(ws[4]));
}
/*}}}*/
/*{{{  PROC C.pony.int.get.tdesc.data.us (MOBILE.CHAN? svr, RESULT INT nchans, nsvrreaders, typehash)*/
void _pony_int_get_tdesc_data_us (int *ws)
{
	pony_real_get_tdesc_data ((unsigned int *)(ws[1]), (int *)(ws[2]), (int *)(ws[3]), (unsigned int *)(ws[4]));
}
/*}}}*/
/*{{{  PROC C.pony.int.get.tdesc.data.ss (SHARED MOBILE.CHAN? svr, RESULT INT nchans, nsvrreaders, typehash)*/
void _pony_int_get_tdesc_data_ss (int *ws)
{
	pony_real_get_tdesc_data ((unsigned int *)(ws[1]), (int *)(ws[2]), (int *)(ws[3]), (unsigned int *)(ws[4]));
}
/*}}}*/

/*{{{  PROC C.pony.int.increase.ref.count.uc (RESULT MOBILE.CHAN! cli, VAL INT ctb.ptr)*/
void _pony_int_increase_ref_count_uc (int *ws)
{
	pony_real_increase_ref_count ((ct_BASIC **)&(ws[0]), (int)(ws[2]));
}
/*}}}*/
/*{{{  PROC C.pony.int.increase.ref.count.sc (RESULT SHARED MOBILE.CHAN! cli, VAL INT ctb.ptr)*/
void _pony_int_increase_ref_count_sc (int *ws)
{
	pony_real_increase_ref_count ((ct_BASIC **)&(ws[0]), (int)(ws[2]));
}
/*}}}*/
/*{{{  PROC C.pony.int.increase.ref.count.us (RESULT MOBILE.CHAN? svr, VAL INT ctb.ptr)*/
void _pony_int_increase_ref_count_us (int *ws)
{
	pony_real_increase_ref_count ((ct_BASIC **)&(ws[0]), (int)(ws[2]));
}
/*}}}*/
/*{{{  PROC C.pony.int.increase.ref.count.ss (RESULT SHARED MOBILE.CHAN? svr, VAL INT ctb.ptr)*/
void _pony_int_increase_ref_count_ss (int *ws)
{
	pony_real_increase_ref_count ((ct_BASIC **)&(ws[0]), (int)(ws[2]));
}
/*}}}*/

/*{{{  PROC C.pony.cleancifprocess (INT addr)*/
void _pony_cleancifprocess (int *ws)
{
	Process **p = (Process **)(ws[0]);

	ProcAllocClean (*p);
	*p = NULL;
}
/*}}}*/
/*{{{  PROC C.pony.int.alloc.ctb.uc (RESULT INT paddr, RESULT MOBILE.CHAN! cli, VAL INT nct.id, PONY.NETHOOKHANDLE! net.hook.handle, MOBILE []PONY.DECODEHANDLE! dec.handle.array, MOBILE []PONY.ENCODEHANDLE! enc.handle.array, RESULT INT ctb.ptr)*/
void _pony_int_alloc_ctb_uc (int *ws)
{
	ct_BASIC *ct = pony_real_alloc_ctb ((unsigned int *)(ws[2]));
	ct_BASIC **dhans = (ct_BASIC **)(ws[5]);
	int ndhans = ws[6];
	ct_BASIC **ehans = (ct_BASIC **)(ws[7]);
	int nehans = ws[8];
	ct_BASIC *nhh = (ct_BASIC *)(ws[4]);

	Process *p = ProcAlloc (pony_alloc_ctb, pony_alloc_ctb_stacksize, 8, ct, (unsigned int *)(ws[2]), ws[3], nhh,
			dhans, ndhans, ehans, nehans);

	/* deal with parameters */
	ws[1] = (int)ct;
	ws[4] = (int)NULL;
	ws[5] = (int)NULL;
	ws[6] = 0;
	ws[7] = (int)NULL;
	ws[8] = 0;

	*(int *)(ws[9]) = (int)ct;
	*(int *)(ws[0]) = (int)p;
}
/*}}}*/
/*{{{  PROC C.pony.int.alloc.ctb.sc (RESULT INT raddr, RESULT SHARED MOBILE.CHAN! cli, VAL INT nctid, ...)*/
void _pony_int_alloc_ctb_sc (int *ws)
{
	_pony_int_alloc_ctb_uc (ws);
}
/*}}}*/
/*{{{  PROC C.pony.int.alloc.ctb.us (RESUNT INT raddr, RESULT MOBILE.CHAN? svr, VAL INT nctid, PONY.NETHOOKHANDLE! nhh, ...)*/
void _pony_int_alloc_ctb_us (int *ws)
{
	_pony_int_alloc_ctb_uc (ws);
}
/*}}}*/
/*{{{  PROC C.pony.int.alloc.ctb.ss (RESULT INT raddr, RESULT SHARED MOBILE.CHAN? svr, VAL INT nctid, PONY.NETHOOKHANDLE! nhh, ...)*/
void _pony_int_alloc_ctb_ss (int *ws)
{
	_pony_int_alloc_ctb_uc (ws);
}
/*}}}*/

/*{{{  PROC C.pony.int.shutdown.ctbs (MOBILE []INT ctbptra)*/
void _pony_int_shutdown_ctbs (int *ws)
{
	pony_real_shutdown_ctbs ((int *)(ws[0]), (int)(ws[1]));
}
/*}}}*/
/*{{{  PROC C.pony.int.get.nct.id (VAL INT ctb.ptr, RESULT INT nct.id, result)*/
void _pony_int_get_nct_id (int *ws)
{
	pony_real_get_nct_id ((int)(ws[0]), (int *)(ws[1]), (int *)(ws[2]));
}
/*}}}*/
/*}}}*/

