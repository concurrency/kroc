/*
 *	ponyintcif.c -- pony protocol converters
 *	Copyright (C) 2005, 2006 Mario Schweigler
 *	Copyright (C) 2005, 2006 Fred Barnes
 *	Copyright (C) 2005, 2006 University of Kent
 *	Copyright (C) 2008 Adam Sampson <ats@offog.org>
 *
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

#include <cif.h>
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
/*{{{  protocol tag values */
/* PROTOCOL PONY.DECODEHANDLE.TO.DECODER */
enum PDI_tags {
	PDI_activate = 0,
	PDI_make_ctb_networked_confirm,	/* INT; PONY.NETHOOKHANDLE!; MOBILE []PONY.DECODEHANDLE!; MOBILE []PONY.ENCODEHANDLE! */
	PDI_ack,
	PDI_cancel,
	PDI_cancel_encode,
	PDI_cancel_encode_ack,
	PDI_term
};

/* PROTOCOL PONY.DECODEHANDLE.FROM.DECODER */
enum PDO_tags {
	PDO_first_clc = 0,		/* INT; INT; BOOL */
	PDO_rest_clcs,			/* INT; INT */
	PDO_data_item_nlc,		/* INT; INT */
	PDO_make_ctb_networked,		/* INT; INT; INT; INT; INT */
	PDO_ct_end_nlc,			/* INT; INT; INT */
	PDO_cancel_confirm,
	PDO_encode_cancelled,
	PDO_encode_not_cancelled
};

/* PROTOCOL PONY.ENCODEHANDLE.TO.ENCODER */
enum PEI_tags {
	PEI_first_clc = 0,		/* INT; INT; BOOL */
	PEI_rest_clcs,			/* INT; INT */
	PEI_data_item_nlc,		/* MOBILE []BYTE */
	PEI_clone_ctb,			/* INT */
	PEI_alloc_new_ctb,		/* INT */
	PEI_alloc_new_ctb_confirm,	/* PONY.NETHOOKHANDLE!; MOBILE []PONY.DECODEHANDLE!; MOBILE []PONY.ENCODEHANDLE! */
	PEI_cancel,
	PEI_term
};

/* PROTOCOL PONY.ENCODEHANDLE.FROM.ENCODER */
enum PEO_tags {
	PEO_alloc_new_ctb_confirm = 0,	/* INT; INT; INT */
	PEO_ack
};
/*}}}*/
/*{{{ forward declarations */
static mt_cb_t *new_ctb (Workspace wptr, unsigned int *typedesc);
static void make_ctb_networked (Workspace wptr, mt_cb_t *cb, unsigned int *typedesc, int nct_id, mt_cb_t *nhh, mt_array_t *dec_handle_array, mt_array_t *enc_handle_array);
/*}}}*/

/*{{{ typedesc helper functions */
/*{{{ typedesc_id */
/*
 *	returns the type ID from the start of a typedesc
 */
static inline unsigned int typedesc_id (unsigned int typedesc) {
	return typedesc >> 24;
}
/*}}}*/
/*{{{ typedesc_len */
/*
 *	returns the typedesc length from the start of a typedesc
 */
static inline unsigned int typedesc_len (unsigned int typedesc) {
	return (typedesc & 0x00FFFFFF) / sizeof (unsigned int);
}
/*}}}*/
/*{{{ typedesc_data_size */
/*
 *	get the size of the data in a primitive type
 */
static long long typedesc_data_size (Workspace wptr, unsigned int **pdesc) {
	const unsigned int type = typedesc_id ((*pdesc)[0]);
	(*pdesc)++;
	CTRACE ("typedesc_data_size: type = %d\n", 1, type);

	switch (type) {
	case MTID_PRIM:
	case MTID_RECORD:
		{
			const unsigned int item_size = (*pdesc)[0];
			(*pdesc) += 2;
			return item_size;
		}
	case MTID_ARRAY:
		{
			const unsigned int num_elements = (*pdesc)[0];
			(*pdesc)++;
			return num_elements * typedesc_data_size (wptr, pdesc);
		}
	default:
		CFATAL ("typedesc_data_size: expected primitive type; this is type %d\n", 1, type);
		return -1;
	}
}
/*}}}*/
/*{{{ typedesc_chantype_nchans */
/*
 *	returns the number of channels in a channel-type given its type-descriptor
 */
static int typedesc_chantype_nchans (Workspace wptr, unsigned int *tdesc)
{
	if (typedesc_id (tdesc[0]) != MTID_CHANTYPE) {
		CFATAL ("typedesc_chantype_nchans: not a channel type\n", 0);
	}
	return (int) tdesc[1];
}
/*}}}*/
/*{{{ typedesc_chantype_server_read */
/*
 *	returns the number of server-read channels in a channel-type given its type-descriptor
 */
static int typedesc_chantype_server_read (Workspace wptr, unsigned int *tdesc)
{
	int i, nchans, count = 0;

	if (typedesc_id (tdesc[0]) != MTID_CHANTYPE) {
		CFATAL ("typedesc_chantype_server_read: not a channel type\n", 0);
	}

	nchans = typedesc_chantype_nchans (wptr, tdesc);
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
			CFATAL ("typedesc_chantype_server_read: expected input or output channel, found %d %08x\n", 2, id, tdesc[0]);
		}

		tdesc += typedesc_len (tdesc[0]);
	}

	return count;
}
/*}}}*/
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

typedef struct {
	pony_walk_mode mode;

	Channel *pwi, *pwo, *user, *to_kern, *from_kern;
	unsigned int *pdesc;

	int clc;
	int clc_total;
	pony_walk_counts counts;

	int deferred_min;

	void **temp_list;
	int temp_list_used;
	int temp_list_size;

	void **comm_list;
	int comm_list_used;
	int comm_list_size;
} pony_walk_state;

#define SCTRACE(s, format, n, args...) \
	CTRACE("[Chan#%08x Mode%d CLC%d] " format, n + 3, (unsigned int) (s)->user, (s)->mode, (s)->clc, ##args)

/*}}}*/
/*{{{ vector_add */
/*
 *	add an item to a dynamically-sized array
 */
static void *vector_add (Workspace wptr, void **list, size_t item_size, int *used, int *size, int default_size)
{
	void *new_list, *ptr;

	if (*used == *size) {
		if (*size == 0) {
			*size = default_size;
		} else {
			*size *= 2;
		}
		CTRACE ("%d used; stretched list to %d items\n", 2, *used, *size);
		new_list = malloc (*size * item_size);
		if (*list != NULL) {
			memcpy (new_list, *list, *used * item_size);
			free (*list);
		}
		*list = new_list;
	}

	ptr = ((char *) *list) + *used * item_size;
	*used += 1;
	return ptr;
}
/*}}}*/
/*{{{ temp_list_add */
/*
 *	add a block of malloc-allocated memory to those to free at the end of this walk
 */
static void temp_list_add (Workspace wptr, pony_walk_state *s, void *ptr) {
	void **p;

	CTRACE ("adding to temp list: 0x%08x\n", 1, (unsigned int) ptr);

	p = vector_add (wptr, (void **) &s->temp_list, sizeof *s->temp_list, &s->temp_list_used, &s->temp_list_size, 16);
	*p = ptr;
}
/*}}}*/
/*{{{ comm_list_add */
/*
 *	add a communicated mobile to those to free at the end of the walk, if not cancelled
 */
static void comm_list_add (Workspace wptr, pony_walk_state *s, void *item)
{
	void **p;

	p = vector_add (wptr, (void **) &s->comm_list, sizeof *s->comm_list, &s->comm_list_used, &s->comm_list_size, 16);
	*p = item;
}
/*}}}*/
/*{{{ alloc_temp_mem */
/*
 *	allocate temporary memory for the duration of this walk
 */
static void *alloc_temp_mem (Workspace wptr, pony_walk_state *s, size_t size)
{
	void *mem = malloc (size);
	CTRACE ("allocated %d bytes at %08x\n", 2, size, (unsigned int) mem);
	temp_list_add (wptr, s, mem);
	return mem;
}
/*}}}*/
/*{{{ process_temp_list */
/*
 *	free temporary memory
 */
static void process_temp_list (Workspace wptr, pony_walk_state *s)
{
	int i;

	CTRACE ("freeing %d temporary items\n", 1, s->temp_list_used);
	for (i = 0; i < s->temp_list_used; i++) {
		CTRACE ("freeing item %d: 0x%08x\n", 2, i, (unsigned int) s->temp_list[i]);
		free (s->temp_list[i]);
	}
	s->temp_list_used = 0;
}
/*}}}*/
/*{{{ process_comm_list */
/*
 *	free communicated items
 */
static void process_comm_list (Workspace wptr, pony_walk_state *s)
{
	int i;

	CTRACE ("freeing %d items\n", 1, s->comm_list_used);
	for (i = 0; i < s->comm_list_used; i++) {
		void *item = s->comm_list[i];
		CTRACE ("freeing item %d: ptr 0x%08x\n", 2, i, (unsigned int) item);

		if (item != NULL) {
			MTRelease (wptr, item);
		}
	}
	s->comm_list_used = 0;
}
/*}}}*/
/*{{{ free_walk_state */
/*
 *	release any memory used by the walk state
 */
static void free_walk_state (Workspace wptr, pony_walk_state *s)
{
	process_temp_list (wptr, s);
	if (s->temp_list != NULL) {
		free (s->temp_list);
	}

	process_comm_list (wptr, s);
	if (s->comm_list != NULL) {
		free (s->comm_list);
	}
}
/*}}}*/
/*{{{ copy_di */
/*
 *	copy a data item into temporary memory if necessary
 */
static void *copy_di (Workspace wptr, pony_walk_state *s, void *data, size_t size)
{
	/* Since the pony kernel expects to be able to hang on to
	 * pointers it's given for the duration of the ULC (since it wants to be able
	 * to writev() them), we have to copy the data the user's given us.
	 */
	void *data_copy;
	if (s->clc == 0 || s->clc == (s->clc_total - 1)) {
		data_copy = data;
	} else {
		data_copy = alloc_temp_mem (wptr, s, size);
		memcpy (data_copy, data, size);
		CTRACE ("copied data item, size %d, from 0x%08x to 0x%08x\n", 3, size, (unsigned int) data, (unsigned int) data_copy);
	}

	return data_copy;
}
/*}}}*/
/*{{{ output_di */
/*
 *	output a data item to the pony kernel
 */
static void output_di (Workspace wptr, pony_walk_state *s, void *data, size_t size)
{
	if (s->mode == PW_cancel) {
		SCTRACE (s, "cancel CLC %d\n", 1, s->clc);
	} else {
		data = copy_di (wptr, s, data, size);

		SCTRACE (s, "output CLC %d data.item.nlc; data 0x%08x; size %d\n", 3, s->clc, (unsigned int) data, size);
		ChanOutChar (wptr, s->to_kern, PDO_data_item_nlc);
		ChanOutInt (wptr, s->to_kern, (int) data);
		ChanOutInt (wptr, s->to_kern, (int) size);
	}
}
/*}}}*/
/*{{{ input_di_unchecked */
/*
 *	input a data item from the pony kernel without checking the size
 */
static mt_array_t *input_di_unchecked (Workspace wptr, pony_walk_state *s)
{
	char tag;
	mt_array_t *array;

	SCTRACE (s, "reading data.item.nlc\n", 0);
	ChanInChar (wptr, s->from_kern, &tag);
	if (tag != PEI_data_item_nlc) {
		CFATAL ("input_di: expected data.item.nlc, got %d\n", 1, tag);
	}
	MTChanIn (wptr, s->from_kern, (void **) &array);

	if (array == NULL) {
		/* Zero-length array communicated as NULL -- allocate a dummy array
		 * so we don't ever return NULL from this function. */
		array = MTAllocArray (wptr, MT_MAKE_NUM (MT_NUM_BYTE), 1, 0);
	}

	SCTRACE (s, "... read data.item.nlc; mobile %08x; size %d\n", 2, (int) array, array->dimensions[0]);
	return array;
}
/*}}}*/
/*{{{ input_di */
/*
 *	input a data item from the pony kernel
 */
static mt_array_t *input_di (Workspace wptr, pony_walk_state *s, size_t size)
{
	mt_array_t *array = input_di_unchecked (wptr, s);

	if (array->dimensions[0] != size) {
		CFATAL ("input_di: expected size %d in data.item.nlc, got size %d\n", 2, size, array->dimensions[0]);
	}

	return array;
}
/*}}}*/
/*{{{ count_di */
/*
 *	add data items to the count
 */
static void count_di (Workspace wptr, pony_walk_state *s, int num)
{
	if (s->clc == 0) {
		s->counts.di_first += num;
	} else {
		s->counts.di_rest += num;
	}
}
/*}}}*/
/*{{{ get_data */
/*
 *	get the address of the sending process's data
 */
static void *get_data (Workspace wptr, pony_walk_state *s)
{
	if (s->mode == PW_input) {
		/* There isn't a sending process. */
		return NULL;
	} else {
		Workspace *other = (Workspace *)(*s->user);
		if (other == NULL) {
			CFATAL ("get_data: no sending process for channel at 0x%08x (channel word is NULL)\n", 1, (unsigned int) s->user);
		}
		return (void *) other[Pointer];
	}
}
/*}}}*/
/*{{{ get_mobile */
/*
 *	Get the mobile that's being communicated across s->user, deferring the
 *	completion of the input until later (in case the communication is
 *	cancelled).
 */
static word *get_mobile (Workspace wptr, pony_walk_state *s)
{
	word **data = get_data (wptr, s);

	SCTRACE (s, "deferring MTChanXIn\n", 0);
	s->deferred_min = 1;
	return *data;
}
/*}}}*/
/*{{{ release_user */
/*
 *	release the user process from a communication
 */
static void release_user (Workspace wptr, pony_walk_state *s, int enable)
{
	if (s->mode == PW_output || s->mode == PW_cancel) {
		SCTRACE (s, "releasing user process\n", 0);

		if (s->deferred_min) {
			void *mobile;

			SCTRACE (s, "doing deferred MTChanXIn\n", 0);
			MTChanXIn (wptr, s->user, &mobile);
			comm_list_add (wptr, s, mobile);

			s->deferred_min = 0;
		}

		ChanXEnd (wptr, s->user);
		if (enable) {
			ChanXAble (wptr, s->user);
		}
	}
}
/*}}}*/
/*{{{ new_clc */
/*
 *	start a new CLC
 */
static int new_clc (Workspace wptr, pony_walk_state *s, int inhibit_release)
{
	s->clc++;
	SCTRACE (s, "new CLC\n", 0);

	if (s->mode != PW_count) {
		char c;

		SCTRACE (s, "asking whether to continue\n", 0);
		ChanOutChar (wptr, s->pwo, PWO_more);
		ChanInChar (wptr, s->pwi, &c);
		if (c == PWI_stop) {
			SCTRACE (s, "got PWI_stop, stopping here\n", 0);
			return 1;
		} else if (c != PWI_next) {
			CFATAL ("new_clc: expected PWI_stop or PWI_next, got %d\n", 1, c);
		}
		CTRACE ("got PWI_next, continuing\n", 0);
	}

	if (!inhibit_release) {
		release_user (wptr, s, 1);
	}

	return 0;
}
/*}}}*/
/*{{{ is_primitive */
/*
 *	say whether a typedesc id is a primitive type
 */
static inline int is_primitive (unsigned int id)
{
	return id == MTID_PRIM || id == MTID_RECORD;
}
/*}}}*/
/*{{{ walk_inner */
/*
 *	walk part of a type descriptor
 */
static int walk_inner (Workspace wptr, pony_walk_state *s)
{
	const unsigned int type = typedesc_id (s->pdesc[0]);
	const unsigned int type_len = typedesc_len (s->pdesc[0]);
	unsigned int *orig_pdesc = s->pdesc;
	s->pdesc++;

	SCTRACE (s, "type is %d length %d\n", 2, type, type_len);
	switch (type) {
	/*{{{  PRIM, RECORD, ARRAY */
	case MTID_PRIM:
	case MTID_RECORD:
	case MTID_ARRAY:
		{
			long long data_size;
			size_t size;

			s->pdesc--;
			data_size = typedesc_data_size (wptr, &(s->pdesc));
			size = data_size;

			if (s->mode == PW_output && data_size >= 0x80000000) {
				CFATAL ("walk_inner_primitive: attempting to send enormous data item; this won't fit in the count\n", 0);
			}
			CTRACE ("primitive type of size %d\n", 1, (int) size);

			switch (s->mode) {
			case PW_count:
				{
					count_di (wptr, s, 1);
					break;
				}
			case PW_output:
			case PW_cancel:
				{
					void *data = get_data (wptr, s);
					output_di (wptr, s, data, size);
					break;
				}
			case PW_input:
				{
					mt_array_t *data = input_di (wptr, s, size);
					SCTRACE (s, "doing ChanOut; data 0x%08x; size %d\n", 2, (unsigned int) data, size);
					ChanOut (wptr, s->user, data->data, size);
					SCTRACE (s, "ChanOut done\n", 0);
					MTRelease (wptr, data);
					break;
				}
			}
		}
		break;
	/*}}}*/
	/*{{{  SEQPROTO */
	case MTID_SEQPROTO:
		{
			unsigned int i;
			const unsigned int nitems = s->pdesc[0];
			s->pdesc++;

			for (i = 0; i < nitems; i++) {
				SCTRACE (s, "item %d of %d in protocol\n", 2, i, nitems);
				if (i > 0) {
					if (new_clc (wptr, s, 0)) {
						return 1;
					}
				}
				if (walk_inner (wptr, s)) {
					return 1;
				}
				SCTRACE (s, "done item %d of %d\n", 2, i, nitems);
			}
		}
		break;
	/*}}}*/
	/*{{{  TAGPROTO */
	case MTID_TAGPROTO:
		{
			char tag;
			unsigned int i;
			const unsigned int ntags = s->pdesc[0];
			s->pdesc++;

			SCTRACE (s, "tagged protocol with %d possible tags\n", 1, ntags);

			/*{{{  process the tag */
			switch (s->mode) {
			case PW_count:
				{
					/* Read the user's data anyway -- we need to know which tag it is! */
					void *data = get_data (wptr, s);
					tag = *((char *) data);

					count_di (wptr, s, 1);
					break;
				}
			case PW_output:
			case PW_cancel:
				{
					void *data = get_data (wptr, s);
					tag = *((char *) data);

					output_di (wptr, s, data, 1);
					break;
				}
			case PW_input:
				{
					mt_array_t *data = input_di (wptr, s, 1);
					tag = *((char *) data->data);
					MTRelease (wptr, data);

					SCTRACE (s, "doing ChanOut for tag\n", 0);
					ChanOutChar (wptr, s->user, tag);
					SCTRACE (s, "ChanOut done\n", 0);
					break;
				}
			}
			SCTRACE (s, "... and this tag is %d\n", 1, tag);
			/*}}}*/
			/*{{{  process the remaining items */
			for (i = 0; i < ntags; i++) {
				const unsigned int tag_id = typedesc_id (s->pdesc[0]);
				const unsigned int tag_len = typedesc_len (s->pdesc[0]);

				if (tag_id != MTID_TAG) {
					CFATAL ("walk_inner: expected MTID_TAG, found type %d length %d\n", 2, tag_id, tag_len);
				}

				if (s->pdesc[1] == tag) {
					unsigned int j;
					const unsigned int nitems = s->pdesc[2];
					s->pdesc += 3;

					CTRACE ("found tag; it's number %d\n", 1, i);
					for (j = 0; j < nitems; j++) {
						CTRACE ("item %d of %d in tagged protocol\n", 2, i, nitems);
						if (new_clc (wptr, s, 0)) {
							return 1;
						}
						if (walk_inner (wptr, s)) {
							return 1;
						}
					}

					/* Skip over the remaining tags. */
					s->pdesc = orig_pdesc + type_len;
					break;
				} else {
					CTRACE ("it's not tag %d\n", 1, i);
					s->pdesc += tag_len;
				}
			}
			if (i == ntags) {
				CFATAL ("walk_inner: failed to find a matching tag for %d\n", 1, tag);
			}
			/*}}}*/
		}
		break;
	/*}}}*/
	/*{{{  COUNTED */
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

			CTRACE ("handling counted array as (probably) %d CLCs\n", 1, first_clc ? 2 : 1);

			if (!is_primitive (count_id)) {
				CFATAL ("walk_inner: expected primitive counted array count type, found type %d length %d\n", 2, count_id, count_len);
			}
			count_size = s->pdesc[1];
			s->pdesc += 3;

			subtype_id = typedesc_id (s->pdesc[0]);
			subtype_len = typedesc_len (s->pdesc[0]);
			if (!is_primitive (subtype_id)) {
				CFATAL ("walk_inner: expected primitive counted array data type, found type %d length %d\n", 2, subtype_id, subtype_len);
			}
			subtype_size = s->pdesc[1];
			s->pdesc += 3;

			switch (s->mode) {
			case PW_count:
				count_di (wptr, s, 1);
				if (first_clc) {
					/* Special case: if this is the first CLC, and the count is zero,
					 * then we must only send *one* NLC, so that the pony kernel knows
					 * the communication is completed before the user process is released
					 * -- else we get deadlock inside the pony kernel. Argh! */
					/* FIXME: get rid of count mode entirely in favour of buffering */
					CTRACE ("reading count %d bytes from 0x%08x\n", 2, count_size, (unsigned int) get_data (wptr, s));
					memcpy (&count, get_data (wptr, s), count_size);
					CTRACE ("counted array count is %d\n", 1, (int) count);

					if (count != 0) {
						if (new_clc (wptr, s, 1)) {
							return 1;
						}
						count_di (wptr, s, 1);
					} else {
						CTRACE ("... so we're actually handling it as *one* CLC\n", 0);
					}
				}
				break;
			case PW_output:
			case PW_cancel:
				{
					/* XXX: Endianness-dependent code. */
					memcpy (&count, get_data (wptr, s), count_size);
					CTRACE ("counted array count is %d\n", 1, (int) count);

					if (count == 0) {
						if (first_clc) {
							/* As above: this is the first CLC, and the count is zero, so
							 * the count is the only NLC we send. */
							CTRACE ("... so we're actually handling it as *one* CLC\n", 0);
							CTRACE ("output counted array size CLC %d\n", 1, s->clc);
							if (s->mode == PW_output) {
								void *count_data = alloc_temp_mem (wptr, s, count_size);
								memcpy (count_data, &count, count_size);

								ChanOutChar (wptr, s->to_kern, PDO_data_item_nlc);
								ChanOutInt (wptr, s->to_kern, (int) count_data);
								ChanOutInt (wptr, s->to_kern, count_size);
							}
						} else {
							if (s->mode == PW_output) {
								CTRACE ("output fake data CLC %d (0, NULL)\n", 1, s->clc);
								ChanOutChar (wptr, s->to_kern, PDO_data_item_nlc);
								ChanOutInt (wptr, s->to_kern, (int) NULL);
								ChanOutInt (wptr, s->to_kern, 0);
							}
						}
					} else {
						void *data;
						const long long data_size = (size_t) count * subtype_size;

						CTRACE ("array data size is %d\n", 1, (int) data_size);
						if (data_size >= 0x80000000) {
							CFATAL ("walk_inner: counted array data size %d (%d items of size %d) is too large to send\n", 3, (int) data_size, count, subtype_size);
						}

						if (first_clc) {
							CTRACE ("output counted array size CLC %d\n", 1, s->clc);
							if (s->mode == PW_output) {
								void *count_data = alloc_temp_mem (wptr, s, count_size);
								memcpy (count_data, &count, count_size);

								ChanOutChar (wptr, s->to_kern, PDO_data_item_nlc);
								ChanOutInt (wptr, s->to_kern, (int) count_data);
								ChanOutInt (wptr, s->to_kern, count_size);
							}

							if (new_clc (wptr, s, 0)) {
								return 1;
							}
						} else {
							release_user (wptr, s, 1);
						}
						if (s->mode == PW_output) {
							data = copy_di (wptr, s, get_data (wptr, s), (size_t) data_size);
							CTRACE ("output counted array data CLC %d\n", 1, s->clc);
							ChanOutChar (wptr, s->to_kern, PDO_data_item_nlc);
							ChanOutInt (wptr, s->to_kern, (int) data);
							ChanOutInt (wptr, s->to_kern, (int) data_size);
						}
					}
				}
				break;
			case PW_input:
				{
					mt_array_t *array;

					if (first_clc) {
						CTRACE ("input counted array size CLC %d\n", 1, s->clc);
						array = input_di (wptr, s, count_size);

						if (array->dimensions[0] > sizeof count) {
							CFATAL ("walk_inner: counted array size type is invalid; was %d, expected %d\n", 2, array->dimensions[0], count_size);
						}
						memcpy (&count, array->data, array->dimensions[0]);
						MTRelease (wptr, array);

						CTRACE ("output counted array size %d (count size %d; value %08x)\n", 3, (int) count, count_size, (int) count);
						ChanOut (wptr, s->user, &count, count_size);

						if (count != 0) {
							if (new_clc (wptr, s, 0)) {
								return 1;
							}
						} else {
							CTRACE ("... so we're actually handling it as *one* CLC\n", 0);
						}
					}

					if (!(first_clc && count == 0)) {
						CTRACE ("input counted array data CLC %d\n", 1, s->clc);
						array = input_di_unchecked (wptr, s);

						if (!first_clc) {
							CTRACE ("output counted array size; size is %d, count is %d\n", 2, array->dimensions[0], array->dimensions[0] / subtype_size);
							ChanOutInt (wptr, s->user, array->dimensions[0] / subtype_size);
						}

						if (array->dimensions[0] != 0) {
							CTRACE ("output counted array data at %08x\n", 1, (unsigned int) array->data);
							ChanOut (wptr, s->user, array->data, array->dimensions[0]);
						}

						MTRelease (wptr, array);
					}

					CTRACE ("output counted array done\n", 0);
				}
				break;
			}
		}
		break;
	/*}}}*/
	/*{{{  MARRAY */
	case MTID_MARRAY:
		{
			const unsigned int dimcount = s->pdesc[0];
			const unsigned int subtype_id = typedesc_id (s->pdesc[1]);
			const unsigned int subtype_len = typedesc_len (s->pdesc[1]);
			unsigned int subtype_size;
			CTRACE ("mobile array, dimcount %d, subtype %d length %d\n", 3, dimcount, subtype_id, subtype_len);

			if (!is_primitive (subtype_id)) {
				CFATAL ("walk_inner: mobile array subtype must be primitive; found type %d length %d\n", 2, subtype_id, subtype_len);
			}
			subtype_size = s->pdesc[2];
			CTRACE ("marray subtype item size is %d\n", 1, subtype_size);
			s->pdesc += 4;

			switch (s->mode) {
			case PW_count:
				{
					count_di (wptr, s, (dimcount == 1) ? 1 : 2);
				}
				break;
			case PW_output:
			case PW_cancel:
				{
					int i;
					unsigned long long data_size;
					mt_array_t *array = (mt_array_t *) get_mobile (wptr, s);
					unsigned int *dimensions;
					void *data;

					CTRACE ("output marray, %d dimensions, mobile at 0x%08x\n", 2, dimcount, (int) array);

					if (array == NULL) {
						/* Empty array, communicated as NULL, so we need to
						 * allocate a dummy dimensions array. */
						CTRACE ("empty marray\n", 0);
						dimensions = alloc_temp_mem (wptr, s, dimcount * sizeof (int));
						for (i = 0; i < dimcount; i++) {
							dimensions[i] = 0;
						}
						data = NULL;
					} else {
						dimensions = array->dimensions;
						data = array->data;
					}

					data_size = subtype_size;
					for (i = 0; i < dimcount; i++) {
						CTRACE ("dimension %d is %d\n", 2, i, dimensions[i]);
						data_size *= dimensions[i];
					}
					CTRACE ("marray data size is %d\n", 1, (int) data_size);

					if (data_size >= 0x80000000) {
						CFATAL ("walk_inner: mobile array data size too large to transmit\n", 0);
					}

					if (s->mode == PW_output) {
						if (dimcount > 1) {
							CTRACE ("output CLC %d (marray dimension block at 0x%08x)\n", 2, s->clc, (unsigned int) dimensions);
							ChanOutChar (wptr, s->to_kern, PDO_data_item_nlc);
							ChanOutInt (wptr, s->to_kern, (int) dimensions);
							ChanOutInt (wptr, s->to_kern, (int) dimcount * sizeof (word));
						}

						CTRACE ("output CLC %d (marray data at 0x%08x)\n", 2, s->clc, (int) data);
						ChanOutChar (wptr, s->to_kern, PDO_data_item_nlc);
						ChanOutInt (wptr, s->to_kern, (int) data);
						ChanOutInt (wptr, s->to_kern, (int) data_size);
					} else {
						CTRACE ("cancelling CLC %d marray\n", 1, s->clc);
					}

					/* The array will be freed when comm_list is processed. */
				}
				break;
			case PW_input:
				{
					int i;
					mt_array_t *array, *output;
					int num_elements;

					/* We allocate mobile arrays as arrays of BYTEs, from the mobile type system's
					 * perspective. It doesn't seem entirely unreasonable that this might break
					 * something in the future. */
					const word subtype_mt = MT_MAKE_NUM (MT_NUM_BYTE);

					if (dimcount == 1) {
						CTRACE ("input CLC %d (marray data for single dimension)\n", 1, s->clc);
						array = input_di_unchecked (wptr, s);
						num_elements = array->dimensions[0] / subtype_size;

						/* We can just reuse the array we received, since it has one dimension already. */
						output = array;
						output->dimensions[0] = num_elements;
					} else {
						mt_array_t *dimensions;

						CTRACE ("input CLC %d (marray dimension block)\n", 1, s->clc);
						dimensions = input_di (wptr, s, dimcount * sizeof (word));
						num_elements = 1;
						for (i = 0; i < dimcount; i++) {
							num_elements *= ((word *) dimensions->data)[i];
						}

						CTRACE ("input CLC %d (marray data for %d dimensions)\n", 2, s->clc, dimcount);
						array = input_di (wptr, s, num_elements * subtype_size);

						output = MTAlloc (wptr, MT_MAKE_ARRAY_TYPE (dimcount, subtype_mt), num_elements * subtype_size);
						for (i = 0; i < dimcount; i++) {
							output->dimensions[i] = ((word *) dimensions->data)[i];
						}

						CTRACE ("received %d items, total size %d\n", 2, num_elements, array->dimensions[0]);
						memcpy (output->data, array->data, array->dimensions[0]);
						MTRelease (wptr, array);
					}

					CTRACE ("doing output for marray; mobile 0x%08x\n", 1, (unsigned int) output);
					MTChanOut (wptr, s->user, (void **) &output);
					CTRACE ("output done\n", 0);
				}
				break;
			}
		}
		break;
	/*}}}*/
	/*{{{  MOBILE */
	case MTID_MOBILE:
		{
			long long data_size = typedesc_data_size (wptr, &(s->pdesc));
			size_t size = data_size;
			mt_array_t *array;

			if (s->mode == PW_output && data_size >= 0x80000000) {
				CFATAL ("walk_inner_primitive: attempting to send enormous data item; this won't fit in the count\n", 0);
			}
			CTRACE ("fixed-size mobile of size %d\n", 1, (int) size);

			switch (s->mode) {
			case PW_count:
				{
					count_di (wptr, s, 1);
					break;
				}
			case PW_output:
			case PW_cancel:
				{
					void *mobile = NULL;

					CTRACE ("doing swap-and-copy input for fixed-size mobile\n", 0);
					MTChanXXChg (wptr, s->user, &mobile);
					output_di (wptr, s, mobile, size);
					MTChanXXChg (wptr, s->user, &mobile);
					CTRACE ("input done\n", 0);

					break;
				}
			case PW_input:
				{
					/* FIXME: It would be nice if we could do this using the same
					 * double-swap trick as above, since we know the other process
					 * already has a mobile of the right size. */

					void *mobile = MAlloc (wptr, size);

					array = input_di (wptr, s, size);
					memcpy (mobile, array->data, size);

					CTRACE ("doing exchange output for fixed-size mobile\n", 0);
					MTChanXChg (wptr, s->user, &mobile);
					CTRACE ("output done\n", 0);

					MTRelease (wptr, array);
					MTRelease (wptr, mobile);

					break;
				}
			}
		}
		break;
	/*}}}*/
	/*{{{  MCHANEND_* */
	case MTID_MCHANEND_IU:
	case MTID_MCHANEND_OU:
	case MTID_MCHANEND_IS:
	case MTID_MCHANEND_OS:
		{
			const int is_client = (type == MTID_MCHANEND_OU || type == MTID_MCHANEND_OS);
			const int is_shared = (type == MTID_MCHANEND_IS || type == MTID_MCHANEND_OS);

			CTRACE ("got mobile channel end, client %d shared %d\n", 2, 2, is_client, is_shared);

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
					mt_cb_t *ctb = (mt_cb_t *) get_mobile (wptr, s);
					mt_cb_pony_state_t *pony = mt_cb_get_pony_state (ctb);

					CTRACE ("CTB at 0x%08x typedesc 0x%08x uiohook 0x%08x\n", 3, (unsigned int) ctb, (unsigned int) pony->typedesc, (unsigned int) pony->uiohook);

					if (s->mode == PW_output) {
						/* FIXME: MTLock should be extended so we don't have to
						 * use the semaphore ops directly. */
						ccsp_cif_Y_sem_claim (wptr, &(pony->statesem));
						if (pony->uiohook == NULL) {
							int nct_id;
							mt_cb_t *nhh;
							mt_array_t *dha, *eha;
							char tag;

							CTRACE ("CTB not networked; sending make.ctb.networked; state 0x%08x\n", 1, pony->state);
							ChanOutChar (wptr, s->to_kern, PDO_make_ctb_networked);
							ChanOutInt (wptr, s->to_kern, (int) ctb);
							ChanOutInt (wptr, s->to_kern, pony->state & 0xFFFF);
							ChanOutInt (wptr, s->to_kern, pony->state >> 16);
							ChanOutInt (wptr, s->to_kern, typedesc_chantype_nchans (wptr, pony->typedesc));
							ChanOutInt (wptr, s->to_kern, typedesc_chantype_server_read (wptr, pony->typedesc));

							CTRACE ("reading make.ctb.networked.confirm\n", 0);
							ChanInChar (wptr, s->from_kern, &tag);
							if (tag != PDI_make_ctb_networked_confirm) {
								CFATAL ("walk_inner: expected make.ctb.networked.confirm; got %d\n", 1, tag);
							}
							ChanInInt (wptr, s->from_kern, (int *) &nct_id);
							MTChanIn (wptr, s->from_kern, (void **) &nhh);
							MTChanIn (wptr, s->from_kern, (void **) &dha);
							MTChanIn (wptr, s->from_kern, (void **) &eha);

							CTRACE ("actually making the CTB networked -- nct.id now 0x%08x\n", 1, nct_id);
							make_ctb_networked (wptr, ctb, pony->typedesc, nct_id, nhh, dha, eha);

							MTRelease (wptr, nhh);
							MTRelease (wptr, dha);
							MTRelease (wptr, eha);
						}
						ccsp_cif_X_sem_release (wptr, &(pony->statesem));

						CTRACE ("output CT CLC %d; nct.id 0x%08x\n", 2, s->clc, (unsigned int) pony->state);
						ChanOutChar (wptr, s->to_kern, PDO_ct_end_nlc);
						ChanOutInt (wptr, s->to_kern, pony->state); /* the nct.id, now it's networked */
						ChanOutInt (wptr, s->to_kern, is_client ? 0 : 1);
						ChanOutInt (wptr, s->to_kern, is_shared ? 2 : 1);
						CTRACE ("output CT done\n", 0);
					} else {
						CTRACE ("cancelling CTB\n", 0);
					}

					/* The CTB will be freed when comm_list is processed. */
				}
				break;
			case PW_input:
				{
					/* For channel types, the typehash is actually the typedesc pointer. */
					unsigned int *typedesc = (unsigned int *) s->pdesc[0];
					mt_cb_t *ctb;
					char tag;

					CTRACE ("reading either clone.ctb or alloc.new.ctb\n", 0);
					ChanInChar (wptr, s->from_kern, &tag);
					switch (tag) {
					case PEI_clone_ctb:
						{
							ChanInInt (wptr, s->from_kern, (int *) &ctb);
							CTRACE ("increasing ref count on CTB 0x%08x\n", 1, (unsigned int) ctb);
							MTClone (wptr, ctb);
						}
						break;
					case PEI_alloc_new_ctb:
						{
							mt_cb_pony_state_t *pony;
							int nct_id;
							mt_cb_t *nhh;
							mt_array_t *dha, *eha;

							ChanInInt (wptr, s->from_kern, &nct_id);
							CTRACE ("alloc new CTB, NCT-ID 0x%08x\n", 1, nct_id);

							ctb = new_ctb (wptr, typedesc);
							pony = mt_cb_get_pony_state (ctb);

							CTRACE ("sending alloc.new.ctb.confirm\n", 0);
							ChanOutChar (wptr, s->to_kern, PEO_alloc_new_ctb_confirm);
							ChanOutInt (wptr, s->to_kern, (int) ctb);
							ChanOutInt (wptr, s->to_kern, typedesc_chantype_nchans (wptr, pony->typedesc));
							ChanOutInt (wptr, s->to_kern, typedesc_chantype_server_read (wptr, pony->typedesc));

							CTRACE ("reading alloc.new.ctb.confirm\n", 0);
							ChanInChar (wptr, s->from_kern, &tag);
							if (tag != PEI_alloc_new_ctb_confirm) {
								CFATAL ("walk_inner: expected alloc.new.ctb.confirm; got %d\n", 1, tag);
							}
							MTChanIn (wptr, s->from_kern, (void **) &nhh);
							MTChanIn (wptr, s->from_kern, (void **) &dha);
							MTChanIn (wptr, s->from_kern, (void **) &eha);

							CTRACE ("actually making the CTB networked; CTB is 0x%08x\n", 1, ctb);
							make_ctb_networked (wptr, ctb, pony->typedesc, nct_id, nhh, dha, eha);

							MTRelease (wptr, nhh);
							MTRelease (wptr, dha);
							MTRelease (wptr, eha);
						}
						break;
					default:
						CFATAL ("walk_inner: expecting clone.ctb or alloc.new.ctb; got %d\n", 1, tag);
					}

					CTRACE ("sending received end to user\n", 0);
					MTChanOut (wptr, s->user, (void **) &ctb);
					CTRACE ("CTB receive done\n", 0);
				}
				break;
			}
			s->pdesc++;
		}
		break;
	/*}}}*/
	/*{{{  unimplemented types */
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
		CFATAL ("walk_inner: unhandled type in typedesc: type %d length %d\n", 2, type, type_len);
	/*}}}*/
	}

	if (s->pdesc != orig_pdesc + type_len) {
		CFATAL ("walk_inner: typedesc size mismatch: type %d length %d, measured %d\n", 3, type, type_len, s->pdesc - orig_pdesc);
	}

	SCTRACE (s, "done\n", 0);
	return 0;
}
/*}}}*/
/*{{{ type_walker */
/*
 *	process for walking over type descriptors, doing counting, encoding or decoding as appropriate
 */
static const int type_walker_stacksize = 16384;
static void type_walker (Workspace wptr)
{
	Channel *pwi		= ProcGetParam (wptr, 0, Channel *);
	Channel *pwo		= ProcGetParam (wptr, 1, Channel *);
	Channel *user		= ProcGetParam (wptr, 2, Channel *);
	Channel *to_kern	= ProcGetParam (wptr, 3, Channel *);
	Channel *from_kern	= ProcGetParam (wptr, 4, Channel *);
	unsigned int *pdesc	= ProcGetParam (wptr, 5, unsigned int *);

	pony_walk_state state;

	state.mode = PW_count;
	state.pwi = pwi;
	state.pwo = pwo;
	state.user = user;
	state.to_kern = to_kern;
	state.from_kern = from_kern;
	state.deferred_min = 0;
	state.temp_list = NULL;
	state.temp_list_used = 0;
	state.temp_list_size = 0;
	state.comm_list = NULL;
	state.comm_list_used = 0;
	state.comm_list_size = 0;

	CTRACE ("starting; user channel at 0x%08x; to_kern 0x%08x; from_kern 0x%08x\n", 3, (unsigned int) user, (unsigned int) to_kern, (unsigned int) from_kern);
	while (1) {
		int stopped;
		char c;

		ChanInChar (wptr, pwi, &c);
		process_temp_list (wptr, &state);
		process_comm_list (wptr, &state);
		if (c == PWI_term) {
			break;
		} else if (c != PWI_reset) {
			CFATAL ("pony_walk_type: expected PWI_reset or PWI_term; got %d\n", 1, c);
		}
		ChanInInt (wptr, pwi, (int *) &state.mode);

		state.pdesc = pdesc;
		state.clc = 0;
		if (state.mode == PW_count) {
			state.clc_total = 0;
		}
		state.counts.di_first = 0;
		state.counts.ct_first = 0;
		state.counts.di_rest = 0;
		state.counts.ct_rest = 0;

		SCTRACE (&state, "walk starting in mode %d\n", 1, state.mode);
		stopped = walk_inner (wptr, &state);
		SCTRACE (&state, "walk finished: stopped = %d\n", 1, stopped);

		if (state.mode == PW_count) {
			pony_walk_counts *counts = &state.counts;
			ChanOutChar (wptr, pwo, PWO_counts);
			ChanOut (wptr, pwo, &counts, sizeof counts);

			state.clc_total = state.clc + 1;
		} else if (!stopped) {
			ChanOutChar (wptr, pwo, PWO_done);
		}

		if (state.mode == PW_output) {
			ChanXAble (wptr, pwi);
			ChanXIn (wptr, pwi, &c, 1);
			if (c == PWI_output_ok) {
				SCTRACE (&state, "output walk completed successfully; releasing user\n", 0);
				release_user (wptr, &state, 0);
				SCTRACE (&state, "processing comm_list\n", 0);
				process_comm_list (wptr, &state);
			} else if (c == PWI_output_cancelled) {
				SCTRACE (&state, "output walk was cancelled; discarding comm_list\n", 0);
				state.comm_list_used = 0;
			} else {
				CFATAL ("type_walker: expected PWI_output_ok or PWI_output_cancelled\n", 0);
			}
			ChanXEnd (wptr, pwi);
		}
	}

	free_walk_state (wptr, &state);
	CTRACE ("terminating\n", 0);
}
/*}}} */
/*}}} */

/*{{{ protocol decoder */
/*{{{ try_first_clc */
/*
 *	try to send the first.clc message to the pony kernel to say we've got data to send
 */
static const int try_first_clc_stacksize = 512;
static void try_first_clc (Workspace wptr)
{
	Channel *csync			= ProcGetParam (wptr, 0, Channel *);
	const pony_walk_counts *counts	= ProcGetParam (wptr, 1, const pony_walk_counts *);
	Channel *dh_out			= ProcGetParam (wptr, 2, Channel *);

	ChanOutChar (wptr, dh_out, PDO_first_clc);
	ChanOutInt (wptr, dh_out, counts->di_first);
	ChanOutInt (wptr, dh_out, counts->ct_first);
	ChanOutInt (wptr, dh_out, (counts->di_rest + counts->ct_rest) > 0);

	ChanOutChar (wptr, csync, 0);
}
/*}}}*/
/*{{{ cancel_decode_inner */
/*
 *	cancelling helper process (inside decode)
 */
static const int cancel_decode_inner_stacksize = 512;
static void cancel_decode_inner (Workspace wptr)
{
	Channel *csync	= ProcGetParam (wptr, 0, Channel *);
	Channel *dh_in	= ProcGetParam (wptr, 1, Channel *);
	int *cflag	= ProcGetParam (wptr, 2, int *);

	int idx;
	char tag;

	idx = ProcAlt (wptr, dh_in, csync, NULL);
	switch (idx) {
		/*{{{  0 -- communication from pony kernel*/
	case 0:
		ChanInChar (wptr, dh_in, &tag);
		CTRACE ("got cdi cancel\n", 0);
		if (tag == PDI_cancel) {
			ChanInChar (wptr, csync, &tag);
			*cflag = 1;
		} else {
			CFATAL ("cancel_decode_inner: expected cancel, got %d\n", 1, tag);
		}
		break;
		/*}}}*/
		/*{{{  1 -- cancel sync*/
	case 1:
		ChanInChar (wptr, csync, &tag);
		CTRACE ("cdi not cancelled\n", 0);
		*cflag = 0;
		break;
		/*}}}*/
		/*{{{  default -- error*/
	default:
		CFATAL ("cancel_decode_inner: bad ProcAlt index %d\n", 1, idx);
		/*}}}*/
	}
}
/*}}}*/
/*{{{ pony_protocol_decoder */
/*
 *	pony specific decoder implementation.  "in" is the application-level channel;
 *	"pdesc" points at the constant type description for this channel; "dhan" is a client-end
 *	for communication back to the pony kernel.
 */
static const int pony_protocol_decoder_stacksize = 8192;
static void pony_protocol_decoder (Workspace wptr)
{
	Channel *in		= ProcGetParam (wptr, 0, Channel *);
	unsigned int *pdesc	= ProcGetParam (wptr, 1, unsigned int *);
	mt_cb_t *dhan		= ProcGetParam (wptr, 2, mt_cb_t *);

	Channel pw_in, pw_out;
	Channel *dh_in = &(dhan->channels[0]);
	Channel *dh_out = &(dhan->channels[1]);
	Workspace walker;
	int running = 1;

	CTRACE ("protocol decoder starting (%08x)\n", 1, (int) in);
	ChanInit (wptr, &pw_in);
	ChanInit (wptr, &pw_out);
	walker = ProcAlloc (wptr, 6, type_walker_stacksize);
	ProcParam (wptr, walker, 0, &pw_in);
	ProcParam (wptr, walker, 1, &pw_out);
	ProcParam (wptr, walker, 2, in);
	ProcParam (wptr, walker, 3, dh_out);
	ProcParam (wptr, walker, 4, dh_in);
	ProcParam (wptr, walker, 5, pdesc);
	ProcStart (wptr, walker, type_walker);

	while (running) {
		char tag;
		int irun;

		CTRACE ("doing input -- managed channel at 0x%08x, control channel at 0x%08x\n", 2, (unsigned int)in, (unsigned int)dh_in);
		ChanIn (wptr, dh_in, &tag, 1);
		switch (tag) {
			/*{{{  PDI_activate -- start decoding*/
		case PDI_activate:
			CTRACE ("activate\n", 0);
			irun = 1;
			while (irun) {
				int idx;

				CTRACE ("alting; dh_in=0x%8.8x, in=0x%8.8x, *in=0x%8.8x\n", 3, (unsigned int)dh_in, (unsigned int)in, (unsigned int)*in);
				idx = ProcAlt (wptr, dh_in, in, NULL);
				switch (idx) {
					/*{{{  0 -- cancel from pony kernel*/
				case 0:
					CTRACE ("cancel 1\n", 0);
					ChanIn (wptr, dh_in, &tag, 1);
					if (tag == PDI_cancel) {
						irun = 0;
						tag = PDO_cancel_confirm;
						ChanOut (wptr, dh_out, &tag, 1);
					} else {
						CFATAL ("pony_protocol_decoder: expected cancel, got %d\n", 1, tag);
					}
					CTRACE ("cancel 1 done\n", 0);
					break;
					/*}}}*/
					/*{{{  1 -- from application (extended input)*/
				case 1:
					CTRACE ("input from app, channel=0x%8.8x cword=0x%8.8x (@-2=0x%8.8x, @-4=0x%8.8x, @-6=0x%8.8x)\n", 5, (unsigned int)in, *in, ((unsigned int*)(*in))[-2], ((unsigned int *)(*in))[-4], ((unsigned int*)(*in))[-6]);
					{
						Workspace i1, i2;
						word stack_i1[WORKSPACE_SIZE (3, try_first_clc_stacksize)];
						word stack_i2[WORKSPACE_SIZE (3, cancel_decode_inner_stacksize)];
						Channel csync;
						int was_cancelled = 0;
						char walk_tag;
						const pony_walk_counts *counts;
						int di_rest, ct_rest;

						CTRACE ("telling walker to count\n", 0);
						ChanOutChar (wptr, &pw_in, PWI_reset);
						ChanOutInt (wptr, &pw_in, PW_count);
						ChanInChar (wptr, &pw_out, &walk_tag);
						if (walk_tag != PWO_counts) {
							CFATAL ("pony_protocol_decoder: expected PWO_counts, got %d\n", 1, walk_tag);
						}
						ChanIn (wptr, &pw_out, &counts, sizeof counts);
						CTRACE ("walker count done -- received 0x%08x\n", 1, counts);
						CTRACE ("counts are %d %d %d %d\n", 4, counts->di_first, counts->ct_first, counts->di_rest, counts->ct_rest);
						di_rest = counts->di_rest;
						ct_rest = counts->ct_rest;

						ChanInit (wptr, &csync);

						i1 = LightProcInit (wptr, stack_i1, 3, try_first_clc_stacksize);
						ProcParam (wptr, i1, 0, &csync);
						ProcParam (wptr, i1, 1, counts);
						ProcParam (wptr, i1, 2, dh_out);

						i2 = LightProcInit (wptr, stack_i2, 3, cancel_decode_inner_stacksize);
						ProcParam (wptr, i2, 0, &csync);
						ProcParam (wptr, i2, 1, dh_in);
						ProcParam (wptr, i2, 2, &was_cancelled);

						ProcPar (wptr, 2, i1, try_first_clc, i2, cancel_decode_inner);

						if (was_cancelled) {
							CTRACE ("ifa cancelled\n", 0);
							irun = 0;
							ChanOutChar (wptr, dh_out, PDO_cancel_confirm);
							CTRACE ("ifa cancelled done\n", 0);
						} else {
							CTRACE ("ifa not cancelled -- outputting first CLC\n", 0);

							CTRACE ("in=0x%08x telling walker to output\n", 1, (unsigned int) in);
							ChanOutChar (wptr, &pw_in, PWI_reset);
							ChanOutInt (wptr, &pw_in, PW_output);
							ChanInChar (wptr, &pw_out, &walk_tag);
							CTRACE ("in=0x%08x got tag %d\n", 2, (unsigned int) in, walk_tag);

							ChanInChar (wptr, dh_in, &tag);
							switch (tag) {
							case PDI_cancel:
								CTRACE ("got cancel from kernel\n", 0);
								irun = 0;
								ChanOutChar (wptr, dh_out, PDO_cancel_confirm);
								if (walk_tag != PWO_done) {
									ChanOutChar (wptr, &pw_in, PWI_stop);
								}

								ChanOutChar (wptr, &pw_in, PWI_output_cancelled);

								CTRACE ("cancel confirmed -- deactivating\n", 0);
								break;
							case PDI_ack:
								/* "Reduce refcount of CTBs for first CLC" --
								 * will happen when walker comm_list is processed.
								 */
								CTRACE ("got ack from kernel\n", 0);
								if (walk_tag != PWO_done) {
									CTRACE ("sending rest.clcs\n", 0);
									ChanOutChar (wptr, dh_out, PDO_rest_clcs);
									ChanOutInt (wptr, dh_out, di_rest);
									ChanOutInt (wptr, dh_out, ct_rest);

									while (walk_tag != PWO_done) {
										ChanOutChar (wptr, &pw_in, PWI_next);
										ChanInChar (wptr, &pw_out, &walk_tag);
									}
									CTRACE ("in=0x%08x sending done; awaiting ack\n", 1, (unsigned int) in);

									ChanInChar (wptr, dh_in, &tag);
									if (tag != PDI_ack) {
										CFATAL ("pony_protocol_decoder: expecting ack\n", 0);
									}
									CTRACE ("in=0x%08x got ack\n", 1, (unsigned int) in);
									/* "Reduce refcount of remaining CTBs" --
									 * will happen when walker comm_list is
									 * processed.
									 */
								}

								ChanOutChar (wptr, &pw_in, PWI_output_ok);
								break;
							}
						}
					}
					CTRACE ("input from app done\n", 0);
					break;
					/*}}}*/
					/*{{{  default -- error*/
				default:
					CFATAL ("pony_protocol_decoder: bad ProcAlt index %d\n", 1, idx);
					/*}}}*/
				}
			}
			CTRACE ("deactivate\n", 0);
			break;
			/*}}}*/
			/*{{{  PDI_cancel_encode -- cancel encode*/
		case PDI_cancel_encode:
			CTRACE ("cancel.encode\n", 0);
			{
				int idx;
				char tag, walk_tag;

				idx = ProcAlt (wptr, dh_in, in, NULL);
				switch (idx) {
				/*{{{  dh_in */
				case 0:
					ChanInChar (wptr, dh_in, &tag);
					if (tag != PDI_cancel_encode_ack) {
						CFATAL ("pony_protocol_decoder: expected cancel.encode.ack, got %d\n", 1, tag);
					}
					CTRACE ("encode.not.cancelled\n", 0);
					ChanOutChar (wptr, dh_out, PDO_encode_not_cancelled);
					break;
				/*}}}*/
				/*{{{  in */
				case 1:
					CTRACE ("cancelling input from user\n", 0);
					ChanOutChar (wptr, &pw_in, PWI_reset);
					ChanOutInt (wptr, &pw_in, PW_cancel);
					ChanInChar (wptr, &pw_out, &walk_tag);
					CTRACE ("cancelling, received tag %d\n", 1, walk_tag);

					if (walk_tag != PWO_done) {
						ChanOutChar (wptr, &pw_in, PWI_stop);
					}

					CTRACE ("releasing protocol encoder after cancel\n", 0);
					ChanXEnd (wptr, in);

					CTRACE ("awaiting ack\n", 0);
					ChanInChar (wptr, dh_in, &tag);
					if (tag != PDI_cancel_encode_ack) {
						CFATAL ("pony_protocol_decoder: expected cancel.encode.ack, got %d\n", 1, tag);
					}
					CTRACE ("encode.cancelled\n", 0);
					ChanOutChar (wptr, dh_out, PDO_encode_cancelled);
					break;
				/*}}}*/
				}
				CTRACE ("done cancelling\n", 0);
			}
			break;
			/*}}}*/
			/*{{{  PDI_term -- terminate*/
		case PDI_term:
			CTRACE ("terminate\n", 0);
			running = 0;
			MTRelease (wptr, dhan);
			break;
			/*}}}*/
			/*{{{  default -- error*/
		default:
			CFATAL ("pony_protocol_decoder: expected PDI_active, PDI_cancel_encode or PDI_term, got %d\n", 1, tag);
			/*}}}*/
		}
	}

	ChanOutChar (wptr, &pw_in, PWI_term);
}
/*}}}*/
/*}}}*/

/*{{{ protocol encoder */
/*{{{ pony_protocol_encoder */
/*
 *	pony specific encoder implementation.  "out" is the application-level output channel;
 *	"prot_desc" points at the constant type description for this channel; "enc_handle" is a client-end
 *	for communication back to the pony kernel.
 */
static const int pony_protocol_encoder_stacksize = 8192;
static void pony_protocol_encoder (Workspace wptr)
{
	Channel *out		= ProcGetParam (wptr, 0, Channel *);
	unsigned int *pdesc	= ProcGetParam (wptr, 1, unsigned int *);
	mt_cb_t *ehan		= ProcGetParam (wptr, 2, mt_cb_t *);

	Channel pw_in, pw_out;
	Channel *eh_in = &(ehan->channels[0]);
	Channel *eh_out = &(ehan->channels[1]);
	Workspace walker;
	int running = 1;

	CTRACE ("protocol encoder starting (%08x)\n", 1, (int) out);
	ChanInit (wptr, &pw_in);
	ChanInit (wptr, &pw_out);
	walker = ProcAlloc (wptr, 6, type_walker_stacksize);
	ProcParam (wptr, walker, 0, &pw_in);
	ProcParam (wptr, walker, 1, &pw_out);
	ProcParam (wptr, walker, 2, out);
	ProcParam (wptr, walker, 3, eh_out);
	ProcParam (wptr, walker, 4, eh_in);
	ProcParam (wptr, walker, 5, pdesc);
	ProcStart (wptr, walker, type_walker);

	while (running) {
		char tag;

		CTRACE ("doing input -- managed channel at 0x%08x, control channel at 0x%08x\n", 2, (unsigned int)out, (unsigned int)eh_in);
		ChanIn (wptr, eh_in, &tag, 1);
		CTRACE ("done input\n", 0);
		switch (tag) {
			/*{{{  PEI_first_clc -- first CLC from pony kernel*/
		case PEI_first_clc:
			{
				char walk_tag;
				int has_rest;
				pony_walk_counts counts;

				ChanInInt (wptr, eh_in, &counts.di_first);
				ChanInInt (wptr, eh_in, &counts.ct_first);
				ChanInInt (wptr, eh_in, &has_rest);
				CTRACE ("first.clc %d %d %d\n", 3, counts.di_first, counts.ct_first, has_rest);

				/* We can't count the tree ourself, since we can't tell the size if it's got a tagged protocol in it. */

				ChanOutChar (wptr, &pw_in, PWI_reset);
				ChanOutInt (wptr, &pw_in, PW_input);
				ChanInChar (wptr, &pw_out, &walk_tag);

				CTRACE ("sending ack, having got %d from walker\n", 1, walk_tag);
				ChanOutChar (wptr, eh_out, PEO_ack);
				CTRACE ("ack taken\n", 0);

				if (walk_tag != PWO_done) {
					CTRACE ("waiting for cancel/rest.clcs from kernel\n", 0);
					ChanInChar (wptr, eh_in, &tag);
					if (tag == PEI_cancel) {
						CTRACE ("cancelled\n", 0);
						ChanOutChar (wptr, &pw_in, PWI_stop);
					} else if (tag == PEI_rest_clcs) {
						ChanInInt (wptr, eh_in, &counts.di_rest);
						ChanInInt (wptr, eh_in, &counts.ct_rest);
						CTRACE ("rest.clcs %d %d\n", 2, counts.di_rest, counts.ct_rest);

						CTRACE ("reading remaining NLCs\n", 0);
						while (walk_tag != PWO_done) {
							ChanOutChar (wptr, &pw_in, PWI_next);
							ChanInChar (wptr, &pw_out, &walk_tag);
							CTRACE ("read tag %d\n", 1, walk_tag);
						}
						CTRACE ("sending ack\n", 0);
						ChanOutChar (wptr, eh_out, PEO_ack);
						CTRACE ("ack sent\n", 0);
					} else {
						CFATAL ("pony_protocol_encoder: expected cancel or rest.clcs; got %d\n", 1, tag);
					}
				}
			}
			break;
			/*}}}*/
			/*{{{  PEI_term -- terminate*/
		case PEI_term:
			CTRACE ("term\n", 0);
			running = 0;
			MTRelease (wptr, ehan);
			break;
			/*}}}*/
			/*{{{  default -- error*/
		default:
			CFATAL ("pony_protocol_encoder: expecting PEI_first_clc or PEI_term, got %d\n", 1, tag);
			/*}}}*/
		}
	}

	ChanOutChar (wptr, &pw_in, PWI_term);
}
/*}}}*/
/*}}} */

/*{{{ new_ctb */
/*
 *	Allocate a new channel bundle, given a type descriptor.
 */
static mt_cb_t *new_ctb (Workspace wptr, unsigned int *typedesc)
{
	/* XXX should this always be SHARED? */
	word type = MT_SIMPLE | MT_MAKE_TYPE (MT_CB) | MT_CB_SHARED | MT_CB_STATE_SPACE;
	mt_cb_t *cb;
	mt_cb_pony_state_t *pony;

	cb = MTAlloc (wptr, type, typedesc_chantype_nchans (wptr, typedesc));

	pony = mt_cb_get_pony_state (cb);
	pony->typedesc = typedesc;
	pony->uiohook = NULL;

	return cb;
}
/*}}}*/
/*{{{ make_ctb_networked */
/*
 *	Given a channel bundle, set up the appropriate fields in it to make it
 *	networked, and fire off the protocol-encoder/decoder processes.
 */
static void make_ctb_networked (Workspace wptr, mt_cb_t *ctb, unsigned int *typedesc, int nct_id, mt_cb_t *nhh, mt_array_t *dec_handle_array, mt_array_t *enc_handle_array)
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
	int nchans = typedesc_chantype_nchans (wptr, typedesc);

	mt_cb_t **dha = (mt_cb_t **) dec_handle_array->data;
	mt_cb_t **eha = (mt_cb_t **) enc_handle_array->data;

	mt_cb_pony_state_t *pony = mt_cb_get_pony_state (ctb);

	/* store nct_id in the state-field */
	pony->state = nct_id;

	/* setup uio-hook */
	pony->uiohook = MTClone (wptr, nhh);

	/* XXX Hack: bump the reference count of the channel bundle, since pony
	 * now holds a reference to it (and it'll release it eventually). */
	MTClone (wptr, ctb);

	CTRACE ("starting, ctb @ 0x%8.8x\n", 1, (unsigned int)ctb);

	if (typedesc_id (typedesc[0]) != MTID_CHANTYPE) {
		CFATAL ("make_ctb_networked: not a channel-type!\n", 0);
	} else {
		/*{{{  create encode/decode processes*/
		int i, coffset;
		int fi, ti;

		for (i=0, fi=0, ti=(nchans-1), coffset=3; i<nchans; i++) {
			unsigned int *chandesc = typedesc + coffset;
			Workspace pd = 0, pe = 0;
			CTRACE ("ctb %08x channel %d is %08x\n", 3, ctb, i, &(ctb->channels[i]));

			switch (typedesc_id (chandesc[0])) {
				/*{{{  MTID_CHAN_I -- input channel*/
			case MTID_CHAN_I:
				pd = ProcAlloc (wptr, 3, pony_protocol_decoder_stacksize);
				ProcParam (wptr, pd, 0, &(ctb->channels[i]));
				ProcParam (wptr, pd, 1, &(chandesc[1]));
				ProcMTMove (wptr, pd, 2, &(dha[fi]));

				pe = ProcAlloc (wptr, 3, pony_protocol_encoder_stacksize);
				ProcParam (wptr, pe, 0, &(ctb->channels[i]));
				ProcParam (wptr, pe, 1, &(chandesc[1]));
				ProcMTMove (wptr, pe, 2, &(eha[fi]));

				fi++;
				break;
				/*}}}*/
				/*{{{  MTID_CHAN_O -- output channel*/
			case MTID_CHAN_O:
				pd = ProcAlloc (wptr, 3, pony_protocol_decoder_stacksize);
				ProcParam (wptr, pd, 0, &(ctb->channels[i]));
				ProcParam (wptr, pd, 1, &(chandesc[1]));
				ProcMTMove (wptr, pd, 2, &(dha[ti]));

				pe = ProcAlloc (wptr, 3, pony_protocol_encoder_stacksize);
				ProcParam (wptr, pe, 0, &(ctb->channels[i]));
				ProcParam (wptr, pe, 1, &(chandesc[1]));
				ProcMTMove (wptr, pe, 2, &(eha[ti]));

				ti--;
				break;
				/*}}}*/
			default:
				CFATAL ("make_ctb_networked: channel %d (at offset %d) not a channel ? (type = %d)\n", 3, i, coffset, typedesc_id (chandesc[0]));
			}
			coffset += typedesc_len (chandesc[0]);

			/* start processes */
			ProcStart (wptr, pd, pony_protocol_decoder);
			ProcStart (wptr, pe, pony_protocol_encoder);
		}
		/*}}}*/
	}

	CTRACE ("leaving, encoders and decoders started\n", 0);
}
/*}}}*/

/*{{{  CIF processes */
/*{{{ PROC CIF.pony.int.get.tdesc.data.uc (MOBILE.CHAN! cli, RESULT INT nchans, nsvrread, typehash) */
/*
 *	Extract data from a channel bundle's type descriptor.
 */
void pony_int_get_tdesc_data_uc (Workspace wptr)
{
	/* 0 is cli (we only care about the type descriptor) */
	unsigned int *tdesc	= ProcGetParam (wptr, 1, unsigned int *);
	int *nchans		= ProcGetParam (wptr, 2, int *);
	int *nsvrread		= ProcGetParam (wptr, 3, int *);
	int *typehash		= ProcGetParam (wptr, 4, int *);

	CTRACE ("get_tdesc_data (%08x)\n", 1, (int) tdesc);
	if (typedesc_id (tdesc[0]) != MTID_CHANTYPE) {
		*nchans = 0;
		*nsvrread = 0;
		*typehash = 0;
	} else {
		*nchans = typedesc_chantype_nchans (wptr, tdesc);
		*typehash = tdesc[2];
		*nsvrread = typedesc_chantype_server_read (wptr, tdesc);
	}
}
/*}}}*/
/*{{{ PROC CIF.pony.int.get.tdesc.data.sc (SHARED MOBILE.CHAN! cli, RESULT INT nchans, nsvrread, typehash) */
void pony_int_get_tdesc_data_sc (Workspace wptr)
{
	pony_int_get_tdesc_data_uc (wptr);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.get.tdesc.data.us (MOBILE.CHAN? svr, RESULT INT nchans, nsvrread, typehash) */
void pony_int_get_tdesc_data_us (Workspace wptr)
{
	pony_int_get_tdesc_data_uc (wptr);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.get.tdesc.data.ss (SHARED MOBILE.CHAN? svr, RESULT INT nchans, nsvrread, typehash) */
void pony_int_get_tdesc_data_ss (Workspace wptr)
{
	pony_int_get_tdesc_data_uc (wptr);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.clone.ctb.uc (RESULT MOBILE.CHAN! cli, VAL INT ctb.ptr) */
/*
 *	Clone a channel bundle end, given its address.
 */
void pony_int_clone_ctb_uc (Workspace wptr)
{
	/* 0 is cli return slot */
	/* 1 is typedesc for cli */
	int ctb_ptr = ProcGetParam (wptr, 2, int);

	CTRACE ("clone_ctb_uc (%08x)\n", 1, ctb_ptr);
	ProcParam (wptr, wptr, 0, MTClone (wptr, (mt_cb_t *) ctb_ptr));
}
/*}}}*/
/*{{{ PROC CIF.pony.int.clone.ctb.sc (RESULT SHARED MOBILE.CHAN! cli, VAL INT ctb.ptr) */
void pony_int_clone_ctb_sc (Workspace wptr)
{
	pony_int_clone_ctb_uc (wptr);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.clone.ctb.us (RESULT MOBILE.CHAN? svr, VAL INT ctb.ptr) */
void pony_int_clone_ctb_us (Workspace wptr)
{
	pony_int_clone_ctb_uc (wptr);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.clone.ctb.ss (RESULT SHARED MOBILE.CHAN? svr, VAL INT ctb.ptr) */
void pony_int_clone_ctb_ss (Workspace wptr)
{
	pony_int_clone_ctb_uc (wptr);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.alloc.ctb.uc (RESULT MOBILE.CHAN! cli, VAL INT nct.id, PONY.NETHOOKHANDLE! net.hook.handle, MOBILE []PONY.DECODEHANDLE! dec.handle.array, MOBILE []PONY.ENCODEHANDLE! enc.handle.array, RESULT INT ctb.ptr) */
/*
 *	Allocate a new channel bundle.
 *
 *	This must leave the CB with a reference count of 2, since the pony
 *	kernel expects to be able to hang on to the address even if no
 *	userspace stuff is using it.
 */
void pony_int_alloc_ctb_uc (Workspace wptr)
{
	/* 0 is cli return slot */
	unsigned int *cb_tdesc		= ProcGetParam (wptr, 1, unsigned int *);
	int nct_id			= ProcGetParam (wptr, 2, int);
	mt_cb_t *net_hook_handle	= ProcGetParam (wptr, 3, mt_cb_t *);
	/* 4 is data */
	mt_array_t *dec_handle_array	= ProcGetParam (wptr, 5, mt_array_t *);
	/* 6 is dimension 0 */
	/* 7 is data */
	mt_array_t *enc_handle_array	= ProcGetParam (wptr, 8, mt_array_t *);
	/* 9 is dimension 0 */
	int *ctb_ptr			= ProcGetParam (wptr, 10, int *);

	mt_cb_t *cb;

	CTRACE ("alloc_ctb_uc (%08x, %08x)\n", 2, (int) cb_tdesc, nct_id);
	cb = new_ctb (wptr, cb_tdesc);
	make_ctb_networked (wptr, cb, cb_tdesc, nct_id, net_hook_handle, dec_handle_array, enc_handle_array);

	ProcParam (wptr, wptr, 0, cb);
	ProcParam (wptr, wptr, 3, NULL);
	MTRelease (wptr, dec_handle_array);
	ProcParam (wptr, wptr, 5, NULL);
	MTRelease (wptr, enc_handle_array);
	ProcParam (wptr, wptr, 8, NULL);
	*ctb_ptr = (int) cb;
	CTRACE ("alloc_ctb_uc done\n", 0);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.alloc.ctb.sc (RESULT SHARED MOBILE.CHAN! cli, VAL INT nct.id, PONY.NETHOOKHANDLE! net.hook.handle, MOBILE []PONY.DECODEHANDLE! dec.handle.array, MOBILE []PONY.ENCODEHANDLE! enc.handle.array, RESULT INT ctb.ptr) */
void pony_int_alloc_ctb_sc (Workspace wptr)
{
	pony_int_alloc_ctb_uc (wptr);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.alloc.ctb.us (RESULT MOBILE.CHAN? svr, VAL INT nct.id, PONY.NETHOOKHANDLE! net.hook.handle, MOBILE []PONY.DECODEHANDLE! dec.handle.array, MOBILE []PONY.ENCODEHANDLE! enc.handle.array, RESULT INT ctb.ptr) */
void pony_int_alloc_ctb_us (Workspace wptr)
{
	pony_int_alloc_ctb_uc (wptr);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.alloc.ctb.ss (RESULT SHARED MOBILE.CHAN? svr, VAL INT nct.id, PONY.NETHOOKHANDLE! net.hook.handle, MOBILE []PONY.DECODEHANDLE! dec.handle.array, MOBILE []PONY.ENCODEHANDLE! enc.handle.array, RESULT INT ctb.ptr) */
void pony_int_alloc_ctb_ss (Workspace wptr)
{
	pony_int_alloc_ctb_uc (wptr);
}
/*}}}*/
/*{{{ PROC CIF.pony.int.shutdown.ctbs (MOBILE []INT ctbs) */
/*
 *	Free a list of channel bundles, given their addresses.
 */
void pony_int_shutdown_ctbs (Workspace wptr)
{
	/* 0 is data */
	mt_array_t *ctbs = ProcGetParam (wptr, 1, mt_array_t *);
	/* 2 is dimension 0 */

	int i;
	mt_cb_t **p;

	CTRACE ("shutdown_ctbs (%08x)\n", 1, (int) ctbs);
	p = (mt_cb_t **) ctbs->data;
	for (i = 0; i < ctbs->dimensions[0]; i++, p++) {
		if (*p != NULL) {
			mt_cb_get_pony_state (*p)->uiohook = NULL;
			MTRelease (wptr, *p);
		}
	}
}
/*}}}*/
/*{{{ PROC CIF.pony.int.get.nct.id (VAL INT ctb.ptr, RESULT INT nct.id, result) */
/*
 *	Retrieve the "nct-id", stored in the state field
 */
void pony_int_get_nct_id (Workspace wptr)
{
	int ctb_ptr	= ProcGetParam (wptr, 0, int);
	int *nct_id	= ProcGetParam (wptr, 1, int *);
	int *result	= ProcGetParam (wptr, 2, int *);

	mt_cb_pony_state_t *pony;

	CTRACE ("get_nct_id (%08x)\n", 1, ctb_ptr);
	pony = mt_cb_get_pony_state ((mt_cb_t *) ctb_ptr);
	if (pony->uiohook == NULL) {
		*nct_id = -1;
		*result = -2;
	} else {
		*nct_id = pony->state;
		*result = 0;
	}
}
/*}}}*/
/*}}}*/
