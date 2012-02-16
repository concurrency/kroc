/*
 *	ccsp_pony.h -- CCSP support for networked channels
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
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 *	02110-1301, USA.
 */

#ifndef __CCSP_PONY_H
#define __CCSP_PONY_H

#include <mobile_types.h>

/*{{{  mt_cb_pony_state_t */
/* For channel bundles with MT_CB_SHARED_SPACE set, this structure will be
 * stored in memory immediately after the last channel.
 */
typedef struct _mt_cb_pony_state_t {
	unsigned int	*typedesc;	/* generation 1 type descriptor */
	mt_cb_t		*uiohook;	/* channel bundle to pony kernel,
					   or NULL for non-networked bundle */
	int		state;		/* sharing state: 0xCCCCSSSS */
	mt_sem_t	statesem;	/* lock protecting state */
} _PACK_STRUCT mt_cb_pony_state_t;
/*}}}*/

/*{{{  static inline mt_cb_pony_state_t *mt_cb_get_pony_state (mt_cb_t *cb) */
/* Given a channel bundle, return a pointer to its pony structure.
 */
#if defined(__GNUC__)
__attribute__ ((unused)) /* make GCC ignore when unused */
#endif
static inline mt_cb_pony_state_t *mt_cb_get_pony_state (mt_cb_t *cb) {
	const word *cb_raw = (const word *) cb;
	const int num_channels = MT_CB_CHANNELS (cb_raw[MTType]);
	return (mt_cb_pony_state_t *) &(cb->channels[num_channels]);
}
/*}}}*/

#endif
