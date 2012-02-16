/*
 *	kif.c - kernel call table conversion cache
 *	Copyright (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include "main.h"
#include "kif.h"

static kif_entrytype 	*calltable[K_MAX_SUPPORTED];
static int		initialised = 0;

kif_entrytype *kif_entry (int offset) {
	if (!initialised) {
		int i;

		for (i = 0; i < K_MAX_SUPPORTED; ++i) {
			calltable[i] = NULL;
		}

		for (i = 0; i < (sizeof (ccsp_entrytable) / sizeof (ccsp_entrytype)); ++i) {
			ccsp_entrytype *entry = &(ccsp_entrytable[i]);
			calltable[entry->call_offset] = (kif_entrytype *) entry;
		}

		initialised = !initialised;
	}

	return calltable[offset];
}

