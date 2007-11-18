/*
 *	mobproc.c -- special handling for mobile processes
 *	Copyright (C) 2004-2005 Fred Barnes <frmb@kent.ac.uk>
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

/*{{{  general includes*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef RMOX_BUILD
	#include <rmox_if.h>
#else	/* !RMOX_BUILD */
	#include <stdio.h>
	#include <string.h>
	#include <sys/types.h>
	#include <sys/fcntl.h>
	#include <unistd.h>
#endif	/* !RMOX_BUILD */

#include <sched_consts.h>
#include <sched_types.h>
#include <rts.h>
#include <dmem_if.h>

#ifdef DYNAMIC_PROCS
#if defined(RMOX_BUILD)
#warning not using dynamic processes with RMoX
#else
#include <dlfcn.h>
#endif
#endif	/* DYNAMIC_PROCS */
/*}}}*/
/*{{{  mobile process includes/defines/types/locals*/
#include <mobproc.h>

#ifndef MAXNAMELENGTH
#define MAXNAMELENGTH 255
#endif

typedef struct TAG_cmapcache {
	void **codemap;
	char **namemap;
	int size;
} cmapcache_t;

static cmapcache_t **cached_maps = NULL;
static int n_cached_maps = 0;

#if defined(DYNAMIC_PROCS)

typedef struct TAG_rtlibrary {
	struct TAG_rtlibrary *next;
	char *lname;
	int llen;
	void *dlhandle;
	int rcount;
} rtlibrary_t;

static rtlibrary_t *loadedlibs = NULL;

#endif

/*}}}*/


/*{{{  static mp_mapchain *new_mapchain (void)*/
/*
 *	creates a new map-chain entry
 */
static mp_mapchain *new_mapchain (void)
{
	mp_mapchain *tmp = (mp_mapchain *)dmem_alloc (sizeof (mp_mapchain));

	tmp->next = NULL;
	tmp->mapdata = NULL;
	tmp->wsoffset = 0;
	return tmp;
}
/*}}}*/
/*{{{  static void free_mapchain (mp_mapchain *mc)*/
/*
 *	frees a map-chain entry
 */
static void free_mapchain (mp_mapchain *mc)
{
	dmem_release (mc);
	return;
}
/*}}}*/
/*{{{  static int decode_entry (unsigned char **ptr)*/
/*
 *	decodes an integer constant from a workspace-map
 *	encoding scheme is mentioned in mobproc.h
 *	advances ptr appropriately
 */
static int decode_entry (unsigned char **ptr)
{
	int val = 0;

	if (!(**ptr & 0x80)) {
		/* high bit zero, 7-bit signed number encoded */
		if (**ptr & 0x40) {
			/* sign bit set, -ve number */
			val = (int)(0xffffff80 | (**ptr & 0x7f));
		} else {
			val = (int)(**ptr & 0x7f);
		}
		*ptr = *ptr + 1;
	} else if ((**ptr & 0xc0) == 0x80) {
		/* high bit pair one-zero, 14-bit signed number encoded */
		if (**ptr & 0x20) {
			/* sign bit set, -ve number */
			val = (int)(0xffffc000 | ((**ptr & 0x3f) << 8) | (*ptr)[1]);
		} else {
			val = (int)(((**ptr & 0x3f) << 8) | (*ptr)[1]);
		}
		*ptr = *ptr + 2;
	} else {
		/* high bit pair one-one, 22-bit signed number encoded */
		if (**ptr & 0x20) {
			/* sign bit set, -ve number */
			val = (int)(0xffc00000 | ((**ptr & 0x3f) << 16) | ((*ptr)[1] << 8) | (*ptr)[2]);
		} else {
			val = (int)(((**ptr & 0x3f) << 16) | ((*ptr)[1] << 8) | (*ptr)[2]);
		}
		*ptr = *ptr + 3;
	}
	return val;
}
/*}}}*/
#if 0		/* not needed at the moment */
/*{{{  static int encode_entry (unsigned char **ptr, int val)*/
/*
 *	encodes an integer constant from a workspace-map
 *	if ptr is non-null, assigns there and advances it.  otherwise just counts.
 *	returns the number of bytes for the encoding
 */
static int encode_entry (unsigned char **ptr, int val)
{
	if ((val < (1 << 6)) && (val >= -(1 << 6))) {
		if (ptr) {
			(*ptr)[0] = (val & 0x7f);
			(*ptr)++;
		}
		return 1;
	} else if ((val < (1 << 13) && (val >= - (1 << 13)))) {
		if (ptr) {
			unsigned int uval = (unsigned int)val;

			(*ptr)[0] = 0x80 | ((uval >> 8) & 0x3f);
			(*ptr)[1] = (uval & 0xff);
			(*ptr) += 2;
		}
		return 2;
	} else if ((val < (1 << 21) && (val >= -(1 << 21)))) {
		if (ptr) {
			unsigned int uval = (unsigned int)val;

			(*ptr)[0] = (0xc0 | ((uval >> 16) & 0x3f));
			(*ptr)[1] = ((uval >> 8) & 0xff);
			(*ptr)[2] = (uval & 0xff);
			(*ptr) += 3;
		}
		return 3;
	}
	BMESSAGE ("encode_entry(): constant too big (%d)\n", val);
	return 0;
}
/*}}}*/
#endif
#if !defined(RMOX_BUILD)
/*{{{  static void dump_workspace (FILE *stream, void **wsptr, int wsbytes, mp_mapchain *mc)*/
/*
 *	dumps a block of workspace
 */
static void dump_workspace (FILE *stream, void **wsptr, int wsbytes, mp_mapchain *mc)
{
	int i;
	static char *typecstrings[] = {NULL, "\033[31m", "\033[1;31m", "\033[32m", "\033[1;32m", "\033[33m", "\033[1;33m", "\033[34m",
					"\033[1;34m", "\033[35m", "\033[1;35m", "\033[36m", "\033[1;36m", "\033[43m", "\033[45m", "\033[1;44m", NULL};
	void **orig_wsptr = wsptr;

	MESSAGETO (stream, "workspace for %p:", wsptr);
	for (i=0; wsbytes > 0; wsbytes -= 4, wsptr++, i++) {
		unsigned int ival = (unsigned int)(*wsptr);
		mp_mapchain *tmp;
		int did_attr = 0;

		if (!(i % 4)) {
			MESSAGETO (stream, "\n0x%8.8x: ", (unsigned int)wsptr);
		}
		/* this is horribly expensive, but hey-ho.. */
		MESSAGETO (stream, "  0x");
		for (tmp = mc; tmp; tmp=tmp->next) {
			unsigned char *mapptr = tmp->mapdata;
			int maplen = (int)((mapptr[2] << 8) | mapptr[3]);
			unsigned char *mapmax = mapptr + maplen + 4;
			void **wsbase = orig_wsptr + tmp->wsoffset;
			
			for (mapptr += 4; mapptr < mapmax;) {
				int offset = decode_entry (&mapptr);
				int type = decode_entry (&mapptr);
				int any;
				int nslots = 1;

				switch (type & WSMAP_TYPEMASK) {
				case WSMAP_MOB_DA:
					any = decode_entry (&mapptr);
					nslots = any;
					any = decode_entry (&mapptr);
					break;
				case WSMAP_MOB_CT:
					any = decode_entry (&mapptr);
					any = decode_entry (&mapptr);
					any = decode_entry (&mapptr);
					break;
				}
				if (((wsptr >= wsbase + offset) && (wsptr < wsbase + offset + nslots)) && (type >= 0) && (type < 16) && typecstrings[type]) {
					did_attr = 1;
					MESSAGETO (stream, "%s", typecstrings[type]);
				}
			}
		}
		MESSAGETO (stream, "%8.8x", ival);
		if (did_attr) {
			MESSAGETO (stream, "\033[0m");
		}
		fflush (stream);
	}
	MESSAGETO (stream, "\n");
	return;
}
/*}}}*/
/*{{{  static void dump_workspace_mapchain (FILE *stream, mp_mapchain *mc)*/
/*
 *	dumps a workspace map in human-readable form
 */
static void dump_workspace_mapchain (FILE *stream, mp_mapchain *mc)
{
	unsigned char *mapptr = mc->mapdata;
	int maplen = (int)((mapptr[2] << 8) | mapptr[3]);
	unsigned char *mapmax = mapptr + maplen + 4;

	MESSAGETO (stream, "workspace mapchain %p (at %p, wsoffset=%d):\n", mc, mapptr, mc->wsoffset);
	for (mapptr += 4; mapptr < mapmax;) {
		int offset = decode_entry (&mapptr);
		int type = decode_entry (&mapptr);
		int any;
		int nslots = 1;

		switch (type & WSMAP_TYPEMASK) {
		case WSMAP_MOB_DA:
			nslots = decode_entry (&mapptr);
			any = decode_entry (&mapptr);
			break;
		case WSMAP_MOB_CT:
			any = decode_entry (&mapptr);
			any = decode_entry (&mapptr);
			any = decode_entry (&mapptr);
			break;
		}
		MESSAGETO (stream, "    %-6d %-4d %d\n", offset, nslots, type);
	}
	return;
}
/*}}}*/
#endif
/*{{{  static void recover_entries (mp_mapchain *mc, mp_ctrlblk *mp)*/
/*
 *	this recovers any dynamic items that got left in a "dead" mobile process
 */
static void recover_entries (mp_mapchain *mc, mp_ctrlblk *mp)
{
	unsigned char *mapptr = mc->mapdata;
#if 0
	int mapentries = (int)((mapptr[0] << 8) | mapptr[1]);
#endif
	int maplen = (int)((mapptr[2] << 8) | mapptr[3]);
	unsigned char *mapmax = mapptr + maplen + 4;
	void **wsbase = mp->wsbase;

	mapptr += 4;		/* point at data start */
	/* offset workspace base for this map */
	wsbase += mc->wsoffset;
	while (mapptr < mapmax) {
		int offset = decode_entry (&mapptr);
		int type = decode_entry (&mapptr);

#if 0
MESSAGE ("recover_entries (mc = %p, mp = %p): offset = %d, type = %d\n", mc, mp, offset, type);
#endif
		switch (type & WSMAP_TYPEMASK) {
			/*{{{  WSMAP_MOB_DA*/
		case WSMAP_MOB_DA:
			{
				int nslots, basebytes;
				
				nslots = decode_entry (&mapptr);
				basebytes = decode_entry (&mapptr);

#if 0
MESSAGE ("recover_entries: WSMAP_MOB_DA: slots = %d, basebytes = %d\n", nslots, basebytes);
#endif
				if ((int)(wsbase[offset + 1])) {
					/* should be a pointer to free */
					dmem_release (wsbase[offset]);
					wsbase[offset + 1] = (void *)0;
				}
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_MOB_CT*/
		case WSMAP_MOB_CT:
			{
				int nchans, is_server, is_shared;
				unsigned int *chanwords = (unsigned int *)(wsbase[offset]);

				nchans = decode_entry (&mapptr);
				is_server = decode_entry (&mapptr);
				is_shared = decode_entry (&mapptr);

#if 0
MESSAGE ("recover_entries: WSMAP_MOB_CT: nchans = %d, is_server = %d, is_shared = %d\n", nchans, is_server, is_shared);
#endif
				if (chanwords[nchans] == 1) {
					/* last reference, can free this */
					chanwords[nchans] = 0;
					dmem_release (chanwords);
				} else {
					chanwords[nchans] = chanwords[nchans] - 1;
				}
				wsbase[offset] = (void *)0;
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_MOB_PT*/
		case WSMAP_MOB_PT:
			{
				mp_ctrlblk *process = (mp_ctrlblk *)(wsbase[offset]);

				if (process) {
					/* cleanup and recover this one */
					mpcb_rm_wsmap (process);
					dmem_release (process->wsbase);
					process->wsbase = NULL;
					if (process->vsbase) {
						dmem_release (process->vsbase);
						process->vsbase = NULL;
					}
					/* FIXME: mobilespace */
					if (process->msbase) {
						dmem_release (process->msbase);
						process->msbase = NULL;
					}
					dmem_release (process);
					wsbase[offset] = (void *)0;
				}
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_FIRSTPARAM*/
		case WSMAP_FIRSTPARAM:
			/* this marks the start of parameter maps, can't recover anything past here */
			mapptr = mapmax;
			break;
			/*}}}*/
		}
	}
	return;
}
/*}}}*/
/*{{{  static void remap_workspace (mp_ctrlblk *newproc, mp_ctrlblk *oldproc, mp_mapchain *mc, const int doparams)*/
/*
 *	this remaps pointers inside a workspace
 */
static void remap_workspace (mp_ctrlblk *newproc, mp_ctrlblk *oldproc, mp_mapchain *mc, const int doparams)
{
	void **newws = (void **)(newproc->wsbase);
	void **oldws = (void **)(oldproc->wsbase);
	unsigned char *mapptr = mc->mapdata;
#if 0
	int mapentries = (int)((mapptr[0] << 8) | mapptr[1]);
#endif
	int maplen = (int)((mapptr[2] << 8) | mapptr[3]);
	unsigned char *mapmax = mapptr + maplen + 4;
	int i;

	mapptr += 4;		/* point at data start */
	/* offset workspace base for this map */
	newws += mc->wsoffset;
	oldws += mc->wsoffset;
	while (mapptr < mapmax) {
		int offset = decode_entry (&mapptr);
		int type = decode_entry (&mapptr);

		switch (type & WSMAP_TYPEMASK) {
			/*{{{  WSMAP_CHANWORD*/
		case WSMAP_CHANWORD:
			newws[offset] = NotProcess_p;
			break;
			/*}}}*/
			/*{{{  WSMAP_CHANPTR, WSMAP_STATICLINK*/
		case WSMAP_CHANPTR:
		case WSMAP_STATICLINK:
			{
				int boffset;

				/* remap channel pointer or static-link */
				boffset = (int)(oldws[offset]) - (int)(oldws);
				newws[offset] = (void *)(((int)newws) + boffset);
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_GENPTR*/
		case WSMAP_GENPTR:
			if ((oldws[offset] >= oldproc->wsbase) && (oldws[offset] < (void *)((int)(oldproc->wsbase) + oldproc->wssize))) {
				/* remapping workspace pointer */
				int boffset = (int)(oldws[offset]) - (int)(oldws);
				
				newws[offset] = (void *)(((int)newws) + boffset);
			} else if ((oldws[offset] >= oldproc->vsbase) && (oldws[offset] < (void *)((int)(oldproc->vsbase) + oldproc->vssize))) {
				/* remapping vectorspace pointer */
				int boffset = (int)(oldws[offset]) - (int)(oldproc->vsbase);

				newws[offset] = (void *)((int)(newproc->vsbase) + boffset);
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_MOB_DA*/
		case WSMAP_MOB_DA:
			{
				int nslots, basebytes;
				
				nslots = decode_entry (&mapptr);
				basebytes = decode_entry (&mapptr);

				/* blank out the entry, clone happens separately */
				newws[offset] = NULL;
				for (i=1; i<nslots; i++) {
					newws[offset + i] = 0;
				}
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_MOB_CT*/
		case WSMAP_MOB_CT:
			{
				int nchans, is_server, is_shared;

				nchans = decode_entry (&mapptr);
				is_server = decode_entry (&mapptr);
				is_shared = decode_entry (&mapptr);

				/* blank out the entry, clone happens separately */
				newws[offset] = NULL;
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_MOB_PT*/
		case WSMAP_MOB_PT:
			{
				/* just blank out the slot for these --- clone must duplicate */
				newws[offset] = NULL;
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_FIRSTPARAM*/
		case WSMAP_FIRSTPARAM:
			if (!doparams) {
				/* stop here */
				mapptr = mapmax;
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_MPP*/
		case WSMAP_MPP:
			if (oldws[offset] == (void *)oldproc) {
				newws[offset] = (void *)newproc;
			} else {
				/* can't remap something else like this -- should not happen! */
				newws[offset] = NULL;
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_VSPTR*/
		case WSMAP_VSPTR:
			{
				int boffset;

				/* remap vectorspace pointer */
				boffset = (int)(oldws[offset]) - (int)(oldproc->vsbase);
				newws[offset] = (void *)((int)(newproc->vsbase) + boffset);
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_CODEPTR*/
		case WSMAP_CODEPTR:
			/* nothing for this locally */
			break;
			/*}}}*/
		}
	}
	return;
}
/*}}}*/
/*{{{  static int clone_workspace (void **newws, void **oldws, mp_mapchain *mc, const int doparams)*/
/*
 *	this clones dynamic objects inside a workspace.
 *	returns 0 on failure, non-zero on success
 */
static int clone_workspace (void **newws, void **oldws, mp_mapchain *mc, const int doparams)
{
	unsigned char *mapptr = mc->mapdata;
#if 0
	int mapentries = (int)((mapptr[0] << 8) | mapptr[1]);
#endif
	int maplen = (int)((mapptr[2] << 8) | mapptr[3]);
	unsigned char *mapmax = mapptr + maplen + 4;
	int i;
	int seen_first_param = 0;

	mapptr += 4;		/* point at data start */
	/* offset workspace base for this map */
	newws += mc->wsoffset;
	oldws += mc->wsoffset;
#if 0
MESSAGE ("clone_workspace(): newws = %p, oldws = %p\n", newws, oldws);
#endif
	while (mapptr < mapmax) {
		int offset = decode_entry (&mapptr);
		int type = decode_entry (&mapptr);

		switch (type & WSMAP_TYPEMASK) {
			/*{{{  WSMAP_MOB_DA*/
		case WSMAP_MOB_DA:
			{
				int nslots = decode_entry (&mapptr);
				int basebytes = decode_entry (&mapptr);

#if 0
MESSAGE ("clone_workspace(): MOB_DA: basebytes = %d, nslots = %d, oldws + offset = %p, oldws[offset + 1] = %d, oldws[offset] = %p\n",
		basebytes, nslots, oldws + offset, (int)(oldws[offset + 1]), oldws[offset]);
#endif
				/* blank out the entry, clone happens separately */
				if (oldws[offset + 1]) {
					/* duplicate the array */
					int size = basebytes;

					for (i=1; i<nslots; i++) {
						newws[offset + i] = oldws[offset + i];
						size *= (int)(newws[offset + i]);
					}
#if 0
MESSAGE ("clone_workspace(): MOB_DA: size = %d, basebytes = %d, nslots = %d\n", size, basebytes, nslots);
#endif
					newws[offset] = (void *)dmem_alloc (size);
					memcpy (newws[offset], oldws[offset], size);
				}
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_MOB_CT*/
		case WSMAP_MOB_CT:
			{
				int nchans;
				int is_server;
				int is_shared;
				unsigned int *chanwords;

				nchans = decode_entry (&mapptr);
				is_server = decode_entry (&mapptr);
				is_shared = decode_entry (&mapptr);
#if 0
MESSAGE ("clone_workspace(): MOB_CT: nchans = %d, is_server = %d, is_shared = %d\n", nchans, is_server, is_shared);
#endif
				if (!is_shared) {
					if (oldws[offset]) {
						/* defined -- not allowed to clone */
						return 0;
					}
					/* otherwise undefined */
					newws[offset] = (void *)0;
				} else {
					newws[offset] = oldws[offset];
					chanwords = (unsigned int *)(newws[offset]);
					/* increase reference-count -- but only if not a parameter! */
					if (!seen_first_param) {
						chanwords[nchans] = chanwords[nchans] + 1;
					}
				}
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_MOB_PT*/
		case WSMAP_MOB_PT:
			{
				mp_ctrlblk *process = (mp_ctrlblk *)(oldws[offset]);
				mp_ctrlblk *newp;

				if (process) {
					/* well, blind clone..! */
					newp = mpcb_mpp_clone (process);
					if (!newp) {
						/* failed to clone the process */
						return 0;
					}
					newws[offset] = (void *)newp;
				} else {
					newws[offset] = NULL;
				}
			}
			break;
			/*}}}*/
			/*{{{  WSMAP_FIRSTPARAM*/
		case WSMAP_FIRSTPARAM:
			if (!doparams) {
				/* stop here */
				mapptr = mapmax;
			}
			seen_first_param = 1;
			break;
			/*}}}*/
		}
	}
	return 1;
}
/*}}}*/
/*{{{  static int mcache_check (void *addr, char *name, void ***cmapp, char ***nmapp, int *sizep)*/
/*
 *	checks map-cache to see if we already have this one loaded -- does a simple pointer comparison
 *	returns 0 if no match, number of entries if found
 */
static int mcache_check (void *addr, char *name, void ***cmapp, char ***nmapp, int *sizep)
{
	int i;

	for (i=0; i<n_cached_maps; i++) {
		if ((cached_maps[i]->codemap[0] == addr) && (cached_maps[i]->namemap[0] == name)) {
			*cmapp = cached_maps[i]->codemap;
			*nmapp = cached_maps[i]->namemap;
			*sizep = cached_maps[i]->size;
			return cached_maps[i]->size;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static void mcache_add (void **cmap, char **nmap, int entries)*/
/*
 *	adds an entry to the map-cache
 */
static void mcache_add (void **cmap, char **nmap, int entries)
{
	cmapcache_t *cmc = (cmapcache_t *)dmem_alloc (sizeof (cmapcache_t));

	cmc->codemap = cmap;
	cmc->namemap = nmap;
	cmc->size = entries;

	if (!(n_cached_maps & 0x07)) {
		/* increase size */
		if (!cached_maps) {
			cached_maps = (cmapcache_t **)dmem_alloc (n_cached_maps + 8);
		} else {
			cmapcache_t **newmaps = (cmapcache_t **)dmem_alloc (n_cached_maps + 8);

			memcpy (newmaps, cached_maps, n_cached_maps * sizeof (cmapcache_t *));
			dmem_release (cached_maps);
			cached_maps = newmaps;
		}
	}

	/* add this one */
	cached_maps[n_cached_maps] = cmc;
	n_cached_maps++;

	return;
}
/*}}}*/
/*{{{  static void mcache_clearall (void)*/
/*
 *	clears all cached maps
 */
static void mcache_clearall (void)
{
	int i;

	for (i=0; i<n_cached_maps; i++) {
		dmem_release (cached_maps[i]);
		cached_maps[i] = NULL;
	}
	n_cached_maps = 0;

	return;
}
/*}}}*/
/*{{{  static char *make_c_name (char *oname, int olen)*/
/*
 *	transforms an occam-name, "my.proc", into a real label name, "O_my_proc"
 *	returns a pointer to a local static buffer
 */
static char *make_c_name (char *oname, int olen)
{
	static char namebuf[MAXNAMELENGTH];
	int i;

	if ((olen + 2) >= MAXNAMELENGTH) {
		olen = MAXNAMELENGTH - 3;
	}
	namebuf[0] = 'O';
	namebuf[1] = '_';
	for (i=2; olen; i++, oname++, olen--) {
		switch (*oname) {
		case '.':
			namebuf[i] = '_';
			break;
		default:
			namebuf[i] = *oname;
			break;
		}
	}
	namebuf[i] = '\0';

	return (char *)namebuf;
}
/*}}}*/
/*{{{  static void word_roundup (int *val)*/
/*
 *	rounds up the given integer to the next word boundary
 */
static void word_roundup (int *val)
{
	if (*val & 0x03) {
		*val = (*val & ~0x03) + 4;
	}
	return;
}
/*}}}*/


/*{{{  void mpcb_add_wsmap (mp_ctrlblk *mp, unsigned char *mapdata, unsigned int *wptr)*/
/*
 *	adds a workspace map to a mobile-process chain
 */
void mpcb_add_wsmap (mp_ctrlblk *mp, unsigned char *mapdata, unsigned int *wptr)
{
	mp_mapchain **mcp;

#if 0
MESSAGE ("mpcb_add_wsmap: adding workspace map for process block at %p, mapdata at %p\n", mp, mapdata);
#endif
	if (!mp || !mapdata) {
		return;
	}
	for (mcp = &(mp->mapchain); *mcp; mcp = &((*mcp)->next));
	*mcp = new_mapchain ();
	(*mcp)->mapdata = mapdata;
	(*mcp)->mapsize = (int)((mapdata[0] << 8) | mapdata[1]);
	(*mcp)->wsoffset = (((int)wptr - (int)(mp->wsbase)) >> WSH);
#if 0
MESSAGE ("mpcb_add_wsmap: mp=%p chain=%p mapdata=%p (%d entries, %d bytes) Wptr=%p, (*mcp)->wsoffset=%d\n",
		mp, mp->mapchain, mapdata, (int)((mapdata[0] << 8) | mapdata[1]), (int)((mapdata[2] << 8) | mapdata[3]), wptr, (*mcp)->wsoffset);
#endif
#if 0
dump_workspace_mapchain (stderr, *mcp);
#endif

	return;
}
/*}}}*/
/*{{{  void mpcb_del_wsmap (mp_ctrlblk *mp, unsigned char *mapdata, unsigned int *wptr)*/
/*
 *	called to remove a map-entry from a mobile process
 */
void mpcb_del_wsmap (mp_ctrlblk *mp, unsigned char *mapdata, unsigned int *wptr)
{
	mp_mapchain **mcp, *tmp;
	int eoffset = (((int)wptr - (int)(mp->wsbase)) >> WSH);
	int mapsize = (int)((mapdata[0] << 8) | mapdata[1]);

#if 0
MESSAGE ("mpcb_del_wsmap: removing workspace map for process block at %p, mapdata at %p\n", mp, mapdata);
#endif
	if (!mp || !mapdata) {
		return;
	}
	for (mcp = &(mp->mapchain); *mcp && ((*mcp)->wsoffset != eoffset) && ((*mcp)->mapsize != mapsize) && !(((*mcp)->mapdata == mapdata) || (!(*mcp)->mapdata)); mcp = &((*mcp)->next));
#if 0
MESSAGE ("mpcb_del_wsmap: mp=%p chain=%p mapdata=%p (%d entries, %d bytes) Wptr=%p\n",
		mp, mp->mapchain, mapdata, (int)((mapdata[0] << 8) | mapdata[1]), (int)((mapdata[2] << 8) | mapdata[3]), wptr);
#endif
	if (!*mcp) {
#if 0
MESSAGE ("mpcb_del_wsmap: no such map!\n");
#endif
		return;
	}
	tmp = *mcp;
	*mcp = (*mcp)->next;
	free_mapchain (tmp);

#if 0
MESSAGE ("mpcb_del_wsmap: returning\n");
#endif
	return;
}
/*}}}*/
/*{{{  void mpcb_rm_wsmap (mp_ctrlblk *mp)*/
/*
 *	called to clean-up a mobile process -- recovers dynamic data inside the process, etc.
 */
void mpcb_rm_wsmap (mp_ctrlblk *mp)
{
	mp_mapchain *next, *tmp;

#if 0
MESSAGE ("mpcb_rm_wsmap: mp=%p chain=%p\n", mp, mp->mapchain);
#endif
	for (tmp = mp->mapchain; tmp; tmp = next) {
		next = tmp->next;
		recover_entries (tmp, mp);
		free_mapchain (tmp);
	}
	return;
}
/*}}}*/
/*{{{  mp_ctrlblk *mpcb_mpp_clone (mp_ctrlblk *mp)*/
/*
 *	produces a clone of a mobile process, using its workspace map
 */
mp_ctrlblk *mpcb_mpp_clone (mp_ctrlblk *mp)
{
	mp_ctrlblk *tmp;
	mp_mapchain **mcp, *mc;
	int ws_byte_adjust = 0;

	tmp = (mp_ctrlblk *)dmem_alloc (sizeof (mp_ctrlblk));

	/*{{{  copy entire workspace and vectorspace first*/
	tmp->wsbase = (void *)dmem_alloc (mp->wssize);
	tmp->wssize = mp->wssize;
	memcpy (tmp->wsbase, mp->wsbase, mp->wssize);

	ws_byte_adjust = (int)(tmp->wsbase) - (int)(mp->wsbase);

	if (mp->vsbase) {
		tmp->vsbase = (void *)dmem_alloc (mp->vssize);
		tmp->vssize = mp->vssize;
		memcpy (tmp->vsbase, mp->vsbase, mp->vssize);
	} else {
		tmp->vsbase = NULL;
	}


	/*}}}*/
	/*{{{  then mobilespace (FIXME)*/
	if (mp->msbase) {
		tmp->msbase = (void *)dmem_alloc (mp->mssize);
		tmp->mssize = mp->mssize;
		tmp->mshook = mp->mshook;
		tmp->msdesc = mp->msdesc;
		memcpy (tmp->msbase, mp->msbase, mp->mssize);
	} else {
		tmp->msbase = NULL;
	}

	/*}}}*/
	/*{{{  code offsets do not change, copy pointers directly*/
	tmp->iptr = mp->iptr;
	tmp->aiptr = mp->aiptr;
	tmp->mapchain = mp->mapchain;
	for (mcp = &(tmp->mapchain); *mcp; mcp = &((*mcp)->next)) {
		mp_mapchain *nmc = new_mapchain ();

		nmc->next = (*mcp)->next;
		nmc->mapdata = (*mcp)->mapdata;
		nmc->wsoffset = (*mcp)->wsoffset;
		*mcp = nmc;
	}

	/*}}}*/
	/*{{{  remap pointers in the workspace and CLONE any nested items*/
#if 0
MESSAGE ("mpcb_mpp_clone(): remapping from wsbase=%p to wsbase=%p..\n", mp->wsbase, tmp->wsbase);
#endif
	for (mc = tmp->mapchain; mc; mc = mc->next) {
		remap_workspace (tmp, mp, mc, (mc != tmp->mapchain));
		if (!clone_workspace ((void **)(tmp->wsbase), (void **)(mp->wsbase), mc, (mc != tmp->mapchain))) {
			mp_mapchain *nextmap;

			/* failed to clone..! */
			if (tmp->vsbase) {
				dmem_release (tmp->vsbase);
			}
			if (tmp->wsbase) {
				dmem_release (tmp->wsbase);
			}
			for (mc = tmp->mapchain; mc; mc = nextmap) {
				nextmap = mc->next;
				dmem_release ((void *)mc);
			}
			dmem_release (tmp);

			return NotProcess_p;
		}
	}

	/*}}}*/
	/*{{{  repair suspended Wptr*/
	if (mp->wptr != NotProcess_p) {
		tmp->wptr = (void *)((int)(mp->wptr) + ws_byte_adjust);
	} else {
		tmp->wptr = NotProcess_p;
	}

	/*}}}*/
	/* FIXME: mp->barrier */
	#if 0
	/*{{{  repair the list of suspended processes*/
	tmp->becnt = mp->becnt;
	tmp->bcnt = mp->bcnt;
	if (mp->bfptr) {
		void *pptr;

		tmp->bfptr = (void *)((int)(mp->bfptr) + ws_byte_adjust);
		tmp->bbptr = (void *)((int)(mp->bbptr) + ws_byte_adjust);

		for (pptr = tmp->bfptr; pptr != tmp->bbptr; pptr = ((void **)pptr)[Link]) {
			void **linkp = &(((void **)pptr)[Link]);

			if (*linkp) {
				*linkp = (void *)((int)(*linkp) + ws_byte_adjust);
			}
		}
	} else {
		tmp->bfptr = NULL;
		tmp->bbptr = NULL;
	}
	/*}}}*/
	#endif
#if 0
dump_workspace (stderr, (void **)(mp->wsbase), mp->wssize, mp->mapchain);
dump_workspace (stderr, (void **)(tmp->wsbase), tmp->wssize, tmp->mapchain);
#endif

	return tmp;
}
/*}}}*/
/*{{{  static int checkmax (void ***mapentries, char ***nameentries, int *nentries, int *nmax, int n)*/
/*
 *	checks the size of the arrays and extends for n more if necessary
 *	returns the number of entries free
 */
static int checkmax (void ***mapentries, char ***nameentries, int *nentries, int *nmax, int n)
{
	if ((*nentries + n) >= *nmax) {
		int bytes = sizeof (void *) * (*nmax + n);
		void **newmaps = (void **)dmem_alloc (bytes);
		char **newnames = (char **)dmem_alloc (bytes);
		int i;

#if 0
fprintf (stderr, "checkmax(): resizing: nentries=%d, nmax=%d, n=%d, bytes=%d, mapentries @%p, nameentries @%p, newmaps @%p, newnames @%p\n", *nentries, *nmax, n, bytes, *mapentries, *nameentries, newmaps, newnames);
#endif
		if (*nmax) {
			memcpy (newmaps, *mapentries, *nmax * sizeof (void *));
			memcpy (newnames, *nameentries, *nmax * sizeof (char *));
		}
		for (i=*nmax; i < (*nmax + n); i++) {
			newmaps[i] = NULL;
			newnames[i] = NULL;
		}
		if (*nmax) {
			dmem_release (*mapentries);
			dmem_release (*nameentries);
		}

		*nmax = *nmax + n;
		*mapentries = newmaps;
		*nameentries = newnames;
	}
	return *nmax - *nentries;
}
/*}}}*/
/*{{{  static int make_codemap (void **cmap, void ***mapentries, char ***nameentries, int *nentries, int *nmax)*/
/*
 *	builds a codemap (recursively);  returns the number of entries added.
 */
static int make_codemap (void **cmap, void ***mapentries, char ***nameentries, int *nentries, int *nmax)
{
	int nsubmaps = (int)(cmap[3]);
	int i, r = 0;

	checkmax (mapentries, nameentries, nentries, nmax, 8);

#if 0
fprintf (stderr, "make_codemap(): map ptr 0x%8.8x, nentries=%d, nmax=%d, mapentries @%p, namenetries @%p\n", (unsigned int)cmap[0], *nentries, *nmax, *mapentries, *nameentries);
#endif
	/* add the top-level */
	for (i=0; i<*nentries; i++) {
		if ((*mapentries)[i] == cmap[0]) {
			/* got this one already */
#if 0
fprintf (stderr, "make_codemap(): skipping map ptr at 0x%8.8x\n", (unsigned int)cmap[0]);
#endif
			return 0;
		}
	}

	(*mapentries)[*nentries] = cmap[0];
	(*nameentries)[*nentries] = (char *)(cmap[2]);
	*nentries = *nentries + 1;
	r = 1;

	for (i=0; i<nsubmaps; i++) {
		void **nextmap = (void **)(cmap[(i << 2) + 5]);
		void *taddr = cmap[(i << 2) + 4];
		char *tname = (char *)(cmap[(i << 2) + 6]);

		checkmax (mapentries, nameentries, nentries, nmax, 8);

#if 0
fprintf (stderr, "make_codemap(): submaps: taddr=0x%8.8x, nextmap=0x%8.8x, tname=[%s]\n", (unsigned int)taddr, (unsigned int)nextmap, tname);
#endif
		if ((int)nextmap != -1) {
			r += make_codemap (nextmap, mapentries, nameentries, nentries, nmax);
		} else {
			int j;

			/* check that we didn't already add this (shouldn't have) */
			for (j=0; (j<*nentries) && ((*mapentries)[j] != taddr); j++);
#if 0
fprintf (stderr, "make_codemap(): adding submap entry 0x%8.8x if (%d == %d)\n", (unsigned int)taddr, j, *nentries);
#endif
			if (j == *nentries) {
				/* add this one */
				(*mapentries)[*nentries] = taddr;
				(*nameentries)[*nentries] = tname;
				*nentries = *nentries + 1;
				r++;
			}
		}
	}

	return r;
}
/*}}}*/
/*{{{  int mpcb_mpp_serialise (mp_ctrlblk **mpp, unsigned int *thashp, int *raddr, int *rsize)*/
/*
 *	serialises a mobile process
 *	returns 0 on failure, non-zero on success
 */
int mpcb_mpp_serialise (mp_ctrlblk **mpp, unsigned int *thashp, int *raddr, int *rsize)
{
	mp_ctrlblk *blk = *mpp;
	int bytes = 0;
	unsigned int *sblk = NULL;	/* serialised block, organised as integers */
	mp_filehdr *fhdr = NULL;
	unsigned int *sws = NULL;
	unsigned int *svs = NULL;
	unsigned int *rws = NULL;
	unsigned int *rvs = NULL;
	void **codemap = NULL;
	int nlen;
	mp_mapchain *mc = NULL;

	int ncmapentries = 0;
	int mcmapentries = 0;
	void **cmapentries = NULL;
	char **cmaplabels = NULL;

	int wsmbytes = 0;
	int wsmblks = 0;
	unsigned int *swsmap = NULL;
	int swsoffs = 0;

	int objspacebytes = 0;
	int objspaceents = 0;
	unsigned int *objspacebiptr = NULL;
	char *objspaceptr = NULL;

#if 0
mpcb_dump_process (blk);
dump_workspace (stderr, (void **)blk->wsbase, blk->wssize, blk->mapchain);
#endif

#if 0
MESSAGE ("mpcb_mpp_serialise (mpp = %p, *mpp = %p, thashp = %p, *thashp = 0x%8.8x, raddr = %p, rsize = %p)\n", mpp, *mpp, thashp, *thashp, raddr, rsize);
#endif

	/*{{{  get codemap (cached, or build fresh)*/
	codemap = (void **)blk->codemap;
	if (!codemap) {
		BMESSAGE ("error: cannot serialise mobile process (no code-map)\n");
		return 0;
	}
	ncmapentries = mcache_check (codemap[0], (char *)codemap[2], &cmapentries, &cmaplabels, &mcmapentries);
	if (!ncmapentries) {
		make_codemap (codemap, &cmapentries, &cmaplabels, &ncmapentries, &mcmapentries);
#if 0
{ int i;
MESSAGE ("codemap (%d/%d) cmapentries @%p, cmaplabels @%p:\n", ncmapentries, mcmapentries, cmapentries, cmaplabels);
for (i=0; i<ncmapentries; i++) {
	MESSAGE ("    0x%8.8x  %s\n", (unsigned int)(cmapentries[i]), cmaplabels[i]);
}
}
#endif
		mcache_add (cmapentries, cmaplabels, ncmapentries);
	}

	/*}}}*/
#if 0
{ int i;
MESSAGE ("codemap (%d/%d):\n", ncmapentries, mcmapentries);
for (i=0; i<ncmapentries; i++) {
	MESSAGE ("    0x%8.8x  %s\n", (unsigned int)(cmapentries[i]), cmaplabels[i]);
}
}
#endif
	/*{{{  determine how much space we need, allocate and populate header */
	nlen = strlen (cmaplabels[0]) + 1;
	word_roundup (&nlen);

	bytes = sizeof (mp_filehdr) + blk->wssize + (blk->vsbase ? blk->vssize : 0) + nlen;
	/* FIXME: mobilespace size */

	/*{{{  walk through workspace map-chain and count up size -- some things collected in objectspace*/
	rws = blk->wsbase;
	wsmbytes = 0;
	for (mc = blk->mapchain; mc; mc = mc->next) {
		unsigned char *mapptr = mc->mapdata;
		int maplen = (int)((mapptr[2] << 8) | mapptr[3]);
		unsigned char *mapmax = mapptr + maplen + 4;
		void **orgws = (void **)rws + mc->wsoffset;
#if 0
fprintf (stderr, "serialising from workspace map at %p: %d bytes, %d entries\n", mapptr, maplen, (int)((mapptr[0] << 8) | mapptr[1]));
#endif

		/* this bit counts size for objectspace (objspacebytes) */
		mapptr += 4;		/* point at data start */
		while (mapptr < mapmax) {
			int offset = decode_entry (&mapptr);
			int type = decode_entry (&mapptr);

			switch (type & WSMAP_TYPEMASK) {
				/*{{{  WSMAP_CHANWORD, WSMAP_CHANPTR, WSMAP_STATICLINK, WSMAP_GENPTR, WSMAP_FIRSTPARAM, WSMAP_MPP, WSMAP_VSPTR, WSMAP_MSPTR, WSMAP_CODEPTR*/
			case WSMAP_CHANWORD:
			case WSMAP_CHANPTR:
			case WSMAP_STATICLINK:
			case WSMAP_GENPTR:
			case WSMAP_FIRSTPARAM:
			case WSMAP_MPP:
			case WSMAP_VSPTR:
			case WSMAP_MSPTR:
			case WSMAP_CODEPTR:
				/* always going to be empty if we SUSPENDed */
				break;
				/*}}}*/
				/*{{{  WSMAP_MOB_DA*/
			case WSMAP_MOB_DA:
				{
					int nslots, basebytes, icount;
					int i;

					nslots = decode_entry (&mapptr);
					basebytes = decode_entry (&mapptr);

					icount = basebytes;
					for (i=1; i<nslots; i++) {
						icount *= (int)orgws[offset + i];
					}
#if 0
MESSAGE ("mpcb_mpp_serialise(): WSMAP_MOB_DA: nslots=%d, basebytes=%d, icount=%d\n", nslots, basebytes, icount);
#endif
					if (icount) {
						/* save [nslots,basebytes,dim1..dimN] before actual data */
						icount += ((nslots + 1) * sizeof (unsigned int));

						/* round up if not word aligned */
						word_roundup (&icount);
						objspacebytes += icount;

						objspaceents++;
					}
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_MOB_CT*/
			case WSMAP_MOB_CT:
				{
					int nchans, is_server, is_shared;

					nchans = decode_entry (&mapptr);
					is_server = decode_entry (&mapptr);
					is_shared = decode_entry (&mapptr);
#if 0
MESSAGE ("mpcb_mpp_serialise(): WSMAP_MOB_CT: nchans=%d, is_server=%d, is_shared=%d\n", nchans, is_server, is_shared);
#endif

					/* FIXME! dynamic mobile channel-type */
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_MOB_PT*/
			case WSMAP_MOB_PT:
				/* FIXME! dynamic mobile process */
				break;
				/*}}}*/
				/*{{{  default -- warning*/
			default:
				BMESSAGE ("warning: unknown workspace-map type %d in chain at %p.  Corrupt map ?\n", type, mc);
				break;
				/*}}}*/
			}
		}

		/* don't need to re-encode now -- save chain bits separately */
		word_roundup (&maplen);
		wsmbytes += maplen;
		wsmbytes += (2 * sizeof (unsigned int));		/* leading info */
#if 0
MESSAGE ("mpcb_mpp_serialise(): mapchain %d at %p offset %d, (%d entries, %d bytes (%d used))\n", wsmblks, mc, mc->wsoffset, (int)((mapptr[0] << 8) | mapptr[1]), (int)((mapptr[2] << 8) | mapptr[3]), maplen);
#endif
		wsmblks++;
	}
	swsoffs = ((wsmblks + 1) * sizeof (unsigned int));
	wsmbytes += swsoffs;
	bytes += wsmbytes;

	objspacebytes += ((objspaceents + 1) * sizeof (unsigned int));
	bytes += objspacebytes;

	/*}}}*/
	/*{{{  allocate "bytes" and populate header*/
	sblk = (unsigned int *)dmem_alloc (bytes);
	fhdr = (mp_filehdr *)sblk;
	memcpy (fhdr->hdr, "OCMP001\0", 8);
	fhdr->typehash = blk->typehash;
	fhdr->wsbytes = blk->wssize;
	fhdr->vsbytes = blk->vsbase ? blk->vssize : 0;
	fhdr->msbytes = blk->msbase ? blk->mssize : 0;
	fhdr->wsmapbytes = wsmbytes;
	fhdr->cmapentries = ncmapentries;
	fhdr->objbytes = objspacebytes;
	fhdr->iptr = 0;
	fhdr->wsptr = 0;
	fhdr->barrier = 0;
	#if 0
	fhdr->bbptr = 0;
	fhdr->becnt = blk->becnt;
	#endif
	fhdr->pname_offs = (unsigned int)(sizeof (mp_filehdr));
	fhdr->wsdata_offs = fhdr->pname_offs + nlen;
	fhdr->vsdata_offs = fhdr->wsdata_offs + fhdr->wsbytes;
	fhdr->msdata_offs = fhdr->vsdata_offs + fhdr->vsbytes;
	fhdr->wsmap_offs = fhdr->msdata_offs + fhdr->msbytes;
	fhdr->obj_offs = fhdr->wsmap_offs + fhdr->wsmapbytes;
	/*}}}*/

	/*}}}*/
	/*{{{  copy name, workspace and vectorspace, setup for data into objectspace*/
	memcpy ((byte *)fhdr + fhdr->pname_offs, cmaplabels[0], strlen (cmaplabels[0]) + 1);

	rws = blk->wsbase;
	sws = (unsigned int *)((byte *)fhdr + fhdr->wsdata_offs);
	memcpy (sws, blk->wsbase, fhdr->wsbytes);

	if (fhdr->vsbytes) {
		svs = (unsigned int *)((byte *)fhdr + fhdr->vsdata_offs);
		memcpy (svs, blk->vsbase, fhdr->vsbytes);
		rvs = blk->vsbase;
	}

	/* from here, objspacebytes is the offset from the start of objectspace */
	objspacebytes = ((objspaceents + 1) * sizeof (unsigned int));
	objspacebiptr = (unsigned int *)((byte *)fhdr + fhdr->obj_offs);		/* point at where we do [count, offs_0, offs_1, ...] */
	objspaceptr = (char *)((byte *)objspacebiptr + objspacebytes);			/* point at where objectspace data starts */
	*objspacebiptr = objspaceents;
	objspacebiptr++;
	objspaceents = 0;

	/*}}}*/
	/*{{{  fixup specials*/
	fhdr->wsptr = (unsigned int)((int)blk->wptr - (int)blk->wsbase);
	if (blk->iptr == NotProcess_p) {
		fhdr->iptr = (unsigned int)ENCODED_NOTPROCESS;
	} else {
		int i;

		for (i=0; i<ncmapentries; i++) {
			if (cmapentries[i] == blk->iptr) {
				fhdr->iptr = i;
				break;		/* for() */
			}
		}
		if (i == ncmapentries) {
			BMESSAGE ("error: iptr [%p] not found in code-map\n", blk->iptr);
		}
	}
	/* FIXME: handle blk->barrier */
	#if 0
	if (blk->bfptr == NotProcess_p) {
		/* nothing on the barrier queue */
		fhdr->bfptr = (unsigned int)ENCODED_NOTPROCESS;
		fhdr->bbptr = (unsigned int)ENCODED_NOTPROCESS;
	} else {
		fhdr->bfptr = (unsigned int)((int)blk->bfptr - (int)blk->wsbase);
		fhdr->bbptr = (unsigned int)((int)blk->bbptr - (int)blk->wsbase);
	}
	#endif
	/*}}}*/
	/*{{{  go through workspace map-chain and fixup workspace, also copy map-data into local (for output) and any dynamic stuffs into objectspace*/
	swsmap = (unsigned int *)((byte *)fhdr + fhdr->wsmap_offs);		/* points at space for [count, offset.0, offset.., offset.(n-1)] ++ [ws-offs, (raw map data)] */
	swsmap[0] = wsmblks;
	wsmblks = 0;

	for (mc = blk->mapchain; mc; mc = mc->next) {
		unsigned char *mapptr = mc->mapdata;
		int maplen = (int)((mapptr[2] << 8) | mapptr[3]);
		unsigned char *mapmax = mapptr + maplen + 4;
		void **relws = (void **)sws + mc->wsoffset;
		void **orgws = (void **)rws + mc->wsoffset;

#if 0
MESSAGE ("mpcb_mpp_serialise(): storing wsmap %d, swsoffs=%d\n", wsmblks, swsoffs);
#endif
		swsmap[wsmblks+1] = swsoffs;
		swsmap[(swsoffs >> 2)] = mc->wsoffset;
		/* copy map data */
		memcpy ((byte *)swsmap + swsoffs + sizeof (unsigned int), mapptr, maplen + sizeof (unsigned int));

		mapptr += 4;		/* point at data start */
		while (mapptr < mapmax) {
			int offset = decode_entry (&mapptr);
			int type = decode_entry (&mapptr);

			switch (type & WSMAP_TYPEMASK) {
				/*{{{  WSMAP_CHANWORD*/
			case WSMAP_CHANWORD:
				/* always going to be empty if we SUSPENDed */
				relws[offset] = ENCODED_NOTPROCESS;
				break;
				/*}}}*/
				/*{{{  WSMAP_CHANPTR, WSMAP_STATICLINK*/
			case WSMAP_CHANPTR:
			case WSMAP_STATICLINK:
				if (orgws[offset] == NotProcess_p) {
					/* flag as NotProcess */
					relws[offset] = (void *)ENCODED_NOTPROCESS;
				} else {
					/* remap channel-pointer or static-link */
					relws[offset] = (void *)((int)orgws[offset] - (int)rws);
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_GENPTR*/
			case WSMAP_GENPTR:
				if (((byte *)orgws[offset] >= (byte *)rws) && ((byte *)orgws[offset] < ((byte *)rws + fhdr->wsbytes))) {
					relws[offset] = (void *)((int)orgws[offset] - (int)rws);
				} else if (fhdr->vsbytes && ((byte *)orgws[offset] >= (byte *)rvs) && ((byte *)orgws[offset] < ((byte *)rvs + fhdr->vsbytes)) && (type & WSMAP_FLAG_VS)) {
					relws[offset] = (void *)((int)orgws[offset] - (int)rvs);
				} else if (orgws[offset] == NotProcess_p) {
					relws[offset] = (void *)ENCODED_NOTPROCESS;		/* flag as notprocess */
				} else {
					BMESSAGE ("warning: mobile process contains unknown general pointer [%p]\n", orgws[offset]);
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_MOB_DA*/
			case WSMAP_MOB_DA:
				{
					int nslots, basebytes, icount;
					int i;
					unsigned int *osuptr = (unsigned int *)objspaceptr;
					char *osdptr;

					nslots = decode_entry (&mapptr);
					basebytes = decode_entry (&mapptr);

					icount = basebytes;
					for (i=1; i<nslots; i++) {
						icount *= (int)orgws[offset + i];
					}

#if 0
MESSAGE ("mpcb_mpp_serialise(): WSMAP_MOB_DA: nslots=%d, basebytes=%d, icount=%d\n", nslots, basebytes, icount);
#endif
					if (icount) {
						osuptr[0] = nslots;
						osuptr[1] = basebytes;

						for (i=1; i<nslots; i++) {
							osuptr[i+1] = (int)orgws[offset + i];
						}

						osdptr = objspaceptr + ((nslots + 1) * sizeof (unsigned int));
						if (icount) {
							memcpy (osdptr, orgws[offset], icount);
						}

						icount += ((nslots + 1) * sizeof (unsigned int));

						/* round up if not word aligned */
						word_roundup (&icount);
						relws[offset] = (void *)objspaceents;

						*objspacebiptr = objspacebytes;		/* offset in objectspace */
						objspacebiptr++;
						objspacebytes += icount;
						objspaceptr += icount;
						objspaceents++;
					} else {
						relws[offset] = ENCODED_NOTPROCESS;
						for (i=1; i<nslots; i++) {
							relws[offset+i] = 0;		/* set any/all dimensions to zero */
						}
					}
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_MOB_CT*/
			case WSMAP_MOB_CT:
				{
					int nchans, is_server, is_shared;

					nchans = decode_entry (&mapptr);
					is_server = decode_entry (&mapptr);
					is_shared = decode_entry (&mapptr);

					/* FIXME! dynamic mobile channel-type */
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_MOB_PT*/
			case WSMAP_MOB_PT:
				/* FIXME! dynamic mobile process */
				break;
				/*}}}*/
				/*{{{  WSMAP_FIRSTPARAM*/
			case WSMAP_FIRSTPARAM:
				if (mc == blk->mapchain) {
					/* this is the top-level, so parameters not important anymore */
					mapptr = mapmax;
				}

				/* it should be the mobile-process pointer anyway (also captured by MPP case)*/
				if (orgws[offset] != (void *)blk) {
					BMESSAGE ("warning: first parameter [%p] not mobile-process pointer [%p]\n", orgws[offset], blk);
				} else {
					relws[offset] = ENCODED_MPP;
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_MPP*/
			case WSMAP_MPP:
				/* this is the mobile-process pointer, should be us */
				if (orgws[offset] == (void *)blk) {
					relws[offset] = ENCODED_MPP;
				} else {
					BMESSAGE ("warning: mobile process pointer [%p] not us! expected [%p]\n", orgws[offset], blk);
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_VSPTR*/
			case WSMAP_VSPTR:
				if (orgws[offset] == NotProcess_p) {
					relws[offset] = ENCODED_NOTPROCESS;
				} else {
					relws[offset] = (void *)((int)orgws[offset] - (int)rvs);
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_MSPTR*/
			case WSMAP_MSPTR:
				if (orgws[offset] == NotProcess_p) {
					relws[offset] = ENCODED_NOTPROCESS;
				} else {
					/* FIXME! mobilespace */
				}
				break;
				/*}}}*/
				/*{{{  WSMAP_CODEPTR*/
			case WSMAP_CODEPTR:
				if (orgws[offset] == NotProcess_p) {
					relws[offset] = ENCODED_NOTPROCESS;
				} else if (orgws[offset] == (void *)blk->aiptr) {
					relws[offset] = ENCODED_AIPTR;			/* activator return address (not really important) */
				} else {
					int i;

					for (i=0; i<ncmapentries; i++) {
						if (cmapentries[i] == orgws[offset]) {
							relws[offset] = (void *)i;
							break;		/* for() */
						}
					}
					if (i == ncmapentries) {
						BMESSAGE ("error: code-pointer [%p] not found in code-map\n", orgws[offset]);
						relws[offset] = ENCODED_NOTPROCESS;
					}
				}
				break;
				/*}}}*/
				/*{{{  default -- warning*/
			default:
				BMESSAGE ("warning: unknown workspace-map type %d in chain at %p.  Corrupt map ?\n", type, mc);
				break;
				/*}}}*/
			}
		}

		/* updates offsets for local storage */
		word_roundup (&maplen);
		swsoffs += (maplen + (2 * sizeof (unsigned int)));
		wsmblks++;
	}
	/*}}}*/

#if 0
mpcb_dump_process (*mpp);
dump_workspace (stderr, (void **)sws, blk->wssize, blk->mapchain);
#endif

	if (*rsize) {
		/* free existing process here */
		dmem_release ((void *)(*raddr));
	}
	*raddr = (int)fhdr;
	*rsize = bytes;

	return 1;
}
/*}}}*/
/*{{{  int mpcb_mpp_deserialise (int addr, int size, mp_ctrlblk **mpp, unsigned int *thashp)*/
/*
 *	de-serialises a mobile process
 *	returns 0 on failure, non-zero on success
 */
int mpcb_mpp_deserialise (int addr, int size, mp_ctrlblk **mpp, unsigned int *thashp)
{
#if !defined(DYNAMIC_PROCS) || defined(RMOX_BUILD)
	BMESSAGE ("no dynamic process support, cannot de-serialise process.\n");
	return 0;
#else	/* DYNAMIC_PROCS && !RMOX_BUILD*/
	mp_ctrlblk *blk = *mpp;
	mp_filehdr *fhdr = NULL;
	unsigned int *sws = NULL;
	unsigned int *rws = NULL;
	unsigned int *rvs = NULL;
	char *pname;
	void *dlhandle, *ep_addr, *map_addr;
	int nchains, i;
	mp_mapchain **chainp;

	void **codemap = NULL;
	int ncmapentries = 0;
	int mcmapentries = 0;
	void **cmapentries = NULL;
	char **cmaplabels = NULL;

	unsigned int *objspacebiptr = NULL;
	char *objspaceptr = NULL;
	int objspaceents = 0;
	int objspaceidx = 0;

	if (!addr) {
		BMESSAGE ("null mobile process!\n");
		return 0;
	}
	if (!blk) {
		/*{{{  allocate a new block*/
		blk = (mp_ctrlblk *)dmem_alloc (sizeof (mp_ctrlblk));
		*mpp = blk;

		blk->wptr = (void *)NotProcess_p;
		blk->iptr = (void *)NotProcess_p;
		blk->aiptr = (void *)NotProcess_p;
		blk->mapchain = NULL;

		blk->wsbase = NULL;
		blk->wssize = 0;
		blk->vsbase = NULL;
		blk->msbase = NULL;

		blk->barrier = NULL;
		if (!thashp) {
			blk->typehash = 0;
		} else {
			blk->typehash = *thashp;
		}
		blk->codemap = NULL;

		blk->vssize = 0;
		blk->mshook = NULL;
		blk->mssize = 0;
		blk->msdesc = NULL;
		/*}}}*/
	}

	/*{{{  check file header*/
	if (size < sizeof (mp_filehdr)) {
		BMESSAGE ("damaged mobile process (too small)\n");
		return 0;
	}

	fhdr = (mp_filehdr *)addr;

	if (memcmp (&(fhdr->hdr[0]), "OCMP", 4)) {
		goto out_bad_header;
	}
	if (!thashp) {
		blk->typehash = fhdr->typehash;
	} else if (fhdr->typehash != *thashp) {
		BMESSAGE ("expected hash 0x%8.8x, got 0x%8.8x\n", fhdr->typehash, *thashp);
		goto out_bad_header;
	}
	if ((fhdr->pname_offs > size) || (fhdr->wsdata_offs > size) || ((fhdr->wsdata_offs + fhdr->wsbytes) > size) ||
			(fhdr->vsdata_offs > size) || ((fhdr->vsdata_offs + fhdr->vsbytes) > size) ||
			(fhdr->msdata_offs > size) || ((fhdr->msdata_offs + fhdr->msbytes) > size) ||
			(fhdr->wsmap_offs > size) || ((fhdr->wsmap_offs + fhdr->wsmapbytes) > size) ||
			(fhdr->obj_offs > size) || ((fhdr->obj_offs + fhdr->objbytes) > size)) {
		BMESSAGE ("damaged mobile process\n");
		return 0;
	}

	/*}}}*/
	/*{{{  lookup entry-point and workspace map for this process*/
	pname = (char *)((byte *)fhdr + fhdr->pname_offs);
	pname = make_c_name (pname, strlen (pname));
#if 0
MESSAGE ("mpcb_mpp_deserialise(): process name [%s]\n", pname);
#endif

	dlhandle = dlopen (NULL, RTLD_LAZY);
	ep_addr = dlsym (dlhandle, pname);
	*pname = 'M';		/* map next */
	map_addr = dlsym (dlhandle, pname);
	dlclose (dlhandle);

	if (!ep_addr || !map_addr) {
		BMESSAGE ("no code for \"%s\"\n", (char *)((byte *)fhdr + fhdr->pname_offs));
		return 0;
	}

	blk->codemap = (char *)map_addr;

	/*}}}*/
	/*{{{  get codemap (cached, or build fresh)*/
	codemap = (void **)blk->codemap;
	ncmapentries = mcache_check (codemap[0], (char *)codemap[2], &cmapentries, &cmaplabels, &mcmapentries);
	if (!ncmapentries) {
		make_codemap (codemap, &cmapentries, &cmaplabels, &ncmapentries, &mcmapentries);
		mcache_add (cmapentries, cmaplabels, ncmapentries);
	}

	if (ncmapentries != fhdr->cmapentries) {
		BMESSAGE ("error: codemap size mismatch (got %d, expected %d)\n", fhdr->cmapentries, ncmapentries);
		return 0;
	}
	/*}}}*/
	/*{{{  rebuild workspace and vectorspace*/
	blk->wssize = fhdr->wsbytes;
	blk->wsbase = dmem_alloc (blk->wssize);
	
	memcpy (blk->wsbase, (unsigned char *)fhdr + fhdr->wsdata_offs, blk->wssize);
	sws = (unsigned int *)((byte *)fhdr + fhdr->wsdata_offs);
	rws = (unsigned int *)blk->wsbase;

	if (fhdr->vsbytes) {
		blk->vssize = fhdr->vsbytes;
		blk->vsbase = dmem_alloc (blk->vssize);

		memcpy (blk->vsbase, (unsigned char *)fhdr + fhdr->vsdata_offs, blk->vssize);
		rvs = (unsigned int *)blk->vsbase;
	} else {
		rvs = NULL;
	}

	objspacebiptr = (unsigned int *)((byte *)fhdr + fhdr->obj_offs);
	objspaceents = *objspacebiptr;
	objspacebiptr++;
	objspaceptr = (char *)fhdr + fhdr->obj_offs;
	objspaceidx = 0;


	/*}}}*/
	/* FIXME: rebuild mobilespace */
	/*{{{  rebuild workspace-map chain and process*/
	nchains = *(unsigned int *)((byte *)fhdr + fhdr->wsmap_offs);
	chainp = &blk->mapchain;
	for (i=0; i<nchains; i++) {
		unsigned int chainoffs = ((unsigned int *)((byte *)fhdr + fhdr->wsmap_offs))[i+1];
		unsigned char *chaindata;
		int chainlen, wsoffs;

		if ((chainoffs + fhdr->wsmap_offs + (2 * sizeof (int))) > size) {
			BMESSAGE ("damaged mobile process\n");
			return 0;
		}
		chaindata = (unsigned char *)((byte *)fhdr + fhdr->wsmap_offs + chainoffs);
		wsoffs = *(int *)chaindata;
		chaindata += sizeof (int);		/* skip ws-offset */
		chainlen = (int)((chaindata[2] << 8) | chaindata[3]);

		*chainp = (mp_mapchain *)dmem_alloc (sizeof (mp_mapchain) + chainlen + 4);		/* hide the chain-data here */
		(*chainp)->next = NULL;
		(*chainp)->mapdata = ((unsigned char *)*chainp) + sizeof (mp_mapchain);
		(*chainp)->wsoffset = wsoffs;
		(*chainp)->mapsize = (int)((chaindata[0] << 8) | chaindata[1]);
		memcpy ((*chainp)->mapdata, chaindata, chainlen + 4);
#if 0
MESSAGE ("mpcb_mpp_deserialise(): re-created chain %d, data at %p offset at %d (%d entries, %d bytes)\n", i, (*chainp)->mapdata, wsoffs, (*chainp)->mapsize, chainlen);
#endif
		/*{{{  re-map workspace proper*/
		{
			unsigned char *mapptr = (*chainp)->mapdata;
			int maplen = (int)((mapptr[2] << 8) | mapptr[3]);
			unsigned char *mapmax = mapptr + maplen + 4;
			void **relws = (void **)sws + (*chainp)->wsoffset;
			void **orgws = (void **)rws + (*chainp)->wsoffset;

#if 0
fprintf (stderr, "deserialising from workspace map at %p: %d bytes, %d entries\n", mapptr, maplen, (int)((mapptr[0] << 8) | mapptr[1]));
#endif

			mapptr += 4;		/* point at data start */
			while (mapptr < mapmax) {
				int offset = decode_entry (&mapptr);
				int type = decode_entry (&mapptr);

				switch (type & WSMAP_TYPEMASK) {
					/*{{{  WSMAP_CHANWORD*/
				case WSMAP_CHANWORD:
					if (relws[offset] != ENCODED_NOTPROCESS) {
						BMESSAGE ("warning: CHANWORD not encoded NotProcess\n");
					}
					orgws[offset] = NotProcess_p;
					break;
					/*}}}*/
					/*{{{  WSMAP_CHANPTR, WSMAP_STATICLINK*/
				case WSMAP_CHANPTR:
				case WSMAP_STATICLINK:
					if (relws[offset] == ENCODED_NOTPROCESS) {
						orgws[offset] = NotProcess_p;
					} else {
						/* remap channel-pointer or static-link */
						if ((int)relws[offset] > blk->wssize) {
							BMESSAGE ("error: CHANPTR/STATICLINK workspace offset %d out of range\n", (int)relws[offset]);
							orgws[offset] = NotProcess_p;
						} else {
							orgws[offset] = (void *)((int)(relws[offset]) + (int)rws);
						}
					}
					break;
					/*}}}*/
					/*{{{  WSMAP_GENPTR*/
				case WSMAP_GENPTR:
					if (relws[offset] == ENCODED_NOTPROCESS) {
						orgws[offset] = NotProcess_p;
					} else if (type & WSMAP_FLAG_VS) {
						/* vectorspace */
						if (!blk->vsbase || !blk->vssize) {
							BMESSAGE ("error: GENPTR in non-existant vectorspace\n");
							orgws[offset] = NotProcess_p;
						} else if (blk->vsbase && ((int)relws[offset] > blk->vssize)) {
							BMESSAGE ("error: GENPTR vectorspace offset %d out of range\n", (int)relws[offset]);
							orgws[offset] = NotProcess_p;
						} else {
							orgws[offset] = (void *)((int)relws[offset] + (int)rvs);
						}
					} else {
						/* workspace */
						if ((int)relws[offset] > blk->wssize) {
							BMESSAGE ("error: GENPTR workspace offset %d out of range\n", (int)relws[offset]);
							orgws[offset] = NotProcess_p;
						} else {
							orgws[offset] = (void *)((int)relws[offset] + (int)rws);
						}
					}
					break;
					/*}}}*/
					/*{{{  WSMAP_MOB_DA*/
				case WSMAP_MOB_DA:
					{
						int nslots, basebytes;
						int i;

						nslots = decode_entry (&mapptr);
						basebytes = decode_entry (&mapptr);

						if (relws[offset] == ENCODED_NOTPROCESS) {
							orgws[offset] = NotProcess_p;
							for (i=1; i<nslots; i++) {
								relws[offset+i] = (void *)0;
							}
						} else if ((int)relws[offset] >= objspaceents) {
							BMESSAGE ("error: object-space entity %d out of range (%d entries)\n", (int)relws[offset], objspaceents);
						} else {
							int objoffs = objspacebiptr[(int)relws[offset]];
							unsigned int *objuptr = (unsigned int *)(objspaceptr + objoffs);
							char *objptr = (char *)(objuptr + (nslots + 1));
							int icount;

							/* at objuptr, got [nslots,basebytes,dim1..dimN] */
							icount = basebytes;
							for (i=1; i<nslots; i++) {
								icount *= objuptr[i+1];
								orgws[offset + i] = (void *)(objuptr[i+1]);
							}
							orgws[offset] = dmem_alloc (icount);
							memcpy (orgws[offset], objptr, icount);		/* copy data back in */
#if 0
MESSAGE ("re-serialised dynamic mobile array, pointer at %p for %d bytes (nslots=%d, first dimension=%d)\n", orgws[offset], icount, nslots, (int)orgws[offset+1]);
#endif
						}
					}
					break;
					/*}}}*/
					/*{{{  WSMAP_MOB_CT*/
				case WSMAP_MOB_CT:
					{
						int nchans, is_server, is_shared;

						nchans = decode_entry (&mapptr);
						is_server = decode_entry (&mapptr);
						is_shared = decode_entry (&mapptr);

						/* FIXME! dynamic mobile channel-type */
					}
					break;
					/*}}}*/
					/*{{{  WSMAP_MOB_PT*/
				case WSMAP_MOB_PT:
					/* FIXME! dynamic mobile process */
					break;
					/*}}}*/
					/*{{{  WSMAP_FIRSTPARAM*/
				case WSMAP_FIRSTPARAM:
					if ((*chainp) == blk->mapchain) {
						/* this is the top-level, so parameters not important anymore */
						mapptr = mapmax;
					}
					if (relws[offset] != ENCODED_MPP) {
						BMESSAGE ("warning: first parameter [%p] not encoded MPP\n", relws[offset]);
					} else {
						orgws[offset] = (void *)blk;
					}
					break;
					/*}}}*/
					/*{{{  WSMAP_MPP*/
				case WSMAP_MPP:
					if (relws[offset] != ENCODED_MPP) {
						BMESSAGE ("warning: mobile process pointer [%p] not encoded MPP\n", relws[offset]);
					} else {
						orgws[offset] = (void *)blk;
					}
					break;
					/*}}}*/
					/*{{{  WSMAP_VSPTR*/
				case WSMAP_VSPTR:
					if (relws[offset] == ENCODED_NOTPROCESS) {
						orgws[offset] = NotProcess_p;
					} else if (!blk->vsbase || !blk->vssize) {
						BMESSAGE ("error: VSPTR in non-existant vectorspace\n");
						orgws[offset] = NotProcess_p;
					} else if (blk->vsbase && ((int)relws[offset] > blk->vssize)) {
						BMESSAGE ("error: VSPTR vectorspace offset %d out of range\n", (int)relws[offset]);
						orgws[offset] = NotProcess_p;
					} else {
						orgws[offset] = (void *)((int)relws[offset] + (int)rvs);
					}
					break;
					/*}}}*/
					/*{{{  WSMAP_MSPTR*/
				case WSMAP_MSPTR:
					if (relws[offset] == ENCODED_NOTPROCESS) {
						orgws[offset] = NotProcess_p;
					} else {
						/* FIXME! mobilespace */
					}
					break;
					/*}}}*/
					/*{{{  WSMAP_CODEPTR*/
				case WSMAP_CODEPTR:
					if (relws[offset] == ENCODED_NOTPROCESS) {
						orgws[offset] = NotProcess_p;
					} else if (relws[offset] == ENCODED_AIPTR) {
						orgws[offset] = NULL;				/* activator return address (not relevant) */
					} else {
						int cinum = (int)relws[offset];

						if ((cinum < 0) || (cinum >= ncmapentries)) {
							BMESSAGE ("error: code-pointer-index %d not in code-map\n", cinum);
							orgws[offset] = NotProcess_p;
						} else {
							orgws[offset] = cmapentries[cinum];
						}
					}
					break;
					/*}}}*/
					/*{{{  default -- warning*/
				default:
					BMESSAGE ("warning: unknown workspace-map type %d in chain at %p.  Corrupt map ?\n", type, (*chainp));
					break;
					/*}}}*/
				}
			}
		}
		/*}}}*/
		chainp = &((*chainp)->next);
	}
	/*}}}*/
	/*{{{  fixup specials*/
	blk->wptr = (byte *)blk->wsbase + fhdr->wsptr;
	if (fhdr->iptr == (unsigned int)ENCODED_NOTPROCESS) {
		blk->iptr = NotProcess_p;
	} else {
		int cinum = (int)fhdr->iptr;

		if ((cinum < 0) || (cinum >= ncmapentries)) {
			BMESSAGE ("error: mobile-process Iptr index %d not in code-map\n", cinum);
		} else {
			blk->iptr = cmapentries[cinum];
		}
	}
	/* FIXME: fhdr->barrier */
	#if 0
	if (fhdr->bfptr == (unsigned int)ENCODED_NOTPROCESS) {
		blk->bfptr = NotProcess_p;
		blk->bbptr = NotProcess_p;
	} else {
		blk->bfptr = (void *)blk->wsbase + fhdr->bfptr;
		blk->bbptr = (void *)blk->wsbase + fhdr->bbptr;
	}
	blk->becnt = fhdr->becnt;
	#endif
	/*}}}*/
#if 0
MESSAGE ("mpcb_mpp_deserialise (mpp = %p, *mpp = %p, addr = %p, size = %p)\n", mpp, *mpp, (void *)addr, (void *)size);
#endif
#if 0
mpcb_dump_process (*mpp);
dump_workspace (stderr, (void **)(blk->wsbase), blk->wssize, blk->mapchain);
#endif
	return 1;
out_bad_header:
	BMESSAGE ("error: cannot de-serialise mobile process, bad header\n");
	return 0;
#endif	/* DYNAMIC_PROCS */
}
/*}}}*/
#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
/*{{{  static void mpcb_mpp_checkroutine (const char *saddr, int slen, int *result)*/
/*
 *	checks whether a particular routine is available.
 *	returns a BOOL value in "result"
 */
static void mpcb_mpp_checkroutine (const char *saddr, int slen, int *result)
{
	char *iname;
	void *dlhandle = dlopen (NULL, RTLD_LAZY);
	void *ep_addr, *map_addr;

	iname = make_c_name ((char *)saddr, slen);
	ep_addr = dlsym (dlhandle, iname);
	*iname = 'M';
	map_addr = dlsym (dlhandle, iname);
	dlclose (dlhandle);

	if (ep_addr && map_addr) {
		*result = 1;
	} else {
		rtlibrary_t *walk;

		*result = 0;
		for (walk=loadedlibs; walk && !*result; walk=walk->next) {
			*iname = 'O';
			ep_addr = dlsym (walk->dlhandle, iname);
			*iname = 'M';
			map_addr = dlsym (walk->dlhandle, iname);

			*result = !!(ep_addr && map_addr);
		}
	}
	return;
}
/*}}}*/
/*{{{  static void mpcb_mpp_loadlibrary (const char *lname, int llen, int *result)*/
/*
 *	loads a library into the run-time system
 */
static void mpcb_mpp_loadlibrary (const char *lname, int llen, int *result)
{
	rtlibrary_t *lib = NULL;

	for (lib=loadedlibs; lib; lib=lib->next) {
		if ((lib->llen == llen) && !strncmp (lib->lname, lname, llen)) {
			/* already loaded */
			*result = 1;
			lib->rcount++;
			return;
		}
	}
	/* create fresh */
	lib = (rtlibrary_t *)dmem_alloc (sizeof (rtlibrary_t) + llen + 8);
	lib->next = loadedlibs;
	lib->lname = ((char *)lib) + sizeof (rtlibrary_t);
	memcpy (lib->lname, lname, llen);
	lib->lname[llen] = '\0';
	lib->llen = llen;
	lib->rcount = 1;

	/* FIXME: this should look at what the name is, before deciding how to load it.. */
	lib->dlhandle = dlopen (lib->lname, RTLD_NOW | RTLD_GLOBAL);
	if (!lib->dlhandle) {
		/* failed to open */
		*result = 0;
		dmem_release (lib);
		return;
	}

	loadedlibs = lib;
	return;
}
/*}}}*/
/*{{{  static void mpcb_mpp_unloadlibrary (const char *lname, int llen, int *result)*/
/*
 *	unloads a library from the run-time system
 */
static void mpcb_mpp_unloadlibrary (const char *lname, int llen, int *result)
{
	return;
}
/*}}}*/
#endif	/* defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)*/
#if !defined(RMOX_BUILD)
/*{{{  static void dump_codemap (FILE *stream, void **codemap, int indent)*/
/*
 *	dumps the codemap for a process (debugging)
 */
static void dump_codemap (FILE *stream, void **codemap, int indent)
{
	int subs, i, j;

	subs = (int)(codemap[3]);
	for (j=0; j<indent; MESSAGETO (stream, "    "), j++);
	MESSAGETO (stream, "CMAP:    \"%s\" at %p, %d bytes (%d subs)\n", (char *)(codemap[2]), codemap[0], (int)(codemap[1]), subs);
	for (i=0; i<subs; i++) {
		void **nextmap = (void **)(codemap[(i * 4) + 5]);

		for (j=0; j<indent; MESSAGETO (stream, "    "), j++);
		MESSAGETO (stream, "CMAPSUB: \"%s\" at %p", (char *)(codemap[(i * 4) + 6]), (void *)(codemap[(i*4) + 4]));
		if ((int)nextmap != -1) {
			MESSAGETO (stream, ", nextmap at %p:\n", nextmap);
			dump_codemap (stream, nextmap, indent + 1);
		} else{
			MESSAGETO (stream, "\n");
		}
	}
	return;
}
/*}}}*/
#endif
/*{{{  void mpcb_dump_process (mp_ctrlblk *mp)*/
/*
 *	dumps (on stderr) the workspace of a process, highlighted with
 *	map information.
 */
void mpcb_dump_process (mp_ctrlblk *mp)
{
#if defined(RMOX_BUILD)
	MESSAGE ("no dump mobile process block at %p\n", mp);
#else
	MESSAGE ("mobile process block at %p:\n", mp);
	MESSAGE ("    wptr=%p, iptr=%p, aiptr=%p, mapchain=%p\n", mp->wptr, mp->iptr, mp->aiptr, mp->mapchain);
	MESSAGE ("    wsbase=%p, wssize=%d, vsbase=%p, msbase=%p\n", mp->wsbase, mp->wssize, mp->vsbase, mp->msbase);
	MESSAGE ("    barrier=%p\n", mp->barrier);
	MESSAGE ("    typehash=0x%8.8x, codemap=%p\n", mp->typehash, mp->codemap);
	dump_workspace (stderr, (void **)(mp->wsbase), mp->wssize, mp->mapchain);
	if (mp->codemap) {
		dump_codemap (stderr, (void **)(mp->codemap), 0);
	}
#endif
	return;
}
/*}}}*/

/*{{{  int mpp_checkroutine (char *name)*/
/*
 *	checks whether a given routine exists (checks entry-point and code-map)
 *	returns 0 if not available, non-zero otherwise
 */
int mpp_checkroutine (char *name)
{
	int r = 0;

#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	mpcb_mpp_checkroutine (name, strlen (name), &r);
#endif
	return r;
}
/*}}}*/
/*{{{  int mpp_loadlibrary (char *lname)*/
/*
 *	loads a library.  returns 0 on failure, non-zero on success
 */
int mpp_loadlibrary (char *lname)
{
	int r = 0;

#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	mpcb_mpp_loadlibrary (lname, strlen (lname), &r);
#endif
	return r;
}
/*}}}*/
/*{{{  int mpp_unloadlibrary (char *lname)*/
/*
 *	unloads a library.  returns 0 on failure, non-zero on success
 */
int mpp_unloadlibrary (char *lname)
{
	int r = 0;

#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	mpcb_mpp_unloadlibrary (lname, strlen (lname), &r);
#endif
	return r;
}
/*}}}*/


/*{{{  occam interfaces -- dropped in the occam8 library for MPP.SERIALISE, MPP.DESERIALISE, MPP.CHECKROUTINE, MPP.LOADLIBRARY and MPP.UNLOADLIBRARY*/
void _do_mpp_serialise (int *ws)
{
	mpcb_mpp_serialise ((mp_ctrlblk **)(ws + 0), (unsigned int *)(ws + 1), (int *)(ws + 2), (int *)(ws + 3));
}

void _do_mpp_deserialise (int *ws)
{
	mpcb_mpp_deserialise ((int)(ws[0]), (int)(ws[1]), (mp_ctrlblk **)(ws + 2), (unsigned int *)(ws + 3));
}

void _do_mpp_checkroutine (int *ws)
{
#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	mpcb_mpp_checkroutine ((char *)(ws[0]), (int)(ws[1]), (int *)(ws[2]));
#else
	*(int *)(ws[2]) = 0;
#endif
}

void _do_mpp_loadlibrary (int *ws)
{
#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	mpcb_mpp_loadlibrary ((char *)(ws[0]), (int)(ws[1]), (int *)(ws[2]));
#else
	*(int *)(ws[2]) = 0;
#endif
}

void _do_mpp_unloadlibrary (int *ws)
{
#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	mpcb_mpp_unloadlibrary ((char *)(ws[0]), (int)(ws[1]), (int *)(ws[2]));
#else
	*(int *)(ws[2]) = 0;
#endif
}

/*}}}*/

