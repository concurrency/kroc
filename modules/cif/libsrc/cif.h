/*
	cif.h: C support for C interface to the scheduler
	Copyright (C) 2007  University of Kent

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation, either
	version 2 of the License, or (at your option) any later version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library.  If not, see
	<http://www.gnu.org/licenses/>.
 */

#ifndef CIF_H
#define CIF_H

#include <stdarg.h>

/*{{{ pull in CIF basic operations */
/* In the future we might have multiple implementations of CIF. For now, we
 * just include the CCSP CIF header for the basic operations.
 */

#include "ccsp_cif.h"
/*}}}*/

/*{{{ CIF convenience functions */
/*{{{ void ChanInInt (Workspace wptr, Channel *c, int *i) */
static inline void ChanInInt (Workspace wptr, Channel *c, int *i)
{
	ChanInWord (wptr, c, (word *) i);
}
/*}}}*/
/*{{{ void ChanOutInt (Workspace wptr, Channel *c, int i) */
static inline void ChanOutInt (Workspace wptr, Channel *c, int i)
{
	ChanOutWord (wptr, c, (word) i);
}
/*}}}*/
/*{{{ void ChanInChar (Workspace wptr, Channel *c, char *b) */
static inline void ChanInChar (Workspace wptr, Channel *c, char *b)
{
	ChanInByte (wptr, c, (byte *) b);
}
/*}}}*/
/*{{{ void ChanOutChar (Workspace wptr, Channel *c, char b) */
static inline void ChanOutChar (Workspace wptr, Channel *c, char b)
{
	ChanOutByte (wptr, c, (byte) b);
}
/*}}}*/
/*{{{ void TimerDelay (Workspace wptr, Time delay) */
static inline void TimerDelay (Workspace wptr, Time delay)
{
	Time t = TimerRead (wptr);
	TimerWait (wptr, Time_PLUS (t, delay));
}
/*}}}*/
/*{{{ void ProcPar (Workspace wptr, int numprocs, ...) */
static inline void ProcPar (Workspace wptr, int numprocs, ...)
{
	LightProcBarrier bar;
	va_list ap;

	LightProcBarrierInit (wptr, &bar, numprocs);

	va_start (ap, numprocs);
	while (numprocs-- > 0) {
		Workspace ws = va_arg (ap, Workspace);
		Process func = va_arg (ap, Process);

		LightProcStart (wptr, &bar, ws, func);
	}
	va_end (ap);

	LightProcBarrierWait (wptr, &bar);
}
/*}}}*/
/*{{{ int ProcAlt (Workspace wptr, ...) */
static inline int ProcAlt (Workspace wptr, ...)
{
	int i, fired = -1;
	va_list ap;

	Alt (wptr);

	/*{{{ enable */
	va_start (ap, wptr);
	for (i = 0; ; i++) {
		Channel *c = va_arg (ap, Channel *);
		if (c == NULL)
			break;

		if (AltEnableChannel (wptr, i, c)) {
			fired = i;
			break;
		}
	}
	va_end (ap);
	/*}}}*/

	if (fired == -1) {
		AltWait (wptr);
	}

	/*{{{ disable */
	va_start (ap, wptr);
	for (i = 0; ; i++) {
		Channel *c = va_arg (ap, Channel *);
		if (c == NULL)
			break;

		AltDisableChannel (wptr, i, c);
		
		if (i == fired)
			break;
	}
	va_end (ap);
	/*}}}*/

	return AltEnd (wptr);
}
/*}}}*/
/*{{{ mt_array_t *MTAllocArray (Workspace wptr, word element_type, int dimensions, ...) */
static inline mt_array_t *MTAllocArray (Workspace wptr, word element_type, int dimensions, ...)
{
	va_list ap;
	int size = 1;
	mt_array_t *array;
	int i;

	va_start (ap, dimensions);
	for (i = 0; i < dimensions; i++)
		size *= va_arg (ap, int);
	va_end (ap);

	array = MTAlloc (wptr, MT_MAKE_ARRAY_TYPE (dimensions, element_type), size);

	va_start (ap, dimensions);
	for (i = 0; i < dimensions; i++)
		array->dimensions[i] = va_arg (ap, int);
	va_end (ap);

	return array;
}
/*}}}*/
/*{{{ mt_cb_t *MTAllocChanType (Workspace wptr, int channels, bool shared) */
static inline mt_cb_t *MTAllocChanType (Workspace wptr, int channels, bool shared)
{
	word type = MT_SIMPLE | MT_MAKE_TYPE (MT_CB);

	if (shared)
		type |= MT_CB_SHARED;

	return MTAlloc (wptr, type, channels);
}
/*}}}*/
/*{{{ mt_array_t *MTResize1D (Workspace wptr, mt_array_t *array, int new_size) */
static inline mt_array_t *MTResize1D (Workspace wptr, mt_array_t *array, int new_size)
{
	return (mt_array_t *) MTResize (wptr, MT_RESIZE_DATA, array, new_size);
}
/*}}}*/
/*}}}*/

#endif
