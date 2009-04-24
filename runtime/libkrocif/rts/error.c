/*
 *	KRoC error hooks
 *	Copyright (C) 2009 Carl Ritson
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
	#include <config.h>
#endif

#include <rts.h>

void etc_error_bounds (void *sched, word *Wptr, char *file, int line)
{
	MESSAGE ("Bounds error at %s:%d (Wptr = %p)\n", file, line, Wptr);
	user_bad_exit ();
}

void etc_error_overflow (void *sched, word *Wptr, char *file, int line)
{
	MESSAGE ("Arithmetic overflow at %s:%d (Wptr = %p)\n", file, line, Wptr);
	user_bad_exit ();
}

void etc_error_div (void *sched, word *Wptr, char *file, int line)
{
	MESSAGE ("Divide error at %s:%d (Wptr = %p)\n", file, line, Wptr);
	user_bad_exit ();
}

void etc_error_set (void *sched, word *Wptr, char *file, int line)
{
	MESSAGE ("Runtime error at %s:%d (Wptr = %p)\n", file, line, Wptr);
	user_bad_exit ();
}

void etc_error_null (void *sched, word *Wptr, char *file, int line)
{
	MESSAGE ("Null pointer check error at %s:%d (Wptr = %p)\n", file, line, Wptr);
	user_bad_exit ();
}

