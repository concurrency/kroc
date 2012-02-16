/*
 *	KRoC interface to SP library
 *	Copyright (C) 1996 Michael Poole
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

/*
 * $Source: /proj/ofa/inmoslibs/hostio/RCS/spinterf.c,v $
 *
 * $Id: spinterf.c,v 1.5 1997/10/23 17:11:08 djb1 Exp $
 *
 * (C) Copyright 1996 M. D. Poole <M.D.Poole@ukc.ac.uk>
 * University of Kent at Canterbury
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <spunixhdr.h>

typedef int word;

void _fopen (w)
word w[3];
{       C_fopen ((int *)w[0], w[1], w[2]);
}

void _fflush (w)
word w[2];
{       C_fflush ((int *)w[0], w[1]);
}

void _fclose (w)
word w[2];
{       C_fclose ((int *)w[0], w[1]);
}

void _fread (w)
word w[5];
{       C_fread ((int *)w[0], w[1], w[2], w[3], (int *)w[4]);
}

void _fgets (w)
word w[5];
{       C_fgets ((int *)w[0], w[1], w[2], w[3], (int *)w[4]);
}

void _fwrite (w)
word w[5];
{       C_fwrite ((int *)w[0], w[1], w[2], w[3], (int *)w[4]);
}

void _fremove (w)
word w[2];
{       C_fremove ((int *)w[0], w[1]);
}

void _frename (w)
word w[3];
{       C_frename ((int *)w[0], w[1], w[2]);
}

void _fseek (w)
word w[4];
{       C_fseek ((int *)w[0], w[1], w[2], w[3]);
}

void _ftell (w)
word w[3];
{       C_ftell ((int *)w[0], w[1], (int *)w[2]);
}

void _comdline (w)
word w[5];
{       C_comdline ((int *)w[0], w[1], (int *)w[2], w[3], w[4]);
}

void _getenvval (w)
word w[5];
{       C_getenv ((int *)w[0], w[1], (int *)w[2], w[3], w[4]);
}

void _timenow (w)
word w[2];
{       C_time ((int *)w[0], (int *)w[1]);
}

void _system (w)
word w[3];
{       C_system ((int *)w[0], (int *)w[1], w[2]);
}

void _exitoccam (w)      /* MUST not be _exit as there is a Unix function with this name */
word w[2];
{       C_exit ((int *)w[0], w[1]);
}

