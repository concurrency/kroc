/*
	occSDLhelpers-c.c: helper code for occSDL wrapper
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

#include <stdio.h>
#include "SDL_wrap.h"

/* C.occ.SDL.make.surface (VAL [][]INT pixels, RESULT SDL.Surface surface) */
DLLPREFIX void _occ_SDL_make_surface (int w[])
{
	int *pixels = (int *) w[0];
	int height = w[1], width = w[2];
	SDL_Surface **surface = (SDL_Surface **) w[3];

	*surface = SDL_CreateRGBSurfaceFrom (pixels, width, height, 32, width * 4,
	                                     0x00FF0000, 0x0000FF00, 0x000000FF, 0);
}

