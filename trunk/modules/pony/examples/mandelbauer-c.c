// Copyright: Adam Sampson (C) 2006
// Institution: Computing Laboratory, University of Kent, Canterbury, UK
// Description: C code to calculate points in the Mandelbrot set

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

#include <stdio.h>

void calculate_mandel (double x_init, double y_init, double x_stride, double y_stride, int width, int height, int max_count, int *data)
{
	int yi, xi;
	for (yi = 0; yi < height; yi++) {
		double y = y_init + yi * y_stride;
		for (xi = 0; xi < width; xi++) {
			double x = x_init + xi * x_stride;
			double zr = x, zi = y;
			int count = 0;
			while (1) {
				double nzr = zr * zr - zi * zi;
				zi = 2.0 * zr * zi;
				zr = nzr;
				zr += x;
				zi += y;
				if (zr * zr + zi * zi > 4.0) {
					*data++ = count;
					break;
				} else if (count == max_count) {
					*data++ = -1;
					break;
				}
				count++;
			}
		}
	}
}

void _calculate_mandel (int *ws) {
	calculate_mandel (*(double *) (ws[0]), *(double *) (ws[1]), *(double *) (ws[2]), *(double *) (ws[3]),
	                  ws[4], ws[5], ws[6], (int *) ws[7]);
}

