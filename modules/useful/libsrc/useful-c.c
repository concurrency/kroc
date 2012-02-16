/*
	C support code for the useful module
	Copyright (C) 2008 Adam Sampson <ats@offog.org>

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

void _format_print_stderr (int w[])
{
	const char *s = (const char *) w[0];
	const int s_len = w[1];

	fwrite (s, 1, s_len, stderr);
}
