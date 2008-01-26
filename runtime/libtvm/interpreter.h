/*
tvm - interpreter.h
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef INTERPRETER_H
#define INTERPRETER_H

#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#ifdef HAVE_SETJMP_H
#	include <setjmp.h>
#endif

#ifdef HAVE_SETJMP_H
extern jmp_buf runloop_env; 
#else
extern int running;
#endif
extern int exit_value;

void exit_runloop(int ret_val);

#endif
