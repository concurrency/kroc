/*
tvm - transputer.c
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

#include "transputer.h"

BPOOTER iptr;             /* Instruction pointer */
POOTER wptr;              /* Workspace pointer */

WORD areg;                /* Evaluation stack */
WORD breg;                /* Evaluation stack */
WORD creg;                /* Evaluation stack */
WORD oreg;                /* Operand register */

POOTER tptr[NUM_PRI];     /* Timer queue pointers */
WORD tnext[NUM_PRI];      /* Timeout register */
POOTER fptr[NUM_PRI];     /* Front pointer (scheduler queue) */
POOTER bptr[NUM_PRI];     /* Back pointer (scheduler queue) */

WORD error_flag;         
WORD halt_on_error_flag;

/* State not in the original Transputer */

//WORD pri;                 /* Current priority level */
