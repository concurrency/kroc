/* $Id: midinc.h,v 1.1 1996/04/15 10:52:14 djb1 Exp $ */

/*
 *	common include files for occam compiler generic parts
 *	Copyright (C) 1991 Inmos Limited
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

#include <setjmp.h>
#include <assert.h>

#include <stdio.h>
#define IMS_INCLUDED_STDIO_H
#include <stddef.h>   /* offsetof */

#include "imstype.h"  /* IMPORTED */
#include "imsvals.h"  /* IMPORTED */
#include "imsmisc.h"  /* IMPORTED */
#include "imsstd.h"   /* IMPORTED */
#include "txlib.h"    /* IMPORTED */

#include "ochdr.h"    /* occam  compiler declarations */

#include "lexhdr.h"
#include "nameshdr.h"
#include "treedef.h"
#include "errhdr.h"

#include "occamfe.h"  /* abstract access to occam compiler frontend */

#include "vti.h"
#include "strpool.h"

#include "constdef.h" /* contant folding definitions */

#include "listhdr.h"
/*#include "ocmisc.h"*/ /* now replaced by "suplib.h" */
