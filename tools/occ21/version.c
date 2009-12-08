/* $Id: version.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	Occam two compiler version details
 *	Copyright (C) 1987-1990 Inmos Limited
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

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#if 0
#ifdef LICENCE_MANAGER
#include "imsflm.h" /* IMPORTED */
#endif
#endif

#include "midinc.h"
#include "arg.h"

#include "version.h"
/*}}}*/


arg2_help_page_info oc_help_info = {
	.tool_name = TOOL,
	.toolset_description = TOOL_TOOLSET,
	.tool_description = TOOL_LONG,
	.version = TOOL_VERSION,
	.build_time = TOOL_DATE,
	.copyright_years = TOOL_COPYRIGHT,
	.usage_line = TOOL_USAGE,
	.env_var = TOOL_OPTENV
};
#if 0
/*{{{  PUBLIC void check_licence_manager */
PUBLIC void check_licence_manager(void); /* to shut gcc up */
PUBLIC void check_licence_manager(void)
{
#ifdef LICENCE_MANAGER
  const int status = ims_flexlm_checkout(IMS_LM_FEATURE, IMS_LM_VERSION);
  if (status != 0)
    exit(EXIT_FAILURE); /* get out as quickly as possible */
#else
  return;  
#endif
}
/*}}}*/
#endif
