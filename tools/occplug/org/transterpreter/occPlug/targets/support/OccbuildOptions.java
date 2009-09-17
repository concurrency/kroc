package org.transterpreter.occPlug.targets.support;

/*
 * OccbuildOptions.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2009 Christian L. Jacobsen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.transterpreter.occPlug.OccPlugUtil;

public abstract class OccbuildOptions
{
	public boolean long_error_messages = false;
	public String target_cpu           = null;
	public boolean verbose             = OccPlugUtil.getVerbose();
	public String toolchain;
	public String[] search             = null;
	public String[] systemSearch;
	public final Hashtable defines     = new Hashtable();
	
	
	/* Options that I previously used, but don't know if I still should, check what they do:
	 * 		occbuildCommand.add("--prefix="
			+ MiscUtilities.getParentOfPath(occbuildPath));
	 */
	public String[] generateOptions()
	{
		int i;
		ArrayList options = new ArrayList();
		
		if(!long_error_messages)
		{
			options.add("--occ21-opts");
			options.add("-b");
		}
		
		if(target_cpu != null)
		{
			options.add("--target-cpu");
			options.add(target_cpu);				
		}
		
		if(verbose) options.add("-v");
		
		
		for(i = 0; i < systemSearch.length; i++)
		{
			options.add("--search");
			options.add(systemSearch[i]);
		}

		if(search != null){
			for(i = 0; i < search.length; i++)
			{
				options.add("--search");
				options.add(search[i]);
			}		
		}
		
		for (Enumeration e = defines.keys() ; e.hasMoreElements() ;) {
			String key = (String) e.nextElement();
			String value = (String) defines.get(key);
			if(value != null)
			{
				options.add("-D" + key + "=" + value);
			}
			else
			{
				options.add("-D" + key);
			}
		}
		
		return (String[]) options.toArray(new String[0]);
	}
}