package org.transterpreter.occPlug.targets.support; 

/*
 * OccbuildTVMOptions.java
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

import org.transterpreter.occPlug.OccPlugUtil;
import org.transterpreter.occPlug.hosts.BaseHost;

public class OccbuildTVMOptions extends OccbuildOptions
{
	public OccbuildTVMOptions()
	{
		this("tvm");
	}
	
	public OccbuildTVMOptions(String tool)
	{
		BaseHost host = BaseHost.getHostObject();

		toolchain = "tvm";
		this.tool = tool;
		
		systemSearch = new String[] {
				OccPlugUtil.pathifyXXX(host.getPath(tool, "include")), 
				OccPlugUtil.pathifyXXX(host.getPath(tool, "lib"))};

	}
}