package org.transterpreter.occPlug.hosts;

/*
 * Unix.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2010 Christian L. Jacobsen
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

import java.io.File;
import java.io.FilenameFilter;

public class Unix extends BaseHost {
	
	protected Unix(String base)
	{
		super(base);
	}
	
	public String[] getSerialPorts()
	{
		FilenameFilter filter = new FilenameFilter() {
			public boolean accept(File dir, String name) {
				return (
						/* Ubuntu 8, 9, 10 */
						name.startsWith("ttyUSB") 
						/* Mac OSX */
						|| name.startsWith("tty.usbserial-")
						/* *NIX */
						|| name.equals("ttys0") 
						|| name.equals("ttys1")
						|| name.equals("ttys2") 
						|| name.equals("ttys3"));
			}
		};
		
		File dir = new File("/dev");
		String[] devices = dir.list(filter);
		
		for(int i = 0; i < devices.length; i++)
			devices[i] = "/dev/" + devices[i];
		
		return devices;
	}
}
