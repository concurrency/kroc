package org.transterpreter.occPlug.hosts;

import java.util.Arrays;

/*
 * OSX.java
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


public class OSX extends Unix {

	public OSX()
	{
		super("osx");
		if(System.getProperty("os.arch").equals("ppc"))
		{
			base = "osx-ppc";
		}
	}

	public String[] getSerialPorts() {
		String []ttys = {"/dev/ttys0", "/dev/ttys1", "/dev/ttys2", "/dev/ttys3"};
		String []usbs = Native.getSerialPortNames();
		Arrays.sort(usbs);
		
		String[] all = new String[ttys.length+usbs.length];
		System.arraycopy(usbs, 0, all, 0, usbs.length);
		System.arraycopy(ttys, 0, all, usbs.length, ttys.length);
		
		return all;
	}
}
