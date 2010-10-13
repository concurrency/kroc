package org.transterpreter.occPlug.hosts;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Enumeration;
import java.util.Set;

import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.PluginJAR;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.util.Log;
import org.transterpreter.occPlug.OccPlugPlugin;


/*
 * Native.java
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

public class Native {

	public static String kUSBVendorID            = "idVendor";
	public static String kUSBProductID           = "idProduct";
	public static String kUSBProductString       = "USB Product Name";
	public static String kUSBVendorString        = "USB Vendor Name";
	public static String kUSBSerialNumberString	 = "USB Serial Number";
	
	public static native Map<String, Map<String, Object>> _getSerialPorts();
	
	public static Map<String, Map<String, Object>> getSerialPorts()
	{
    	try
    	{
            return _getSerialPorts();
    	}
    	catch(UnsatisfiedLinkError e)
    	{
    		return new HashMap<String, Map<String, Object>>();
    	}		
	}
	
	public static String[] getSerialPortNames()
	{
		Map<String, Map<String, Object>> m = getSerialPorts();
		String[] a = new String[m.size()];
		int i = 0;
	     for (Iterator<String> s = m.keySet().iterator(); s.hasNext() ; i++) {
	         a[i] = s.next();
	     }
	     
	     return a;
	}
	
	public static void printSerialPorts()
	{
		Map<String, Map<String, Object>> m = getSerialPorts();
		
		if(m == null)
		{
			System.out.println("Map returned from getSerialPorts was null!");
			return;
		}
		
		for(final String k : m.keySet())
		{
			System.out.println(k);
			Map<String, Object> v = m.get(k);
			for(final String ek : v.keySet())
			{
				System.out.println("  " + ek + ": " + v.get(ek));
			}
		}
	}
	
    static 
    {
    	/* FIXME: This will change for other platforms. I think there is a way to get 
    	 * the right extension though I can't remember how
    	 */
    	final String jarLibName = "native.dylib";
    	
        final PluginJAR jar = jEdit.getPlugin(OccPlugPlugin.class.getName()).getPluginJAR();
        final String libraryPath = MiscUtilities.constructPath(
                MiscUtilities.getParentOfPath(jar.getPath()), jarLibName);

    	try
    	{
    		System.load(libraryPath);
    	}
    	catch(UnsatisfiedLinkError e)
    	{
    		Log.log(Log.ERROR, null, "Could not find the native library, the following exception was caught:");
    		Log.log(Log.ERROR, null, e);
    	}	
    }
}
