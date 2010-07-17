package org.transterpreter.occPlug.hosts;

/*
 * Ubuntu.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2010 Christian L. Jacobsen, Matthew C. Jadud
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


import org.gjt.sp.util.Log;
import java.io.*;
import org.gjt.sp.jedit.jEdit;
import java.util.regex.*;

public class Ubuntu extends Unix {

	public Ubuntu()
	{
		super("ubuntu");
	}

	public void hostStartup () 
	{

		Log.log(Log.MESSAGE, this, "START CALLED FOR UBUNTU");
		String modeDir = jEdit.getSettingsDirectory() + "/modes";

		File catalog = new File(modeDir + "/catalog");

		if ( catalog.exists() ) 
		{
      // It returns false if File or directory does not exist
			Log.log(Log.MESSAGE, this, "CATALOG EXISTS");
			if ( ! isOccampiInCatalog(catalog) ) 
			{
				Log.log(Log.MESSAGE, this, "OCCAM-PI NOT IN CATALOG.");
			} else {
				Log.log(Log.MESSAGE, this, "OCCAM-PI IS IN CATALOG.");
			}
    } else { /* No catalog file. */
      // It returns true if File or directory exists
			Log.log(Log.MESSAGE, this, "CATALOG DOES NOT EXISTS");
    }
		
	}

	private boolean isOccampiInCatalog(File cat) 
	{
		boolean result = false;

		try {
    	FileInputStream fis = new FileInputStream(cat);
      BufferedInputStream bis = new BufferedInputStream(fis);
      DataInputStream dis = new DataInputStream(bis);
      
			Pattern pat = Pattern.compile("occam-pi");

			while (dis.available() != 0) {		
				String line = dis.readLine();
        Log.log(Log.MESSAGE, this, "CAT LINE: " + line);
				Matcher m = pat.matcher(line);
			
				if ( m.find() ) {
					Log.log(Log.MESSAGE, this, "FOUND OCCAM-PI!");
					result = true;
				}
				
      }

      fis.close();
      bis.close();
      dis.close();
		} catch (Exception e) {
			Log.log(Log.MESSAGE, this, "Error checking for occam-pi in catalog: " + e);
		}

			return result;
	}

}
