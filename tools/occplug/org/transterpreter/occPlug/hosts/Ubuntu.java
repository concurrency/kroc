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
import java.util.*;

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
			if ( ! isOccamPiInCatalog(catalog) ) 
			{
				Log.log(Log.MESSAGE, this, "OCCAM-PI NOT IN CATALOG.");
				addOccamPiToCatalog(catalog);
			} else {
				Log.log(Log.MESSAGE, this, "OCCAM-PI IS IN CATALOG.");
			}
    } else { /* No catalog file. */
      // It returns true if File or directory exists
			Log.log(Log.MESSAGE, this, "CATALOG DOES NOT EXISTS");
    }

		copyOccamPiXML();
		
	}

	public static void copyOccamPiXML () {
		String sourceDir  = "/usr/share/jedit/modes";
		File   sourceFile = new File(sourceDir + "/occam-pi.xml"); 
		String destDir    = jEdit.getSettingsDirectory() + "/modes";
		File   destFile   = new File(destDir + "/occam-pi.xml");

		if ( ! destFile.exists() ) {	
			try {	
				FileReader fr = new FileReader(sourceFile);
				BufferedReader br = new BufferedReader(fr);
			
				FileOutputStream out = new FileOutputStream(destFile);
				PrintStream p = new PrintStream(out);
			
				while (br.ready()) {
					p.println(br.readLine());
				}
				br.close();
				p.close();			

				Log.log(Log.MESSAGE, Ubuntu.class, "Done copying occam-pi.xml");

			} catch (Exception e) {
				Log.log(Log.MESSAGE, Ubuntu.class, "Error copying occam-pi.xml: " + e);
			}
		}
	}



	private void addOccamPiToCatalog(File cat) {
		try {
			FileReader fr = new FileReader(cat);
			BufferedReader br = new BufferedReader(fr);
			Pattern pat = Pattern.compile("</MODES>");
			ArrayList<String> file = new ArrayList<String>();

			while (br.ready()) {
				String line = br.readLine();
				Matcher m = pat.matcher(line);
			
				/* If we hit the closing tag, insert the new catalog entry first. */
				if ( m.find() ) {
					String insert = jEdit.getProperty("plugin.OccPlugPlugin.linux.occampi.catalog");
					file.add(insert);
				}

				/* But always add what we find along the way...*/
				file.add(line);
			} 

     	br.close();
			fr.close();
			
			/* Now, write the file back out */
			FileOutputStream out = new FileOutputStream(cat.toString());
			PrintStream p = new PrintStream(out);
	
			for (String s : file) {
				p.println(s);
			}
			p.close();
				
		} catch (Exception e) {
			Log.log(Log.MESSAGE, this, "ERROR INSERTING MODE IN CATALOG: " + e);
		}
}
			
			

	private boolean isOccamPiInCatalog(File cat) 
	{
		boolean result = false;

		try {
			FileReader fr = new FileReader(cat);
			BufferedReader br = new BufferedReader(fr);
			Pattern pat = Pattern.compile("occam-pi");

			while (br.ready()) {		
				String line = br.readLine();
				Matcher m = pat.matcher(line);
			
				if ( m.find() ) {
					Log.log(Log.MESSAGE, this, "FOUND OCCAM-PI!");
					result = true;
				}
				
      }
			br.close();
			fr.close();
		} catch (Exception e) {
			Log.log(Log.MESSAGE, this, "Error checking for occam-pi in catalog: " + e);
		}

			return result;
	}

}
