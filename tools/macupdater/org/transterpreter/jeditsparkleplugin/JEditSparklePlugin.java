package org.transterpreter.jeditsparkleplugin;

import org.gjt.sp.jedit.EditPlugin;
import org.gjt.sp.util.Log;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.PluginJAR;

import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.File;
import java.io.IOException;
import java.util.zip.ZipFile;


/*
 * JEditSparklePlugin.java
 * part of the Spakle plugin for the jEdit text editor
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

public class JEditSparklePlugin extends EditPlugin {
	public static final String NAME = "JEditSparklePlugin";
	public static final String OPTION_PREFIX = "options.jeditsparkleplugin.";

	private native void initSparkle();

	private final String jarLibName = "JEditSparkleNativeBridge.dylib";
	public void start()
	{
		/* I'm not quite sure where the best place to extract the dylib
		   is. I've looked around on the jEdit mailing list, and seen
		   a few people do similar things to what I'm doing here:
		     * http://jedit.svn.sourceforge.net/viewvc/jedit/plugins/WincryptCipher/trunk/src/wincrypt/WincryptCipher.java?revision=9117&view=markup
		       (from: http://marc.info/?l=jedit-devel&m=124947547532162&w=2)
		   In 4.3pre? a getPluginHome and better support for a plugin
		   using external files have been included, but I cant use those
		   until we upgrade jEdit.
		*/
		final PluginJAR jar = getPluginJAR();
		final String libraryPath = MiscUtilities.constructPath(
			MiscUtilities.getParentOfPath(
				jar.getPath()), jarLibName);
		

		/* Always extract the native lib, in case it has been updated */
		try
		{
			ZipFile jarZip = jar.getZipFile();
			InputStream i = jarZip.getInputStream(
				jarZip.getEntry(jarLibName));
			OutputStream o = new FileOutputStream(
				new File(libraryPath));
			byte []b = new byte[4096];
			int r;
			while((r = i.read(b)) != -1)
			{
				o.write(b, 0, r);
			}
			o.close();
				
			System.load(libraryPath);
		}
		catch(IOException ex)
		{
			Log.log(Log.ERROR, this, 
			  "Could not extract native library from:" + 
			  jar + " to " + libraryPath);
			Log.log(Log.ERROR, this, ex);
		}
		catch(java.lang.UnsatisfiedLinkError ex)
		{
			Log.log(Log.ERROR, this, 
			  "Could not load native library from:" + 
			  libraryPath);
			Log.log(Log.ERROR, this, ex);
		}

	
		initSparkle();
	}
}
