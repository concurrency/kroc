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
		final PluginJAR jar = getPluginJAR();
		final String libraryPath = MiscUtilities.constructPath(
			MiscUtilities.getParentOfPath(
				jar.getPath()), jarLibName);

		try
		{
			System.load(libraryPath);
		}
		catch(UnsatisfiedLinkError e)
		{
			Log.log(Log.ERROR, null, "Could not find the native library, the following exception was caught:");
			Log.log(Log.ERROR, null, e);
		}

		initSparkle();
	}
}
