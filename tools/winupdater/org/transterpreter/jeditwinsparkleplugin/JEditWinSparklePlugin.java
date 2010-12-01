package org.transterpreter.jeditwinsparkleplugin;

import org.gjt.sp.jedit.EditPlugin;
import org.gjt.sp.util.Log;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.PluginJAR;
import org.gjt.sp.jedit.jEdit;

import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.File;
import java.io.IOException;
import java.util.zip.ZipFile;

 public class JEditWinSparklePlugin extends EditPlugin
 {
	public static final String Name = "JEditWinSparklePlugin";
	
	private static final String dllName = "WinSparkle";
	
	private native void init(String company, 
				 String app, 
				 String version, 
				 String appCastURL);
	private native void unload();
	public static native void performUpdateCheckWithUI();
	
	private final String jarLibNames[] = {
		"WinSparkle.dll",
		"JEditWinSparkleNativeBridge.dll", 
		};
	
	public void start()
	{
        final PluginJAR jar = getPluginJAR();
		final String libraryDir = MiscUtilities.getParentOfPath(jar.getPath());
                
                

		try
		{
			for(String lib: jarLibNames)
			{
				String libraryPath = MiscUtilities.constructPath(libraryDir, lib);
				System.load(libraryPath);
			}
		}
		catch(java.lang.UnsatisfiedLinkError ex)
                {
                        Log.log(Log.ERROR, this,
                          "Could not load native library from:" +
                          libraryDir);
                        Log.log(Log.ERROR, this, ex);
                }

		String company = jEdit.getProperty("options.jeditwinsparkleplugin.company");
		if(company == null) throw new RuntimeException("please set options.jeditwinsparkleplugin.company");
		String app = jEdit.getProperty("options.jeditwinsparkleplugin.app");
		if(app == null) throw new RuntimeException("please set options.jeditwinsparkleplugin.app");
		String version = jEdit.getProperty("options.jeditwinsparkleplugin.version");
		if(version == null) throw new RuntimeException("please set options.jeditwinsparkleplugin.version");
		String url = jEdit.getProperty("options.jeditwinsparkleplugin.appcast");
		if(url == null) throw new RuntimeException("please set options.jeditwinsparkleplugin.appcast");
	

		//Initate Winsparkle
		init(company, app, version, url);
	}

	public void stop()
	{
		unload();
	}
 }
 
