package org.transterpreter.jeditwinsparkleplugin;

import org.gjt.sp.jedit.EditPlugin;
import org.gjt.sp.util.Log;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.PluginJAR;

 public class JEditWinSparklePlugin extends EditPlugin
 {
	public static final String Name = "JEditWinSparklePlugin";
	
	private static final String dllName = "WinSparkle";
	private static final String url = "http://127.0.0.1/Concurrency.cc/appcast.xml";
	
	private native void win_sparkle_init();
	private native void win_sparkle_set_appcast_url(String url);
	
	//Load the Library in a static initializer
	static
	{
		try
		{
			System.loadLibrary(dllName);
		}
		catch (java.lang.UnsatisfiedLinkError ex)
		{
			//TODO: Handle Error
		}
		
	}
	
	public void start()
	{
		//Set the appcast url - http://127.0.0.1/Concurrency.cc/appcast.xml
		win_sparkle_set_appcast_url(url);
		//Initate Winsparkle
		win_sparkle_init();
	}
 }
 