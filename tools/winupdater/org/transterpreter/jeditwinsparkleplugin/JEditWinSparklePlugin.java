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
                final String libraryDir = MiscUtilities.getParentOfPath(jar.getPath());
                


                /* Always extract the native lib, in case it has been updated */
                try
                {
					ZipFile jarZip = jar.getZipFile();
					for(String lib: jarLibNames)
					{
                        InputStream i = jarZip.getInputStream(jarZip.getEntry(lib));
                        String libraryPath = MiscUtilities.constructPath(libraryDir, lib);
                        OutputStream o = new FileOutputStream(new File(libraryPath));
							
                        byte []b = new byte[4096];
                        int r;
                        while((r = i.read(b)) != -1)
                        {
                                o.write(b, 0, r);
                        }
                        o.close();

                        System.load(libraryPath);
                    }
                }
                catch(IOException ex)
                {
                        Log.log(Log.ERROR, this,
                          "Could not extract native library from:" +
                          jar); // + " to " + libraryPath);
                        Log.log(Log.ERROR, this, ex);
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
 
