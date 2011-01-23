/**
 * 
 */
package org.transterpreter.occPlug;

/*
 * OccPlugUtil.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2004-2007 Christian L. Jacobsen
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
import java.io.IOException;
import java.net.URL;

import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.OperatingSystem;
import org.gjt.sp.jedit.jEdit;
import org.transterpreter.occPlug.OccPlug.DocumentWriter;

/**
 * @author clj3
 * 
 */
public class OccPlugUtil {
	private final static String	pfix	= OccPlugPlugin.OPTION_PREFIX;

	public static String pathify(String cmd) {
		if (!MiscUtilities.isAbsolutePath(cmd)) {
			String pathifyOverride = jEdit
					.getProperty(pfix + "pathifyOverride");
			if (pathifyOverride != null) {
				return MiscUtilities.constructPath(MiscUtilities.constructPath(
						pathifyOverride, "bin"), cmd);
			}
			return MiscUtilities
					.constructPath(MiscUtilities.constructPath(MiscUtilities
							.getParentOfPath(jEdit.getJEditHome()), "bin"), cmd);
		}
		return new String(cmd);
	}

	public static String pathifyXXX(String cmd) {
		if (!MiscUtilities.isAbsolutePath(cmd)) {
			String pathifyOverride = jEdit
					.getProperty(pfix + "pathifyOverride");
			if (pathifyOverride != null) {
				return MiscUtilities.constructPath(pathifyOverride, cmd);
			}
			return MiscUtilities.constructPath(MiscUtilities
					.getParentOfPath(jEdit.getJEditHome()), cmd);
		}
		return new String(cmd);
	}

	public static String getOccBuildCmd() {
		return jEdit.getProperty(pfix + "occbuildCmd", "");
	}

	public static String getTvmCmd() {
		return jEdit.getProperty(pfix + "tvmCmd", "");
	}

	// Do tvm args?
	/*
	 * if(jEdit.getProperty(pfix + "tvmArgs", "").trim().length() > 0) {
	 * this.tvmArgs = jEdit.getProperty(pfix + "tvmArgs", "").trim().split(",");
	 * }
	 */

	public static String getDllCmd() {
		return jEdit.getProperty(pfix + "dllCmd", "");
	}

	public static String[] getDllArgs() {
		if (jEdit.getProperty(pfix + "dllArgs", "").trim().length() > 0) {
			return jEdit.getProperty(pfix + "dllArgs", "").trim().split(",");
		}

		return new String[0];
	}

	public static String getFirmdlCmd() {
		return jEdit.getProperty(pfix + "firmdlCmd", "");
	}

	public static String[] getFirmdlArgs() {
		if (jEdit.getProperty(pfix + "firmdlArgs", "").trim().length() > 0) {
			return jEdit.getProperty(pfix + "firmdlArgs", "").trim().split(",");
		}

		return new String[0];
	}

	public static String getOccdocCmd() {
		return jEdit.getProperty(pfix + "occdocCmd", "");
	}

	// Do tvm args?
	/*
	 * if(jEdit.getProperty(pfix + "occdocArgs", "").trim().length() > 0) {
	 * this.occdocArgs = jEdit.getProperty(pfix + "occdocArgs",
	 * "").trim().split(","); }
	 */

	public static String getMasterLibraryPath() {
		return jEdit.getProperty(pfix + "MasterLibraryPath", "");
	}

	public static boolean getVerbose() {
		return jEdit.getBooleanProperty(pfix + "verbose", false);
	}

	public static String getLegoTowerPort() {
		return jEdit.getProperty(pfix + "legoTowerPort", "DEFAULT");
	}

	public static boolean getOptimalPrefixing() {
		return jEdit.getBooleanProperty(
				"options.occPlug.slinker.optimalPrefixing", false);
	}

	/*
	 * FIXME: I would like to do a yes, no, ask, mode for this property
	 */
	public static boolean getSaveOnCompile() {
		return jEdit.getBooleanProperty(pfix + "saveOnCompile", true);
	}

	/**
	 * Let the given URL be shown in a browser window.
	 * 
	 * @param url
	 *            the URL or file path to be shown.
	 * @return true if the web browser could be started, false otherwise.
	 */
	public static boolean openWebBrowser(String url) {

		if (OperatingSystem.isWindows()) { // Windows

			String cmd;
			// catering for stupid differences in Windows shells...
			if (OperatingSystem.isWindows9x()) // win95/98/Me
				cmd = "command.com";
			else
				// other
				cmd = "cmd.exe";

			try {
				// more stupid Windows differences...
				if (OperatingSystem.isWindows9x()) {
					Runtime.getRuntime()
							.exec(
									new String[] { cmd, "/c", "start",
											'"' + url + '"' });
				} else {
					Runtime.getRuntime().exec(
							new String[] { cmd, "/c", "start", "\"\"",
									'"' + url + '"' });
				}
			} catch (IOException e) {
				// FIXME: A message box here or something?
				// Debug.reportError("could not start web browser. exc: "+ e);
				return false;
			}
		} else { // Mac, Unix and other

			// The string should be either a URL or a file path
			try {
				return openWebBrowser(new URL(url));
			} catch (java.net.MalformedURLException mfue) {
				// return openWebBrowser(new File(url));
				try {
					return openWebBrowser(new File(url).toURI().toURL());
				} catch (java.net.MalformedURLException mfuexxx) {
					throw new RuntimeException(mfuexxx);
				}
			}

		}
		return true;
	}

	/**
	 * Let the given URL be shown in a browser window.
	 * 
	 * @param url
	 *            the URL to be shown.
	 * @return true if the web browser could be started, false otherwise.
	 */
	public static boolean openWebBrowser(URL url) {

		if (OperatingSystem.isMacOS()) { // Mac
			try {
				com.apple.eio.FileManager.openURL(url.toString());
			} catch (IOException e) {
				// FIXME: report error
				// Debug.reportError("could not start web browser. exc: " + e);
				return false;
			}
		} else if (OperatingSystem.isWindows()) {
			// Windows

			return openWebBrowser(url.toString());
		} else { // Unix and other

			/*
			 * FIXME: I'm going to ignore unix for now String cmd =
			 * mergeStrings(Config.getPropString("browserCmd1"),
			 * url.toString());
			 * 
			 * try { Process p = Runtime.getRuntime().exec(cmd);
			 * 
			 * // wait for exit code. 0 indicates success, otherwise // we try
			 * second command int exitCode = p.waitFor();
			 * 
			 * cmd = Config.getPropString("browserCmd2");
			 * 
			 * if(exitCode != 0 && cmd != null && cmd.length() > 0) { cmd =
			 * mergeStrings(cmd, url.toString()); // Debug.message(cmd); p =
			 * Runtime.getRuntime().exec(cmd); } } catch(InterruptedException e)
			 * { // FIXME: report error
			 * //Debug.reportError("cannot start web browser: " + cmd);
			 * //Debug.reportError("caught exc " + e); return false; }
			 * catch(IOException e) { // FIXME: report error
			 * //Debug.reportError("could not start web browser.  exc: "+ e);
			 * return false; }
			 */
		}
		return true;
	}
	
	// FIXME: This ought to be static, and probably not in here...
	public static void writeVerbose(String str, DocumentWriter doc) {
		if (OccPlugUtil.getVerbose()) {
			doc.writeRegular(str);
		}
	}

	/* From: 
           http://stackoverflow.com/questions/941272/
             how-do-i-trim-a-file-extension-from-a-string-in-java 
        */
	public static String removeExtension(String s) {

	    String separator = System.getProperty("file.separator");
	    String filename;

	    // Remove the path upto the filename.
	    int lastSeparatorIndex = s.lastIndexOf(separator);
	    if (lastSeparatorIndex == -1) {
		filename = s;
	    } else {
		filename = s.substring(lastSeparatorIndex + 1);
	    }

	    // Remove the extension.
	    int extensionIndex = filename.lastIndexOf(".");
	    if (extensionIndex == -1)
		return filename;

	    return filename.substring(0, extensionIndex);
	}

}
