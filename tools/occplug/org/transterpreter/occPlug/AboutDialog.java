/**
 * 
 */
package org.transterpreter.occPlug;
/*
 * AboutDialog.java
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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.EnhancedDialog;
import org.transterpreter.occPlug.process.ExecWorker;
import org.transterpreter.occPlug.process.ExecWorkerHelper;
import org.transterpreter.occPlug.process.ReaderConduit;
import org.transterpreter.occPlug.process.SimpleWriter;

//{{{ private class occPlugAboutDialog extends EnhancedDialog implements ActionListener
public class AboutDialog extends EnhancedDialog implements ActionListener
{
	private JButton ok;
	
	private JPanel content;
	private JPanel versions;
	
	private JLabel occPlugVersion;
	private JLabel transterpreterVersion;
	private JLabel skrocVersion;
	private JLabel slinkerVersion;
	private JLabel library2Version;
	private JLabel occ21Version;
	private JLabel ilibrVersion;
	private JLabel tvmVersion;
	private JLabel tranx86Version;
	
	private ExecWorker execWorker;
	
	public AboutDialog(java.awt.Frame frame)
  {
		super(frame, "occPlug Versions", true);
		setup(frame);
	}

	private void setup(Component comp)
	{
		/* Set up the window + button */
		content = new JPanel(new BorderLayout(12,12));
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		ok = new JButton(jEdit.getProperty("common.ok"));
		getRootPane().setDefaultButton(ok);
		ok.addActionListener(this);	
		
		content.add(ok, BorderLayout.SOUTH);
		
		/* Do the version numbers */
		content.add(new JLabel("occPlug Version Numbers"), BorderLayout.NORTH);
		versions = new JPanel(new GridLayout(9, 2));
		versions.add(new JLabel("occPlug:"));
		occPlugVersion = new JLabel(
			jEdit.getProperty("plugin.org.transterpreter.occPlug.OccPlugPlugin.version") + " (Revision: " + Revision.svnRevision + ")");
		versions.add(occPlugVersion);
		// Transterpreter
		versions.add(new JLabel("Transterpreter package:"));
		transterpreterVersion = new JLabel(jEdit.getProperty("occPlug.TransterpreterVersion"));
		versions.add(transterpreterVersion);
		// skroc
		versions.add(new JLabel("skroc:"));
		skrocVersion = new JLabel("getting version information...");
		versions.add(skrocVersion);
		// slinker
		versions.add(new JLabel("slinker:"));
		slinkerVersion = new JLabel("getting version information...");
		versions.add(slinkerVersion);
		// library2
		versions.add(new JLabel("library2:"));
		library2Version = new JLabel("getting version information...");
		versions.add(library2Version);
		// occ21
		versions.add(new JLabel("occ21:"));
		occ21Version = new JLabel("getting version information...");
		versions.add(occ21Version);
		// ilibr
		versions.add(new JLabel("ilibr:"));
		ilibrVersion = new JLabel("getting version information...");
		versions.add(ilibrVersion);
		// tranx86
		versions.add(new JLabel("tranx86:"));
		tranx86Version = new JLabel("getting version information...");
		versions.add(tranx86Version);
		// tvm
		versions.add(new JLabel("tvm:"));
		tvmVersion = new JLabel("getting version information...");
		versions.add(tvmVersion);
		/* Add the panel */
		content.add(versions, BorderLayout.CENTER); 
		
		/* It would be cool to set everything to be visible at this point, and
		 * then let the labels update themselves as external processes are
		 * gathering the information, but I cannot get that to work!!!
		 */

		//setResizable(true);
		//setVisible(true);

		/* Start getting version numbers */
		Thread threads[] = new Thread[7];
		int counter = 0;
		threads[counter++] = getSkrocVersion();
		threads[counter++] = getSlinkerVersion();
		threads[counter++] = getLibrary2Version();
		threads[counter++] = getOcc21Version();
		threads[counter++] = getIlibrVersion();
		threads[counter++] = getTranx86Version();
		threads[counter++] = getTvmVersion();
		
		/* Wait for all the threads */
		for(int i = 0; i < threads.length; i++)
		{
			if(threads[i] != null)
			{
				try
				{
					threads[i].join();
				}
				catch(InterruptedException e)
				{
					// Should we do something about this????
				}
			}
		}
		
		/* Make everything look pretty, HA!*/
		pack();
		setLocationRelativeTo(GUIUtilities.getParentDialog(comp));
		
		setResizable(false);
		setVisible(true);		
	}

	//{{{ class CaptureExecWorkerHelper
	class CaptureExecWorkerHelper extends ExecWorkerHelper
	{
		public final ArrayList stdOutText = new ArrayList();
		public final ArrayList stdErrText = new ArrayList();
		public final JLabel label;
		
		public CaptureExecWorkerHelper(JLabel label)
		{
			super(false, true, true);
			this.label = label;
		}
		
		public Thread stdoutHandlerSetup(InputStream stdout)
		{
			return new ReaderConduit(new BufferedReader(new InputStreamReader(stdout)), new SimpleWriter()
			{
				public void write(String str)
				{
					stdOutText.add(str);
				}
			});
		}
		
		public Thread stderrHandlerSetup(InputStream stderr)
		{
			return new ReaderConduit(new BufferedReader(new InputStreamReader(stderr)), new SimpleWriter()
			{
				public void write(String str)
				{
					stdErrText.add(str);
				}
			});
		}
		
		public void interruptedExceptionHandler(Exception e)
		{
			label.setText("An Interrupted Exception occured" + e);
		}

		public void ioHandlerExceptionHandler(Exception e)
		{
			label.setText("An IO Exception occured" + e);
		}
		
		public void cannotExec(Exception e)
		{
			label.setText("Could not execute command");
		}		
	}
	//}}}
	
	//{{{ private Thread getSkrocVersion()
	private Thread getSkrocVersion()
	{
		/* $ skroc --version
		 * skroc version 0.5 (Revision: 756)
		 * Copyright 2005 M. C. Jadud, C. L. Jacobsen - www.transterpreter.org
		 */
		ArrayList skrocCommand = new ArrayList();
		ArrayList skrocEnv = new ArrayList();
		skrocCommand.add(OccPlugUtil.pathify(OccPlugUtil.getSkrocCmd()));
		skrocEnv.add("SKROCPATH=" + MiscUtilities.getParentOfPath((String) skrocCommand.get(0)));
		
		/* Add the --version flag */
		skrocCommand.add("--version");

		CaptureExecWorkerHelper worker = new CaptureExecWorkerHelper(skrocVersion)
		{
			public void cmdExited(int exitCode)
			{
				label.setText((String) stdOutText.get(0));
			}
		};
		execWorker = new ExecWorker(
			(String []) skrocCommand.toArray(new String[1]),
			(String []) skrocEnv.toArray(new String[1]),
			(File) null,
			worker);
		
		execWorker.start();
		
		return execWorker;
	}
	//}}}
	
	//{{{ private Thread getSlinkerVersion()
	private Thread getSlinkerVersion()
	{
		/*
		 * $ skroc --slinker-version
		 * Path to slinker: /Users/clj3/bin/slinker   <-- stderr
		 * slinker/library2 version 0.5 (Revision: 2162)
		 * Copyright 2006 M. C. Jadud, C. L. Jacobsen - www.transterpreter.org
		 * ... gpl disclamer ...
		 */
		ArrayList skrocCommand = new ArrayList();
		ArrayList skrocEnv = new ArrayList();
		skrocCommand.add(OccPlugUtil.pathify(OccPlugUtil.getSkrocCmd()));
		skrocEnv.add("SKROCPATH=" + MiscUtilities.getParentOfPath((String) skrocCommand.get(0)));
		
		/* Add the --version flag */
		skrocCommand.add("--slinker-version");

		CaptureExecWorkerHelper worker = new CaptureExecWorkerHelper(slinkerVersion)
		{
			public void cmdExited(int exitCode)
			{
				label.setText((String) stdOutText.get(0));
			}
		};
		execWorker = new ExecWorker(
			(String []) skrocCommand.toArray(new String[1]),
			(String []) skrocEnv.toArray(new String[1]),
			(File) null,
			worker);
		
		execWorker.start();
		
		return execWorker;
	}
	//}}}
	
	//{{{ private Thread getLibrary2Version()
	private Thread getLibrary2Version()
	{
		/*
		 * $ skroc --library2-version
		 * Path to slinker: /Users/clj3/bin/slinker   <-- stderr
		 * slinker/library2 version 0.5 (Revision: 2162)
		 * Copyright 2006 M. C. Jadud, C. L. Jacobsen - www.transterpreter.org
		 * ... gpl disclamer ...
		 */
		ArrayList skrocCommand = new ArrayList();
		ArrayList skrocEnv = new ArrayList();
		skrocCommand.add(OccPlugUtil.pathify(OccPlugUtil.getSkrocCmd()));
		skrocEnv.add("SKROCPATH=" + MiscUtilities.getParentOfPath((String) skrocCommand.get(0)));
		
		/* Add the --version flag */
		skrocCommand.add("--library2-version");

		CaptureExecWorkerHelper worker = new CaptureExecWorkerHelper(library2Version)
		{
			public void cmdExited(int exitCode)
			{
				label.setText((String) stdOutText.get(0));
			}
		};
		execWorker = new ExecWorker(
			(String []) skrocCommand.toArray(new String[1]),
			(String []) skrocEnv.toArray(new String[1]),
			(File) null,
			worker);
		
		execWorker.start();
		
		return execWorker;
	}
	//}}}
	
	//{{{ private Thread getIlibrVersion()
	private Thread getIlibrVersion()
	{
		/* $ skroc --ilibr-version
		 * Path to ilibr: /Users/clj3/bin/ilibr  <-- stderr
		 * ilibr : INMOS toolset librarian tool
		 * PC Version 2.01.03, (Jan 28 2005)
		 * Compiled from SGS-THOMSON sources with permission
		 * (c) Copyright INMOS Limited 1990, 1991, 1992, 1993.
		 *
		 * Usage: ilibr {option} filename
		 * ...
		 */
		ArrayList skrocCommand = new ArrayList();
		ArrayList skrocEnv = new ArrayList();
		skrocCommand.add(OccPlugUtil.pathify(OccPlugUtil.getSkrocCmd()));
		skrocEnv.add("SKROCPATH=" + MiscUtilities.getParentOfPath((String) skrocCommand.get(0)));
		
		/* Add the --version flag */
		skrocCommand.add("--ilibr-version");

		CaptureExecWorkerHelper worker = new CaptureExecWorkerHelper(ilibrVersion)
		{
			public void cmdExited(int exitCode)
			{
				label.setText((String) stdOutText.get(1));
			}
		};
		execWorker = new ExecWorker(
			(String []) skrocCommand.toArray(new String[1]),
			(String []) skrocEnv.toArray(new String[1]),
			(File) null,
			worker);
		
		execWorker.start();
		
		return execWorker;
	}
	//}}}

	//{{{ private Thread getOcc21Version()
	private Thread getOcc21Version()
	{
		/*
		 * $ skroc --occ21-version
		 * Path to occ21: /Users/clj3/bin/occ21  <-- stderr
		 * occ21 : occam 2.1 compiler
		 * occam 2.1 compiler Version OFA 1.3.3K (Jan 28 2005) (powerpc-apple-darwin7.7.0)
		 * (c) Copyright SGS-THOMSON Microelectronics Limited 1995,1996,1997
		 * Modifications (c) Copyright 1998, 1999, 2000, 2001 (see AUTHORS)
		 * 
		 * Usage: occ21 filename { -option }
		 * 
		 * Options include:
		 * ...
		 */
		ArrayList skrocCommand = new ArrayList();
		ArrayList skrocEnv = new ArrayList();
		skrocCommand.add(OccPlugUtil.pathify(OccPlugUtil.getSkrocCmd()));
		skrocEnv.add("SKROCPATH=" + MiscUtilities.getParentOfPath((String) skrocCommand.get(0)));
		
		/* Add the --version flag */
		skrocCommand.add("--occ21-version");

		CaptureExecWorkerHelper worker = new CaptureExecWorkerHelper(occ21Version)
		{
			public void cmdExited(int exitCode)
			{
				label.setText((String) stdOutText.get(1));
			}
		};
		execWorker = new ExecWorker(
			(String []) skrocCommand.toArray(new String[1]),
			(String []) skrocEnv.toArray(new String[1]),
			(File) null,
			worker);
		
		execWorker.start();
		
		return execWorker;
	}
	//}}}
	
	//{{{ private Thread getTranx86Version()
	private Thread getTranx86Version()
	{
		/*
		 * skroc --tranx86-version
		 * Path to tranx86: /Users/clj3/bin/tranx86 <-- stderr
		 * tranx86 version 0.9.8-10
		 */
		ArrayList skrocCommand = new ArrayList();
		ArrayList skrocEnv = new ArrayList();
		skrocCommand.add(OccPlugUtil.pathify(OccPlugUtil.getSkrocCmd()));
		skrocEnv.add("SKROCPATH=" + MiscUtilities.getParentOfPath((String) skrocCommand.get(0)));
		
		/* Add the --version flag */
		skrocCommand.add("--tranx86-version");

		CaptureExecWorkerHelper worker = new CaptureExecWorkerHelper(tranx86Version)
		{
			public void cmdExited(int exitCode)
			{
				label.setText((String) stdOutText.get(0));
			}
		};
		execWorker = new ExecWorker(
			(String []) skrocCommand.toArray(new String[1]),
			(String []) skrocEnv.toArray(new String[1]),
			(File) null,
			worker);
		
		execWorker.start();
		
		return execWorker;
	}
	//}}}
	
	//{{{ private Thread getTvmVersion()
	private Thread getTvmVersion()
	{
		/*
		 * tvm version 0.5
		 *   usage: tvm <bytecodefile> [memoryinbytes]
		 *  
		 *   Where the 'bytecodefile' is a valid transterpreter bytecode
		 * ...
		 */
		ArrayList tvmCommand = new ArrayList();
		tvmCommand.add(OccPlugUtil.pathify(OccPlugUtil.getTvmCmd()));

		CaptureExecWorkerHelper worker = new CaptureExecWorkerHelper(tvmVersion)
		{
			public void cmdExited(int exitCode)
			{
				label.setText((String) stdOutText.get(0));
			}
		};
		execWorker = new ExecWorker(
			(String []) tvmCommand.toArray(new String[1]),
			(String []) null,
			(File) null,
			worker);
		
		execWorker.start();
		
		return execWorker;
	}
	//}}}
		
	public void actionPerformed(ActionEvent evt)
	{
		Object source = evt.getSource();
		if(source == ok)
		{
			ok();
		}
	}
	
	public void ok()
	{
		dispose();
	}
	
	public void cancel()
	{
		dispose();
	}		
}


