package org.transterpreter.occPlug;
/*
 * occPlug.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2004-2008 Christian L. Jacobsen, Jon Simpson
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


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.AttributeSet;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.EBComponent;
import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.EditBus;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.OperatingSystem;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.DockableWindowManager;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.util.Log;
import org.transterpreter.occPlug.process.ExecWorker;
import org.transterpreter.occPlug.process.ExecWorkerHelper;
import org.transterpreter.occPlug.process.ReaderConduit;
import org.transterpreter.occPlug.process.ReaderConduitTest;
import org.transterpreter.occPlug.process.ReaderConduitTest2;
import org.transterpreter.occPlug.process.SimpleWriter;

import de.mud.terminal.vt320;
import errorlist.DefaultErrorSource;
import errorlist.ErrorSource;

public class OccPlug extends JPanel implements EBComponent
{	
	private org.gjt.sp.jedit.View view;
	private boolean floating;
	private OccPlugToolPanel toolPanel;
	/* BUG:
	 * There seems to be a problem with this component, ie when adding a very
	 * long line to it, it can make the application hang, I am not sure if this
	 * is related to bug 4180751, though this one has been closed and fixed!
	 * I cannot find anything else related :(
	 */
	/* This is the compile console really, FIXME: rename this variable */
	private JTextPane textArea;
	private JScrollPane textAreaPane;
	/* This is where the running programs will display themselves */
	private vt320 terminal;
	private BlinkableSwingTerminal terminalArea;
	/* The cardlayout where the two terminals live */
	private JPanel displayPanel;
	
	private boolean errorSourceRegistered = false;
	private DefaultErrorSource errorSource;
	
	private Pattern errorPattern;
	private Pattern warningPattern;
	private Pattern undeclaredPattern;
	
	private Pattern libaryPathPattern;
	
	private HashMap provisionalErrorList = new HashMap();
	
	private OutputStream keyboardOutputStream;
	
	private ExecWorker execWorker = null;
  
	private SRV srv = null;
	
	private JTextField commandText;
	private JButton sendBtn;
	
	private class ErrorSet
	{
		public ErrorKey key;
		public Error error;
	}
	
	private class ErrorKey
	{
		public int lineNo;
		public String fileName;
		
		public ErrorKey(int lineNo, String fileName)
		{
			this.lineNo = lineNo;
			this.fileName = fileName;
		}
		
		public int hashCode()
		{
			return lineNo + fileName.hashCode();
		}
		
		public boolean equals(Object other)
		{
			if(other instanceof ErrorKey)
			{
				ErrorKey realOther = (ErrorKey) other;
				if(lineNo != realOther.lineNo)
					return false;
				return fileName.equals(realOther.fileName);
			}
			else
				return false;	
		}
	};
	
	private class Error
		{
			public int type;
			//public String msg;
			public ArrayList msgs = new ArrayList(2);
			
			public Error(int type, String msg)
			{
				this.type = type;
				msgs.add(msg);
			}
			
			public void setType(int type)
			{
				// We are only allowed to upgrade from warning to error
				// if an Error class is already an error, it cannot change
				// back to a warning...
				if(this.type == ErrorSource.WARNING)
				{
					this.type = type;
				}
			}
			
			public void addTxt(int type, String msg)
			{
				setType(type);
				msgs.add(msg);
			}
		};
		
		
	private class NullOutputStream extends OutputStream
	{
		public void write(int b)
		{
			/* Discard the input */
		}
	}
	//
	// Constructors
	//
	
	//{{{ Constructor: OccPlug(org.gjt.sp.jedit.View view, String position)
	public OccPlug(final org.gjt.sp.jedit.View view, final String position)
	{
		super(new BorderLayout());      
				
		this.view = view;
		this.floating = position.equals(DockableWindowManager.FLOATING);
	
		/* FIXME: What did this do? */
		if(jEdit.getSettingsDirectory() != null)
		{
			//obtainSettings();
		}
		
		this.toolPanel = new OccPlugToolPanel(this);
		add(BorderLayout.NORTH, this.toolPanel);

		if(floating)
			this.setPreferredSize(new Dimension(500, 250));
		
		/* Create the text area where things are going to be displayed */
		textArea = new JTextPane();
		/* We want hyperlinks though */
		textArea.setEditorKit(new StyledLinkEditorKit());
		textArea.setEditable(false);
		/* For getting key strokes */
		textArea.setFocusable(true);
		textArea.setFocusTraversalKeysEnabled(false);
		textArea.addKeyListener(new KeyListener()
		{
			/** Handle the key typed event from the text field. */
			public void keyTyped(KeyEvent e) 
			{
				//GUIUtilities.message(textArea, e.getKeyChar(), null);
				try
				{ 
					OutputStream o = getKeyboardOutputStream();
					o.write(e.getKeyChar());
					o.flush();
				}
				catch(IOException ex)
				{
					throw new RuntimeException("Got an error: " + ex);
				}
			}
			
			/** Handle the key pressed event from the text field. */
			public void keyPressed(KeyEvent e) 
			{
				/* Ignore */
			}
			
			/** Handle the key released event from the text field. */
			public void keyReleased(KeyEvent e) 
			{
				/* Ignore */
			}
		});
		textArea.addHyperlinkListener(new HyperlinkListener()
		{
			public void hyperlinkUpdate(HyperlinkEvent event)
			{
				JEditorPane pane = (JEditorPane) event.getSource();
				HyperlinkEvent.EventType type = event.getEventType();
				Element sourceElement = event.getSourceElement();
				
				ErrorSet err = (ErrorSet) sourceElement.getAttributes().getAttribute(StyledLinkEditorKit.LINK);
				
				JEditTextArea ta = view.getTextArea();
				//ta.moveCaretPosition(err.key.lineNo, true);
				//ta.centerCaret();
				ta.scrollTo(err.key.lineNo, 0, true);
				
				Log.log(Log.DEBUG, this, (new Date().toString()) + " Hyperlink: " + err);
			}
		});
		setKeyboardOutputStream(new NullOutputStream());
		/* Add styles to the document */
		addStylesToDocument(textArea.getStyledDocument());
		/* Add the text area to a scrollpane */
		textAreaPane = new JScrollPane(textArea);
		/* Create the terminal view */
		terminal = new vt320()
		{
			public void beep()
			{
				 /* Audiable bell */
				 Toolkit.getDefaultToolkit().beep();
				 /* Visual Bell */
				 terminalArea.blink();
			}
			
			public void write(byte[] b)
			{
				try
				{
					OutputStream o = getKeyboardOutputStream();
					o.write(b);
					o.flush();
				}
				catch(IOException e)
				{
					/* Donno what to do with this */
					throw new RuntimeException(e);
				}
			}
			
			public void clear()
			{
				final String clear = new String( new char[] { 27, '[', '2', 'J' });
				
				// VT220 Erase screen
				putString(clear);				
			}
		};
		terminalArea = new BlinkableSwingTerminal(terminal);
		/* Set up the panel which will display either the console or the terminal */
		displayPanel = new JPanel(new CardLayout());
		displayPanel.add(textAreaPane, "console");
		displayPanel.add(terminalArea, "terminal");
		add(BorderLayout.CENTER, displayPanel);

		/* Command field & send button */
		commandText = new JTextField();
		commandText.setEnabled(false);
		commandText.addActionListener(
			new ActionListener()
			{
				public void actionPerformed(ActionEvent evt) {
				 	sendBtn.doClick();
					commandText.grabFocus();
				}
			}
		);
		sendBtn = new JButton("Send");
		sendBtn.setEnabled(false);
		sendBtn.addActionListener(
			 new ActionListener()
			 {
				public void actionPerformed(ActionEvent evt) {
					final DocumentWriter compileConsole = new DocumentWriter(textArea.getStyledDocument());
					final String text = commandText.getText();
					commandText.setText("");
					sendBtn.setEnabled(false);
					commandText.setEnabled(false);
					compileConsole.writeRegular(">> " + text + "\n");
					Runnable runner = new Runnable() {
						public void run() {
							OutputStream os = srv.getOutputStream();
							try {
								byte[] cmd = text.getBytes();
								os.write(cmd);
								os.write((byte) '\n');
								os.flush();
								setCommandBarEnabled(true);
							} catch (IOException e) {
								compileConsole.writeError("IO Exception when sending command\n");
							}
						}
					};
					(new Thread(runner)).start();
				}
			}
		);
		Container c = Box.createHorizontalBox();
		c.add(new JLabel(" Command: "));
		c.add(commandText);
		c.add(sendBtn);
		
		add(BorderLayout.SOUTH, c);
		
		// Patterns for finding errors and warnings in occ21 output
		errorPattern = Pattern.compile("Error-oc(?:c21)?-(.*?)\\((.*?)\\)- (.*)\n?");
		warningPattern = Pattern.compile("Warning-oc(?:c21)?-(.*?)\\((.*?)\\)- (.*)\n?");
		undeclaredPattern = Pattern.compile("() undeclared on \"()\" lines ([0-9]*,?)*\n?");
		
		// Patterns for finding libary pahts in library collections, these can
		// then be inserted into occ21's ISEARCH environment variable
		/* FIXME: This will not work for things with spaces in their names */
		libaryPathPattern = Pattern.compile("-L\\s*(\\S*)");
		
		//obtainSettings();
	}
	//}}}	
	
	public OutputStream getKeyboardOutputStream()
	{
		synchronized(keyboardOutputStream)
		{
			return keyboardOutputStream;
		}
	}
	
	public void setKeyboardOutputStream(OutputStream keyboardOutputStream)
	{
		synchronized(keyboardOutputStream)
		{
			this.keyboardOutputStream = keyboardOutputStream;
		}
	}

	//
	// EBComponent implementation
	//

	//{{{ Method: handleMessage(EBMessage message)
	public void handleMessage(EBMessage message)
	{
		/*
		if(message instanceof PropertiesChanged)
		{
			obtainSettings();
		} 
		*/
		/*
		if(message instanceof PropertiesChanged)
		{
			propertiesChanged();
		} 
		else if(message instanceof EditPaneUpdate)
		{
			JEditTextArea textArea = ((EditPaneUpdate) message).getEditPane().getTextArea();
			canvas.updateContourRandom(textArea);
		}
		*/
	}
	//}}}
	
	//
	// These JComponent methods provide the appropriate points
	// to subscribe and unsubscribe this object to the EditBus
	//
	
	//{{{ Method: addNotidy()
	public void addNotify()
	{
		super.addNotify();
		EditBus.addToBus(this);
		
		// FIXME: Donno where the best place to do this is...
		errorSource = new DefaultErrorSource("occPlug");
		registerErrorSource();
	}
	//}}}

	//{{{ Method: removeNotify
	public void removeNotify()
	{
		super.removeNotify();
		EditBus.removeFromBus(this);
		
		unregisterErrorSource();
	}
	//}}}
	
	public void registerErrorSource()
	{
		if(!errorSourceRegistered)
		{
		  ErrorSource.registerErrorSource(errorSource);
			
			errorSourceRegistered = true;
		}
	}
	
	public void unregisterErrorSource()
	{
		if(errorSourceRegistered)
		{
			ErrorSource.unregisterErrorSource(errorSource);
			
			errorSourceRegistered = false;
		}
	}
	
	public StyledDocument getConsoleDoc()
	{
		return textArea.getStyledDocument();	
	}
	
	public void updateErrorSource(String path)
	{
		Iterator errors = provisionalErrorList.entrySet().iterator();
		
		while(errors.hasNext())
		{
			Map.Entry item = (Map.Entry) errors.next();
			ErrorKey key = (ErrorKey) item.getKey();
			Error err = (Error) item.getValue();
			/*
			errorSource.addError(
			  err.type,
				MiscUtilities.constructPath(path, key.fileName),
				key.lineNo,
				0,
				0,
				err.msgs.get(0));
				*/
			DefaultErrorSource.DefaultError newError = new DefaultErrorSource.DefaultError(
			  errorSource,
			  err.type,
				MiscUtilities.constructPath(path, key.fileName),
				key.lineNo,
				0,
				0,
				(String) err.msgs.get(0));
				
			if(err.msgs.size() > 1)
			{
				for(int i = 1; i < err.msgs.size(); i++)
				{
					newError.addExtraMessage((String) err.msgs.get(i));
				}
			}
			
			errorSource.addError(newError);
		}
	}


	public synchronized ErrorSet checkForErrors(String str, String path)
	{		
		Matcher m;
		
		m = errorPattern.matcher(str);
		if(m.matches())
		{
			/*
			errorSource.addError(
				ErrorSource.ERROR,
				MiscUtilities.constructPath(path, m.group(1)),
				,
				0,
				0,
				"Error: " + m.group(3));
			*/
			ErrorSet err = new ErrorSet();
			
			err.key = new ErrorKey(
				new Integer(m.group(2)).intValue() - 1, 
				m.group(1));
			if(provisionalErrorList.containsKey(err.key))
			{
				((Error) provisionalErrorList.get(err.key)).addTxt(
				  ErrorSource.ERROR,
					"Error: " + m.group(3));
				err.error = (Error) provisionalErrorList.get(err.key);
			}
			else
			{
				err.error = new Error(ErrorSource.ERROR, "Error: " + m.group(3));
				provisionalErrorList.put(err.key, err.error);
			}
				
			return err;
		}
		m = warningPattern.matcher(str);
		if(m.matches())
		{
			/*
			errorSource.addError(
				ErrorSource.WARNING,
				MiscUtilities.constructPath(path, m.group(1)),
				new Integer(m.group(2)).intValue() - 1,
				0,
				0,
				"Warning: " + m.group(3));
			*/
			ErrorSet err = new ErrorSet();
			
			err.key = new ErrorKey(
				new Integer(m.group(2)).intValue() - 1, 
				m.group(1));
			if(provisionalErrorList.containsKey(err.key))
			{
				((Error) provisionalErrorList.get(err.key)).addTxt(
				  ErrorSource.WARNING,
					"Warning: " + m.group(3));
				err.error = (Error) provisionalErrorList.get(err.key);
			}
			else
			{
				err.error =new Error(ErrorSource.WARNING, "Warning: " + m.group(3));
				provisionalErrorList.put(err.key, err.error);
			}		
			
			return err;
		}
		
		return null;
	}
				
	public void writeVerbose(String str, DocumentWriter doc)
	{
		if(OccPlugUtil.getVerbose())
		{
			doc.writeRegular(str);
		}
	}

	public void clearTextArea()
	{
		/* FIXME: ??? not sure, could be ok */
		textArea.setText("");
		
		/* Talk VT220 to the terminal... */
		final String clear = new String(new char[] { 27, '[', '2', 'J' });
		final String zero = new String(new char[] {27, '[', '1', ';', '1', 'H'});
		terminal.putString(clear);
		terminal.putString(zero);
	}
	
	public void stopRunningProcess()
	{
		if(execWorker != null)
		{
			execWorker.kill();
		}
		else if (srv != null)
		{
			final SRV srvCopy = srv;
			Runnable runner = new Runnable() {
				public void run() {
					srvCopy.sendCommand(SRV.CMD_KILL);
					srvCopy.sendCommand(SRV.CMD_KILL);
					srvCopy.sendCommand(SRV.CMD_KILL);
					srvCopy.sendCommand(SRV.CMD_KILL);
					srvCopy.disconnect();
				}
			};
			(new Thread(runner)).start();
			srv = null;
			setCommandBarEnabled(false);
		}
		else
		{
			throw new RuntimeException("Tried to stop a running process, but there are no processes running!");
		}
	}
	
//{{{	public int compile(String type, org.gjt.sp.jedit.View view, Buffer buffer)
	public int compile(String type, org.gjt.sp.jedit.View view, Buffer buffer)
	{			
		final long startTime = System.currentTimeMillis();
		/* Java complains that these may not have been initialised if I dont do this */
		long skrocStartTime = 0;
		long skrocEndTime = 0;
		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea.getStyledDocument());
	
		final String occFile  = buffer.getName();
    final String baseFile = MiscUtilities.getFileNameNoExtension(occFile);
		final String tceFile  = baseFile + ".tce";
		final String tbcFile  = baseFile + ".tbc"; 
		final String srecFile = baseFile + ".srec";
    final String srvFile  = baseFile + ".srv";
		final String directory = buffer.getDirectory();
		
		setVisibleDisplayArea("console");
		
		if (occFile.toLowerCase().endsWith(".occ")) 
		{
			// Get the library arguments
			/*String slinkerLibs = OccPlugPlugin.slinkerLibProps.getProperty(toolPanel.getSLinkerLibsSelection());*/
			
			// Clear the text editor
			textArea.setText("");
			// Clear the provisional error list
			provisionalErrorList.clear();
			// Clear the error source
			errorSource.clear();
			unregisterErrorSource();

			/* FIXME: This needs to be done better... ie some options of what to do 
			 * when the file is dirty, ie save, ask, or dont ask... */
			/* Check if the file is dirty, and ask if user wants to save if it is */
			
			/* There's now an option to save automatically on compile, which is enabled by default.
			 * if disabled, the user is prompted */
			 
			
			 if(buffer.isDirty() && OccPlugUtil.getSaveOnCompile()) {
				 // Save the file, and let the user know we saved the file for them..
				 compileConsoleDoc.writeWarning("Warning: Changed source file saved.\n");
				 
				 buffer.save(view, null);
				 VFSManager.waitForRequests();
			 } 	 
			else if (buffer.isDirty() && !OccPlugUtil.getSaveOnCompile())
			{
				int answer = JOptionPane.showConfirmDialog(view, "Save file?", "Save file?", JOptionPane.YES_NO_CANCEL_OPTION);
				
				if(answer == JOptionPane.CANCEL_OPTION)
				{
					// Dont complete the compile
					compileConsoleDoc.writeError("Compilation canceled\n");
					return 1;
				}
			
				if(answer == JOptionPane.YES_OPTION)
				{
					// Save the file, keeping the file name the same
					buffer.save(view, null); 
				}
				else
				{
					compileConsoleDoc.writeWarning("WARNING: SOURCE FILE NOT SAVED!!!\n");
				}
			}
		
	
			/* Update buttons on the toolpanel */
			toolPanel.setState(toolPanel.RUNNING);
		
			/* 
			 * Start of skroc command
			 */
			ArrayList occbuildCommand = new ArrayList();
			ArrayList occbuildEnv = new ArrayList();
			/* We need to set the SKROCPATH environment variable, or skroc is not
			 * going to be able to find any of its binaries
			 */
			final String occbuildPath;
			/* If this is an absolute path, dont touch it, otherwise try to
			   put in the path we think this should be */
			if(!MiscUtilities.isAbsolutePath(OccPlugUtil.getOccBuildCmd()))
			{
				final String occbuildCmdWithPath = MiscUtilities.constructPath(
					MiscUtilities.constructPath(MiscUtilities.getParentOfPath(jEdit.getJEditHome()), "bin"), OccPlugUtil.getOccBuildCmd());
				occbuildCommand.add(occbuildCmdWithPath);
				occbuildPath = MiscUtilities.getParentOfPath(occbuildCmdWithPath);
			}
			else
			{
				occbuildCommand.add(OccPlugUtil.getOccBuildCmd());
				occbuildPath = MiscUtilities.getParentOfPath(OccPlugUtil.getOccBuildCmd());
			}
			/*
			skrocEnv.add("SKROCPATH=" + skrocPath);
      skrocEnv.add("SKROC_TVM_CONFIG_H=" + 
          MiscUtilities.constructPath(
            MiscUtilities.constructPath(
              MiscUtilities.getParentOfPath(jEdit.getJEditHome()), "include"),
            "tvm_config.h"));
			// Mac OS X Specific things
			if(OperatingSystem.isMacOS())
			{
				// If on OSX set up the DYLD_FRAMEWORK_PATH so scheme works...
				// is this a safe thing to do?
				skrocEnv.add("DYLD_FRAMEWORK_PATH=" + skrocPath);
			} /* if OperatingSystem.isMacOS() */
	    		occbuildEnv.add("OCC21=" + MiscUtilities.constructPath(occbuildPath, "occ21"));
			occbuildEnv.add("TCE-DUMP.PL=" + MiscUtilities.constructPath(occbuildPath, "tce-dump.pl"));
			occbuildEnv.add("PLINKER.PL=" + MiscUtilities.constructPath(occbuildPath, "plinker.pl"));
			/* Make error messages brief */
			occbuildCommand.add("--occ21-opts");
			occbuildCommand.add("-b");
			occbuildCommand.add("--prefix=" + MiscUtilities.getParentOfPath(occbuildPath));
			// Verbose SKRoC if we are verbose
			if(OccPlugUtil.getVerbose())
			{
				occbuildCommand.add("-v");
			}
			// Set up the master library path to the one contained within
			// the Transterpreter if none is specified
			occbuildCommand.add("--search");
			if(OccPlugUtil.getMasterLibraryPath().equals(""))
			{
				occbuildCommand.add(MiscUtilities.constructPath(MiscUtilities.getParentOfPath(jEdit.getJEditHome()), "lib"));
			}
			else
			{
				occbuildCommand.add(OccPlugUtil.getMasterLibraryPath());
			}
      
			/* Libraries */
/*			if(!slinkerLibs.trim().equals(""))
			{
				String[] libsAsArgs = slinkerLibs.trim().split(" ");
				for(int i = 0; i < libsAsArgs.length; i++)
				{
					if(!libsAsArgs[i].equals(""))
					{
						skrocCommand.add(libsAsArgs[i]);
					}
				}
			}
*/
			/* FIXME:
			 * This is a horrible way of doing this flag, it should not test the
			 * type thing like this, rather the type should have the wordlength 
			 * embedded or something
			 */
			String cm = (System.getProperty("os.name").startsWith("Windows") ? "\\" : "");
			if(type.equals("Surveyor SRV-1"))
			{
				//skrocCommand.add("--slinker-opts");
				//skrocCommand.add("--tlp-types " + cm + "\"(CHAN BYTE IN, CHAN BYTE OUT, CHAN P.LASER@#105438A5 OUT, CHAN P.LED@#BDCB1BE5 OUT, CHAN P.MOTOR@#779CE8A5 OUT)" + cm + "\"");
				occbuildCommand.add("--blackfin");
				occbuildCommand.add("--no-std-libs");
				occbuildCommand.add("-f");
				occbuildCommand.add(srvFile);
			}
			else if(type.equals("Mindstorms RCX"))
			{
				//occbuildCommand.add("--slinker-opts");
				//occbuildCommand.add("--tlp-types " + cm + "\"()" + cm + "\"");
				occbuildCommand.add("--target");
				occbuildCommand.add("t2");
				occbuildCommand.add("--srec");
				occbuildCommand.add("-f");
				occbuildCommand.add(srecFile);
			}
			else if(type.equals("Desktop"))
			{
				//occbuildCommand.add("--slinker-opts");
				//occbuildCommand.add("--tlp-types " + cm + "\"(CHAN BYTE IN, CHAN BYTE OUT, CHAN BYTE OUT)" + cm + "\"");
				//occbuildCommand.add("--target");
				//occbuildCommand.add("t8");
				//occbuildCommand.add("--bytecode");
				//occbuildCommand.add("-f");
				//occbuildCommand.add(tbcFile);
				occbuildCommand.add("--toolchain=tvm");
				occbuildCommand.add("--program");
			} else {
				compileConsoleDoc.writeError("Error: I don't understand '" + type + "' as a platform type!\n");
				return 0;
			}
			occbuildCommand.add(occFile);

			// Say what we are doing
			compileConsoleDoc.writeRegular("Compiling: " + occFile + "\n");
			//compileConsoleDoc.writeRegular(skrocCommand + "\n");			
			writeVerbose(occbuildCommand + "\n", compileConsoleDoc);
			
			// Set up the environment
			String[] env = (String[]) occbuildEnv.toArray(new String[1]);
			//compileConsoleDoc.writeRegular(skrocEnv + "\n");
			writeVerbose(occbuildEnv + "\n", compileConsoleDoc);
						
			execWorker = new ExecWorker(
				(String []) occbuildCommand.toArray(new String[1]),
				(String []) occbuildEnv.toArray(new String[1]),
				new File(directory),
				new NonInteractiveExecWorkerHelper("occbuild", buffer.getDirectory())
				{
					public Thread stdoutHandlerSetup(InputStream stdout)
					{
						return new ReaderConduit(new BufferedReader(new InputStreamReader(stdout)), new SimpleWriter()
						{
							public void write(String str)
							{
								/* Returns the style in errStatus which should be used for 
								 * displaying the error message
								 */
								ErrorSet errStatus = checkForErrors(str, directory);
								String style = "regular";
								/* Check if it was an error (ie something other than regular 
								 * was returned 
								 */
								MutableAttributeSet a = null;
								/* errstatus is null if there was no error */
								if(errStatus != null)
								{
									switch(errStatus.error.type)
									{
										case ErrorSource.ERROR:
											a = new SimpleAttributeSet();
											a.addAttribute(StyledLinkEditorKit.LINK, errStatus);
											style = "error";
											break;
										case ErrorSource.WARNING:
											a = new SimpleAttributeSet();
											a.addAttribute(StyledLinkEditorKit.LINK, errStatus);
											style = "warning";
											break;
									}
								}
								compileConsoleDoc.write(str, style, a);
							}
						});
					}
				});
			
			execWorker.start();

			//compileConsoleDoc.writeOK("Success!!!\n");
		}
		else
		{
			compileConsoleDoc.writeError("Error: Only occam (.occ) source files can be compiled.\n");
			compileConsoleDoc.writeError("       The current buffer does not contain a .occ file!\n");
		}
			 
		return 0;	
	}
	
	public void compile(String type)
	{
		this.compile(type, this.view, this.view.getBuffer());
	}
//}}}
	
//{{{ public void legodl(String filename, String workingDir, int prognum) throws IOException
	public void legodl(String filename, String workingDir, int prognum) throws IOException
	{	
		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea.getStyledDocument());
		
		ArrayList dllCommand = new ArrayList();
		
		/* Update buttons on the toolpanel */
		toolPanel.setState(toolPanel.RUNNING);
			
		dllCommand.add(OccPlugUtil.pathify(OccPlugUtil.getDllCmd()));
		for(int i = 0; i < OccPlugUtil.getDllArgs().length; i++)
		{
			dllCommand.add(OccPlugUtil.getDllArgs()[i]);
		}
		if(prognum != -1)
		{
			dllCommand.add("-p" + prognum);
		}
		if(!OccPlugUtil.getLegoTowerPort().equals("DEFAULT"))
		{
			dllCommand.add("--tty=" + OccPlugUtil.getLegoTowerPort());
		}
		dllCommand.add(filename);
		
		compileConsoleDoc.clear();
		compileConsoleDoc.writeRegular("Uploading: " + filename + " \n");
		compileConsoleDoc.writeRegular(dllCommand + " \n");
			
		/* FIXME: Why do we do this???? */
		File wDir = null;
		if(workingDir != null)
		{
			wDir = new File(workingDir);
		}

		execWorker = new ExecWorker(
			(String []) dllCommand.toArray(new String[1]),
			(String []) null,
			wDir,
			new InteractiveExecWorkerHelper("dll"));
		
		execWorker.start();
	}
	//}}}
	
	
	//{{{ private class NonInteractiveExecWorkerHelper
	private class NonInteractiveExecWorkerHelper extends ExecWorkerHelper
	{
		protected final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea.getStyledDocument());
		protected final String cmdName;
		protected final String path;
		
		public NonInteractiveExecWorkerHelper(String cmdName, String path)
		{
			super(true, true, true);
			this.cmdName = cmdName;
			this.path = path;
		}
		
		public Thread stdoutHandlerSetup(InputStream stdout)
		{
			return new ReaderConduitTest(new BufferedReader(new InputStreamReader(stdout)), new SimpleWriter()
			// return new ReaderConduit(stdout, new SimpleWriter()
			{
				public void write(String str)
				{
					compileConsoleDoc.writeRegular(str);
				}
			});
		}
		
		public Thread stderrHandlerSetup(InputStream stderr)
		{
			return stdoutHandlerSetup(stderr);
		}
		
		public void finalizer()
		{
			/* Update buttons on the toolpanel */
			toolPanel.setState(toolPanel.NORMAL);
			execWorker = null;
			setKeyboardOutputStream(new NullOutputStream());
			
			if(path != null)
			{
				updateErrorSource(path);
				if(errorSource.getErrorCount() != 0)
					registerErrorSource();
			}
		}
		
		public void interruptedExceptionHandler(Exception e)
		{
			/* FIXME: Ehh? */
			compileConsoleDoc.writeError(
				"FIXME: I should deal better with this: " + e + "\n");
			throw new RuntimeException(e);
		}

		public void ioHandlerExceptionHandler(Exception e)
		{
			/* FIXME: Ehh? */
			compileConsoleDoc.writeError(
				"FIXME: I should deal better with this: " + e + "\n");
			throw new RuntimeException(e);
		}
		
		public void cannotExec(Exception e)
		{
			compileConsoleDoc.writeError("Error while running " + cmdName + ": " + e);
		}
		
		public void cmdExited(int status)
		{
			if(status != 0)
			{
				compileConsoleDoc.writeError(
					cmdName + " exited with error code: " + status + "\n");
			} else if (cmdName.equals("skroc")) {
				// We probably don't want to be explicit about what succeded if it was a compile.
				compileConsoleDoc.writeOK("Compilation Succeeded\n");
			} else {
				// This is a generic OK message, for the other things that use this handler.
				compileConsoleDoc.writeOK(cmdName + " completed sucessfully\n");
			}
		}
	}
	//}}}

	//{{{ private class InteractiveExecWorkerHelper
	private class InteractiveExecWorkerHelper extends NonInteractiveExecWorkerHelper
	{
		public InteractiveExecWorkerHelper(String s)
		{
			super(s, null);
		}
		
		public Thread stdoutHandlerSetup(InputStream stdout)
		{
			//return new ReaderConduitTest(new BufferedReader(new InputStreamReader(stdout)), new SimpleWriter()
			return new ReaderConduit(new BufferedReader(new InputStreamReader(stdout)), new SimpleWriter()
			{
				public void write(String str)
				{
					compileConsoleDoc.writeRegular(str);
				}
			});
		}
		
		public Thread stdinHandlerSetup(OutputStream stdin)
		{
			setKeyboardOutputStream(new BufferedOutputStream(stdin));
			return null;
		}
	}
	//}}}
	
	private class TerminalExecWorkerHelper extends ExecWorkerHelper
	{
		String cmdName;
		
		public TerminalExecWorkerHelper(String cmdName)
		{
			super(true, true, true);
			this.cmdName = cmdName;
		}
		
		public Thread stdoutHandlerSetup(InputStream stdout)
		{
			return new ReaderConduitTest2(stdout, new SimpleWriter()
			// return new ReaderConduit(stdout, new SimpleWriter()
			{
				public void write(String str)
				{
					terminal.putString(str);
				}
			});
		}
		
		public Thread stderrHandlerSetup(InputStream stderr)
		{
			return stdoutHandlerSetup(stderr);
		}
		
		public void finalizer()
		{
			/* Update buttons on the toolpanel */
			toolPanel.setState(toolPanel.NORMAL);
			execWorker = null;
			setKeyboardOutputStream(new NullOutputStream());
		}
		
		public void interruptedExceptionHandler(Exception e)
		{
			/* FIXME: Ehh? */
			terminal.putString(
				"FIXME: I should deal better with this: " + e + "\r\n");
			throw new RuntimeException(e);
		}

		public void ioHandlerExceptionHandler(Exception e)
		{
			interruptedExceptionHandler(e);
		}
		
		public void cannotExec(Exception e)
		{
			terminal.putString("Error while running " + cmdName + ": " + e);
		}
		
		public void cmdExited(int status)
		{
			if(status != 0)
			{
				terminal.putString(
					cmdName + " exited with error code: " + status + "\r\n");
			} else {
				terminal.putString(cmdName + " completed sucessfully\r\n");
			}
		}
		
		public Thread stdinHandlerSetup(OutputStream stdin)
		{
			setKeyboardOutputStream(new BufferedOutputStream(stdin));
			return null;
		}
	}
	
	private void setVisibleDisplayArea(String s)
	{
		CardLayout cl = (CardLayout)(displayPanel.getLayout());
		cl.show(displayPanel, s);
	}
	
	public void tvmrun(final String filename, final String workingDir) throws IOException
	{
		ArrayList tvmCommand = new ArrayList();
		ArrayList runEnv = new ArrayList();


		String fw_p = MiscUtilities.constructPath(
        MiscUtilities.getParentOfPath(
        MiscUtilities.getParentOfPath(
          OccPlugUtil.pathify(OccPlugUtil.getTvmCmd()))),
        "share/tvm/firmware/tvm-posix.tbc");
		runEnv.add("TVM_FIRMWARE_FILE=" + fw_p);

		tvmCommand.add(OccPlugUtil.pathify(OccPlugUtil.getTvmCmd()));
		tvmCommand.add(filename);
	
		/* Update buttons on the toolpanel */
		toolPanel.setState(toolPanel.RUNNING);
		/* Set the focus to the command window, as we want keystrokes to get there */
		setVisibleDisplayArea("terminal");
		terminalArea.requestFocus();

		terminal.reset(); /* This does not clear the terminal, which is ok */
		terminal.putString("Running: " + filename + "\r\n");

		String[] env = (String[]) runEnv.toArray(new String[1]);
		if(runEnv.size() == 0)
		{
			env = null;
		}
		execWorker = new ExecWorker(
			(String []) tvmCommand.toArray(new String[1]),
			(String []) env,
			new File(workingDir),
			new TerminalExecWorkerHelper(filename));
		
		execWorker.start();
	}
  
	private void setCommandBarEnabled(boolean b)
	{
		final boolean value = b;
		Runnable runner = new Runnable() {
			public void run() {
				commandText.setEnabled(value);
				sendBtn.setEnabled(value);
			}
		};
		SwingUtilities.invokeLater(runner);
	}

	private class SRVConsole implements Runnable {
		private final DocumentWriter 	compileConsole;
		private final byte[] 		bytecode;

		public SRVConsole(DocumentWriter cc, byte[] bc)
		{
			compileConsole = cc;
			bytecode = bc;
		}

		private void handleImage() throws IOException
		{
			byte[] img = srv.readImage();
			DockableWindowManager dwm = view.getDockableWindowManager();
			JComponent dockable = dwm.getDockableWindow("srvCamera");
			SrvCamera cam;
			if (dockable == null) {
				dwm.addDockableWindow("srvCamera");
				cam = (SrvCamera) dwm.getDockableWindow("srvCamera");
			} else {
				cam = (SrvCamera) dockable;
			}
			cam.setImage(img);
		}	

		private void runConsole() throws IOException
		{
			InputStream in = srv.getInputStream();
			final byte[] tail = new byte[8];
			int i = 0;
			int b;

			for (int j = 0; j < tail.length; ++j)
				tail[j] = 0;

			while ((b = in.read()) >= 0) {
				tail[i] = (byte) b;
				compileConsole.writeRegular("" + ((char) b));
				if (b == 'J') {
					final byte[] match = {'#', '#', 'I', 'M', 'J'};
					boolean ok = true;
					int k = tail.length + ((i - match.length) + 1);
					for (int j = 0; j < match.length; j++, k++) {
						ok = ok && (match[j] == tail[k % tail.length]);
					}
					if (ok) {
						/* Junk size byte */
						if ((b = in.read()) < 0) {
							return;
						}
						compileConsole.writeRegular("\n");
						handleImage();
					}
				}	
				i = (i + 1) % tail.length;
			}
		}	

		public void run() 
		{
			final String host = srv.getHost();
			final String port = "" + srv.getPort();

			toolPanel.setState(toolPanel.RUNNING);
			compileConsole.writeRegular("Connecting...\n");
			if (srv != null && srv.connect()) {
				compileConsole.writeRegular("Upload...\n");
				if (srv != null && srv.upload(bytecode)) {
					compileConsole.writeOK("Upload Succeeded\n");
					setCommandBarEnabled(true);
					
					try {
						runConsole();
					} catch (IOException e) {
					} catch (NullPointerException e) { 
					}
					
					compileConsole.writeOK("Disconnected\n");
				} else {
					compileConsole.writeError("Upload failed\n");
				}
				if(srv != null)
					srv.disconnect();
				setCommandBarEnabled(false);
			} else {
				compileConsole.writeError("Unable to connect to Surveyor at " + host + ":" + port + "\n");
			}
			toolPanel.setState(toolPanel.NORMAL);
		}
	}

	public void srvrun(final String filename, final String workingDir) throws IOException
	{
		final String host = jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX + "srvHost");
		final String port = jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX + "srvPort");
		srv = new SRV(host, Integer.parseInt(port));

		final DocumentWriter compileConsole = new DocumentWriter(textArea.getStyledDocument());

		File              file		= new File(workingDir + filename);
		FileInputStream   is		= new FileInputStream(file);
		int               length	= (int) file.length();

		final byte[] bytecode = new byte[length];
		if (is.read(bytecode) < length) {
			throw new IOException("Input shorter than expected.");
		}
		
		(new Thread(new SRVConsole(compileConsole, bytecode))).start();
	}
	
	public void run(String type)
	{
		this.run(type, this.view, this.view.getBuffer());
	}
	
	public void run(String type, org.gjt.sp.jedit.View view, Buffer buffer)
	{
		Process p;
		Runtime r = java.lang.Runtime.getRuntime();
		
		BufferedReader stdout, stderr;
		BufferedWriter stdin;
		
		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea.getStyledDocument());
		
		final String occFile  = buffer.getName();
    final String baseFile = MiscUtilities.getFileNameNoExtension(occFile);
		final String tceFile  = baseFile + ".tce";
		final String tbcFile  = baseFile + ".tbc"; 
		final String srecFile = baseFile + ".srec";
    final String srvFile  = baseFile + ".srv";
		
		/* Check if the buffer contains something which looks valid... */
		if(!occFile.toLowerCase().endsWith(".occ")) 
		{
			/* FIXME: This is slightly wrong, as it is actually .tbc files which are
			 * executed....... does this matter????
			 */
			compileConsoleDoc.writeError("Error: Only occam (.occ) programs can be executed.\n");
			compileConsoleDoc.writeError("       The current buffer does not contain a .occ file!\n");
			
			return;
		}
			
		/* FIXME: Could add some checks too see if the tbc file is out of date here */
		/* FIXME: And if the file has not been saved? */
		
		try
		{
      if (type.equals("Surveyor SRV-1"))
      {
        srvrun(srvFile, buffer.getDirectory());
      }
      else if(type.equals("Desktop"))
			{
				tvmrun(tbcFile, buffer.getDirectory());
			}
			else if(type.equals("Mindstorms RCX"))
			{
				legodl(srecFile, buffer.getDirectory(), -1);
			}
			else if(type.equals("robosim"))
			{
				compileConsoleDoc.writeError(
												 "I don't know how to run/compile this type of target yet (" +
												 type + ")\n");
			}
			else if(type.equals("tvm-dbg"))
			{
				compileConsoleDoc.writeError( 
												 "I don't know how to run/compile this type of target yet (" +
												 type + ")\n");
			}
			else
			{
				compileConsoleDoc.writeRegular(
												 "Unknown target (" +
												 type + ")\n");
			}
		}
		catch(Exception e)
		{
			compileConsoleDoc.writeError("Error while running program: " + e);
			return;
		}
	}
	
	public void occdoc()
	{
		try {
			Buffer b = this.view.getBuffer();
			occdocRun(b.getName(), b.getDirectory());
		}
		catch (Exception e) {
			final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea.getStyledDocument());
			compileConsoleDoc.writeError("Error while running occamdoc: " + e);
			return;
		}
	}
	
	public void occdocRun(final String filename, final String workingDir) throws IOException
	{
		ArrayList occdocCommand = new ArrayList();

		occdocCommand.add(OccPlugUtil.pathify(OccPlugUtil.getOccdocCmd()));
		occdocCommand.add("-d");
		occdocCommand.add(MiscUtilities.getFileNameNoExtension(filename) + "-doc");
		occdocCommand.add(filename);
	
		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea.getStyledDocument());
		compileConsoleDoc.clear();
		compileConsoleDoc.writeRegular("Generating occamdoc for: " + filename + "\n");
			
		execWorker = new ExecWorker(
			(String []) occdocCommand.toArray(new String[1]),
			(String []) null,
			new File(workingDir),
			new NonInteractiveExecWorkerHelper("occamdoc " + filename, null));
		
		execWorker.start();
	}
	
	public static void occdocView(org.gjt.sp.jedit.View view)
	{
		Buffer b = view.getBuffer();

		String file = MiscUtilities.constructPath(MiscUtilities.constructPath(b
				.getDirectory(), MiscUtilities.getFileNameNoExtension(b.getName())
				+ "-doc"), "index.html");

		if (new File(file).isFile())
		{
			OccPlugUtil.openWebBrowser(file);
		} 
		else
		{
			GUIUtilities.message(null, "occPlug.occdoc-not-found", null);
		}
	} 
		 
	public void firmdl()
	{
		firmdl(false);
	}
	
	//{{{ public void firmdl(boolean fast)
	public void firmdl(boolean fast)
	{
		ArrayList firmdlCommand = new ArrayList();
		ArrayList firmdlEnv = new ArrayList();
		
		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea.getStyledDocument());
	
		/* Update buttons on the toolpanel */
		toolPanel.setState(toolPanel.RUNNING);
		
		firmdlCommand.add(OccPlugUtil.pathify(OccPlugUtil.getFirmdlCmd()));
		/*
		 * Bugger me this is shoddy....
		 * But not as shoddy as it used to be....
		 */
		if(!fast)
		{
			firmdlCommand.add("-s");
		}
		/* This is too... */ 
		if(!OccPlugUtil.getLegoTowerPort().equals("DEFAULT"))
		{
			firmdlEnv.add("RCXTTY=" + OccPlugUtil.getLegoTowerPort());
		}	
		/* Other args */
		for(int i = 0; i < OccPlugUtil.getFirmdlArgs().length; i++)
		{
			firmdlCommand.add(new String(OccPlugUtil.getFirmdlArgs()[i]));
		}
		
		compileConsoleDoc.clear();
		compileConsoleDoc.writeRegular("Downloading brickOS firmware\n");
		//compileConsoleDoc.writeRegular(firmdlCommand + " \n");
		writeVerbose(firmdlCommand + " \n", compileConsoleDoc);
		writeVerbose(firmdlEnv + " \n", compileConsoleDoc);
		/* Ha! Java sucks, so you cannot pass an empty array (for the environment
		 * to the exec function, or it dies with a null pointer exception! sucks!
		 */
		String[] x = (String[]) firmdlEnv.toArray(new String[1]);
		if(firmdlEnv.size() == 0)
		{
			x = null;
		}
		execWorker = new ExecWorker(
			(String[]) firmdlCommand.toArray(new String[1]),
			x,
			null, //new File(workingDir),
			new NonInteractiveExecWorkerHelper("firmdl3", null));
		
		execWorker.start();
	}
	//}}}
	
	protected void addStylesToDocument(StyledDocument doc)
	{
		//Initialize some styles.
    Style def = StyleContext.getDefaultStyleContext().
                  getStyle(StyleContext.DEFAULT_STYLE);

    Style regular = doc.addStyle("regular", def);
    StyleConstants.setFontFamily(def, "Courier");
		StyleConstants.setForeground(def, Color.BLACK);
		
		Style s = doc.addStyle("error", regular);
		// FIXME: This is what I want to do:
		//StyleConstants.setForeground(s, ErrorListPlugin.getErrorColor(ErrorSource.ERROR));
		// Though there is no protection modifier on the getErrorColor method, so it is
		// protected, which is a pain in the arse.
		StyleConstants.setForeground(s, Color.RED);
		
		s = doc.addStyle("warning", regular);
	  // FIXME: This is what I want to do:
		//StyleConstants.setForeground(def, ErrorListPlugin.getErrorColor(ErrorSource.WARNING));
		// Though there is no protection modifier on the getErrorColor method, so it is
		// protected, which is a pain in the arse.
		StyleConstants.setForeground(s, Color.ORANGE);
		
		s = doc.addStyle("ok", regular);
		StyleConstants.setForeground(s, Color.GREEN);
	}

	
	//{{{ Class: DocumentWriter
	/**
	 * A convenience class which contains several methods for writing to
	 * a styled document, using the set of styles used by this plugin.
	 * All the methods are thread safe, so that several threads can be
	 * given the same instance of this class, in order to be able to 
	 * safely write to the same styled document.
	 */
	 /* FIXME: I am not quite sure about SWING and threads... I am not being
	  * careful about what I am doing to SWING components here, ie using
		* invokeLater(). Is this right, or redundant? And if I am doing this
		* presumjably methods in this class do not need to be synced???
		*/
	class DocumentWriter
	{
		private final StyledDocument doc;
		
		public DocumentWriter(StyledDocument doc)
		{
			this.doc = doc;
		}
		
		public synchronized void writeRegular(String string)
		{
			write(string, "regular");
		}

		public synchronized void writeWarning(String string)
		{
			write(string, "warning");
		}
		
		public synchronized void writeError(String string)
		{
			write(string, "error");
		}

		public synchronized void writeOK(String string)
		{
			write(string, "ok");
		}
		
		public synchronized void write(final String string, final String type)
		{
			write(string, type, null);
		}
		
		public synchronized void write(final String string, final String type,
			final AttributeSet moreAttributes)
		{
			Runnable doWorkRunnable = new Runnable() 
			{
				public void run() 
				{ 
					try
					{
						Style s = doc.getStyle(type);
						if(moreAttributes != null)
						{
							s = doc.addStyle(null, s);
							s.addAttributes(moreAttributes);
						}
						
						doc.insertString(doc.getLength(), string, s);
					}					
					catch(Exception e)
					{
						throw new RuntimeException(e);
					}								 
				}
			};
			SwingUtilities.invokeLater(doWorkRunnable);
			//Log.log(Log.DEBUG, this, (new Date().toString()) + string);
		}
		
		final String clear = new String( new char[] { 27, '[', '2', 'J' });
		
		public synchronized void clear()
		{
			Runnable doWorkRunnable = new Runnable() 
			{
				public void run() 
				{ 
					try
					{
						/* FIXME!!!!!! */
						textArea.setText("");
						//terminal.putString(clear);
					}					
					catch(Exception e)
					{
						throw new RuntimeException(e);
					}								 
				}
			};
			SwingUtilities.invokeLater(doWorkRunnable);
		}
	}
	//}}}
}
