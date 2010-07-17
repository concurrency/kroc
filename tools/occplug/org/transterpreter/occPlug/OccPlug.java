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

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
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
import org.transterpreter.occPlug.process.helpers.NullOutputStream;

import de.mud.terminal.vt320;
import errorlist.DefaultErrorSource;
import errorlist.ErrorSource;

public class OccPlug extends JPanel implements EBComponent {
	private org.gjt.sp.jedit.View		view;
	private boolean						floating;
	private OccPlugToolPanel			toolPanel;
	private OccPlugFirmwareToolPanel	firmwareToolPanel;
	private JPanel						toolPanelCardContainer;
	/*
	 * BUG: There seems to be a problem with this component, ie when adding a
	 * very long line to it, it can make the application hang, I am not sure if
	 * this is related to bug 4180751, though this one has been closed and
	 * fixed! I cannot find anything else related :(
	 */
	/* This is the compile console really, FIXME: rename this variable */
	private JTextPane					textArea;
	private JScrollPane					textAreaPane;
	/* This is where the running programs will display themselves */
	private vt320						terminal;
	private BlinkableSwingTerminal		terminalArea;
	/* The cardlayout where the two terminals live */
	private JPanel						displayPanel;

	private boolean						errorSourceRegistered	= false;
	private DefaultErrorSource			errorSource;

	private Pattern						errorPattern;
	private Pattern						warningPattern;
	private Pattern						undeclaredPattern;

	private Pattern						libaryPathPattern;

	private HashMap						provisionalErrorList	= new HashMap();

	private ExecWorker					execWorker				= null;

	private SRV							srv						= null;

	private JTextField					commandText;
	private JButton						sendBtn;
	
	private static OccPlug              theOccPlug              = null;

	private class ErrorSet {
		public ErrorKey	key;
		public Error	error;
	}

	private class ErrorKey {
		public int		lineNo;
		public String	fileName;

		public ErrorKey(int lineNo, String fileName) {
			this.lineNo = lineNo;
			this.fileName = fileName;
		}

		public int hashCode() {
			return lineNo + fileName.hashCode();
		}

		public boolean equals(Object other) {
			if (other instanceof ErrorKey) {
				ErrorKey realOther = (ErrorKey) other;
				if (lineNo != realOther.lineNo) return false;
				return fileName.equals(realOther.fileName);
			}
			return false;
		}
	}

	private class Error {
		public int			type;
		// public String msg;
		public ArrayList	msgs	= new ArrayList(2);

		public Error(int type, String msg) {
			this.type = type;
			msgs.add(msg);
		}

		public void setType(int type) {
			// We are only allowed to upgrade from warning to error
			// if an Error class is already an error, it cannot change
			// back to a warning...
			if (this.type == ErrorSource.WARNING) {
				this.type = type;
			}
		}

		public void addTxt(int type, String msg) {
			setType(type);
			msgs.add(msg);
		}
	}

	private class NullOutputStream extends OutputStream {
		public void write(int b) {
			/* Discard the input */
		}
	}

	public void setExecWorker(ExecWorker ew) {
		execWorker = ew;
	}

	//
	// Constructors
	//

	// {{{ Constructor: OccPlug(org.gjt.sp.jedit.View view, String position)
	public OccPlug(final org.gjt.sp.jedit.View view, final String position) {
		super(new BorderLayout());

		/*
		if (theOccPlug != null)
		{
			throw new RuntimeException("Cannot have multiple instance of OccPlug");
		}
		*/
		theOccPlug = this;
		
		this.view = view;
		this.floating = position.equals(DockableWindowManager.FLOATING);

		/* FIXME: What did this do? */
		if (jEdit.getSettingsDirectory() != null) {
			// obtainSettings();
		}

		this.toolPanel = new OccPlugToolPanel(this);
		this.firmwareToolPanel = new OccPlugFirmwareToolPanel(this);
		this.toolPanelCardContainer = new JPanel(new CardLayout());
		this.toolPanelCardContainer.add(toolPanel, "compileAndRun");
		this.toolPanelCardContainer.add(firmwareToolPanel, "firmwareDownload");
		add(BorderLayout.NORTH, this.toolPanelCardContainer);

		if (floating) this.setPreferredSize(new Dimension(500, 250));

		/* Create the text area where things are going to be displayed */
		textArea = new JTextPane();
		/* We want hyperlinks though */
		textArea.setEditorKit(new StyledLinkEditorKit());
		textArea.setEditable(false);
		/* For getting key strokes */
		textArea.setFocusable(true);
		textArea.setFocusTraversalKeysEnabled(false);
		textArea.addKeyListener(new KeyListener() {
			/** Handle the key typed event from the text field. */
			public void keyTyped(KeyEvent e) {
				// GUIUtilities.message(textArea, e.getKeyChar(), null);
				try {
					OutputStream o = getKeyboardOutputStream();
					o.write(e.getKeyChar());
					o.flush();
				} catch (IOException ex) {
					throw new RuntimeException("Got an error: " + ex);
				}
			}

			/** Handle the key pressed event from the text field. */
			public void keyPressed(KeyEvent e) {
				/* Ignore */
			}

			/** Handle the key released event from the text field. */
			public void keyReleased(KeyEvent e) {
				/* Ignore */
			}
		});
		textArea.addHyperlinkListener(new HyperlinkListener() {
			public void hyperlinkUpdate(HyperlinkEvent event) {
				JEditorPane pane = (JEditorPane) event.getSource();
				HyperlinkEvent.EventType type = event.getEventType();
				Element sourceElement = event.getSourceElement();

				ErrorSet err = (ErrorSet) sourceElement.getAttributes()
						.getAttribute(StyledLinkEditorKit.LINK);

				JEditTextArea ta = view.getTextArea();
				// ta.moveCaretPosition(err.key.lineNo, true);
				// ta.centerCaret();
				ta.scrollTo(err.key.lineNo, 0, true);

				Log.log(Log.DEBUG, this, (new Date().toString())
						+ " Hyperlink: " + err);
			}
		});
		/* Add styles to the document */
		addStylesToDocument(textArea.getStyledDocument());
		/* Add the text area to a scrollpane */
		textAreaPane = new JScrollPane(textArea);
		/* Create the terminal view */
		terminal = new vt320() {
			public void beep() {
				/* Audiable bell */
				Toolkit.getDefaultToolkit().beep();
				/* Visual Bell */
				getTerminalArea().blink();
			}

			public void write(byte[] b) {
				try {
					OutputStream o = getKeyboardOutputStream();
					o.write(b);
					o.flush();
				} catch (IOException e) {
					/* Donno what to do with this */
					throw new RuntimeException(e);
				}
			}

			public void clear() {
				final String clear = new String(
						new char[] { 27, '[', '2', 'J' });

				// VT220 Erase screen
				putString(clear);
			}
		};
		terminalArea = new BlinkableSwingTerminal(terminal);
		/*
		 * Set up the panel which will display either the console or the
		 * terminal
		 */
		displayPanel = new JPanel(new CardLayout());
		displayPanel.add(textAreaPane, "console");
		displayPanel.add(getTerminalArea(), "terminal");
		add(BorderLayout.CENTER, displayPanel);

		/* Command field & send button */
		commandText = new JTextField();
		commandText.setEnabled(false);
		commandText.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				sendBtn.doClick();
				commandText.grabFocus();
			}
		});
		sendBtn = new JButton("Send");
		sendBtn.setEnabled(false);
		sendBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				final DocumentWriter compileConsole = new DocumentWriter(
						textArea.getStyledDocument());
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
							compileConsole
									.writeError("IO Exception when sending command\n");
						}
					}
				};
				(new Thread(runner)).start();
			}
		});
		Container c = Box.createHorizontalBox();
		c.add(new JLabel(" Command: "));
		c.add(commandText);
		c.add(sendBtn);

		// FIXME: Disabled, this is SRV specific and should go into the Surveyor target
		//add(BorderLayout.SOUTH, c);

		// Patterns for finding errors and warnings in occ21 output
		errorPattern = Pattern
				.compile("Error-oc(?:c21)?-(.*?)\\((.*?)\\)- (.*)\n?");
		warningPattern = Pattern
				.compile("Warning-oc(?:c21)?-(.*?)\\((.*?)\\)- (.*)\n?");
		undeclaredPattern = Pattern
				.compile("() undeclared on \"()\" lines ([0-9]*,?)*\n?");

		// Patterns for finding libary pahts in library collections, these can
		// then be inserted into occ21's ISEARCH environment variable
		/* FIXME: This will not work for things with spaces in their names */
		libaryPathPattern = Pattern.compile("-L\\s*(\\S*)");

		// obtainSettings();
	}

	// }}}

	
	public OutputStream getKeyboardOutputStream() {
		if (execWorker != null) {
			return execWorker.getStdin();
		} else {
			return new NullOutputStream();
		}
	}

	//
	// EBComponent implementation
	//

	// {{{ Method: handleMessage(EBMessage message)
	public void handleMessage(EBMessage message) {
		/*
		 * if(message instanceof PropertiesChanged) { obtainSettings(); }
		 */
		/*
		 * if(message instanceof PropertiesChanged) { propertiesChanged(); }
		 * else if(message instanceof EditPaneUpdate) { JEditTextArea textArea =
		 * ((EditPaneUpdate) message).getEditPane().getTextArea();
		 * canvas.updateContourRandom(textArea); }
		 */
	}

	// }}}

	//
	// These JComponent methods provide the appropriate points
	// to subscribe and unsubscribe this object to the EditBus
	//

	// {{{ Method: addNotidy()
	public void addNotify() {
		super.addNotify();
		EditBus.addToBus(this);

		// FIXME: Donno where the best place to do this is...
		errorSource = new DefaultErrorSource("occPlug");
		registerErrorSource();
	}

	// }}}

	// {{{ Method: removeNotify
	public void removeNotify() {
		super.removeNotify();
		EditBus.removeFromBus(this);

		unregisterErrorSource();
	}

	// }}}

	public void registerErrorSource() {
		if (!errorSourceRegistered) {
			ErrorSource.registerErrorSource(errorSource);

			errorSourceRegistered = true;
		}
	}

	public void unregisterErrorSource() {
		if (errorSourceRegistered) {
			ErrorSource.unregisterErrorSource(errorSource);

			errorSourceRegistered = false;
		}
	}

	public StyledDocument getConsoleDoc() {
		return textArea.getStyledDocument();
	}

	public void updateErrorSource(String path) {
		Iterator errors = provisionalErrorList.entrySet().iterator();

		while (errors.hasNext()) {
			Map.Entry item = (Map.Entry) errors.next();
			ErrorKey key = (ErrorKey) item.getKey();
			Error err = (Error) item.getValue();
			/*
			 * errorSource.addError( err.type, MiscUtilities.constructPath(path,
			 * key.fileName), key.lineNo, 0, 0, err.msgs.get(0));
			 */
			DefaultErrorSource.DefaultError newError = new DefaultErrorSource.DefaultError(
					errorSource, err.type, MiscUtilities.constructPath(path,
							key.fileName), key.lineNo, 0, 0, (String) err.msgs
							.get(0));

			if (err.msgs.size() > 1) {
				for (int i = 1; i < err.msgs.size(); i++) {
					newError.addExtraMessage((String) err.msgs.get(i));
				}
			}

			errorSource.addError(newError);
		}
	}

	public synchronized ErrorSet checkForErrors(String str, String path) {
		Matcher m;

		m = errorPattern.matcher(str);
		if (m.matches()) {
			/*
			 * errorSource.addError( ErrorSource.ERROR,
			 * MiscUtilities.constructPath(path, m.group(1)), , 0, 0, "Error: "
			 * + m.group(3));
			 */
			ErrorSet err = new ErrorSet();

			err.key = new ErrorKey(new Integer(m.group(2)).intValue() - 1, m
					.group(1));
			if (provisionalErrorList.containsKey(err.key)) {
				((Error) provisionalErrorList.get(err.key)).addTxt(
						ErrorSource.ERROR, "Error: " + m.group(3));
				err.error = (Error) provisionalErrorList.get(err.key);
			} else {
				err.error = new Error(ErrorSource.ERROR, "Error: " + m.group(3));
				provisionalErrorList.put(err.key, err.error);
			}

			return err;
		}
		m = warningPattern.matcher(str);
		if (m.matches()) {
			/*
			 * errorSource.addError( ErrorSource.WARNING,
			 * MiscUtilities.constructPath(path, m.group(1)), new
			 * Integer(m.group(2)).intValue() - 1, 0, 0, "Warning: " +
			 * m.group(3));
			 */
			ErrorSet err = new ErrorSet();

			err.key = new ErrorKey(new Integer(m.group(2)).intValue() - 1, m
					.group(1));
			if (provisionalErrorList.containsKey(err.key)) {
				((Error) provisionalErrorList.get(err.key)).addTxt(
						ErrorSource.WARNING, "Warning: " + m.group(3));
				err.error = (Error) provisionalErrorList.get(err.key);
			} else {
				err.error = new Error(ErrorSource.WARNING, "Warning: "
						+ m.group(3));
				provisionalErrorList.put(err.key, err.error);
			}

			return err;
		}

		return null;
	}

	public void clearTextArea() {
		/* FIXME: ??? not sure, could be ok */
		textArea.setText("");

		/* Talk VT220 to the terminal... */
		final String clear = new String(new char[] { 27, '[', '2', 'J' });
		final String zero = new String(
				new char[] { 27, '[', '1', ';', '1', 'H' });
		terminal.putString(clear);
		terminal.putString(zero);
	}

	/**
	 * Start a new worker. The worker must not already be running and 
	 * there must not already be a worker running.
	 * 
	 * @param worker
	 */
	public void startWorker(ExecWorker worker)
	{
		if (execWorker != null && execWorker.isAlive())
		{
			throw new RuntimeException(
				"Cannot start already running worker!");
		}
		
		if (execWorker == null || execWorker.isAlive() == false) {
			execWorker = worker;
			execWorker.start();
		} else {
			throw new RuntimeException(
					"Tried to start a worker, but there is already a worker running!");
		}		
	}
	
	/**
	 * Kill an executing worker if any is present
	 */
	public void stopWorker()
	{
		if (execWorker != null) {
			execWorker.kill();
			execWorker = null;
		} else {
			throw new RuntimeException(
					"Tried to stop a running woker, but there are no workers running!");
		}
	}
	
	/**
	 * Test if a worker is currently running.
	 * 
	 * @return true if a worker is currently running, false otherwise.
	 */
	public boolean workerIsRunning()
	{
		if (execWorker == null)
		{
			return false;
		}
	
		return execWorker.isAlive();
	}
	
	/* FIXME: Remove target specific code to target specific class */
	public void stopRunningProcess() {
		if (execWorker != null) {
			execWorker.kill();
		} else if (srv != null) {
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
		} else {
			throw new RuntimeException(
					"Tried to stop a running process, but there are no processes running!");
		}
	}

	// {{{ public int compile(String type, org.gjt.sp.jedit.View view, Buffer
	// buffer)
	public int compile(String type, org.gjt.sp.jedit.View view, Buffer buffer) {
		final long startTime = System.currentTimeMillis();
		/*
		 * Java complains that these may not have been initialised if I dont do
		 * this
		 */
		long skrocStartTime = 0;
		long skrocEndTime = 0;
		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea
				.getStyledDocument());

		final String occFile = buffer.getName();
		final String baseFile = MiscUtilities.getFileNameNoExtension(occFile);
		final String tceFile = baseFile + ".tce";
		final String tbcFile = baseFile + ".tbc";
		final String srecFile = baseFile + ".srec";
		final String srvFile = baseFile + ".srv";
		final String directory = buffer.getDirectory();

		setVisibleDisplayArea("console");

		if (occFile.toLowerCase().endsWith(".occ")) {
			// Get the library arguments
			/*
			 * String slinkerLibs =
			 * OccPlugPlugin.slinkerLibProps.getProperty(toolPanel
			 * .getSLinkerLibsSelection());
			 */

			// Clear the text editor
			textArea.setText("");
			// Clear the provisional error list
			provisionalErrorList.clear();
			// Clear the error source
			errorSource.clear();
			unregisterErrorSource();



			/* Update buttons on the toolpanel */
			toolPanel.setState(OccPlugToolPanel.RUNNING);




			/* FIXME: Test and enable RCX and SRV-1
			if (type.equals("Surveyor SRV-1")) {
				occbuildCommand.add("--blackfin");
				occbuildCommand.add("--no-std-libs");
				occbuildCommand.add("-f");
				occbuildCommand.add(srvFile);
			
			} else if (type.equals("Mindstorms RCX")) {
				occbuildCommand.add("--target");
				occbuildCommand.add("t2");
				occbuildCommand.add("--srec");
				occbuildCommand.add("-f");
				occbuildCommand.add(srecFile);
			} else*/ /*if (type.equals("Desktop")) {
				occbuildCommand.add("--toolchain=tvm");
				occbuildCommand.add("--program");
			} else {
				compileConsoleDoc.writeError("Error: I don't understand '"
						+ type + "' as a platform type!\n");
				return 0;
			}*/


//			execWorker = new ExecWorker((String[]) occbuildCommand
//					.toArray(new String[1]), env,
//					new File(directory),
//					new NonInteractiveExecWorkerHelper("occbuild", buffer
//							.getDirectory()) {
//						public Thread stdoutHandlerSetup(InputStream stdout) {
//							return new ReaderConduit(new BufferedReader(
//									new InputStreamReader(stdout)),
//									new SimpleWriter() {
//										public void write(String str) {
//											/*
//											 * Returns the style in errStatus
//											 * which should be used for
//											 * displaying the error message
//											 */
//											ErrorSet errStatus = checkForErrors(
//													str, directory);
//											String style = "regular";
//											/*
//											 * Check if it was an error (ie
//											 * something other than regular was
//											 * returned
//											 */
//											MutableAttributeSet a = null;
//											/*
//											 * errstatus is null if there was no
//											 * error
//											 */
//											if (errStatus != null) {
//												switch (errStatus.error.type) {
//													case ErrorSource.ERROR:
//														a = new SimpleAttributeSet();
//														a
//																.addAttribute(
//																		StyledLinkEditorKit.LINK,
//																		errStatus);
//														style = "error";
//														break;
//													case ErrorSource.WARNING:
//														a = new SimpleAttributeSet();
//														a
//																.addAttribute(
//																		StyledLinkEditorKit.LINK,
//																		errStatus);
//														style = "warning";
//														break;
//												}
//											}
//											compileConsoleDoc.write(str, style,
//													a);
//										}
//									});
//						}
//					});
//
//			execWorker.start();
//		} else {
//			compileConsoleDoc
//					.writeError("Error: Only occam (.occ) source files can be compiled.\n");
//			compileConsoleDoc
//					.writeError("       The current buffer does not contain a .occ file!\n");
//		}
		}
		return 0;
	}

	public void compile(String type) {
		this.compile(type, this.view, this.view.getBuffer());
	}

	// }}}

	// {{{ class NonInteractiveExecWorkerHelper
	class NonInteractiveExecWorkerHelper extends ExecWorkerHelper {
		protected final DocumentWriter	compileConsoleDoc	= new DocumentWriter(
																	textArea
																			.getStyledDocument());
		protected final String			cmdName;
		protected final String			path;

		public NonInteractiveExecWorkerHelper(String cmdName, String path) {
			super(true, true, true);
			this.cmdName = cmdName;
			this.path = path;
		}

		public Thread stdoutHandlerSetup(InputStream stdout) {
			return new ReaderConduitTest(new BufferedReader(
					new InputStreamReader(stdout)), new SimpleWriter()
			// return new ReaderConduit(stdout, new SimpleWriter()
					{
						public void write(String str) {
							compileConsoleDoc.writeRegular(str);
						}
					});
		}

		public Thread stderrHandlerSetup(InputStream stderr) {
			return stdoutHandlerSetup(stderr);
		}

		public void finalizer() {
			/* Update buttons on the toolpanel */
			toolPanel.setState(toolPanel.NORMAL);
			execWorker = null;

			if (path != null) {
				updateErrorSource(path);
				if (errorSource.getErrorCount() != 0) registerErrorSource();
			}
		}

		public void interruptedExceptionHandler(Exception e) {
			/* FIXME: Ehh? */
			compileConsoleDoc
					.writeError("FIXME: I should deal better with this: " + e
							+ "\n");
			throw new RuntimeException(e);
		}

		public void ioHandlerExceptionHandler(Exception e) {
			/* FIXME: Ehh? */
			compileConsoleDoc
					.writeError("FIXME: I should deal better with this: " + e
							+ "\n");
			throw new RuntimeException(e);
		}

		public void cannotExec(Exception e) {
			compileConsoleDoc.writeError("Error while running " + cmdName
					+ ": " + e);
		}

		public void cmdExited(int status) {
			if (status != 0) {
				compileConsoleDoc.writeError(cmdName
						+ " exited with error code: " + status + "\n");
			} else if (cmdName.equals("skroc")) {
				// We probably don't want to be explicit about what succeded if
				// it was a compile.
				compileConsoleDoc.writeOK("Compilation Succeeded\n");
			} else {
				// This is a generic OK message, for the other things that use
				// this handler.
				compileConsoleDoc.writeOK(cmdName + " completed sucessfully\n");
			}
		}
	}

	// }}}

	
	// }}}

	public void setVisibleDisplayArea(String s) {
		CardLayout cl = (CardLayout) (displayPanel.getLayout());
		cl.show(displayPanel, s);
		cl = (CardLayout) (toolPanelCardContainer.getLayout());
		cl.show(toolPanelCardContainer, "compileAndRun");
	}

	private void setCommandBarEnabled(boolean b) {
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
		private final DocumentWriter	compileConsole;
		private final byte[]			bytecode;

		public SRVConsole(DocumentWriter cc, byte[] bc) {
			compileConsole = cc;
			bytecode = bc;
		}

		private void handleImage() throws IOException {
			byte[] img = srv.readImage();
			DockableWindowManager dwm = getView().getDockableWindowManager();
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

		private void runConsole() throws IOException {
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
					final byte[] match = { '#', '#', 'I', 'M', 'J' };
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

		public void run() {
			final String host = srv.getHost();
			final String port = "" + srv.getPort();

			toolPanel.setState(OccPlugToolPanel.RUNNING);
			compileConsole.writeRegular("Connecting...\n");
			if (srv != null && srv.connect()) {
				compileConsole.writeRegular("Upload...\n");
				if (srv != null && srv.upload(bytecode)) {
					compileConsole.writeOK("Upload Succeeded\n");
					setCommandBarEnabled(true);

					try {
						runConsole();
					} catch (IOException e) {
						// Do nothing
					} catch (NullPointerException e) {
						// Do nothing
					}

					compileConsole.writeOK("Disconnected\n");
				} else {
					compileConsole.writeError("Upload failed\n");
				}
				if (srv != null) srv.disconnect();
				setCommandBarEnabled(false);
			} else {
				compileConsole.writeError("Unable to connect to Surveyor at "
						+ host + ":" + port + "\n");
			}
			toolPanel.setState(OccPlugToolPanel.NORMAL);
		}
	}

	public void srvrun(final String filename, final String workingDir)
			throws IOException {
		final String host = jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX
				+ "srvHost");
		final String port = jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX
				+ "srvPort");
		srv = new SRV(host, Integer.parseInt(port));

		final DocumentWriter compileConsole = new DocumentWriter(textArea
				.getStyledDocument());

		File file = new File(workingDir + filename);
		FileInputStream is = new FileInputStream(file);
		int length = (int) file.length();

		final byte[] bytecode = new byte[length];
		if (is.read(bytecode) < length) {
			throw new IOException("Input shorter than expected.");
		}

		(new Thread(new SRVConsole(compileConsole, bytecode))).start();
	}

	


	public void occdoc() {
		try {
			Buffer b = this.view.getBuffer();
			occdocRun(b.getName(), b.getDirectory());
		} catch (Exception e) {
			final DocumentWriter compileConsoleDoc = new DocumentWriter(
					textArea.getStyledDocument());
			compileConsoleDoc.writeError("Error while running occamdoc: " + e);
			return;
		}
	}

	public void occdocRun(final String filename, final String workingDir)
			throws IOException {
		ArrayList occdocCommand = new ArrayList();

		occdocCommand.add(OccPlugUtil.pathify(OccPlugUtil.getOccdocCmd()));
		occdocCommand.add("-d");
		occdocCommand.add(MiscUtilities.getFileNameNoExtension(filename)
				+ "-doc");
		occdocCommand.add(filename);

		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea
				.getStyledDocument());
		compileConsoleDoc.clear();
		compileConsoleDoc.writeRegular("Generating occamdoc for: " + filename
				+ "\n");

		execWorker = new ExecWorker(
				(String[]) occdocCommand.toArray(new String[1]),
				(String[]) null,
				new File(workingDir),
				new NonInteractiveExecWorkerHelper("occamdoc " + filename, null));

		execWorker.start();
	}

	public static void occdocView(org.gjt.sp.jedit.View view) {
		Buffer b = view.getBuffer();

		String file = MiscUtilities.constructPath(MiscUtilities.constructPath(b
				.getDirectory(), MiscUtilities.getFileNameNoExtension(b
				.getName())
				+ "-doc"), "index.html");

		if (new File(file).isFile()) {
			OccPlugUtil.openWebBrowser(file);
		} else {
			GUIUtilities.message(null, "occPlug.occdoc-not-found", null);
		}
	}

	public void firmdl() {
		firmdl(false);
	}

	// {{{ public void firmdl(boolean fast)
	public void firmdl(boolean fast) {
		ArrayList firmdlCommand = new ArrayList();
		ArrayList firmdlEnv = new ArrayList();

		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea
				.getStyledDocument());

		/* Update buttons on the toolpanel */
		toolPanel.setState(OccPlugToolPanel.RUNNING);

		firmdlCommand.add(OccPlugUtil.pathify(OccPlugUtil.getFirmdlCmd()));
		/*
		 * Bugger me this is shoddy.... But not as shoddy as it used to be....
		 */
		if (!fast) {
			firmdlCommand.add("-s");
		}
		/* This is too... */
		if (!OccPlugUtil.getLegoTowerPort().equals("DEFAULT")) {
			firmdlEnv.add("RCXTTY=" + OccPlugUtil.getLegoTowerPort());
		}
		/* Other args */
		for (int i = 0; i < OccPlugUtil.getFirmdlArgs().length; i++) {
			firmdlCommand.add(new String(OccPlugUtil.getFirmdlArgs()[i]));
		}

		compileConsoleDoc.clear();
		compileConsoleDoc.writeRegular("Downloading brickOS firmware\n");
		// compileConsoleDoc.writeRegular(firmdlCommand + " \n");
		OccPlugUtil.writeVerbose(firmdlCommand + " \n", compileConsoleDoc);
		OccPlugUtil.writeVerbose(firmdlEnv + " \n", compileConsoleDoc);
		/*
		 * Ha! Java sucks, so you cannot pass an empty array (for the
		 * environment to the exec function, or it dies with a null pointer
		 * exception! sucks!
		 */
		String[] x = (String[]) firmdlEnv.toArray(new String[1]);
		if (firmdlEnv.size() == 0) {
			x = null;
		}
		execWorker = new ExecWorker((String[]) firmdlCommand
				.toArray(new String[1]), x, null, // new File(workingDir),
				new NonInteractiveExecWorkerHelper("firmdl3", null));

		execWorker.start();
	}

	// }}}

	protected void addStylesToDocument(StyledDocument doc) {
		// Initialize some styles.
		Style def = StyleContext.getDefaultStyleContext().getStyle(
				StyleContext.DEFAULT_STYLE);

		Style regular = doc.addStyle("regular", def);
		StyleConstants.setFontFamily(def, "Courier");
		StyleConstants.setForeground(def, Color.BLACK);

		Style s = doc.addStyle("error", regular);
		// FIXME: This is what I want to do:
		// StyleConstants.setForeground(s,
		// ErrorListPlugin.getErrorColor(ErrorSource.ERROR));
		// Though there is no protection modifier on the getErrorColor method,
		// so it is
		// protected, which is a pain in the arse.
		StyleConstants.setForeground(s, Color.RED);

		s = doc.addStyle("warning", regular);
		// FIXME: This is what I want to do:
		// StyleConstants.setForeground(def,
		// ErrorListPlugin.getErrorColor(ErrorSource.WARNING));
		// Though there is no protection modifier on the getErrorColor method,
		// so it is
		// protected, which is a pain in the arse.
		StyleConstants.setForeground(s, Color.ORANGE);

		s = doc.addStyle("ok", regular);
		StyleConstants.setForeground(s, Color.GREEN);
	}

	// {{{ Class: DocumentWriter
	/**
	 * A convenience class which contains several methods for writing to a
	 * styled document, using the set of styles used by this plugin. All the
	 * methods are thread safe, so that several threads can be given the same
	 * instance of this class, in order to be able to safely write to the same
	 * styled document.
	 */
	public /*
	 * FIXME: I am not quite sure about SWING and threads... I am not being
	 * careful about what I am doing to SWING components here, ie using
	 * invokeLater(). Is this right, or redundant? And if I am doing this
	 * presumjably methods in this class do not need to be synced???
	 */
	class DocumentWriter {
		private final StyledDocument	doc;

		public DocumentWriter(StyledDocument doc) {
			this.doc = doc;
		}

		public synchronized void writeRegular(String string) {
			write(string, "regular");
		}

		public synchronized void writeWarning(String string) {
			write(string, "warning");
		}

		public synchronized void writeError(String string) {
			write(string, "error");
		}

		public synchronized void writeOK(String string) {
			write(string, "ok");
		}

		public synchronized void write(final String string, final String type) {
			write(string, type, null);
		}

		public synchronized void write(final String string, final String type,
				final AttributeSet moreAttributes) {
			Runnable doWorkRunnable = new Runnable() {
				public void run() {
					try {
						Style s = doc.getStyle(type);
						if (moreAttributes != null) {
							s = doc.addStyle(null, s);
							s.addAttributes(moreAttributes);
						}

						doc.insertString(doc.getLength(), string, s);
					} catch (Exception e) {
						throw new RuntimeException(e);
					}
				}
			};
			SwingUtilities.invokeLater(doWorkRunnable);
			// Log.log(Log.DEBUG, this, (new Date().toString()) + string);
		}

		final String	clear	= new String(new char[] { 27, '[', '2', 'J' });

		public synchronized void clear() {
			Runnable doWorkRunnable = new Runnable() {
				public void run() {
					try {
						/* FIXME!!!!!! */
						textArea.setText("");
						// terminal.putString(clear);
					} catch (Exception e) {
						throw new RuntimeException(e);
					}
				}
			};
			SwingUtilities.invokeLater(doWorkRunnable);
		}
	}

	// }}}

	/********************************
	 * Firmware related GUI methods *
	 ********************************/
	public void showFirmwareDownload() {
		CardLayout cl = (CardLayout) (displayPanel.getLayout());
		cl.show(displayPanel, "console");
		cl = (CardLayout) (toolPanelCardContainer.getLayout());
		cl.show(toolPanelCardContainer, "firmwareDownload");
		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea
				.getStyledDocument());
		compileConsoleDoc.clear();
	}

	public void hideFirmwareDownload() {
		CardLayout cl = (CardLayout) (displayPanel.getLayout());
		cl.show(displayPanel, "console");
		cl = (CardLayout) (toolPanelCardContainer.getLayout());
		cl.show(toolPanelCardContainer, "compileAndRun");
		final DocumentWriter compileConsoleDoc = new DocumentWriter(textArea
				.getStyledDocument());
		compileConsoleDoc.clear();
	}

	public org.gjt.sp.jedit.View getView() {
		return view;
	}
	
	public static OccPlug getOccPlugInstance()
	{
		if(theOccPlug == null)
		{
			throw new RuntimeException("OccPlug has not yet been instantiated");
		}
		
		return theOccPlug;
	}

	public vt320 getTerminal() {
		return terminal;
	}

	public BlinkableSwingTerminal getTerminalArea() {
		return terminalArea;
	}
}
