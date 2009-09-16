package org.transterpreter.occPlug;

/*
 * OccPlugFirmwareToolPanel.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2004-2009 Christian L. Jacobsen, Jon Simpson
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
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import org.gjt.sp.jedit.jEdit;
import org.transterpreter.occPlug.process.ExecWorker;
import org.transterpreter.occPlug.process.ExecWorkerHelper;
import org.transterpreter.occPlug.process.ReaderConduitTest;
import org.transterpreter.occPlug.process.SimpleWriter;

public class OccPlugFirmwareToolPanel extends JPanel {
	private OccPlug				theOccPlug;
	private JComboBox			downloadTarget;
	private JComboBox			arduinoPort;

	private JButton				downloadButton, doneButton;

	private ArrayList			disableOnDownload	= new ArrayList();

	public static final int		NORMAL				= 1;
	public static final int		RUNNING				= 2;
	public static final int		ALLON				= 3;
	public static final int		ALLOFF				= 4;

	public static final String	firmwareTargets[]	= { "Arduino (and compatible)",
													// "Surveyor SRV-1",
													// "Mindstorms RCX"
													};

	/*
	 * public static final int SRV_IDX = 0; public static final int DESKTOP_IDX
	 * = 1; public static final int RCX_IDX = 2;
	 */

	public OccPlugFirmwareToolPanel(OccPlug thePlug) {
		theOccPlug = thePlug;
		final JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);
		final JPanel options = new JPanel(new CardLayout());

		/* Focus Listener for option saving */
		final FocusListener saveOptionsFocusListener = new FocusListener() {
			public void focusGained(FocusEvent e) {
				// Not used
			}

			public void focusLost(FocusEvent e) {
				saveOptions();
			}
		};

		/* Download button */
		downloadButton = new JButton("Download firmware");
		downloadButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent evt) {
				uploadArduinoFirmware();
			}
		});
		downloadButton.setEnabled(true);
		disableOnDownload.add(downloadButton);
		toolBar.add(downloadButton);

		toolBar.add(new JLabel(" for "));

		/* Target */
		downloadTarget = new JComboBox(firmwareTargets);
		downloadTarget.addFocusListener(saveOptionsFocusListener);
		disableOnDownload.add(downloadTarget);
		toolBar.add(downloadTarget);

		/* Options */
		toolBar.add(options);

		/* Done button */
		doneButton = new JButton("Done");
		doneButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				theOccPlug.hideFirmwareDownload();
			}
		});
		JToolBar doneToolbar = new JToolBar();
		doneToolbar.setFloatable(false);
		disableOnDownload.add(doneButton);
		doneToolbar.add(doneButton);

		/* Arduino options */
		JPanel arduinoOptions = new JPanel();
		arduinoOptions.add(new JLabel(" Port: "));
		final SortedSet arduinoPortItems = new TreeSet();
		arduinoPort = new JComboBox();
		arduinoPort.setEditable(true);
		PopupMenuListener arduinoPortPopupListener = new PopupMenuListener() {
			public void popupMenuCanceled(PopupMenuEvent e) {
				// Not used
			}

			public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
				// Not used
			}

			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
				FilenameFilter filter = new FilenameFilter() {
					public boolean accept(File dir, String name) {
						return (name.startsWith("tty.usbserial-")
								|| name.equals("ttys0") || name.equals("ttys1")
								|| name.equals("ttys2") || name.equals("ttys3"));
					}
				};
				File dir = new File("/dev");
				String[] devices = dir.list(filter);

				for (int i = 0; i < devices.length; i++) {
					if (arduinoPortItems.add("/dev/" + devices[i])) {
						arduinoPort.addItem("/dev/" + devices[i]);
					}
				}
			}
		};
		arduinoPortPopupListener.popupMenuWillBecomeVisible(null);
		arduinoPort.addPopupMenuListener(arduinoPortPopupListener);
		arduinoPort.addFocusListener(saveOptionsFocusListener);
		arduinoPort.setSelectedItem(jEdit
				.getProperty(OccPlugPlugin.OPTION_PREFIX + "arduino.port"));
		disableOnDownload.add(arduinoPort);
		arduinoOptions.add(arduinoPort);
		options.add(arduinoOptions, "arduino");

		/* Set up the toolbars */
		this.setLayout(new BorderLayout(10, 0));
		this.add(BorderLayout.WEST, toolBar);
		this.add(BorderLayout.EAST, doneToolbar);
		this.setBorder(BorderFactory.createEmptyBorder(0, 0, 3, 10));
	}

	private void saveOptions() {
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "arduino.port",
				(String) arduinoPort.getSelectedItem());
	}

	private void setToolBarEnabled(boolean b) {
		Iterator i = disableOnDownload.iterator();
		while (i.hasNext()) {
			Component item = (Component) i.next();
			item.setEnabled(b);
		}
	}

	private void uploadArduinoFirmware() {
		final OccPlug.DocumentWriter compileConsoleDoc = theOccPlug.new DocumentWriter(
				theOccPlug.getConsoleDoc());

		setToolBarEnabled(false);
		compileConsoleDoc.clear();

		String port = (String) arduinoPort.getSelectedItem();
		if (port == null || port.trim().equals("")) {
			compileConsoleDoc.writeError("Please specify a port");
			setToolBarEnabled(true);
			return;
		}

		ArrayList firmdlCommand = new ArrayList();
		firmdlCommand.add(OccPlugUtil.pathifyXXX("bin/avrdude"));
		firmdlCommand.add("-C");
		firmdlCommand.add(OccPlugUtil.pathifyXXX("lib/avrdude.conf"));
		firmdlCommand.add("-U");
		firmdlCommand.add("flash:w:"
				+ OccPlugUtil.pathifyXXX("lib/tvm-arduino.hex"));
		firmdlCommand.add("-V");
		firmdlCommand.add("-F");
		firmdlCommand.add("-P");
		firmdlCommand.add(arduinoPort.getSelectedItem());
		// FIXME: Need a sensible way of setting these
		firmdlCommand.add("-c");
		firmdlCommand.add("stk500v1");
		firmdlCommand.add("-p");
		firmdlCommand.add("atmega328p");
		firmdlCommand.add("-b");
		firmdlCommand.add("57600");

		compileConsoleDoc.writeRegular("Downloading Plumbing firmware\n");
		theOccPlug.writeVerbose(firmdlCommand + " \n", compileConsoleDoc);

		ExecWorker execWorker = new ExecWorker((String[]) firmdlCommand
				.toArray(new String[1]), null, null, // new File(workingDir),
				new FirmwareDownloadExecWorkerHelper("firmware download", null));

		execWorker.start();
	}

	// FIXME: This thing is probably too much like:
	// NonInteractiveExecWorkerHelper in OccPlug.java
	class FirmwareDownloadExecWorkerHelper extends ExecWorkerHelper {
		protected final OccPlug.DocumentWriter	compileConsoleDoc	= theOccPlug.new DocumentWriter(
																			theOccPlug
																					.getConsoleDoc());
		protected final String					cmdName;
		protected final String					path;

		public FirmwareDownloadExecWorkerHelper(String cmdName, String path) {
			super(true, true, true);
			this.cmdName = cmdName;
			this.path = path;
		}

		public Thread stdoutHandlerSetup(InputStream stdout) {
			return new ReaderConduitTest(new BufferedReader(
					new InputStreamReader(stdout)), new SimpleWriter() {
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
			setToolBarEnabled(true);
		}

		public void interruptedExceptionHandler(Exception e) {
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
			} else {
				compileConsoleDoc.writeOK(cmdName + " completed sucessfully\n");
			}
		}
	}
}
