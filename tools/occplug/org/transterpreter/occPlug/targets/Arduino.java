/**
 * 
 */
package org.transterpreter.occPlug.targets;

/*
 * Arduino.java
 * part of the occPlug plugin for the jEdit text editor
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

import java.awt.Component;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import org.gjt.sp.jedit.jEdit;
import org.transterpreter.occPlug.OccPlugPlugin;
import org.transterpreter.occPlug.OccPlugUtil;
import org.transterpreter.occPlug.OccPlug.DocumentWriter;
import org.transterpreter.occPlug.process.ExecWorker;
import org.transterpreter.occPlug.targets.support.BaseTarget;
import org.transterpreter.occPlug.targets.support.CompileAbility;
import org.transterpreter.occPlug.targets.support.FirmwareAbility;
import org.transterpreter.occPlug.targets.support.FirmwareTarget;
import org.transterpreter.occPlug.targets.support.TargetExecWorkerHelper;

/**
 * @author clj
 * 
 */
public class Arduino extends BaseTarget implements FirmwareAbility,
		CompileAbility {

	private final FirmwareTarget[]	firmwareTargets		= {
			new FirmwareTarget("Arduino (and compatible)", this)};
	private JComboBox				arduinoPort;
	private JPanel					arduinoOptions		= null;
	private ArrayList				disableOnDownload	= new ArrayList();

	public FirmwareTarget[] getFirmwareTargets() {
		return firmwareTargets;
	}

	public JPanel getFirmwareOptions(FirmwareTarget target) {
		/* Only make the options once */
		if (arduinoOptions != null) return arduinoOptions;

		/* Focus Listener for option saving */
		final FocusListener saveFirmwareOptionsFocusListener = new FocusListener() {
			public void focusGained(FocusEvent e) {
				// Not used
			}

			public void focusLost(FocusEvent e) {
				saveFirmwareOptions();
			}
		};

		/* Arduino options */
		arduinoOptions = new JPanel();
		arduinoOptions.add(new JLabel("Port: "));
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
		arduinoPort.addFocusListener(saveFirmwareOptionsFocusListener);
		arduinoPort.getEditor().getEditorComponent().addFocusListener(
				saveFirmwareOptionsFocusListener);
		arduinoPort.addPopupMenuListener(arduinoPortPopupListener);
		arduinoPort.setSelectedItem(jEdit
				.getProperty(OccPlugPlugin.OPTION_PREFIX
						+ "firmware.targets.arduino.port"));
		disableOnDownload.add(arduinoPort);
		arduinoOptions.add(arduinoPort);

		return arduinoOptions;
	}

	protected void saveFirmwareOptions() {
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX
				+ "firmware.targets.arduino.port", (String) arduinoPort
				.getSelectedItem());
	}

	public void uploadFirmware(FirmwareTarget target, DocumentWriter output,
			Runnable finished) {
		String port = (String) arduinoPort.getSelectedItem();
		if (port == null || port.trim().equals("")) {
			output.writeError("Please specify a port");
			finished.run();
			return;
		}

		final String[] firmdlCommand = { OccPlugUtil.pathifyXXX("bin/avrdude"),
				"-C", OccPlugUtil.pathifyXXX("lib/avrdude.conf"), "-U",
				"flash:w:" + OccPlugUtil.pathifyXXX("lib/tvm-arduino.hex"),
				"-V", "-F", "-P", (String) arduinoPort.getSelectedItem(),
				// FIXME: Need a sensible way of setting these
				("-c"), "stk500v1", "-p", "atmega328p", "-b", "57600", };

		output.writeRegular("Downloading Plumbing firmware\n");
		OccPlugUtil.writeVerbose(firmdlCommand + " \n", output);

		final Runnable[] finalisers = { finished };
		ExecWorker execWorker = new ExecWorker(firmdlCommand, null, null, // new
																			// File(workingDir),
				new TargetExecWorkerHelper("firmware download", output,
						finalisers));

		execWorker.start();
	}

	public void setEnabledForFirmwareOptions(boolean enabled) {
		Iterator i = disableOnDownload.iterator();
		while (i.hasNext()) {
			Component item = (Component) i.next();
			item.setEnabled(enabled);
		}
	}
}
