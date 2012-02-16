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
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import org.gjt.sp.jedit.jEdit;
import org.transterpreter.occPlug.OccPlug.DocumentWriter;
import org.transterpreter.occPlug.targets.Targets;
import org.transterpreter.occPlug.targets.support.FirmwareTarget;

public class OccPlugFirmwareToolPanel extends JPanel {
	private OccPlug		theOccPlug;
	private JComboBox	target;

	private JButton		uploadButton, doneButton;

	private ArrayList	disableOnDownload	= new ArrayList();

	final JPanel		options				= new JPanel();

	public OccPlugFirmwareToolPanel(OccPlug thePlug) {
		/* FIXME: Move into occplug or somewhere central */
		final Targets targets = new Targets();

		theOccPlug = thePlug;
		final JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);

		/* Focus Listener for option saving */
		final FocusListener saveOptionsFocusListener = new FocusListener() {
			public void focusGained(FocusEvent e) {
				// Not used
			}

			public void focusLost(FocusEvent e) {
				saveOptions();
			}
		};

		/* Upload button */
		uploadButton = new JButton("Upload firmware");
		uploadButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				uploadFirmware((FirmwareTarget) target.getSelectedItem());
			}
		});
		disableOnDownload.add(uploadButton);
		toolBar.add(uploadButton);

		toolBar.add(new JLabel(" for "));

		/* Target */
		target = new JComboBox(targets.getAllFirmwareTargets());
		target.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				updateOptionPane();
			}
		});
		target.addFocusListener(saveOptionsFocusListener);
		String lastSelected = jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX
				+ "firmware.target");
		if (lastSelected != null) {
			FirmwareTarget[] allTargets = targets.getAllFirmwareTargets();
			for (int i = 0; i < allTargets.length; i++) {
				if (lastSelected.equals(allTargets[i].name)) {
					target.setSelectedItem(allTargets[i]);
					break;
				}
			}
		}
		updateOptionPane();
		disableOnDownload.add(target);
		toolBar.add(target);

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

		/* Set up the toolbars */
		this.setLayout(new BorderLayout(10, 0));
		this.add(BorderLayout.WEST, toolBar);
		this.add(BorderLayout.EAST, doneToolbar);
		this.setBorder(BorderFactory.createEmptyBorder(0, 0, 3, 10));
	}

	protected void updateOptionPane() {
		FirmwareTarget t = (FirmwareTarget) target.getSelectedItem();
		JPanel o = t.handler.getFirmwareOptions(t);
		options.removeAll();
		if (o != null) options.add(o);
		options.revalidate();
	}

	protected void uploadFirmware(final FirmwareTarget theTarget) {
		setToolBarEnabled(false);
		theTarget.handler.setEnabledForFirmwareOptions(false);

		final DocumentWriter output = theOccPlug.new DocumentWriter(theOccPlug
				.getConsoleDoc());
		output.clear();

		theTarget.handler.uploadFirmware(theTarget, new Runnable() {
			public void run() {
				setToolBarEnabled(true);
				theTarget.handler.setEnabledForFirmwareOptions(true);
			}
		});
	}

	private void saveOptions() {
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "firmware.target",
				((FirmwareTarget) target.getSelectedItem()).name);
	}

	private void setToolBarEnabled(boolean b) {
		Iterator i = disableOnDownload.iterator();
		while (i.hasNext()) {
			Component item = (Component) i.next();
			item.setEnabled(b);
		}
	}
}
