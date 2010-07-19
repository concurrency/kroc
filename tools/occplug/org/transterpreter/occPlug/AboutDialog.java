/**
 * 
 */
package org.transterpreter.occPlug;

/*
 * AboutDialog.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2004-2010 Christian L. Jacobsen
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

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.EnhancedDialog;

//{{{ private class occPlugAboutDialog extends EnhancedDialog implements ActionListener
public class AboutDialog extends EnhancedDialog implements ActionListener {
	private JButton		ok;

	private JPanel		content;
	private JPanel		versions;

	private JLabel		occPlugVersion;
	private JLabel		transterpreterVersion;
	public AboutDialog(java.awt.Frame frame) {
		super(frame, "occPlug Versions", true);
		setup(frame);
		this.setSize(400, 300);
	}

	private void setup(Component comp) {
		/* Set up the window + button */
		content = new JPanel(new BorderLayout(12, 12));
		content.setBorder(new EmptyBorder(12, 12, 12, 12));
		setContentPane(content);

		ok = new JButton(jEdit.getProperty("common.ok"));
		getRootPane().setDefaultButton(ok);
		ok.addActionListener(this);

		content.add(ok, BorderLayout.SOUTH);

		/* Do the version numbers */
		content.add(new JLabel("occPlug Version Numbers"), BorderLayout.NORTH);
		versions = new JPanel(new GridLayout(2, 2));
		versions.add(new JLabel("occPlug:"));
		occPlugVersion = new JLabel(
				jEdit
						.getProperty("plugin.org.transterpreter.occPlug.OccPlugPlugin.version")
						+ " (Revision: " + Revision.svnRevision + ")");
		versions.add(occPlugVersion);
		// Transterpreter
		versions.add(new JLabel("Transterpreter package:"));
		transterpreterVersion = new JLabel(jEdit
				.getProperty("occPlug.TransterpreterVersion"));
		versions.add(transterpreterVersion);
	
		content.add(versions, BorderLayout.CENTER);
		/* Make everything fit */
		pack();
		setLocationRelativeTo(GUIUtilities.getParentDialog(comp));

		setResizable(false);
		setVisible(true);
	}

	public void actionPerformed(ActionEvent evt) {
		Object source = evt.getSource();
		if (source == ok) {
			ok();
		}
	}

	public void ok() {
		dispose();
	}

	public void cancel() {
		dispose();
	}
}
