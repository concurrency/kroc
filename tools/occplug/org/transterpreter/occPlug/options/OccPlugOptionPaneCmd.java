package org.transterpreter.occPlug.options;

/*
 * OccPlugOptionPaneCmd.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2004-2007 Christian L. Jacobsen
 * Contributions: Jon Simpson
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.gjt.sp.jedit.AbstractOptionPane;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;
import org.transterpreter.occPlug.OccPlugPlugin;

public class OccPlugOptionPaneCmd extends AbstractOptionPane {

	private JCheckBox	saveOnCompile;
	private JCheckBox	verbose;

	// private FontSelector font;

	public OccPlugOptionPaneCmd() {
		super("occPlugCmds");
	}

	public void _init() {
		/* SAVE BEFORE COMPILING ? */

		boolean saveOnCompileState = jEdit.getBooleanProperty(
				OccPlugPlugin.OPTION_PREFIX + "saveOnCompile", true);
		saveOnCompile = new JCheckBox(
				"Automatically save source files when compiling");
		saveOnCompile.setSelected(saveOnCompileState);
		addComponent(saveOnCompile);

		/* VERBOSE */

		boolean verboseState = jEdit.getBooleanProperty(
				OccPlugPlugin.OPTION_PREFIX + "verbose", true);
		verbose = new JCheckBox(
				"Enable verbose output");
		verbose.setSelected(verboseState);
		addComponent(verbose);
	}

	public void _save() {
		jEdit.setBooleanProperty(OccPlugPlugin.OPTION_PREFIX + "saveOnCompile",
				saveOnCompile.isSelected());
		jEdit.setBooleanProperty(OccPlugPlugin.OPTION_PREFIX + "verbose",
				verbose.isSelected());
	}
	// end AbstractOptionPane implementation

}
