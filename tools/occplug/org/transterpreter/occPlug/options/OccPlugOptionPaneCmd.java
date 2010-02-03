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
	// private JCheckBox showPath;
	private JTextField	skrocCmdName;
	private JTextField	tvmCmdName;
	private JTextField	firmdlCmdName;
	private JTextField	dllCmdName;
	private JTextField	libPath;
	private JComboBox	legoTowerPort;
	private JCheckBox	saveOnCompile;

	// private FontSelector font;

	public OccPlugOptionPaneCmd() {
		super("occPlugCmds");
	}

	public void _init() {
		/*
		 * showPath = new JCheckBox(jEdit.getProperty(
		 * OccPlugPlugin.OPTION_PREFIX + "show-filepath.title"),
		 * jEdit.getProperty(QuickNotepadPlugin.OPTION_PREFIX +
		 * "show-filepath").equals("true")); addComponent(showPath);
		 */

		/*
		 * pathName = new JTextField(jEdit.getProperty(
		 * QuickNotepadPlugin.OPTION_PREFIX + "filepath")); JButton pickPath =
		 * new JButton(jEdit.getProperty( QuickNotepadPlugin.OPTION_PREFIX +
		 * "choose-file")); pickPath.addActionListener(this);
		 * 
		 * JPanel pathPanel = new JPanel(new BorderLayout(0, 0));
		 * pathPanel.add(pathName, BorderLayout.CENTER); pathPanel.add(pickPath,
		 * BorderLayout.EAST);
		 * 
		 * addComponent(jEdit.getProperty( QuickNotepadPlugin.OPTION_PREFIX +
		 * "file"), pathPanel);
		 */

		/* SKROC COMMAND */

		skrocCmdName = new JTextField(jEdit
				.getProperty(OccPlugPlugin.OPTION_PREFIX + "skrocCmd"));
		JButton skrocCmdPickPath = new JButton(
		// JButton occCmdPickPath = new JButton(jEdit.getProperty(
				// OccPlugPlugin.OPTION_PREFIX + "choose-file"));
				"Choose");
		skrocCmdPickPath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				String[] paths = GUIUtilities.showVFSFileDialog(null, null,
						JFileChooser.OPEN_DIALOG, false);
				if (paths != null) {
					skrocCmdName.setText(paths[0]);
				}
			}
		});

		JPanel skrocCmdPathPanel = new JPanel(new BorderLayout(0, 0));
		skrocCmdPathPanel.add(skrocCmdName, BorderLayout.CENTER);
		skrocCmdPathPanel.add(skrocCmdPickPath, BorderLayout.EAST);

		addComponent("skroc command:", skrocCmdPathPanel);
		// addComponent("skroc command:");
		// addComponent(skrocCmdPathPanel);

		/* TVM COMMAND */

		tvmCmdName = new JTextField(jEdit
				.getProperty(OccPlugPlugin.OPTION_PREFIX + "tvmCmd"));
		JButton tvmCmdPickPath = new JButton(
		// JButton occCmdPickPath = new JButton(jEdit.getProperty(
				// OccPlugPlugin.OPTION_PREFIX + "choose-file"));
				"Choose");
		tvmCmdPickPath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				String[] paths = GUIUtilities.showVFSFileDialog(null, null,
						JFileChooser.OPEN_DIALOG, false);
				if (paths != null) {
					tvmCmdName.setText(paths[0]);
				}
			}
		});

		JPanel tvmCmdPathPanel = new JPanel(new BorderLayout(0, 0));
		tvmCmdPathPanel.add(tvmCmdName, BorderLayout.CENTER);
		tvmCmdPathPanel.add(tvmCmdPickPath, BorderLayout.EAST);

		addComponent("tvm command:", tvmCmdPathPanel);

		/* FIRMDL3 COMMAND */

		firmdlCmdName = new JTextField(jEdit
				.getProperty(OccPlugPlugin.OPTION_PREFIX + "firmdlCmd"));
		JButton firmdlCmdPickPath = new JButton(
		// JButton occCmdPickPath = new JButton(jEdit.getProperty(
				// OccPlugPlugin.OPTION_PREFIX + "choose-file"));
				"Choose");
		firmdlCmdPickPath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				String[] paths = GUIUtilities.showVFSFileDialog(null, null,
						JFileChooser.OPEN_DIALOG, false);
				if (paths != null) {
					firmdlCmdName.setText(paths[0]);
				}
			}
		});

		JPanel firmdlCmdPathPanel = new JPanel(new BorderLayout(0, 0));
		firmdlCmdPathPanel.add(firmdlCmdName, BorderLayout.CENTER);
		firmdlCmdPathPanel.add(firmdlCmdPickPath, BorderLayout.EAST);

		addComponent("firmdl command:", firmdlCmdPathPanel);

		/* DLL COMMAND */

		dllCmdName = new JTextField(jEdit
				.getProperty(OccPlugPlugin.OPTION_PREFIX + "dllCmd"));
		JButton dllCmdPickPath = new JButton(
		// JButton occCmdPickPath = new JButton(jEdit.getProperty(
				// OccPlugPlugin.OPTION_PREFIX + "choose-file"));
				"Choose");
		dllCmdPickPath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				String[] paths = GUIUtilities.showVFSFileDialog(null, null,
						JFileChooser.OPEN_DIALOG, false);
				if (paths != null) {
					dllCmdName.setText(paths[0]);
				}
			}
		});

		JPanel dllCmdPathPanel = new JPanel(new BorderLayout(0, 0));
		dllCmdPathPanel.add(dllCmdName, BorderLayout.CENTER);
		dllCmdPathPanel.add(dllCmdPickPath, BorderLayout.EAST);

		addComponent("dll command:", dllCmdPathPanel);

		/* LEGO TOWER PORT */

		String legoTowerPortOptions[] = { "DEFAULT", "USB", "COM1", "COM2",
				"COM3", "COM4", "COM5", "COM6" };
		legoTowerPort = new JComboBox(legoTowerPortOptions);
		String legoTowerPortSelected = jEdit
				.getProperty(OccPlugPlugin.OPTION_PREFIX + "legoTowerPort");
		/*
		 * for(int i = 0; i < legoTowerPortOptions.length; i++) {
		 * if(legoTowerPortSelected.equals(legoTowerPortOptions) legoTowerPort.
		 * }
		 */
		legoTowerPort.setSelectedItem(legoTowerPortSelected);
		org.gjt.sp.util.Log.log(org.gjt.sp.util.Log.DEBUG, this, "Got: "
				+ legoTowerPortSelected);

		addComponent("Lego tower port:", legoTowerPort);

		/* Lib Path */
		libPath = new JTextField(jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX
				+ "MasterLibraryPath"));
		addComponent("Library path: ", libPath);

		/* SAVE BEFORE COMPILING ? */

		boolean saveOnCompileState = jEdit.getBooleanProperty(
				OccPlugPlugin.OPTION_PREFIX + "saveOnCompile", true);

		saveOnCompile = new JCheckBox(
				"Automatically save source files when compiling");
		saveOnCompile.setSelected(saveOnCompileState);

		addComponent(saveOnCompile);

	}

	public void _save() {

		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "skrocCmd",
				skrocCmdName.getText());
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "tvmCmd", tvmCmdName
				.getText());
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "firmdlCmd",
				firmdlCmdName.getText());
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "dllCmd", dllCmdName
				.getText());
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "MasterLibraryPath",
				libPath.getText());
		/*
		 * jEdit.setBooleanProperty( "options.occPlug.slinker.optimalPrefixing",
		 * slinkerOptimalPrefix.getModel().isSelected());
		 */
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "legoTowerPort",
				(String) legoTowerPort.getSelectedItem());
		jEdit.setBooleanProperty(OccPlugPlugin.OPTION_PREFIX + "saveOnCompile",
				saveOnCompile.isSelected());
		org.gjt.sp.util.Log.log(org.gjt.sp.util.Log.DEBUG, this, "Set: "
				+ legoTowerPort.getSelectedItem());
	}
	// end AbstractOptionPane implementation

}
