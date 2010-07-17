package org.transterpreter.occPlug;

/*
 * OccPlugToolPanel.java
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
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.Iterator;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.RolloverButton;
import org.gjt.sp.jedit.io.VFSManager;
import org.transterpreter.occPlug.OccPlug.DocumentWriter;
import org.transterpreter.occPlug.targets.Targets;
import org.transterpreter.occPlug.targets.support.CompileTarget;

public class OccPlugToolPanel extends JPanel {
	private OccPlug				theOccPlug;
	private JComboBox			target;
	
	final JPanel		options				= new JPanel();

	private AbstractButton		compileBtn, runBtn, stopBtn, clearBtn;

	public static final int		NORMAL				= 1;
	public static final int		RUNNING				= 2;
	public static final int		ALLON				= 3;
	public static final int		ALLOFF				= 4;

	public OccPlugToolPanel(OccPlug thePlug) {
		/* FIXME: Move into occplug or somewhere central */
		final Targets targets = new Targets();
		
		theOccPlug = thePlug;
		final JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);

		/* Save options focus listener*/
		FocusListener saveOptionsFocusListener = new FocusListener() {
			public void focusGained(FocusEvent e) {
				// Not used
			}

			public void focusLost(FocusEvent e) {
				savePreferences();
			}
		};
		
		compileBtn = makeCustomButton("occPlug.compile", new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				stop((CompileTarget) target.getSelectedItem());
				compile((CompileTarget) target.getSelectedItem());
			}
		});
		toolBar.add(compileBtn);

		runBtn = makeCustomButton("occPlug.run", new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				savePreferences();
				run((CompileTarget) target.getSelectedItem());
			}
		});
		toolBar.add(runBtn);

		stopBtn = makeCustomButton("occPlug.stop", new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				stop((CompileTarget) target.getSelectedItem());
			}
		});
		toolBar.add(stopBtn);

		clearBtn = makeCustomButton("occPlug.clearTextArea",
				new ActionListener() {
					public void actionPerformed(ActionEvent evt) {
						OccPlugToolPanel.this.theOccPlug.clearTextArea();
					}
				});
		toolBar.add(clearBtn);

		JLabel compileTargetLabel = new JLabel("Platform: ");
		toolBar.add(compileTargetLabel);

		target = new JComboBox(targets.getAllCompileTargets());
		target.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				updateOptionPane();
			}
		});
		target.addFocusListener(saveOptionsFocusListener);
		String lastSelected = jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX
				+ "compile.target");
		if(lastSelected == null)
		{
			lastSelected = Targets.defaultTargetName;
		}	
		CompileTarget[] allTargets = targets.getAllCompileTargets();
		for (int i = 0; i < allTargets.length; i++) {
			if (lastSelected.equals(allTargets[i].name)) {
				target.setSelectedItem(allTargets[i]);
				break;
			}
		}
		updateOptionPane();
		toolBar.add(target);

		toolBar.add(options);
		
		this.setLayout(new BorderLayout(10, 0));
		this.add(BorderLayout.WEST, toolBar);
		this.setBorder(BorderFactory.createEmptyBorder(0, 0, 3, 10));

		setState(NORMAL);
	}

	protected void stop(CompileTarget selectedItem) {
		// FIXME: Potential race here, workIsRunning may return true, 
		// but worker may stop, before stopWorker is called resulting 
		// in an exception...
		if(theOccPlug.workerIsRunning())
		{
			theOccPlug.stopWorker();
		}
	}

	protected void run(final CompileTarget theTarget) {	

		
//		String filename = "CHANGEME";
//		
//		theOccPlug.setVisibleDisplayArea("terminal");
//		theOccPlug.terminalArea.requestFocus();
//
//		theOccPlug.terminal.reset(); /* This does not clear the terminal, which is ok */
//		theOccPlug.terminal.putString("Running: " + filename + "\r\n");
//		
//		final DocumentWriter output = theOccPlug.new DocumentWriter(theOccPlug
//				.getConsoleDoc());
//		output.clear();
		
		setState(RUNNING);
		theTarget.handler.runProgram(theTarget, new Runnable() {
			public void run() {
				theTarget.handler.setEnabledForCompileOptions(true);
				setState(NORMAL);
			}
		});
	}

	protected void compile(final CompileTarget theTarget) {
		
		theOccPlug.setVisibleDisplayArea("console");
		
		View view = theOccPlug.getView();
		Buffer buffer = view.getBuffer();
		
		final DocumentWriter output = theOccPlug.new DocumentWriter(theOccPlug
				.getConsoleDoc());
		output.clear();
		
		/*
		 * There's now an option to save automatically on compile, which is
		 * enabled by default. if disabled, the user is prompted
		 */
		if (buffer.isDirty() && OccPlugUtil.getSaveOnCompile()) {
			// Save the file, and let the user know we saved the file for
			// them..
			output.writeWarning("Warning: Changed source file saved.\n");

			buffer.save(view, null);
			VFSManager.waitForRequests();
		} else if (buffer.isDirty() && !OccPlugUtil.getSaveOnCompile()) {
			int answer = JOptionPane.showConfirmDialog(view, "Save file?",
					"Save file?", JOptionPane.YES_NO_CANCEL_OPTION);

			if (answer == JOptionPane.CANCEL_OPTION) {
				// Dont complete the compile
				output.writeError("Compilation canceled\n");
				return;
			}

			if (answer == JOptionPane.YES_OPTION) {
				// Save the file, keeping the file name the same
				buffer.save(view, null);
			} else {
				output.writeWarning("WARNING: SOURCE FILE NOT SAVED!!!\n");
			}
		}
		

		setState(ALLOFF);
		theTarget.handler.compileProgram(theTarget, new Runnable() {
			public void run() {
				theTarget.handler.setEnabledForCompileOptions(true);
				setState(NORMAL);
			}
		});
		
	}

	protected void updateOptionPane() {
		CompileTarget t = (CompileTarget) target.getSelectedItem();
		JPanel o = t.handler.getCompileOptions(t);
		options.removeAll();
		if (o != null) options.add(o);
		options.revalidate();
	}

	private AbstractButton makeCustomButton(String name, ActionListener listener) {
		String toolTip = jEdit.getProperty(name.concat(".label"));
		AbstractButton b = new RolloverButton(GUIUtilities.loadIcon(jEdit
				.getProperty(name + ".icon")));
		if (listener != null) {
			b.addActionListener(listener);
			b.setEnabled(true);
		} else {
			b.setEnabled(false);
		}
		b.setToolTipText(toolTip);
		return b;
	}

	private void savePreferences() {
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "compile.target",
				((CompileTarget) target.getSelectedItem()).name);
	}

	public void setState(final int state) {
		/* This might be executed from another thread... so just in case */
		Runnable doWorkRunnable = new Runnable() {
			public void run() {
				switch (state) {
					case RUNNING:
						compileBtn.setEnabled(false);
						runBtn.setEnabled(false);
						stopBtn.setEnabled(true);
						clearBtn.setEnabled(true);
						target.setEnabled(false);
						break;
					case NORMAL:
					case ALLON:
						compileBtn.setEnabled(true);
						runBtn.setEnabled(true);
						stopBtn.setEnabled(true);
						clearBtn.setEnabled(true);
						target.setEnabled(true);
						break;
					case ALLOFF:
						compileBtn.setEnabled(false);
						runBtn.setEnabled(false);
						stopBtn.setEnabled(false);
						clearBtn.setEnabled(false);
						target.setEnabled(false);
						break;
					default:
						throw new RuntimeException("Bad state in setState");
				}
			}
		};
		SwingUtilities.invokeLater(doWorkRunnable);
	}
}
