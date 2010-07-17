package org.transterpreter.occPlug.targets;

/*
 * Surveyor.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2009 Christian L. Jacobsen, Jon Simpson, Carl Ritson
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

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.gjt.sp.jedit.jEdit;
import org.transterpreter.occPlug.OccPlugPlugin;
import org.transterpreter.occPlug.targets.support.BaseTarget;
import org.transterpreter.occPlug.targets.support.CompileAbility;
import org.transterpreter.occPlug.targets.support.CompileTarget;

/* FIXME: This this target is incomplete. The compile options used 
 * in the old occPlug for the surveyor was these:
 * 				occbuildCommand.add("--blackfin");
 *				occbuildCommand.add("--no-std-libs");
 *				occbuildCommand.add("-f");
 *				occbuildCommand.add(srvFile);
 * However, they may not work with the newer occbuild based build 
 * system...
 */

public class Surveyor extends BaseTarget implements CompileAbility {

	private final CompileTarget[]	compileTargets		= {
			new CompileTarget("Surveyor SRV-1", this)};
	private JPanel	surveyorOptions;
	private JTextField	host;
	private JTextField	port;

	public CompileTarget[] getCompileTargets() {
		return compileTargets;
	}

	public JPanel getCompileOptions(CompileTarget target) {
		/* Only make the options once */
		if (surveyorOptions != null) return surveyorOptions;

		/* Focus Listener for option saving */
		final FocusListener saveCompileOptionsFocusListener = new FocusListener() {
			public void focusGained(FocusEvent e) {
				// Not used
			}

			public void focusLost(FocusEvent e) {
				saveCompileOptions();
			}
		};
		
		surveyorOptions = new JPanel();
		surveyorOptions.add(new JLabel(" Host: "));
		host = new JTextField(16);
		host.addFocusListener(saveCompileOptionsFocusListener);
		host.setText(jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX
						+ "compile.targets.surveyor.host"));
		surveyorOptions.add(host);
		surveyorOptions.add(new JLabel(" Port: "));
		port = new JTextField(6);
		port.addFocusListener(saveCompileOptionsFocusListener);
		port.setText(jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX
						+ "compile.targets.surveyor.port"));
		surveyorOptions.add(port);

		return surveyorOptions;
	}

	protected void saveCompileOptions() {	
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "compile.targets.surveyor.host", host.getText());
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "compile.targets.surveyor.port", port.getText());
	}

	public void compileProgram(CompileTarget target, Runnable finished) {
		// TODO Auto-generated method stub
		
	}

	public void runProgram(CompileTarget theTarget, Runnable runnable) {
		// TODO Auto-generated method stub
		
	}

	public void setEnabledForCompileOptions(boolean b) {
		// TODO Auto-generated method stub
		
	}
}
