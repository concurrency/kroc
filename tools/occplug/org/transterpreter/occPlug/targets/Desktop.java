package org.transterpreter.occPlug.targets;

/*
 * Desktop.java
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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.JPanel;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.MiscUtilities;
import org.transterpreter.occPlug.OccPlugUtil;
import org.transterpreter.occPlug.OccPlug.DocumentWriter;
import org.transterpreter.occPlug.process.ExecWorker;
import org.transterpreter.occPlug.targets.support.BaseTarget;
import org.transterpreter.occPlug.targets.support.CompileAbility;
import org.transterpreter.occPlug.targets.support.CompileTarget;
import org.transterpreter.occPlug.targets.support.OccbuildHelper;
import org.transterpreter.occPlug.targets.support.TargetExecWorkerHelper;

public class Desktop extends BaseTarget implements CompileAbility {

	private final CompileTarget   target_kroc    = new CompileTarget("Desktop (KRoC)", this);
	private final CompileTarget   target_tvm     = new CompileTarget("Desktop (TVM)", this);
	private final CompileTarget[] compileTargets = {
			target_tvm,
			target_kroc,
			};

	public CompileTarget[] getCompileTargets() {
		return compileTargets;
	}

	public JPanel getCompileOptions(CompileTarget target) {
		// This target has no options
		return null;
	}

	public void compileProgram(CompileTarget target, Buffer buffer, DocumentWriter output,
			Runnable finished) {

		final String occFile = buffer.getName();
		
		if (!occFile.toLowerCase().endsWith(".occ")) {
			output.writeError("Error: Only occam (.occ) source files can be compiled.\n");
			output.writeError("       The current buffer does not contain a .occ file!\n");
			finished.run();
			return;
		}
	

		String[] occbuildCommand;
		if(target == target_tvm)
			occbuildCommand = OccbuildHelper.makeOccbuildProgramCommand("tvm", occFile);
		else if(target == target_kroc)
			occbuildCommand = OccbuildHelper.makeOccbuildProgramCommand("kroc", occFile);
		else
			throw new RuntimeException("Invalid target passed to compileProgram");
		
		// Say what we are doing
		output.writeRegular("Compiling: " + occFile + "\n");
		OccPlugUtil.writeVerbose(Arrays.asList(occbuildCommand) + "\n", output);

		// Set up the environment
		String[] env = OccbuildHelper.makeOccbuildEnvironment();
		OccPlugUtil.writeVerbose(Arrays.asList(env) + "\n", output);

		final Runnable[] finalisers = { finished };
		ExecWorker execWorker = new ExecWorker(occbuildCommand, env, 
				new File(buffer.getDirectory()),
				new TargetExecWorkerHelper("compile", output,
						finalisers));

		execWorker.start();		
	}

	public void runProgram(CompileTarget theTarget, Buffer buffer,
			DocumentWriter output, Runnable finished) {
		ArrayList runCommand = new ArrayList();
		ArrayList runEnv = new ArrayList();
		
		final String filename;
		if(theTarget == target_tvm)
		{
			filename = MiscUtilities.getFileNameNoExtension(buffer.getName()) + ".tbc";
			
			String fw_p = MiscUtilities.constructPath(MiscUtilities
					.getParentOfPath(MiscUtilities.getParentOfPath(OccPlugUtil
							.pathify(OccPlugUtil.getTvmCmd()))),
					"share/tvm/firmware/tvm-posix.tbc");
			runEnv.add("TVM_FIRMWARE_FILE=" + fw_p);
			String lib_p = MiscUtilities.constructPath(MiscUtilities
					.getParentOfPath(MiscUtilities.getParentOfPath(OccPlugUtil
							.pathify(OccPlugUtil.getTvmCmd()))), "lib");
			runEnv.add("DYLD_LIBRARY_PATH=" + lib_p);
	
			runCommand.add(OccPlugUtil.pathifyXXX("bin/tvm"));
		}
		else if(theTarget == target_kroc)
		{
			filename = MiscUtilities.constructPath(
					buffer.getDirectory(),
					MiscUtilities.getFileNameNoExtension(buffer.getName()));
		}
		else
			throw new RuntimeException("invalid target passed to runProgram");

		runCommand.add(filename);
		
		final Runnable[] finalisers = { finished };
		ExecWorker execWorker = new ExecWorker((String[]) runCommand.toArray(new String[0]), 
				(String[]) runEnv.toArray(new String[0]), 
				new File(buffer.getDirectory()),
				new TargetExecWorkerHelper("run", output,
						finalisers));

		execWorker.start();	
		/* Update buttons on the toolpanel */
		//toolPanel.setState(OccPlugToolPanel.RUNNING);
		/*
		 * Set the focus to the command window, as we want keystrokes to get
		 * there
		 */
		//setVisibleDisplayArea("terminal");
		//terminalArea.requestFocus();

		//terminal.reset(); /* This does not clear the terminal, which is ok */
		//terminal.putString("Running: " + filename + "\r\n");

		/*
		String[] env = (String[]) runEnv.toArray(new String[1]);
		if (runEnv.size() == 0) {
			env = null;
		}
		execWorker = new ExecWorker((String[]) tvmCommand
				.toArray(new String[1]), env, new File(workingDir),
				new TerminalExecWorkerHelper(filename));

		execWorker.start();
		*/
	}
}
