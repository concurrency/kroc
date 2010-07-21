package org.transterpreter.occPlug.process;

/*
 * ExecWorker.java
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

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.ProcessBuilder;
//}}}
import java.util.Map;

/**
 * A class defining a worker which will run an external process. It will take
 * care of any output and input if desired
 * 
 * There used to be some text like this where I had the first iteration of this
 * code: // Check JCompiler line 558 or thereabouts for insipation // Also
 * possibly just use console tools, but not its window
 */
// {{{ class ExecWorker
public class ExecWorker extends Thread implements Killable {
	private boolean					killed	= false;
	private Process					currentProcess = null;
	
	private final					Command[] commands;
	
	private OutputStream 			stdin	= null;
	private Thread 					stdout	= null;
	private Thread 					stderr	= null;
	
	private final Runnable[]			finalisers;

	public ExecWorker(String[] cmd, String[] envp, File dir,
			ExecWorkerHelper helper) {
		this(new Command(cmd, envp, dir, helper));
	}
	
	public ExecWorker(Command command) {
		this(new Command[] {command});
	}
	
	public ExecWorker(Command[] commands) {
		this(commands, (Runnable[]) null);
	}
	
	public ExecWorker(Command[] commands, Runnable finaliser) {
		this(commands, new Runnable[] { finaliser });
	}
	
	public ExecWorker(Command[] commands, Runnable[] finaliser) {
		this.commands = commands;
		this.finalisers = finaliser;
	}
	
	public void run() {
		for (Command command : commands) {
			ProcessBuilder pb = new ProcessBuilder(command.cmd);
			Map<String, String> env = pb.environment();
			if (command.envp != null) {
				for (int i = 0; i < command.envp.length; ++i) {
					String[] bits = command.envp[i].split("=");
					env.put(bits[0], bits[1]);
				}
			}
			pb.directory(command.dir);

			/* Execute the command */
			try {
				synchronized(this) {
					if(!killed)
					{
						currentProcess = pb.start();
					}
					else
					{
						runFinaliser();
						return;					
					}
				}
			} catch (IOException e) {
				command.helper.cannotExec(e);
				command.helper.finalizer();
				runFinaliser();
				return;
			}

			/* Set up all the helpers for dealing with IO */
			try {
				// STDIN
				if (command.helper.stdinUsed()) {
					stdin = command.helper.stdinHandlerSetup(currentProcess.getOutputStream());
				} else {
					currentProcess.getOutputStream().close();
				}
				// STDOUT
				if (command.helper.stdoutUsed()) {
					stdout = command.helper.stdoutHandlerSetup(currentProcess.getInputStream());
					if (stdout != null) {
						stdout.start();
					}
				} else {
					currentProcess.getInputStream().close();
				}
				// STDERR
				if (command.helper.stderrUsed()) {
					stderr = command.helper.stderrHandlerSetup(currentProcess.getErrorStream());
					if (stderr != null) {
						stderr.start();
					}
				} else {
					currentProcess.getErrorStream().close();
				}
			} catch (IOException e) {
				command.helper.ioHandlerExceptionHandler(e);
				runFinaliser();
				return;				
			}

			/* Wait for the execution of the external command to finish */
			try {
				currentProcess.waitFor();
				if (stdout != null) {
					stdout.join();
				}
				if (stderr != null) {
					stderr.join();
				}
			} catch (InterruptedException e) {
				command.helper.interruptedExceptionHandler(e);
				runFinaliser();
				return;
			}

			/* We're done */
			command.helper.cmdExited(currentProcess.exitValue());
			command.helper.finalizer();
			if(currentProcess.exitValue() != 0)
			{
				runFinaliser();
				return;
			}
		}
		runFinaliser();
		return;
	}

	public synchronized void kill() {
		if (!killed) {
			currentProcess.destroy();
			killed = true;
		}
	}

	public synchronized boolean wasKilled() {
		return killed;
	}
	
	public synchronized OutputStream getStdin() {
		if (!killed) {
			return stdin;
		} else {
			return null;
		}
	}
	
	private void runFinaliser()
	{
		if(finalisers != null)
		{
			for(Runnable finaliser: finalisers)
			{
				finaliser.run();
			}
		}
	}
}