package org.transterpreter.occPlug.process;

/*
 * ExecWorker.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2004-2007 Christian L. Jacobsen
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

//}}}

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
	private Process					p;
	private final Runtime			r		= java.lang.Runtime.getRuntime();
	private boolean					killed	= false;

	private final ExecWorkerHelper	helper;

	private final String[]			cmd;
	private final String[]			envp;
	private final File				dir;

	public ExecWorker(String[] cmd, String[] envp, File dir,
			ExecWorkerHelper helper) {
		this.cmd = cmd;
		this.envp = envp;
		this.dir = dir;

		this.helper = helper;
	}

	public void run() {
		Thread stdin = null;
		Thread stdout = null;
		Thread stderr = null;

		/* Execute the command */
		try {
			p = r.exec(cmd, envp, dir);
		} catch (Exception e) {
			helper.cannotExec(e);
			helper.finalizer();
		}

		/* Set up all the helpers for dealing with IO */
		try {
			// STDIN
			if (helper.stdinUsed()) {
				stdin = helper.stdinHandlerSetup(p.getOutputStream());
				if (stdin != null) {
					stdin.start();
				}
			} else {
				p.getOutputStream().close();
			}
			// STDOUT
			if (helper.stdoutUsed()) {
				stdout = helper.stdoutHandlerSetup(p.getInputStream());
				if (stdout != null) {
					stdout.start();
				}
			} else {
				p.getInputStream().close();
			}
			// STDERR
			if (helper.stderrUsed()) {
				stderr = helper.stderrHandlerSetup(p.getErrorStream());
				if (stderr != null) {
					stderr.start();
				}
			} else {
				p.getErrorStream().close();
			}
		} catch (IOException e) {
			helper.ioHandlerExceptionHandler(e);
		}

		/* Wait for the execution of the external command to finish */
		try {
			p.waitFor();
			if (stdin != null) {
				stdin.join();
			}
			if (stdout != null) {
				stdout.join();
			}
			if (stderr != null) {
				stderr.join();
			}
		} catch (InterruptedException e) {
			helper.interruptedExceptionHandler(e);
		}

		/* We're done */
		helper.cmdExited(p.exitValue());
		helper.finalizer();
	}

	public synchronized void kill() {
		if (!killed) {
			p.destroy();
			killed = true;
		}
	}

	public synchronized boolean wasKilled() {
		return killed;
	}
}