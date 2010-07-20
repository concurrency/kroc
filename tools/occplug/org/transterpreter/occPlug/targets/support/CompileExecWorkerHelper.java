package org.transterpreter.occPlug.targets.support;

/*
 * CompileExecWorkerHelper.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2010 Christian L. Jacobsen
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

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;

import org.transterpreter.occPlug.OccPlug;
import org.transterpreter.occPlug.OccPlug.DocumentWriter;
import org.transterpreter.occPlug.OccPlug.ErrorSet;
import org.transterpreter.occPlug.process.ExecWorkerHelper;
import org.transterpreter.occPlug.process.ReaderConduit;
import org.transterpreter.occPlug.process.ReaderConduitTest;
import org.transterpreter.occPlug.process.SimpleWriter;

import errorlist.ErrorSource;

public class CompileExecWorkerHelper extends ExecWorkerHelper {
	protected final OccPlug.DocumentWriter	output;
	protected final String					cmdName;
	protected final Runnable[]				finalisers;

	public CompileExecWorkerHelper(String cmdName, DocumentWriter output,
			Runnable[] finalisers) {
		super(true, true, true);
		this.cmdName = cmdName;
		this.finalisers = finalisers;
		this.output = output;
	}

	public Thread stdoutHandlerSetup(InputStream stdout) {
		return new ReaderConduit(new BufferedReader(new InputStreamReader(
				stdout)), new SimpleWriter() {
			public void write(String str) {
				ErrorSet errStatus = OccPlug.getOccPlugInstance().checkForErrors(str);
				if(errStatus != null)
				{
					if(errStatus.error.type == ErrorSource.WARNING)
					{
						output.writeWarning(str);
					}
					else
					{
						output.writeError(str);
					}
				}
				else
				{
					output.writeRegular(str);
				}
			}
		});
	}

	public Thread stderrHandlerSetup(InputStream stderr) {
		return stdoutHandlerSetup(stderr);
	}

	public void finalizer() {
		for (int i = 0; i < finalisers.length; i++) {
			finalisers[i].run();
		}
	}

	public void interruptedExceptionHandler(Exception e) {
		ioHandlerExceptionHandler(e);
	}

	public void ioHandlerExceptionHandler(Exception e) {
		output.writeError("Unexpected error: " + e + "\n");
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		e.printStackTrace(new PrintStream(b));
		output.writeError(b.toString());
		throw new RuntimeException(e);
	}

	public void cannotExec(Exception e) {
		output.writeError("Error while running " + cmdName + ": " + e + "\n");
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		e.printStackTrace(new PrintStream(b));
		output.writeError(b.toString());
	}

	public void cmdExited(int status) {
		if (status != 0) {
			output.writeError(cmdName + " exited with error code: " + status
					+ "\n");
		} else {
			output.writeOK(cmdName + " completed sucessfully\n");
		}
	}
}