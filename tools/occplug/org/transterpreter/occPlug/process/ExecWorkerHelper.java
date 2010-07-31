package org.transterpreter.occPlug.process;

/*
 * ExecWorkerHelper.java
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

import java.io.InputStream;
import java.io.OutputStream;

//{{{ class ExecWorkerHelper
public class ExecWorkerHelper {
	private final boolean	stdin;
	private final boolean	stdout;
	private final boolean	stderr;

	public ExecWorkerHelper(boolean stdin, boolean stdout, boolean stderr) {
		this.stdin = stdin;
		this.stdout = stdout;
		this.stderr = stderr;
	}

	public ExecWorkerHelper() {
		this(false, false, false);
	}

	public boolean stdinUsed() {
		return stdin;
	}

	public boolean stdoutUsed() {
		return stdout;
	}

	public boolean stderrUsed() {
		return stderr;
	}

	public void finalizer() {
		// Should be overridden by user
	}

	public void cannotExec(Exception e) {
		// Should be overridden by user
	}

	public void cmdExited(int status) {
		// Should be overridden by user
	}

	public void interruptedExceptionHandler(Exception e) {
		// Should be overridden by user
	}

	public void ioHandlerExceptionHandler(Exception e) {	
		// Should be overridden by user
	}

	public Thread stdoutHandlerSetup(InputStream r) {
		return null;
	}

	public Thread stderrHandlerSetup(InputStream r) {
		return null;
	}

	public OutputStream stdinHandlerSetup(OutputStream r) {
		return null;
	}
}