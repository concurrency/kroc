package org.transterpreter.occPlug.process.helpers;

import java.io.BufferedOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

import org.transterpreter.occPlug.OccPlugToolPanel;
import org.transterpreter.occPlug.process.ExecWorkerHelper;
import org.transterpreter.occPlug.process.ReaderConduitTest2;
import org.transterpreter.occPlug.process.SimpleWriter;
import org.transterpreter.occPlug.process.helpers.NullOutputStream;

import de.mud.terminal.vt320;

public class TerminalExecWorkerHelper extends ExecWorkerHelper {
	private String	    cmdName;
	private vt320       terminal;
	private Runnable[]	finalisers;

	public TerminalExecWorkerHelper(String cmdName, vt320 terminal, Runnable[] finalisers) {
		super(true, true, true);
		this.cmdName    = cmdName;
		this.terminal   = terminal;
		this.finalisers = finalisers;
	}

	public Thread stdoutHandlerSetup(InputStream stdout) {
		return new ReaderConduitTest2(stdout, new SimpleWriter()
		// return new ReaderConduit(stdout, new SimpleWriter()
				{
					public void write(String str) {
						terminal.putString(str);
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
		/* FIXME: Ehh? */
		terminal.putString("FIXME: I should deal better with this: " + e
				+ "\r\n");
		throw new RuntimeException(e);
	}

	public void ioHandlerExceptionHandler(Exception e) {
		interruptedExceptionHandler(e);
	}

	public void cannotExec(Exception e) {
		terminal.putString("Error while running " + cmdName + ": " + e);
	}

	public void cmdExited(int status) {
		if (status != 0) {
			terminal.putString(cmdName + " exited with error code: "
					+ status + "\r\n");
		} else {
			terminal.putString(cmdName + " completed sucessfully\r\n");
		}
	}

	public OutputStream stdinHandlerSetup(OutputStream stdin) {
		return new BufferedOutputStream(stdin);
	}
}