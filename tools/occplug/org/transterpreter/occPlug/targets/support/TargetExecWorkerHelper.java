package org.transterpreter.occPlug.targets.support;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.transterpreter.occPlug.OccPlug;
import org.transterpreter.occPlug.OccPlug.DocumentWriter;
import org.transterpreter.occPlug.process.ExecWorkerHelper;
import org.transterpreter.occPlug.process.ReaderConduitTest;
import org.transterpreter.occPlug.process.SimpleWriter;

public class TargetExecWorkerHelper extends ExecWorkerHelper {
	protected final OccPlug.DocumentWriter	output;
	protected final String					cmdName;
	protected final Runnable[]				finalisers;

	public TargetExecWorkerHelper(String cmdName, DocumentWriter output,
			Runnable[] finalisers) {
		super(true, true, true);
		this.cmdName = cmdName;
		this.finalisers = finalisers;
		this.output = output;
	}

	public Thread stdoutHandlerSetup(InputStream stdout) {
		return new ReaderConduitTest(new BufferedReader(new InputStreamReader(
				stdout)), new SimpleWriter() {
			public void write(String str) {
				output.writeRegular(str);
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
		output.writeError("Unexpected error: " + e + "\n");
		throw new RuntimeException(e);
	}

	public void ioHandlerExceptionHandler(Exception e) {
		output.writeError("Unexpected error: " + e + "\n");
		throw new RuntimeException(e);
	}

	public void cannotExec(Exception e) {
		output.writeError("Error while running " + cmdName + ": " + e);
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