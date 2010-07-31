package org.transterpreter.occPlug.process;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

import javax.swing.JLabel;


class CaptureExecWorkerHelper extends ExecWorkerHelper {
	public final ArrayList	stdOutText	= new ArrayList();
	public final ArrayList	stdErrText	= new ArrayList();
	public final JLabel		label;

	public CaptureExecWorkerHelper(JLabel label) {
		super(false, true, true);
		this.label = label;
	}

	public Thread stdoutHandlerSetup(InputStream stdout) {
		return new ReaderConduit(new BufferedReader(new InputStreamReader(
				stdout)), new SimpleWriter() {
			public void write(String str) {
				stdOutText.add(str);
			}
		});
	}

	public Thread stderrHandlerSetup(InputStream stderr) {
		return new ReaderConduit(new BufferedReader(new InputStreamReader(
				stderr)), new SimpleWriter() {
			public void write(String str) {
				stdErrText.add(str);
			}
		});
	}

	public void interruptedExceptionHandler(Exception e) {
		label.setText("An Interrupted Exception occured" + e);
	}

	public void ioHandlerExceptionHandler(Exception e) {
		label.setText("An IO Exception occured" + e);
	}

	public void cannotExec(Exception e) {
		label.setText("Could not execute command");
	}
}