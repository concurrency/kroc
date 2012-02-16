package org.transterpreter.occPlug.process.helpers;

import java.io.OutputStream;

public class NullOutputStream extends OutputStream {
	public void write(int b) {
		/* Discard the input */
	}
}
