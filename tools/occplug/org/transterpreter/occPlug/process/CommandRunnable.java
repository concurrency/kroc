package org.transterpreter.occPlug.process;

public class CommandRunnable extends Command {

	public final Runnable runnable;

	public CommandRunnable(Runnable runnable)
	{
		this.runnable = runnable;
	}
}
