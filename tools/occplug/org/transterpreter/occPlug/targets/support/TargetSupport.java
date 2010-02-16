package org.transterpreter.occPlug.targets.support;

import javax.swing.JPanel;

import org.transterpreter.occPlug.OccPlug;
import org.transterpreter.occPlug.OccPlug.DocumentWriter;
import org.transterpreter.occPlug.process.ExecWorker;

import de.mud.terminal.vt320;

public class TargetSupport {
	private final OccPlug occPlug;
	
	public TargetSupport()
	{
		super();
		
		this.occPlug = OccPlug.getOccPlugInstance();
	}
	
	public String getActiveFileName()
	{
		return this.occPlug.getView().getBuffer().getPath();
	}
	
	public String getActiveDirectory()
	{
		return this.occPlug.getView().getBuffer().getDirectory();
	}
	
	public void startWorker(ExecWorker worker)
	{
		this.occPlug.startWorker(worker);
	}
	
	public void stopWorker()
	{
		this.occPlug.stopWorker();
	}
	
	public boolean workerIsRunning()
	{
		return this.occPlug.workerIsRunning();
	}
	
	public DocumentWriter getDefaultOutput()
	{
		return occPlug.new DocumentWriter(occPlug.getConsoleDoc());
	}
	
	public void setVisibleDisplayArea(String name)
	{
		this.occPlug.setVisibleDisplayArea(name);
	}
	
	public void setVisibleDisplayArea(JPanel panel)
	{
		throw new RuntimeException("Not implemented");
	}

	public vt320 getTerminal() {
		return this.occPlug.getTerminal();
	}
	
	public void focusTerminal()
	{
		this.occPlug.getTerminalArea().requestFocus();
	}
	
}
