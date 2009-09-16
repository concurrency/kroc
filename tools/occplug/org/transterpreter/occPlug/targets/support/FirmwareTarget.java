package org.transterpreter.occPlug.targets.support;

public class FirmwareTarget {
	public final String				name;
	public final FirmwareAbility	handler;

	public FirmwareTarget(String name, FirmwareAbility handler) {
		this.name = name;
		this.handler = handler;
	}

	public String toString() {
		return name;
	}
}
