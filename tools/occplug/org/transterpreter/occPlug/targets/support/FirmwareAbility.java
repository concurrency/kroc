package org.transterpreter.occPlug.targets.support;

import javax.swing.JPanel;

import org.transterpreter.occPlug.OccPlug;

public interface FirmwareAbility {

	public FirmwareTarget[] getFirmwareTargets();

	public void uploadFirmware(FirmwareTarget target,
			OccPlug.DocumentWriter output, Runnable finished);

	public JPanel getFirmwareOptions(FirmwareTarget target);

	public void setEnabledForFirmwareOptions(boolean enabled);
}
