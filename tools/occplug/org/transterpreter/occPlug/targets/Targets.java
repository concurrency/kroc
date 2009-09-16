package org.transterpreter.occPlug.targets;

import java.util.ArrayList;
import java.util.Arrays;

import org.transterpreter.occPlug.targets.support.BaseTarget;
import org.transterpreter.occPlug.targets.support.CompileAbility;
import org.transterpreter.occPlug.targets.support.FirmwareAbility;
import org.transterpreter.occPlug.targets.support.FirmwareTarget;

public class Targets {

	/* Add new targets to this list */
	private static final BaseTarget[]	allTargets	= { new Arduino() };

	/*************************************************
	 * Nothing below this should need to be altered * to add a new target *
	 *************************************************/

	private final FirmwareAbility[]		firmwareTargets;
	private final CompileAbility[]		compileTargets;

	public Targets() {
		final ArrayList f = new ArrayList();
		final ArrayList c = new ArrayList();
		for (int i = 0; i < allTargets.length; i++) {
			final BaseTarget t = allTargets[i];
			if (t instanceof FirmwareAbility) f.add(t);
			if (t instanceof CompileAbility) c.add(t);
		}
		firmwareTargets = (FirmwareAbility[]) f.toArray(new FirmwareAbility[1]);
		compileTargets = (CompileAbility[]) f.toArray(new CompileAbility[1]);
	}

	public BaseTarget[] allTargets() {
		return allTargets;
	}

	public FirmwareAbility[] targetsWithFirmware() {
		return firmwareTargets;
	}

	public CompileAbility[] targetsWithCompile() {
		return compileTargets;
	}

	public FirmwareTarget[] getAllFirmwareTargets() {
		ArrayList l = new ArrayList();
		for (int i = 0; i < firmwareTargets.length; i++) {
			FirmwareAbility t = firmwareTargets[i];
			l.addAll(Arrays.asList(t.getFirmwareTargets()));
		}

		return (FirmwareTarget[]) l.toArray(new FirmwareTarget[1]);
	}
}
