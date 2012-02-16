package org.transterpreter.occPlug.targets;

/*
 * Targets.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2009 Christian L. Jacobsen
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

import java.util.ArrayList;
import java.util.Arrays;

import org.transterpreter.occPlug.targets.support.BaseTarget;
import org.transterpreter.occPlug.targets.support.CompileAbility;
import org.transterpreter.occPlug.targets.support.CompileTarget;
import org.transterpreter.occPlug.targets.support.FirmwareAbility;
import org.transterpreter.occPlug.targets.support.FirmwareTarget;

public class Targets {

	/* Add new targets to this list */
	private final BaseTarget[]	allTargets	= { 
		new Arduino(),
		new Desktop(),
		//new Surveyor()
		};
	/* FIXME: A better way of setting the default, perhaps this is ok? */
	public static final String defaultTargetName = "Desktop (TVM)";

	/************************************************
	 * Nothing below this should need to be altered *
	 * to add a new target                          *
	 ************************************************/

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
		firmwareTargets = (FirmwareAbility[]) f.toArray(new FirmwareAbility[0]);
		compileTargets = (CompileAbility[]) c.toArray(new CompileAbility[0]);
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

		return (FirmwareTarget[]) l.toArray(new FirmwareTarget[0]);
	}

	public CompileTarget[] getAllCompileTargets() {
		ArrayList l = new ArrayList();
		for (int i = 0; i < compileTargets.length; i++) {
			CompileAbility t = compileTargets[i];
			l.addAll(Arrays.asList(t.getCompileTargets()));
		}

		return (CompileTarget[]) l.toArray(new CompileTarget[0]);
	}
}
