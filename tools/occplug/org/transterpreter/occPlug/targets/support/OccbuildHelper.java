package org.transterpreter.occPlug.targets.support;

/*
 * OccbuildHelper.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2009-2010 Christian L. Jacobsen
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
import java.util.Enumeration;
import java.util.Hashtable;

import org.gjt.sp.jedit.MiscUtilities;
import org.transterpreter.occPlug.OccPlugUtil;
import org.transterpreter.occPlug.hosts.BaseHost;

public class OccbuildHelper {
	private final static BaseHost host = BaseHost.getHostObject();

	public static String[] makeOccbuildEnvironment(String toolchain)
	{
		return makeOccbuildEnvironment(toolchain, (Hashtable) null);
	}

	public static String[] makeOccbuildEnvironment(String toolchain, String[] additional)
	{
		Hashtable h = new Hashtable();
		for(int i = 0; i < additional.length; i++)
		{
			String s = additional[i];
			int j = s.indexOf('=');
			h.put(s.substring(0, j), s.substring(j + 1));	
		}
		return makeOccbuildEnvironment(toolchain, h);
	}
	
	public static String[] makeOccbuildEnvironment(String toolchain, final Hashtable additional)
	{		
		final Hashtable occbuildEnv = new Hashtable();
		final String bin = getBinPath(toolchain);
		
		occbuildEnv.put("OCC21", MiscUtilities.constructPath(bin, host.getCommandName("occ21")));
		occbuildEnv.put("TCE-DUMP.PL", MiscUtilities.constructPath(bin, host.getCommandName("tce-dump")));
		occbuildEnv.put("PLINKER.PL", MiscUtilities.constructPath(bin, host.getCommandName("plinker")));
		occbuildEnv.put("KROC", MiscUtilities.constructPath(bin, host.getCommandName("kroc")));
		occbuildEnv.put("OCTRAN", MiscUtilities.constructPath(bin, host.getCommandName("tranx86")));
		
		if(additional != null)
		{
			for (Enumeration e = additional.keys() ; e.hasMoreElements() ;) {
				String key = (String) e.nextElement();
				occbuildEnv.put(key, additional.get(key));
			}
		}
		
		final String[] env = new String[occbuildEnv.size()];
		int i = 0;
		for (Enumeration e = occbuildEnv.keys(); e.hasMoreElements() ; i++) {
			String key = (String) e.nextElement();
			env[i] = key + "=" + occbuildEnv.get(key);
		}
		
		return env;
	}
	
	public static String[] makeOccbuildProgramCommand(final String compiler, final String fileName)
	{
		OccbuildOptions options;
		if(compiler.equals("kroc"))
		{
			options = new OccbuildKRoCOptions();
		}
		else if(compiler.equals("tvm"))
		{
			options = new OccbuildTVMOptions();
		}
		else
			throw new RuntimeException("argument 'compiler' must be either 'tvm' or 'kroc'");
		
		return makeOccbuildCommand(options, "--program", fileName);
	}
	
	public static String[] makeOccbuildProgramCommand(final OccbuildOptions options, final String fileName)
	{	
		return makeOccbuildCommand(options, "--program", fileName);
	}
	
	public static String[] makeOccbuildCommand(final OccbuildOptions options, final String command, final String fileName)
	{
		ArrayList occbuildCommand = new ArrayList();
		occbuildCommand.add(OccbuildHelper.getOccbuildPath(options));
		occbuildCommand.addAll(Arrays.asList(options.generateOptions()));
		occbuildCommand.add(command);
		occbuildCommand.add(fileName);
		
		return (String[]) occbuildCommand.toArray(new String[0]);
	}
	
	public static String getOccbuildPath(final OccbuildOptions options)
	{		
		return OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(host.getPath(options.toolchain, "bin"), options.occbuildName));
	}
	
	public static String getBinPath(final String toolchain)
	{
		return OccPlugUtil.pathifyXXX(host.getPath(toolchain, "bin"));
	}
}
