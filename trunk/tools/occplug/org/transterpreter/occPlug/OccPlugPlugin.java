package org.transterpreter.occPlug;

/*
 * OccPlugPlugin.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2004-2008 Christian L. Jacobsen, Jon Simpson
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

import org.gjt.sp.jedit.EditPlugin;
import org.transterpreter.occPlug.hosts.BaseHost;

/**
 * The occam Plugin
 * 
 * @author Christian L. Jacobsen
 */
public class OccPlugPlugin extends EditPlugin {
	public static final String	NAME			= "occPlug";
	public static final String	MENU			= "occPlug.menu";
	public static final String	PROPERTY_PREFIX	= "plugin.OccPlugPlugin.";
	public static final String	OPTION_PREFIX	= "options.occPlug.";

	/* Call host-specific startup. Nothing happens
	 * if it isn't implemented for a particular host. */
	public void start() {
		BaseHost bh = BaseHost.getHostObject();
		bh.hostStartup();
	}

	public void stop() {
		// Not used
	}
 }
