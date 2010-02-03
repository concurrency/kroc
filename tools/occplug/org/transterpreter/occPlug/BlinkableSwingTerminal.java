package org.transterpreter.occPlug;

/*
 * BlinkableSwingTerminal.java
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2004-2007 Christian L. Jacobsen
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

import java.awt.Color;
import java.awt.Font;

import de.mud.terminal.SwingTerminal;
import de.mud.terminal.VDUBuffer;

public class BlinkableSwingTerminal extends SwingTerminal {

	/* These are private in swingterminal, how stupid */
	/* definitions of standards for the display unit */
	private final static int	COLOR_FG_STD	= 7;
	private final static int	COLOR_BG_STD	= 0;

	/**
	 * Create a new video display unit with the passed width and height in
	 * characters using a special font and font size. These features can be set
	 * independently using the appropriate properties.
	 * 
	 * @param buffer
	 *            a VDU buffer to be associated with the display
	 * @param font
	 *            the font to be used (usually Monospaced)
	 */
	public BlinkableSwingTerminal(VDUBuffer buffer, Font font) {
		super(buffer, font);
	}

	/**
	 * Create a display unit with size 80x24 and Font "Monospaced", size 12.
	 */
	public BlinkableSwingTerminal(VDUBuffer buffer) {
		super(buffer);
	}

	/* This clearly did not work like I thought it would */
	public void blink() {
		Color[] original = getColorSet();
		Color fg = original[COLOR_FG_STD];
		Color bg = original[COLOR_BG_STD];

		// Color[] invert = new Color[original.length];
		// System.arraycopy(original, 0, invert, 0, original.length);

		// invert[COLOR_FG_STD] = bg;
		// invert[COLOR_BG_STD] = fg;

		/* Invert the colours */
		setForeground(bg);
		setBackground(fg);
		// setColorSet(invert);
		getVDUBuffer().update[0] = true;
		redraw();

		/* Sleep for a bit */
		try {
			Thread.sleep(100);
		} catch (InterruptedException e) {
			/* We dont care... */
		}

		/* Set the colours back to normal */
		setForeground(fg);
		setBackground(bg);
		// setColorSet(original);
		getVDUBuffer().update[0] = true;
		redraw();
	}
}
