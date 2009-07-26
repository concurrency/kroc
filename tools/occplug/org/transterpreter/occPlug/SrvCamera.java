package org.transterpreter.occPlug;
/*
 * SrvCamera.java
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


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.EBComponent;
import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.EditBus;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.OperatingSystem;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.DockableWindowManager;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.util.Log;

public class SrvCamera extends JPanel implements EBComponent
{	
	private org.gjt.sp.jedit.View view;
	private boolean floating;

	private JLabel imageLabel;

	public SrvCamera(final org.gjt.sp.jedit.View view, final String position)
	{
		super(new BorderLayout());      

		this.view = view;
		this.floating = position.equals(DockableWindowManager.FLOATING);

		if(floating)
			this.setPreferredSize(new Dimension(320, 256));

		imageLabel = new JLabel();
		imageLabel.setHorizontalAlignment(JLabel.CENTER);
		imageLabel.setVerticalAlignment(JLabel.CENTER);
		add(BorderLayout.CENTER, imageLabel);
	}

	private class Update implements Runnable
	{
		private final byte[] data;

		public Update(byte[] data) { this.data = data; }

		public void run()
		{
			ImageIcon img = new ImageIcon(data);
			int width = img.getIconWidth();
			int height = img.getIconHeight();
			imageLabel.setPreferredSize(new Dimension(width, height));
			setPreferredSize(new Dimension(width + 10, height + 10));
			if (floating) {
				Container c = getParent();
				while(!JFrame.class.isInstance(c)) {
					c = c.getParent();
				}
				c.setSize(width + 50, height + 50);
			}
			imageLabel.setIcon(img);
		}
	}

	public void setImage(byte[] data)
	{
		SwingUtilities.invokeLater(new Update(data));
	}


	public void handleMessage(EBMessage message)
	{
	}
}

