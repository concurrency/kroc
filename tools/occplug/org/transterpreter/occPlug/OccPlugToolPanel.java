package org.transterpreter.occPlug;
/*
 * OccPlugToolPanel.java
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

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusListener;
import java.awt.event.ItemListener;

import javax.swing.AbstractButton;
import javax.swing.AbstractListModel;
import javax.swing.BorderFactory;
import javax.swing.ComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;

import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.RolloverButton;

public class OccPlugToolPanel extends JPanel
{
	private OccPlug theOccPlug;
	private JComboBox compileTarget;
	/* For the SRV-1 */
	private JTextField host, port;
  
	private AbstractButton compileBtn, runBtn, stopBtn, clearBtn;

	public static final int NORMAL = 1;
	public static final int RUNNING = 2;
	public static final int ALLON = 3;
	public static final int ALLOFF = 4;

	public static final String compileTargets[] = { 
    "Surveyor SRV-1", "Desktop", "Mindstorms RCX" };
  public static final int SRV_IDX = 0;
  public static final int DESKTOP_IDX = 1;
  public static final int RCX_IDX = 2;

	public OccPlugToolPanel(OccPlug thePlug)
	{
		theOccPlug = thePlug;
		final JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);
    final JPanel options = new JPanel(new CardLayout());

		compileBtn = makeCustomButton("occPlug.compile", new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
        OccPlugToolPanel.this.theOccPlug.compile((String) compileTarget
          .getSelectedItem());
			}
		});
		toolBar.add(compileBtn);

		runBtn = makeCustomButton("occPlug.run", new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				savePreferences();
				OccPlugToolPanel.this.theOccPlug.run((String) compileTarget
						.getSelectedItem());
			}
		});
		toolBar.add(runBtn);

		stopBtn = makeCustomButton("occPlug.stop", new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				OccPlugToolPanel.this.theOccPlug.stopRunningProcess();
			}
		});
		toolBar.add(stopBtn);

		clearBtn = makeCustomButton("occPlug.clearTextArea", new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				OccPlugToolPanel.this.theOccPlug.clearTextArea();
			}
		});
		toolBar.add(clearBtn);

		JLabel compileTargetLabel = new JLabel("Platform: ");
		toolBar.add(compileTargetLabel);

		/*libraries = new JComboBox(new LibrariesComboBoxModel());*/
		
		compileTarget = new JComboBox(compileTargets);
    compileTarget.addActionListener(new ActionListener() 
    {
      public void actionPerformed(ActionEvent e) 
      {
        String selected = (String) compileTarget.getSelectedItem();
        // Save the currently selected platform target
        jEdit.setProperty(
          OccPlugPlugin.OPTION_PREFIX + "lastPlatformTarget",
          selected);
        // Switch to the options for that target
        CardLayout cl = (CardLayout)(options.getLayout());
        cl.show(options, selected);
      }
    });


		toolBar.add(compileTarget);
    
		/* SRV-1 Console Options */
		FocusListener srvFocus = new FocusListener () 
		{
			public void focusGained(FocusEvent e) { }
			public void focusLost(FocusEvent e)
			{
				savePreferences();
			}
		};

    // Setup SRV options
    JPanel srvOptions = new JPanel();
		srvOptions.add(new JLabel(" Host: "));
		host = new JTextField(16);
		host.addFocusListener(srvFocus);
		host.setText(jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX + "srvHost"));
		srvOptions.add(host);
		srvOptions.add(new JLabel(" Port: "));
		port = new JTextField(6);
		port.addFocusListener(srvFocus);
		port.setText(jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX + "srvPort"));
		srvOptions.add(port);

    // Set up the cards in the options panel
    options.add(srvOptions, compileTargets[SRV_IDX]);
    options.add(new JPanel(), compileTargets[DESKTOP_IDX]);
    options.add(new JPanel(), compileTargets[RCX_IDX]);

    toolBar.add(options);
    
    // Set the selected target and update the options panel
    // FIXME: Check that the last selected target is valid otherwise set it to
    // the default or something
    String lastSelected = jEdit.getProperty(
        OccPlugPlugin.OPTION_PREFIX + "lastPlatformTarget",
        compileTargets[DESKTOP_IDX]);
    compileTarget.setSelectedItem(lastSelected);
    CardLayout cl = (CardLayout)(options.getLayout());
    cl.show(options, lastSelected);

		this.setLayout(new BorderLayout(10, 0));
		this.add(BorderLayout.WEST, toolBar);
		this.setBorder(BorderFactory.createEmptyBorder(0, 0, 3, 10));

		setState(NORMAL);
	}

	private AbstractButton makeCustomButton(String name, ActionListener listener)
	{
		String toolTip = jEdit.getProperty(name.concat(".label"));
		AbstractButton b = new RolloverButton(GUIUtilities.loadIcon(jEdit
				.getProperty(name + ".icon")));
		if (listener != null)
		{
			b.addActionListener(listener);
			b.setEnabled(true);
		} else
		{
			b.setEnabled(false);
		}
		b.setToolTipText(toolTip);
		return b;
	}

	private void savePreferences()
	{
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "srvHost", host.getText());
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX + "srvPort", port.getText());
	}

	public void setState(final int state)
	{
		/* This might be executed from another thread... so just in case */
		Runnable doWorkRunnable = new Runnable()
		{
			public void run()
			{
				switch (state)
				{
					case NORMAL:
						compileBtn.setEnabled(true);
						runBtn.setEnabled(true);
						stopBtn.setEnabled(false);
						clearBtn.setEnabled(true);
						host.setEnabled(true);
						port.setEnabled(true);
						break;
					case RUNNING:
						compileBtn.setEnabled(false);
						runBtn.setEnabled(false);
						stopBtn.setEnabled(true);
						clearBtn.setEnabled(true);
						host.setEnabled(false);
						port.setEnabled(false);
						break;
					case ALLON:
						compileBtn.setEnabled(true);
						runBtn.setEnabled(true);
						stopBtn.setEnabled(true);
						clearBtn.setEnabled(true);
						break;
					case ALLOFF:
						compileBtn.setEnabled(false);
						runBtn.setEnabled(false);
						stopBtn.setEnabled(false);
						clearBtn.setEnabled(false);
						break;
					default:
						throw new RuntimeException("Bad state in setState");
				}
			}
		};
		SwingUtilities.invokeLater(doWorkRunnable);
	}
}
