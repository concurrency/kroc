package org.transterpreter.occPlug;

/*
 * StyledLinkEditorKit.java
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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JEditorPane;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.text.Element;
import javax.swing.text.Position;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;

/**
 * A StyledEditorKit which supports hyperlinks.
 * 
 * Adapted from Christian Kaufhold's code found on:
 * http://www.codecomments.com/archive250-2004-8-261138.html
 */
class StyledLinkEditorKit extends StyledEditorKit {
	// attribute on inline elements; if value is URL, will be used for the
	// HyperlinkEvent
	public static final Object		LINK		= new StringBuffer("link");

	// can be static because it picks up the editor from the MouseEvent
	private static MouseListener	linkHandler	= new LinkHandler();

	public StyledLinkEditorKit() {
		// Do nothing
	}

	public void install(JEditorPane p) {
		super.install(p);

		p.addMouseListener(linkHandler);
	}

	public void deinstall(JEditorPane p) {
		p.removeMouseListener(linkHandler);

		super.deinstall(p);
	}

	private static class LinkHandler extends MouseAdapter {
		private Element characterElementAt(MouseEvent e) {
			JEditorPane p = (JEditorPane) e.getComponent();

			Position.Bias[] bias = new Position.Bias[1];
			int position = p.getUI().viewToModel(p, e.getPoint(), bias);

			if (bias[0] == Position.Bias.Backward && position != 0) --position;

			Element c = ((StyledDocument) p.getDocument())
					.getCharacterElement(position);

			// should test whether really inside

			return c;
		}

		/*
		 * public void mousePressed(MouseEvent e) { if
		 * (!SwingUtilities.isLeftMouseButton(e)) return;
		 * 
		 * JEditorPane p = (JEditorPane) e.getComponent();
		 * 
		 * if (p.isEditable()) return;
		 * 
		 * Element c = characterElementAt(e);
		 * 
		 * if (c != null && c.getAttributes().getAttribute(LINK) != null)
		 * activeElement = c; }
		 * 
		 * public void mouseReleased(MouseEvent e) { if
		 * (!SwingUtilities.isLeftMouseButton(e) || activeElement == null)
		 * return;
		 * 
		 * JEditorPane p = (JEditorPane) e.getComponent();
		 * 
		 * Element c = characterElementAt(e);
		 * 
		 * if (!p.isEditable() && c == activeElement) // too restrictive, should
		 * // find attribute run { activeElement = null;
		 * 
		 * Object target = c.getAttributes().getAttribute(LINK);
		 * 
		 * if (!(target instanceof URL)) target = null;
		 * 
		 * p.fireHyperlinkUpdate(new HyperlinkEvent(p,
		 * HyperlinkEvent.EventType.ACTIVATED, (URL) target, null, c)); } }
		 */

		public void mouseClicked(MouseEvent e) {
			/* Get the editor pane */
			JEditorPane p = (JEditorPane) e.getComponent();

			/*
			 * If the pane is editable, or its not the left mouse btn let other
			 * mouse handlers do their thing
			 */
			if (p.isEditable() || !SwingUtilities.isLeftMouseButton(e)) {
				super.mouseClicked(e);
				return;
			}

			/* Find the element that was clicked on */
			Element c = characterElementAt(e);

			/* Was there anything where we clicked? */
			if (c == null) {
				super.mouseClicked(e);
				return;
			}

			/* See if it has the link attribute */
			Object target = c.getAttributes().getAttribute(LINK);
			if (target == null) {
				super.mouseClicked(e);
				return;
			}

			p.fireHyperlinkUpdate(new HyperlinkEvent(p,
					HyperlinkEvent.EventType.ACTIVATED, null, null, c));

		}
	}
}
