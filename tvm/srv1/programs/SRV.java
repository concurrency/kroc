/*
package org.transterpreter.occPlug;
*/

import java.io.*;
import java.net.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class SRV {
	/* Constants */
	public static final int	CMD_GRAB_IMAGE	= 'I';
	public static final int CMD_KILL	= '!';
	public static final int CMD_UPLOAD	= 'U';

	public static final int	RES_EXTENDED	= '#';
	public static final int RES_IMJ1	= -1;
	public static final int RES_IMJ3	= -2;
	public static final int RES_IMJ5	= -3;
	public static final int RES_IMJ7	= -4;
	public static final int RES_IMJ9	= -5;
	public static final int RES_UNKNOWN	= 0;


	/* Instance variables */
	public final String	host;
	public final int 	port;

	private Socket		conn;
	private InputStream	in;
	private OutputStream	out;

	public SRV(String hostname, int port)
	{
		this.host = hostname;
		this.port = port;
	}

	public String getHost()
	{
		return host;
	}

	public int getPort()
	{
		return port;
	}	

	public void disconnect()
	{
		if (conn != null) {
			try {
				conn.close();
			} catch (IOException e) {
				System.out.println(e.toString());
			}
			conn	= null;
			in	= null;
			out	= null;
		}
	}
  
	public boolean connect()
	{
		disconnect();
		try {
			conn 	= new Socket(host, port);
			in	= conn.getInputStream();
			out	= conn.getOutputStream();
			return true;
		} catch (Exception e) {
			System.out.println(e.toString());
			return false;
		}
	}

	public boolean sendCommand(int cmd, String arg)
	{
		if (out == null)
			return false;
		try {
			out.write((byte) cmd);
			switch (cmd) {
				case CMD_KILL:
					out.write((byte) '\n');
					break;
				default:
					break;
			}
			out.flush();
			return true;
		} catch (IOException e) {
			System.out.println(e.toString());
			return false;
		}
	}
	
	public boolean sendCommand(int cmd)
	{
		return sendCommand(cmd, "");
	}

	public InputStream getInputStream()
	{
		return in;
	}

	public OutputStream getOutputStream()
	{
		return out;
	}

	public byte[] readImage() throws IOException
	{
		byte[] data;
		int size = 0;

		for (int i = 0; i < 4; ++i) {
			int b = in.read();

			if (b == -1)
				return null;

			size += (b << (i * 8));
		}

		data = new byte[size];
		for (int i = 0; i < size; ++i) {
			int b = in.read();

			if (b == -1)
				return null;

			data[i] = (byte) b;
		}

		return data;
	}

	public Response readResponse()
	{
		if (in == null)
			return null;
		try {
			int c;
			
			if ((c = in.read()) != '#') {
				return new Response(RES_UNKNOWN, Integer.toHexString(c));
			}
			
			if ((c = in.read()) != RES_EXTENDED) {
				return new Response(c);
			}

			StringBuffer str = new StringBuffer();

			if ((c = in.read()) == 'I') {
				str.append ((char) c);
				if ((c = in.read()) == 'M') {
					str.append ((char) c);
					if ((c = in.read()) == 'J') {
						int type = RES_UNKNOWN;
						str.append ((char) c);
						c = in.read();
						switch (c) {
							case '1': type = RES_IMJ1; break;
							case '3': type = RES_IMJ3; break;
							case '5': type = RES_IMJ5; break;
							case '7': type = RES_IMJ7; break;
							case '9': type = RES_IMJ9; break;
							default: break;
						}
						if (type != RES_UNKNOWN) {
							byte[] image = readImage();
							return new Response(type, image);
						}
					}
				}
			} 
			if (c != (-1)) {
				do {
					str.append ((char) c);
					c = in.read();
				} while (c != '\n' && c != (-1));
				
				return new Response(RES_EXTENDED, str.toString());
			} else {
				return null;
			}
		} catch (IOException e) {
			System.out.println(e.toString());
			return null;
		}
	}

	public byte[] getFrame()
	{
		if (sendCommand(CMD_GRAB_IMAGE)) {
			System.out.println ("Sent image command.");
		} else {
			System.out.println ("Failed to send image command.");
			return null;
		}
		
		Response r = readResponse();
		if (r != null) {
			System.out.println ("Got response: " + r);
			return r.data;
		} else {
			System.out.println ("Fail to get response.");
			return null;
		}
	}

	private boolean waitForResponse(String beginning, int timeout) throws IOException
	{
		do {
			if (in.available() > 0) {
				Response r = readResponse();
				if (r == null)
					return false;
				System.out.println (r);
				if (r.type == RES_EXTENDED && r.value != null) {
					if (r.value.startsWith(beginning))
						return true;
				}
			} else {
				try {
					Thread.sleep(100);
					timeout -= 100;
				} catch (InterruptedException e) {
					System.out.println(e.toString());
				}
			}
		} while (timeout > 0);

		return false;
	}

	public boolean upload(byte[] bytecode)
	{
		if (out == null)
			return false;
		if (bytecode.length < 20)
			return false;
		
		try {
			System.err.println("Sending kill command.");
			sendCommand(CMD_KILL);
			sendCommand(CMD_KILL);
			sendCommand(CMD_KILL);
			sendCommand(CMD_KILL);

			waitForResponse("Send 'U' to begin", 2000);
				
			System.err.println("Sending upload command.");
			sendCommand(CMD_UPLOAD);

			System.err.println("Waiting...");
			if (!waitForResponse("Waiting for header", 3000))
				return false;

			System.err.println("Sending header...");
			for(int i = 0; i < 20; ++i)
				out.write(bytecode[i]);

			System.err.println("Waiting...");
			if (!waitForResponse("Waiting for bytecode", 3000))
				return false;

			System.err.println("Uploading bytecode...");

			int ack = 0;

			for (int i = 20; i < bytecode.length; ++i) {
				out.write(bytecode[i]);

				if (in.available() > 0) {
					int c = in.read();
					if (c != '.')
						return false;
					System.err.print(".");
					ack++;
				}
			}
			
			while (ack < (bytecode.length - 20)) {
				int c = in.read();
				if (c != '.')
					return false;
				System.err.print(".");
				ack++;
			}
			
			if (in.read() != '\n')
				return false;

			System.err.println("");
			System.err.println("Complete!");

			return true;
		} catch (IOException e) {
			System.out.println(e.toString());
			return false;
		}
	}

	public static class Response
	{
		public final int 	type;
		public final String	value;
		public final byte[]	data;

		public Response (int type)
		{
			this.type	= type;
			this.value	= null;
			this.data	= null;
		}
		
		public Response (int type, String value)
		{
			this.type	= type;
			this.value	= value;
			this.data	= null;
		}

		public Response (int type, byte[] data)
		{
			this.type	= type;
			this.value	= null;
			this.data	= data;
		}

		public String toString()
		{
			return 	"type = " + type + ", " +
				"value = " + (value != null ? "\"" + value + "\"" : "null") + ", " +
				"data = " + (data != null ? "[" + data.length + " bytes]" : "null");
		}
	}

	public static class Viewer implements Runnable {
		private final SRV 	srv;
		private JFrame		frame;
		private JLabel		label;
		private boolean		shutdown = false;

		public Viewer (String host, int port)
		{
			this.srv = new SRV(host, port);
			
			if (srv.connect()) {
				System.out.println("Connected.");
			} else {
				System.out.println("Failed to connected.");
				return;
			}

			byte[] imageData = srv.getFrame();
			if (imageData == null) {
				System.out.println("Fail to get initial frame.");
				return;
			}
		
			try {
				UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
			} catch (Exception e) {
				// ignore
			}

			frame = new JFrame("SRV1 Viewer");
			label = new JLabel(new ImageIcon(imageData));

			//frame.addWindowListener(this);
			//frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
			
			SwingUtilities.invokeLater(this);

			frame.add(label);
			frame.pack();
			frame.setVisible(true);
		}

		public void run()
		{
			byte[] imageData = srv.getFrame();
			if (imageData != null) {
				label.setIcon(new ImageIcon(imageData));
			}
			if (!shutdown) {
				SwingUtilities.invokeLater(this);
			}
		}
	}

	public static void main(String[] args) {
		if (args.length < 3) {
			System.out.println("Usage: java SRV <host> <port> <command>");
			System.out.println("");
			System.out.println("Where command is one of the following:");
			System.out.println("     upload <file>      uploads a bytecode file");
			System.out.println("     viewer             basic image viewer");
			System.out.println("");
			System.exit(1);
		}

		final String	host	= args[0];
		final int 	port	= Integer.parseInt(args[1]);
		final String	cmd	= args[2];

		if (cmd.startsWith("up")) {
			if (args.length < 4) {
				System.out.println("Usage: java SRV <host> <port> upload <file>");
				System.exit(1);
			}
			final String fn = args[3];
			byte[] bytecode	= null;

			try {
				File		file	= new File(fn);
				FileInputStream is	= new FileInputStream(file);
				int		length	= (int) file.length();

				bytecode = new byte[length];
				if (is.read(bytecode) < length) {
					throw new IOException("Input shorter than expected.");
				}
			} catch (IOException e) {
				System.out.println(e.toString());
				System.out.println("Unable to read bytecode from \"" + fn + "\"");
				System.exit(1);
			}

			final SRV srv = new SRV(host, port);

			if (srv.connect()) {
				System.out.println("Connected.");
				if (srv.upload(bytecode)) {
					System.out.println("Upload complete.");
					System.exit(0);
				} else {
					System.out.println("Upload failed.");
				}
			} else {
				System.out.println("Failed to connected.");
			}

			System.exit(1);
		} else if (cmd.startsWith("view")) {
			final SRV.Viewer srv = new SRV.Viewer(host, port);
		} else {
			System.out.println("Unknown command: " + cmd);
			System.exit(1);
		}
	}
}
