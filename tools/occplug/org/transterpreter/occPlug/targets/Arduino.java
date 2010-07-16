/**
 * 
 */
package org.transterpreter.occPlug.targets;

/*
 * Arduino.java
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

import java.awt.Component;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Properties;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.jEdit;
import org.transterpreter.occPlug.OccPlugPlugin;
import org.transterpreter.occPlug.OccPlugUtil;
import org.transterpreter.occPlug.OccPlug.DocumentWriter;
import org.transterpreter.occPlug.hosts.BaseHost;
import org.transterpreter.occPlug.process.ExecWorker;
import org.transterpreter.occPlug.targets.support.BaseTarget;
import org.transterpreter.occPlug.targets.support.CompileAbility;
import org.transterpreter.occPlug.targets.support.CompileTarget;
import org.transterpreter.occPlug.targets.support.FirmwareAbility;
import org.transterpreter.occPlug.targets.support.FirmwareTarget;
import org.transterpreter.occPlug.targets.support.OccbuildHelper;
import org.transterpreter.occPlug.targets.support.OccbuildOptions;
import org.transterpreter.occPlug.targets.support.OccbuildTVMOptions;
import org.transterpreter.occPlug.targets.support.TargetExecWorkerHelper;

/**
 * @author clj
 * 
 */
public class Arduino extends BaseTarget implements FirmwareAbility,
		CompileAbility {

	private class ArduinoDevice
	{
		private String id;
		private static final String PREFIX = OccPlugPlugin.PROPERTY_PREFIX + "arduino.device.";
		
		public ArduinoDevice(String id)
		{
			this.id = id;
		}
		
		public String getProperty(String propertyName, boolean required)
		{
			String name = jEdit.getProperty(PREFIX + id + "." + propertyName);
			if(name == null && required)
				throw new RuntimeException("Arduino device " + id + " has no property: " + propertyName);
			return name;			
		}
		
		public String getID()
		{
			return id;
		}
		
		public String getName()
		{
			return getProperty("name", true);
		}
		
		public String getConfig()
		{
			return getProperty("conf", true);
		}
		
		public String toString()
		{
			return getName();
		}
	}
	
	private class DeviceProperties
	{
		private final ArduinoDevice device;
		private final Properties props = new Properties();
		private final String configFile;
		
		public DeviceProperties(ArduinoDevice device)
		{
			this.device = device;
			
			BaseHost host = BaseHost.getHostObject();
			
			configFile = OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(host.getPath("tvm-arduino", "conf"), device.getConfig())); 
			try {
				FileInputStream in = new FileInputStream(configFile);
				props.load(in);
			} catch (FileNotFoundException e) {
				throw new RuntimeException(e);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		
		protected String getProp(String name)
		{
			return props.getProperty(name);
		}
		
		public String getMCU()
		{
			return getProp("TVM_MCU");
		}
		
		public String getUploadRate()
		{
			return getProp("TVM_UPLOAD_RATE");
		}
		
		public String getFirmware()
		{
			return getProp("TVM_ARDUINO_FIRMWARE");
		}
		
		public String getFCPU()
		{
			return getProp("TVM_F_CPU");
		}

		public String getBytecodeAddr()
		{
			return getProp("TVM_BYTECODE_ADDR");
		}
		
		public String getConfigFileName() {
			return configFile;
		}

		public String getPlatform() {
			return device.getID();
		}		
	}
	
	private final FirmwareTarget[]		firmwareTargets			= { 
			new FirmwareTarget("Arduino", this) };
	private final CompileTarget[]		compileTargets			= { 
			new CompileTarget("Arduino", this) };
	private final DefaultComboBoxModel	arduinoPort				= new DefaultComboBoxModel();
	private final SortedSet				arduinoPortItems		= new TreeSet();
	private final JPanel				arduinoFirmwareOptions;
	private final JPanel				arduinoCompileOptions;
	private final ArrayList				disableOnDownload		= new ArrayList();
	private final ArduinoDevice[]		arduinoDevices;
	private final DefaultComboBoxModel	arduinoDevicesModel;
	
	public Arduino()
	{
		super();
		
		String devicesString = jEdit.getProperty(OccPlugPlugin.PROPERTY_PREFIX + "arduino.devices");
		String[] deviceIDs = devicesString.split(",");
		arduinoDevices = new ArduinoDevice[deviceIDs.length];
		for(int i = 0; i < deviceIDs.length; i++)
		{
			arduinoDevices[i] = new ArduinoDevice(deviceIDs[i]);
		}
		arduinoDevicesModel = new DefaultComboBoxModel(arduinoDevices);
		
		arduinoFirmwareOptions = makeOptionsPanel();
		arduinoCompileOptions = makeOptionsPanel();
	}
	
	public boolean useVT220ForRunning()
	{
		return false;
	}
	
	public FirmwareTarget[] getFirmwareTargets() {
		return firmwareTargets;
	}

	protected JPanel makeOptionsPanel()
	{
		/* Focus Listener for option saving */
		final FocusListener saveFirmwareOptionsFocusListener = new FocusListener() {
			public void focusGained(FocusEvent e) {
				// Not used
			}

			public void focusLost(FocusEvent e) {
				saveOptions();
			}
		};

		/* Arduino options */
		JPanel options = new JPanel();
		options.add(new JLabel("Device: "));
		JComboBox device = new JComboBox(arduinoDevicesModel);
		disableOnDownload.add(device);
		device.addFocusListener(saveFirmwareOptionsFocusListener);
		options.add(device);
		String selected = 
			jEdit.getProperty(OccPlugPlugin.OPTION_PREFIX + "targets.arduino.device");
		for(ArduinoDevice arduinoDevice : arduinoDevices)
		{
			if(arduinoDevice.getID().equals(selected))
			{
				device.setSelectedItem(arduinoDevice);
				break;
			}
		}
		
		options.add(new JLabel("Port: "));
		JComboBox port = new JComboBox(arduinoPort);
		port.setEditable(true);
		PopupMenuListener portPopupListener = new PopupMenuListener() {
			public void popupMenuCanceled(PopupMenuEvent e) {
				// Not used
			}

			public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
				// Not used
			}

			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
				String[] devices = BaseHost.getHostObject().getSerialPorts();

				if (devices == null) return;

				for (int i = 0; i < devices.length; i++) {
					if (arduinoPortItems.add(devices[i])) {
						arduinoPort.addElement(devices[i]);
					}
				}
			}
		};
		portPopupListener.popupMenuWillBecomeVisible(null);
		port.addFocusListener(saveFirmwareOptionsFocusListener);
		port.getEditor().getEditorComponent().addFocusListener(
				saveFirmwareOptionsFocusListener);
		port.addPopupMenuListener(portPopupListener);
		port.setSelectedItem(jEdit
				.getProperty(OccPlugPlugin.OPTION_PREFIX
						+ "targets.arduino.port"));
		disableOnDownload.add(port);
		options.add(port);

		return options;		
	}
	
	public JPanel getFirmwareOptions(FirmwareTarget target) {
		return arduinoFirmwareOptions;
	}
	
	public JPanel getCompileOptions(CompileTarget target) {
		return arduinoCompileOptions;
	}
	
	protected void saveOptions() {
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX
				+ "targets.arduino.port", (String) arduinoPort
				.getSelectedItem());
		jEdit.setProperty(OccPlugPlugin.OPTION_PREFIX
				+ "targets.arduino.device", ((ArduinoDevice) arduinoDevicesModel.getSelectedItem()).getID());
	}

	public void uploadFirmware(FirmwareTarget target, Runnable finished) {
		final DocumentWriter output = targetSupport.getDefaultOutput();
		
		BaseHost host = BaseHost.getHostObject();

		String port = (String) arduinoPort.getSelectedItem();
		if (port == null || port.trim().equals("")) {
			output.writeError("Please specify a port");
			finished.run();
			return;
		}

		String bin = host.getPath("tvm-arduino", "bin");
		ArduinoDevice selectedDevice = (ArduinoDevice) arduinoDevicesModel.getSelectedItem();
		DeviceProperties props = new DeviceProperties(selectedDevice);
		
		
		final String[] firmdlCommand = { 
				OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(bin, host.getCommandName("avrdude"))),
				"-C", OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(bin, "avrdude.conf")), 
				"-U", "flash:w:" + OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(host.getPath("tvm-arduino", "firmware"), props.getFirmware())) + ":i",
				"-F", 
				"-P", (String) arduinoPort.getSelectedItem(),
				"-c", "arduino", 
				"-p", props.getMCU(), 
				"-b", props.getUploadRate() };

		output.writeRegular("Uploading Plumbing firmware\n");
		OccPlugUtil.writeVerbose("Device id: " + selectedDevice.getID() + "\n", output);
		OccPlugUtil.writeVerbose("Using config: " + props.getConfigFileName() + " \n", output);
		OccPlugUtil.writeVerbose("Command: " + Arrays.asList(firmdlCommand) + " \n", output);

		final Runnable[] finalisers = { finished };
		ExecWorker worker = new ExecWorker(firmdlCommand, null, null, // new
																			// File(workingDir),
				new TargetExecWorkerHelper("firmware upload", output,
						finalisers));
		targetSupport.startWorker(worker);
	}

	public void setEnabledForFirmwareOptions(boolean enabled) {
		Iterator i = disableOnDownload.iterator();
		while (i.hasNext()) {
			Component item = (Component) i.next();
			item.setEnabled(enabled);
		}
	}

	public void compileProgram(CompileTarget target, Runnable finished) {
		
		final DocumentWriter output = targetSupport.getDefaultOutput();

		final String occFile = targetSupport.getActiveFileName();
		
		if (!occFile.toLowerCase().endsWith(".occ")) {
			output.writeError("Error: Only occam (.occ) source files can be compiled.\n");
			output.writeError("       The current buffer does not contain a .occ file!\n");
			finished.run();
			return;
		}
	
		BaseHost host = BaseHost.getHostObject();
		String bin = host.getPath("tvm-arduino", "bin");
		ArduinoDevice selectedDevice = (ArduinoDevice) arduinoDevicesModel.getSelectedItem();
		DeviceProperties props = new DeviceProperties(selectedDevice);
		
		OccbuildOptions options = new OccbuildTVMOptions("tvm-arduino");
		options.systemSearch = new String[] {
				//OccPlugUtil.pathifyXXX("share/tvm-arduino/plumbing-include"),
				OccPlugUtil.pathifyXXX(host.getPath("tvm-arduino", "lib")),
				OccPlugUtil.pathifyXXX(host.getPath("tvm-arduino", "include")),
				OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(host.getPath("tvm-arduino", "include"), "arch", props.getMCU())),
				OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(host.getPath("tvm-arduino", "include"), "arch", "common")),
				OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(host.getPath("tvm-arduino", "include"), props.getPlatform()))};
		options.defines.put("F.CPU", props.getFCPU()); 
		// FIXME: occbuild for avr builds should always set -TLE? otherwise builds fail on BE machines
		options.extra_options.add(new String[] { "--occ21-opts", "-tle"});
		String[] occbuildCommand = OccbuildHelper.makeOccbuildProgramCommand(options, occFile);
		
		// Say what we are doing
		output.writeRegular("Compiling: " + occFile + "\n");
		OccPlugUtil.writeVerbose("Command: " + Arrays.asList(occbuildCommand) + "\n", output);

		final String[] env = OccbuildHelper.makeOccbuildEnvironment("tvm-arduino");
		OccPlugUtil.writeVerbose("Environment: " + Arrays.asList(env) + "\n", output);
		
		final Runnable[] finalisers = { finished };
		ExecWorker worker = new ExecWorker(occbuildCommand, 
				env, 
				new File(targetSupport.getActiveDirectory()),
				new TargetExecWorkerHelper("compile", output,
						finalisers));
		targetSupport.startWorker(worker);		
	}


	public CompileTarget[] getCompileTargets() {
		return compileTargets;
	}

	public void runProgram(final CompileTarget theTarget, Runnable finished) {
	
		final DocumentWriter output = targetSupport.getDefaultOutput();
		
		String port = (String) arduinoPort.getSelectedItem();
		if (port == null || port.trim().equals("")) {
			output.writeError("Please specify a port");
			finished.run();
			return;
		}

		BaseHost host = BaseHost.getHostObject();
		String bin = host.getPath("tvm-arduino", "bin");
		ArduinoDevice selectedDevice = (ArduinoDevice) arduinoDevicesModel.getSelectedItem();
		DeviceProperties props = new DeviceProperties(selectedDevice);
		
		final String fileBase = MiscUtilities.getFileNameNoExtension(targetSupport.getActiveFileName());
		final String tbcFile = fileBase + ".tbc";
		final String ihexFile = fileBase + ".ihex";
		final File curDir = new File(targetSupport.getActiveDirectory());
		final String [] ihexCommand = {
				OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(bin, host.getCommandName("binary-to-ihex"))),
				props.getBytecodeAddr(),
				tbcFile,
				ihexFile
		};

		final String[] runCommand = { 
				OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(bin, host.getCommandName("avrdude"))),
				"-C",OccPlugUtil.pathifyXXX(MiscUtilities.constructPath(bin, "avrdude.conf")),
				"-V", 
				"-F", 
				"-P", (String) arduinoPort.getSelectedItem(),
				"-D",
				"-c", "arduino", 
				"-p", props.getMCU(), 
				"-b", props.getUploadRate(), 
				"-U", "flash:w:" + ihexFile + ":i"};
		final Runnable[] finalisers = { finished };
		
		final Runnable[] intermediaryFinalisers =
		{
				new Runnable() {
					public void run() {
						ExecWorker execWorker = new ExecWorker(runCommand, 
								null, 
								curDir,
								new TargetExecWorkerHelper("run", output,
										finalisers));
						execWorker.start();
					}
				}
		};
		ExecWorker worker = new ExecWorker(ihexCommand, 
				null, 
				curDir,
				new TargetExecWorkerHelper("run", output,
						intermediaryFinalisers));
		targetSupport.startWorker(worker);
	}
}
