#!/usr/bin/python 
from util import *

def confget(file):
	d = {}
	f = open(file)
	for line in f:
		if (re.search('^#', line) == None):
			r = re.search('(.*?)=(.*)', line) 
			if (r):
				d[r.group(1)] = r.group(2)
	return d
	
def build_firmwares():
	header("BUILDING ARDUINO FIRMWARES")
	# Run the build script in the wrapper.
	# This generates virtual machines for multipl
	# targets (m328, m1280, 3.3V and 5V (8MHz and 16MHz, respectively))
	with pushd():
		arduino = config.get('BUILD', 'arduino')
		cd(arduino)
		cmd(['autoreconf', '-vfi'])

		build_dir = "%s/%s" % (arduino, "build")
		remove_and_mkdir(build_dir)

		boards  = "%s/%s" % (arduino, "occam/conf")
		for target in os.listdir(boards):
			# Only look at the .in files.
			if (re.search('in$', target)):
				
				target_name = re.search("(.*?)\.conf\.in", target).group(1)
				tbuild = "%s/%s" % (build_dir, target_name)
				mkdir(tbuild)

				bc = confget('%s/occam/conf/%s' % (arduino, target))
				
				with pushd():
					cd(tbuild)
					header('CONFIGURING FOR %s' % bc['TVM_ARDUINO_FIRMWARE'])
					cmd(['../../configure',
							'--host=avr',
							'--with-bytecode-addr=%s' % bc['TVM_BYTECODE_ADDR'],
							'--with-mcu=%s' % bc['TVM_GCC_MCU'],
							'--with-fcpu=%s' % bc['TVM_F_CPU'],
							'--with-firmware-name=%s' % bc['TVM_ARDUINO_FIRMWARE'],
							])
					cmd(['make'])
					cmd(['make', 'firmware.hex'])
				
				mkdir('output')
				copy('%s/%s' % (tbuild, bc['TVM_ARDUINO_FIRMWARE']), 'output')
	
def copy_firmwares():
	mkdir(config.get('BUILD', 'firmware'))
	copy_files('.*hex', 
							'%s/output' % config.get('BUILD', 'arduino'), 	
							config.get('BUILD', 'firmware'))

build_firmwares()
copy_firmwares()
