#!/usr/bin/python 
from util import *

def copy_arduino_config():
	header("COPYING CONFIGURATION FILES")	
	mkdir(config.get('DESTDIR', 'conf'))
	with pushd():
		cd(config.get('BUILD', 'conf'))
		print "COPING CONFIG FROM %s" % config.get('BUILD', 'conf')
		for filename in os.listdir(config.get('BUILD', 'conf')):
			if re.search(".*conf.in$", filename):
				subst_and_copy(filename, config.get('BUILD', 'conf'), config.get('DESTDIR', 'conf'))

def copy_arduino_build():
	header("COPYING ARDUINO BUILD PRODUCTS TO PACKAGE")

	# Copy scripts directory to destination
	copy_dir(config.get('BUILD', 'scripts'), config.get('DESTDIR', 'bin'))

	subst_and_copy("plumb.in", config.get('BUILD', 'scripts'), config.get('DESTDIR', 'bin'))
	cmd(["chmod", "755",
			"%s/%s" % (config.get('DESTDIR', 'bin'), "plumb")])

	# What was going into LIB should actually go into SHARE. 	
	#copy_dir(config.get('SOURCE_INCLUDE'), config.get('DEST_INCLUDE'))	

copy_arduino_config()
copy_arduino_build()
