from util import *

def copy_arduino_config():
	header("COPYING CONFIGURATION FILES")	
	with pushd():
		cd(config.get('SOURCE_CONF'))
		for filename in os.listdir(config.get('SOURCE_CONF')):
			if re.search(".*conf.in$", filename):
				subst_and_copy(filename, config.get('SOURCE_CONF'), config.get('DEST_CONF'))

def main ():
	
