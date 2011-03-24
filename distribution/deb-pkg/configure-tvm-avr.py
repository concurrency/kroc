#!/usr/bin/python 
from util import *

def autoreconf():
	header("RUNNING AUTORECONF")
	with pushd():
		cd(config.get('BUILD', 'svn'))
		cmd(["autoreconf", "-vfi"])

def configure():
	header("CONFIGURING THE TVM")

	remove_and_mkdir(config.get('BUILD', 'obj'))
	
	# Do the configure from within the obj directory.
	with pushd():
		cd(config.get('BUILD', 'obj'))

		cmd(["../configure",
					"--prefix=%s" % config.get('INSTALL', 'final'),
					"--with-toolchain=%s" % config.get('AVR', 'toolchain'),
					"--target=%s" % config.get('AVR', 'target'),
					"--with-wrapper=%s" % config.get('AVR', 'wrapper')])

def confget(file):
	d = {}
	f = open(file)
	for line in f:
		if (re.search('^#', line) == None):
			r = re.search('(.*?)=(.*)', line) 
			if (r):
				d[r.group(1)] = r.group(2)
	return d

#autoreconf()
configure()

