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
			

def build():
	header("RUNNING MAKE")
	# Create a destdir for the build
	remove_and_mkdir(config.get('BUILD', 'destdir'))

	# 'make' in the object directory
	with pushd():
		cd(config.get('BUILD', 'obj'))
		cmd("make")
		cmd(["make", 
				 "DESTDIR=%s" % config.get('BUILD', 'destdir'), 	
					"install"])

build()
