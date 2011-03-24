#!/usr/bin/python 
from util import *

def install():
	with pushd():
		cd(config.get('BUILD', 'obj'))
		cmd(["make", 
			 "DESTDIR=%s" % config.get('DESTDIR','final'), 
			 "install"])

install()
