from __future__ import with_statement
from optparse import OptionParser
import contextlib
import datetime
import commands
import subprocess
import inspect
import time
import os
import sys
import re
import ConfigParser

# UTILITY FUNCTIONS 
def concat(ls):
	str = ""
	for s in ls:
		str += s
	return str

def props(obj):
    pr = {}
    for name in dir(obj):
        value = getattr(obj, name)
        if not name.startswith('__') and not inspect.ismethod(value):
            pr[name] = value
    return pr

@contextlib.contextmanager
def pushd():
    curdir= os.getcwd()
    try: yield
    finally: cd(curdir)

def cd(dir):
	print "CD [ %s ]" % dir
	os.chdir(dir)

# Not needed anymore... but, until I remove it from all of the code,
# we'll let it do nothing...
def build_command(ls):
	#return ' '.join(ls)
	return ls

def cmd(ls):
	print "COMMAND %s" % ls
	# result = commands.getstatusoutput(str)
	p = subprocess.Popen(ls, stdout = subprocess.PIPE)
	for line in iter(p.stdout.readline, ''):
		print line.rstrip()

def remove_dir(dir):
	print " -- REMOVING -- %s" % dir
	cmd(build_command(["rm", "-rf", dir]))

def remove_files(filespec):
	print " -- REMOVING FILES -- "
	if not os.path.exists(filespec):
		print "FILE DOES NOT EXIST: %s" % filespec
	else:
		cmd(build_command(['rm', filespec]))

def remove_and_mkdir(dir):
	remove_dir(dir)
	mkdir(dir)

def copy_dir(src, dst):
	print "DIRECTORY COPY [%s] TO [%s]" % (src, dst)
	cmd(build_command(["rsync", "-vaz", 
										 "--exclude=*svn*",
										 "--exclude=.svn",
										 "--exclude=*.in",
										 "%s/" % src,
										 "%s/" % dst]))

def copy_files(pat, src_dir, dest_dir):
	print "COPYING %s FROM %s" % (pat, src_dir)
	for filename in os.listdir(src_dir):
		if re.search(pat, filename):
			print "COPYING FILE [%s] TO [%s]" % (filename, dest_dir)
			if (re.search("\.svn", filename)):
				header("WILL NOT COPY A SVN FILE: %s" % filename)
			else:
				cmd(build_command(["cp", 
													 "%s/%s" % (src_dir, filename),
													 "%s/%s" % (dest_dir, filename)]))

def copy (src, dest):
	cmd(['cp', src, dest])

def mkdir(dir):
	cmd(build_command(["mkdir", "-p", dir]))

def subst_and_copy(file, source_dir, dest_dir):
	print "DOING SUBST ON [ %s/%s ]" % (source_dir, file)

	with open("%s/%s" % (source_dir, file), 'r') as input:
		dest_file = re.search("(.*).in", file).group(1)
		print "DESTINATION FILE: %s/%s" % (dest_dir, dest_file)
	
		with open("%s/%s" % (dest_dir, dest_file), 'w') as output:
			for line in input:
				pat = re.compile('@(.*?),(.*?)@')
				res = re.search(pat, line)
				if res:
					section = res.group(1)
					tag     = res.group(2)
					print "Replacing [%s %s] in\n %s" % (section, tag, line)
					line = re.sub(pat, config.get(section, tag), line)

				# After replacements, write the line
				output.write(line)

# MEAT AND POTATOES
def repeat_print(count, str):
	for i in range(0, count):
		print str,
	print

def center_print(count,str):
	sides = (count - (len(str) + 2)) / 2
	for i in range(0, sides):
		print "  ",
	print " %s " % str,
	for i in range(0, sides):
		print " ",
	print

def header(str):
	repeat_print(40,"*")
	center_print(40,str)
	repeat_print(40,"*")

# Read the config
config = ConfigParser.SafeConfigParser()
config.read('build.conf')


