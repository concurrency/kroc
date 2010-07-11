from __future__ import with_statement
from optparse import OptionParser
import contextlib
import datetime
import commands
import inspect
import time
import os
import re

import config

# USAGE
# You can find out what parameters are avaialble by invoking this 
# script with:
#
# python make-deb-pkg.py --help
#
# Typical usage will be:
# 
# python make-deb-pkg.py --all <SVN URL>
#
# The "all" command takes the URL of the repository you want to
# check out and build from. It leaves a .deb and .rpm in /tmp
# that can then be used.
#
# When updating occam libraries, you can get away with running
# individual steps. I added 
#
# python make-deb-pkg.py --refresh-libs
#
# which re-runs the steps that copy libraries in, and 
# then a new .deb is generated.
#
# A common usage then becomes:
#
# python make-deb-pkg.py --refresh-libs \
#        && dpkg -r concurrency \
#        && dpkg -i /tmp/concurrency_..._i386.deb
#
# Thus refreshing the package, removing the old, and installing the new.
# This is useful while developing... it saves a complete rebuild,
# but gets new libraries in place.
# 
# 20100706 MCJ: Running on my Ubuntu 9.10 VM on my 2.4GHz Mac, this
#               takes around 18 minutes to complete, start to finish.

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
	os.chdir(dir)

def build_command(ls):
	return ' '.join(ls)

def cmd(str):
	print "COMMAND [ %s ]" % str
	result = commands.getstatusoutput(str)

def remove_and_create_dir(dir):
	cmd(build_command(["rm", "-rf", dir]))
	cmd(build_command(["mkdir", dir]))

def remove_dir(dir):
	cmd(build_command(["rm" "-rf", dir]))

def copy_dir(src, dst):
	print "COPYING [%s] TO [%s]" % (src, dst)
	cmd(build_command(["rsync", "-vaz", 
										 "--exclude=*svn*",
										 "--exclude=.svn",
										 "--exclude=*.in",
										 "%s/" % src,
										 "%s/" % dst]))

def copy_files(pat, src_dir, dest_dir):
	for filename in os.listdir(src_dir):
		if re.search(pat, filename):
			print "COPYING FILE [%s] TO [%s]" % (filename, dest_dir)
			cmd(build_command(["cp", 
												 "%s/%s" % (src_dir, filename),
												 "%s/%s" % (dest_dir, filename)]))

def mkdir(dir):
	cmd(build_command(["mkdir -p", dir]))

# MEAT AND POTATOES

def checkout(url):
	remove_and_create_dir(config.get('SVN'))
	print "In %s" % os.getcwd()
	cmd(build_command(["svn", "co", url, config.get('SVN')]))

def autoreconf():
	with pushd():
		cd(config.get('SVN'))
		cmd("autoreconf -vfi")

def configure():
	# Make the object directory
	with pushd():
		cd(config.get('SVN'))
		mkdir(config.get('OBJ'))
	# Do the configure from within it.
	with pushd():
		cd(config.get('OBJ'))
		cmd(build_command(["../configure", concat(["--prefix=", FINAL]), 
				"--with-toolchain=tvm", "--target=avr",
				"--with-wrapper=arduino"]))

def build():
	# 'make' in the object directory
	with pushd():
		cd(config.get('OBJ'))
		cmd("make")
		cmd(build_command(["make", concat(["DESTDIR=", config.get('DEST')]), "install"]))
	# Run the build script in the wrapper.
	# This generates virtual machines for multipl
	# targets (m328, m1280, 3.3V and 5V (8MHz and 16MHz, respectively))
	with pushd():
		cd(concat([config.get('SVN'), "/tvm/arduino"]))
		cmd("./build.sh")

def install():
	with pushd():
		cmd("make")
		cmd(build_command(["make", "install", concat(["DESTDIR=", config.get('FINAL')])]))

def make_destdirs():
	for d in DESTINATIONS:
		mkdir(d)

def subst_and_copy(file, source_dir, dest_dir):
	with open("%s/%s" % (source_dir, file), 'r') as input:
		dest_file = re.search("(.*).in", file).group(1)
		print "WRITING CONFIG FILE: %s/%s" % (dest_dir, dest_file)

		with open("%s/%s" % (dest_dir, dest_file), 'w') as output:
			for line in input:
				if re.search("@FINAL@", line):
					line = re.sub("@FINAL@", config.get('FINAL'), line)
				elif re.search("@PLATFORM@", line):
					line = re.sub("@PLATFORM@", re.search("(.*?)\.(.*)", file).group(1), line)
				elif re.search("@TVM_INST_ROOT@", line):
					line = re.sub("@TVM_INST_ROOT@", config.get('FINAL'), line)
				elif re.search("@PACKAGEVERSION@", line):
					line = re.sub("@PACKAGEVERSION@", config.get('YMD'), line)
				elif re.search("@DSTBIN@", line):
					line = re.sub("@DSTBIN@", config.get('FINAL'), line)

				# After replacements, write the line
				output.write(line)

def copy_config():
	with pushd():
		cd(config.get('SOURCE_CONF'))
		for filename in os.listdir(config.get('SOURCE_CONF')):
			if re.search(".*conf.in$", filename):
				subst_and_copy(filename, config.get('SOURCE_CONF'), config.get('DEST_CONF'))

def deployment_version():
	with pushd():
		with open("%s/deployment.version" % config.get('DEST_CONF'), 'w') as dv:
			dv.write(config.get('YMDHMS'))
			dv.close()

def chmod(mode, path, file):
	cmd(build_command(["chmod", mode, "%s/%s" % (path, file)]))

def copy():
	# Copy scripts directory to destination
	copy_dir(config.get('SOURCE_SCRIPTS'), config.get('DEST_BIN'))

	subst_and_copy("plumb.in", config.get('SOURCE_SCRIPTS'), config.get('DEST_BIN'))
	chmod("755", config.get('DEST_BIN'), "plumb")

	copy_files(".*.hex", config.get('SOURCE_FIRMWARE'), config.get('DEST_FIRMWARE'))
	
	copy_files("avrdude.conf", config.get('SOURCE_ARDUINO'), config.get('DEST_CONF'))
	
	copy_dir(config.get('SOURCE_LIB'), config.get('DEST_LIB'))	

def deb():
	remove_and_create_dir(config.get('DEST_DEBIAN'))
	
	for filename in os.listdir(config.get('SOURCE_DEBIAN')):
		if not re.search("in$", filename):
			copy_files(filename, config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
			chmod("755", config.get('DEST_DEBIAN'), filename)
	
	copy_files("preinst", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "preinst")

	copy_files("postrm", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "postrm")

	copy_files("preinst", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "preinst")

	copy_files("prerm", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "prerm")

	subst_and_copy("control.in", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "control")

	subst_and_copy("postinst.in", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "postinst")

	with pushd():
		cd(config.get('TEMP'))
		print "PACKAGING %s" % config.get('PACKAGE_NAME')
		cmd(build_command(["dpkg", "--build", config.get('PACKAGE_NAME'), "./"]))

# This needs to run `sudo' 
def rpm():
	with pushd():
		cd(TEMP())
		print "Converting to RPM"
		cmd(build_command(["alien", "--scripts", "-r", 
											 "%s_%s_i386.deb" % (config.get('PACKAGE_NAME'), 
											 config.get('YMD'))]))

def with_temp_dir(path):
		config.rebase('TEMP', path)

def all(url):
	checkout(url)
	autoreconf()
	configure()
	build()
	install()
	make_destdirs()
	copy_config()
	deployment_version()
	copy()
	deb()

def refresh_libs(path):
	config.rebase('LIB_PATH', path)
	copy_config()
	deployment_version()
	copy()
	deb()

#############################################
# COMMAND LINE PARSING

OPTIONS = [
	["checkout", "store", "CHECKOUT_SVN_URL", "Checkout a fresh tree from this Subversion URL.", 
		checkout ],
	["autoreconf", "store_true", "Run 'autoreconf -vfi' on the tree.",
		autoreconf ],
	["configure", "store_true", "Configure the tree for an AVR crossbuild.",
		configure ],
	["build", "store_true", "Build the tree and wrappers.", 
		build ],
	["install", "store_true", "Do a DESTDIR install.",
		install ],
	["make-destdirs", "store_true", "Make destination directories.", 
		make_destdirs ],
	["copy-config", "store_true", "Copy configuration files to destination.",
		copy_config ],
	["deployment-version", "store_true", "Write deployment version to config directory.",
		deployment_version],
	["copy", "store_true", "Copy libraries and compiled files into place.",
		copy],
	["deb", "store_true", "Build the Debian package (.deb).",
		deb],
	["all", "store", "ALL_SVN_URL", "Do everything up to this point.",
		all],
	["refresh-libs", "store", "LIB_PATH", "Re-run copy-config, copy, deb, and rpm.", refresh_libs],
	["with-temp-dir", "store", "TEMP_DIR", "Set the temp build directory.",
		with_temp_dir],
	["rpm", "store_true", "Convert the Debian package to a Fedora package (.rpm).", rpm]
	] 
	
parser = OptionParser()

for OPT in OPTIONS:
	if OPT[1] == "store_true":
		parser.add_option("--%s" % OPT[0], action=OPT[1], dest=OPT[0].upper(), help=OPT[2])
	elif OPT[1] == "store":
		parser.add_option("--%s" % OPT[0], action=OPT[1], dest=OPT[2], help=OPT[3])

# DO THE PARSE
(options, args) = parser.parse_args()

# HANDLE THE RESULTS
def ignored(str):
	return re.match("CHECKOUT", str) 

def call_handler(str, arg):
	for OPT in OPTIONS:
		if str == OPT[2]:
			print "CALLING [ %s ] " % OPT[2]
			# Invoke the last element of the list
			OPT[len(OPT) - 1](arg)

#for key, val in props(options).iteritems():
for key, val in props(options).iteritems():
	if val != None:
		call_handler(re.sub("-", "_", key), val)

