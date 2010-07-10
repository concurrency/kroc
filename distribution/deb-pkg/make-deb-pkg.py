from __future__ import with_statement
from optparse import OptionParser
import contextlib
import datetime
import commands
import inspect
import time
import os
import re

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


# CONFIGURATION 

# Current date (YYYYMMDD)
now = datetime.datetime.now()
YMD = now.strftime("%Y%m%d")
YMDHMS = now.strftime("%Y%m%d%H%M%S")

# Package name
PACKAGE_NAME = "concurrency"

# Root for temporary build process.
TEMP_PATH = [ "/tmp" ]
def TEMP():
		return TEMP_PATH[0]

def PACKAGE_BUILD():
		return "%s/%s" % (TEMP(), PACKAGE_NAME)

# Location for checkout of fresh codebase
def SVN():
		return "%s/deb-pkg-src" % (TEMP())

# Location for the obj-avr build
def OBJ():
		return "%s/obj" % (SVN())

# For use when referencing into the source tree
# Functions, because I want to be able to change the 
# root path for the SVN tree.
def SOURCE_ARDUINO(url=SVN()):
	return "%s/%s" % (url, "/tvm/arduino")

def SOURCE_CONF(url=SVN()):
	return "%s/%s" % (url, "/tvm/arduino/occam/share/conf")

def SOURCE_SCRIPTS(url=SVN()):
	return "%s/%s" % (url, "/tvm/arduino/scripts")

def SOURCE_FIRMWARE(url=SVN()):
	return "%s/%s" % (url, "/tvm/arduino")

def SOURCE_LIB(url=SVN()):
	return "%s/%s" % (url, "/tvm/arduino/occam/include")

def SOURCE_DEBIAN(url=SVN()):
	return "%s/%s" % (url, "/distribution/deb-pkg/DEBIAN.in")

# Desired filesystem path for installation
INSTPATH = "opt/occam/arduino"
# Final installation path
FINAL    = "/%s" % (INSTPATH)

# Destdir installation paths
# All copy/install actions in the script should
# target these paths.
def DEST():
	return "%s/%s" % (TEMP(), PACKAGE_NAME)
def DEST_ROOT():
	return "%s/%s" % (DEST(), INSTPATH)
def DEST_BIN():
	return "%s/%s" % (DEST_ROOT(), "bin")
def DEST_SHARE():
	return "%s/%s" % (DEST_ROOT(), "share")
def DEST_FIRMWARE():
	return "%s/%s" % (DEST_SHARE(), "firmwares")
def DEST_CONF():
	return "%s/%s" % (DEST_SHARE(), "conf")
def DEST_LIB():
	return "%s/%s" % (DEST_ROOT(), "lib")
def DEST_DEBIAN():
	return "%s/%s" % (PACKAGE_BUILD(), "DEBIAN")

#DESTINATIONS  = [DEST_ROOT, DEST_BIN, DEST_SHARE,
#								 DEST_FIRMWARE, DEST_CONF, DEST_LIB, DEST_DEBIAN]


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
	remove_and_create_dir(SVN())
	print "In %s" % os.getcwd()
	cmd(build_command(["svn", "co", url, SVN()]))

def autoreconf():
	with pushd():
		cd(SVN())
		cmd("autoreconf -vfi")

def configure():
	# Make the object directory
	with pushd():
		cd(SVN())
		mkdir(OBJ())
	# Do the configure from within it.
	with pushd():
		cd(OBJ())
		cmd(build_command(["../configure", concat(["--prefix=", FINAL]), 
				"--with-toolchain=tvm", "--target=avr",
				"--with-wrapper=arduino"]))

def build():
	# 'make' in the object directory
	with pushd():
		cd(OBJ())
		cmd("make")
		cmd(build_command(["make", concat(["DESTDIR=", DEST()]), "install"]))
	# Run the build script in the wrapper.
	# This generates virtual machines for multipl
	# targets (m328, m1280, 3.3V and 5V (8MHz and 16MHz, respectively))
	with pushd():
		cd(concat([SVN, "/tvm/arduino"]))
		cmd("./build.sh")

def install():
	with pushd():
		cmd("make")
		cmd(build_command(["make", "install", concat(["DESTDIR=", FINAL])]))

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
					line = re.sub("@FINAL@", FINAL, line)
				elif re.search("@PLATFORM@", line):
					line = re.sub("@PLATFORM@", re.search("(.*?)\.(.*)", file).group(1), line)
				elif re.search("@TVM_INST_ROOT@", line):
					line = re.sub("@TVM_INST_ROOT@", FINAL, line)
				elif re.search("@PACKAGEVERSION@", line):
					line = re.sub("@PACKAGEVERSION@", YMD, line)
				elif re.search("@DSTBIN@", line):
					line = re.sub("@DSTBIN@", FINAL, line)

				# After replacements, write the line
				output.write(line)

def copy_config(path=None):
	if path == None:
		path = SVN()

	with pushd():
		cd(SOURCE_CONF(path))
		for filename in os.listdir(SOURCE_CONF(path)):
			if re.search(".*conf.in$", filename):
				subst_and_copy(filename, SOURCE_CONF(path), DEST_CONF())

def deployment_version():
	with pushd():
		with open("%s/deployment.version" % DEST_CONF(), 'w') as dv:
			dv.write(YMDHMS)
			dv.close()

def chmod(mode, path, file):
	cmd(build_command(["chmod", mode, "%s/%s" % (path, file)]))

def copy(url=SVN()):
	# Copy scripts directory to destination
	copy_dir(SOURCE_SCRIPTS(url), DEST_BIN())

	subst_and_copy("plumb.in", SOURCE_SCRIPTS(url), DEST_BIN())
	chmod("755", DEST_BIN(), "plumb")

	copy_files(".*.hex", SOURCE_FIRMWARE(url), DEST_FIRMWARE())
	
	copy_files("avrdude.conf", SOURCE_ARDUINO(url), DEST_CONF())
	
	copy_dir(SOURCE_LIB(url), DEST_LIB())	

def deb(url=SVN()):
	remove_and_create_dir(DEST_DEBIAN())
	
	for filename in os.listdir(SOURCE_DEBIAN(url)):
		if not re.search("in$", filename):
			copy_files(filename, SOURCE_DEBIAN(url), DEST_DEBIAN())
			chmod("755", DEST_DEBIAN(), filename)
	
	copy_files("preinst", SOURCE_DEBIAN(url), DEST_DEBIAN())
	chmod("755", DEST_DEBIAN(), "preinst")

	copy_files("postrm", SOURCE_DEBIAN(url), DEST_DEBIAN())
	chmod("755", DEST_DEBIAN(), "postrm")

	copy_files("preinst", SOURCE_DEBIAN(url), DEST_DEBIAN())
	chmod("755", DEST_DEBIAN(), "preinst")

	copy_files("prerm", SOURCE_DEBIAN(url), DEST_DEBIAN())
	chmod("755", DEST_DEBIAN(), "prerm")

	subst_and_copy("control.in", SOURCE_DEBIAN(url), DEST_DEBIAN())
	chmod("755", DEST_DEBIAN(), "control")

	subst_and_copy("postinst.in", SOURCE_DEBIAN(url), DEST_DEBIAN())
	chmod("755", DEST_DEBIAN(), "postinst")

	with pushd():
		cd(TEMP())
		print "PACKAGING %s" % PACKAGE_NAME
		cmd(build_command(["dpkg", "--build", PACKAGE_NAME, "./"]))

# This needs to run `sudo' 
def rpm():
	with pushd():
		cd(TEMP())
		print "Converting to RPM"
		cmd(build_command(["alien", "--scripts", "-r", 
											 "%s_%s_i386.deb" % (PACKAGE_NAME, YMD)]))

def with_temp_dir(path):
		print "SETTING TEMP PATH"
		TEMP_PATH[0] = path
		print "SET TO %s" % TEMP()

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
	copy_config(path)
	deployment_version()
	copy(path)
	deb(path)

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

def call_handler(str):
	tag = str.lower()
	for OPT in OPTIONS:
		if tag == OPT[0]:
			# Invoke the last element of the list
			OPT[len(OPT) - 1]()

for key, val in props(options).iteritems():
	if (key == "LIB_PATH") and (val != None):
		print "PATH IS %s" % options.LIB_PATH
		refresh_libs(options.LIB_PATH)
	if (key == "TEMP_DIR") and (val != None):
		with_temp_dir(options.TEMP_DIR)

for key, val in props(options).iteritems():
	if (key == "CHECKOUT_SVN_URL") and (val != None):
		checkout(options.CHECKOUT_SVN_URL)
	if (key == "ALL_SVN_URL") and (val != None):
		all(options.ALL_SVN_URL)
	elif val:
		print "*** RUNNING [ %s ] ***" % key
		call_handler(re.sub("_", "-", key))

