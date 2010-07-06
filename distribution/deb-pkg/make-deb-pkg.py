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
TEMP = "/tmp"
PACKAGE_BUILD = "%s/%s" % (TEMP, PACKAGE_NAME)

# Location for checkout of fresh codebase
SVN  = "%s/dpkg-%s" % (TEMP, YMD)
# Location for the obj-avr build
OBJ  = "%s/obj" % (SVN)

# For use when referencing into the source tree
SOURCE_ARDUINO  = "%s/%s" % (SVN, "/tvm/arduino")
SOURCE_CONF     = "%s/%s" % (SVN, "/tvm/arduino/occam/share/conf")
SOURCE_SCRIPTS  = "%s/%s" % (SVN, "/tvm/arduino/scripts")
SOURCE_FIRMWARE = "%s/%s" % (SVN, "/tvm/arduino")
SOURCE_LIB      = "%s/%s" % (SVN, "/tvm/arduino/occam/include")
SOURCE_DEBIAN   = "%s/%s" % (SVN, "/distribution/deb-pkg/DEBIAN.in")

# Desired filesystem path for installation
INSTPATH = "opt/occam/arduino"
# Final installation path
FINAL    = "/%s" % (INSTPATH)

# Destdir installation paths
# All copy/install actions in the script should
# target these paths.
DEST          = "%s/%s" % (TEMP, PACKAGE_NAME)
DEST_ROOT     = "%s/%s" % (DEST, INSTPATH)
DEST_BIN      = "%s/%s" % (DEST_ROOT, "bin")
DEST_SHARE    = "%s/%s" % (DEST_ROOT, "share")
DEST_FIRMWARE = "%s/%s" % (DEST_SHARE, "firmares")
DEST_CONF     = "%s/%s" % (DEST_SHARE, "conf")
DEST_LIB      = "%s/%s" % (DEST_ROOT, "lib")
DEST_DEBIAN   = "%s/%s" % (PACKAGE_BUILD, "DEBIAN")

DESTINATIONS  = [DEST_ROOT, DEST_BIN, DEST_SHARE,
								 DEST_FIRMWARE, DEST_CONF, DEST_LIB, DEST_DEBIAN]


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

def checkout(SOURCE_URL):
	remove_and_create_dir(SVN)
	print "In %s" % os.getcwd()
	cmd(build_command(["svn", "co", SOURCE_URL, SVN]))

def autoreconf():
	with pushd():
		cd(SVN)
		cmd("autoreconf -vfi")

def configure():
	# Make the object directory
	with pushd():
		cd(SVN)
		mkdir(OBJ)
	# Do the configure from within it.
	with pushd():
		cd(OBJ)
		cmd(build_command(["../configure", concat(["--prefix=", FINAL]), 
				"--with-toolchain=tvm", "--target=avr",
				"--with-wrapper=arduino"]))

def build():
	# 'make' in the object directory
	with pushd():
		cd(OBJ)
		cmd("make")
		cmd(build_command(["make", concat(["DESTDIR=", DEST]), "install"]))
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

def copy_config():
	with pushd():
		cd(SOURCE_CONF)
		for filename in os.listdir(SOURCE_CONF):
			if re.search(".*conf.in$", filename):
				subst_and_copy(filename, SOURCE_CONF, DEST_CONF)

def deployment_version():
	with pushd():
		with open("%s/deployment.version" % DEST_CONF, 'w') as dv:
			dv.write(YMDHMS)
			dv.close()

def chmod(mode, path, file):
	cmd(build_command(["chmod", mode, "%s/%s" % (path, file)]))

def copy():
	# Copy scripts directory to destination
	copy_dir(SOURCE_SCRIPTS, DEST_BIN)

	subst_and_copy("plumb.in", SOURCE_SCRIPTS, DEST_BIN)
	chmod("755", DEST_BIN, "plumb")

	copy_files(".*.hex", SOURCE_FIRMWARE, DEST_FIRMWARE)
	
	copy_files("avrdude.conf", SOURCE_ARDUINO, DEST_CONF)
	
	copy_dir(SOURCE_LIB, DEST_LIB)	

def deb():
	remove_and_create_dir(DEST_DEBIAN)

	for filename in os.listdir(SOURCE_DEBIAN):
		if not re.search("in$", filename):
			copy_files(filename, SOURCE_DEBIAN, DEST_DEBIAN)
			chmod("755", DEST_DEBIAN, filename)
	
	copy_files("preinst", SOURCE_DEBIAN, DEST_DEBIAN)
	chmod("755", DEST_DEBIAN, "preinst")

	copy_files("postrm", SOURCE_DEBIAN, DEST_DEBIAN)
	chmod("755", DEST_DEBIAN, "postrm")

	copy_files("preinst", SOURCE_DEBIAN, DEST_DEBIAN)
	chmod("755", DEST_DEBIAN, "preinst")

	copy_files("prerm", SOURCE_DEBIAN, DEST_DEBIAN)
	chmod("755", DEST_DEBIAN, "prerm")

	subst_and_copy("control.in", SOURCE_DEBIAN, DEST_DEBIAN)
	chmod("755", DEST_DEBIAN, "control")

	subst_and_copy("postinst.in", SOURCE_DEBIAN, DEST_DEBIAN)
	chmod("755", DEST_DEBIAN, "postinst")

	with pushd():
		cd(TEMP)
		print "PACKAGING %s" % PACKAGE_NAME
		cmd(build_command(["dpkg", "--build", PACKAGE_NAME, "./"]))

# This needs to run `sudo' 
def rpm():
	with pushd():
		cd(TEMP)
		print "Converting to RPM"
		cmd(build_command(["alien", "--scripts", "-r", 
											 "%s_%s_i386.deb" % (PACKAGE_NAME, YMD)]))


def all():
	checkout(options.ALL)
	autoreconf()
	configure()
	build()
	install()
	make_destdirs()
	copy_config()
	deployment_version()
	copy()
	deb()
	rpm()

def refresh_libs():
	copy_config()
	deployment_version()
	copy()
	deb()
	rpm()

#############################################
# COMMAND LINE PARSING

OPTIONS = [
	["checkout", "store", "Checkout a fresh tree from this Subversion URL.", 
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
	["rpm", "store_true", "Convert the Debian package to a Fedora package (.rpm).",
		rpm],
	["all", "store", "Do everything up to this point.",
		all],
	["refresh-libs", "store_true", "Re-run copy-config, copy, deb, and rpm.",
		refresh_libs]
	] 
	
parser = OptionParser()

for OPT in OPTIONS:
	parser.add_option("--%s" % OPT[0], action=OPT[1],
										dest=OPT[0].upper(), help=OPT[2])

# DO THE PARSE
(options, args) = parser.parse_args()

# HANDLE THE RESULTS
def ignored(str):
	return re.match("CHECKOUT", str) 

def call_handler(str):
	tag = str.lower()
	for OPT in OPTIONS:
		if tag == OPT[0]:
			OPT[3]()


for key, val in props(options).iteritems():
	if (key == "CHECKOUT") and (val != None):
		checkout(options.CHECKOUT)
	elif val:
		print "*** RUNNING [ %s ] ***" % key
		call_handler(re.sub("_", "-", key))

