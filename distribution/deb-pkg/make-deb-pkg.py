from __future__ import with_statement
from optparse import OptionParser
import contextlib
import datetime
import commands
import os


# CONFIGURATION 

# Current date (YYYYMMDD)
now = datetime.datetime.now()
YMD = now.strftime("%Y%m%d")

# Package name
PACKAGE_NAME = "concurrency"

# Root for temporary build process.
TEMP = "/tmp"
# Location for checkout of fresh codebase
SVN  = "%s/dpkg-%s" % (TEMP, YMD)
# Location for the obj-avr build
OBJ  = "%s/obj" % (SVN)

# Desired filesystem path for installation
INSTPATH = "opt/occam/arduino"
# Final installation path
FINAL    = "/%s" % (INSTPATH)

# Destdir installation paths
# All copy/install actions in the script should
# target these paths.
DEST       = "%s/%s" % (TEMP, PACKAGE_NAME)
DEST_ROOT  = "%s/%s" % (DEST, INSTPATH)
DEST_BIN   = "%s/%s" % (DEST_ROOT, "bin")
DEST_SHARE = "%s/%s" % (DEST_ROOT, "share")
DEST_CONF  = "%s/%s" % (DEST_ROOT, "conf")
DEST_LIB   = "%s/%s" % (DEST_ROOT, "lib")

# UTILITY FUNCTIONS 
def cd(dir):
	os.chdir(dir)

@contextlib.contextmanager
def pushd():
    curdir= os.getcwd()
    try: yield
    finally: cd(curdir)

def build_command(ls):
	return ' '.join(ls)

def cmd(str):
	print "[ %s ]" % str
	result = commands.getstatusoutput(str)

def remove_and_create_dir(dir):
	cmd(build_command(["rm", "-rf", dir]))
	cmd(build_command(["mkdir", dir]))

def remove_dir(dir):
	cmd(build_command(["rm" "-rf", dir]))

# MEAT AND POTATOES

def checkout(SOURCE_URL):
	remove_and_create_dir(SVN)
	print "In %s" % os.getcwd()
	cmd(build_command(["svn", "co", SOURCE_URL, SVN]))

def autoreconf():
	with pushd():
		cd(SVN)
		cmd("autoreconf -vfi")

#############################################
# COMMAND LINE PARSING

parser = OptionParser()
parser.add_option("--checkout", action="store", 
									dest="CHECKOUT", help="Checkout a fresh tree from this Subversion URL.")

parser.add_option("--autoreconf", action="store_true",
									dest="AUTORECONF", default=False,
									help="Run 'autoreconf -vfi' on the tree.")

parser.add_option("--configure", action="store_true",
									dest="CONFIGURE", default=False,
									help="Configure the tree for an AVR crossbuild.")

# DO THE PARSE
(options, args) = parser.parse_args()

# HANDLE THE RESULTS
if options.CHECKOUT:
	checkout(options.CHECKOUT)
elif options.AUTORECONF:
	autoreconf()
