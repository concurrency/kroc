#!/bin/env python

import sys
import os
import errno
import shutil
import datetime
import glob
import subprocess
import zipfile

BUILD               = 'build'
OUTPUT              = 'output'
APP_DIR             = os.path.join(OUTPUT, 'Transterpreter')
BIN_DIR             = os.path.join(APP_DIR, 'bin')
POSIX_TVM_INC_DIR   = os.path.join(APP_DIR, 'posix', 'tvm', 'include')
POSIX_TVM_LIB_DIR   = os.path.join(APP_DIR, 'posix', 'tvm', 'lib')
POSIX_TVM_FW_DIR    = os.path.join(APP_DIR, 'posix', 'tvm', 'firmware')
ARDUINO_TVM_INC_DIR = os.path.join(APP_DIR, 'arduino', 'tvm', 'include')
ARDUINO_TVM_LIB_DIR = os.path.join(APP_DIR, 'arduino', 'tvm', 'lib')
ARDUINO_TVM_FW_DIR  = os.path.join(APP_DIR, 'arduino', 'tvm', 'firmware')
ARDUINO_PLUMBING_DIR= os.path.join(APP_DIR, 'arduino', 'plumbing-book')
ARDUINO_TVM_CONF_DIR= os.path.join(APP_DIR, 'arduino', 'tvm', 'conf')
JEDIT_DIR           = os.path.join(APP_DIR, 'jEdit')

BINARIES = "occ21.exe ilibr.exe mkoccdeps.exe tvm.exe".split()

build_date  = datetime.datetime.utcnow()
version     = build_date.strftime('%Y%m%d.%H%M')
svnversion  = subprocess.Popen(['svnversion', 
                                '-nc', 
                                '../../'], 
                               stdout=subprocess.PIPE).communicate()[0]
shortversion= 'Development version: %s (%s)' % (version, svnversion)


# from: 
# http://stackoverflow.com/questions/600268/mkdir-p-functionality-in-python
def mkdirs(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST:
            pass
        else: raise

def copy_file(src, dst):
    print '%s -> %s' % (src, dst)
    shutil.copy2(src, dst)

def copy_tree(src_dir, dest_dir, excludes=[]):
    print src_dir, dest_dir
    for root, dirs, files in os.walk(src_dir):
        path = root[len(src_dir) + 1:]
        dest_path = os.path.join(dest_dir, path)
        mkdirs(dest_path)
        for f in files:
            skip = False
            src_file = os.path.join(root, f)
            for f in excludes:
                if src_file.endswith(f): 
                    skip = True
            if not skip:
                copy_file(src_file, dest_path)
        if '.svn' in dirs:
            dirs.remove('.svn')  # don't visit .svn directories 

def copy_files(src, dest_path):
    files = glob.iglob(src)
    for f in files:
        copy_file(f, dest_path)

create_zip = not os.path.exists(APP_DIR)

mkdirs(APP_DIR)
mkdirs(BIN_DIR)

# General binaries
for binary in BINARIES:
    src = os.path.join('install/bin/', binary)
    copy_file(src, BIN_DIR)

# The stub
copy_file('build/stub/Transterpreter.exe', APP_DIR)

# py2exe generated files
copy_tree('py2exe-dist', BIN_DIR)

# perl files 
# FIXME: This will need to be generated natively at some point
import urllib2
mkdirs('perlfiles')
if not os.path.exists('perlfiles/plinker.exe'):
    f = open('perlfiles/plinker.exe', 'wb')
    f.write(urllib2.urlopen('http://lyderjacobsen.org/misc/plinker.exe').read())
    f.close()
if not os.path.exists('perlfiles/tce-dump.exe'):
    f = open('perlfiles/tce-dump.exe', 'wb')
    f.write(urllib2.urlopen('http://lyderjacobsen.org/misc/tce-dump.exe').read())
    f.close()
copy_files('perlfiles/*', BIN_DIR)

# tvm native libraries
# FIXME: These should go elsewhere
copy_files('install/lib/tvm/*.dll', BIN_DIR)


# posix module includes
mkdirs(POSIX_TVM_INC_DIR)
copy_tree('install/share/tvm/vtinclude', POSIX_TVM_INC_DIR)

# posix module libs
mkdirs(POSIX_TVM_LIB_DIR)
copy_tree('install/share/tvm/vtlib', POSIX_TVM_LIB_DIR)

# posix firmware
mkdirs(POSIX_TVM_FW_DIR)
copy_file('install/share/tvm/firmware/tvm-posix.tbc', POSIX_TVM_FW_DIR)

# SDL
copy_file('install/bin/SDL.dll', BIN_DIR)

# Arduino dirs
mkdirs(ARDUINO_TVM_INC_DIR)
mkdirs(ARDUINO_TVM_LIB_DIR)
mkdirs(ARDUINO_TVM_FW_DIR)
mkdirs(ARDUINO_TVM_CONF_DIR)

# Arduino firmware
copy_files('build/kroc-tvm-avr-*-*-wrapper/tvm-avr-*-*.hex', ARDUINO_TVM_FW_DIR)

# Arduino config files
for conf in glob.iglob('../../tvm/arduino/occam/conf/*.conf.in'):
    copy_file(conf, 
              os.path.join(ARDUINO_TVM_CONF_DIR, os.path.basename(conf)[:-2]))

# Arduino tools
# FIXME: These will have to be py2exed I think...
copy_file('../../tvm/arduino/scripts/binary-to-ihex', BIN_DIR)
copy_file('../../tvm/arduino/scripts/read-arduino', BIN_DIR)

# Arduino includes
copy_tree('../../tvm/arduino/occam/include', ARDUINO_TVM_INC_DIR, excludes='.ss')

# Plumbing examples
# FIXME: Should this live in the root for better visibility???
mkdirs(ARDUINO_PLUMBING_DIR)
copy_files('../../tvm/arduino/occam/book/ch*.occ', ARDUINO_PLUMBING_DIR)

# arduino module includes
copy_tree('install-avr/share/tvm/avr-vtinclude', ARDUINO_TVM_LIB_DIR)

# arduino module libs
copy_tree('install-avr/share/tvm/avr-vtlib', ARDUINO_TVM_LIB_DIR)

# avrdude
copy_file('install-avrdude/bin/avrdude.exe', BIN_DIR)
copy_file('install-avrdude/etc/avrdude.conf', BIN_DIR)
# FIXME: Figure out if we are going to include the giveio stuff

# jEdit
mkdirs(JEDIT_DIR)
copy_tree('build/jedit/jedit-program', JEDIT_DIR, excludes=['LatestVersion.jar'])
copy_file('build/occPlug/OccPlug.jar', os.path.join(JEDIT_DIR, 'jars'))
copy_file('../common/jEdit/occam.xml', os.path.join(JEDIT_DIR, 'modes'))

# Updater
copy_file('build/winupdater/JEditWinSparklePlugin.jar', os.path.join(JEDIT_DIR, 'jars'))

# Copy custom properties
copy_file('../common/jEdit/properties.props', os.path.join(JEDIT_DIR, 'properties'))

# Patch jEdit catalog
retcode = subprocess.call(["patch", 
           os.path.join(JEDIT_DIR, 'modes/catalog'), 
           '../common/jEdit/catalog.patch'], shell=True)
if retcode != 0:
   print 'Patch of jEdit catalog failed, aborting'
   sys.exit(1)

f = open(os.path.join(APP_DIR, 'VERSION.txt'), 'w')
f.write(shortversion)
f.close()

f = open(os.path.join(OUTPUT, 'VERSION'), 'w')
f.write(version)
f.close()

update_props = """\
options.jeditwinsparkleplugin.company=Concurrency.cc
options.jeditwinsparkleplugin.app=The Transterpreter
options.jeditwinsparkleplugin.version=%(version)s
options.jeditwinsparkleplugin.appcast=http://download.transterpreter.org/appcast/win-dev-zip.xml
""" % dict(version=version)

f = open(os.path.join(os.path.join(JEDIT_DIR, 'properties', 'update.props')), 'w')
f.write(update_props)
f.close()

if create_zip:
    zip_name = 'Transterpreter-win-dev-%s.zip' % version
    print 'Crating zip file: %s' % zip_name
    # Create a zip file
    zip = zipfile.ZipFile(os.path.join(OUTPUT, zip_name), 'w')
    for root, dirs, files in os.walk(APP_DIR):
        for f in files:
            src_file = os.path.join(root, f)
            dst_file = os.path.join(*src_file.split('\\')[1:])
            method = zipfile.ZIP_DEFLATED
            zip.write(src_file, dst_file, method)
            
    zip.close()
else:
  print 'WARNING: Not creating potentially dirty zipfile'
  print '         Transterpreter dir already existed in output directory'
  print '         Delete and rerun if zip is required.'

print 'Built app version: %s (%s)' % (version, shortversion,)
