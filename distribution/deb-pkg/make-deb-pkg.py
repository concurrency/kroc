from __future__ import with_statement
from optparse import OptionParser
import contextlib
import datetime
import commands
import inspect
import urllib
import time
import os
import sys
import re
import zipfile

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
	print "CD [ %s ]" % dir
	os.chdir(dir)

def build_command(ls):
	return ' '.join(ls)

def cmd(str):
	print "COMMAND [ %s ]" % str
	result = commands.getstatusoutput(str)

def remove_dir(dir):
	print " -- REMOVING -- %s" % dir
	cmd(build_command(["rm", "-rf", dir]))

def remove_files(filespec):
	print " -- REMOVING FILES -- "
	if not os.path.exists(filespec):
		print "FILE DOES NOT EXIST: %s" % filespec
	else:
		cmd(build_command(['rm', filespec]))

def remove_and_create_dir(dir):
	remove_dir(dir)
	mkdir(dir)

def copy_dir(src, dst):
	# print "DIRECTORY COPY [%s] TO [%s]" % (src, dst)
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

def mkdir(dir):
	cmd(build_command(["mkdir -p", dir]))

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

def checkout(url):
	header("RUNNING CHECKOUT")
	config.refresh()
	if url == 'trunk':
		url = config.get('SVN_TRUNK')

	remove_and_create_dir(config.get('SVN'))

	# print "In %s" % os.getcwd()
	cmd(build_command(["svn", "co", url, config.get('SVN')]))

def checkout_to(url, path):
	header("RUNNING CHECKOUT TO...")
	config.refresh()
	cmd(build_command(["svn", "co", url, path]))

def autoreconf():
	header("RUNNING AUTORECONF")
	with pushd():
		cd(config.get('SVN'))
		cmd("autoreconf -vfi")

def configure():
	header("RUNNING CONFIGURE")

	# Make the object directory
	with pushd():
		cd(config.get('SVN'))
		mkdir(config.get('OBJ'))
	# Do the configure from within it.
	with pushd():
		cd(config.get('OBJ'))
		TOOLCHAIN = config.get('TOOLCHAIN')
		TARGET    = config.get('TARGET')
		WRAPPER   = config.get('WRAPPER')
		PARAMS    = ''
		if TOOLCHAIN != 'kroc':
			PARAMS += "--with-toolchain=%s " % config.get('TOOLCHAIN')
		if TARGET != 'posix':
			PARAMS += "--target=%s " % config.get('TARGET')
		if WRAPPER != 'posix':
				PARAMS += "--with-wrapper=%s" % config.get('WRAPPER')

		cmd(build_command(["../configure", concat(["--prefix=", config.get('FINAL')]), 
											PARAMS]))

def build():
	header("RUNNING MAKE")

	# 'make' in the object directory
	with pushd():
		cd(config.get('OBJ'))
		cmd("make")
		cmd(build_command(["make", concat(["DESTDIR=", config.get('DEST')]), "install"]))

	if (config.get('WRAPPER') == 'arduino'):
		header("BUILDING ARDUINO WRAPPER")
		# Run the build script in the wrapper.
		# This generates virtual machines for multipl
		# targets (m328, m1280, 3.3V and 5V (8MHz and 16MHz, respectively))
		with pushd():
			header("BUILDING  FIRMWARES")
			cd(concat([config.get('SVN'), "/tvm/arduino"]))
			cmd("./build.sh")

def install():
	header("DOING MAKE INSTALL")
	with pushd():
		cd(config.get('OBJ'))
		cmd("make")
		cmd(build_command(["make", "install", concat(["DESTDIR=", config.get('DEST')])]))

def make_destdirs():
	header("MAKING DESTINATION DIRECTORIES")
	DESTINATIONS = [
		'DEST_ROOT', 
		'DEST_BIN', 
		'DEST_SHARE',
		'DEST_CONF', 
		'DEST_DEBIAN', 
		'DEST_FIRMWARE',
		'DEST_OCCPLUG_JARS', 
		'DEST_OCCPLUG_MODES',
		'DEST_OCCPLUG_DEBIAN', 
		'DEST_INCLUDE',
		'DEST_META_DEBIAN'
		] 
	for d in DESTINATIONS:
		mkdir(config.get(d))

def subst_and_copy(file, source_dir, dest_dir):
	print "DOING SUBST ON [ %s/%s ]" % (source_dir, file)

	with open("%s/%s" % (source_dir, file), 'r') as input:
		dest_file = re.search("(.*).in", file).group(1)
		# print "WRITING CONFIG FILE: %s/%s" % (dest_dir, dest_file)
	
		with open("%s/%s" % (dest_dir, dest_file), 'w') as output:
			for line in input:
				for key, val in config.CFG.iteritems():
					# print "LOOKING FOR '@%s@' in %s" % (key, line)
					if re.search(re.compile('@%s@' % key), line):
						print "FOUND %s, NOW %s " % (key, val)
						line = re.sub(re.compile('@%s@' % key), val, line)

				if re.search("@PLATFORM@", line):
					line = re.sub("@PLATFORM@", re.search("(.*?)\.(.*)", file).group(1), line)
				
				if re.search("@KROC-SETUP@", line):
					#if config.get('TOOLCHAIN') in ['kroc', 'tvm']:
					#	line = re.sub("@KROC-SETUP@", "kroc-setup.sh", line)
					#else:
					# Actually... can we change the name of this file?
						line = re.sub("@KROC-SETUP@", 
													"%s-occam-setup.sh" % config.get('TARGET'),
													line)

				# After replacements, write the line
				output.write(line)

def copy_arduino_config():
	header("COPYING CONFIGURATION FILES")	
	with pushd():
		cd(config.get('SOURCE_CONF'))
		for filename in os.listdir(config.get('SOURCE_CONF')):
			if re.search(".*conf.in$", filename):
				subst_and_copy(filename, config.get('SOURCE_CONF'), config.get('DEST_CONF'))

def deployment_version():
	header("WRITING DEPLOYMENT VERSION")
	with pushd():
		with open("%s/deployment.version" % config.get('DEST_CONF'), 'w') as dv:
			dv.write(config.get('YMDHMS'))
			dv.close()

def chmod(mode, path, file):
	cmd(build_command(["chmod", mode, "%s/%s" % (path, file)]))

def copy_arduino_build():
	header("COPYING ARDUINO BUILD PRODUCTS TO PACKAGE")

	# Copy scripts directory to destination
	copy_dir(config.get('SOURCE_SCRIPTS'), config.get('DEST_BIN'))

	subst_and_copy("plumb.in", config.get('SOURCE_SCRIPTS'), config.get('DEST_BIN'))
	chmod("755", config.get('DEST_BIN'), "plumb")

	header("COPYING FIRMWARE FILES")
	copy_files(".*.hex", config.get('SOURCE_ARDUINO'), config.get('DEST_FIRMWARE'))
	
	#copy_files("avrdude.conf", config.get('SOURCE_ARDUINO'), config.get('DEST_CONF'))

	# What was going into LIB should actually go into SHARE. 	
	copy_dir(config.get('SOURCE_INCLUDE'), config.get('DEST_INCLUDE'))	

def copy_native_build():
	header("COPYING NATIVE BUILD PRODUCTS TO PACKAGE")
	# No-op for native? "make install" does everything...

def copy_native_tvm_build():
	header("COPYING NATIVE TVM BUILD PRODUCTS TO PACKAGE")

def deb():
	header("BUILDING DEBIAN PACKAGE")
	
	remove_unwanted_build_products()
	remove_and_create_dir(config.get('DEST_DEBIAN'))
	
	for filename in os.listdir(config.get('SOURCE_DEBIAN')):
		if not re.search("in$", filename):
			copy_files(filename, config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
			chmod("755", config.get('DEST_DEBIAN'), filename)
	
	copy_files("preinst", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "preinst")

	copy_files("postrm", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "postrm")

	copy_files("prerm", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "prerm")

	print "SOURCE: %s" % config.get('SOURCE_DEBIAN')
	subst_and_copy("control.in", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "control")

	subst_and_copy("postinst.in", config.get('SOURCE_DEBIAN'), config.get('DEST_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "postinst")

	with pushd():
		cd(config.get('TEMP'))
		print "PACKAGING %s" % config.get('PACKAGE_NAME')
		cmd(build_command(["dpkg", "--build", config.get('PACKAGE_NAME'), "./"]))


def meta_deb():
	header("BUILDING META PACKAGE")

	for filename in os.listdir(config.get('SOURCE_META_DEBIAN')):
		if not re.search("in$", filename):
			copy_files(filename, config.get('SOURCE_META_DEBIAN'), config.get('DEST_META_DEBIAN'))
			chmod("755", config.get('DEST_META_DEBIAN'), filename)
	
	copy_files("preinst", config.get('SOURCE_META_DEBIAN'), config.get('DEST_META_DEBIAN'))
	chmod("755", config.get('DEST_META_DEBIAN'), "preinst")

	copy_files("postrm", config.get('SOURCE_META_DEBIAN'), config.get('DEST_META_DEBIAN'))
	chmod("755", config.get('DEST_META_DEBIAN'), "postrm")

	copy_files("prerm", config.get('SOURCE_META_DEBIAN'), config.get('DEST_META_DEBIAN'))
	chmod("755", config.get('DEST_META_DEBIAN'), "prerm")

	print "SOURCE: %s" % config.get('SOURCE_META_DEBIAN')
	subst_and_copy("control.in", config.get('SOURCE_META_DEBIAN'), config.get('DEST_META_DEBIAN'))
	chmod("755", config.get('DEST_META_DEBIAN'), "control")

	with pushd():
		cd(concat([config.get('TEMP_ROOT')]))
		print "PACKAGING %s" % config.get('META_NAME')
		cmd(build_command(["dpkg", "--build", config.get('META_NAME'), "./"]))
		


# This needs to run `sudo' 
def rpm():
	header("ALIENIFYING DEB PACKAGE INTO RPM")
	with pushd():
		cd(config.get('TEMP'))
		print "Converting to RPM"
		cmd(build_command(["alien", "--scripts", "-r", 
											 "%s_%s_i386.deb" % (config.get('PACKAGE_NAME'), 
											 config.get('YMD'))]))

def with_temp_dir(path):
		config.refresh()
		header('SETTING TEMP DIR TO %s/%s' % (path, config.get('BUILD_ARCHITECTURE')))
		config.rebase('TEMP_ROOT', '%s/%s' % (path, config.get('BUILD_ARCHITECTURE')))

def with_lib_path(path):
	header('SETTING LIB PATH TO %s' % path)
	config.rebase('SVN', path)

def echo_config():
	for key, val in config.CFG.iteritems():
		print "%s\t\t%s" % (key, val)


def build_avr():
	header("SETTING AVR BUILD OPTIONS")
	config.rebase('TOOLCHAIN', 'tvm')
	config.rebase('TARGET', 'avr')
	config.rebase('WRAPPER', 'arduino')
	config.set('PACKAGE_DESCRIPTION', 'occam for the AVR/Arduino.')
	#echo_config()

def build_native_kroc():
	header("SETTING NATIVE BUILD OPTIONS")
	config.rebase('TOOLCHAIN', 'kroc')
	config.rebase('TARGET', 'posix')
	config.rebase('WRAPPER', 'posix')
	config.set('PACKAGE_DESCRIPTION', 'A native occam-pi for POSIX platforms.')
	#echo_config()
		
def build_native_tvm():
	header("SETTING NATIVE TVM BUILD OPTIONS")
	config.rebase('TOOLCHAIN', 'tvm')
	config.rebase('TARGET', 'posix')
	config.rebase('WRAPPER', 'posix')
	config.set('PACKAGE_DESCRIPTION', 'occam-pi on the Transterpreter for POSIX platforms.')
	#echo_config()

def ttw():
	return [config.get('TOOLCHAIN'), config.get('TARGET'), config.get('WRAPPER')]

def all(url):

	if (url != 'LOCAL'):
		checkout(url)

	autoreconf()
	configure()
	build()
	install()

	if ['tvm', 'avr', 'arduino'] == ttw():
		make_destdirs()
		copy_arduino_config()
		copy_arduino_build()
	if ['kroc', 'posix', 'posix'] == ttw():
		copy_native_build()
	if ['tvm', 'posix', 'posix'] == ttw():
		copy_native_tvm_build()

def remove_unwanted_build_products():
	header("DELETE UNWANTED BUILD PRODUCTS")

	# These are not dev packages; I'll pull this out of all of them.
	# (There are more things I could remove, but... this is a start.
	# That, and they conflict with each-other.)
	remove_dir(concat([config.get('DEST'), '/usr/share/aclocal']))

	if config.get('WRAPPER') == 'arduino':
		remove_dir(concat([config.get('DEST'), '/usr/share/kroc']))
		remove_dir(concat([config.get('DEST'), '/usr/include/tvm']))
		remove_files(concat([config.get('DEST'), '/usr/lib/libtvm.a']))
		remove_files(concat([config.get('DEST'), '/usr/bin/reset-arduino']))
		remove_files(concat([config.get('DEST'), '/usr/bin/arduino-firmware-upload']))
		remove_files(concat([config.get('DEST'), '/usr/bin/arduino-upload']))

	if config.get('TOOLCHAIN') == 'tvm':
		#MAN1 = ['occamdoc.1', 'mkoccdeps.1', 'occ21.1']
		#MAN3 = ['libhostio-inmos.3', 'libcourse-cycles.3', 'libfile.3', 'libcourse-nets.3', 'libstring-inmos.3', 'libcourse-utils.3', 'libproc.3', 'libsock.3', 'libstreamio-inmos.3', 'libconvert-inmos.3', 'libmath-inmos.3']
		#for M1 in MAN1:
		#	remove_files(concat([config.get('DEST'), '/usr/share/man/man1/%s'	% M1]))
		#for M3 in MAN3:
		#	remove_files(concat([config.get('DEST'), '/usr/share/man/man1/%s'	% M3]))
	
		# How about... no man files in the Transterpreter.
		remove_dir(concat([config.get('DEST'), '/usr/share/man']))
		remove_dir(concat([config.get('DEST'), '/usr/share/kroc']))
		remove_files(concat([config.get('DEST'), '/usr/bin/cdxview']))
		remove_files(concat([config.get('DEST'), '/usr/bin/netbard']))
		
		DUPBIN = ['kroc-setup.sh', 'ilibr', 'occamdoc', 'occbuild', 'kroc-setup.csh', 'occ21', 'mkoccdeps']
		for DUP in DUPBIN:
			remove_files(concat([config.get('DEST'), '/usr/bin/%s'	% DUP]))


def refresh_libs():
	if config.get('WRAPPER') == 'arduino':
		make_destdirs()
		copy_arduino_config()
		copy_arduino_build()

def unzip_file_into_dir(file, dir):
	print "[unzip %s -> %s" % (file, dir)
	zfobj = zipfile.ZipFile(file)
	for name in zfobj.namelist():
		if name.endswith('/'):
			os.mkdir(os.path.join(dir, name))
		else:
			outfile = open(os.path.join(dir, name), 'wb')
			outfile.write(zfobj.read(name))
			outfile.close()

def occplug_deb():
	header("BUILDING occPlug DEBIAN PACKAGE")

	remove_and_create_dir(config.get('DEST_OCCPLUG_DEBIAN'))
	
	for filename in os.listdir(config.get('SOURCE_OCCPLUG_DEBIAN')):
		if not re.search("in$", filename):
			copy_files(filename, config.get('SOURCE_OCCPLUG_DEBIAN'), config.get('DEST_OCCPLUG_DEBIAN'))
			chmod("755", config.get('DEST_OCCPLUG_DEBIAN'), filename)
	
	copy_files("preinst", config.get('SOURCE_OCCPLUG_DEBIAN'), config.get('DEST_OCCPLUG_DEBIAN'))
	chmod("755", config.get('DEST_DEBIAN'), "preinst")

	copy_files("postrm", config.get('SOURCE_OCCPLUG_DEBIAN'), config.get('DEST_OCCPLUG_DEBIAN'))
	chmod("755", config.get('DEST_OCCPLUG_DEBIAN'), "postrm")

	copy_files("preinst", config.get('SOURCE_OCCPLUG_DEBIAN'), config.get('DEST_OCCPLUG_DEBIAN'))
	chmod("755", config.get('DEST_OCCPLUG_DEBIAN'), "preinst")

	copy_files("prerm", config.get('SOURCE_OCCPLUG_DEBIAN'), config.get('DEST_OCCPLUG_DEBIAN'))
	chmod("755", config.get('DEST_OCCPLUG_DEBIAN'), "prerm")

	print "SOURCE: %s" % config.get('SOURCE_OCCPLUG_DEBIAN')
	subst_and_copy("control.in", config.get('SOURCE_OCCPLUG_DEBIAN'), config.get('DEST_OCCPLUG_DEBIAN'))
	chmod("755", config.get('DEST_OCCPLUG_DEBIAN'), "control")

	# Copy in the highlighting file.
	copy_files('occam-pi.xml', concat([config.get('SOURCE_COMMON'), '/jEdit']), config.get('DEST_OCCPLUG_MODES'))

	with pushd():
		cd(config.get('TEMP'))
		print "PACKAGING %s" % config.get('OCCPLUG_PACKAGE_NAME')
		cmd(build_command(["dpkg", "--build", config.get('OCCPLUG_PACKAGE_NAME'), "./"]))

def build_occplug():
	header("BUILDING OCCPLUG")
	config.refresh() 
	config.rebase('TEMP', '@TEMP_ROOT@/occPlug')
	remove_dir(config.get('TEMP'))

	src_ext = '/tools/occplug' 
	dist_ext = '/distribution/deb-pkg'

	#srcpath = concat([config.get('TEMP'), '/src', src_ext])
	#mkdir(srcpath)
	
	#distpath = concat([config.get('TEMP'), '/src', dist_ext])
	#mkdir(distpath)
	config.set('OCCPLUG_DISTRIBUTION', 
						concat([config.get('SVN'), dist_ext]))

	#config.rebase('SOURCE_OCCPLUG', srcpath)

	make_destdirs()
	remove_dir(concat([config.get('TEMP'), '/', config.get('PACKAGE_NAME')]))

	# remove_and_create_dir(config.get('TEMP'))
	# remove_and_create_dir(config.get('DEST_OCCPLUG'))	

	print 'SOURCE OCCPLUG: %s' % config.get('SOURCE_OCCPLUG')

	#checkout_to(concat([config.get('SVN_TRUNK'), src_ext]), config.get('SOURCE_OCCPLUG'))
	#checkout_to(concat([config.get('SVN_TRUNK'), dist_ext]), distpath)

	zipfile = 'ErrorList-1.5-bin.zip'
	jarfile = 'ErrorList.jar'

	with pushd():
		cd(config.get('SOURCE_OCCPLUG'))
		cmd(build_command(['wget', config.get('ERRORLIST_URL')]))
		unzip_file_into_dir(zipfile, config.get('DEST_OCCPLUG_JARS'))
	
		# Move the ErrorList straight to the destination; we can build against it there.
		#cmd(build_command(['mv', jarfile, config.get('DEST_OCCPLUG')]))
		cmd(build_command(['ant', 
											'-Djedit.install.dir=/usr/share/jedit', 
											concat(['-Dinstall.dir=', config.get('DEST_OCCPLUG_JARS')]),
											concat(['-Dbuild.dir=', config.get('TEMP')]),
											concat(['-lib ', config.get('DEST_OCCPLUG_JARS')])  ]))	

	# Insert the ubuntu.props file into the .jar
	with pushd():
		cd(config.get('OCCPLUG_DISTRIBUTION'))

		cmd(build_command([
				'jar -uf', 
				concat([config.get('DEST_OCCPLUG_JARS'), '/', 'OccPlug.jar']),
				'ubuntu.props'
				]))
	
	config.set('BUILD_ARCHITECTURE', 'all')

# Builds multiple packages for all architectures

def all_arch(url):
	config.refresh()
	
	if url == 'trunk':
		url = config.get('SVN_TRUNK')

	TOOLCHAINS    = ['kroc', 'tvm', 'avr']
	arch = config.get('BUILD_ARCHITECTURE')

	for tool in TOOLCHAINS:
		# I think we need 386 and 686 packages to make
		# life easier for end-users.
		#with_temp_dir("%s/%s" % (arch, config.get('TEMP_ROOT') ))

		# Set the config for the platform.
		if tool == 'avr':
			build_avr()
		elif tool == 'tvm':
			build_native_tvm()
		elif tool == 'kroc':
			build_native_kroc()
		
		all(url)

	build_occplug()

def getUTC():
	dto = datetime.datetime.utcnow()
	return dto.strftime('%Y%m%d.%H%M')

def ubuntu_version(name):
	header("UPLOADING UBUNTU VERSION: %s" % name)
	config.rebase('UVN', name)

def build_arch(arch):
	header("SETTING BUILD ARCH TO %s" % arch)
	config.rebase('BUILD_ARCHITECTURE', arch)

def dependencies():
	config.rebase('DEPENDS',
									config.get(concat([config.get('TOOLCHAIN'), 
															'-',
															config.get('TARGET'),
															'-',
															config.get('WRAPPER'),
															'-deps'])))
	header("DEPS NOW: %s" % config.get('DEPENDS'))

def upload():
	header("UPLOADING FILES")
	print("VERSION: %s" % config.get('UVN'))

	if config.get('UVN') == 'NO_NAME':
		header("SET UBUNTU VERSION WITH --ubuntu-version")
		sys.exit()

	with pushd():
		root = config.get('TEMP_ROOT')

		# Remove old packages
		with pushd():
			cd(root)
			remove_dir('PACKAGES')

		cd(root)
		mkdir('PACKAGES/binary')

		config.rebase('VERSION', getUTC())
		config.refresh()
		header("SHIPPING VERSION: %s" % config.get('VERSION'))
		
		header("PACKAGING AVR")
		build_avr()
		dependencies()
		deb()			
		header("PACKAGING NATIVE KROC")
		build_native_kroc()
		dependencies()
		deb()
		header("PACKAGING NATIVE TVM")
		build_native_tvm()
		dependencies()
		deb()
		header("PACKAGING META")
		meta_deb()
	
		arch = config.get('BUILD_ARCHITECTURE')

		header("PACKAGING OCCPLUG")
		build_occplug()
		occplug_deb()
		
		config.rebase('BUILD_ARCHITECTURE', arch)
		META = concat(['occam-pi', '_', 
											 config.get('VERSION'), '_', 
											 config.get('BUILD_ARCHITECTURE'), '.deb'])
		
		KROC = concat([config.get('STEM'), '-kroc-posix-posix', '_', 
											 config.get('VERSION'), '_', 
											 config.get('BUILD_ARCHITECTURE'), '.deb'])

		TVM = concat([config.get('STEM'), '-tvm-posix-posix', '_', 
											 config.get('VERSION'), '_', 
											 config.get('BUILD_ARCHITECTURE'), '.deb'])

		AVR = concat([config.get('STEM'), '-tvm-avr-arduino', '_', 
											 config.get('VERSION'), '_', 
											 config.get('BUILD_ARCHITECTURE'), '.deb'])
		
		PLUG = concat([config.get('STEM'), '-occplug', '_', 
											 config.get('VERSION'), '_', 
											 'all', '.deb'])
		
	
		with pushd():
			cd(root)
			copy_files(META, '.', 'PACKAGES/binary')

		with pushd():
			cd(root + '/kroc-posix-posix')
			copy_files(KROC, '.', '../PACKAGES/binary')
	
		with pushd():
			cd(root + '/tvm-posix-posix')
			copy_files(TVM, '.', '../PACKAGES/binary')

		with pushd():
			cd(root + '/tvm-avr-arduino')
			copy_files(AVR, '.', '../PACKAGES/binary')

		with pushd():
			cd(root + '/occPlug')
			copy_files(PLUG, '.', '../PACKAGES/binary')

		with pushd():
			cd(root + '/PACKAGES')
			cmd(build_command(['dpkg-scanpackages', 'binary', '/dev/null', '|', 'gzip', '-9c', '>', 'binary/Packages.gz']))
			
		with pushd():
			cd(root + '/PACKAGES/binary')
			cmd(config.get('SCP_CMD'))



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
	["copy-arduino-config", "store_true", "Copy configuration files to destination.",
		copy_arduino_config ],
	["deployment-version", "store_true", "Write deployment version to config directory.",
		deployment_version],
	["copy-arduino-build", "store_true", "Copy libraries and compiled files into place.",
		copy_arduino_build],
	["copy-native-build", "store_true", "Copy libraries and compiled files into place.",
		copy_native_build],
	["deb", "store_true", "Build the Debian package (.deb).", deb],
	["meta-deb", "store_true", "Build the meta package (.deb).", meta_deb],
	["rpm", "store_true", "Convert the Debian package to a Fedora package (.rpm).", rpm],
	["all", "store", "ALL_SVN_URL", "Do everything up to this point.", all],
	["local-all", "store_true", "Do everything in a currently checked-out tree.", 
		(lambda : all('LOCAL'))],
	["refresh-avr-libs", "store_true", "Re-run copy-config, copy, deb, and rpm.", refresh_libs],
	["with-avr-svn-path", "store", "LIB_PATH", "Set path for where we will refresh from.", with_lib_path],
	["with-temp-dir", "store", "TEMP_DIR", "Set the temp build directory.",
		with_temp_dir],
	["build-avr", "store_true", "Set build vars for AVR.", build_avr],
	["build-kroc", "store_true","Set build vars for a native KRoC build.", build_native_kroc],
	["build-tvm", "store_true", "Set build vars for a native TVM build.", build_native_tvm],
	["build-occplug", "store_true", "Build the occPlug.", build_occplug],
	["uber", "store", "UBER_URL", "Build all toolchains and architectures.", all_arch],
	["ubuntu-version", "store", 'UBUNTU_VERSION', "Set the Ubuntu version name.", ubuntu_version],
	["build_arch", "store", 'ARCH', "Set the build architecture.", build_arch],
	["upload", "store_true", "Upload everything to the server.", upload]
	] 
	
parser = OptionParser()

for OPT in OPTIONS:
	if len(OPT) == 4:    #OPT[1] == "store_true":
		parser.add_option("--%s" % OPT[0], action=OPT[1], dest=OPT[0].upper(), help=OPT[2])
	elif len(OPT) == 5:   # OPT[1] == "store":
		parser.add_option("--%s" % OPT[0], action=OPT[1], dest=OPT[2], help=OPT[3])

# DO THE PARSE
(options, args) = parser.parse_args()

# HANDLE THE RESULTS
def ignored(str):
	return re.match("CHECKOUT", str) 

def call_handler(str, arg):
	#print "GOT %s %s" % (str, arg)
	tag = re.sub("-", "_", str.lower()) 
	#print "TAG %s" % tag
	for OPT in OPTIONS:
		if str == OPT[2]:
			#print "CALLING [ %s ] " % OPT[2]
			if arg == None:
				OPT[len(OPT) - 1]()
			else:
				OPT[len(OPT) - 1](arg)
		elif (tag == re.sub("-", "_", OPT[0].lower())):
			OPT[len(OPT) - 1]()
		elif (tag == re.sub("_", "+", OPT[0].lower())):
			OPT[len(OPT) - 1]()

PLAT       = ["BUILD-TVM", "BUILD-AVR", "BUILD-KROC"]
DIR_PARAMS = ["TEMP_DIR", "LIB_PATH"]
NAMING     = ["UBUNTU_VERSION", "ARCH"]
FIRST      = NAMING + DIR_PARAMS

def check_for_build_target():
	found = False
	for key, val in props(options).iteritems():
		if (key in PLAT) and (val != None) or (key == 'UBER_URL'):
			found = True
	if not found:
		print "You need to include a build target (eg. --build-tvm)."
		sys.exit()

def driver():
	check_for_build_target()
	# Need to do a better job with architecture... perhaps a flag.
	# config.rebase('BUILD_ARCHITECTURE', 'i686')

	for key, val in props(options).iteritems():
		if (key in PLAT) and (val != None):
			# print "PLAT: %s, %s" % (key, val)
			call_handler(key, val)
		if (key in FIRST) and (val != None):
			# print "FIRST: %s, %s" % (key, val)
			# print "HANDLING DIR PARAMS"
			call_handler(re.sub("-", "_", key), val)

	#for key, val in props(options).iteritems():
	for key, val in props(options).iteritems():
		if (key not in FIRST):
			if (val != None):
				call_handler(re.sub("-", "_", key), val)

	
driver()
