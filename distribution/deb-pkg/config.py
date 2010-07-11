import datetime
import re

# API
# get(key) -- returns the value for the given key
# set(key, value) - sets a value in CFG
# rebase(key, value) - sets a variable and regenerates CFG
#   (use for changing things like TEMP)

now    = datetime.datetime.now()
BASE = {'YMD'            : now.strftime("%Y%m%d"),
				'YMDHMS'         : now.strftime("%Y%m%d%H%M%S"),
				'TEMP'           : '/tmp',
				'PACKAGE_NAME'   : 'concurrency',
				'PACKAGE_BUILD'  : '@TEMP@/@PACKAGE_NAME@',
				'SVN'            : '@TEMP@/deb-pkg-src',
				'OBJ'            : '@SVN@/obj',
				# Where we pull things from
				'SOURCE_ARDUINO' : '@SVN@/tvm/arduino',
				'SOURCE_CONF'    : '@SVN@/tvm/arduino/occam/share/conf',
				'SOURCE_SCRIPTS' : '@SVN@/tvm/arduino/scripts',
				'SOURCE_FIRMWARE': '@SVN@/tvm/arduino',
				'LIB_PATH'       : '@SVN@',
				'SOURCE_LIB'     : '@LIB_PATH@/tvm/arduino/occam/include',
				'SOURCE_DEBIAN'  : '@SVN@/distribution/deb-pkg/DEBIAN.in',
				# Desired filesystem path for installation
				'INSTPATH'       : 'opt/occam/arduino',
				'FINAL'          : '/@INSTPATH@',
				# Dstdir installation paths
				'DEST'           : '@TEMP@/@PACKAGE_NAME@',
				'DEST_ROOT'      : '@DEST@/@INSTPATH@',
				'DEST_BIN'       : '@DEST_ROOT@/bin',
				'DEST_SHARE'     : '@DEST_ROOT@/share',
				'DEST_FIRMWARE'  : '@DEST_SHARE@/firmwares',
				'DEST_CONF'      : '@DEST_SHARE@/conf',
				'DEST_LIB'       : '@DEST_ROOT@/lib',
				'DEST_DEBIAN'    : '@PACKAGE_BUILD@/DEBIAN'
			}

CFG = { }

def get (key):
	return CFG[key]

def copy (src):
	tmp = { }
	for key, value in src.iteritems():
		tmp[key] = value
	return tmp

def no_subst_left(hash):
	result = True

	for key, value in hash.iteritems():
		print "CHECKING %s, %s" % (key, value)

		if re.search('@.*?@', value):
			result = False

	return result

def hassubst(str):
	if re.search('@.*?@', str):
		return True
	else:
		return False

def dosubst(str):
	if hassubst(str):
		repls = re.findall('@(.*?)@', str)
		for pat in repls:
			str = re.sub(re.compile('@%s@' % pat), BASE[pat], str)
		return dosubst(str)
	else:
		return str

def refresh ():
	tmp = copy(BASE)
	# Go through each key
	for key, value in BASE.iteritems():
		value = dosubst(value)
		tmp[key] = value
	for k, v in tmp.iteritems():
		CFG[k] = v

def rebase (key, value):
	BASE[key] = value
	refresh ()

def set (key, value):
	CFG[key] = value
