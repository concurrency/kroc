import re, sys, subprocess, os, glob

import re, sys, subprocess, os, glob
svn_url   = 'http://projects.cs.kent.ac.uk/projects/kroc/svn/kroc/branches/distribution'
release   = '1.5.0'
HOME      = '/home/reynoldsm'

# Allow building of multiple configurations in one go.
# SPECFILES = ['tvm.spec', 'avr.spec']
SPECFILES = ['tvm.spec']
# TAGS      = ['kroc-%s' % release, 'kroc-avr-%s' % release]
TAGS      = ['kroc-%s' % release]

RPMBUILD  = os.path.join(HOME, 'rpmbuild')
SOURCES   = os.path.join(RPMBUILD, 'SOURCES')
SPECS     = os.path.join(RPMBUILD, 'SPECS')
RPMS      = os.path.join(RPMBUILD, 'RPMS')
I586      = os.path.join(RPMS, 'i586')

def remove_build_dir ():
	p = subprocess.Popen(['rm', '-rf', RPMBUILD])
	sts = os.waitpid(p.pid, 0)
	return sts[1]

def do_checkout (TAG):
	print "Checking out the source tree."
	cmd = ['svn', 'co', svn_url, TAG]
	p = subprocess.Popen(cmd, cwd = SOURCES)
	sts = os.waitpid(p.pid, 0)
	# Tar it up for the RPM builder
	p = subprocess.Popen(['tar', 'cvzf', '%s.tar.gz' % TAG, TAG], cwd = SOURCES)
	sts = os.waitpid(p.pid, 0)
	return sts[1]
	
def setup_build_dir ():
	print "Setting up build tree."
	p = subprocess.Popen (['rpmdev-setuptree'])
	sts = os.waitpid(p.pid, 0)
	return sts[1]

def fetch_specfile (THE_SPEC):
	print "Fetching the SPEC file."
	p = subprocess.Popen(['cp', os.path.join(HOME, THE_SPEC), SPECS])
	sts = os.waitpid(p.pid, 0)
	return sts[1]

def build_package (THE_SPEC):
	print "Building the package with rpmbuild."
	p = subprocess.Popen(['rpmbuild', '-ba', THE_SPEC], cwd=SPECS)
	sts = os.waitpid(p.pid, 0)
	return sts[1]

def test_package ():
	print "Testing the package with rpmlint."
	p = subprocess.Popen(['rpmlint', '*.rpm'], cwd=I586)
	sts = os.waitpid(p.pid, 0)
	return sts[1]

def run ():
	# Now, doing the POSIX and AVR builds.
	for spec,tag in zip(SPECFILES, TAGS):
		print "Building %s with %s." % (tag, spec)
		if remove_build_dir() != 0:
			sys.exit("Error removing build dir.")
		if setup_build_dir() != 0:
			sys.exit("Error setting up build dir.")
		if do_checkout(tag) != 0:
			sys.exit("Error doing checkout.")
		if fetch_specfile(spec) != 0:
			sys.exit("Error fetching specfile.")
		if build_package(spec) != 0:
			sys.exit("Error building package.")
		if test_package() != 0:
			sys.exit("Error testing package.")

run()

