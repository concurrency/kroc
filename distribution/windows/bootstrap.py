#!/bin/python

import sys
import os
import os.path
import subprocess
import tarfile
import zipfile
import urllib
import urllib2
import fnmatch
import shutil
import re

VERIFY = False

paths = dict(
  downloads = 'downloads',
  mingw     = 'mingw',
  msys      = 'msys',
  tmp       = 'tmp',
)

class UrlMapping(dict):
    def url_list(self):
        return self.values()
    def file_list(self):
        values = self.values()
        return [os.path.join(paths['downloads'], v.split('/')[-1]) for v in values]
    def url_dict(self):
        return dict(self.items())
    def file_dict(self):
        items = self.items()
        d = dict()
        for k, v in items:
            d[k] = os.path.join(paths['downloads'], v.split('/')[-1])
        return d
   
# Check that there are no spaces in the name of the current path, that would
# be bad...
if os.getcwd().count(' ') > 0:
    name = os.path.basename(__file__)
    sys.stdout.write('Please run %s from a directory without spaces\n' % (name,))
    sys.stdout.write('You ran %s from "%s"\n' % (name, os.getcwd()))
    sys.exit(1)

def find_file(name, path, ext=''):
    """Given a pathsep-delimited path string, find name,.
    Returns path to name, if found, otherwise None.
    Also allows for files with implicit extensions (eg, .exe), but
    always returning name, as was provided.
    >>> find_file('ls', '/usr/bin:/bin', ext='.exe')
    '/bin/ls'
    """
    if os.path.isfile(name):
        return name
    if ext and os.path.isfile(name + ext):
        # Already absolute path.
        return name + ext
    for p in path.split(os.pathsep):
        candidate = os.path.join(p, name)
        if (os.path.isfile(candidate)):
            return candidate
        if ext and os.path.isfile(candidate + ext):
            return candidate + ext
    return None


def download(url, dest = None):
    sys.stdout.write('%s\n' % (url, ))
    if dest != None:
        if not VERIFY and os.path.exists(dest):
            sys.stdout.write('  already downloaded, skipping...\n')
            return None
        f = urllib2.urlopen(url)
        expected_size = f.info().getheader('Content-Length')
        try:
            file_size    = os.path.getsize(dest)
            if int(expected_size) == int(file_size):
                sys.stdout.write('  already downloaded, skipping...\n')   
                return None
        except WindowsError:
            pass
    progress_printed = [0]
    def progress(block, block_size, total_size):
        progress = min(((block * block_size * 100)/total_size), 100)
        progress = int(progress * 0.70)
        if progress > progress_printed[0]:
            sys.stdout.write('#' * (progress - progress_printed[0]))
            progress_printed[0] += progress - progress_printed[0]
    if sys.stdout.isatty():
        sys.stdout.write('  !' + (' ' * 33) + '50%' + (' ' * 32) + '! 100%\n')
        sys.stdout.write('  ')
        u = urllib.urlretrieve(url, dest, progress)
        sys.stdout.write('\n')
        return u[0]
    else:
        u = urllib.urlretrieve(url, dest)
        return u[0]

def makedir(dir):
    if os.path.exists(dir):
        if not os.path.isdir(dir):
            sys.stdout.write('Existing file "%s" is not a directory, plase delete\n' % dir)
            sys.exit(1)
    else:
        os.makedirs(dir)

def is_naughty_archive(names):
    """Check if there is more than one file/dir in the root of the archive"""
    name = names[0].split('/', 1)[0]
    for n in names:
        n = n.split('/', 1)[0]
        if n != name:
            return True
    return False

def extract(files, dest):
    for f in files:
        sys.stdout.write('extracting: %s\n' %(f, ))
        if f.endswith('.lzma'):
            lzma = subprocess.Popen([os.path.join(paths['tmp'], 'xz-5.0.0-windows', 'bin_i486', 'xz'), '-d', '-c', '-q', f], stdout=subprocess.PIPE)
            a = tarfile.open(fileobj=lzma.stdout, mode='r|', errorlevel=0)
            # FIXME: check that no files will be written outside dest
            a.extractall(dest)
            a.close()
            # FIXME: both xz (5.0.0) and 7zip cmd line (960) fail (hang) if we wait on lzma.
            #        not sure who is at fault here, but let us not wait
            #r = lzma.wait()
            r=0
            if r != 0:
                sys.stdout.write('lzma failed with error code: %d %s\n' %(r, f))
            lzma.stdout.close()
        elif f.endswith('.zip'):
            a = zipfile.ZipFile(f, 'r')
            if is_naughty_archive(a.namelist()):
                d = os.path.join(dest, filename2dirname(f))
            else:
                d = dest
            # extractall is broken for zipfile...
            names = a.namelist()
            for name in names:
                if name[-1] == '/':
                    pass
                else:
                    # FIXME: check that no files will be written outside dest
                    a.extract(name, d)
            a.close()
        else:
            a = tarfile.open(f, 'r', errorlevel=0)
            # FIXME: check that no files will be written outside dest
            a.extractall(dest)
            a.close()
        
def read_urls(file_name, mapping=False):
    fp = open(file_name, 'r')
    lines = process_lines(fp.readlines())
    fp.close()
    if mapping:
        mapping = UrlMapping()
        for line in lines:
            k, v = line.split(':', 1)
            mapping[k.strip()] = v.strip()
        return mapping
    return lines

def process_lines(lines):
    return [x for x in [x.strip() for x in lines] if not (x.startswith('#') or len(x) == 0)]

def filename2dirname(path, add_ext=''):
    ext = (add_ext + ' .tar.bz2 .tar.lzma .tar.gz .zip').split()
    if '\\' in path or '/' in path:
        path = path.replace('\\', '/').split('/')[-1]
    for e in ext:
        if e in path:
            path = path[:-len(e)]
    return path
    
mingw_urls = read_urls('mingw.urls')
msys_urls = read_urls('msys.urls')
other_urls = read_urls('other.urls', mapping=True)
python_urls = read_urls('python.urls')

mingw_files = []
msys_files = []
python_files = []
for url in mingw_urls:
    mingw_files.append(os.path.join(paths['downloads'], url.split('/')[-1]))
for url in msys_urls:
    msys_files.append(os.path.join(paths['downloads'], url.split('/')[-1]))
for url in python_urls:
    python_files.append(os.path.join(paths['downloads'], url.split('/')[-1]))

urls  = mingw_urls + msys_urls + other_urls.url_list() + python_urls
files = mingw_files + msys_files + other_urls.file_list() + python_files

#urls  = msys_urls; files = msys_files
#urls = []

makedir(paths['downloads'])
for url, dest in zip(urls, files):
    url = url.strip()
    if url.startswith('#'): continue
    if len(url) == 0: continue
    download(url, dest)
 
makedir(paths['mingw'])
makedir(paths['msys'])
makedir(paths['tmp'])
makedir(os.path.join(paths['tmp'], 'python'))

extract(other_urls.file_list(), paths['tmp'])
extract(msys_files, paths['msys'])
extract(mingw_files, paths['mingw'])
extract(python_files, os.path.join(paths['tmp'], 'python'))

print 'moving tools'
tools = [(other_urls['curl'], 'curl'), 
         (other_urls['ant'], 'ant'),
         (other_urls['unzip'], 'unzip')]
for tool_src, tool_dir in tools:
    tool_path = os.path.abspath(os.path.join(paths['msys'], tool_dir))
    if os.path.exists(tool_path):
        shutil.rmtree(tool_path)
    if tool_dir == 'ant':
        # morons:
        dirname = filename2dirname(tool_src, add_ext='-bin.tar.bz2')
    else:
        dirname = filename2dirname(tool_src)
    shutil.move(os.path.join(paths['tmp'], dirname), tool_path)


print 'creating fstab'
tvmdir = os.path.abspath(os.path.join(os.getcwd(), '../../')).replace('\\', '/')
msysdir = os.path.abspath(paths['mingw']).replace('\\', '/')
fp = open('msys/etc/fstab', 'w')
fp.write('%s\t%s\n' % (tvmdir, '/tvm'))
fp.write('%s\t%s\n' % (msysdir, '/mingw'))
fp.close()


print 'setting extra msys config'
cfg_dir     = os.path.join(paths['msys'], 'etc', 'profile.d')
cfg_file    = os.path.join(cfg_dir, 'tvm_config.sh')
perl_bin    = '/perl/bin'
install_bin = '/tvm/distribution/windows/install/bin'
curl_bin    = '/curl'
ant_bin     = '/ant/bin'
ant_home    = '/ant'
unzip_bin   = '/unzip/bin'
path_var    = [perl_bin, install_bin, curl_bin, ant_bin, unzip_bin]
if not os.path.exists(cfg_dir):
    os.makedirs(cfg_dir)
fp = open(cfg_file, 'w')
fp.write(
"""
# tvm related config options for the msys shell
# created automagically by bootstrap.py

export PATH=.:%(path)s:$PATH
export EDITOR=vim
export ANT_HOME=%(ant_home)s
""" % dict(path=':'.join(path_var), ant_home=ant_home))
fp.close()


print 'setting up java path script'
cfg_file    = os.path.join(cfg_dir, 'java_path.sh')
fp = open(cfg_file, 'w')
fp.write(r"""
JDK_REG_ROOT="HKLM\\SOFTWARE\\JavaSoft\\Java Development Kit"

javac=`which javac 2>/dev/null`
reg64=
is64bit=

if test "x$javac" != x ; then
    echo "Javac found in $javac, not altering path"
    return
fi

version=`reg query "$JDK_REG_ROOT" //v CurrentVersion 2>/dev/null`
if [ "x$version" == "x" ] ; then

  # If a 64 bit java is installed on a 64 bit OS, we won't find it as 
  # MinGW is a 32 bit app and Windows will do a registry redirect so we 
  # look at the 32 bit reg which does not contain a key for java. Force 
  # reg.exe to look in the 64 bit registry:
  version=`reg query "$JDK_REG_ROOT" /reg:64 //v CurrentVersion 2>/dev/null`
  reg64="/reg:64"
  is64bit=" (64 bit)"
fi
version=`echo "$version" | grep CurrentVersion | gawk "{print \\$3}"`

home=`reg query "$JDK_REG_ROOT\\\\$version" //v JavaHome $reg64 2>/dev/null | grep JavaHome | gawk "{ print substr(\\$0, index(\\$0, \\$3)) }"`

if test "x$home" != x ; then
    echo "JDK version $version$is64bit found, in $home"
    path=`echo $home | sed -e "s/:// ; s|\\\\\|/|g"`
    path="/$path/bin"
    echo "Setting path to include $path"
    export PATH="$PATH":"$path"
    echo "Setting JAVA_HOME to include $home"
    export JAVA_HOME="$home"
    return
fi

echo "Javac not found and could not find a JDK through the registry"
echo "Please make sure you have an appropritate java version installed"
""")
fp.close()


print 'setting up python settings script'
msys_dir    = os.path.join(os.getcwd(), paths['msys'])
cfg_file    = os.path.join(cfg_dir, 'python.sh')
python_inst = os.path.abspath(os.path.join(msys_dir, 'python'))
python_path = os.path.abspath(os.path.join(msys_dir, 'python', 'Lib', 'site-packages'))
python_bin  = '/' + os.path.dirname(sys.executable).replace('\\', '/').replace(':', '/')
fp = open(cfg_file, 'w')
fp.write(r"""
export PYTHONPATH=%s
export PATH=$PATH:/python/Scripts

python --version > /dev/null 2>&1
if [ "$?" == "127" ]; then
  echo "Did not find a python executable in the PATH"
  echo "Setting path to include the python used for bootstrapping"
  export PATH=$PATH:%s
fi
""" % (python_path.replace('\\', '\\\\'), python_bin))
fp.close()


print 'adding pkg-config proxy script'
pkg_config_file = os.path.abspath(os.path.join(
    msys_dir, 'python', 'Scripts', 'pkg-config'))
if not os.path.exists(os.path.dirname(pkg_config_file)):
    os.makedirs(os.path.dirname(pkg_config_file))
fp = open(pkg_config_file, 'w')
fp.write(
r"""#!/bin/bash
pykg-config.py "$@"
""")
fp.close()



print 'renaming autotools'
r = re.compile('(aclocal|auto.*?)-.*')
bindir = os.path.join(paths['mingw'], 'bin')
autotools = os.listdir(bindir)
autotools = [at for at in autotools if r.match(at)]
tools = [t[:t.index('-')] for t in autotools]
for tool in tools:
    versions = fnmatch.filter(autotools, tool + '-*')
    versions.sort()
    shutil.copy(os.path.join(bindir, versions[0]),
                os.path.join(bindir, tool))

build_dir   = os.path.join(
                  paths['tmp'], 
                  other_urls.file_dict()['perl'][len('downloads/'):-len('.tar.bz2')],
                  'win32')
dmake_dir   = os.path.join(os.getcwd(), paths['tmp'], 'dmake')
mingw_dir   = os.path.join(os.getcwd(), paths['mingw'])
msys_dir    = os.path.join(os.getcwd(), paths['msys'])
mingw_bin   = os.path.join(mingw_dir, 'bin')
env         = dict(os.environ)
env['PATH'] = ';'.join([env['PATH'], dmake_dir, mingw_dir, mingw_bin])
dmake       = find_file('dmake', env['PATH'], '.exe')
inst_dir    = os.path.join(msys_dir, 'perl')


print 'patching perl makefile'
re1 = re.compile(r'^\s*INST_DRV\s+\*=.*', re.MULTILINE)
re2 = re.compile(r'^\s*INST_TOP\s+\*=.*', re.MULTILINE)
re3 = re.compile(r'^\s*CCHOME\s+\*=.*', re.MULTILINE)
drive, path = os.path.splitdrive(inst_dir)
fp = open(os.path.join(build_dir, 'makefile.mk'), 'r+')
text = fp.read()
text = re1.sub('INST_DRV *= ' + drive, text)
text = re2.sub('INST_TOP *= $(INST_DRV)' + path.replace('\\', '\\\\'), text)
text = re3.sub('CCHOME *= ' + mingw_dir.replace('\\', '\\\\'), text)
fp.seek(0)
fp.truncate()
fp.write(text)
fp.close()


print 'installing python packages'
for p in python_files:
    setup_path = os.path.join(paths['tmp'], 'python', filename2dirname(p))
    try:
        retcode = subprocess.call(['python', 'setup.py', 'install', '--prefix=' + python_inst], cwd=setup_path, env=env)
        if retcode < 0:
            print >>sys.stderr, "Child was terminated by signal", -retcode
        elif retcode != 0:
            print >>sys.stderr, "Child returned", retcode
    except OSError, e:
        print >>sys.stderr, "Execution failed:", e
    

print 'compiling perl'
try:
    retcode = subprocess.call(dmake, cwd=build_dir, env=env)
    if retcode < 0:
        print >>sys.stderr, "Child was terminated by signal", -retcode
    elif retcode != 0:
        print >>sys.stderr, "Child returned", retcode
except OSError, e:
    print >>sys.stderr, "Execution failed:", e

print 'installing perl'
try:
    retcode = subprocess.call([dmake, 'install'], cwd=build_dir, env=env)
    if retcode < 0:
        print >>sys.stderr, "Child was terminated by signal", -retcode
    elif retcode != 0:
        print >>sys.stderr, "Child returned", retcode
except OSError, e:
    print >>sys.stderr, "Execution failed:", e


