#!/bin/python

import sys
import os
import os.path
import subprocess
import tarfile
import urllib
import urllib2
import fnmatch
import shutil
import re

paths = dict(
  downloads = 'downloads',
  mingw     = 'mingw',
  msys      = 'msys'
)
  
# Check that there are no spaces in the name of the current path, that would
# be bad...
if os.getcwd().count(' ') > 0:
    name = os.path.basename(__file__)
    sys.stdout.write('Please run %s from a directory without spaces\n' % (name,))
    sys.stdout.write('You ran %s from "%s"\n' % (name, os.getcwd()))
    sys.exit(1)
 
def download(url, dest = None):
    sys.stdout.write('%s\n' % (url, ))
    if dest != None:
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

def extract(files, dest):
    for f in files:
        sys.stdout.write('extracting: %s\n' %(f, ))
        if f.endswith('.lzma'):
            lzma = subprocess.Popen([os.path.join(paths['msys'], 'bin', 'lzma'), '-d', '-c', '-q', f], stdout=subprocess.PIPE)
            a = tarfile.open(fileobj=lzma.stdout, mode='r|')
            # FIXME: check that no files will be written outside dest
            a.extractall(dest)
            a.close()
            r = lzma.wait()
            if r != 0:
                sys.stdout.write('lzma failed with error code: %d %s\n' %(r, f))

        else:
            a = tarfile.open(f, 'r')
            # FIXME: check that no files will be written outside dest
            a.extractall(dest)
            a.close()
        
def process_lines(lines):
    return [x for x in [x.strip() for x in lines] if not (x.startswith('#') or len(x) == 0)]

fp = open('mingw.urls')
mingw_urls = fp.readlines()
fp.close()
urls = mingw_urls
files = []
fp = open('msys.urls')
msys_urls = fp.readlines()
fp.close()

mingw_urls = process_lines(mingw_urls)
msys_urls  = process_lines(msys_urls)

mingw_files = []
msys_files = []
for url in mingw_urls:
    mingw_files.append(os.path.join(paths['downloads'], url.split('/')[-1]))
for url in msys_urls:
    msys_files.append(os.path.join(paths['downloads'], url.split('/')[-1]))

urls  = mingw_urls + msys_urls
files = mingw_files + msys_files

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

extract(msys_files, paths['msys'])
extract(mingw_files, paths['mingw'])

print 'creating fstab'
tvmdir = os.path.abspath(os.path.join(os.getcwd(), '../../')).replace('\\', '/')
msysdir = os.path.abspath(paths['mingw']).replace('\\', '/')
fp = open('msys/etc/fstab', 'w')
fp.write('%s\t%s\n' % (tvmdir, '/tvm'))
fp.write('%s\t%s\n' % (msysdir, '/mingw'))
fp.close()

print 'renaming autotools'
r = re.compile('(aclocal|auto.*?)-.*')
bindir = os.path.join(paths['mingw'], 'bin')
autotools = os.listdir(bindir)
autotools = [at for at in autotools if r.match(at)]
tools = [t[:t.index('-')] for t in autotools]
print autotools
print tools
for tool in tools:
    versions = fnmatch.filter(autotools, tool + '-*')
    versions.sort()
    shutil.copy(os.path.join(bindir, versions[0]),
                os.path.join(bindir, tool))