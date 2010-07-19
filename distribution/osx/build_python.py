#!/usr/bin/env python
import sys
import os

# Pick the remaining packages from the common directory
sys.path.append('../common/python-lib')
from distribution_tools.copy import mkdirs, copy_file, copy_files, copy_tree
from distribution_tools.download import download
from distribution_tools.archive import extract
from distribution_tools import command

BUILD               = 'build'
PYTHON_BUILD        = os.path.abspath(os.path.join(BUILD, 'python'))

PYSERIAL            = 'pyserial-2.5-rc2'
PYSERIAL_TAR_GZ     = PYSERIAL + '.tar.gz'

# Download and extract pyserial
download(os.path.join(BUILD, PYSERIAL_TAR_GZ),
        'http://prdownloads.sourceforge.net/pyserial/%s' % PYSERIAL_TAR_GZ)
extract(os.path.join(BUILD, PYSERIAL_TAR_GZ), BUILD)

command.execute(['python', 'setup.py', 'install', '--prefix=%s' % (PYTHON_BUILD,)],
        cwd=os.path.join(BUILD, PYSERIAL))
