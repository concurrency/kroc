#!/bin/env python
from distutils.core import setup
import py2exe
from glob import glob
import sys

class Target:
    def __init__(self, **kw):
        self.__dict__.update(kw)
        # for the versioninfo resources
        self.version = "0.0.0.0"
        self.company_name = ""
        self.copyright = "Copyright 2010 The Transterpreter"
        self.name = ""

msvc90crt_manifest = '''
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<!-- Copyright (c) Microsoft Corporation.  All rights reserved. -->
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
    <noInheritable/>
    <assemblyIdentity
        type="win32"
        name="Microsoft.VC90.CRT"
        version="9.0.21022.8"
        processorArchitecture="x86"
        publicKeyToken="1fc8b3b9a1e18e3b"
    />
    <file name="msvcr90.dll" /> <file name="msvcp90.dll" /> <file name="msvcm90.dll" />
</assembly>
'''
f = open('build/Microsoft.VC90.CRT.manifest', 'w')
f.write(msvc90crt_manifest)
f.close()

data_files = [
    ("Microsoft.VC90.CRT", 
     ['build/Microsoft.VC90.CRT.manifest'] + 
     glob(r'C:\WINDOWS\WinSxS\x86_Microsoft.VC90.CRT*9.0.21022.8*\msvcr90.dll'))
]

manifest_template = '''
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
<assemblyIdentity
    version="0.0.0.0"
    processorArchitecture="x86"
    name="%(prog)s"
    type="win32"
/>
<description>%(prog)s Program</description>
<dependency>
    <dependentAssembly>
        <assemblyIdentity
            type="win32"
            name="Microsoft.Windows.Common-Controls"
            version="6.0.0.0"
            processorArchitecture="X86"
            publicKeyToken="6595b64144ccf1df"
            language="*"
        />
    </dependentAssembly>
</dependency>
<dependency> 
    <dependentAssembly> 
        <assemblyIdentity 
            type="win32" 
            name="Microsoft.VC90.CRT" 
            version="9.0.21022.8" 
            processorArchitecture="x86" 
            publicKeyToken="1fc8b3b9a1e18e3b" 
        /> 
    </dependentAssembly> 
</dependency> 
</assembly>
'''

RT_MANIFEST = 24

occbuild = Target(
  description='occbuild',
  script='install/bin/occbuild',
  other_resources=[(RT_MANIFEST, 1, manifest_template % dict(prog='occplug'))],
)

# Make it appear like we wrote py2exe on the commandline
sys.argv.append('py2exe')

opts = {
    "py2exe": {
        "dist_dir": "py2exe-dist",
    }
}

setup(
	options=opts,
	console=[
		occbuild
],
data_files=data_files
)
