#!/bin/env python
from distutils.core import setup
import py2exe
from glob import glob
import sys

class Target:
    def __init__(self, **kw):
        self.__dict__.update(kw)
        # for the versioninfo resources
        self.version = "0.6.1"
        self.company_name = "No Company"
        self.copyright = "no copyright"
        self.name = "py2exe sample files"

#data_files = [("Microsoft.VC90.CRT", glob(r'c:\dev\ms-vc-runtime\*.*'))]
# C:\Program Files\Microsoft Visual Studio 9.0\VC\redist\x86\Microsoft.VC90.CRT
data_files = [("Microsoft.VC90.CRT", glob(r'C:\Program Files\Microsoft Visual Studio 9.0\VC\redist\x86\Microsoft.VC90.CRT\*.*'))]

manifest_template = '''
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
<assemblyIdentity
    version="5.0.0.0"
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
version="9.0.30729.4148" 
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
#data_files=data_files
)
