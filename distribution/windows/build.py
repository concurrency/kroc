from package import *
import package

package.verbose = True

libxml2 = Package('libxml2',
	url='ftp://xmlsoft.org/libxml2/libxml2-2.7.6.tar.gz',
	#srcPath=os.path.join(ROOT, 'src'),
	srcPath=os.path.join(ROOT, 'libxml2-2.7.6'),
	#buildPath=os.path.join(ROOT, 'build', 'libxml2'),
	buildPath=os.path.join(ROOT, 'libxml2-2.7.6'),
	buildCfg=Autotools,
        configArgs=['--without-threads', 
                    '--prefix=' + os.path.join(ROOT, 'install')])

b = Builder(libxml2)
b.download()
b.unarchive()
b.build()
