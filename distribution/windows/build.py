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

libxslt = Package('libxslt',
        url='ftp://xmlsoft.org/libxslt/libxslt-1.1.26.tar.gz',
	srcPath=os.path.join(ROOT, 'libxslt-1.1.26'),
		buildPath=os.path.join(ROOT, 'libxslt-1.1.26'),
	buildCfg=Autotools,
        configArgs=['--with-libxml-src=' + os.path.join(ROOT, 'libxml2-2.7.6'),
                    '--prefix=' + os.path.join(ROOT, 'install')],
        deps = [libxml2]
)

b = Builder(libxslt)
b.download()
b.unarchive()
b.build()
#b.install()
