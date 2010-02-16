from __future__ import generators
import copy
import os
import os.path
import sys
import shutil
import urllib
import urlparse
import subprocess
ROOT = os.getcwd()

verbose = False
dry_run = False

def indentStr(str, tw=4):
    return '\n'.join([(' ' * 4) + s for s in str.split('\n')]).rstrip(' ')

# From: http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52560
def uniq(alist):
    set = {}
    return [set.setdefault(e,e) for e in alist if e not in set]

# From: http://mail.python.org/pipermail/python-list/2002-August/157829.html
def which (filename):
    if not os.environ.has_key('PATH') or os.environ['PATH'] == '':
        p = os.defpath
    else:
        p = os.environ['PATH']
    pathlist = p.split (os.pathsep)
    for path in pathlist:
        f = os.path.join(path, filename)
        if os.access(f, os.X_OK): return f
    return None

def output(stuff):
    print stuff

def outputVerbose(stuff):
    if verbose:
        output(stuff)

def mergedicts(a, *l):
    d = a.copy()
    for b in l: 
        for k in b: d[k] = b[k]
    return d    

class ExecuteException(Exception):
    def __init__(self, cmd, args, env, ret):
        self.cmd  = cmd
        self.args = args
        self.env  = env
        self.ret  = ret
    def __str__(self):
        return 'the command "%s" returned %s' % (self.cmd, self.ret)

def findFile(seekName, path, implicitExt=''):
    """Given a pathsep-delimited path string, find seekName.
    Returns path to seekName if found, otherwise None.
    Also allows for files with implicit extensions (eg, .exe), but
    always returning seekName as was provided.
    >>> findFile('ls', '/usr/bin:/bin', implicitExt='.exe')
    '/bin/ls'
    """
    if (os.path.isfile(seekName)
          or implicitExt and os.path.isfile(seekName + implicitExt)):
        # Already absolute path.
        return seekName
    for p in path.split(os.pathsep):
        candidate = os.path.join(p, seekName)
        if (os.path.isfile(candidate)):
            return candidate
        if implicitExt and os.path.isfile(candidate + implicitExt):
            return candidate + implicitExt
    return None
    
def execute(cmd, args=[], env={}):
    assert(type(args) == list)
    assert(type(env) == dict)
    env = mergedicts(os.environ, env)
    cmd_abs = findFile(cmd, env['PATH'], '.exe')
    fp = open(cmd_abs)
    line = fp.readline()
    fp.close()
    if line.startswith('#!'):
       args.insert(0, cmd_abs.replace('\\', '/'))
       cmd = line[2:].strip()
       cmd = cmd.split()
       cmd[0] = os.path.basename(cmd[0])
    else:
       cmd = [cmd]
    if verbose:
        print cmd, args, env
    # This might not always be the right thing to do! maybe check for a drive letter
    # to be more sure that it is a path we are changing...
    args = [a.replace('\\', '/') for a in args]
    if not dry_run:
        ret = subprocess.call(cmd + args, env=env, shell=False)
        # ret = os.spawnvpe(os.P_WAIT, cmd, [os.path.basename(cmd)] + args, env)
        if ret != 0: raise ExecuteException(cmd, args, env, ret)
    return # Dry-run commands always succeed

def passIfDryRun(thing):
    if dry_run:
        return True
    return thing

def banner(stuff):
    c = "*"
    l = 70
    ml = (l - len(stuff)) / 2
    odd = len(stuff) % 2
    output(c * ml + " " + stuff + " " + c * (ml + odd))

def renderPList(plist):
    def render(plist):
        if type(plist) == dict:
            s = ''
            for k in plist:
                s += '<key>%s</key>\n' % (k) + \
                     render(plist[k])
            return '<dict>\n' + indentStr(s) + '</dict>\n'
        elif type(plist) == list:
            return '<array>\n' + indentStr(''.join(map(render, plist))) + '</array>\n'
        elif type(plist) == str or type(plist) == unicode: # basestring new in 2.3
            return '<string>%s</string>\n' % plist
        elif type(plist) == bool:
            return (plist and '<true/>' or '<false/>')
        else: raise Exception('Unsupported type for plist: %s' % type(plist))
    head="""
<?xml version=1.0 encoding=UTF-8?>
<!DOCTYPE plist PUBLIC -//Apple Computer//DTD PLIST 1.0//EN http://www.apple.com/DTDs/PropertyList-1.0.dtd>
<plist version=1.0>
"""
    return head + indentStr(render(plist)) + '</plist>'

class PackageException(Exception):
    pass

class Env(object):
    class EnvException(Exception):
        pass
    class Var(object):
        def __init__(self, var):
            self.var = var
        def join(self, var):
            return var
        def __str__(self):
            return str(self.var)
        def __repr__(self):
            return '%s(\'%s\')' % (self.__class__.__name__, self.var)
    class JoinableVar(Var):
        def __init__(self, var, joinChar):
            if not type(var) == list:
                var = [var]
            Env.Var.__init__(self, var)
            self.joinChar = joinChar
        def join(self, *args):
            if len(args) == 0: raise ArgumentException("Not enough args")
            #if not issubclass(args[0], self.__class__): EnvException("Args not subclass of JoinableVar")
            if not isinstance(args[0], self.__class__): EnvException("Args not subclass of JoinableVar")
            for a in args[1:]:
                if not isinstance(args[0].__class__, a): raise EnvException("Args not of same type")
            return self.__class__(self.var + reduce(lambda x, y: x + y, map(lambda x: x.var, list(args))))
        def __str__(self):
            return self.joinChar.join(self.var)
        def __repr__(self):
            return '%s(%s)' % (self.__class__.__name__, self.var)
    class PathVar(JoinableVar):
        def __init__(self, var):
            # FIXME: Detect windows
            Env.JoinableVar.__init__(self, var, ':')
        def __str__(self):
            return self.joinChar.join(uniq(self.var))
    def __init__(self, *args, **kwargs):
        if len(args) > 0 and len(kwargs) > 0:
            raise ArgumentException("Cannot mix keyword and non keyword arguments for this class")
        if len(args) > 1:    
            raise ArgumentException("This class takes one non-keyword argument only")
        if len(args) == 1:
            # Merge
            pass
        else:
            self.envDict = {}
            for k in kwargs:
                if isinstance(kwargs[k], Env.Var):
                    self.envDict[k] = kwargs[k]
                else:
                    self.envDict[k] = Env.Var(kwargs[k])
    def join(self, env):
        s = self.envDict.copy()
        for k in env.envDict:
            if k in s:
                s[k] = s[k].join(env.envDict[k])
            else:
                s[k] = env.envDict[k]
        return Env(**s)        
    def __str__(self):
        return str(self.envDict)
    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, self.envDict)
    def expand(self):
        s = self.envDict.copy()
        for k in s:
            s[k] = str(s[k])
        return s    

           

def getKWVals(kwdict, endingWith, initial={}):
    keys = filter(lambda x: x.lower().endswith(endingWith), kwdict.keys())
    for k in keys:
        if k.lower() == endingWith:
            initial['default'] = kwdict[k]
        else:
            initial[k[:-len(endingWith)]] = kwdict[k] 
    return initial


class Package(object):

    packages = {}

    def __init__(self, name, deps=None, 
            varname=None,
            url=None,
            path=None, srcPath=None, buildPath=None, installPath=None, 
            required=True,
            buildCfg=None, 
            srcDownloader=None,
            unarchiver=None,
            **kwargs):
        # FIXME: Just keep hold of any old kwargs passed to us
        self.name = name
        if not varname: varname = name
        self.required = required
        self.url = url
        # Set the various paths, using path as the default
        self.srcPath     = srcPath     or path
        self.buildPath   = buildPath   or path
        self.installPath = installPath
        # Get every argument that ends in 'env' or 'penv' and put them in the
        # appropriate environment dictrionary
        self.env   = getKWVals(kwargs, endingWith='env', initial=dict(default=Env()))
        self.penv  = getKWVals(kwargs, endingWith='penv', initial=dict(default=Env()))
        self.args  = getKWVals(kwargs, endingWith='args', initial=dict(default=[]))
        self.buildCfg    = buildCfg
        self.retriever   = srcDownloader
        if self.retriever == None and url == None:
            self.retriever = SrcRetriever
        elif self.retriever == None and url != None:
            if url.startswith('http') or url.startswith('ftp'):
                self.retriever = HTTPDownloader
            else:
                raise ArgumentException('Dont know URL scheme of url: %s' % url)
        else:
           raise ArgumentException('Arg(h)!')
        self.unarchiver = unarchiver
        if self.unarchiver == None and self.getRetriever().getSrcArchive():
            print self.getRetriever().getSrcArchive()
            if self.getRetriever().getSrcArchive().endswith('.tar.gz'):
                self.unarchiver = TGZUnarchiver
            else:
                self.unarchiver = Unarchiver
        else:
            self.unarchiver = Unarchiver
        if deps:
            self.dependencies = uniq(deps)
        else:
            self.dependencies = None
        if name in Package.packages: 
            raise PackageException("Package %s already exists" % name)    
        if varname in globals():
            raise PackageException(
            ("Package %s already exists (package name already in globals, " +
            "use 'varname=somename' to create a variable with a different " +
            "name, or 'varname=None' to not create a variable at all") % name)
        globals()[varname] = self    
        Package.packages[name] = self

    def depTree(self, first=True):
        s = ""
        if self.dependencies:
            for dep in self.dependencies:
                s += dep.name + '\n'
                r = dep.depTree(first=False)
                if r: s += indentStr(r)
        if first:        
            return "[%s]\n%s" % (self.name, indentStr(s))
        return s

    def deps(self, first=True):
        s = []
        if self.dependencies:
            for dep in self.dependencies:
                s += dep.deps(first=False)
        if first: return s        
        return s + [self]

    def getArgs(self, sub=None):
        if sub: 
            if sub in self.args: return self.args[sub]
        return self.args['default']

    def getEnv(self, sub='default'):
        return reduce(lambda x, y: x.join(y), self.__getEnv(sub), Env()).expand()

    def __getEnv(self, sub='default', start=True):
        cEnv = []
        deps = self.dependencies
        if deps:
            for dep in deps:
                cEnv += dep.__getEnv(sub, start=False)
        e = (start and self.env or self.penv)        
        if sub in e: return [e[sub]] + cEnv
        return [e['default']] + cEnv

    def getBuilder(self):
        if self.buildCfg: return self.buildCfg(self)
        return None

    def getRetriever(self):
        if self.retriever: return self.retriever(self)
        return None

    def getUnarchiver(self):
        if self.unarchiver: return self.unarchiver(self)
        return None

    def buildOrder(cls, pkg = None):
        if pkg == None:
            packages = copy.deepcopy(cls.packages)
        elif type(pkg) == dict:
            packages = copy.deepcopy(pkg)
        elif type(pkg) == str:
            pkg = cls.packages[pkg]
            if not pkg.dependencies: 
                yield pkg
                return 
            npackages = copy.deepcopy(cls.packages)
            toCheck  = [npackages[pkg.name]]
            checked  = []
            packages = {pkg.name: npackages[pkg.name]}
            while toCheck:
                c = toCheck.pop()
                checked.append(c.name)
                if c.dependencies: 
                    for d in c.dependencies:
                        if d.name not in packages:
                            packages[d.name] = npackages[d.name]
                        if d not in checked:
                            toCheck.append(npackages[d.name])
        while len(packages) != 0:
            for pName in copy.copy(packages):
                p = packages[pName]
                if p.dependencies == None or len(p.dependencies) == 0:
                    del packages[pName]
                    for pxName in packages:
                        if packages[pxName].dependencies and p in packages[pxName].dependencies:
                            packages[pxName].dependencies.remove(p)
                    yield cls.packages[p.name]        
    buildOrder = classmethod(buildOrder)

    def __repr__(self):
        return "<%s object: %s>" % (self.__class__.__name__, self.name)

P = Package.packages

class Builder(object):
    def __init__(self, package=None):
        self.package  = package
        if package == None:
            self.packages = Package.packages
        else:
            self.packages = {package.name: package}
            for p in package.deps():
                self.packages[p.name] = p
    def rdepend(self, pkg=None):
        pkgs = (pkg and [pkg] or self.packages)
        rdep = {}
        for p in pkgs:
            for d in pkgs[p].deps():
                if (d in rdep):
                    if (p not in rdep[d]):
                        rdep[d] = rdep[d] + [p]
                else:
                    rdep[d] = [p]
        print rdep        
        return rdep        
    def download(self):
        for p in Package.buildOrder(self.packages):
            p.getRetriever().download()
    def unarchive(self):
        for p in Package.buildOrder(self.packages):
            print p.getUnarchiver()
            if p.getUnarchiver():
                p.getUnarchiver().unarchive()
    def build(self):
        for p in Package.buildOrder(self.packages):
            banner(p.name)
            try:
                step = 'bootstraping'
                p.getBuilder().bootstrap()
                step = 'configuring'
                p.getBuilder().config()
                step = 'building'
                p.getBuilder().build()
            except ExecuteException, e:
                if p.required:
                    # FIXME: use error output fn
                    print 'error: command "%s" failed with error code %s while %s "%s"' % (e.cmd, e.ret, step, p.name)
                    # FIXME: On verbose, print args and env
                    sys.exit(1)
                else:
                    raise "prune"
    def clean(self):
        pkgs = list(Package.buildOrder(self.packages))
        pkgs.reverse()
        for p in pkgs:
            banner(p.name)
            p.getBuilder().clean()
    def veryclean(self):
        pkgs = list(Package.buildOrder(self.packages))
        pkgs.reverse()
        for p in pkgs:
            banner(p.name)
            p.getBuilder().veryclean()

class SrcRetriever(object):
   def __init__(self, pkg):
      self.pkg = pkg
   def getSrcPath(self):
      return self.pkg.srcPath
   def getSrcArchive(self):
      return None
   def download(self):
      pass

class HTTPDownloader(SrcRetriever):
    def getSrcArchive(self):
        dest = urlparse.urlsplit(self.pkg.url)
        dest = os.path.basename(dest.path)
        return dest
    def download(self):
        class Reporter(object):
            numdots = 40
            def __init__(self):
                self.printed = 0 
            def __call__(self, tfr, bsize, tsize): 
                prnt = (tfr * self.numdots) / (tsize / bsize) - self.printed
                if prnt > 0:
                    for i in range(prnt):
                        #outputNoLog('.')
                        sys.stdout.write('.')
                self.printed += prnt
                sys.stdout.flush()
        dest = self.getSrcArchive()        
        if not os.path.exists(dest):
            output("Downloading %s" % self.pkg.url)
            urllib.urlretrieve(self.pkg.url, dest, Reporter())
            sys.stdout.write('\n')

class Unarchiver(object):
    def __init__(self, pkg):
        self.pkg = pkg
    def unarchive(self):
        pass

class TGZUnarchiver(Unarchiver):
    def unarchive(self):
        import tarfile
        #try:
        #    os.makedirs(self.pkg.srcPath)
        #except WindowsError:
        #    pass
        #os.chdir(self.pkg.srcPath)
        archive = self.pkg.getRetriever().getSrcArchive()
        fp = tarfile.open(archive, 'r:gz')
        fp.extractall()
        fp.close()

class BuildCfg(object):
    def __init__(self, pkg):
        self.pkg = pkg
    def cd(self, name, dir):    
        outputVerbose("Changing cwd to %s dir: %s" % (name, dir))
        os.chdir(dir)
    def cdSrc(self):
        self.cd('src', self.pkg.srcPath)
    def cdBuild(self):    
        self.cd('build', self.pkg.buildPath)
    def cdInstall(self):
        self.cd('install', self.pkg.installPath)
    # FIXME: Perhaps move these out into another class (subclass of this one)
    # which other classes can subclass if they want to implement only a few
    # steps    
    def bootstrap(self):
        pass
    def config(self):
        pass
    def build(self):
        pass
    def install(self):
        pass
    def clean(self):
        pass
    def veryclean(self):
        pass

class Autotools(BuildCfg):
    def bootstrap(self):
        self.cdSrc()
        outputVerbose("Checking whether to boostrap %s" % self.pkg.name)
        if os.access("configure", os.X_OK):
            for c in ["configure.ac", "configure.in", None]:
                if c == None or os.path.exists(c): break
            if c == None:
                outputVerbose("Could not find the source configure script, might be ok")
                return # This could happen for handwritten configure scripts
            if os.path.getmtime(c) > os.path.getmtime("configure"):
                output("%s has changed!" % c)
            else: return
        output("Bootstrapping %s" % self.pkg.name)
        execute('autoreconf', ['-i'] + self.pkg.getArgs('bootstrap'), self.pkg.getEnv('bootstrap'))
        if passIfDryRun(not os.access("configure", os.X_OK)):
            fatal("Could not bootstrap %s, " \
                  "configure script missing after autoreconf -i" % self.pkg.name)
    def config(self):
        self.cdBuild()
        if (not os.path.exists("config.status")) or \
                (os.path.getmtime("config.status") < os.path.getmtime(os.path.join(self.pkg.srcPath, 'configure'))):
            output("Configuring %s" % self.pkg.name)
            execute(os.path.join(self.pkg.srcPath, 'configure'),
                    self.pkg.getArgs('config'), self.pkg.getEnv('config'))
        else:
            output("Configuration for %s seems up to date..." % self.pkg.name)
    def build(self):
        self.cdBuild()
        output("Building %s" % self.pkg.name)
        execute('make', self.pkg.getArgs('build'), self.pkg.getEnv('build'))
    def check(self):
        pass
    def install(self):
        pass
    def clean(self):
        self.cdBuild()
        output("Cleaning %s" % self.pkg.name)
        execute('make', ['clean'] + self.pkg.getArgs('clean'), self.pkg.getEnv('clean'))
    def veryclean(self):
        self.cdBuild()
        output("Dist-Cleaning %s" % self.pkg.name)
        execute('make', ['distclean'] + self.pkg.getArgs('clean'), self.pkg.getEnv('clean'))
   
#class TvmAppDir(BuildCfg):
#    plist=dict(
#            CFBundleIdentifier='org.transterpreter.transterpreter',
#            CFBundleName='Transterpreter',
#            CFBundlePackageType='APPL',
#            CFBundleShortVersionString='CHANGEME',
#            CFBundleSignature='tptr', # FIXME: Is this valid? does it have to be registered w/apple???
#            NSAppleScriptEnabled='No',
#            LSHasLocalizedDisplayName=True,
#            NSHumanReadableCopyright='Copyright %s Transterpreter.org' % '2008', # FIXME: automagic
#            CFBundleIconFile='Transterpreter.icns',
#            CFBundleExecutable='JavaApplicationStub',
#            Java=dict(
#                Arguments='-background -settings=~/.transterpreter/jEdit',
#                ClassPath='Transterpreter.app/Contents/Resources/jEdit/jedit.jar:/System/Library/Java/',
#                JVMVersion='1.4+',
#                MainClass='org.gjt.sp.jedit.jEdit'),
#            CFBundleDocumentTypes=[
#                dict(
#                    CFBundleTypeIconFile='text.icns',
#                    CFBundleTypeName='Transterpreter bytecode',
#                    CFBundleTypeExtensions=[ 'tbc' ],
#                    CFBundleTypeRole='Viewer'),
#                dict(
#                    CFBundleTypeIconFile='text.icns',
#                    CFBundleTypeName='occam sourcecode',
#                    CFBundleTypeExtensions=[ 'occ' ],
#                    CFBundleTypeRole='Editor')])
#    def build(self):
#        def dotapp(x, *args): return os.path.join(x, 'Transterpreter.app', *args)
#        # Make directories and copy the Transterpreter.app skeleton
#        # FIXME: Get rid of the .gitignore files in that directory structure
#        outputPath = os.path.join(self.pkg.buildPath, 'osx')
#        if not os.path.exists(outputPath): os.makedirs(outputPath)
#        if os.path.exists(dotapp(outputPath)): shutil.rmtree(dotapp(outputPath))
#        shutil.copytree(dotapp(self.pkg.srcPath), dotapp(outputPath))
#        # Write the plist
#        fd = open(dotapp(outputPath, 'Contents', 'Info.plist'), 'w+')
#        fd.write(renderPList(self.plist))
#        fd.close()
#        # Copy the java application stub
#        shutil.copy(
#                '/System/Library/Frameworks/JavaVM.framework/Versions/A/Resources/MacOS/JavaApplicationStub',
#                dotapp(outputPath, 'Contents', 'MacOS'))
#        # Extract jEdit into the app bundle
#        self.cd('jEdit', dotapp(outputPath, 'Contents', 'Resources', 'jEdit'))
#        execute('tar', ['-xjf'] + [os.path.join(jedit.buildPath, 'installer', 'jedit-program.tar.bz2')])
#        execute('tar', ['-xjf'] + [os.path.join(jedit.buildPath, 'installer', 'jedit-mac.tar.bz2')])
#        # Copy occPlug
#        # FIXME: This should be fetched and compiled???
#        shutil.copy('/Users/clj3/.jedit/jars/OccPlug.jar', dotapp(outputPath, 'Contents', 'Resources', 'jEdit', 'jars'))
#        occPlugProps = """
#occPlug.dock-position=bottom
#options.occPlug.skrocCmd=skroc
#options.occPlug.tvmCmd=tvm
#options.occPlug.tvmArgs=
#options.occPlug.MasterLibraryPath=
#
#options.occPlug.verbose=false
#
#occPlug.TransterpreterVersion=%s
#""" % ('CHANGEME')
#        fd = open(dotapp(outputPath, 'Contents', 'Resources', 'jEdit', 'properties', 'occPlug.props'), 'w+')
#        fd.write(occPlugProps)
#        fd.close()
#        # Copy mzscheme runtime
#        pltPath=os.path.dirname(os.path.dirname(which('mzc')))
#        shutil.copytree(
#                os.path.join(pltPath, 'lib', 'PLT_MzScheme.framework'),
#                dotapp(outputPath, 'Contents', 'Resources', 'bin', 'PLT_MzScheme.framework'))
#        # Copy tools
#        exes = [skroc, slinker, (slinker, 'library2'), ilibr, occ21, (posix, 'tvm')] #, tinyswig]
#        for exe in exes:
#            if type(exe) == tuple:
#                exe, name = exe
#            else:
#                exe, name = (exe, exe.name)
#            shutil.copy(
#                    os.path.join(exe.buildPath, name), 
#                    dotapp(outputPath, 'Contents', 'Resources', 'bin'))
#        # Copy the include file
#        shutil.copy(
#                os.path.join(libtvm.buildPath, 'tvm_config.h'),
#                dotapp(outputPath, 'Contents', 'Resources', 'include'))
#        # Copy Modules    
#        modules = [course, fmtoutlib, 
#                (inmoslibs, 'convert', 'forall', 'hostio', 'hostsp', 'maths', 'streamio', 'string'), 
#                (bsclib, 'filelib', 'httplib', 'proclib', 'socklib'),
#                useful] 
#        def copymodule(path):
#            for file in os.listdir(path):
#                if os.path.splitext(file)[1] in ['.lib', '.inc', '.module', '.precomp']:
#                    shutil.copy(
#                            os.path.join(path, file),
#                            dotapp(outputPath, 'Contents', 'Resources', 'lib'))
#        for module in modules:
#            if type(module) == tuple:
#                for m in module[1:]:
#                    copymodule(os.path.join(module[0].srcPath, m))
#            else:
#                copymodule(module.srcPath)
#    def veryclean(self):
#        pass
#
#class JEditExtract(BuildCfg):
#    def build(self):
#        self.cdBuild()
#        # FIXME: Make things that can download stuff, ie http, git, svn, darcs,
#        # etc
#        jarFile = os.path.join(self.pkg.buildPath, os.path.basename(self.pkg.url))
#        if not os.path.exists(self.pkg.buildPath): os.makedirs(self.pkg.buildPath)
#        if not os.path.exists(jarFile):
#            urllib.urlretrieve(self.pkg.url, jarFile)
#        execute('jar', ['-xf'] + [jarFile])
#
#class Tool(Package):
#    def __init__(self, *args, **kwargs):
#        if not 'path' in kwargs:
#            kwargs['path'] = os.path.join(ROOT, 'tools', args[0])
#        if not 'buildCfg' in kwargs:
#            kwargs['buildCfg'] = Autotools
#        kwargs['penv'] = Env(
#                SKROCPATH=Env.PathVar(os.path.join(ROOT, kwargs['path']))).join(kwargs.get('penv', Env()))
#        Package.__init__(self, *args, **kwargs)
#
#class Runtime(Package):
#    def __init__(self, *args, **kwargs):
#        kwargs['path'] = os.path.join(ROOT, 'runtime', args[0])
#        if not 'buildCfg' in kwargs:
#            kwargs['buildCfg'] = Autotools
#        Package.__init__(self, *args, **kwargs)
#
#class Module(Package):
#    args = []
#    env  = Env()
#    def setTargetArgs(cls, args):
#        cls.args = args
#    setTargetArgs = classmethod(setTargetArgs)    
#    def setTargetEnv(cls, env):
#        cls.env  = env
#    setTargetEnv = classmethod(setTargetEnv)    
#    def setDeps(cls, deps):
#        cls.dependencies  = deps
#    setDeps = classmethod(setDeps)    
#    def __init__(self, *args, **kwargs):
#        if not 'path' in kwargs:
#            kwargs['path'] = os.path.join(ROOT, 'modules', args[0], 'libsrc')
#        if not 'buildCfg' in kwargs:
#            kwargs['buildCfg'] = Autotools
#        if not 'required' in kwargs:
#            kwargs['required'] = False
#        kwargs['configArgs'] = self.args
#        kwargs['env'] = self.env
#        kwargs['penv'] = Env(ISEARCH=Env.PathVar(os.path.join(ROOT, 'modules', args[0],
#            'libsrc'))).join(kwargs.get('penv', Env()))
#        kwargs['deps'] = self.dependencies + kwargs.get('deps', [])
#        Package.__init__(self, *args, **kwargs)
#
#class ModuleDemos(Package):
#    def __init__(self, *args, **kwargs):
#        if not 'path' in kwargs:
#            kwargs['path'] = os.path.join(ROOT, 'modules', args[0], 'examples')
#        if not 'buildCfg' in kwargs:
#            kwargs['buildCfg'] = Autotools
#        nargs = ['Module examples: ' + args[0]] + list(args)[1:]
#        kwargs['deps'] = [globals()[args[0]]] + kwargs.get('deps', [])
#        Package.__init__(self, *nargs, **kwargs)
#
#class Demos(Package):
#    def __init__(self, *args, **kwargs):
#        if not 'path' in kwargs:
#            kwargs['path'] = os.path.join(ROOT, 'demos', args[0])
#        if not 'buildCfg' in kwargs:
#            kwargs['buildCfg'] = Autotools
#        nargs = ['Examples: ' + args[0]] + list(args)[1:]
#        kwargs['deps'] = globals()['compile'] + kwargs.get('deps', [])
#        Package.__init__(self, *nargs, **kwargs)
#
#class Tests(Package):
#    def __init__(self, *args, **kwargs):
#        if not 'path' in kwargs:
#            kwargs['path'] = os.path.join(ROOT, 'tests', args[0])
#        if not 'buildCfg' in kwargs:
#            kwargs['buildCfg'] = Autotools
#        nargs = ['Tests: ' + args[0]] + list(args)[1:]
#        kwargs['deps'] = globals()['compile'] + [globals()['inmoslibs']] + kwargs.get('deps', [])
#        Package.__init__(self, *nargs, **kwargs)
#
#class TVMTarget(Package):
#   def __init__(self, *args, **kwargs):
#        if not 'path' in kwargs:
#            kwargs['path'] = os.path.join(ROOT, 'tvm', args[0])
#        if not 'buildCfg' in kwargs:
#            kwargs['buildCfg'] = Autotools
#        kwargs['deps'] = globals()['compile'] + [globals()['libtvm']] + kwargs.get('deps', [])
#        Package.__init__(self, *args, **kwargs)
#
######################################################################
## Tools
######################################################################
#
#Tool("occ21")
#Tool("ilibr")
#Tool("slinker")
#Tool('skroc', 
#     deps=[occ21, slinker, ilibr],
#     penv=Env(
#         SKROC_TVM_CONFIG_H=os.path.join(ROOT, 'runtime', 'libtvm', 'tvm_config.h'),
#         SKROC=os.path.join(ROOT, 'tools', 'skroc', 'skroc')
#         ))
#Package("occbuild", 
#        path=os.path.join(ROOT, 'tools', 'kroc'),
#        deps=[skroc],
#        buildCfg=Autotools,
#        penv=Env(OCCBUILD=os.path.join(ROOT, 'tools', 'kroc', 'occbuild')))
#
######################################################################
## Runtimes
######################################################################
#
#Runtime("libtvm")
#
## Dependency bundles
#compile    = [occbuild, libtvm]
#libcompile = [occbuild, libtvm]
#
######################################################################
## Transterpreter targets
######################################################################
#
#TVMTarget('posix')
#
######################################################################
## Modules
######################################################################
#
## Defaults for modules
#Module.setTargetArgs(['--with-toolchain=tvm'])
#Module.setTargetEnv(Env(ISEARCH=Env.PathVar(os.path.join(ROOT, 'tvm', 'posix'))))
#Module.setDeps(libcompile)
#
#Module("inmoslibs", 
#        required=True,
#        penv=Env(ISEARCH=Env.PathVar(
#            map(lambda d: os.path.join(ROOT, 'modules', 'inmoslibs', 'libsrc', d),
#                ['convert', 'forall', 'hostio', 'hostsp', 'maths', 'streamio', 'string']))))
#Module("course")
#Module("bsclib", 
#        penv=Env(ISEARCH=Env.PathVar(
#            map(lambda d: os.path.join(ROOT, 'modules', 'bsclib', 'libsrc', d),
#                ['cspdrvlib', 'filelib', 'httplib', 'proclib', 'socklib']))))
##configureAndBuildModule("cdx", extra_env=module_env)
##Module("dynproc", deps=[course, inmoslibs])
#Module('fmtoutlib', deps=[inmoslibs])
##Module('moa')
#
#Module("occGL", deps=[inmoslibs])
#Module("occSDL", deps=[inmoslibs])
#Module("occade", deps=[inmoslibs, occGL, occSDL])
##configureAndBuildModule("pony", extra_env=module_env)
##configureAndBuildModule("raster", extra_env=module_env)
##configureAndBuildModule("sdlraster", extra_env=module_env)
##configureAndBuildModule("udc", extra_env=module_env)
#Module("useful", deps=[course, bsclib, inmoslibs])
#
######################################################################
## Module examples
######################################################################
#ModuleDemos('course')
#ModuleDemos('bsclib')
##ModuleDemos('dynproc')
#
######################################################################
## Other examples
######################################################################
#Demos('bar', deps=[course])
#Demos('ttygames', deps=[course])
#
######################################################################
## Tests
######################################################################
#Tests('cgtests')
#Tests('hereticc')
#
######################################################################
## Installers
######################################################################
#Package('jedit',
#        url='http://downloads.sourceforge.net/jedit/jedit42install.jar',
#        buildPath=os.path.join(ROOT, 'output', 'jEdit'),
#        buildCfg=JEditExtract)
#Package('.app dir', varname='appdir',
#        srcPath=os.path.join(ROOT, 'packages'),
#        buildPath=os.path.join(ROOT, 'output'),
#        deps=[skroc, jedit],
#        buildCfg=TvmAppDir)
#
#
#b = Builder(appdir)
##b.rdepend()
#b.build()
##b.clean()
##b.veryclean()
#
##print "dependency tree"
##
##for p in P:
##    print P[p].depTree(),
##
##print "Build order for everything"
##
##for o in Package.buildOrder():
##    print o
##
##print 
##
##for p in P:
##    print "To build:", p
##    print "  Env:", P[p].getEnv()
##    for o in Package.buildOrder(P[p]):
##        print '  ' + str(o)
## vim:ts=4:sw=4:et:    
