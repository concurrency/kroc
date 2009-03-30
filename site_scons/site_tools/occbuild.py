# See: http://www.scons.org/wiki/UsingCodeGenerators
from SCons.Builder import Builder
from SCons.Action  import Action
from SCons.Scanner import Scanner, FindPathDirs
from SCons.Environment import Environment
from SCons.Util import AddMethod, CLVar
import os
import pideps

# Monkeypatching
pideps.sourceprefix = ""
pideps.die = pideps.warn
def find_dep(fn, dep, incpath):
    for dir in [os.path.dirname(fn)] + incpath:
        dpath = os.path.normpath(os.path.join(dir, dep))
        if os.access(dpath, os.F_OK):
            return dpath
    return os.path.join(os.path.dirname(fn), dep)
pideps.find_dep = find_dep


# TODO: This generally didn't work out as well as I'd hoped, but it it still 
# better than typing in dependencies by hand for now. 
# This can be looked at later.
def pideps_scan(node, env, path):
    # FIXME: occbuild sets OCCBUILD.TVM and presumably OCCBUILD.KROC, add those
    # here automatically? Evaluate and extract -D out the $OCCBUILDCOMSTR to see 
    # what to add here
    defines = {}
    binaries = []
    deps = {}
    path = [os.path.abspath(d.path) for d in path]
    pideps.parse(node.path, deps, path, binaries, defines)
    deplines = deps.keys()
    #deplines.sort()
    deplines = [l.split(' ')[1].split(' ') for l in deplines]
    deplines = reduce(lambda x, y: x + y, deplines, [])
    deplines = [os.path.abspath(x) for x in deplines]
    print deplines
    return deplines

# This emitter will be used later by a Builder, and set an explicit dependency
# on the argument passed in as 'dependency', most probably occbuild
def occbuild_depend_emitter(target, source, env, dependency):
    env.Depends(target, dependency)
    return (target, source)

def occbuild_program_emitter(target, source, env):
    # FIXME: For kroc compile no .tbc file will be generated
    target_name     = str(target[0])
    if target[0].suffix:
        tbc         = target_name.replace(target[0].suffix, '.tbc')
    else:
        tbc         = target_name + '.tbc'
    target          = target + [tbc]
    return (target, source)

def occbuild_library_emitter(target, source, env):
    # FIXME: for kroc compile, no precomp will be generated
    target_name = str(target[0])
    precomp     = target_name.replace(target[0].suffix, '.precomp')
    module      = target_name.replace(target[0].suffix, '.module')
    if len(source) != 0:
        # No precomp is generated if there are no sources, ie a 'meta' library.
        target.append(precomp)
    target.append(module)
    return (target, source)

def generate(env, **kw):
    occbuild = kw.get('occbuild', None)
    if occbuild:
        occbuild_path  = occbuild[0].abspath
        depend_emitter = lambda target, source, env: occbuild_depend_emitter(target, source, env, occbuild)
    else:
        occbuild_path  = 'occbuild'
        depend_emitter = None
    pideps_scanner = Scanner(function = pideps_scan, skeys = ['.occ'], path_function = FindPathDirs('INCPATH'))
    tce_bld = Builder(action = Action('$OCCBUILDCOM', '$OCCBUILDCOMSTR'),
                      emitter = depend_emitter,
                      suffix = '.tce',
                      src_suffix = '.occ')
                      # FIXME: The source scanner does not work well enough yet :/
                      #source_scanner = pideps_scanner)
    lib_bld = Builder(action = Action('$OCCBUILDLIBRARYCOM', '$OCCBUILDLIBRARYCOMSTR'),
                      emitter = [depend_emitter, occbuild_library_emitter],
                      suffix = '.lib',
                      src_suffix = '.tce',
                      src_builder = [tce_bld])
    prog_bld = Builder(action = Action('$OCCBUILDPROGRAMCOM', '$OCCBUILDPROGRAMCOMSTR'),
                      emitter = [depend_emitter, occbuild_program_emitter],
                      suffix='$PROGSUFFIX',
                      src_suffix = ['.occ', '.tce'],
                      # FIXME: If I leave the sourcebuilder in, scons seems to
                      # want to turn my .occ extensions when I have a mixed
                      # .occ, .tce source list into .tce using the builder
                      )#src_builder = [tce_bld])
    tbc_headr_bld = Builder(action = Action('$TBCHEADERCOM', '$TBCHEADERCOMSTR'),
                      emitter = [depend_emitter],
                      suffix='.h',
                      src_suffix = ['.occ', '.tce'],
                      src_bulider = [tce_bld])
    # Add the new Builder to the list of builders
    # Use of $( $)  causes bracketed flags not trigger rebuild when changed
    env['BUILDERS']['OccamObject']  = tce_bld
    env['OCCBUILDCOM']              = '$OCCBUILD $_OCCBUILD_TOOLCHAIN $_OCCBUILD_SEARCH_DIRS $OCCBUILDFLAGS --object $SOURCES'
    env['BUILDERS']['OccamLibrary'] = lib_bld
    env['OCCBUILDLIBRARYCOM']       = '$OCCBUILD $_OCCBUILD_TOOLCHAIN $_OCCBUILD_SEARCH_DIRS $OCCBUILDFLAGS --library $TARGET $SOURCES'
    env['BUILDERS']['OccamProgram'] = prog_bld
    env['OCCBUILDPROGRAMCOM']       = '$OCCBUILD $_OCCBUILD_TOOLCHAIN $_OCCBUILD_SEARCH_DIRS $OCCBUILDFLAGS --program $SOURCES'
    env['BUILDERS']['OccamBytecodeHeader'] = tbc_headr_bld
    env['TBCHEADERCOM']             = '$SKROC $_SKROC_SEARCH_DIRS $SKROCFLAGS --c -f $TARGET $SOURCES'
    env['OCCBUILD']                 = occbuild_path
    env['_OCCBUILD_SEARCH_DIRS']    = '$( ${_concat(OCCBUILD_SEARCH_PFX, INCPATH, "", __env__, RDirs, TARGET, SOURCE)} $)'
    env['_SKROC_SEARCH_DIRS']       = '$( ${_concat(SKROC_SEARCH_PFX, INCPATH, "", __env__, RDirs, TARGET, SOURCE)} $)'
    env['OCCBUILD_SEARCH_PFX']      = '--search '
    env['SKROC_SEARCH_PFX']         = '-L '
    env['OCCBUILD_TOOLCHAIN']       = None
    env['_OCCBUILD_TOOLCHAIN']      = '${(OCCBUILD_TOOLCHAIN and "--toolchain $OCCBUILD_TOOLCHAIN" or "")}'
    def OccLibDepend(self, node, lib_name):
        if not isinstance(lib_name, list): list(lib_name)
        for lib in lib_name:
            self.Depends(node, self['OCCLIBS'][lib]['dep'])
            if 'inc' in self['OCCLIBS'][lib]:
                for n in node:
                    n.env.AppendUnique(INCPATH=self['OCCLIBS'][lib]['inc'])
    env.AddMethod(OccLibDepend)
    env['OCCLIBS'] = dict()
    env['INCPATH'] = CLVar('')
    env['OCCBUILDFLAGS'] = CLVar('')



def exists(env):
    return 1
