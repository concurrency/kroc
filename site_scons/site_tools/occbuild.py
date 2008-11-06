# See: http://www.scons.org/wiki/UsingCodeGenerators
from SCons.Builder import Builder
from SCons.Action  import Action
from SCons.Scanner import Scanner, FindPathDirs
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
def occbuild_emitter(target, source, env, dependency):
    env.Depends(target, dependency)
    return (target, source)

def generate(env, **kw):
    occbuild = kw.get('occbuild', None)
    if occbuild:
        occbuild_path = occbuild[0].abspath
        emitter       = lambda target, source, env: occbuild_emitter(target, source, env, occbuild)
    else:
        occbuild_path = 'occbuild'
        emitter       = None
    pideps_scanner = Scanner(function = pideps_scan, skeys = ['.occ'], path_function = FindPathDirs('INCPATH'))
    tce_bld = Builder(action = Action('$OCCBUILDCOM', '$OCCBUILDCOMSTR'),
                      emitter = emitter,
                      suffix = '.tce',
                      src_suffix = '.occ')
                      # FIXME: The source scanner does not work well enough yet :/
                      #source_scanner = pideps_scanner)
    lib_bld = Builder(action = Action('$OCCBUILDLIBRARYCOM', '$OCCBUILDLIBRARYCOMSTR'),
                      emitter = emitter,
                      suffix = '.lib',
                      src_suffix = '.tce',
                      src_builder = [tce_bld])
    prog_bld = Builder(action = Action('$OCCBUILDPROGRAMCOM', '$OCCBUILDPROGRAMCOMSTR'),
                      emitter = emitter,
                      src_suffix = ['.occ', '.tce'],
                      src_builder = [tce_bld])
    # Add the new Builder to the list of builders
    # Use of $( $)  causes bracketed flags not trigger rebuild when changed
    env['BUILDERS']['OccamObject']  = tce_bld
    env['OCCBUILDCOM']              = '$OCCBUILD $_OCCBUILD_TOOLCHAIN $_OCCBUILD_SEARCH_DIRS $OCCBUILDFLAGS --object $SOURCES'
    env['BUILDERS']['OccamLibrary'] = lib_bld
    env['OCCBUILDLIBRARYCOM']       = '$OCCBUILD $_OCCBUILD_TOOLCHAIN $_OCCBUILD_SEARCH_DIRS $OCCBUILDFLAGS --library $TARGET $SOURCES'
    env['BUILDERS']['OccamProgram'] = prog_bld
    env['OCCBUILDPROGRAMCOM']       = '$OCCBUILD $_OCCBUILD_TOOLCHAIN $_OCCBUILD_SEARCH_DIRS $OCCBUILDFLAGS --program $SOURCES'
    env['OCCBUILD']                 = occbuild_path
    env['_OCCBUILD_SEARCH_DIRS']    = '$( ${_concat(OCCBUILD_SEARCH_PFX, INCPATH, "", __env__, RDirs, TARGET, SOURCE)} $)'
    env['OCCBUILD_SEARCH_PFX']      = '--search '
    env['OCCBUILD_TOOLCHAIN']       = None
    env['_OCCBUILD_TOOLCHAIN']      = '${(OCCBUILD_TOOLCHAIN and "--toolchain $OCCBUILD_TOOLCHAIN" or "")}'

def exists(env):
    return 1
