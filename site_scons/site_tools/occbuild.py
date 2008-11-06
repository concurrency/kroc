# See: http://www.scons.org/wiki/UsingCodeGenerators
from SCons.Builder import Builder
from SCons.Action  import Action
import os



# This emitter will be used later by a Builder, and has an explcit dependency on
# occbuild
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
    tce_bld = Builder(action = Action('$OCCBUILDCOM', '$OCCBUILDCOMSTR'),
                      emitter = emitter,
                      suffix = '.tce',
                      src_suffix = '.occ')
    lib_bld = Builder(action = Action('$OCCBUILDLIBRARYCOM', '$OCCBUILDLIBRARYCOMSTR'),
                      emitter = emitter,
                      suffix = '.lib',
                      src_suffix = '.tce',
                      src_builder = [tce_bld])
    # Add the new Builder to the list of builders
    env['BUILDERS']['OccamObject']  = tce_bld
    env['OCCBUILDCOM']              = '$OCCBUILD $_OCCBUILD_TOOLCHAIN $_OCCBUILD_SEARCH_DIRS $OCCBUILDFLAGS --object $SOURCES'
    env['BUILDERS']['OccamLibrary'] = lib_bld
    env['OCCBUILDLIBRARYCOM']       = '$OCCBUILD $_OCCBUILD_TOOLCHAIN $_OCBUILD_SEARCH_DIRS $OCCBUILDFLAGS --library $TARGET $SOURCES'
    env['OCCBUILD']                 = occbuild_path
    env['_OCCBUILD_SEARCH_DIRS']    = '$( ${_concat(OCCBUILD_SEARCH_PFX, INCPATH, "", __env__, RDirs, TARGET, SOURCE)} $)'
    env['OCCBUILD_SEARCH_PFX']      = '--search '
    env['OCCBUILD_TOOLCHAIN']       = None
    env['_OCCBUILD_TOOLCHAIN']      = '$( ${(OCCBUILD_TOOLCHAIN and "--toolchain $OCCBUILD_TOOLCHAIN" or "")} $)'

def exists(env):
    return 1
