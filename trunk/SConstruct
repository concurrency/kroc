import os

# TODO:
#   * The make driver scripts seem to always be called.


# Create a global environment
tools = ['default',
         'subst',
	 'scheme']
env  = Environment(tools=tools,
		   ENV = {'PATH' : os.environ['PATH']})

# Pretty builds
comstrings = dict(CCCOMSTR              = 'Compiling $TARGET',
                  LINKCOMSTR            = 'Linking $TARGET',
		  ARCOMSTR              = 'Creating archive $TARGET',
		  RANLIBCOMSTR          = 'Indexing archive $TARGET',
		  MZCCOMSTR             = 'Compiling $TARGET',
		  OCCBUILDCOMSTR        = 'Compiling $TARGET',
		  OCCBUILDLIBRARYCOMSTR = 'Linking $TARGET',
		  OCCBUILDPROGRAMCOMSTR = 'Compiling $TARGET',
		  TBCHEADERCOMSTR       = 'Compiling bytecode header $TARGET')
if ARGUMENTS.get('VERBOSE') != '1':
    for k in comstrings:
	env[k] = comstrings[k]


# Export it for use in the SConscripts
Export('env')

# Build mkoccdeps
SConscript('tools/mkoccdeps/SConscript')
SConscript('tools/ilibr/SConscript')

SConscript('tools/schemescanner/SConscript')
SConscript('tools/tinyswig/SConscript')
SConscript('tools/skroc/SConscript')
SConscript('tools/slinker/SConscript')
SConscript('tools/occ21/SConscript')
SConscript('tools/kroc/SConscript')

SConscript('runtime/libtvm/SConscript')

# Ensure that things that build with occbuild have triggered the building of all
# the required tools, there might be a better palce and better way to do this
env.Depends(env['SKROC'],    env['SCHEMESCANNER'])
env.Depends(env['SLINKER'],  env['SCHEMESCANNER'])
env.Depends(env['LIBRARY2'], env['SCHEMESCANNER'])
env.Depends(env['OCCBUILD'], env['SKROC'])
env.Depends(env['OCCBUILD'], env['ILIBR'])
env.Depends(env['OCCBUILD'], env['OCC21'])
env.Depends(env['OCCBUILD'], env['SLINKER'])
env.Depends(env['OCCBUILD'], env['LIBRARY2'])
env.Tool('occbuild', occbuild=env['OCCBUILD'])

# Not sure if this is necessary
env.Depends(env['SKROC'], env['TVM_CONFIG_H'])

env['OCCBUILD_TOOLCHAIN'] = 'tvm'


SConscript('modules/inmoslibs/libsrc/SConscript')
SConscript('modules/course/SConscript')

SConscript('tvm/posix/SConscript')

# A bit tedious, and not needed right now.
# SConscript('tools/occamdoc/SConscript')

