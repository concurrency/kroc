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
comstrings = dict(CCCOMSTR     = 'Compiling $TARGET',
                  LINKCOMSTR   = 'Linking $TARGET',
		  ARCOMSTR     = 'Creating archive $TARGET',
		  RANLIBCOMSTR = 'Indexing archive $TARGET',
		  # FIXME: Why do PLT tools always have to be different?
		  MZCCOMSTR    = 'Compiling $TARGET')
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

SConscript('modules/inmoslibs/libsrc/SConscript')

SConscript('runtime/libtvm/SConscript')

# A bit tedious, and not needed right now.
# SConscript('tools/occamdoc/SConscript')

