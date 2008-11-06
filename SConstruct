import os
import scheme_utils

# Create a global environment
env  = Environment(ENV = {'PATH' : os.environ['PATH']})

# Pretty builds
comstrings = dict(CCCOMSTR     = 'Compiling $TARGET',
                  LINKCOMSTR   = 'Linking $TARGET',
		  ARCOMSTR     = 'Creating archive $TARGET',
		  RANLIBCOMSTR = 'Indexing archive $TARGET')
# FIXME: Not sure how to get nice comstr's for
#        * env.Command commands
#        * custom builders like the mzc builder
#for k in comstrings:
#    env[k] = comstrings[k]


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

SConscript('runtime/libtvm/SConscript')

# A bit tedious, and not needed right now.
# SConscript('tools/occamdoc/SConscript')

