import os
import scheme_utils

# Create a global environment
env  = Environment(ENV = {'PATH' : os.environ['PATH']})

# Export it for use in the SConscripts
Export('env')

# Build mkoccdeps
SConscript('tools/mkoccdeps/SConscript')
SConscript('tools/ilibr/SConscript')
SConscript('tools/schemescanner/SConscript')

# A bit tedious, and not needed right now.
# SConscript('tools/occamdoc/SConscript')

