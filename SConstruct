# Create a global environment
env  = Environment()

# Export it for use in the SConscripts
Export('env')

# Build mkoccdeps
SConscript('tools/mkoccdeps/SConscript')
SConscript('tools/ilibr/SConscript')
# A bit tedious, and not needed right now.
# SConscript('tools/occamdoc/SConscript')

