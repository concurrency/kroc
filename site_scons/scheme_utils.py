import os, string
from SCons.Builder import Builder

# This ia custom builder we can hang off of 
# an Environment(). Usage might look like:
#
# local = env.Clone(BUILDERS = {"MZC" : scheme_utils.mzc})
# 
# and then
#
# local.MZC("schemescanner", "schemescanner.scm") 
#
mzc = Builder(action = ['mzc --exe \"$TARGET\" \"$SOURCE\"'],
              src_suffix = ".scm",
              single_source = True)

