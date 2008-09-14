import os, string
from SCons.Builder import Builder

#
# mzc_build
# 
# Usage is along the lines of:
# local.Command("schemescanner", "schemescanner.scm", mzc_build)
#
# This is probably only used in the context of a Command() 
# build action.
#
# This builder just does a syscall to invoke the 
# MzC compiler on a single Scheme file.a
def mzc_build (target, source, env):
  cmd = string.Template("mzc --exe $TGT $SRC")
  cmdstring = cmd.substitute(
        TGT = target[0].rstr(),
        SRC = source[0].rstr())
  os.system(cmdstring)

mzc = Builder(action = ['mzc --exe \"$TARGET\" \"$SOURCE\"'],
              src_suffix = ".scm")

