import os, re, string

#
# CheckForExecutable : context exe -> {0,1}
#
# Usage is along the lines of:
# conf  = local.Configure(custom_tests = {"CheckForExecutable" : CheckForExecutable })
# Probably only used in the context of a Configure().
#
def CheckForExecutable(context, env, exe):
  # Using 0 and 1 for the result let SCons correctly
  # print "yes" and "no" in the output to the user.
  result = 0
  context.Message("Checking for %s... " % exe)
  # print context.env["ENV"] 
  for dir in env["ENV"]["PATH"].split(":"):
    if os.access(os.path.join(dir, exe), os.X_OK):
      result = 1
      break
  
  # We need to stuff this back into the context object.
  # So sayth the documentation.
  context.Result(result)
  return result




