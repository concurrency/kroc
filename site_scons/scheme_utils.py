import re, os, string, popen2, select
import build_utils
from SCons.Builder import Builder
from SCons.Scanner import Scanner
from SCons.Script import Exit

def exe (command):
  child = os.popen(command)
  data = child.read()
  err = child.close()
  return data

def schemefile_scan(node, env, path):
  if not env.GetOption("clean"):
    results = []
    stringresults = ''
    filtered = []

    # print "THE NODE ", str(node)
    reg = re.compile('.*schemescanner\\.scm$')
    
    #print "REGEXP ", str(re.match(reg, str(node)))
    #if reg.match(str(node)):
    #  print "NOT SCANNING SCHEMESCANNER"

    if not reg.match(str(node)):
      # WARNING
      # 'EXESUFFIX', as it stands, needs to be set so that we get the 
      # right, platform-specific extension for "schemescanner". eg. ".exe"
      # on Windows.
      # cmd = '%s %s' % (env['schemescanner'] + env.get('EXESUFFIX', ""), str(node))
      cmd = '%s %s' % (env['schemescanner'], str(node))

      stringresults = exe(cmd)
      # print "RESULTS ", results
    results_split = re.split(' ', stringresults)
    for i in results_split:
      if i != '':
        filtered.append(i)
    # print "RESULTS SPLIT ", filtered
    return filtered

SchemeScanner = Scanner(function = schemefile_scan, skeys = ['.scm', '.ss'])


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
              single_source = True,
              source_scanner = SchemeScanner)

def BuildSchemeApp(env, app, ext = "scm"):
  local = env.Clone(BUILDERS = {"MZC" : mzc})

  # Adding our custom checker for the an executable.
  conf  = local.Configure(custom_tests = {"CheckForExecutable" : build_utils.CheckForExecutable })

  # If the compiler is found, build the Scheme executable.
  # Otherwise, bail with an error for the user.
  if not conf.CheckForExecutable(local, "mzc"):
    print "mzc cannot be found!" 
    # Cannot exit here or we never get to display the help with --help
    #Exit(1)

  local = conf.Finish()

  return local.MZC(app, "%s.%s" % (app, ext))

