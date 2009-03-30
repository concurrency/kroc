import re, os
from SCons.Builder import Builder
from SCons.Scanner import Scanner
from SCons.Action import Action

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


mzc = Builder(action = Action('$MZCCOM', '$MZCCOMSTR'),
              src_suffix = ".scm",
              single_source = True,
              source_scanner = SchemeScanner)

def generate(env, *kw):
    env['BUILDERS']['Mzc'] = mzc
    env['MZC']             = 'mzc'
    env['MZCCOM']          = '$MZC --exe $TARGET $SOURCE'


def exists(env):
    return 1
