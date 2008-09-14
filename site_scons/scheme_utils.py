import re, os, string
from SCons.Builder import Builder
from SCons.Scanner import Scanner

def schemefile_scan(node, env, path):
  results = []
  stringresults = ''
  filtered = []

  # print "THE NODE ", str(node)
  reg = re.compile('.*schemescanner\\.scm$')
  
  #print "REGEXP ", str(re.match(reg, str(node)))
  #if reg.match(str(node)):
  #  print "NOT SCANNING SCHEMESCANNER"

  if not reg.match(str(node)):
    cmd = '%s %s' % (env['schemescanner'] + env['EXESUFFIX'], str(node))
    # print "CMD ", cmd
    stringresults = nbcmd.exe(cmd)
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
              

