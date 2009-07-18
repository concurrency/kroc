#!/usr/bin/env python
import sys
import os
import os.path
import subprocess
import re

occ21      = sys.argv[1]
occ21_args = ('-etc -w -y -znd -znec -udo -zncc -init -xin -mobiles -zrpe -zcxdiv ' +
              '-zcxrem -zep -t8 -zqa -DEF OCCBUILD.TVM -b').split(' ')

test_header_re = re.compile('--\s*TEST:\s*(.*)')
expected_re    = re.compile('(ERROR|WARNING):\s*(.*?),\s*[\'"](.*?)[\'"]')

verbose = False

if len(sys.argv) > 3 and sys.argv[3] == '-v': verbose = True

def compile(filename):
    # setup defaults
    mode     = 'pass'
    expected = []
    # check if this test has special args
    testfile = open(filename, 'r').read()
    m = test_header_re.search(testfile)
    if m:
        if m.group(1).lower() not in ['pass', 'fail']:
            print '%s:' % filename
            print 'Invalid test mode: %s' % m.group(1)
            sys.exit(1)
        else:
            mode = m.group(1).lower()
    ms = expected_re.findall(testfile)
    for m in ms:
        (type, line, msg) = m
        if line != '?':
            line = int(line)
        if type.lower() not in ['warning', 'error']:
            print '%s:' % filename
            print 'Invalid message type: ' % type
            sys.exit(1)
        regexp = type + '-occ21-' + re.escape(filename) + r'\(([0-9]+)\)-' + re.escape(' ' + msg).replace(r'\ ', r'\s*')
        expected += [(type, line, msg, re.compile(regexp, re.IGNORECASE))]
    # run the test
    cmd = [occ21] + occ21_args + [filename]
    p = subprocess.Popen(
            cmd,
            bufsize=-1,
            stdin=None, 
            stdout=subprocess.PIPE, 
            stderr=subprocess.PIPE, 
            )
    (child_stdout, child_stderr) = (p.stdout, p.stderr)
    out = child_stdout.read()
    err = child_stderr.read()
    ret = p.wait()
    # check how we did
    if (ret == 0 and mode == 'pass') or (ret != 0 and mode == 'fail'):
        test_result = 0
        if expected:
            for e in expected:
                type, line, msg, regexp = e
                ms = regexp.findall(err)
                if not ms:
                    print '%s:' % filename
                    print 'WARNING: expected %s on line %d not found: %s' % (type.lower(), line, msg)
                    alternative_re = re.compile(type + '-occ21-' + re.escape(filename) + '\(%d\)-(.*)' % line, re.IGNORECASE)
                    m = alternative_re.search(err)
                    if m:
                        print '         found the follwing on line %d: %s' % (line, m.group(1).strip())
                    else:
                        print '         no %s found on line %d' % (type.lower(), line)

        if not verbose:
            try:
                filename_tce = os.path.splitext(filename)[0] + '.tce'
                os.remove(filename_tce)
            except OSError, e:
                pass
    else:
        test_result = 1
    # print useful info
    if test_result == 1 or verbose:
        print '%s:' % filename
        print ' '.join(cmd)
        print out.strip()
        print err.strip()
    return test_result

sys.exit(compile(sys.argv[2]))
