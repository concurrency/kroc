import subprocess
import sys

def execute(command):
    print 'running command: %s' % (' '.join(command), )
    ret = subprocess.call(command)
    if ret != 0:
        print 'command failed with error code %d' % (ret, )
        sys.exit(ret)

def capture(command):
    print 'capturing command: %s' % (' '.join(command), )
    return subprocess.Popen(command, stdout=subprocess.PIPE).communicate()[0]
