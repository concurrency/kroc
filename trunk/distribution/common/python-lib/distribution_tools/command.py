import subprocess
import sys

def execute(command, **kwargs):
    print 'running command: %s' % (' '.join(command), )
    ret = subprocess.call(command, **kwargs)
    if ret != 0:
        print 'command failed with error code %d' % (ret, )
        sys.exit(ret)

def capture(command, **kwargs):
    print 'capturing command: %s' % (' '.join(command), )
    return subprocess.Popen(command, stdout=subprocess.PIPE, **kwargs).communicate()[0]
