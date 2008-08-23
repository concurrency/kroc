#!/usr/bin/env python
import getopt, sys, time

getopt_opts = ["help", "reset" ,"bootldr"]

try:
    import serial
    hasPySerial = True
    getopt_opts.append('term')
except ImportError:
    import termios, os, fcntl, array, time, sys, getopt
    print 'pyserial not available, --term not available'
    hasPySerial = False



def usage():
    print "Usage: "
    if hasPySerial:
        print "%s --<reset|bootldr> [--term] <serialport>" % sys.argv[0]
    else:
        print "%s --<reset|bootldr> <serialport>" % sys.argv[0]

def main():
    # Deal with the command line
    try:
        opts, args = getopt.getopt(sys.argv[1:], "", getopt_opts)
    except getopt.GetoptError, err:
        print str(err)
        usage()
        sys.exit(2)
    cmd  = None
    port = None
    doTerm = False
    for o, a in opts:
        if o == '--reset':
            cmd = 'reset'
        elif o == '--bootldr':
            cmd = 'bootldr'
        elif o == '--help':
            usage()
            sys.exit(0)
        elif o == '--term':
            doTerm = True
    if cmd == None:
        print 'You must specify a command'
        usage()
        sys.exit(2)
    if len(args) > 1:
        print 'Unexpected number of arguments'
        usage()
        sys.exit(2)
    if len(args) != 1:
        print 'You must specify a serial port'
        usage()
        sys.exit(2)
    port = args[0]
    # This is where stuff happens
    if hasPySerial:
        # Using PySerial
        s = serial.Serial(port, baudrate=38400, timeout=1)
        if cmd == 'reset':
            print '%s: resetting' % port
            s.setDTR(0)
        else:
            print '%s: bootloader' % port
            pass
        if doTerm:
            print 'serial output:'
            while True:
                sys.stdout.write(s.read(1))
        else:
            time.sleep(.5)
    else:
        # Using IOCTL
        try:
            fd = os.open(port, os.O_NONBLOCK | os.O_RDWR)
        except OSError, err:
            print 'Could not open serial port'
            print str(err)
            sys.exit(1)
        try:
            buf = array.array('h', [0])
            if cmd == 'reset':
                print '%s: resetting' % port
                buf[0] = termios.TIOCM_DTR
                fcntl.ioctl(fd, termios.TIOCMBIC, buf, 0)
            else:
                print '%s: bootloader' % port
                #buf[0] = termios.TIOCM_RTS
                #fcntl.ioctl(fd, termios.TIOCMBIC, buf, 0)
            time.sleep(.5)
        finally:
            os.close(fd)

if __name__ == '__main__':
    main()

# On second thoughts, this might have been marginally easier with pyserial:
# (one less dependency w/o pyserial though)
#import serial
#s = serial.Serial('/dev/tty.usbserial')
#s.setDTR(0) 
#  or
#s.setRTS(0)  

