# Remote control interface for NXT
# Requires libbluetooth, bluez-utils, python-bluetooth
#

import bluetooth
import socket

#Replace name and address of target NXT
target_name = "ESL"
target_address = "00:16:53:06:3C:3D"

port = 1
sock=bluetooth.BluetoothSocket(bluetooth.RFCOMM)

print "Connecting"
sock.connect((target_address, port))
print "Connected.." 
sock.send(str(8))
while True:
    foo = raw_input("Send next text:")
    if foo == "q":
        break;
    sock.send(foo)
sock.close();
