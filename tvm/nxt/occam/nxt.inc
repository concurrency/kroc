
#PRAGMA EXTERNAL "PROC C.tvmspecial.0.nx.display.clear () = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.1.nx.display.set.pos (VAL INT x, y) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.2.nx.display.string (VAL []BYTE str) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.3.nx.motors.rotate (VAL INT motor, speed) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.4.nx.motors.rotate.angle (VAL INT motor, speed, angle, VAL BOOL brake) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.5.nx.motors.rotate.time (VAL INT motor, speed, ms, VAL BOOL brake) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.6.nx.motors.stop (VAL INT motor, VAL BOOL brake) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.7.nx.motors.get.tach.count (VAL INT motor, RESULT INT count) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.8.nx.i2c.memory.init (VAL INT sensor, VAL BYTE address, VAL BOOL lego.compat) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.9.nx.i2c.memory.close (VAL INT sensor) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.10.nx.i2c.memory.read (VAL INT sensor, VAL BYTE address, []BYTE buffer, RESULT INT result) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.11.nx.i2c.memory.write (VAL INT sensor, VAL BYTE address, VAL []BYTE buffer, RESULT INT result) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.12.nx.sensors.analog.init (VAL INT sensor) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.13.nx.sensors.analog.get (VAL INT sensor, RESULT INT result) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.14.nx.display.int (VAL INT value) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.15.nx.sound (VAL INT freq, VAL INT len) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.16.nx.button (RESULT INT but) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.17.nx.bt.init () = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.18.nx.bt.set.discoverable (VAL BOOL set) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.19.nx.bt.open.port (RESULT INT handle) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.20.nx.bt.close.port (VAL INT handle, RESULT INT return) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.21.nx.bt.connection.pending (RESULT INT return) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.22.nx.bt.accept.connection (VAL BOOL accept) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.23.nx.bt.connection.established (RESULT INT value) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.24.nx.bt.stream.open (VAL INT handle) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.25.nx.bt.stream.write (VAL []BYTE data) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.26.nx.bt.stream.data.written (RESULT BOOL written) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.27.nx.bt.stream.opened (RESULT BOOL open) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.28.nx.bt.stream.read ([]BYTE str) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.29.nx.bt.stream.data.read (RESULT INT len) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.30.nx.bt.stream.close () = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.31.nx.bt.set.friendly.name (VAL []BYTE name) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.32.nx.bt.add.known.device (VAL [6]BYTE addr, VAL []BYTE name, VAL [4]BYTE class, RESULT INT result) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.33.nx.bt.remove.known.device (VAL [6]BYTE addr, RESULT INT result) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.34.nx.bt.has.dev.waiting.for.pin (RESULT BOOL result) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.35.nx.bt.send.pin (VAL []BYTE pin) = 0"


INLINE PROC nx.display.clear ()
  C.tvmspecial.0.nx.display.clear ()
:

INLINE PROC nx.display.set.pos (VAL INT x, y)
  C.tvmspecial.1.nx.display.set.pos (x, y)
:

INLINE PROC nx.display.string (VAL []BYTE str)
  C.tvmspecial.2.nx.display.string (str)
:

INLINE PROC nx.motors.rotate (VAL INT motor, speed)
  C.tvmspecial.3.nx.motors.rotate (motor, speed)
:

INLINE PROC nx.motors.rotate.angle (VAL INT motor, speed, angle, VAL BOOL brake)
  C.tvmspecial.4.nx.motors.rotate.angle (motor, speed, angle, brake)
:

INLINE PROC nx.motors.rotate.time (VAL INT motor, speed, ms, VAL BOOL brake)
  C.tvmspecial.5.nx.motors.rotate.time (motor, speed, ms, brake)
:

INLINE PROC nx.motors.stop (VAL INT motor, VAL BOOL brake)
  C.tvmspecial.6.nx.motors.stop (motor, brake)
:

INLINE PROC nx.motors.get.tach.count (VAL INT motor, RESULT INT count)
  C.tvmspecial.7.nx.motors.get.tach.count (motor, count)
:

INLINE PROC nx.i2c.memory.init (VAL INT sensor, VAL BYTE address, VAL BOOL lego.compat)
  C.tvmspecial.8.nx.i2c.memory.init (sensor, address, lego.compat)
:

INLINE PROC nx.i2c.memory.close (VAL INT sensor)
  C.tvmspecial.9.nx.i2c.memory.close (sensor)
:

INLINE PROC nx.i2c.memory.read (VAL INT sensor, VAL BYTE address, []BYTE buffer, RESULT INT result)
  C.tvmspecial.10.nx.i2c.memory.read (sensor, address, buffer, result)
:

INLINE PROC nx.i2c.memory.write (VAL INT sensor, VAL BYTE address, VAL []BYTE buffer, RESULT INT result)
  C.tvmspecial.11.nx.i2c.memory.write (sensor, address, buffer, result)
:

INLINE PROC nx.sensors.analog.init (VAL INT sensor)
  C.tvmspecial.12.nx.sensors.analog.init (sensor)
:

INLINE PROC nx.sensors.analog.get (VAL INT sensor, RESULT INT result)
  C.tvmspecial.13.nx.sensors.analog.get (sensor, result)
:

INLINE PROC nx.display.int (VAL INT value)
  C.tvmspecial.14.nx.display.int (value)
:

INLINE PROC nx.sound (VAL INT freq, len)
  C.tvmspecial.15.nx.sound (freq, len)
:

INLINE PROC nx.button (RESULT INT but)
  C.tvmspecial.16.nx.button(but)
:

INLINE PROC nx.bt.init ()
  C.tvmspecial.17.nx.bt.init ()
:

INLINE PROC nx.bt.set.discoverable (VAL BOOL set)
  C.tvmspecial.18.nx.bt.set.discoverable (set)
:

INLINE PROC nx.bt.open.port (RESULT INT handle)
  C.tvmspecial.19.nx.bt.open.port (handle)
:

INLINE PROC nx.bt.close.port (VAL INT handle, RESULT INT return)
  C.tvmspecial.20.nx.bt.close.port (handle, return)
:

INLINE PROC nx.bt.connection.pending (RESULT INT return)
  C.tvmspecial.21.nx.bt.connection.pending (return)
:

INLINE PROC nx.bt.accept.connection (VAL BOOL accept)
  C.tvmspecial.22.nx.bt.accept.connection (accept)
:

INLINE PROC nx.bt.connection.established (RESULT INT value)
  C.tvmspecial.23.nx.bt.connection.established (value)
:

INLINE PROC nx.bt.stream.open (VAL INT handle)
  C.tvmspecial.24.nx.bt.stream.open (handle)
:

INLINE PROC nx.bt.stream.write (VAL []BYTE data)
  C.tvmspecial.25.nx.bt.stream.write (data)
:

INLINE PROC nx.bt.stream.data.written (RESULT BOOL written)
  C.tvmspecial.26.nx.bt.stream.data.written (written)
:

INLINE PROC nx.bt.stream.opened (RESULT BOOL open)
  C.tvmspecial.27.nx.bt.stream.opened (open)
:

INLINE PROC nx.bt.stream.read ([]BYTE data)
  C.tvmspecial.28.nx.bt.stream.read (data)
:

INLINE PROC nx.bt.stream.data.read (RESULT INT len)
  C.tvmspecial.29.nx.bt.stream.data.read (len)
:

INLINE PROC nx.bt.stream.close ()
  C.tvmspecial.30.nx.bt.stream.close ()
:

INLINE PROC nx.bt.set.friendly.name (VAL []BYTE name)
  C.tvmspecial.31.nx.bt.set.friendly.name (name)
:

INLINE PROC nx.bt.add.known.device (VAL [6]BYTE addr, VAL []BYTE name, VAL [4]BYTE class, RESULT INT result)
  C.tvmspecial.32.nx.bt.add.known.device (addr, name, class, result)
:

INLINE PROC nx.bt.remove.known.device (VAL [6]BYTE addr, RESULT INT result)
  C.tvmspecial.33.nx.bt.remove.known.device (addr, result)
:

INLINE PROC nx.bt.has.dev.waiting.for.pin (RESULT BOOL result) 
  C.tvmspecial.34.nx.bt.has.dev.waiting.for.pin (result)
:

INLINE PROC nx.bt.send.pin (VAL []BYTE pin)
  C.tvmspecial.35.nx.bt.send.pin (pin)
:


VAL INT I2C.ERR.OK              IS 0:
VAL INT I2C.ERR.UNKNOWN.SENSOR  IS 1:
VAL INT I2C.ERR.NOT.READY       IS 2:
VAL INT I2C.ERR.TXN.FULL        IS 3:
VAL INT I2C.ERR.DATA            IS 4:

PROC delay.ms (VAL INT ms)
  TIMER time:
  INT now:
  SEQ
    time ? now
    time ? AFTER (now PLUS ms)
:

PROC display.hex (VAL INT x)
  VAL []BYTE lookup IS [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' ]:
  SEQ i = 0 FOR 4
    nx.display.string ([lookup FROM ((x >> (12 - (i * 4))) /\ #F) FOR 1 ])
:
