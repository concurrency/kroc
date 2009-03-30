#IF NOT DEFINED (SRV1.INC)
#DEFINE SRV1.INC

--{{{  Constants
--{{{  Errors
VAL INT ERROR.INV.PARAM   IS -1: -- invalid parameter (e.g. camera mode)
VAL INT ERROR.INV.STATE   IS -2: -- invalid state (e.g. camera stopped)
VAL INT ERROR.HARDWARE    IS -3: -- hardware error (e.g. camera got upset)
VAL INT ERROR.NO.MEMORY   IS -4: -- not enough space/memory
VAL INT ERROR.TIMEOUT     IS -5: -- hardware timeout (e.g. bus timeout)
VAL INT ERROR.FIFO        IS -6: -- FIFO overrun/underrun
VAL INT ERROR.INTERRUPT   IS -7: -- interrupt (e.g. lost bus arbitration)
--}}}
--{{{  Timing
VAL INT MILLIS            IS 1000:
VAL INT SECONDS           IS (MILLIS * 1000):
--}}}
--}}}

--{{{  Data Types
--{{{  SRV.HEADER
DATA TYPE SRV.HEADER
  PACKED RECORD
    INT tbc.version:
    INT ws.size:
    INT vs.size:
    INT ms.size:
    INT inst.size:
:
--}}}
--{{{  UYVY
DATA TYPE UYVY
  PACKED RECORD
    BYTE u:
    BYTE y.0:
    BYTE v:
    BYTE y.1:
:
--}}}
--}}}

--{{{  Channel Types
--{{{  CAMERA
--{{{  P.CAMERA.REQ
PROTOCOL P.CAMERA.REQ
  CASE
    disconnected        = 0
    -- get.frame        => frame | error
    get.frame           = 1
    -- get.mode         => capture.mode | stream.mode | error
    get.mode            = 2
    -- init             => (no response)
    init                = 3
    -- set.capture.mode => ok | error
    set.capture.mode    = 4; INT; INT -- width, height
    -- set.stream.mode  => ok | error
    set.stream.mode     = 5; INT; INT -- width, height
    -- stop             => (no response)
    stop                = 6
    -- set.auto.adjust	=> ok | error
    set.auto.adjust	= 7; BOOL
    -- get.auto.adjust	=> auto.adjust
    get.auto.adjust	= 8
:
--}}}
--{{{  P.CAMERA.RSP
PROTOCOL P.CAMERA.RSP
  CASE
    disconnected  = 0
    error         = 1; INT      -- error number
    frame         = 2; MOBILE []BYTE
    capture.mode  = 3; INT; INT -- width, height
    stream.mode   = 4; INT; INT -- width, height
    ok            = 5
    auto.adjust   = 6; BOOL
:
--}}}
CHAN TYPE CAMERA
  MOBILE RECORD
    CHAN P.CAMERA.REQ req?:
    CHAN P.CAMERA.RSP rsp!:
:
--}}}

--{{{  CONSOLE
--{{{  P.CONSOLE.REQ
PROTOCOL P.CONSOLE.REQ
  CASE
    disconnected  = 0
    -- bulk.out; TRUE; ...  => bulk.complete | error
    -- bulk.out; FALSE; ... => (no response)
    bulk.out      = 1; BOOL; INT; MOBILE []BYTE -- synchronous, bytes, data
:
--}}}
--{{{  P.CONSOLE.RSP
PROTOCOL P.CONSOLE.RSP
  CASE
    disconnected  = 0
    bulk.complete = 1; INT; MOBILE []BYTE -- bytes sent, data
    error         = 2; INT
:
--}}}
CHAN TYPE CONSOLE
  MOBILE RECORD
    CHAN BYTE           in!:
    CHAN BYTE           out?:
    CHAN P.CONSOLE.REQ  cmd.req?:
    CHAN P.CONSOLE.RSP  cmd.rsp!:
:
--}}}

--{{{  LASER
--{{{  P.LASER.REQ
PROTOCOL P.LASER.REQ
  CASE
    disconnected  = 0
    left          = 1; BOOL
    right         = 2; BOOL
    all           = 3; BOOL
:
--}}}
--{{{  P.LASER.RSP
PROTOCOL P.LASER.RSP
  CASE
    disconnected  = 0
:
--}}}
CHAN TYPE LASER
  MOBILE RECORD
    CHAN P.LASER.REQ req?:
    CHAN P.LASER.RSP rsp!:
:
--}}}

--{{{  MOTOR
--{{{  P.MOTOR.REQ
PROTOCOL P.MOTOR.REQ
  CASE
    disconnected  = 0
    left          = 1; INT
    right         = 2; INT
    all           = 3; INT; INT
    delta         = 4; INT; INT
    turn          = 5; INT
    back.turn     = 6; INT
    stop          = 7
:
--}}}
--{{{  P.MOTOR.RSP
PROTOCOL P.MOTOR.RSP
  CASE
    disconnected  = 0
:
--}}}
CHAN TYPE MOTOR
  MOBILE RECORD
    CHAN P.MOTOR.REQ req?:
    CHAN P.MOTOR.RSP rsp!:
:
--}}}

--{{{  TWI
--{{{  P.TWI.REQ
PROTOCOL P.TWI.REQ
  CASE
    disconnected  = 0
    -- get.clock.div  => clock.div
    get.clock.div = 1
    -- get.sccb       => sccb
    get.sccb      = 2
    -- set.clock.div  => ok | error 
    set.clock.div = 3; BYTE; BYTE -- clock hi, clock lo
    -- set.sccb       => (no response)
    set.sccb      = 4; BOOL
    -- recv           => ret 
    recv          = 5; BYTE; MOBILE []BYTE -- address, buffer
    -- recv.send      => ret.2
    recv.send     = 6; BYTE; MOBILE []BYTE; MOBILE []BYTE -- address, recv buffer, send buffer
    -- send           => ret
    send          = 7; BYTE; MOBILE []BYTE -- address, buffer
    -- send.recv      => ret.2
    send.recv     = 8; BYTE; MOBILE []BYTE; MOBILE []BYTE -- address, send buffer, recv buffer
:
--}}}
--{{{  P.TWI.RSP
PROTOCOL P.TWI.RSP
  CASE
    disconnected  = 0
    clock.div     = 1; BYTE; BYTE -- clock hi, clock lo
    error         = 2; INT
    ok            = 3
    ret           = 4; INT; MOBILE []BYTE -- result, buffer
    ret.2         = 5; INT; MOBILE []BYTE; MOBILE []BYTE -- result, buffer 1, buffer 2
    sccb          = 6; BOOL
:
--}}}
CHAN TYPE TWI
  MOBILE RECORD
    CHAN P.TWI.REQ req?:
    CHAN P.TWI.RSP rsp!:
:
--}}}

--{{{  SYSTEM
--{{{  P.SYSTEM.REQ
PROTOCOL P.SYSTEM.REQ
  CASE
    disconnected  = 0
    -- get.version  => version
    get.version   = 1
    -- open.twi     => twi
    open.twi      = 2
    -- set.led      => (no response)
    set.led       = 3; INT; BOOL
:
--}}}
--{{{  P.SYSTEM.RSP
PROTOCOL P.SYSTEM.RSP
  CASE
    disconnected  = 0
    twi           = 1; SHARED TWI!
    version       = 2; INT; MOBILE []BYTE -- numeric version, version string
:
--}}}
CHAN TYPE SYSTEM
  MOBILE RECORD
    CHAN P.SYSTEM.REQ req?:
    CHAN P.SYSTEM.RSP rsp!:
:
--}}}
--}}}

--{{{  FFI Interface
--{{{  PRAGMAs
#PRAGMA EXTERNAL "PROC C.tvmspecial.0.run.user (VAL []BYTE bytecode, VAL INT ws, vs, ms, VAL []BYTE tlp, FIXED CAMERA! camera, FIXED CONSOLE! console, FIXED LASER! laser, FIXED MOTOR! motor, FIXED SYSTEM! system) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.1.kill.user () = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.2.query.user (BOOL running, INT state, []BYTE context) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.3.reset.dynamic.memory () = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.4.set.register.16 (INT16 addr, VAL INT set, clear) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.5.jpeg.encode.frame (VAL INT width, height, quality, VAL []BYTE input, []BYTE output, INT used) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.6.draw.caption.on.frame (VAL INT frame.width, VAL []BYTE caption, []BYTE frame) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.7.test.disconnected (CHAN OF ANY c, BOOL b) = 0"
--}}}
--{{{  Wrappers
INLINE PROC firmware.run.user (VAL []BYTE bytecode, VAL INT ws, vs, ms, CAMERA! camera, CONSOLE! console, LASER! laser, MOTOR! motor, SYSTEM! system)
  C.tvmspecial.0.run.user (bytecode, ws, vs, ms, "CCCCC", camera, console, laser, motor, system)
:

INLINE PROC firmware.kill.user ()
  C.tvmspecial.1.kill.user ()
:

INLINE PROC firmware.query.user (BOOL running, INT state, []BYTE context)
  C.tvmspecial.2.query.user (running, state, context)
:

INLINE PROC reset.dynamic.memory ()
  C.tvmspecial.3.reset.dynamic.memory ()
:

INLINE PROC safe.set.register.16 (INT16 reg, VAL INT set, clear)
  C.tvmspecial.4.set.register.16 (reg, set, clear)
:

INLINE PROC jpeg.encode.frame (VAL INT width, height, quality, VAL []BYTE input, []BYTE output, INT used)
  C.tvmspecial.5.jpeg.encode.frame (width, height, quality, input, output, used)
:

INLINE PROC draw.caption.on.frame (VAL INT frame.width, VAL []BYTE caption, []BYTE frame)
  C.tvmspecial.6.draw.caption.on.frame (frame.width, caption, frame)
:

INLINE PROC test.disconnected (CHAN OF ANY c, BOOL b)
  C.tvmspecial.7.test.disconnected (c, b)
:
--}}}
--}}}

#ENDIF -- !SRV1.INC
