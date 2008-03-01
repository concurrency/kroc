#IF NOT DEFINED (SRV1.INC)
#DEFINE SRV1.INC

--{{{  Constants
--{{{  Errors
VAL INT ERROR.INV.PARAM   IS -1: -- invalid parameter (e.g. camera mode)
VAL INT ERROR.INV.STATE   IS -2: -- invalid state (e.g. camera stopped)
VAL INT ERROR.HARDWARE    IS -3: -- hardware error (e.g. camera got upset)
VAL INT ERROR.NO.MEMORY   IS -4: -- not enough space/memory
VAL INT ERROR.UNKNOWN     IS 0:
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

--{{{  SYSTEM
--{{{  P.SYSTEM.REQ
PROTOCOL P.SYSTEM.REQ
  CASE
    disconnected  = 0
    -- get.version  => version
    get.version   = 1
    -- set.led      => (no response)
    set.led       = 2; INT; BOOL
:
--}}}
--{{{  P.SYSTEM.RSP
PROTOCOL P.SYSTEM.RSP
  CASE
    disconnected  = 0
    version       = 1; INT; MOBILE []BYTE -- numeric version, version string
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
