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
--{{{  P.CAMERA.RQ
PROTOCOL P.CAMERA.RQ
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
:
--}}}
--{{{  P.CAMERA.RE
PROTOCOL P.CAMERA.RE
  CASE
    disconnected  = 0
    error         = 1; INT      -- error number
    frame         = 2; MOBILE []BYTE
    capture.mode  = 3; INT; INT -- width, height
    stream.mode   = 4; INT; INT -- width, height
    ok            = 5
:
--}}}
CHAN TYPE CAMERA
  MOBILE RECORD
    CHAN P.CAMERA.RQ rq?:
    CHAN P.CAMERA.RE re!:
:
--}}}

--{{{  CONSOLE
CHAN TYPE CONSOLE
  MOBILE RECORD
    CHAN BYTE in!:
    CHAN BYTE out?:
:
--}}}

--{{{  LASER
--{{{  P.LASER.RQ
PROTOCOL P.LASER.RQ
  CASE
    disconnected  = 0
    left          = 1; BOOL
    right         = 2; BOOL
    all           = 3; BOOL
:
--}}}
--{{{  P.LASER.RE
PROTOCOL P.LASER.RE
  CASE
    disconnected  = 0
:
--}}}
CHAN TYPE LASER
  MOBILE RECORD
    CHAN P.LASER.RQ rq?:
    CHAN P.LASER.RE re!:
:
--}}}

--{{{  MOTOR
--{{{  P.MOTOR.RQ
PROTOCOL P.MOTOR.RQ
  CASE
    disconnected  = 0
    speed         = 1; INT
    speed.delta   = 2; INT
    turn          = 3; INT
    back.turn     = 4; INT
    stop          = 5
:
--}}}
--{{{  P.MOTOR.RE
PROTOCOL P.MOTOR.RE
  CASE
    disconnected  = 0
:
--}}}
CHAN TYPE MOTOR
  MOBILE RECORD
    CHAN P.MOTOR.RQ rq?:
    CHAN P.MOTOR.RE re!:
:
--}}}

--{{{  SYSTEM
--{{{  P.SYSTEM.RQ
PROTOCOL P.SYSTEM.RQ
  CASE
    disconnected  = 0
    -- get.version  => version
    get.version   = 1
    -- set.led      => (no response)
    set.led       = 2; INT; BOOL
:
--}}}
--{{{  P.SYSTEM.RE
PROTOCOL P.SYSTEM.RE
  CASE
    disconnected  = 0
    version       = 1; INT; MOBILE []BYTE -- numeric version, version string
:
--}}}
CHAN TYPE SYSTEM
  MOBILE RECORD
    CHAN P.SYSTEM.RQ rq?:
    CHAN P.SYSTEM.RE re!:
:
--}}}
--}}}

--{{{  FFI Interface
--{{{  PRAGMAs
#PRAGMA EXTERNAL "PROC C.tvmspecial.0.run.user (VAL []BYTE bytecode, VAL INT ws, vs, ms, VAL []BYTE tlp, FIXED CAMERA! camera, FIXED CONSOLE! console, FIXED LASER! laser, FIXED MOTOR! motor, FIXED SYSTEM! system) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.1.kill.user () = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.2.query.user (BOOL running, INT state, []BYTE context) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.3.reset.dynamic.memory () = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.4.set.register.16 (VAL INT addr, set, mask) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.5.jpeg.encode.frame (VAL INT width, height, quality, VAL []BYTE input, []BYTE output, INT used) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.6.draw.caption.on.frame (VAL INT frame.width, VAL []BYTE caption, []BYTE frame) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.7.blit.to.uart0 (VAL []BYTE data) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.8.test.disconnected (CHAN OF ANY c, BOOL b) = 0"
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

INLINE PROC safe.set.register.16 (VAL INT addr, set, mask)
  C.tvmspecial.4.set.register.16 (addr, set, mask)
:

INLINE PROC jpeg.encode.frame (VAL INT width, height, quality, VAL []BYTE input, []BYTE output, INT used)
  C.tvmspecial.5.jpeg.encode.frame (width, height, quality, input, output, used)
:

INLINE PROC draw.caption.on.frame (VAL INT frame.width, VAL []BYTE caption, []BYTE frame)
  C.tvmspecial.6.draw.caption.on.frame (frame.width, caption, frame)
:

INLINE PROC blit.to.uart0 (VAL []BYTE data)
  C.tvmspecial.7.blit.to.uart0 (data)
:

INLINE PROC test.disconnected (CHAN OF ANY c, BOOL b)
  C.tvmspecial.8.test.disconnected (c, b)
:
--}}}
--}}}

#ENDIF -- !SRV1.INC
