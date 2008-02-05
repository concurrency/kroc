#IF NOT DEFINED (SRV1.INC)
#DEFINE SRV1.INC

--{{{  Constants
--{{{  Camera
VAL INT CAMERA.OFF        IS 0:
VAL INT CAMERA.160.128    IS 1:
VAL INT CAMERA.320.256    IS 2:
VAL INT CAMERA.640.512    IS 3:
VAL INT CAMERA.1280.1024  IS 4:
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

--{{{  Protocols
--{{{  P.MOTOR
PROTOCOL P.MOTOR
  CASE
    disconnected  = 0
    speed         = 1; INT
    speed.delta   = 2; INT
    turn          = 3; INT
    back.turn     = 4; INT
    stop          = 5
:
--}}}

--{{{  P.LASER
PROTOCOL P.LASER
  CASE
    disconnected  = 0
    left          = 1; BOOL
    right         = 2; BOOL
    all           = 3; BOOL
:
--}}}

--{{{  P.LED
PROTOCOL P.LED
  CASE
    disconnected  = 0
    led           = 1; INT; BOOL
:
--}}}
--}}}

--{{{  FFI Interface
--{{{  PRAGMAs
#PRAGMA EXTERNAL "PROC C.tvmspecial.0.run.user (VAL []BYTE bytecode, VAL INT ws, vs, ms, VAL []BYTE tlp, VAL INT argc, CHAN BYTE console.in?, console.out!, CHAN P.LASER lasers!, CHAN P.LED leds!, CHAN P.MOTOR motors!) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.1.kill.user () = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.2.query.user (BOOL running, INT state, []BYTE context) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.3.set.register.16 (VAL INT addr, set, mask) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.4.set.camera.mode (VAL INT mode) = 0"
#PRAGMA EXTERNAL "PROC C.tvmspecial.5.get.camera.frame ([]BYTE frame) = 0"
--}}}
--{{{  Wrappers
PROC firmware.run.user (VAL []BYTE bytecode, VAL INT ws, vs, ms, CHAN BYTE console.in?, console.out!, CHAN P.LASER lasers!, CHAN P.LED leds!, CHAN P.MOTOR motors!)
  C.tvmspecial.0.run.user (bytecode, ws, vs, ms, "?!!!!", 5, console.in?, console.out!, lasers!, leds!, motors!)
:

PROC firmware.kill.user ()
  C.tvmspecial.1.kill.user ()
:

PROC firmware.query.user (BOOL running, INT state, []BYTE context)
  C.tvmspecial.2.query.user (running, state, context)
:

INLINE PROC safe.set.register.16 (VAL INT addr, set, mask)
  C.tvmspecial.3.set.register.16 (VAL INT addr, set, mask)
:

PROC set.camera.mode (VAL INT mode)
  C.tvmspecial.4.set.camera.mode (mode)
:

PROC get.camera.frame ([]BYTE frame)
  C.tvmspecial.5.get.camera.frame (frame)
:
--}}}
--}}}

#ENDIF -- !SRV1.INC
