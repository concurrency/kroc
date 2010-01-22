--
--	Definitions for Pioneer Robot module
--	Copyright (C) 2008  Carl Ritson <cgr@kent.ac.uk>
--
--	This library is free software; you can redistribute it and/or
--	modify it under the terms of the GNU Lesser General Public
--	License as published by the Free Software Foundation, either
--	version 2 of the License, or (at your option) any later version.
--
--	This library is distributed in the hope that it will be useful,
--	but WITHOUT ANY WARRANTY; without even the implied warranty of
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--	Lesser General Public License for more details.
--
--	You should have received a copy of the GNU Lesser General Public
--	License along with this library.  If not, see
--	<http://www.gnu.org/licenses/>.
--

-- @module pioneer

VAL BYTE CMD.SYNC0 IS 0:
VAL BYTE CMD.SYNC1 IS 1:
VAL BYTE CMD.SYNC2 IS 2:
VAL BYTE CMD.PULSE IS 0:
VAL BYTE CMD.OPEN IS 1:
VAL BYTE CMD.CLOSE IS 2:
VAL BYTE CMD.POLLING IS 3:
VAL BYTE CMD.ENABLE IS 4:
VAL BYTE CMD.SETA IS 5:
VAL BYTE CMD.SETV IS 6:
VAL BYTE CMD.SETO IS 7:
VAL BYTE CMD.MOVE IS 8:
VAL BYTE CMD.ROTATE IS 9:
VAL BYTE CMD.SETRV IS 10:
VAL BYTE CMD.VEL IS 11:
VAL BYTE CMD.HEAD IS 12:
VAL BYTE CMD.DHEAD IS 13:
VAL BYTE CMD.SAY IS 15:
VAL BYTE CMD.CONFIG IS 18:
VAL BYTE CMD.ENCODER IS 19:
VAL BYTE CMD.RVEL IS 21:
VAL BYTE CMD.DCHEAD IS 22:
VAL BYTE CMD.SETRA IS 23:
VAL BYTE CMD.SONAR IS 28:
VAL BYTE CMD.STOP IS 29:
VAL BYTE CMD.DIGOUT IS 30:
VAL BYTE CMD.VEL2 IS 32:
VAL BYTE CMD.GRIPPER IS 33:
VAL BYTE CMD.ADSEL IS 35:
VAL BYTE CMD.GRIPPERVAL IS 36:
VAL BYTE CMD.GRIPREQUEST IS 37:
VAL BYTE CMD.IOREQUEST IS 40:
VAL BYTE CMD.PTUPOS IS 41:
VAL BYTE CMD.TTY2 IS 42:
VAL BYTE CMD.GETAUX IS 43:
VAL BYTE CMD.BUMP.STALL IS 44:
VAL BYTE CMD.TCM2 IS 45:
VAL BYTE CMD.DOCK IS 46:
VAL BYTE CMD.JOYDRIVE IS 47:
VAL BYTE CMD.SONAR.CYCLE IS 48:
VAL BYTE CMD.HOSTBAUD IS 50:
VAL BYTE CMD.AUX1BAUD IS 51:
VAL BYTE CMD.AUX2BAUD IS 52:
VAL BYTE CMD.E.STOP IS 55:
VAL BYTE CMD.M.STALL IS 56:
VAL BYTE CMD.LEDGE IS 57:
VAL BYTE CMD.STEP IS 64:
VAL BYTE CMD.TTY3 IS 65:
VAL BYTE CMD.GETAUX2 IS 67:
VAL BYTE CMD.CHARGE IS 68:

PROTOCOL P.PIONEER.CMD
  CASE
    cmd; BYTE
    cmd.int; BYTE; BOOL; INT
    cmd.str; BYTE; MOBILE []BYTE
    shutdown
:

DATA TYPE POS
  RECORD
    INT time:
    INT x, y, th:
    INT compass:
:

DATA TYPE SONAR.UPDATE
  RECORD
    INT sensor:
    INT value:
:

DATA TYPE SONAR
  RECORD
    INT time:
    INT new:
    [8]REAL32 front:
    [8]REAL32 back:
:

DATA TYPE MOTOR
  RECORD
    INT time:
    BOOL enabled, running:
    BOOL left.stall, right.stall:
    INT left.v, right.v:
:

DATA TYPE GRIP
  RECORD
    INT time:
    INT uio, state:
    INT grasp:
:

DATA TYPE GRIPPER
  RECORD
    INT time:

    BOOL left.paddle:
    BOOL right.paddle:
    BOOL inner.beam:
    BOOL outer.beam:

    BOOL gripper.open:
    BOOL gripper.closed:
    BOOL paddles.moving:
    BOOL paddles.limit:
    BOOL paddles.error:

    BOOL gripper.up:
    BOOL gripper.down:
    BOOL lift.moving:
    BOOL lift.limit:
    BOOL lift.error:

    INT pressure:
: 

--{{{  CT.POSITION
PROTOCOL P.POSITION.REQ
  CASE
    get.latest.position
    get.newer.position; INT
:

PROTOCOL P.POSITION.RSP IS POS:

CHAN TYPE CT.POSITION
  MOBILE RECORD
    CHAN P.POSITION.REQ req?:
    CHAN P.POSITION.RSP rsp!:
:
--}}}


--{{{  CT.SONAR
PROTOCOL P.SONAR.REQ
  CASE
    get.latest.sonar
    get.newer.sonar; INT
    enable.sonar
    disable.sonar
:

PROTOCOL P.SONAR.RSP IS SONAR:

CHAN TYPE CT.SONAR
  MOBILE RECORD
    CHAN P.SONAR.REQ req?:
    CHAN P.SONAR.RSP rsp!:
:
--}}}


--{{{  CT.DRIVE
PROTOCOL P.DRIVE.REQ
  CASE
    get.latest.motor.state
    get.newer.motor.state; INT
    enable.motors
    disable.motors
    adjust.heading; INT
    translate; INT
    set.velocity; INT
    set.angular.velocity; INT
    set.motors; INT; INT
    stop; BOOL; BOOL -- wait; emergency
:

PROTOCOL P.DRIVE.RSP
  CASE
    motor.state; MOTOR
    ok
:

CHAN TYPE CT.DRIVE
  MOBILE RECORD
    CHAN P.DRIVE.REQ req?:
    CHAN P.DRIVE.RSP rsp!:
:
--}}}


--{{{  CT.GRIPPER
PROTOCOL P.GRIPPER.REQ
  CASE
    get.latest.gripper.state
    get.newer.gripper.state; INT
    open.paddles
    close.paddles
    stop.paddles
    raise.lift
    lower.lift
    stop.lift
    store.gripper
    deploy.gripper
    halt.gripper
    set.pressure; INT
    set.lift.move; INT
:

PROTOCOL P.GRIPPER.RSP
  CASE
    gripper; GRIPPER
    ok
:

CHAN TYPE CT.GRIPPER
  MOBILE RECORD
    CHAN P.GRIPPER.REQ req?:
    CHAN P.GRIPPER.RSP rsp!:
:
--}}}

