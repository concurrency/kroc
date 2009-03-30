--
--	occSDLjoy.inc -- joystick interface for SDL wrapper
--	Copyright (C) 2008 Fred Barnes <frmb@kent.ac.uk>
--
--	This program is free software; you can redistribute it and/or modify
--	it under the terms of the GNU General Public License as published by
--	the Free Software Foundation; either version 2 of the License, or
--	(at your option) any later version.
--
--	This program is distributed in the hope that it will be useful,
--	but WITHOUT ANY WARRANTY; without even the implied warranty of
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--	GNU General Public License for more details.
--
--	You should have received a copy of the GNU General Public License
--	along with this program; if not, write to the Free Software
--	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
--

--** @module occSDL

VAL INT SDLJS.ITEMS IS 2:           -- axes and buttons

--* Joystick event protocol.
PROTOCOL P.SDLJS.EVENT
  CASE
    --* Reporting change in joystick status.
    -- @item ax.delta Bit-field indicating changes to axis values.
    -- @item bt.delta Bit-field indicating changes to button values.
    -- @item axis 16-bit joystick axis values.
    -- @item btns Byte array of joystick button values.
    change; INT; INT; MOBILE []INT16; MOBILE []BYTE
    --* Joystick server process terminating.
    quit
:

--* Joystick event input protocol.
PROTOCOL P.SDLJS.EVENT.IN
  CASE
    force.event                     --* Pretend a change happened and generate a new event.
:

--* Joystick event channel-type.
CHAN TYPE CT.SDLJSEVENT
  MOBILE RECORD
    CHAN P.SDLJS.EVENT.IN in?:      --* Commands to control joystick events.
    CHAN P.SDLJS.EVENT out!:        --* Joystick events.
:

--* Joystick calibration input protocol.
PROTOCOL P.SDLJSCALIBRATE.IN
  CASE
    calibrate                       --* Start joystick calibration.
:

--* Joystick calibration output protocol.
PROTOCOL P.SDLJSCALIBRATE.OUT
  CASE
    message; MOBILE []BYTE          --* Message to be reported to the user.
    done                            --* Calibration complete.
:

--* Joystick calibration channel-type.
CHAN TYPE CT.SDLJSCALIBRATE
  MOBILE RECORD
    CHAN P.SDLJSCALIBRATE.IN in?:   --* Commands to calibrator process.
    CHAN P.SDLJSCALIBRATE.OUT out!: --* Responses from calibrator process.
:

--* Joystick input protocol.
PROTOCOL P.SDLJS.IN
  CASE
    enable                          --* Enable joystick data capture, returns [@ref CT.SDLJSEVENT] client channel-end.
    disable                         --* Disable joystick data capture.
    query                           --* Query joystick (number of axes and buttons).
:

--* Joystick output protocol.
PROTOCOL P.SDLJS.OUT
  CASE
    event.chan; CT.SDLJSEVENT!      --* Channel-end on which events are received.
    query; [SDLJS.ITEMS]INT         --* Number of axes and buttons.
:

--* Joystick channel-type.
CHAN TYPE CT.SDLJS
  MOBILE RECORD
    CHAN P.SDLJS.IN in?:            --* Commands to joystick.
    CHAN P.SDLJS.OUT out!:          --* responses from joystick.
:

--* Joystick server input protocol.
PROTOCOL P.SDLJOY.IN
  CASE
    num.sticks                      --* Report number of joysticks present.
    query.stick; INT                --* Query characteristics of a particular stick.
    quit                            --* Tell the server process to terminate.
    connect; INT                    --* Connect to specified joystick.
:

--* Joystick server output protocol.
PROTOCOL P.SDLJOY.OUT
  CASE
    num.sticks; INT                 --* Number of sticks.
    query; INT; [SDLJS.ITEMS]INT    --* Query result.
    error                           --* General error result.
    joystick; SHARED CT.SDLJS!      --* Connection to joystick handler.
:

--* Joystick server channel-type.
CHAN TYPE CT.SDLJOY
  MOBILE RECORD
    CHAN P.SDLJOY.IN in?:           --* Commands to joystick server.
    CHAN P.SDLJOY.OUT out!:         --* Responses from joystick server.
:


