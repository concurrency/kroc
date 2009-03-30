--
--	video.inc -- video-4-linux-2 frame-grabber interface
--	Copyright (C) 2008 Fred Barnes, University of Kent  <frmb@kent.ac.uk>
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

--** V4L(2) frame-grabber interface.
--
-- This library allows an application to open a V4L(2) device and
-- interact with it.  This currently only supports frame-grabbing from
-- devices.
--
-- To use this library:
--
-- [@code
-- #INCLUDE "video.module"]
--
-- @module video

#IF NOT DEFINED (VIDEO.INC)
#DEFINE VIDEO.INC

#INCLUDE "raster.module"

-- NOTE: the PACKED RECORD structures here have equivalents in C (video-v4l.h)

--{{{  VIDEO.IDENTITY
--* Maximum length of a video identity.
VAL INT VIDEO.IDENTITY.NAMEMAX IS 64:

--* Generic video device identity
DATA TYPE VIDEO.IDENTITY
  PACKED RECORD
    [VIDEO.IDENTITY.NAMEMAX]BYTE name:
    INT namelen:
:

--}}}
--{{{  VIDEO.INPUT
--* Maximum length of a input name.
VAL INT VIDEO.INPUT.NAMEMAX IS 64:

--* Generic video input information.
DATA TYPE VIDEO.INPUT
  PACKED RECORD
    INT id:
    [VIDEO.INPUT.NAMEMAX]BYTE name:
    INT namelen:
    INT type:
    INT flags:
    INT minw:
    INT minh:
    INT maxw:
    INT maxh:
:

--}}}
--{{{  VIDEO.PICTURE
--* Generic video picture information/settings.
DATA TYPE VIDEO.PICTURE
  PACKED RECORD
    INT brightness:
    INT hue:
    INT colour:
    INT contrast:
    INT whiteness:
:
--}}}
--{{{  VIDEO.FRAMEINFO
--* Generic video frame information/settings.
DATA TYPE VIDEO.FRAMEINFO
  PACKED RECORD
    INT width:
    INT height:
    INT format:
    INT bytes:
:
--}}}
--{{{  video input/output types and flags
VAL INT   VIDEO.TYPE.CAMERA   IS #1:
VAL INT   VIDEO.TYPE.TUNER    IS #2:
VAL INT   VIDEO.FLAG.AUDIO    IS #1:
--}}}
--{{{  video palette constants (from linux/videodev.h)
VAL INT16 VIDEO.PAL.INVALID   IS 0:
VAL INT16 VIDEO.PAL.GRAY      IS 1:
VAL INT16 VIDEO.PAL.HI240     IS 2:
VAL INT16 VIDEO.PAL.RGB565    IS 3:
VAL INT16 VIDEO.PAL.RGB24     IS 4:
VAL INT16 VIDEO.PAL.RGB32     IS 5:
VAL INT16 VIDEO.PAL.RGB555    IS 6:
VAL INT16 VIDEO.PAL.YUV422    IS 7:
VAL INT16 VIDEO.PAL.YUYV      IS 8:
VAL INT16 VIDEO.PAL.UYVY      IS 9:
VAL INT16 VIDEO.PAL.YUV420    IS 10:
VAL INT16 VIDEO.PAL.YUV411    IS 11:
VAL INT16 VIDEO.PAL.YUV422P   IS 13:
VAL INT16 VIDEO.PAL.YUV411P   IS 14:
VAL INT16 VIDEO.PAL.YUV420P   IS 15:
VAL INT16 VIDEO.PAL.YUV410P   IS 16:
VAL INT16 VIDEO.PAL.BGR24     IS 17:
VAL INT16 VIDEO.PAL.BGR32     IS 18:

VAL [][]BYTE VIDEO.PAL.NAMES IS ["invalid", "gray   ", "hi240  ", "rgb565 ",
                                 "rgb24  ", "rgb32  ", "rgb555 ", "yuv422 ",
                                 "yuyv   ", "uyvy   ", "yuv420 ", "yuv411 ",
                                 "invalid", "yuv422p", "yuv411p", "yuv420p",
                                 "yuv410p", "bgr24  ", "bgr32  "]:

--}}}

--{{{  P.VIDEO.REQ
--* Protocol used for communicating with a video-server process.
PROTOCOL P.VIDEO.REQ
  CASE
    get.identity                                  -- -> identity [] error
    get.inputs                                    -- -> inputs [] error
    get.current.input                             -- -> input.number [] error
    set.input; INT                                -- -> ok [] error
    get.picture                                   -- -> picture [] error
    set.picture; VIDEO.PICTURE                    -- -> picture [] error
    get.frameinfo                                 -- -> frameinfo [] error
    set.frameinfo; VIDEO.FRAMEINFO                -- -> frameinfo [] error
    grab.frame; BOOL                              -- -> raster [] frame [] error
    capture; BOOL                                 -- -> (raster [] frame)* [] error
    stop                                          -- -> ok [] error
:
--}}}
--{{{  P.VIDEO.RESP
--* Protocol used for communicating with a video-server process.
PROTOCOL P.VIDEO.RESP
  CASE
    ok                                            -- generic ok
    error; MOBILE []BYTE                          -- generic error
    identity; VIDEO.IDENTITY
    inputs; MOBILE []VIDEO.INPUT
    input.number; INT
    picture; VIDEO.PICTURE
    frameinfo; VIDEO.FRAMEINFO
    frame; MOBILE []BYTE
    raster; RASTER
:
--}}}
--{{{  CT.VIDEO
--* Channel-type used for communicating with a video-server process.
REC CHAN TYPE CT.VIDEO
  MOBILE RECORD
    CHAN P.VIDEO.REQ req?:
    CHAN P.VIDEO.RESP resp!:
    CHAN CT.VIDEO! return?:
:
--}}}


#ENDIF

