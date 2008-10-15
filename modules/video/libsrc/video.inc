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

--** A library for interfacing with video-4-linux devices from occam-pi
--
-- This library allows an application to open a video-4-linux device and
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
--{{{  VIDEO.CAMINPUT
--* Maximum length of a camera input name.
VAL INT VIDEO.CAMINPUT.NAMEMAX IS 64:

--* Generic video camera input information.
DATA TYPE VIDEO.CAMINPUT
  PACKED RECORD
    INT id:
    [VIDEO.CAMINPUT.NAMEMAX]BYTE name:
    INT namelen:
    INT minw:
    INT minh:
    INT maxw:
    INT maxh:
:

--}}}
--{{{  VIDEO.DEVICE
--* Maximum length of a video device name.
VAL INT VIDEO.DEVICE.FNAMEMAX IS 128:

--* Generic video device information.
DATA TYPE VIDEO.DEVICE
  PACKED RECORD
    [VIDEO.DEVICE.FNAMEMAX]BYTE fname:
    INT fnamelen:
    INT fd:
    INT api:
:

--}}}
--{{{  VIDEO.PICTURE
--* Generic video picture information/settings.
DATA TYPE VIDEO.PICTURE
  PACKED RECORD
    INT16 brightness:
    INT16 hue:
    INT16 colour:
    INT16 contrast:
    INT16 whiteness:
    INT16 depth:
    INT16 palette:

    INT16 dummy:
:
--}}}
--{{{  VIDEO.IODATA
--* Describes how video data is accessed (including width/height).
DATA VIDEO.IODATA
  PACKED RECORD
    BOOL use.mmap:
    INT mmap.addr:
    INT mmap.size:
    INT width:
    INT height:
:
  
--}}}
--{{{  video palette constants (from linux/videodev.h)
VAL INT16 VIDEO.PAL.INVALID IS 0:
VAL INT16 VIDEO.PAL.GRAY IS 1:
VAL INT16 VIDEO.PAL.HI240 IS 2:
VAL INT16 VIDEO.PAL.RGB565 IS 3:
VAL INT16 VIDEO.PAL.RGB24 IS 4:
VAL INT16 VIDEO.PAL.RGB32 IS 5:
VAL INT16 VIDEO.PAL.RGB555 IS 6:
-- FIXME: the rest of these please! (and need to cater for changing constants..)

VAL [][]BYTE VIDEO.PAL.NAMES IS ["invalid", "gray   ", "hi240  ", "rgb565 ",
                                 "rgb24  ", "rgb32  ", "rgb555 "]:

--}}}



#ENDIF

