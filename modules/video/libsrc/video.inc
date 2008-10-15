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

--{{{  V4L2.CAPABILITY
--* Must match struct v4l2_capability
DATA TYPE V4L2.CAPABILITY
  PACKED RECORD
    [16]BYTE driver:
    [32]BYTE card:
    [32]BYTE businfo:
    INT32 version:
    INT32 capabilities:
    [4]INT32 reserved:

    -- occampi fields
    INT driverlen, cardlen, businfolen:       -- lengths of string fields
:

--}}}
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



#ENDIF

