--
--	Occade definitions
--	Copyright (C) 2007  University of Kent
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

--** A library for programming simple arcade games in occam-pi.
--
-- This library provides facilities for occam programmers similar to those
-- available on microcomputers of the early 1980s: a fixed playfield that's
-- displayed in the background (see [@ref occade.load.playfield]), a number
-- of moving sprites on top of it with collision detection (see [@ref
-- occade.start.sprite]), and event-based input (see [@ref
-- occade.start.input]). Unlike 1980s micros, you've got a 24-bit display and
-- potentially thousands of sprites to play with.
--
-- Image files referenced below must either be in BMP format, or in any format
-- supported by the [@ref rasterio] module (e.g. PNG). Any areas coloured [@ref
-- OCCADE.TRANSPARENT.COLOUR] will appear transparent. Note, however, that
-- collision detection is currently done simply by using the bounding boxes of
-- the sprites -- so don't leave big borders around your images unless you want
-- them to be really easy to hit!
--
-- Font image files, used for text rendering, must contain a 16x16 rectangular
-- grid of characters. The font size will be computed automatically from the
-- image size.
--
-- There are some keypresses that all Occade programs will recognise regardless
-- of whether you've written any input code:
-- [@item] Alt-Return: toggles full-screen mode on platforms that support it.
-- [@item] Alt-Backspace: cause the program to [@code STOP] immediately.
--
-- To use this library:
--
-- [@code
-- #INCLUDE "occade.module"]
--
-- @module occade

#IF NOT DEFINED (OCCADE.INC)
#DEFINE OCCADE.INC

--{{{ constants
--* The colour in images that is considered transparent when drawing.
-- RGB [@text #EE66EE], which is a sort of pinkish purple.
VAL [3]BYTE OCCADE.TRANSPARENT.COLOUR IS [#EE, #66, #EE]:
--}}}

--{{{ startup parameters
--* Startup parameters for Occade.
-- You should initialise this from [@ref occade.default.params] and then just change
-- the fields you want.
DATA TYPE OCCADE.PARAMS
  RECORD
    INT width, height: --* Screen width and height in pixels.
    INT depth: --* Pixel depth in bits.
:

--* Default parameters for Occade.
VAL OCCADE.PARAMS occade.default.params IS
  [800, 600, 32]:
--}}}

--{{{ image cache
--* Commands to the image cache.
-- @private
PROTOCOL OCCADE.TO.IMAGE.CACHE
  CASE
    image; MOBILE []BYTE
    quit
:
--}}}

--{{{ sprites
--* Internal information about a sprite.
-- @private
DATA TYPE OCCADE.SPRITE.INFO
  RECORD
    INT x, y:
    BOOL centre:
    BOOL position.set:
    BOOL visible:
    BOOL collisions:
:

--* Reports of collisions between sprites.
DATA TYPE OCCADE.COLLISION
  RECORD
    INT id: --* ID of the other sprite
    INT x, y: --* Position of the other sprite
    INT w, h: --* Width and height of the other sprite
:

--* Commands that can be sent to the sprite.
PROTOCOL OCCADE.SPRITE.REQ
  CASE
    --* Set the sprite's image from a file.
    -- @item name Image filename
    load.image; MOBILE []BYTE
    --* Set the sprite's image from a raster. Returns [@code return.raster].
    -- @item raster Raster image
    load.raster; RASTER
    --* Set the sprite's image by rendering text.
    -- @item text String to render
    -- @item font Font image filename
    load.text; MOBILE []BYTE; MOBILE []BYTE
    --* Set the sprite's position.
    -- @item x, y Coordinates
    -- @item centre Whether to centre the sprite on these coordinates.
    --  If this is [@code FALSE], the coordinates are the top left corner of
    --  the sprite; if [@code TRUE], they are the centre of the sprite.
    move; INT; INT; BOOL
    --* Show or hide the sprite.
    visible; BOOL
    --* Enable or disable collision reports.
    collisions; BOOL
    --* Get the size of the sprite. Returns [@code get.size].
    get.size
    --* Get the position of the sprite. Returns [@code get.position].
    get.position
    --* Make the sprite exit.
    -- Use [@ref occade.finish.input] rather than sending this by hand, since
    -- that'll mop up any remaining collision reports too.
    quit
:

--* Responses from the sprite.
PROTOCOL OCCADE.SPRITE.RESP
  CASE
    --* Returning a used raster.
    return.raster; RASTER
    --* The size of the sprite.
    -- @item w, h Width and height in pixels
    get.size; INT; INT
    --* The position of the sprite.
    -- @item x, y Coordinates
    -- @item centre Whether the sprite is centred on those coordinates
    get.position; INT; INT; BOOL
:

--* Channel bundle for controlling sprites.
CHAN TYPE OCCADE.SPRITE
  MOBILE RECORD
    --* Commands to the sprite.
    CHAN OCCADE.SPRITE.REQ req?:
    --* Responses from the sprite.
    CHAN OCCADE.SPRITE.RESP resp!:
    --* Reports of collisions with other sprites.
    -- You'll only get messages on this channel if you've enabled them
    -- by sending the [@code collisions; TRUE] command first.
    --
    -- Messages can arrive at any time; you must be prepared to handle them
    -- interleaved with communications on other channels, and you must be
    -- prepared to handle some collisions arriving after you disable collisions
    -- or tell the sprite to exit. (See [@ref occade.finish.input].)
    --
    -- This channel comes from a limited-capacity overwriting buffer, so you
    -- will lose events (rather than deadlocking the system) if you persist in
    -- ignoring it.
    --
    -- Each sprite will only be told about each collision once -- once two
    -- objects are in contact with each other, no more collision reports will
    -- be sent about that pair of objects until they move apart again.
    CHAN OCCADE.COLLISION collisions!:
:
--}}}

--{{{ input events
--* Commands that can be sent to the event filter
PROTOCOL OCCADE.INPUT.REQ
  CASE
    --* Pick the types of events you're interested in.
    -- This should be the bitwise OR of values from [@ref RASTER.ET].
    enable; INT
    --* Close this event filter.
    -- Use [@ref occade.finish.input] rather than sending this by hand, since
    -- that'll mop up any remaining input events too.
    quit
:

--* Responses from the event filter.
PROTOCOL OCCADE.INPUT.RESP
  CASE
:

--* Channel bundle for carrying input events.
CHAN TYPE OCCADE.INPUT
  MOBILE RECORD
    --* Commands to the event filter.
    CHAN OCCADE.INPUT.REQ req?:
    --* Responses from the event filter.
    CHAN OCCADE.INPUT.RESP resp!:
    --* Input events.
    -- You won't receive any events until you turn some on using the
    -- [@code enable; N] command. Once you've done that, events can arrive at
    -- any time.
    CHAN RASTER.EVENT events!:
:
--}}}

--{{{ OCCADE handles and internals
--* Internal channel bundle between the display server and individual sprites.
-- @private
CHAN TYPE OCCADE.SPRITE.INTERNAL
  MOBILE RECORD
    CHAN OCCADE.COLLISION collisions?:
:

--* Internal channel bundle between the display server and event filters.
-- @private
CHAN TYPE OCCADE.FILTER.INTERNAL
  MOBILE RECORD
    CHAN RASTER.EVENT events?:
:

--* Commands that can be sent to the display.
-- @private
PROTOCOL OCCADE.REQ
  CASE
    --* Load an image onto the playfield.
    -- @item name Image filename
    -- @item x, y Top left corner coordinates
    load.playfield; MOBILE []BYTE; INT; INT
    --* Draw a raster image onto the playfield. Returns [@code return.raster].
    -- @item raster Raster image
    -- @item x, y Top left corner coordinates
    draw.playfield; RASTER; INT; INT
    --* Create a new sprite. Returns [@code new.sprite].
    -- @item id User-visible ID
    -- @item cli Internal channel bundle client end
    new.sprite; INT; OCCADE.SPRITE.INTERNAL!
    --* Delete a sprite.
    -- @item id Sprite ID
    delete.sprite; INT
    --* Load a new image for a sprite.
    load.sprite.image; INT; MOBILE []BYTE
    --* Set a sprite's image from a raster. Returns [@code return.raster].
    load.sprite.raster; INT; RASTER
    --* Render a new sprite image from a string.
    load.sprite.text; INT; MOBILE []BYTE; MOBILE []BYTE
    --* Update a sprite's information.
    set.sprite.info; INT; OCCADE.SPRITE.INFO
    --* Get a sprite's size. Returns [@code get.sprite.size].
    get.sprite.size; INT
    --* Attach a new event filter. Returns [@code attach.filter].
    attach.filter; OCCADE.FILTER.INTERNAL!
    --* Detach an event filter.
    -- @item id Stream ID
    detach.filter; INT
    --* Get a random integer between [@code 0] and [@code max - 1].
    -- Returns [@code random].
    -- @item max Limit
    random; INT
    --* Shut down Occade.
    quit
:

--* Responses from the display.
-- @private
PROTOCOL OCCADE.RESP
  CASE
    --* New sprite created.
    -- @item id Sprite ID
    new.sprite; INT
    --* Returning a used raster.
    return.raster; RASTER
    --* A sprite's size.
    get.sprite.size; INT; INT
    --* New event filter attached.
    -- @item id Stream ID
    attach.filter; INT
    --* Random integer generated.
    random; INT
:

--* Channel bundle for communicating with Occade.
-- You shouldn't use the channels inside this in your own code; you can treat
-- the shared client end of this as an opaque "handle" for Occade PROCs to use.
CHAN TYPE OCCADE
  MOBILE RECORD
    CHAN OCCADE.REQ req?:
    CHAN OCCADE.RESP resp!:
:
--}}}

#ENDIF

