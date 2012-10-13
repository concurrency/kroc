--
--	types.inc -- types and protocols for the new game
--	Copyright (C) 2003-2004 Fred Barnes <frmb@kent.ac.uk>
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
--	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--


--{{{  forward declarations of channel-types
CHAN TYPE CT.CLIENT:
CHAN TYPE CT.ROOMLINK:
--}}}

--{{{  GSTR (game string type)
DATA TYPE GSTR
  RECORD
    [256]BYTE str:
    INT slen:
:

--}}}
--{{{  OBJECT
PROTOCOL P.OBJECT.IN
  CASE
    get.name
    examine
    die
    support.action; MOBILE []BYTE
    use.client; MOBILE []BYTE; CT.CLIENT!
    use
    get.damage
    get.health
:
PROTOCOL P.OBJECT.OUT
  CASE
    name; MOBILE []BYTE
    damage; INT
    health; INT
    yes
    no
:
CHAN TYPE CT.OBJECT
  MOBILE RECORD
    CHAN P.OBJECT.IN in?:
    CHAN P.OBJECT.OUT out!:
:
--}}}
--{{{  CLIENT
PROTOCOL P.CLIENT.OUT
  CASE
    say; MOBILE []BYTE                      -- SAY message
    examine; MOBILE []BYTE                  -- EXAMINE action
    get; MOBILE []BYTE                      -- GET action
    use; MOBILE []BYTE                      -- USE action
    drop; MOBILE []BYTE                     -- DROP action
    eat; MOBILE []BYTE                      -- EAT action
    north                                   -- NORTH
    south                                   -- SOUTH
    east                                    -- EAST
    west                                    -- WEST
    look
    inv
    status
    wield; MOBILE []BYTE                    -- WIELD action
    unwield                                 -- UNWIELD action
    attack; MOBILE []BYTE                   -- ATTACK action
    logout
    help                                    -- asking for help
    halt                                    -- attempting to stop the game
    sproom; MOBILE []BYTE; INT              -- spawn room
    spdoor; MOBILE []BYTE; INT; INT         -- spawn door
    spobj; MOBILE []BYTE; INT               -- spawn object
    spbot; MOBILE []BYTE; MOBILE []BYTE; INT; INT; INT; MOBILE []BYTE   -- spawn bot
    spdobj; MOBILE []BYTE; MOBILE []BYTE; INT -- spawn dispenser object
    splinc; INT                             -- spawn linc terminal
    link; INT; INT; INT                     -- link rooms
    bad.input; MOBILE []BYTE                -- non-conformant input

    do.logout

    others
    objects
    roomdrop; MOBILE []BYTE; CT.OBJECT!
    action; MOBILE []BYTE                   -- ACTION message
    have.object; MOBILE []BYTE
    check.wielded
    do.attack; MOBILE []BYTE; MOBILE []BYTE; INT  -- who; object-name; damage
    lookexits
:
PROTOCOL P.CLIENT.IN
  CASE
    get.object; MOBILE []BYTE; CT.OBJECT!
    yes
    no
    wielding; MOBILE []BYTE; INT                -- held object and relative damage
    exits; INT; [4]BOOL
    string; MOBILE []BYTE                       -- list item
    end.of.list                                 -- end of list
    returned.client; MOBILE []BYTE; CT.CLIENT!  -- used during logout
:
PROTOCOL S.CLIENT.IN
  CASE
    get.name                                    -- get name
    genmsg; MOBILE []BYTE                       -- general message
    object; MOBILE []BYTE; CT.OBJECT!           -- object given to client (auto-collect?)
    have.object; MOBILE []BYTE                  -- object query
    enter.room; MOBILE []BYTE                   -- someone entering the room
    leaving.room; MOBILE []BYTE; INT            -- someone leaving the room
    self.enter.room; INT                        -- us entering a room (room-number)
    drop.object; MOBILE []BYTE; MOBILE []BYTE   -- someone dropping something  pname; oname
    say.room; MOBILE []BYTE; MOBILE []BYTE      -- someone saying something
    action.room; MOBILE []BYTE; MOBILE []BYTE   -- someone doing something
    take.damage; MOBILE []BYTE; MOBILE []BYTE; INT -- some damage inflicted (who; object; damage)
    dropall; SHARED CT.ROOMLINK!                -- drop all held objects
    resurrect                                   -- bring client back to life
    status                                      -- request status

    selfmsg; MOBILE []BYTE                      -- for self-actions
    do.logout
:
PROTOCOL S.CLIENT.OUT
  CASE
    name; MOBILE []BYTE                         -- response for get.name
    reject.object; MOBILE []BYTE; CT.OBJECT!    -- response for for object, or yes
    dead                                        -- response if we died for some reason
    yes
    no
    status; INT; INT; MOBILE []BYTE             -- response for status: health; vitality; level/desc.
:
CHAN TYPE CT.CLIENT
  MOBILE RECORD
    CHAN P.CLIENT.IN in?:                       -- client -> game   (request)
    CHAN P.CLIENT.OUT out!:                     -- client -> game   (response)
    CHAN S.CLIENT.IN s.in?:                     -- game -> client   (request)
    CHAN S.CLIENT.OUT s.out!:                   -- game -> client   (response)
:
--}}}
--{{{  ROOMLINK
PROTOCOL P.ROOMLINK.IN
  CASE
    new.client; MOBILE []BYTE; CT.CLIENT!
    new.object; MOBILE []BYTE; CT.OBJECT!
    set.link; INT                           -- 0 = north, 1 = east, 2 = south, 3 = west
    del.link; INT                           -- 0 = north, 1 = east, 2 = south, 3 = west
:
RECURSIVE CHAN TYPE CT.ROOMLINK
  MOBILE RECORD
    CHAN P.ROOMLINK.IN in?:
    CHAN SHARED CT.ROOMLINK! link.in?:
:

PROTOCOL P.ROOMACK.IN IS INT; SHARED CT.ROOMLINK!:
CHAN TYPE CT.ROOMACK
  MOBILE RECORD
    CHAN P.ROOMACK.IN link?:
:
--}}}
--{{{  GAMECTRL
PROTOCOL P.GAMECTRL.IN
  CASE
    spawn.room
    spawn.object
    attach.client; MOBILE []BYTE; CT.CLIENT!
    attach.room; INT; SHARED CT.ROOMLINK!
    dead.client; MOBILE []BYTE; CT.CLIENT!
    link.room; INT; INT; INT
    get.room; INT
    read.room.desc; MOBILE []BYTE
    debug; MOBILE []BYTE
    register.name; MOBILE []BYTE
    unregister.name; MOBILE []BYTE
    get.n.rooms
:
PROTOCOL P.GAMECTRL.OUT
  CASE
    error; MOBILE []BYTE
    string; MOBILE []BYTE
    end.of.list
    room; SHARED CT.ROOMLINK!
    cli.error; MOBILE []BYTE; CT.CLIENT!
    name.in.use
    ok
    n.rooms; INT
:
CHAN TYPE CT.GAMECTRL
  MOBILE RECORD
    CHAN P.GAMECTRL.IN in?:
    CHAN P.GAMECTRL.OUT out!:
:
--}}}
--{{{  IO.LINK
CHAN TYPE IO.LINK
  MOBILE RECORD
    CHAN MOBILE []BYTE in?:
    CHAN MOBILE []BYTE out!:
:
--}}}
--{{{  ARG.POS
DATA TYPE ARG.POS
  RECORD
    INT start:
    INT len:
:
--}}}
--{{{  IRC.GAME.OUT
PROTOCOL IRC.GAME.OUT
  CASE
    message; MOBILE []BYTE; MOBILE []BYTE
    failmessage; MOBILE []BYTE; MOBILE []BYTE
:
--}}}
--{{{  LINCLINK
PROTOCOL P.LINCLINK.IN
  CASE
    new.client; MOBILE []BYTE; CT.CLIENT!; INT
:
PROTOCOL P.LINCLINK.OUT
  CASE
    client.err; MOBILE []BYTE; CT.CLIENT!
:
CHAN TYPE CT.LINCLINK
  MOBILE RECORD
    CHAN P.LINCLINK.IN in?:
    CHAN P.LINCLINK.OUT out!:
:
--}}}
--{{{  CT.PLINC
PROTOCOL PLINC.IN
  CASE
    none
:
CHAN TYPE CT.PLINC
  MOBILE RECORD
    CHAN PLINC.IN in?:
:
--}}}

