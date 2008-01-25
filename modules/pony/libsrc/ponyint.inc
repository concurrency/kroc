-- Copyright: Mario Schweigler (C) 2005-2006
-- Institution: Computing Laboratory, University of Kent, Canterbury, UK
-- Description: pony internal include file

-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
-- MA 02110-1301, USA.

--{{{  Constants
-- Direction-types
VAL INT PONYC.INT.DIRTYPE.CLI IS 0:
VAL INT PONYC.INT.DIRTYPE.SVR IS 1:

-- Share-types
VAL INT PONYC.INT.SHARETYPE.CLAIMED IS 3:

-- Initial error-point
VAL INT PONYC.INT.INITIALERRPOINT IS 0:
--}}}
--{{{  Declarations

--{{{  PROTOCOL PONY.INT.ERRHANDLE.TO.HANDLER
-- To error-handler
PROTOCOL PONY.INT.ERRHANDLE.TO.HANDLER
  CASE
    -- Report error
    -- Carrying: error
    report.err; PONY.ERROR
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:
--}}}
--{{{  CHAN TYPE PONY.INT.ERRHANDLE
-- Internal error-handle (error-handler has server-end)
CHAN TYPE PONY.INT.ERRHANDLE
  MOBILE RECORD
    CHAN PONY.INT.ERRHANDLE.TO.HANDLER to.handler?:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOL PONY.INT.MSGHANDLE.TO.HANDLER
-- To message-handler
PROTOCOL PONY.INT.MSGHANDLE.TO.HANDLER
  CASE
    -- Part of message - string
    -- Carrying: string
    msg.str; MOBILE []BYTE
    ----------------------------------------------------------------------
    -- Part of message - integer
    -- Carrying: integer
    msg.int; INT
    ----------------------------------------------------------------------
    -- Part of message - integer (hex format)
    -- Carrying: integer
    msg.hex; INT
    ----------------------------------------------------------------------
    -- Part of message - boolean
    -- Carrying: boolean
    msg.bool; BOOL
    ----------------------------------------------------------------------
    -- End of message
    -- Carrying: error-flag
    msg.end; BOOL
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:
--}}}
--{{{  CHAN TYPE PONY.INT.MSGHANDLE
-- Internal message-handle (message-handler has server-end)
CHAN TYPE PONY.INT.MSGHANDLE
  MOBILE RECORD
    CHAN PONY.INT.MSGHANDLE.TO.HANDLER to.handler?:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOLs PONY.INT.DECODEHANDLE.TO.HANDLER and PONY.INT.DECODEHANDLE.FROM.HANDLER
-- To decode-handler
PROTOCOL PONY.INT.DECODEHANDLE.TO.HANDLER
  CASE
    -- Start session
    start.session
    ----------------------------------------------------------------------
    -- First CLC or remaining CLCs have been taken by remote user-level
    --   process
    ack
    ----------------------------------------------------------------------
    -- Cancel session
    cancel.session
    ----------------------------------------------------------------------
    -- Cancel encode
    cancel.encode
    ----------------------------------------------------------------------
    -- Cancel encode acknowledgement (CLC was outputted by encoder)
    cancel.encode.ack
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:

-- From decode-handler
PROTOCOL PONY.INT.DECODEHANDLE.FROM.HANDLER
  CASE
    -- First CLC of next ULC
    -- Carrying: number of data-item NLCs in first CLC;
    --           number of channel-type-end NLCs in first CLC;
    --           are there remaining CLCs?
    first.clc; INT; INT; BOOL
    ----------------------------------------------------------------------
    -- Remaining CLCs of ULC
    -- Carrying: number of data-item NLCs in remaining CLCs;
    --           number of channel-type-end NLCs in remaining CLCs
    rest.clcs; INT; INT
    ----------------------------------------------------------------------
    -- Next CLC is an end-over-itself-CLC
    end.over.itself.clc
    ----------------------------------------------------------------------
    -- CLC (first CLC or remaining CLCs)
    -- Carrying: address-array;
    --           size-array
    clc; MOBILE []INT; MOBILE []INT
    ----------------------------------------------------------------------
    -- Confirm cancellation
    cancel.confirm
    ----------------------------------------------------------------------
    -- Cancel encode: CLC was cancelled
    encode.cancelled
    ----------------------------------------------------------------------
    -- Cancel encode: CLC was not cancelled
    encode.not.cancelled
    ----------------------------------------------------------------------
    -- Shutdown - confirm
    shutdown.confirm
:
--}}}
--{{{  CHAN TYPE PONY.INT.DECODEHANDLE
-- Internal decode-handle (decode-handler has server-end, CTB-handler has
--   client-end)
CHAN TYPE PONY.INT.DECODEHANDLE
  MOBILE RECORD
    CHAN PONY.INT.DECODEHANDLE.TO.HANDLER to.handler?:
    CHAN PONY.INT.DECODEHANDLE.FROM.HANDLER from.handler!:
:
--}}}

--{{{  PROTOCOL PONY.INT.DECODEREPLYHANDLE.TO.HANDLER
-- To decode-handler from NCT-manager (via link)
PROTOCOL PONY.INT.DECODEREPLYHANDLE.FROM.NCTMGR
  CASE
    -- Make CTB networked - confirm
    -- Carrying: NCT-ID
    make.ctb.networked.confirm; INT
:
--}}}
--{{{  CHAN TYPE PONY.INT.DECODEREPLYHANDLE
-- Decode-reply-handle (decode-handler has server-end, client-end available
--   via CTB-handler)
CHAN TYPE PONY.INT.DECODEREPLYHANDLE
  MOBILE RECORD
    CHAN PONY.INT.DECODEREPLYHANDLE.FROM.NCTMGR to.handler?:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOLs PONY.INT.ENCODEHANDLE.TO.HANDLER and PONY.INT.ENCODEHANDLE.FROM.HANDLER
-- To encode-handler
PROTOCOL PONY.INT.ENCODEHANDLE.TO.HANDLER
  CASE
    -- First CLC of next ULC
    -- Carrying: number of data-item NLCs in first CLC;
    --           number of channel-type-end NLCs in first CLC;
    --           are there remaining CLCs?
    first.clc; INT; INT; BOOL
    ----------------------------------------------------------------------
    -- Remaining CLCs of ULC
    -- Carrying: number of data-item NLCs in remaining CLCs;
    --           number of channel-type-end NLCs in remaining CLCs
    rest.clcs; INT; INT
    ----------------------------------------------------------------------
    -- CLC (first CLC or remaining CLCs)
    -- Carrying: data-array;
    --           size-array
    clc; MOBILE []BYTE; MOBILE []INT
    ----------------------------------------------------------------------
    -- Cancel session
    cancel.session
    ----------------------------------------------------------------------
    -- Output successful
    output.successful
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:

-- From encode-handler
PROTOCOL PONY.INT.ENCODEHANDLE.FROM.HANDLER
  CASE
    -- First CLC or remaining CLCs have been outputted
    ack
    ----------------------------------------------------------------------
    -- Shutdown - confirm
    shutdown.confirm
:
--}}}
--{{{  CHAN TYPE PONY.INT.ENCODEHANDLE
-- Internal encode-handle (encode-handler has server-end, CTB-handler has
--   client-end)
CHAN TYPE PONY.INT.ENCODEHANDLE
  MOBILE RECORD
    CHAN PONY.INT.ENCODEHANDLE.TO.HANDLER to.handler?:
    CHAN PONY.INT.ENCODEHANDLE.FROM.HANDLER from.handler!:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOLs PONY.INT.CTBMAINHANDLE.FROM.NCTHANDLER, PONY.INT.CTBMAINHANDLE.TO.HANDLER and PONY.INT.CTBMAINHANDLE.FROM.HANDLER
-- To CTB-handler from NCT-handler (via link)
PROTOCOL PONY.INT.CTBMAINHANDLE.FROM.NCTHANDLER
  CASE
    -- Confirm claim - currently no other end claimed
    claim.confirm.nootherend
    ----------------------------------------------------------------------
    -- Confirm claim - other end is claimed
    -- Carrying: node-ID of the other end's node
    claim.confirm.otherendclaimed; INT
    ----------------------------------------------------------------------
    -- New other end (also used as a dummy for confirming a release request)
    -- Carrying: node-ID of the other end's node
    new.other.end; INT
:

-- To CTB-handler
PROTOCOL PONY.INT.CTBMAINHANDLE.TO.HANDLER
  CASE
    -- Start session
    start.session
    ----------------------------------------------------------------------
    -- First CLC of a ULC
    -- Carrying: channel-ID of the relevant channel;
    --           number of data-item NLCs in first CLC;
    --           number of channel-type-end NLCs in first CLC;
    --           are there remaining CLCs?;
    --           data-array;
    --           size-array
    first.clc; INT; INT; INT; BOOL; MOBILE []BYTE; MOBILE []INT
    ----------------------------------------------------------------------
    -- Remaining CLCs of a ULC
    -- Carrying: channel-ID of the relevant channel;
    --           number of data-item NLCs in remaining CLCs;
    --           number of channel-type-end NLCs in remaining CLCs;
    --           data-array;
    --           size-array
    rest.clcs; INT; INT; INT; MOBILE []BYTE; MOBILE []INT
    ----------------------------------------------------------------------
    -- Acknowledge CLC
    -- Carrying: channel-ID of the relevant channel
    ack; INT
    ----------------------------------------------------------------------
    -- Cancel CLC
    -- Carrying: channel-ID of the relevant channel
    cancel; INT
    ----------------------------------------------------------------------
    -- Close session
    close.session
    ----------------------------------------------------------------------
    -- Suspend session
    suspend.session
    ----------------------------------------------------------------------
    -- Confirm `suspend.session'
    suspend.session.confirm
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:

-- From CTB-handler
PROTOCOL PONY.INT.CTBMAINHANDLE.FROM.HANDLER
  CASE
    -- Shutdown - confirm
    shutdown.confirm
:
--}}}
--{{{  CHAN TYPE PONY.INT.CTBMAINHANDLE
-- CTB-main-handle (CTB-handler has server-end, client-end available via
--   CTB-manager)
CHAN TYPE PONY.INT.CTBMAINHANDLE
  MOBILE RECORD
    -- From NCT-handler (via link) regarding client-end
    CHAN PONY.INT.CTBMAINHANDLE.FROM.NCTHANDLER from.ncthandler.re.client?:
    -- From NCT-handler (via link) regarding server-end
    CHAN PONY.INT.CTBMAINHANDLE.FROM.NCTHANDLER from.ncthandler.re.server?:
    -- Anything else
    CHAN PONY.INT.CTBMAINHANDLE.TO.HANDLER to.handler?:
    CHAN PONY.INT.CTBMAINHANDLE.FROM.HANDLER from.handler!:
:
--}}}

--{{{  PROTOCOLs PONY.INT.CTBINSTANTHANDLE.TO.HANDLER and PONY.INT.CTBINSTANTHANDLE.FROM.HANDLER
-- To CTB-handler
PROTOCOL PONY.INT.CTBINSTANTHANDLE.TO.HANDLER
  CASE
    -- Get decode-reply-handle for a given channel-ID
    -- Carrying: channel-ID
    get.dec.reply.handle; INT
    ----------------------------------------------------------------------
    -- Get ID of current remote node
    get.current.remote.node
:

-- From CTB-handler
PROTOCOL PONY.INT.CTBINSTANTHANDLE.FROM.HANDLER
  CASE
    -- Get decode-reply-handle - confirm
    -- Carrying: decode-reply-handle
    get.dec.reply.handle.confirm; SHARED PONY.INT.DECODEREPLYHANDLE!
    ----------------------------------------------------------------------
    -- Get ID of current remote node - currently no session
    get.current.remote.node.nosession
    ----------------------------------------------------------------------
    -- Get ID of current remote node - both ends on same node
    get.current.remote.node.samenode
    ----------------------------------------------------------------------
    -- Get ID of current remote node - confirm
    -- Carrying: ID of remote node
    get.current.remote.node.confirm; INT
:
--}}}
--{{{  CHAN TYPE PONY.INT.CTBINSTANTHANDLE
-- CTB-instant-handle (CTB-handler has server-end, client-end available via
--    CTB-manager)
CHAN TYPE PONY.INT.CTBINSTANTHANDLE
  MOBILE RECORD
    CHAN PONY.INT.CTBINSTANTHANDLE.TO.HANDLER to.handler?:
    CHAN PONY.INT.CTBINSTANTHANDLE.FROM.HANDLER from.handler!:
:
--}}}

--{{{  PROTOCOL PONY.INT.CTBCLAIMHANDLE.TO.HANDLER
-- To CTB-handler
PROTOCOL PONY.INT.CTBCLAIMHANDLE.TO.HANDLER
  CASE
    -- Claim
    claim
    ----------------------------------------------------------------------
    -- Release
    release
:
--}}}
--{{{  CHAN TYPE PONY.INT.CTBCLAIMHANDLE
-- CTB-claim-handle (CTB-handler has server-end, client-end available via
--   CTB-manager)
CHAN TYPE PONY.INT.CTBCLAIMHANDLE
  MOBILE RECORD
    CHAN PONY.INT.CTBCLAIMHANDLE.TO.HANDLER to.handler?:
:
--}}}

--{{{  PROTOCOLs PONY.INT.CTBMGRHANDLE.TO.MGR and PONY.INT.CTBMGRHANDLE.FROM.MGR
-- To CTB-manager
PROTOCOL PONY.INT.CTBMGRHANDLE.TO.MGR
  CASE
    -- Get CTB-main-handle for a given NCT-ID
    -- Carrying: NCT-ID
    get.ctb.main.handle; INT
    ----------------------------------------------------------------------
    -- Get CTB-instant-handle for a given NCT-ID
    -- Carrying: NCT-ID
    get.ctb.instant.handle; INT
    ----------------------------------------------------------------------
    -- Get CTB-claim-handle for a given NCT-ID
    -- Carrying: NCT-ID;
    --           direction-type of the end to be claimed/released
    --             (0 = client-end, 1 = server-end)
    get.ctb.claim.handle; INT; INT
    ----------------------------------------------------------------------
    -- Allocate new channel-type-end
    -- Carrying: NCT-ID
    alloc.ct.end; INT
    ----------------------------------------------------------------------
    -- Allocate new CTB - confirm channel-counts
    -- Carrying: number of channel-words in CTB;
    --           number of reading-ends in server-end
    alloc.new.ctb.confirm.chancounts; INT; INT
    ----------------------------------------------------------------------
    -- Allocate new CTB - confirm CTB-pointer
    -- Carrying: CTB-pointer
    alloc.new.ctb.confirm.ctbptr; INT
    ----------------------------------------------------------------------
    -- Make CTB networked
    -- Carrying: NCT-ID;
    --           CTB-pointer;
    --           client-end initially claimed?;
    --           server-end initially claimed?;
    --           number of channel-words;
    --           number of reading-ends in server-end
    make.ctb.networked; INT; INT; BOOL; BOOL; INT; INT
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:

-- From CTB-manager
PROTOCOL PONY.INT.CTBMGRHANDLE.FROM.MGR
  CASE
    -- Get CTB-main-handle - confirm
    -- Carrying: CTB-main-handle
    get.ctb.main.handle.confirm; SHARED PONY.INT.CTBMAINHANDLE!
    ----------------------------------------------------------------------
    -- Get CTB-instant-handle - invalid NCT-ID
    get.ctb.instant.handle.invalidnctid
    ----------------------------------------------------------------------
    -- Get CTB-instant-handle - confirm
    -- Carrying: CTB-instant-handle
    get.ctb.instant.handle.confirm; SHARED PONY.INT.CTBINSTANTHANDLE!
    ----------------------------------------------------------------------
    -- Get CTB-claim-handle - confirm
    -- Carrying: CTB-claim-handle
    get.ctb.claim.handle.confirm; SHARED PONY.INT.CTBCLAIMHANDLE!
    ----------------------------------------------------------------------
    -- Increase reference-count of CTB
    -- Carrying: CTB-pointer
    clone.ctb; INT
    ----------------------------------------------------------------------
    -- Allocate new CTB
    alloc.new.ctb
    ----------------------------------------------------------------------
    -- Allocate new CTB - confirm
    -- Carrying: network-hook-handle;
    --           array of decode-handles;
    --           array of encode-handles
    alloc.new.ctb.confirm; PONY.NETHOOKHANDLE!;
                           MOBILE []PONY.DECODEHANDLE!;
                           MOBILE []PONY.ENCODEHANDLE!
    ----------------------------------------------------------------------
    -- Make CTB networked - confirm
    -- Carrying: network-hook-handle;
    --           array of decode-handles;
    --           array of encode-handles
    make.ctb.networked.confirm; PONY.NETHOOKHANDLE!;
                                MOBILE []PONY.DECODEHANDLE!;
                                MOBILE []PONY.ENCODEHANDLE!
:
--}}}
--{{{  CHAN TYPE PONY.INT.CTBMGRHANDLE
-- CTB-manager-handle (CTB-manager has server-end)
CHAN TYPE PONY.INT.CTBMGRHANDLE
  MOBILE RECORD
    CHAN PONY.INT.CTBMGRHANDLE.TO.MGR to.mgr?:
    CHAN PONY.INT.CTBMGRHANDLE.FROM.MGR from.mgr!:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOLs PONY.INT.NCTHANDLE.TO.HANDLER and PONY.INT.NCTHANDLE.FROM.HANDLER
-- To NCT-handler
PROTOCOL PONY.INT.NCTHANDLE.TO.HANDLER
  CASE
    -- Claim client-end
    -- Carrying: node-ID of the CTB-handler making the request
    claim.cli; INT
    ----------------------------------------------------------------------
    -- Claim server-end
    -- Carrying: node-ID of the CTB-handler making the request
    claim.svr; INT
    ----------------------------------------------------------------------
    -- Confirm new-other-end
    new.other.end.confirm
    ----------------------------------------------------------------------
    -- Release client-end
    release.cli
    ----------------------------------------------------------------------
    -- Release server-end
    release.svr
    ----------------------------------------------------------------------
    -- End-over-itself operation completed
    -- Carrying: node-ID of the CTB-handler making the request
    eoi.done; INT
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:

-- From NCT-handler
PROTOCOL PONY.INT.NCTHANDLE.FROM.HANDLER
  CASE
    -- Shutdown - confirm
    shutdown.confirm
:
--}}}
--{{{  CHAN TYPE PONY.INT.NCTHANDLE
-- NCT-handle (NCT-handler has server-end, client-end available via
--   NCT-manager)
CHAN TYPE PONY.INT.NCTHANDLE
  MOBILE RECORD
    CHAN PONY.INT.NCTHANDLE.TO.HANDLER to.handler?:
    CHAN PONY.INT.NCTHANDLE.FROM.HANDLER from.handler!:
:
--}}}

--{{{  PROTOCOLs PONY.INT.NCTMGRHANDLE.TO.MGR and PONY.INT.NCTMGRHANDLE.FROM.MGR
-- To NCT-manager
PROTOCOL PONY.INT.NCTMGRHANDLE.TO.MGR
  CASE
    -- Get NCT-handle for a given NCT-ID
    -- Carrying: NCT-ID
    get.nct.handle; INT
    ----------------------------------------------------------------------
    -- Explicit allocation
    -- Carrying: node-ID of the kernel making the request;
    --           direction-type of the end to be allocated
    --             (0 = client-end, 1 = server-end);
    --           share-type of the end to be allocated
    --             (1 = unshared, 2 = shared);
    --           share-type of the opposite end
    --             (0 = unknown, 1 = unshared, 2 = shared);
    --           type-hash of the NCT to be allocated;
    --           name of the NCT
    alloc; INT; INT; INT; INT; INT; MOBILE[] BYTE
    ----------------------------------------------------------------------
    -- Make CTB networked
    -- Carrying: node-ID of the decode-handler making the request;
    --           NCT-ID of the decode-handler making the request;
    --           channel-ID of the decode-handler making the request;
    --           client-end initially claimed?;
    --           server-end initially claimed?
    make.ctb.networked; INT; INT; INT; BOOL; BOOL
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:

-- From NCT-manager
PROTOCOL PONY.INT.NCTMGRHANDLE.FROM.MGR
  CASE
    -- Get NCT-handle - confirm
    -- Carrying: NCT-handle
    get.nct.handle.confirm; SHARED PONY.INT.NCTHANDLE!
    ----------------------------------------------------------------------
    -- Shutdown - confirm
    shutdown.confirm
:
--}}}
--{{{  CHAN TYPE PONY.INT.NCTMGRHANDLE
-- NCT-manager-handle (NCT-manager has server-end)
CHAN TYPE PONY.INT.NCTMGRHANDLE
  MOBILE RECORD
    CHAN PONY.INT.NCTMGRHANDLE.TO.MGR to.mgr?:
    CHAN PONY.INT.NCTMGRHANDLE.FROM.MGR from.mgr!:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOL PONY.INT.LINKHANDLE.TO.HANDLER
-- To link-handler
PROTOCOL PONY.INT.LINKHANDLE.TO.HANDLER
  CASE
    -- For NCT-manager: explicit allocation
    -- Carrying: direction-type of the end to be allocated
    --             (0 = client-end, 1 = server-end);
    --           share-type of the end to be allocated
    --             (1 = unshared, 2 = shared);
    --           share-type of the opposite end
    --             (0 = unknown, 1 = unshared, 2 = shared);
    --           type-hash of the NCT to be allocated;
    --           name of the NCT
    fornctmgr.alloc; INT; INT; INT; INT; MOBILE[] BYTE
    ----------------------------------------------------------------------
    -- For main kernel: explicit allocation - error
    -- Carrying: result
    formainkernel.alloc.error; INT
    ----------------------------------------------------------------------
    -- For main kernel: explicit allocation - confirm
    -- Carrying: NCT-ID of the newly allocated end
    formainkernel.alloc.confirm; INT
    ----------------------------------------------------------------------
    -- For NCT-manager: make CTB networked
    -- Carrying: NCT-ID of the decode-handler making the request;
    --           channel-ID of the decode-handler making the request;
    --           client-end initially claimed?;
    --           server-end initially claimed?
    fornctmgr.make.ctb.networked; INT; INT; BOOL; BOOL
    ----------------------------------------------------------------------
    -- For decode-handler: make CTB networked - confirm
    -- Carrying: NCT-ID of the recipient decode-handler;
    --           channel-ID of the recipient decode-handler;
    --           NCT-ID of the newly allocated end
    fordechandler.make.ctb.networked.confirm; INT; INT; INT
    ----------------------------------------------------------------------
    -- For NCT-handler: claim client-end
    -- Carrying: NCT-ID of the CTB-handler making the request
    forncthandler.claim.cli; INT
    ----------------------------------------------------------------------
    -- For NCT-handler: claim server-end
    -- Carrying: NCT-ID of the CTB-handler making the request
    forncthandler.claim.svr; INT
    ----------------------------------------------------------------------
    -- For NCT-handler: confirm new-other-end
    -- Carrying: NCT-ID of the CTB-handler making the request
    forncthandler.new.other.end.confirm; INT
    ----------------------------------------------------------------------
    -- For NCT-handler: release client-end
    -- Carrying: NCT-ID of the CTB-handler making the request
    forncthandler.release.cli; INT
    ----------------------------------------------------------------------
    -- For NCT-handler: release server-end
    -- Carrying: NCT-ID of the CTB-handler making the request
    forncthandler.release.svr; INT
    ----------------------------------------------------------------------
    -- For NCT-handler: end-over-itself operation completed
    -- Carrying: NCT-ID of the CTB-handler making the request
    forncthandler.eoi.done; INT
    ----------------------------------------------------------------------
    -- For CTB-handler regarding client-end: confirm claim - currently no
    --   other end claimed
    -- Carrying: NCT-ID of the recipient CTB-handler
    forctbhandler.reclient.claim.confirm.nootherend; INT
    ----------------------------------------------------------------------
    -- For CTB-handler regarding server-end: confirm claim - currently no
    --   other end claimed
    -- Carrying: NCT-ID of the recipient CTB-handler
    forctbhandler.reserver.claim.confirm.nootherend; INT
    ----------------------------------------------------------------------
    -- For CTB-handler regarding client-end: confirm claim - other end is
    --   claimed
    -- Carrying: NCT-ID of the recipient CTB-handler;
    --           node-ID of the other end's node
    forctbhandler.reclient.claim.confirm.otherendclaimed; INT; INT
    ----------------------------------------------------------------------
    -- For CTB-handler regarding server-end: confirm claim - other end is
    --   claimed
    -- Carrying: NCT-ID of the recipient CTB-handler;
    --           node-ID of the other end's node
    forctbhandler.reserver.claim.confirm.otherendclaimed; INT; INT
    ----------------------------------------------------------------------
    -- For CTB-handler regarding client-end: new other end (also used as a
    --   dummy for confirming a release request)
    -- Carrying: NCT-ID of the recipient CTB-handler;
    --           node-ID of the other end's node
    forctbhandler.reclient.new.other.end; INT; INT
    ----------------------------------------------------------------------
    -- For CTB-handler regarding server-end: new other end (also used as a
    --   dummy for confirming a release request)
    -- Carrying: NCT-ID of the recipient CTB-handler;
    --           node-ID of the other end's node
    forctbhandler.reserver.new.other.end; INT; INT
    ----------------------------------------------------------------------
    -- In session: start session
    -- Carrying: NCT-ID of the CTB-handler making the request
    insession.start.session; INT
    ----------------------------------------------------------------------
    -- In session: first CLC of a ULC
    -- Carrying: NCT-ID of the CTB-handler making the request;
    --           channel-ID of the relevant channel;
    --           number of data-item NLCs in first CLC;
    --           number of channel-type-end NLCs in first CLC;
    --           are there remaining CLCs?;
    --           address-array;
    --           size-array
    insession.first.clc; INT; INT; INT; INT; BOOL; MOBILE []INT; MOBILE []INT
    ----------------------------------------------------------------------
    -- In session: remaining CLCs of a ULC
    -- Carrying: NCT-ID of the CTB-handler making the request;
    --           channel-ID of the relevant channel;
    --           number of data-item NLCs in remaining CLCs;
    --           number of channel-type-end NLCs in remaining CLCs;
    --           address-array;
    --           size-array
    insession.rest.clcs; INT; INT; INT; INT; MOBILE []INT; MOBILE []INT
    ----------------------------------------------------------------------
    -- In session: acknowledge CLC
    -- Carrying: NCT-ID of the CTB-handler making the request;
    --           channel-ID of the relevant channel
    insession.ack; INT; INT
    ----------------------------------------------------------------------
    -- In session: cancel CLC
    -- Carrying: NCT-ID of the CTB-handler making the request;
    --           channel-ID of the relevant channel
    insession.cancel; INT; INT
    ----------------------------------------------------------------------
    -- In session: close session
    -- Carrying: NCT-ID of the CTB-handler making the request
    insession.close.session; INT
    ----------------------------------------------------------------------
    -- In session: suspend session
    -- Carrying: NCT-ID of the CTB-handler making the request
    insession.suspend.session; INT
    ----------------------------------------------------------------------
    -- In session: confirm `suspend.session'
    -- Carrying: NCT-ID of the CTB-handler making the request
    insession.suspend.session.confirm; INT
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:
--}}}
--{{{  CHAN TYPE PONY.INT.LINKHANDLE
-- Link-handle (link-handler has server-end, client-end available via
--   link-manager)
CHAN TYPE PONY.INT.LINKHANDLE
  MOBILE RECORD
    CHAN PONY.INT.LINKHANDLE.TO.HANDLER to.handler?:
:
--}}}

--{{{  PROTOCOLs PONY.INT.LINKMGRHANDLE.TO.MGR and PONY.INT.LINKMGRHANDLE.FROM.MGR
-- To link-manager
PROTOCOL PONY.INT.LINKMGRHANDLE.TO.MGR
  CASE
    -- Get link-handle for a given node-ID
    -- Carrying: node-ID
    get.link.handle; INT
    ----------------------------------------------------------------------
    -- A link-handler was shut down
    -- Carrying: remote node-ID of the link-handler
    link.handler.shutdown; INT
    ----------------------------------------------------------------------
    -- Shutdown
    shutdown
:

-- From link-manager
PROTOCOL PONY.INT.LINKMGRHANDLE.FROM.MGR
  CASE
    -- Get link-handle - confirm
    -- Carrying: link-handle
    get.link.handle.confirm; SHARED PONY.INT.LINKHANDLE!
:
--}}}
--{{{  CHAN TYPE PONY.INT.LINKMGRHANDLE
-- Link-manager-handle (link-manager has server-end)
CHAN TYPE PONY.INT.LINKMGRHANDLE
  MOBILE RECORD
    CHAN PONY.INT.LINKMGRHANDLE.TO.MGR to.mgr?:
    CHAN PONY.INT.LINKMGRHANDLE.FROM.MGR from.mgr!:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOL PONY.INT.KERNELREPLYHANDLE.TO.HANDLER
-- To main kernel
PROTOCOL PONY.INT.KERNELREPLYHANDLE.TO.HANDLER
  CASE
    -- Explicit allocation - error
    -- Carrying: result
    alloc.error; INT
    ----------------------------------------------------------------------
    -- Explicit allocation - confirm
    -- Carrying: NCT-ID of the newly allocated end
    alloc.confirm; INT
    ----------------------------------------------------------------------
    -- Shutdown CTB-manager - confirm
    -- Carrying: array of formerly networked CTB-pointers
    fromctbmgr.shutdown.confirm; MOBILE []INT
    ----------------------------------------------------------------------
    -- Shutdown link-manager - confirm
    fromlinkmgr.shutdown.confirm
:
--}}}
--{{{  CHAN TYPE PONY.INT.KERNELREPLYHANDLE
-- Kernel-reply-handle (main kernel has server-end)
CHAN TYPE PONY.INT.KERNELREPLYHANDLE
  MOBILE RECORD
    CHAN PONY.INT.KERNELREPLYHANDLE.TO.HANDLER to.kernel?:
:
--}}}

--}}}

