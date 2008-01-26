-- Copyright: Mario Schweigler (C) 2005-2006
-- Institution: Computing Laboratory, University of Kent, Canterbury, UK
-- Description: pony public include file

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
-- Message-types
VAL INT PONYC.MSGTYPE.STATUS IS 0:
VAL INT PONYC.MSGTYPE.ERR IS 1:
VAL INT PONYC.MSGTYPE.STATUSERR IS 2:

-- Network-types
VAL INT PONYC.NETTYPE.TCPIP IS 0:

-- Node-types
VAL INT PONYC.NODETYPE.MASTER IS 0:
VAL INT PONYC.NODETYPE.MASTERRESET IS 1:
VAL INT PONYC.NODETYPE.SLAVE IS 2:
VAL INT PONYC.NODETYPE.SLAVEWAIT IS 3:

-- Share-types
VAL INT PONYC.SHARETYPE.UNKNOWN IS 0:
VAL INT PONYC.SHARETYPE.UNSHARED IS 1:
VAL INT PONYC.SHARETYPE.SHARED IS 2:

--{{{  Results for startup (non-network-type-specific)
-- OK
VAL INT PONYC.RESULT.STARTUP.OK IS 0:
-- Illegal message-type
VAL INT PONYC.RESULT.STARTUP.ILLEGALMSGTYPE IS -1:
-- Illegal network-type
VAL INT PONYC.RESULT.STARTUP.ILLEGALNETTYPE IS -2:
--}}}

--{{{  Results for explicit allocation
-- OK
VAL INT PONYC.RESULT.ALLOC.OK IS 0:
-- Illegal NCT-name (not allowed: empty string)
VAL INT PONYC.RESULT.ALLOC.ILLEGALNCTNAME IS -1:
-- Type-hash mismatch
VAL INT PONYC.RESULT.ALLOC.CHANTYPEMISMATCH IS -2:
-- x2x type mismatch
VAL INT PONYC.RESULT.ALLOC.X2XTYPEMISMATCH IS -3:
-- Mismatch of x2x count (i.e. trying to allocate more than one
--   `one2x' client-end or more than one `x2one' server-end)
VAL INT PONYC.RESULT.ALLOC.X2XCOUNTMISMATCH IS -4:
--}}}

--{{{  Results for error-handling

--{{{  Results for `pony.err.get.nct.id.*'
-- OK
VAL INT PONYC.RESULT.ERR.GNI.OK IS 0:
-- Channel-type-end undefined
VAL INT PONYC.RESULT.ERR.GNI.CTENDUNDEFINED IS -1:
-- Channel-type-end not networked
VAL INT PONYC.RESULT.ERR.GNI.CTENDNOTNETWORKED IS -2:
--}}}
--{{{  Results for `pony.err.get.current.remote.node'
-- OK
VAL INT PONYC.RESULT.ERR.GCRN.OK IS 0:
-- Invalid NCT-ID
VAL INT PONYC.RESULT.ERR.GCRN.INVALIDNCTID IS -1:
-- Currently no session
VAL INT PONYC.RESULT.ERR.GCRN.NOSESSION IS -2:
-- Currently both ends on the same node
VAL INT PONYC.RESULT.ERR.GCRN.SAMENODE IS -3:
--}}}
--{{{  Results for `pony.err.delete.error.point'
-- OK
VAL INT PONYC.RESULT.ERR.DEP.OK IS 0:
-- Invalid error-point
VAL INT PONYC.RESULT.ERR.DEP.INVALIDERRPOINT IS -1:
--}}}
--{{{  Results for `pony.err.get.errors.after'
-- OK
VAL INT PONYC.RESULT.ERR.GEA.OK IS 0:
-- Invalid error-point
VAL INT PONYC.RESULT.ERR.GEA.INVALIDERRPOINT IS -1:
--}}}

--}}}

--}}}
--{{{  Network-type-specific constants (include files for all supported network-types)
#INCLUDE "ponylibtcpip.inc"
--}}}
--{{{  Declarations

--{{{  DATA TYPE PONY.ERROR
DATA TYPE PONY.ERROR
  RECORD
    BOOL ans.concerned:
    BOOL master.concerned:
    BOOL remote.node.concerned:
    INT remote.node.id:
    INT err.code:
:
--}}}

----------------------------------------------------------------------

--{{{  Forward declarations
CHAN TYPE PONY.DECODEHANDLE:
CHAN TYPE PONY.ENCODEHANDLE:
CHAN TYPE PONY.NETHOOKHANDLE:
--}}}

------------------------------------------------------------

--{{{  PROTOCOLs PONY.DECODEHANDLE.TO.DECODER and PONY.DECODEHANDLE.FROM.DECODER
-- To protocol-decoder
PROTOCOL PONY.DECODEHANDLE.TO.DECODER
  CASE
    -- Activate DECODE.CHANNEL
    activate = 0
    ----------------------------------------------------------------------
    -- Make CTB networked - confirm
    -- Carrying: NCT-ID;
    --           network-hook-handle;
    --           array of decode-handles;
    --           array of encode-handles
    make.ctb.networked.confirm = 1; INT; PONY.NETHOOKHANDLE!;
                                    MOBILE []PONY.DECODEHANDLE!;
                                    MOBILE []PONY.ENCODEHANDLE!
    ----------------------------------------------------------------------
    -- First CLC or remaining CLCs have been taken by remote user-level
    --   process
    ack = 2
    ----------------------------------------------------------------------
    -- Deactivate protocol-decoder (and cancel the current ULC if applicable -
    --   do NOT release outputting user-level process if engaged in ULC)
    cancel = 3
    ----------------------------------------------------------------------
    -- Read and cancel the first CLC of a ULC that the matching
    --   protocol-encoder tries to send
    cancel.encode = 4
    ----------------------------------------------------------------------
    -- Acknowledgement from protocol-encoder has been received
    cancel.encode.ack = 5
    ----------------------------------------------------------------------
    -- Terminate
    term = 6
:

-- From protocol-decoder
PROTOCOL PONY.DECODEHANDLE.FROM.DECODER
  CASE
    -- First CLC of next ULC
    -- Carrying: number of data-item NLCs in first CLC;
    --           number of channel-type-end NLCs in first CLC;
    --           are there remaining CLCs?
    first.clc = 0; INT; INT; BOOL
    ----------------------------------------------------------------------
    -- Remaining CLCs of ULC
    -- Carrying: number of data-item NLCs in remaining CLCs;
    --           number of channel-type-end NLCs in remaining CLCs
    rest.clcs = 1; INT; INT
    ----------------------------------------------------------------------
    -- Data-item NLC
    -- Carrying: address;
    --           size
    data.item.nlc = 2; INT; INT
    ----------------------------------------------------------------------
    -- Make CTB networked
    -- Carrying: CTB-pointer;
    --           client-state (1 = unshared,
    --                         2 = shared/unclaimed, 3 = shared/claimed);
    --           server-state (1 = unshared,
    --                         2 = shared/unclaimed, 3 = shared/claimed);
    --           number of channel-words;
    --           number of reading-ends in server-end
    make.ctb.networked = 3; INT; INT; INT; INT; INT
    ----------------------------------------------------------------------
    -- Channel-type-end NLC
    -- Carrying: NCT-ID;
    --           direction-type of the end to be sent
    --             (0 = client-end, 1 = server-end);
    --           share-type of the end to be sent
    --             (1 = unshared, 2 = shared)
    ct.end.nlc = 4; INT; INT; INT
    ----------------------------------------------------------------------
    -- Confirm completion of `cancel' operation
    cancel.confirm = 5
    ----------------------------------------------------------------------
    -- CLC from protocol-encoder was cancelled
    encode.cancelled = 6
    ----------------------------------------------------------------------
    -- CLC from protocol-encoder was not cancelled
    encode.not.cancelled = 7
:
--}}}
--{{{  CHAN TYPE PONY.DECODEHANDLE
-- Decode-handle (decode-handler has server-end, decoder has client-end)
CHAN TYPE PONY.DECODEHANDLE
  MOBILE RECORD
    CHAN PONY.DECODEHANDLE.TO.DECODER to.decoder!:
    CHAN PONY.DECODEHANDLE.FROM.DECODER from.decoder?:
:
--}}}

------------------------------------------------------------

--{{{  PROTOCOLs PONY.ENCODEHANDLE.TO.ENCODER and PONY.ENCODEHANDLE.FROM.ENCODER
-- To protocol-encoder
PROTOCOL PONY.ENCODEHANDLE.TO.ENCODER
  CASE
    -- First CLC of next ULC
    -- Carrying: number of data-item NLCs in first CLC;
    --           number of channel-type-end NLCs in first CLC;
    --           are there remaining CLCs?
    first.clc = 0; INT; INT; BOOL
    ----------------------------------------------------------------------
    -- Remaining CLCs of ULC
    -- Carrying: number of data-item NLCs in remaining CLCs;
    --           number of channel-type-end NLCs in remaining CLCs
    rest.clcs = 1; INT; INT
    ----------------------------------------------------------------------
    -- Data-item NLC
    -- Carrying: data
    data.item.nlc = 2; MOBILE []BYTE
    ----------------------------------------------------------------------
    -- Increase reference-count of CTB
    -- Carrying: CTB-pointer
    clone.ctb = 3; INT
    ----------------------------------------------------------------------
    -- Allocate new CTB
    -- Carrying: NCT-ID
    alloc.new.ctb = 4; INT
    ----------------------------------------------------------------------
    -- Allocate new CTB - confirm
    -- Carrying: network-hook-handle;
    --           array of decode-handles;
    --           array of encode-handles
    alloc.new.ctb.confirm = 5; PONY.NETHOOKHANDLE!;
                               MOBILE []PONY.DECODEHANDLE!;
                               MOBILE []PONY.ENCODEHANDLE!
    ----------------------------------------------------------------------
    -- Cancel the current ULC (i.e. if first CLC was cancelled by matching
    --   protocol-decoder, do not deal with remaining CLCs)
    cancel = 6
    ----------------------------------------------------------------------
    -- Terminate
    term = 7
:

-- From protocol-encoder
PROTOCOL PONY.ENCODEHANDLE.FROM.ENCODER
  CASE
    -- Allocate new CTB - confirm
    -- Carrying: CTB-pointer;
    --           number of channel-words in CTB;
    --           number of reading-ends in server-end
    alloc.new.ctb.confirm = 0; INT; INT; INT
    ----------------------------------------------------------------------
    -- First CLC or remaining CLCs have been taken by user-level process
    --   (or by cancelling protocol-decoder)
    ack = 1
:
--}}}
--{{{  CHAN TYPE PONY.ENCODEHANDLE
-- Encode-handle (encode-handler has server-end, encoder has client-end)
CHAN TYPE PONY.ENCODEHANDLE
  MOBILE RECORD
    CHAN PONY.ENCODEHANDLE.TO.ENCODER to.encoder!:
    CHAN PONY.ENCODEHANDLE.FROM.ENCODER from.encoder?:
:
--}}}

------------------------------------------------------------

--{{{  CHAN TYPE PONY.NETHOOKHANDLE
-- Used to claim/release ends of a CTB (CTB-handler has server-end,
--   channel-words plugged into the CTB's network-hook)
CHAN TYPE PONY.NETHOOKHANDLE
  MOBILE RECORD
    -- Client claim/release request
    CHAN INT client?:
    -- Server claim/release request
    CHAN INT server?:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOLs PONY.NETHANDLE.TO.KERNEL and PONY.NETHANDLE.FROM.KERNEL
-- To pony kernel
PROTOCOL PONY.NETHANDLE.TO.KERNEL
  CASE
    -- Explicit allocation
    -- Carrying: direction-type of the end to be allocated
    --             (0 = client-end, 1 = server-end);
    --           share-type of the end to be allocated
    --             (1 = unshared, 2 = shared);
    --           share-type of the opposite end
    --             (0 = unknown, 1 = unshared, 2 = shared);
    --           number of channel-words;
    --           number of reading-ends in server-end;
    --           type-hash of the NCT to be allocated;
    --           name of the NCT
    alloc; INT; INT; INT; INT; INT; INT; MOBILE[] BYTE
    ----------------------------------------------------------------------
    -- Allocate new CTB - confirm
    -- Carrying: CTB-pointer
    alloc.new.ctb.confirm; INT
    ----------------------------------------------------------------------
    -- Shut down pony kernel
    shutdown
:

-- From pony kernel
PROTOCOL PONY.NETHANDLE.FROM.KERNEL
  CASE
    -- Explicit allocation - error
    -- Carrying: result
    alloc.error; INT
    ----------------------------------------------------------------------
    -- Increase reference-count of CTB
    -- Carrying: CTB-pointer
    clone.ctb; INT
    ----------------------------------------------------------------------
    -- Allocate new CTB
    -- Carrying: NCT-ID;
    --           network-hook-handle;
    --           array of decode-handles;
    --           array of encode-handles
    alloc.new.ctb; INT; PONY.NETHOOKHANDLE!;
                        MOBILE []PONY.DECODEHANDLE!;
                        MOBILE []PONY.ENCODEHANDLE!
    ----------------------------------------------------------------------
    -- Shut down pony kernel - confirm
    -- Carrying: array of formerly networked CTB-pointers
    shutdown.confirm; MOBILE []INT
:
--}}}
--{{{  CHAN TYPE PONY.NETHANDLE
-- Network-handle (pony kernel has server-end)
CHAN TYPE PONY.NETHANDLE
  MOBILE RECORD
    CHAN PONY.NETHANDLE.TO.KERNEL to.kernel?:
    CHAN PONY.NETHANDLE.FROM.KERNEL from.kernel!:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOLs PONY.ERRHANDLE.TO.HANDLER and PONY.ERRHANDLE.FROM.HANDLER
-- To error-handler
PROTOCOL PONY.ERRHANDLE.TO.HANDLER
  CASE
    -- Get ID of current remote node for a given NCT
    -- Carrying: NCT-ID
    get.current.remote.node; INT
    ----------------------------------------------------------------------
    -- Get new error-point
    new.err.point
    ----------------------------------------------------------------------
    -- Delete error-point
    -- Carrying: error-point
    delete.err.point; INT
    ----------------------------------------------------------------------
    -- Get errors after a given error-point
    -- Carrying: error-point;
    --           check ANS?;
    --           check master?;
    --           check all nodes?;
    --           array of nodes to check
    get.errs.after; INT; BOOL; BOOL; BOOL; MOBILE []INT
    ----------------------------------------------------------------------
    -- Shut down error-handler
    shutdown
:

-- From error-handler
PROTOCOL PONY.ERRHANDLE.FROM.HANDLER
  CASE
    -- Get ID of current remote node - confirm
    -- Carrying: ID of remote node;
    --           result
    get.current.remote.node.confirm; INT; INT
    ----------------------------------------------------------------------
    -- Get new error-point - confirm
    -- Carrying: error-point
    new.err.point.confirm; INT
    ----------------------------------------------------------------------
    -- Delete error-point - confirm
    -- Carrying: result
    delete.err.point.confirm; INT
    ----------------------------------------------------------------------
    -- Get errors after a given error-point - confirm
    -- Carrying: array of errors;
    --           new error-point;
    --           result
    get.errs.after.confirm; MOBILE []PONY.ERROR; INT; INT
:
--}}}
--{{{  CHAN TYPE PONY.ERRHANDLE
-- Error-handle (error-handler has server-end)
CHAN TYPE PONY.ERRHANDLE
  MOBILE RECORD
    CHAN PONY.ERRHANDLE.TO.HANDLER to.handler?:
    CHAN PONY.ERRHANDLE.FROM.HANDLER from.handler!:
:
--}}}

----------------------------------------------------------------------

--{{{  PROTOCOLs PONY.MSGHANDLE.TO.HANDLER and PONY.MSGHANDLE.FROM.HANDLER
-- To message-handler
PROTOCOL PONY.MSGHANDLE.TO.HANDLER
  CASE
    -- Request initialisation
    init
    -- Get next message
    get.msg
:

-- From message-handler
PROTOCOL PONY.MSGHANDLE.FROM.HANDLER
  CASE
    -- Request initialisation - confirm
    -- Carrying: own node-ID;
    --           message-type
    init.confirm; INT; INT
    ----------------------------------------------------------------------
    -- Get next message - confirm
    -- Carrying: message;
    --           error-flag
    get.msg.confirm; MOBILE []BYTE; BOOL
    ----------------------------------------------------------------------
    -- Message-handler was shut down
    shutdown
:
--}}}
--{{{  CHAN TYPE PONY.MSGHANDLE
-- Message-handle (message-handler has server-end)
CHAN TYPE PONY.MSGHANDLE
  MOBILE RECORD
    CHAN PONY.MSGHANDLE.TO.HANDLER to.handler?:
    CHAN PONY.MSGHANDLE.FROM.HANDLER from.handler!:
:
--}}}

--}}}

