--*{{{  SELECT Events to select upon.
VAL INT SELECT.READ   IS 1:         --* A read will not block.
VAL INT SELECT.WRITE  IS 2:         --* A write will not block.
--*}}}
--{{{  CHAN TYPE SELECTOR.CT
PROTOCOL SELECTOR.SIGNAL.REQ
  CASE
    --* The FD has become ready.
    ready = 0
    --* The selector is shutting down.
    shutdown = 1
:

PROTOCOL SELECTOR.SIGNAL.RESP
  CASE
    --* Wait for the FD again.
    again = 0
    --* Remove the FD from the waiting set.
    remove = 1
:

--* Channel bundle used for the selector to signal back to the client.
--
-- This may look dodgy from a C-S perspective, but it should be OK: at any
-- given time, the client-server relationship is only going one way...
CHAN TYPE SELECTOR.SIGNAL
  MOBILE RECORD
    CHAN SELECTOR.SIGNAL.REQ signal?:
    CHAN SELECTOR.SIGNAL.RESP ack!:
:

PROTOCOL SELECTOR.REQ
  CASE
    --* Add a file descriptor to the set being waited upon.
    -- @item fd File descriptor (usually non-blocking)
    -- @item events Bitwise OR of events to wait for; see [@ref SELECT]
    -- @item signal Channel bundle to signal over when the event fires
    add = 0; INT; INT; SELECTOR.SIGNAL!
    --* Shut down the selector.
    shutdown = 1
:

CHAN TYPE SELECTOR.CT
  MOBILE RECORD
    CHAN SELECTOR.REQ req?:
:
--}}}
