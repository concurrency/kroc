PROTOCOL DISPLAY
  CASE
    thinking; BYTE            -- phil.id
    wanna.read; BYTE          -- phil.id
    reading; BYTE             -- phil.id
    done.read; BYTE; BYTE     -- phil.id; what.was.read
    wanna.write; BYTE; BYTE   -- phil.id; what.we.wanna.write
    writing; BYTE; BYTE       -- phil.id; what.we.are.writing
    done.write; BYTE; BYTE    -- phil.id; what.was.written
    time; INT                 -- the time
    control                   -- philosopher parameters have changed
    special; BYTE; INT        -- reserved
:
