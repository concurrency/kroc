
VAL INT CLOCK.STEP IS 1:
DATA TYPE ADDR IS INT:
DATA TYPE IPTR IS INT:

DATA TYPE VM.STATE
  PACKED RECORD
    INT        state:
    [3]INT     stack:
    [4]BYTE    type:
    INT        oreg:
    ADDR       wptr:
    IPTR       iptr:
    INT        icount:
    INT        eflags:
:

PROTOCOL P.VM.CTL.RQ
  CASE
    run            = 0 ; INT       -- run until for N instructions or until breakpoint
    step           = 1             -- step traced instruction
    dispatch       = 2 ; INT; INT  -- dispatch an arbitrary instruction, with argument
    set.bp         = 3 ; IPTR      -- set break point
    clear.bp       = 4 ; IPTR      -- clear break point
    get.clock      = 5             -- get clock details
    set.clock      = 6 ; INT; INT  -- set clock type and frequency
    trace          = 7 ; INT; BOOL -- enable/disable trace type (instruction)
    get.state      = 8             -- get VM state
    set.state      = 9 ; VM.STATE  -- set VM state
    read.word      = 10; ADDR      -- read word at address
    read.byte      = 11; ADDR      -- read byte at address
    read.int16     = 12; ADDR      -- read int16 at address
    read.type      = 13; ADDR      -- read type of memory at address
    return.param   = 14; INT       -- release parameter N 
    set.param.chan = 15; INT; MOBILE.CHAN
                                   -- set parameter N to channel
:
PROTOCOL P.VM.CTL.RE
  CASE
    decoded        = 0 ; IPTR; INT; INT
                                   -- new IPTR, instruction, arg
    dispatched     = 1 ; IPTR; ADDR
                                   -- new IPTR and WPTR
    bp             = 2 ; IPTR      -- break pointer IPTR reached
    clock          = 3 ; INT; INT  -- clock type and frequency
    ok             = 4
    error          = 5 ; INT
    state          = 6 ; VM.STATE
    word           = 7 ; INT
    byte           = 8 ; BYTE
    int16          = 9 ; INT16
    type           = 10; INT
    channel        = 11; MOBILE.CHAN
:
CHAN TYPE CT.VM.CTL
  MOBILE RECORD
    CHAN P.VM.CTL.RQ request?:
    CHAN P.VM.CTL.RE response!:
:


PROTOCOL P.BYTECODE.RQ
  CASE
    create.vm      = 0
    get.symbol     = 1; MOBILE []BYTE   -- look up symbol name
    get.symbol.at  = 2; IPTR            -- look up symbol at bytecode offset
    get.file       = 3; INT             -- translate file number to name
    get.line.info  = 4; IPTR            -- get file/line number of address
    get.details    = 5                  -- get bytecode details
    get.tlp        = 6                  -- get top-level-process details
:
PROTOCOL P.BYTECODE.RE
  CASE
    vm             = 0; CT.VM.CTL!
    error          = 1; INT
    file           = 2; MOBILE []BYTE
    line.info      = 3; INT; INT       -- file, line
    symbol         = 4; IPTR; MOBILE []BYTE; MOBILE []BYTE; INT; INT
                        -- offset, name, definition, ws, vs
    details        = 5; INT; INT; INT  -- ws, vs, length
    tlp            = 6; MOBILE []BYTE; MOBILE []BYTE
:
CHAN TYPE CT.BYTECODE
  MOBILE RECORD
    CHAN P.BYTECODE.RQ request?:
    CHAN P.BYTECODE.RE response!:
:


PROTOCOL P.VM.RQ
  CASE
    decode.bytecode = 0; MOBILE []BYTE -- TBC data
    load.bytecode   = 1; MOBILE []BYTE -- resource path
:
PROTOCOL P.VM.RE
  CASE
    bytecode        = 0; CT.BYTECODE!
    error           = 1; INT
:
CHAN TYPE CT.VM
  MOBILE RECORD
    CHAN P.VM.RQ request?:
    CHAN P.VM.RE response!:
:
