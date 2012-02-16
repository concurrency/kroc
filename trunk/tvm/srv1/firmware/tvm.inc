#IF NOT DEFINED (TVM.INC)
#DEFINE TVM.INC

DATA TYPE ECTX
  PACKED RECORD
    INT   iptr:
    
    INT   oreg:
    INT   areg:
    INT   breg:
    INT   creg:

    INT   pri:

    INT   wptr:
    INT   fptr:
    INT   bptr:
    INT   tptr:
    INT   tnext:

    INT   saved.creg:

    INT   eflags:
    INT   state:
:

VAL INT EFLAG.SETERR          IS (1 << 0):
VAL INT EFLAG.INTERR          IS (1 << 1):
VAL INT EFLAG.INTOV           IS (1 << 2):
VAL INT EFLAG.FP              IS (1 << 3):
VAL INT EFLAG.ALT             IS (1 << 4):
VAL INT EFLAG.FFI             IS (1 << 5):
VAL INT EFLAG.CHAN            IS (1 << 6):
VAL INT EFLAG.EXTCHAN         IS (1 << 7):
VAL INT EFLAG.BAR             IS (1 << 8):
VAL INT EFLAG.DMEM            IS (1 << 9):
VAL INT EFLAG.MT              IS (1 << 10):
VAL INT EFLAG.PROC            IS (1 << 11):

VAL INT ECTX.CONTINUE         IS 0:
VAL INT ECTX.INIT             IS 'i':
VAL INT ECTX.INS.INVALID      IS 'I':
VAL INT ECTX.INS.UNSUPPORTED  IS 'U':
VAL INT ECTX.INTERRUPT        IS 'x':
VAL INT ECTX.EMPTY            IS 'e':
VAL INT ECTX.ERROR            IS 'E':
VAL INT ECTX.PREEMPT          IS 'p':
VAL INT ECTX.RUNNING          IS 'r':
VAL INT ECTX.SHUTDOWN         IS 'S':
VAL INT ECTX.SLEEP            IS 's':
VAL INT ECTX.TIME.SLICE       IS 't':

#ENDIF -- !TVM.INC
