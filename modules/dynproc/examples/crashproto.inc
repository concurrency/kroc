
-- used by crashlib.occ/testcrash.occ to create run-time errors

PROTOCOL CRASHPROTO
  CASE
    empty1
    empty2		-- deliberately unhandled
    array.bounds	-- generate an array-bounds error
    int.overflow	-- generates an overflow
    fpu.error		-- generates an FPU error
:


