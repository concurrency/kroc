--** Higher-level bindings to the Player library.
--
-- @module player

-- SYMBOLIC CONSTANTS
--VAL INT num.samples IS PLAYER.LASER.MAX.SAMPLES:
VAL INT num.laser.samples IS 181:
VAL INT num.sonar.samples IS 16:
VAL INT poll.freq.in.hz IS 10:
VAL INT seconds IS 1000000:
VAL INT second  IS 1000000:
VAL INT tenths  IS 100000:
VAL INT default.player.port IS 6665:
VAL []BYTE default.player.host IS "localhost":
VAL REAL32 PI IS 3.14159265535897:
VAL INT MAX.HOST.NAME.LENGTH IS 1024:

-- Data Types
PROTOCOL MOTORS IS INT ; INT ; INT:
DATA TYPE LASER IS [num.laser.samples]INT:
DATA TYPE SONAR IS [num.sonar.samples]INT:
