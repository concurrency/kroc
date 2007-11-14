--MySQL error numbers
VAL INT cr.server.gone.error IS 2006:
VAL INT cr.server.lost IS 2013:

DATA TYPE MYSQL.TYPE IS BYTE:
VAL MYSQL.TYPE tiny       IS  0:
VAL MYSQL.TYPE short      IS  1:
VAL MYSQL.TYPE long       IS  2:
VAL MYSQL.TYPE int24      IS  3:
VAL MYSQL.TYPE longlong   IS  4:
VAL MYSQL.TYPE decimal    IS  5:
VAL MYSQL.TYPE newdecimal IS  6:
VAL MYSQL.TYPE float      IS  7:
VAL MYSQL.TYPE double     IS  8:
VAL MYSQL.TYPE bit        IS  9:
VAL MYSQL.TYPE timestamp  IS 10:
VAL MYSQL.TYPE date       IS 12:
VAL MYSQL.TYPE time       IS 13:
VAL MYSQL.TYPE datetime   IS 14:
VAL MYSQL.TYPE year       IS 15:
VAL MYSQL.TYPE string     IS 16:
VAL MYSQL.TYPE var.string IS 17:
VAL MYSQL.TYPE blob       IS 18:
VAL MYSQL.TYPE set        IS 19:
VAL MYSQL.TYPE enum       IS 20:
VAL MYSQL.TYPE geometry   IS 21:
VAL MYSQL.TYPE null       IS 22:

--control codes for the field.info message
VAL BYTE field.name      IS 0:
VAL BYTE field.org.name  IS 1:
VAL BYTE field.table     IS 2:
VAL BYTE field.org.table IS 3:
VAL BYTE field.db        IS 4:
VAL BYTE field.info      IS 5:

DATA TYPE MYSQL.FIELD.INFO
  RECORD
    INT length, max.length, decimals, charsetnr:
    MYSQL.TYPE type:
    BOOL not.null, pri.key, unique.key:
    BOOL multiple.key, unsigned, zerofill, binary:
    BOOL auto.increment, binary.charset:
:

PROTOCOL MYSQL.CONTROL
  CASE
    query; MOBILE []BYTE
    query.row; MOBILE []BYTE
    query.all; MOBILE []BYTE
    next.row
    remaining.rows
    affected.rows
    null.values
    field.names
    field.info; BYTE
    reconnect
    quit
:

PROTOCOL MYSQL.RESULT
  CASE
    mysql.error; INT; MOBILE []BYTE 
    initialisation.error
    no.data.error
    not.connected.error
    connected 
    data.ready; INT 
    data.row; MOBILE []MOBILE []BYTE
    data.null.values; MOBILE []BOOL
    data.field.names; MOBILE []MOBILE []BYTE
    data.field.info; MOBILE []MYSQL.FIELD.INFO
    data.affected.rows; INT
    end.of.data
    quit
:

