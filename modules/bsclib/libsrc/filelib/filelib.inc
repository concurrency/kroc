--
--	filelib.inc - include file for file library
--	Copyright (C) 2000-2003 Fred Barnes <frmb2@ukc.ac.uk>
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

--** @module file

--*{{{  ACCESS Constants for [@ref file.check.access].
VAL R.OK IS 4:            --* Check read permission
VAL W.OK IS 2:            --* Check write permission
VAL X.OK IS 1:            --* Check execute permission
VAL F.OK IS 0:            --* Check existance
--*}}}
--*{{{  OPEN.MODE Constants for [@ref file.open].

--* Open for reading
VAL O.RDONLY IS #00000000:
--* Open for writing
VAL O.WRONLY IS #00000001:
--* Open for reading and writing
VAL O.RDWR IS #00000002:
--* Create if not already present
VAL O.CREAT IS #00000040:
--* Fail if [@ref O.CREAT] is given and the file already exists
VAL O.EXCL IS #00000080:
--* Don't become controlling terminal
VAL O.NOCTTY IS #00000100:
--* Truncate to zero length after opening
VAL O.TRUNC IS #00000200:
--* Position file pointer at EOF before each write
VAL O.APPEND IS #00000400:
--* Make operations on this FD not block
VAL O.NONBLOCK IS #00000800:
--* Synonym for [@ref O.NONBLOCK]
-- @deprecated
VAL O.NDELAY IS O.NONBLOCK:
--* Open for synchronous IO
VAL O.SYNC IS #00001000:
--* Generate a signal when IO becomes possible
VAL O.ASYNC IS #00002000:
--* Don't cache data for this file
VAL O.DIRECT IS #00004000:
--* Allow the file to be larger than 2GB
VAL O.LARGEFILE IS #00008000:
--* Fail if not opening a directory
VAL O.DIRECTORY IS #00010000:
--* Don't follow symbolic links when opening
VAL O.NOFOLLOW IS #00020000:
--*}}}  
--*{{{  SEEK Constants for [@ref file.seek].
VAL SEEK.SET IS 0:        --* Relative to start of file
VAL SEEK.CUR IS 1:        --* Relative to current location
VAL SEEK.END IS 2:        --* Relative to end of file
--*}}}  
--*{{{  FCNTL Constants for [@ref file.fcntl0] and [@ref file.fcntl1].
VAL F.DUPFD IS 0:         --* Duplicate file descriptor
VAL F.GETFD IS 1:         --* Read the close-on-exec flag
VAL F.SETFD IS 2:         --* Set the close-on-exec flag
--* Get file descriptor flags.
-- See [@ref OPEN.MODE].
VAL F.GETFL IS 3:
--* Set file descriptor flags.
-- See [@ref OPEN.MODE].
VAL F.SETFL IS 4:
--* Get [@text flock] structure.
-- Not fully supported.
VAL F.GETLK IS 5:
--* Set file lock.
-- Not fully supported.
VAL F.SETLK IS 6:
--* Set file lock with wait.
-- Not fully supported.
VAL F.SETLKW IS 7:

--* Set process ID that will receive [@text SIGIO] and [@text SIGURG] signals.
-- Use a negative ID for a process group.
-- Not fully supported.
VAL F.SETOWN IS 8:
--* Get process ID receiving SIGIO and SIGURG signals.
-- Returns a negative ID for a process group.
-- Not fully supported.
VAL F.GETOWN IS 9:
--* Get signal sent when I/O becomes available on the descriptor.
-- [@code 0] means [@text SIGIO]. [@text SA_SIGINFO] may be available.
-- Not fully supported.
VAL F.SETSIG IS 10:
--* Set signal sent when I/O becomes available on the descriptor.
-- [@code 0] means [@text SIGIO]. [@text SA_SIGINFO] may be available.
-- Not fully supported.
VAL F.GETSIG IS 11:

--* Get [@text flock64] structure.
-- Not fully supported.
VAL F.GETLK64 IS 12:
--* Set 64-bit file lock.
-- Not fully supported.
VAL F.SETLK64 IS 13:
--* Set 64-bit file lock with wait.
-- Not fully supported.
VAL F.SETLKW64 IS 14:

VAL FD.CLOEXEC IS 1:      --* Close-on-exec flag for [@ref F.GETFD]/[@ref F.SETFD]
VAL F.RDLCK IS 0:         --* Obtain read lock
VAL F.WRLCK IS 1:         --* Obtain write lock
VAL F.UNLCK IS 2:         --* Release lock

VAL F.INPROGRESS IS 16:   -- for leases
--*}}}
--*{{{  FILE.MODE Constants for file modes.
VAL S.IRWXU IS #1C0:      --* User may read, write and execute
VAL S.IRUSR IS #100:      --* User may read
VAL S.IWUSR IS #80:       --* User may write
VAL S.IXUSR IS #40:       --* User may execute
VAL S.IRWXG IS #38:       --* Group may read, write and execute
VAL S.IRGRP IS #20:       --* Group may read
VAL S.IWGRP IS #10:       --* Group may write
VAL S.IXGRP IS #8:        --* Group may execute
VAL S.IRWXO IS #7:        --* Others may read, write and execute
VAL S.IROTH IS #4:        --* Others may read
VAL S.IWOTH IS #2:        --* Others may write
VAL S.IXOTH IS #1:        --* Others may execute
VAL S.I644 IS #1A4:       --* User may write; all may read
VAL S.I755 IS #1ED:       --* User may write; all may read and execute
VAL INT32 S.IFMT   IS     #F000: --* Bitmask for the file type bitfields
VAL INT32 S.IFSOCK IS     #C000: --* Socket
VAL INT32 S.IFLNK  IS     #A000: --* Symbolic link
VAL INT32 S.IFREG  IS     #8000: --* Regular file
VAL INT32 S.IFBLK  IS     #6000: --* Block device
VAL INT32 S.IFDIR  IS     #4000: --* Directory
VAL INT32 S.IFCHR  IS     #2000: --* Character device
VAL INT32 S.IFIFO  IS     #1000: --* FIFO
VAL INT32 S.ISUID  IS     #800:  --* Set-UID
VAL INT32 S.ISGID  IS     #400:  --* Set-GID
VAL INT32 S.ISVTX  IS     #200:  --* Sticky
--*}}}  
--{{{  external declarations for killable blocking calls
#PRAGMA EXTERNAL "PROC BX.fl.read (CHAN OF INT c, VAL INT fd, []BYTE buffer, INT result) = 3"
#PRAGMA EXTERNAL "PROC BX.fl.write (CHAN OF INT c, VAL INT fd, VAL []BYTE buffer, INT result) = 3"
#PRAGMA EXTERNAL "PROC BX.fl.fd.fd.copy (CHAN OF INT c, VAL INT src.fd, dst.fd, count, INT result) = 3"
--}}}  
--{{{  GETOPT
--* Responses from [@ref file.get.options].
PROTOCOL GETOPT
  CASE
    --* An option (such as [@text -a]).
    -- @item opt The option
    opt; BYTE
    --* An option with an argument (such as [@text -a foo]).
    -- @item opt The option
    -- @item arg The argument
    opt.arg; BYTE; MOBILE []BYTE
    --* A non-option argument (such as [@text foo]).
    -- @item arg The argument
    arg; MOBILE []BYTE
    --* An unrecognised option.
    -- This will be followed immediately by [@code done].
    -- @item opt The option
    bad; BYTE
    --* No more options.
    done
:
--}}}
--{{{  GETOPT.LONG
--* Responses from [@ref file.get.long.options].
PROTOCOL GETOPT.LONG
  CASE
    --* An option (such as [@text -a] or [@text --arg]).
    -- @item opt The option (e.g. [@text a] or [@text arg])
    opt; MOBILE []BYTE
    --* An option with an argument (such as [@text -a foo] or [@text --arg foo]).
    -- @item opt The option (e.g. [@text a] or [@text arg])
    -- @item arg The argument
    opt.arg; MOBILE []BYTE; MOBILE []BYTE
    --* A non-option argument (such as [@text foo]).
    -- @item arg The argument
    arg; MOBILE []BYTE
    --* An unrecognised option.
    -- This will be followed immediately by [@code done].
    -- @item opt The option
    bad; MOBILE []BYTE
    --* No more options.
    done
:
--}}}
--{{{  STAT
--* Information about a filesystem object.
-- This is the result from [@ref file.stat], [@ref file.lstat] and
-- [@ref file.fstat]. See the [@text stat(2)] manual page for more information
-- on the meaning of the fields.

-- The C code depends on the layout of this structure; if you change it, you
-- must also update unpack_stat.
DATA TYPE STAT
  PACKED RECORD
    INT64 dev:            --* Device number
    INT64 ino:            --* Inode number
    INT32 mode:           --* Object type and permissions
    INT32 nlink:          --* Number of links
    INT32 uid:            --* Owner's uid
    INT32 gid:            --* Group's gid
    INT64 rdev:           --* Device type
    INT32 size:           --* Size in bytes
    INT32 blksize:        --* Block size for filesystem IO
    INT32 blocks:         --* Number of blocks allocated
    INT32 atime:          --* Access time
    INT32 mtime:          --* Modification time
    INT32 ctime:          --* Change time
:
--}}}
--{{{  DIRENT
--* Information about a directory entry.
-- This is the result from [@ref file.readdir].

-- This is all the contents that POSIX guarantees (although most OSs have more).
DATA TYPE DIRENT
  RECORD
    [256]BYTE filename:   --* Filename, terminated with [@code '*0']
:
--}}}

