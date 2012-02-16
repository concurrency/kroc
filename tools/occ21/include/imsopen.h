/*{{{  module header */

/*
 *	Standard INMOS header for fopen definitions
 *	Copyright (C) 1987 Inmos Limited
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*}}}*/

#ifndef _IMSOPENH
/*{{{  not included header file */
#define _IMSOPENH

/*{{{  host compiler macros */
/*{{{  MS C specific macros */
#ifdef COMPILER_IS_MSC
/*{{{   */
#define _FOPEN_DOS
/*}}}*/
#endif
/*}}}*/

/*{{{  DEC C specific macros */
#ifdef HOST_OS_IS_VMS
/*{{{   */
#define _FOPEN_ANSI
/*}}}*/
#endif
/*}}}*/

/*{{{  3L C specific macros */
#ifdef COMPILER_IS_LLL
/*{{{   */
#define _FOPEN_ANSI
/*}}}*/
#endif
/*}}}*/

/*{{{  UNIX specific macros */
#ifdef HOST_OS_IS_UNIX
/*{{{   */
#define _FOPEN_UNIX
/*}}}*/
#endif
/*}}}*/

/*{{{  IMS C specific macros */
#ifdef IMS
/*{{{   */
#define _FOPEN_ANSI
/*}}}*/
#endif
/*}}}*/

/*{{{  WATCOM C specific macros */
#ifdef COMPILER_IS_WATCOM
/*{{{   */
#define _FOPEN_DOS
/*}}}*/
#endif
/*}}}*/
/*}}}*/

/*{{{  generic fopen macros */
/*{{{  _fmode style FOPEN commands */
#ifdef _FOPEN_FMODE
/*{{{   */
/*{{{  #include "fcntl.h" */
#include "fcntl.h"

#ifdef _O_TEXT
#ifndef O_TEXT
#define O_TEXT _O_TEXT
#endif
#endif

#ifdef _O_BINARY
#ifndef O_BINARY
#define O_BINARY _O_BINARY
#endif
#endif
/*}}}*/

#define FOPEN_READ_TEXT(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_TEXT;\
    Stream = fopen(Name, "r");\
    _fmode = SavedFmode;\
}
/*}}}*/
#define FOPEN_READ_BINARY(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_BINARY;\
    Stream = fopen(Name, "r");\
    _fmode = SavedFmode;\
}
/*}}}*/

#define FOPEN_WRITE_TEXT(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_TEXT;\
    Stream = fopen(Name, "w");\
    _fmode = SavedFmode;\
}
/*}}}*/
#define FOPEN_WRITE_BINARY(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_BINARY;\
    Stream = fopen(Name, "w");\
    _fmode = SavedFmode;\
}
/*}}}*/

#define FOPEN_APPEND_TEXT(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_TEXT;\
    Stream = fopen(Name, "a");\
    _fmode = SavedFmode;\
}
/*}}}*/
#define FOPEN_APPEND_BINARY(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_BINARY;\
    Stream = fopen(Name, "a");\
    _fmode = SavedFmode;\
}
/*}}}*/

#define FOPEN_READ_UPDATE_TEXT(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_TEXT;\
    Stream = fopen(Name, "r+");\
    _fmode = SavedFmode;\
}
/*}}}*/
#define FOPEN_READ_UPDATE_BINARY(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_BINARY;\
    Stream = fopen(Name, "r+");\
    _fmode = SavedFmode;\
}
/*}}}*/

#define FOPEN_WRITE_UPDATE_TEXT(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_TEXT;\
    Stream = fopen(Name, "w+");\
    _fmode = SavedFmode;\
}
/*}}}*/
#define FOPEN_WRITE_UPDATE_BINARY(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_BINARY;\
    Stream = fopen(Name, "w+");\
    _fmode = SavedFmode;\
}
/*}}}*/

#define FOPEN_APPEND_UPDATE_TEXT(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_TEXT;\
    Stream = fopen(Name, "a+");\
    _fmode = SavedFmode;\
}
/*}}}*/
#define FOPEN_APPEND_UPDATE_BINARY(Name, Stream) \
/*{{{   */
{\
    INT SavedFmode;\
    EXTERNAL INT _fmode;\
    SavedFmode = _fmode;\
    _fmode = O_BINARY;\
    Stream = fopen(Name, "a+");\
    _fmode = SavedFmode;\
}
/*}}}*/
/*}}}*/
#endif
/*}}}*/

/*{{{  ANSI style FOPEN open modes */
#ifdef _FOPEN_ANSI
/*{{{   */
#define _FMODETYPE /* Set mode type flag */

#define FMODE_READ_TEXT            "r"
#define FMODE_READ_BINARY          "rb"

#define FMODE_WRITE_TEXT           "w"
#define FMODE_WRITE_BINARY         "wb"

#define FMODE_APPEND_TEXT          "a"
#define FMODE_APPEND_BINARY        "ab"

#define FMODE_READ_UPDATE_TEXT     "r+"
#define FMODE_READ_UPDATE_BINARY   "r+b"

#define FMODE_WRITE_UPDATE_TEXT    "w+"
#define FMODE_WRITE_UPDATE_BINARY  "w+b"

#define FMODE_APPEND_UPDATE_TEXT   "a+"
#define FMODE_APPEND_UPDATE_BINARY "a+b"
/*}}}*/
#endif
/*}}}*/

/*{{{  UNIX style FOPEN open modes */
#ifdef _FOPEN_UNIX
/*{{{   */
#define _FMODETYPE /* Set mode type flag */

#define FMODE_READ_TEXT            "r"
#define FMODE_READ_BINARY          "r"

#define FMODE_WRITE_TEXT           "w"
#define FMODE_WRITE_BINARY         "w"

#define FMODE_APPEND_TEXT          "a"
#define FMODE_APPEND_BINARY        "a"

#define FMODE_READ_UPDATE_TEXT     "r+"
#define FMODE_READ_UPDATE_BINARY   "r+"

#define FMODE_WRITE_UPDATE_TEXT    "w+"
#define FMODE_WRITE_UPDATE_BINARY  "w+"

#define FMODE_APPEND_UPDATE_TEXT   "a+"
#define FMODE_APPEND_UPDATE_BINARY "a+"
/*}}}*/
#endif
/*}}}*/

/*{{{  DOS style FOPEN open modes */
#ifdef _FOPEN_DOS
/*{{{   */
#define _FMODETYPE /* Set mode type flag */

#define FMODE_READ_TEXT            "rt"
#define FMODE_READ_BINARY          "rb"

#define FMODE_WRITE_TEXT           "wt"
#define FMODE_WRITE_BINARY         "wb"

#define FMODE_APPEND_TEXT          "at"
#define FMODE_APPEND_BINARY        "ab"

#define FMODE_READ_UPDATE_TEXT     "r+t"
#define FMODE_READ_UPDATE_BINARY   "r+b"

#define FMODE_WRITE_UPDATE_TEXT    "w+t"
#define FMODE_WRITE_UPDATE_BINARY  "w+b"

#define FMODE_APPEND_UPDATE_TEXT   "a+t"
#define FMODE_APPEND_UPDATE_BINARY "a+b"
/*}}}*/
#endif
/*}}}*/

/*{{{  generic FOPEN commands */
#ifdef _FMODETYPE
/*{{{   */
#undef _FMODETYPE /* Reset mode type flag */

#define FOPEN_READ_TEXT(Name, Stream)   Stream = fopen(Name, FMODE_READ_TEXT)
#define FOPEN_READ_BINARY(Name, Stream) Stream = fopen(Name, FMODE_READ_BINARY)

#define FOPEN_WRITE_TEXT(Name, Stream)   Stream = fopen(Name, FMODE_WRITE_TEXT)
#define FOPEN_WRITE_BINARY(Name, Stream) Stream = fopen(Name, FMODE_WRITE_BINARY)

#define FOPEN_APPEND_TEXT(Name, Stream)   Stream = fopen(Name, FMODE_APPEND_TEXT)
#define FOPEN_APPEND_BINARY(Name, Stream) Stream = fopen(Name, FMODE_APPEND_BINARY)

#define FOPEN_READ_UPDATE_TEXT(Name, Stream)   Stream = fopen(Name, FMODE_READ_UPDATE_TEXT)
#define FOPEN_READ_UPDATE_BINARY(Name, Stream) Stream = fopen(Name, FMODE_READ_UPDATE_BINARY)

#define FOPEN_WRITE_UPDATE_TEXT(Name, Stream)   Stream = fopen(Name, FMODE_WRITE_UPDATE_TEXT)
#define FOPEN_WRITE_UPDATE_BINARY(Name, Stream) Stream = fopen(Name, FMODE_WRITE_UPDATE_BINARY)

#define FOPEN_APPEND_UPDATE_TEXT(Name, Stream)   Stream = fopen(Name, FMODE_APPEND_UPDATE_TEXT)
#define FOPEN_APPEND_UPDATE_BINARY(Name, Stream) Stream = fopen(Name, FMODE_APPEND_UPDATE_BINARY)
/*}}}*/
#endif
/*}}}*/
/*}}}*/
/*}}}*/
#endif
