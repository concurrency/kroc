/******************************************************************************

Project:           Portable command line ISP for Philips LPC2000 family
                   and Analog Devices ADUC70xx

Filename:          lpc21isp.c

Compiler:          Microsoft VC 6/7, GCC Cygwin, GCC Linux

Autor:             Martin Maurer (Martin.Maurer@clibb.de)

Copyright:         (c) Martin Maurer 2003-2007, All rights reserved
Portions Copyright (c) by Aeolus Development 2004 http://www.aeolusdevelopment.com
*/

#define VERSION_STR "1.48"

/*

Change-History:

 1.00  2004-01-08  Initial Version, tested for MSVC6/7 and GCC under Cygwin
 1.01  2004-01-10  Porting to Linux (at least compiling must work)
 1.02  2004-01-10  Implemented conversion intel hex format -> binary
 1.03  2004-01-25  Preparation to upload to public website
 1.04  2004-02-12  Merged in bugfixes by Soeren Gust
 1.05  2004-03-14  Implement printing of error codes as text / strings
 1.06  2004-03-09  Merged in bugfixes by Charles Manning:
                   The '?' sychronisation does not reliably respond to the first '?'.
                   I added some retries.
                   The LPC2106 sometimes responds to the '?' by echoing them back.
                   This sometimes causes an attempt to match "?Synchonized".
                   Added code to strip off any leading '?'s.
                   Timeouts were too long.
                   Change from RTS/CTS to no flow control.
                   Done because many/most people will use only 3-wire comms.
                   Added some progress tracing.
 1.07  2004-03-14  Implement handling of control lines for easier booting
 1.08  2004-04-01  Bugfix for upload problem
 1.09  2004-04-03  Redesign of upload routine
                   Now always 180 byte blocks are uploaded, to prevent
                   small junks in uuencoding
 1.10  2004-04-03  Clear buffers before sending commands to LPC21xx,
                   this prevents synchronizing errors when previously loaded
                   program does a lot of output, so FIFO of PC runs full
 1.11  2004-04-03  Small optimization for controlling reset line
                   otherwise termonly starts LPC twice, free PC buffers
 1.12  2004-04-04  Add switch to enable logging terminal output to lpc21isp.log
 1.13  2004-05-19  Merged in improvement by Charles Manning:
                   Instead of exiting the wrong hex file size is corrected
 1.14  2004-07-07  Merged in improvement by Alex Holden:
                   Remove little/big endian dependancy
 1.15  2004-09-27  Temporary improvement by Cyril Holweck:
                   Removed test (data echoed = data transmited) on the main
                   data transfert, since this was the biggest failure
                   reason and is covered by checksome anyway.
                   Added COMPILE_FOR_LPC21, to have target dump it's own
                   memory to stdout.
 1.16  2004-10-09  Merged in bugfix / improvement by Sinelnikov Evgeny
                   I found out that Linux and Windows serial port initialization
                   are different with pinouts states. My board don't get
                   reset signal at first cycle of DTR pinout moving.
                   And I add this moving to initalization cycle.
 1.17  2004-10-21  Changes by Cyril Holweck
                   Divide main, take out the real programming function, that can
                   also be used by a target to copy its own code to another.
 1.18  2004-10-26  Changes by Cyril Holweck
                   Added a "G 0 A\r\n" at end of programming to run code.
 1.19  2004-11-03  Changes by Robert Adsett
                   Add support for Analog Devices.
                   Separate file load from programming.
                   Change from a debug on/off flag to debug level
                   Remove if(debug) tests and replace with DebugPrintf
                   statements.
                   Change serial I/O and timing so that the system
                   dependancies are isolated to a few portability functions.
                   Add support for binary serial I/O.
                   Add doxygen support.
 1.20  2004-11-07  Preparation for multiport booting (factory support)
 1.21  2004-11-08  Bugfix from Robert Adsett
                   BinaryLength was not initialized
 1.22  2004-11-08  Changes from Cyril Holweck / Evgeny Sinelnikov
                   Forgotten IspEnvironment-> and bugfixes if COMPILE_FOR_LINUX
                   If COMPILE_FOR_LPC21, PhilipsDownload() 'acts as' main():
                   - it should not be static and should return int.
                   - no sub-function can use exit() but only return()
                   Use 'char' instead of 'byte' ;)
 1.23  2005-01-16  Build in automatic detection of LPC chiptype
                   (needed for 256 KByte support)
 1.24B 2005-06-02  Changes by Thiadmer Riemersma: completed support for other
                   chip types (LPC213x series and others).
 1.24C 2005-06-11  Changes by Thiadmer Riemersma: added the device ID codes for
                   chip types LPC2131 and LPC2132.
 1.25  2005-06-19  Martin Maurer: Setup more parameters in DCB,
                   otherwise wrong code is downloaded (only Windows and Cygwin)
                   when a previous program has changed these parameters
                   Check exact string of "G 0 A\r\n0\r\n" instead of whole received buffer,
                   to prevent checking of already received by program start
                   (error on running program, but reports CMD_SUCCESS)
                   Add ifdefs for all baudrates (needed only for high baudrate,
                   which seem to be not available on Macs...)
 1.26  2005-06-26  Martin Maurer:
                   Correct check again: "G 0 A\r\n0\r\n" is cutted, because of reboot
                   (error on running program, but reports CMD_SUCCESS)
 1.27  2005-06-29  Martin Maurer:
                   Add LPC chip ID's (thanks to Robert from Philips) for
                   missing LPC213x and upcoming new LPC214x chips
                   (currently untested, because i don't have access to these chips,
                    please give me feedback !)
 1.28  2005-07-27  Anders Rosvall / Embedded Artists AB:
                   Changed the reset timeout to 500 ms when entering the bootloader.
                   Some external reset controllers have quite long timeout periods,
                   so extening the timeout delay would be a good thing.
 1.29  2005-09-14  Rob Jansen:
                   Added functionality to download to RAM and run from there.
                   In LoadFile() added record types 04 (Extended Linear Address Record)
                   and 05 (Start Linear Address Record), added address offset
                   (IspEnvironment->BinaryOffset) and start address (...->StartAddress).
                   Changed PhilipsDownload to skip all Flash prepare/erase/copy commands.
                   Note: Tested with VC7 only
1.30   2005-10-04  Rob Jansen:
                   - forgot to change the version string in 1.29
                   - Wrong text in LoadFile corrected (printed text mentions record type 05,
                     this should be 04
                   - Changed LoadFile to accept multiple record types 04
                   - Changed LoadFile to check on memory size, will not load more than x MB
                     if linear extended address records are used
1.31   2005-11-13  Martin Maurer: Thanks to Frank Gutmann
                   Updated number of sectors in device table
                   for LPC2194, LPC2292 and LPC2294
1.32   2005-12-02  Martin Maurer: Corrected missing control of RTS/DTR
                   in case user selected -termonly and -control
                   Small correction (typo in debug)
1.33   2006-10-01  Jean-Marc Koller:
                   Added support for MacOS X (difference on how to set termios baudrate).
1.34   2006-10-01  Cyril Holweck:
                   Made it compile again for lpc21isp
                   Added const keyword to constant variables to make it better
                   code for embeded target. (decrease RAM usage)
                   Replaced all regular call to printf() by DebugPrintf()
                   Removed call to scanf() (not much usefull and cost a lot to my target)
1.35   2006-22-01  Cyril Holweck
                   Added feature for LPC21: will start downloading at Sector 1 and upward,
                   to finish with Sector 0, the one containing the checksum controling BSL entry
1.36   2006-25-01  Cyril Holweck
                   PhilipsDownload() will now return a unique error code for each error
1.37   2006-10-03  Jeroen Domburg
                   Added LPC2103 (and only the 2103, I can't find the IDs for 2101/2102)
                   Corrected a loop which occured if the program completely fits in sector 0
1.38   2007-01-05  Ray Molenkamp
                   Added feature for LPC21: Wipe entire device before programming to enable
                   reflashing of chips with the lpc codeprotection feature enabled.
1.39   2007-01-12  Martin Maurer
                   Added initial support for new processors LPC23xx and LPC24xx
1.40   2007-01-22  Martin Maurer
                   Correction of chip id of LPC2458
1.41   2007-01-28  Jean-Marc Koller
                   Modified Terminal() to disable ECHO with termios only once, instead of
                   modifying and restoring termios in each getch and kbhit call (which caused
                   a strange echo behaviour in MacOS X).
1.42   2007-01-28  Rob Probin
                   Added -localecho command to allow local echoing in terminal mode for use
                   where target does not echo back keystrokes.
1.43   2007-01-29  Martin Maurer
                   Moved keyboard handling routines to own subroutines,
                   so they can be used during aborting synchronisation.
                   Newest cygwin made problems, StringOscillator always contained '\0x0d'
                   at the end, when calling lpc21isp from batch file
1.44   2007-02-23  Yang Yang
                   Added feature for LPC21: Verify the data in Flash after every writes
                   to sector. To detect errors in writing to Flash ROM.
1.45   2007-02-25  Martin Maurer
                   Replace printf syntax of DumpString by a simple pointer to a string
                   printf syntax is a nice thing, but it is not working :-(
                   and therefore makes debugging much more difficult...
                   Moved VERSION_STR to top of file to avoid possible cosmetical errors
1.46   2007-02-25  Martin Maurer
                   Again corrected debug output: should solve output of
                   (FFFFFFB5) instead of (B5)
1.47   2007-02-27  Robert Adsett
                   Raised timeout on AD send packet function.
1.48   2007-04-20  Martin Maurer
                   Thanks to Josef Wolf for preventing to overwrite over end of array

******************************************************************************
******* Don't forget to update the version string on top of file *************
******************************************************************************/

/* LPC_RAMSTART, LPC_RAMBASE
 *
 * Used in PhilipsDownload() to decide whether to Flash code or just place in in RAM
 * (works for .hex files only)
 *
 * LPC_RAMSTART - the Physical start address of the SRAM
 * LPC_RAMBASE  - the base address where downloading starts.
 *                Note that any code in the .hex file that resides in 0x4000,0000 ~ 0x4000,0200
 *                will _not_ be written to the LPCs SRAM.
 *                This is due to the fact that 0x4000,0040 - 0x4000,0200 is used by the bootrom.
 *                Any interrupt vectors must be copied to 0x4000,0000 and remapped to 0x0000,0000
 *                by the startup code.
 */
#define LPC_RAMSTART    0x40000000L
#define LPC_RAMBASE     0x40000200L

/* LPC_FLASHMASK
 *
 * LPC_FLASHMASK - bitmask to define the maximum size of the Filesize to download.
 *                 LoadFile() will check any new segment address record (03) or extended linear
 *                 address record (04) to see if the addressed 64 kByte data block still falls
 *                 in the max. flash size.
 *                 LoadFile() will not load any files that are larger than this size.
 */
#define LPC_FLASHMASK  0xFFC00000 /* 22 bits = 4 MB */


#if defined(_WIN32) && !defined(__CYGWIN__)
    #define COMPILE_FOR_WINDOWS
    #define COMPILED_FOR ("Windows")
#elif defined(__CYGWIN__)
    #define COMPILE_FOR_CYGWIN
    #define COMPILED_FOR ("Cygwin")
#elif defined(__arm__) || defined(__thumb__)
    #define COMPILE_FOR_LPC21
    #define COMPILED_FOR ("ARM")
    #define printf iprintf
#elif defined(__APPLE__)
    #define COMPILE_FOR_LINUX
    #define COMPILED_FOR ("Apple MacOS X")
#else
    #define COMPILE_FOR_LINUX
    #define COMPILED_FOR ("Linux")
#endif

#if defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN
#include <windows.h>
#include <io.h>
#endif // defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

#if defined COMPILE_FOR_WINDOWS
#include <conio.h>
#endif // defined COMPILE_FOR_WINDOWS

#if defined COMPILE_FOR_LINUX
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/ioctl.h>
#endif // defined COMPILE_FOR_LINUX

#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN
#include <termios.h>
#include <unistd.h>     // for read and return value of lseek
#include <sys/time.h>   // for select_time
#endif // defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN

#include <ctype.h>      // isdigit()
#include <stdio.h>      // stdout
#include <stdarg.h>
#include <time.h>

#if defined COMPILE_FOR_LPC21
#include <stdlib.h>
#include <string.h>
//#include <lpc_ioctl.h>  // if using libc serial port communication
#include "lpc21isp.h"   // if using propriatory serial port communication (customize attached lpc21isp.h)
#endif

#if !defined COMPILE_FOR_LPC21
#include <fcntl.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif // O_BINARY

#ifndef DWORD
#define DWORD unsigned long
#endif // DWORD

#if defined COMPILE_FOR_LINUX
#define stricmp strcasecmp
#endif // defined COMPILE_FOR_LINUX


/* Return values used by PhilipsDownload(): reserving all values from 0x1000 to 0x1FFF */

#define NO_ANSWER_WDT       0x1000
#define NO_ANSWER_QM        0x1001
#define NO_ANSWER_SYNC      0x1002
#define NO_ANSWER_OSC       0x1003
#define NO_ANSWER_RBV       0x1004
#define NO_ANSWER_RPID      0x1005
#define ERROR_WRITE_DATA    0x1006
#define ERROR_WRITE_CRC     0x1007
#define ERROR_WRITE_CRC2    0x1008
#define PROGRAM_TOO_LARGE   0x1009

#define USER_ABORT_SYNC     0x100A   /* User aborted synchronisation process */

#define UNLOCK_ERROR        0x1100   /* return value is 0x1100 + philips ISP returned value (0 to 255) */
#define WRONG_ANSWER_PREP   0x1200   /* return value is 0x1200 + philips ISP returned value (0 to 255) */
#define WRONG_ANSWER_ERAS   0x1300   /* return value is 0x1300 + philips ISP returned value (0 to 255) */
#define WRONG_ANSWER_WRIT   0x1400   /* return value is 0x1400 + philips ISP returned value (0 to 255) */
#define WRONG_ANSWER_PREP2  0x1500   /* return value is 0x1500 + philips ISP returned value (0 to 255) */
#define WRONG_ANSWER_COPY   0x1600   /* return value is 0x1600 + philips ISP returned value (0 to 255) */
#define FAILED_RUN          0x1700   /* return value is 0x1700 + philips ISP returned value (0 to 255) */





typedef unsigned char BINARY;   /**< data type used for microcontroller
                                *  memory image.                        */

/*
debug levels
0 - very quiet          - Nothing gets printed at this level
1 - quiet               - Only error messages should be printed
2 - indicate progress   - Add progress messages
3 - first level debug   - Major level tracing
4 - second level debug  - Add detailed debugging
5 - log comm's          - log serial I/O
*/

typedef enum
{
    PHILIPS_ARM,
    ANALOG_DEVICES_ARM
} TARGET;

typedef struct
{
    unsigned long id;
    unsigned Product;
    unsigned FlashSize;     /* in kiB, for informational purposes only */
    unsigned RAMSize;       /* in kiB, for informational purposes only */
    unsigned FlashSectors;  /* total number of sectors */
    unsigned MaxCopySize;   /* maximum size that can be copied to Flash in a single command */
    const int *SectorTable; /* pointer to a sector table with constant the sector sizes */
} LPC_DEVICE_TYPE;

typedef struct
{

#if !defined COMPILE_FOR_LPC21
    TARGET micro;                      /**< The type of micro that will be
                                         * programmed.                           */
    int debug_level;
    unsigned char TerminalAfterUpload;
    unsigned char FormatHex;
    unsigned char ControlLines;
    unsigned char LogFile;
    char *input_file;                   /**< Name of the file to get input from. */
    char *serial_port;                  /**< Name of the serial port to use to
                                         * communicate with the microcontroller.
                                         * Read from the command line.          */

#endif // !defined COMPILE_FOR_LPC21
    unsigned char TerminalOnly;
    unsigned char DetectOnly;
    unsigned char WipeDevice;
    unsigned char LocalEcho;
    unsigned char Verify;
    int           DetectedDevice;       /* index in LPCtypes[] array */
    char *baud_rate;                    /**< Baud rate to use on the serial
                                         * port communicating with the
                                         * microcontroller. Read from the
                                         * command line.                        */

    char StringOscillator[6];           /**< Holds representation of oscillator
                                          * speed from the command line.         */
    BINARY *BinaryContent;              /**< Binary image of the                 */
                                        /* microcontroller's memory.             */
    unsigned long BinaryLength;
    unsigned long BinaryOffset;
    unsigned long StartAddress;

#if defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN
    HANDLE hCom;
#endif // defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_LPC21
    int fdCom;
#endif // defined COMPILE_FOR_LINUX || defined COMPILE_FOR_LPC21

#if defined COMPILE_FOR_LINUX
    struct termios oldtio, newtio;
#endif // defined COMPILE_FOR_LINUX

    unsigned serial_timeout_count;   /**< Local used to track
                                      * timeouts on serial port read. */

} ISP_ENVIRONMENT;


static void DumpString(int level, const void *s, size_t size, const char *prefix_string);
static void SendComPort(ISP_ENVIRONMENT *IspEnvironment, const char *s);
static void ReceiveComPort(ISP_ENVIRONMENT *IspEnvironment, void *Answer, unsigned long MaxSize, unsigned long *RealSize, unsigned long WantedNr0x0A,unsigned timeOutMilliseconds);
static void PhilipsOutputErrorMessage(unsigned char ErrorNumber);
static unsigned char GetErrorNumber(const char *Answer);
static void SerialTimeoutSet(ISP_ENVIRONMENT *IspEnvironment, unsigned timeout_milliseconds);
static void SerialTimeoutTick(ISP_ENVIRONMENT *IspEnvironment);
static int SerialTimeoutCheck(ISP_ENVIRONMENT *IspEnvironment);

#if !defined COMPILE_FOR_LPC21

static int debug_level = 2;

static void DebugPrintf( int level, const char *fmt, ...);
static void ClearSerialPortBuffers(ISP_ENVIRONMENT *IspEnvironment);
static void ControlModemLines(ISP_ENVIRONMENT *IspEnvironment, unsigned char DTR, unsigned char RTS);
static unsigned char Ascii2Hex(unsigned char c);
#endif

static const int SectorTable_210x[] = { 8192, 8192, 8192, 8192, 8192, 8192, 8192, 8192,
                                        8192, 8192, 8192, 8192, 8192, 8192, 8192 };
static const int SectorTable_2103[] = { 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096 };
static const int SectorTable_211x[] = { 8192, 8192, 8192, 8192, 8192, 8192, 8192, 8192,
                                        8192, 8192, 8192, 8192, 8192, 8192, 8192, };
static const int SectorTable_212x[] = { 8192, 8192, 8192, 8192, 8192, 8192, 8192, 8192,
                                        65536, 65536, 8192, 8192, 8192, 8192, 8192, 8192,
                                        8192 };
static const int SectorTable_213x[] = { 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096,
                                        32768, 32768, 32768, 32768, 32768, 32768, 32768, 32768,
                                        32768, 32768, 32768, 32768, 32768, 32768, 4096, 4096,
                                        4096, 4096, 4096 };
static int SectorTable_RAM[]  = { 65000 };

static LPC_DEVICE_TYPE LPCtypes[] =
{
  { 0, 0, 0 },  /* unknown */
  { 0x0004FF11, 2103,  32,  8,  8, 4096, SectorTable_2103 },
  { 0xFFF0FF12, 2104, 128, 16, 15, 8192, SectorTable_210x },
  { 0xFFF0FF22, 2105, 128, 32, 15, 8192, SectorTable_210x },
  { 0xFFF0FF32, 2106, 128, 64, 15, 8192, SectorTable_210x },
  { 0x0101FF12, 2114, 128, 16, 15, 8192, SectorTable_211x },
  { 0x0201FF12, 2119, 128, 16, 15, 8192, SectorTable_211x },
  { 0x0101FF13, 2124, 256, 16, 17, 8192, SectorTable_212x },
  { 0x0201FF13, 2129, 256, 16, 17, 8192, SectorTable_212x },
  { 0x0002FF01, 2131,  32,  8,  8, 4096, SectorTable_213x },
  { 0x0002FF11, 2132,  64, 16,  9, 4096, SectorTable_213x },
  { 0x0002FF12, 2134, 128, 16, 11, 4096, SectorTable_213x },
  { 0x0002FF23, 2136, 256, 32, 15, 4096, SectorTable_213x },
  { 0x0002FF25, 2138, 512, 32, 27, 4096, SectorTable_213x },
  { 0x0402FF01, 2141,  32,  8,  8, 4096, SectorTable_213x },
  { 0x0402FF11, 2142,  64, 16,  9, 4096, SectorTable_213x },
  { 0x0402FF12, 2144, 128, 16, 11, 4096, SectorTable_213x },
  { 0x0402FF23, 2146, 256, 40, 15, 4096, SectorTable_213x },
  { 0x0402FF25, 2148, 512, 40, 27, 4096, SectorTable_213x },
  { 0x0301FF13, 2194, 256, 16, 17, 8192, SectorTable_212x },
  { 0x0301FF12, 2210,   0, 16,  0, 8192, SectorTable_211x }, /* table is a "don't care" */
  { 0x0401FF12, 2212, 128, 16, 15, 8192, SectorTable_211x },
  { 0x0601FF13, 2214, 256, 16, 17, 8192, SectorTable_212x },
  /*            2290; same id as the LPC2210 */
  { 0x0401FF13, 2292, 256, 16, 17, 8192, SectorTable_212x },
  { 0x0501FF13, 2294, 256, 16, 17, 8192, SectorTable_212x },
  { 0x0603FB02, 2364, 128, 34, 11, 4096, SectorTable_213x },
  { 0x0603FB23, 2366, 256, 58, 15, 4096, SectorTable_213x },
  { 0x0603FB25, 2368, 512, 58, 27, 4096, SectorTable_213x },
  { 0x0703FF25, 2378, 512, 58, 27, 4096, SectorTable_213x },
  { 0x0503FF35, 2458, 512, 98, 27, 4096, SectorTable_213x },
  { 0x0603FF35, 2468, 512, 98, 27, 4096, SectorTable_213x }
};

/************* Portability layer. Serial and console I/O differences    */
/* are taken care of here.                                              */

#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN
static int kbhit( void);
static int getch( void);
#endif // defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN

static void PrepareKeyboardTtySettings(void);
static void ResetKeyboardTtySettings(void);

#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN
struct termios keyboard_origtty;
#endif


#if defined COMPILE_FOR_LINUX
static void Sleep(unsigned long MilliSeconds);
#endif // defined COMPILE_FOR_LINUX



#if !defined COMPILE_FOR_LPC21
static void OpenSerialPort(ISP_ENVIRONMENT *IspEnvironment)
{
    // Open COM-Port (different between Windows and Linux)

#if defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN
    DCB    dcb;
    COMMTIMEOUTS commtimeouts;

    IspEnvironment->hCom = CreateFile(IspEnvironment->serial_port, GENERIC_READ | GENERIC_WRITE,0,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);

    if(IspEnvironment->hCom == INVALID_HANDLE_VALUE)
    {
        DebugPrintf( 1, "Can't open COM-Port %s ! - Error: %ld\n", IspEnvironment->serial_port, GetLastError());
        exit(2);
    }

    DebugPrintf( 3, "COM-Port %s opened...\n", IspEnvironment->serial_port);

    GetCommState(IspEnvironment->hCom, &dcb);
    dcb.BaudRate    = atol(IspEnvironment->baud_rate);
    dcb.ByteSize    = 8;
    dcb.StopBits    = ONESTOPBIT;
    dcb.Parity      = NOPARITY;
    dcb.fDtrControl = DTR_CONTROL_DISABLE;
    dcb.fOutX       = FALSE;
    dcb.fInX        = FALSE;
    dcb.fNull       = FALSE;
    dcb.fRtsControl = RTS_CONTROL_DISABLE;
    if(SetCommState(IspEnvironment->hCom, &dcb) == 0)
    {
        DebugPrintf( 1, "Can't set baudrate %s ! - Error: %ld", IspEnvironment->baud_rate, GetLastError());
        exit(3);
    }

    SetCommMask(IspEnvironment->hCom,EV_RXCHAR | EV_TXEMPTY);

    commtimeouts.ReadIntervalTimeout         = MAXDWORD;
    commtimeouts.ReadTotalTimeoutMultiplier  =    0;
    commtimeouts.ReadTotalTimeoutConstant    =    1;
    commtimeouts.WriteTotalTimeoutMultiplier =    0;
    commtimeouts.WriteTotalTimeoutConstant   =    0;
    SetCommTimeouts(IspEnvironment->hCom, &commtimeouts);

#endif // defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

#if defined COMPILE_FOR_LINUX

    IspEnvironment->fdCom = open(IspEnvironment->serial_port, O_RDWR | O_NOCTTY | O_NONBLOCK );

    if(IspEnvironment->fdCom < 0)
    {
        DebugPrintf( 1, "Can't open COM-Port %s !\n", IspEnvironment->serial_port);
        exit(2);
    }

    DebugPrintf( 3, "COM-Port %s opened...\n", IspEnvironment->serial_port);

    /* clear input & output buffers, then switch to "blocking mode" */
    tcflush(IspEnvironment->fdCom, TCOFLUSH);
    tcflush(IspEnvironment->fdCom, TCIFLUSH);
    fcntl(IspEnvironment->fdCom, F_SETFL, fcntl(IspEnvironment->fdCom, F_GETFL) & ~O_NONBLOCK);

    tcgetattr(IspEnvironment->fdCom, &IspEnvironment->oldtio); /* save current port settings */

    bzero(&IspEnvironment->newtio, sizeof(IspEnvironment->newtio));
    IspEnvironment->newtio.c_cflag = CS8 | CLOCAL | CREAD;

#ifdef __APPLE__
#define NEWTERMIOS_SETBAUDARTE(bps) IspEnvironment->newtio.c_ispeed = IspEnvironment->newtio.c_ospeed = bps;
#else
#define NEWTERMIOS_SETBAUDARTE(bps) IspEnvironment->newtio.c_cflag |= bps;
#endif

    switch(atol(IspEnvironment->baud_rate))
    {
#ifdef B1152000
        case 1152000: NEWTERMIOS_SETBAUDARTE( B1152000 ); break;
#endif // B1152000
#ifdef B576000
        case  576000: NEWTERMIOS_SETBAUDARTE( B576000 ); break;
#endif // B576000
#ifdef B230400
        case  230400: NEWTERMIOS_SETBAUDARTE( B230400 ); break;
#endif // B230400
#ifdef B115200
        case  115200: NEWTERMIOS_SETBAUDARTE( B115200 ); break;
#endif // B115200
#ifdef B57600
        case   57600: NEWTERMIOS_SETBAUDARTE( B57600 ); break;
#endif // B57600
#ifdef B38400
        case   38400: NEWTERMIOS_SETBAUDARTE( B38400 ); break;
#endif // B38400
#ifdef B19200
        case   19200: NEWTERMIOS_SETBAUDARTE( B19200 ); break;
#endif // B19200
#ifdef B9600
        case    9600: NEWTERMIOS_SETBAUDARTE( B9600 ); break;
#endif // B9600
        default:
        {
            DebugPrintf( 1, "unknown baudrate %s\n", IspEnvironment->baud_rate);
            exit(3);
        }
    }

    IspEnvironment->newtio.c_iflag = IGNPAR | IGNBRK | IXON | IXOFF;
    IspEnvironment->newtio.c_oflag = 0;

    /* set input mode (non-canonical, no echo,...) */
    IspEnvironment->newtio.c_lflag = 0;

    cfmakeraw(&IspEnvironment->newtio);
    IspEnvironment->newtio.c_cc[VTIME]    = 1;   /* inter-character timer used */
    IspEnvironment->newtio.c_cc[VMIN]     = 0;   /* blocking read until 0 chars received */

    tcflush(IspEnvironment->fdCom, TCIFLUSH);
    tcsetattr(IspEnvironment->fdCom, TCSANOW, &IspEnvironment->newtio);

#endif // defined COMPILE_FOR_LINUX
}

static void CloseSerialPort(ISP_ENVIRONMENT *IspEnvironment)
{
#if defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

    CloseHandle(IspEnvironment->hCom);

#endif // defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

#if defined COMPILE_FOR_LINUX

    tcflush(IspEnvironment->fdCom, TCOFLUSH);
    tcflush(IspEnvironment->fdCom, TCIFLUSH);
    tcsetattr(IspEnvironment->fdCom, TCSANOW, &IspEnvironment->oldtio);

    close(IspEnvironment->fdCom);

#endif // defined COMPILE_FOR_LINUX
}

#endif // !defined COMPILE_FOR_LPC21

/***************************** SendComPortBlock *************************/
/**  Sends a block of bytes out the opened com port.
\param [in] s block to send.
\param [in] n size of the block.
*/
static void SendComPortBlock(ISP_ENVIRONMENT *IspEnvironment, const void *s, size_t n)
{
#if defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

    unsigned long realsize;

#endif // defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

    DumpString( 4, s, n, "Sending ");

#if defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

    WriteFile(IspEnvironment->hCom, s, n, &realsize, NULL);

#endif // defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_LPC21

    write(IspEnvironment->fdCom, s, n);

#endif // defined COMPILE_FOR_LINUX || defined COMPILE_FOR_LPC21
}

/***************************** SendComPort ******************************/
/**  Sends a string out the opened com port.
\param [in] s string to send.
*/
static void SendComPort(ISP_ENVIRONMENT *IspEnvironment, const char *s)
{
    SendComPortBlock( IspEnvironment, s, strlen(s));
}


/***************************** ReceiveComPortBlock **********************/
/**  Receives a buffer from the open com port. Returns all the characters
ready (waits for up to 'n' milliseconds before accepting that no more
characters are ready) or when the buffer is full. 'n' is system dependant,
see SerialTimeout routines.
\param [out] answer buffer to hold the bytes read from the serial port.
\param [in] max_size the size of buffer pointed to by answer.
\param [out] real_size pointer to a long that returns the amout of the
buffer that is actually used.
*/
static void ReceiveComPortBlock( ISP_ENVIRONMENT *IspEnvironment,
                                 void *answer, unsigned long max_size,
                                 unsigned long *real_size)
{
    char tmp_string[32];

#if defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

    ReadFile(IspEnvironment->hCom, answer, max_size, real_size, NULL);

#endif // defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_LPC21

    *real_size = read(IspEnvironment->fdCom, answer, max_size);

#endif // defined COMPILE_FOR_LINUX

    sprintf(tmp_string, "Read(Length=%ld): ", (*real_size));
    DumpString( 5, answer, (*real_size), tmp_string);

    if( *real_size == 0)
    {
        SerialTimeoutTick( IspEnvironment );
    }
}


/***************************** SerialTimeoutSet *************************/
/**  Sets (or resets) the timeout to the timout period requested.  Starts
counting to this period.  This timeout support is a little odd in that the
timeout specifies the accumulated deadtime waiting to read not the total
time waiting to read. They should be close enought to the same for this
use. Used by the serial input routines, the actual counting takes place in
ReceiveComPortBlock.
\param [in] timeout_milliseconds the time in milliseconds to use for
timeout.  Note that just because it is set in milliseconds doesn't mean
that the granularity is that fine.  In many cases (particularly Linux) it
will be coarser.
*/
static void SerialTimeoutSet(ISP_ENVIRONMENT *IspEnvironment, unsigned timeout_milliseconds)
{

#if defined COMPILE_FOR_LINUX
    IspEnvironment->serial_timeout_count = timeout_milliseconds/100;
#elif defined COMPILE_FOR_LPC21
    IspEnvironment->serial_timeout_count = timeout_milliseconds*200;
#else
    IspEnvironment->serial_timeout_count = timeout_milliseconds;
#endif
}

/***************************** SerialTimeoutTick ************************/
/**  Performs a timer tick.  In this simple case all we do is count down
with protection against underflow and wrapping at the low end.
*/
static void SerialTimeoutTick(ISP_ENVIRONMENT *IspEnvironment)
{
    if( IspEnvironment->serial_timeout_count <= 1)
    {
        IspEnvironment->serial_timeout_count = 0;
    }
    else
    {
        IspEnvironment->serial_timeout_count--;
    }
}

/***************************** SerialTimeoutCheck ***********************/
/**  Check to see if the serial timeout timer has run down.
\retval 1 if timer has run out.
\retval 0 if timer still has time left.
*/
static int SerialTimeoutCheck(ISP_ENVIRONMENT *IspEnvironment)
{
    if( IspEnvironment->serial_timeout_count == 0)
    {
        return 1;
    }
    return 0;
}


#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN
/***************************** getch ************************************/
/** Replacement for the common dos function of the same name. Reads a
single unbuffered character from the 'keyboard'.
\return The character read from the keyboard.
*/
static int getch(void)
{
    char ch;

    /* Read in one character */
    read(0,&ch,1);

    return ch;
}
#endif // defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN

#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN
/***************************** kbhit ************************************/
/** Replacement for the common dos function of the same name. Indicates if
there are characters to be read from the console.
\retval 0 No characters ready.
\retval 1 Characters from the console ready to be read.
*/
static int kbhit(void)
{
    /* return 0 for no key pressed, 1 for key pressed */
    int return_value = 0;

    /* time struct for the select() function, to only wait a little while */
    struct timeval select_time;
    /* file descriptor variable for the select() call */
    fd_set readset;

    /* we're only interested in STDIN */
    FD_ZERO(&readset);
    FD_SET(STDIN_FILENO, &readset);

    /* how long to block for - this must be > 0.0, but could be changed
       to some other setting. 10-18msec seems to work well and only
       minimally load the system (0% CPU loading) */
    select_time.tv_sec = 0;
    select_time.tv_usec = 10;

    /* is there a keystroke there? */
    if (select(1, &readset, NULL, NULL, &select_time))
    {
        /* yes, remember it */
        return_value = 1;
    }


    /* return with what we found out */
    return return_value;
}
#endif // defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN


/***************************** PrepareKeyboardTtySettings ***************/
/** Set the keyboard tty to be able to check for new characters via kbhit
getting them via getch
*/
static void PrepareKeyboardTtySettings(void)
{
#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN
    /* store the current tty settings */
    if (!tcgetattr(0, &keyboard_origtty))
    {
        struct termios tty;
        /* start with the current settings */
        tty = keyboard_origtty;
        /* make modifications to put it in raw mode, turn off echo */
        tty.c_lflag &= ~ICANON;
        tty.c_lflag &= ~ECHO;
        tty.c_lflag &= ~ISIG;
        tty.c_cc[VMIN] = 1;
        tty.c_cc[VTIME] = 0;

        /* put the settings into effect */
        tcsetattr(0, TCSADRAIN, &tty);
    }
#endif
}


/***************************** ResetKeyboardTtySettings *****************/
/** Reset the keyboard tty to original settings
*/
static void ResetKeyboardTtySettings(void)
{
#if defined COMPILE_FOR_LINUX || defined COMPILE_FOR_CYGWIN
    /* reset the tty to its original settings */
    tcsetattr(0, TCSADRAIN, &keyboard_origtty);
#endif
}


#if !defined COMPILE_FOR_LPC21
/***************************** ControlModemLines ************************/
/**  Controls the modem lines to place the microcontroller into various
states during the programming process.
error rather abruptly terminates the program.
\param [in] DTR the state to set the DTR line to.
\param [in] RTS the state to set the RTS line to.
*/
static void ControlModemLines(ISP_ENVIRONMENT *IspEnvironment, unsigned char DTR, unsigned char RTS)
{
#if defined COMPILE_FOR_LINUX
    int status;

    if(ioctl(IspEnvironment->fdCom, TIOCMGET, &status) == 0)
    {
        DebugPrintf( 1, "ioctl get ok, status = %X\n",status);
    }
    else
    {
        DebugPrintf( 1, "ioctl get failed\n");
    }

    if(DTR) status |=  TIOCM_DTR;
    else    status &= ~TIOCM_DTR;

    if(RTS) status |=  TIOCM_RTS;
    else    status &= ~TIOCM_RTS;

    if(ioctl(IspEnvironment->fdCom, TIOCMSET, &status) == 0)
    {
        DebugPrintf( 1, "ioctl set ok, status = %X\n",status);
    }
    else
    {
        DebugPrintf( 1, "ioctl set failed\n");
    }

    if(ioctl(IspEnvironment->fdCom, TIOCMGET, &status) == 0)
    {
        DebugPrintf( 1, "ioctl get ok, status = %X\n",status);
    }
    else
    {
        DebugPrintf( 1, "ioctl get failed\n");
    }

#endif // defined COMPILE_FOR_LINUX
#if defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

    if(DTR) EscapeCommFunction(IspEnvironment->hCom, SETDTR);
    else    EscapeCommFunction(IspEnvironment->hCom, CLRDTR);

    if(RTS) EscapeCommFunction(IspEnvironment->hCom, SETRTS);
    else    EscapeCommFunction(IspEnvironment->hCom, CLRRTS);

#endif // defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN

    DebugPrintf( 3, "DTR (%d), RTS (%d)\n", DTR, RTS);
}


/***************************** ClearSerialPortBuffers********************/
/**  Empty the serial port buffers.  Cleans things to a known state.
*/
static void ClearSerialPortBuffers(ISP_ENVIRONMENT *IspEnvironment)
{
#if defined COMPILE_FOR_LINUX
    /* variables to store the current tty state, create a new one */
    struct termios origtty, tty;

    /* store the current tty settings */
    tcgetattr(IspEnvironment->fdCom, &origtty);

    // Flush input and output buffers
    tcsetattr(IspEnvironment->fdCom, TCSAFLUSH, &tty);

    /* reset the tty to its original settings */
    tcsetattr(IspEnvironment->fdCom, TCSADRAIN, &origtty);
#endif // defined COMPILE_FOR_LINUX
#if defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN
    PurgeComm(IspEnvironment->hCom, PURGE_TXABORT | PURGE_RXABORT | PURGE_TXCLEAR | PURGE_RXCLEAR);
#endif // defined COMPILE_FOR_WINDOWS || defined COMPILE_FOR_CYGWIN
}
#endif // !defined COMPILE_FOR_LPC21


#if defined COMPILE_FOR_LINUX
/***************************** Sleep ************************************/
/**  Provide linux replacement for windows function.
\param [in] Milliseconds the time to wait for in milliseconds.
*/
static void Sleep(unsigned long MilliSeconds)
{
    usleep(MilliSeconds*1000); //convert to microseconds
}
#endif // defined COMPILE_FOR_LINUX


#if defined COMPILE_FOR_LPC21
/**  Provide linux replacement for windows function.
\note I implement that one in my private header file today...
\param [in] Milliseconds the time to wait for in milliseconds.
*/
/*static void Sleep(unsigned long MilliSeconds)
{
#   warning Sleep function not implemented
}
*/
#endif // defined COMPILE_FOR_LPC21



/************* Applicationlayer.                                        */

#if !defined COMPILE_FOR_LPC21
/***************************** DebugPrintf ******************************/
/**  Prints a debug string depending the current debug level. The higher
the debug level the more detail that will be printed.  Each print
has an associated level, the higher the level the more detailed the
debugging information being sent.
\param [in] level the debug level of the print statement, if the level
 is less than or equal to the current debug level it will be printed.
\param [in] fmt a standard printf style format string.
\param [in] ... the usual printf parameters.
*/
static void DebugPrintf( int level, const char *fmt, ...)
{
    va_list ap;

    if( level <= debug_level)
    {
        va_start( ap, fmt);
        vprintf( fmt, ap);
        va_end( ap);
        fflush( stdout);
    }
}
#endif // !defined COMPILE_FOR_LPC21


/***************************** ReceiveComPort ***************************/
/**  Receives a buffer from the open com port. Returns when the buffer is
filled, the numer of requested linefeeds has been received or the timeout
period has passed
\param [in] ISPEnvironment.
\param [out] Answer buffer to hold the bytes read from the serial port.
\param [in] MaxSize the size of buffer pointed to by Answer.
\param [out] RealSize pointer to a long that returns the amout of the
buffer that is actually used.
\param [in] WantedNr0x0A the maximum number of linefeeds to accept before
returning.
\param [in] timeOutMilliseconds the maximum amount of time to wait before
reading with an incomplete buffer.
*/
static void ReceiveComPort( ISP_ENVIRONMENT *IspEnvironment,
                            void *Ans, unsigned long MaxSize,
                            unsigned long *RealSize, unsigned long WantedNr0x0A,
                            unsigned timeOutMilliseconds)
{
    unsigned long tmp_realsize;
    unsigned long nr_of_0x0A = 0;
    int eof = 0;
    unsigned long p;
    signed char *Answer;
    char tmp_string[32];

    Answer = Ans;

    SerialTimeoutSet( IspEnvironment, timeOutMilliseconds);

    (*RealSize) = 0;

    do
    {
        ReceiveComPortBlock( IspEnvironment, Answer + (*RealSize), MaxSize - 1 - (*RealSize), &tmp_realsize);

        if(tmp_realsize != 0)
        {
            for(p = (*RealSize); p < (*RealSize) + tmp_realsize; p++)
            {
                if(Answer[p] == 0x0a)
                {
                    nr_of_0x0A++;
                }
                else if( Answer[p] < 0 )
                {
                    eof = 1;
                }
            }
        }

        (*RealSize) += tmp_realsize;

    } while(((*RealSize) < MaxSize) && (SerialTimeoutCheck(IspEnvironment) == 0) && (nr_of_0x0A < WantedNr0x0A) && !eof);

    Answer[(*RealSize)] = 0;

    sprintf(tmp_string, "Answer(Length=%ld): ", (*RealSize));
    DumpString( 3, Answer, (*RealSize), tmp_string);
}


#if !defined COMPILE_FOR_LPC21

/***************************** ReceiveComPortBlockComplete **************/
/**  Receives a fixed block from the open com port. Returns when the
block is completely filled or the timeout period has passed
\param [out] block buffer to hold the bytes read from the serial port.
\param [in] size the size of the buffer pointed to by block.
\param [in] timeOut the maximum amount of time to wait before guvung up on
completing the read.
\return 0 if successful, non-zero otherwise.
*/
static int ReceiveComPortBlockComplete( ISP_ENVIRONMENT *IspEnvironment,
                                        void *block, size_t size, unsigned timeout)
{
    unsigned long realsize = 0, read;
    char *result;
    char tmp_string[32];

    result = block;

    SerialTimeoutSet( IspEnvironment, timeout);

    do
    {
        ReceiveComPortBlock( IspEnvironment, result + realsize, size - realsize, &read);

        realsize += read;

    } while((realsize < size) && (SerialTimeoutCheck(IspEnvironment) == 0));

    sprintf(tmp_string, "Answer(Length=%ld): ", realsize);
    DumpString( 3, result, realsize, tmp_string);

    if( realsize != size)
    {
        return 1;
    }
    return 0;
}

/***************************** ReadArguments ****************************/
/**  Reads the command line arguments and parses it for the various
options. Uses the same arguments as main.  Used to separate the command
line parsing from main and improve its readability.  This should also make
it easier to modify the command line parsing in the future.
\param [in] argc the number of arguments.
\param [in] argv an array of pointers to the arguments.
*/
static void ReadArguments(ISP_ENVIRONMENT *IspEnvironment, int argc, char *argv[])
{
    int i;

    if(argc >= 5)
    {
        for(i = 1; i < argc - 4; i++)
        {
            if(stricmp(argv[i], "-localecho") == 0)
            {
                IspEnvironment->LocalEcho = 1;
                DebugPrintf( 3, "Local echo in terminal mode.\n");
            }
            else if(stricmp(argv[i], "-wipe") == 0)
            {
                 IspEnvironment->WipeDevice = 1;
                 DebugPrintf( 3, "Wipe entire device before writing.\n");
            }
            else if(stricmp(argv[i], "-bin") == 0)
            {
                 IspEnvironment->FormatHex = 0;
                 DebugPrintf( 3, "Binary format file input.\n");
            }
            else if(stricmp(argv[i], "-hex") == 0)
            {
                 IspEnvironment->FormatHex = 1;
                 DebugPrintf( 3, "Hex format file input.\n");
            }
            else if(stricmp(argv[i], "-term") == 0)
            {
                 IspEnvironment->TerminalAfterUpload = 1;
                 DebugPrintf( 3, "Invoke terminal after upload.\n");
            }
            else if(stricmp(argv[i], "-termonly") == 0)
            {
                 IspEnvironment->TerminalOnly = 1;
                 DebugPrintf( 3, "Only provide terminal.\n");
            }
            else if(stricmp(argv[i], "-detectonly") == 0)
            {
                 IspEnvironment->DetectOnly = 1;
                 DebugPrintf( 3, "Only detect LPC chip part id.\n");
            }
            else if(stricmp(argv[i], "-debug") == 0)
            {
                 debug_level = 4;
                 DebugPrintf( 3, "Turn on debug.\n");
            }
            else if(stricmp(argv[i], "-control") == 0)
            {
                 IspEnvironment->ControlLines = 1;
                 DebugPrintf( 3, "Use RTS/DTR to control target state.\n");
            }
            else if(stricmp(argv[i], "-logfile") == 0)
            {
                 IspEnvironment->LogFile = 1;
                 DebugPrintf( 3, "Log terminal output.\n");
            }
            else if( stricmp( argv[i], "-ADARM") == 0)
            {
                IspEnvironment->micro = ANALOG_DEVICES_ARM;
                DebugPrintf( 2, "Target: Analog Devices.\n");
            }
            else if( stricmp( argv[i], "-PHILIPSARM") == 0)
            {
                IspEnvironment->micro = PHILIPS_ARM;
                DebugPrintf( 2, "Target: Philips.\n");
            }
            else if( stricmp( argv[i], "-Verify") == 0)
            {
          			IspEnvironment->Verify = 1;
                DebugPrintf( 2, "Verify after copy RAM to Flash.\n");
          	}
          	else
            {
                DebugPrintf( 2, "Unknown command line option: \"%s\"\n", argv[i]);
            }
        }

        IspEnvironment->input_file = argv[argc - 4];

        // Newest cygwin delivers a '\x0d' at the end of argument
        // when calling lpc21isp from batch file
        for(i = 0; i < strlen(argv[argc - 1]) && i < (sizeof(IspEnvironment->StringOscillator) - 1) &&
                   argv[argc - 1][i] >= '0' && argv[argc - 1][i] <= '9'; i++)
        {
            IspEnvironment->StringOscillator[i] = argv[argc - 1][i];
        }
        IspEnvironment->StringOscillator[i] = 0;

        IspEnvironment->serial_port = argv[argc - 3];
        IspEnvironment->baud_rate = argv[argc - 2];
    }

    if(argc < 5)
    {
        debug_level = (debug_level < 2) ? 2 : debug_level;
    }

    if(argc < 5)
    {
        DebugPrintf( 2, "\n");
        DebugPrintf( 2, "Portable command line ISP for Philips LPC2000 family and \n");
        DebugPrintf( 2, "Version " VERSION_STR "                 Analog Devices ADUC 70xx\n");
        DebugPrintf( 2, "Compiled for %s: %s %s\n", COMPILED_FOR, __DATE__, __TIME__);
        DebugPrintf( 2, "Copyright (c) by Martin Maurer, 2003-2007  Email: Martin.Maurer@clibb.de\n");
        DebugPrintf( 2, "Portions Copyright (c) by Aeolus Development 2004\n");
        DebugPrintf( 2, "                 http://www.aeolusdevelopment.com\n");
        DebugPrintf( 2, "\n");

        DebugPrintf( 1, "Syntax:  lpc21isp [Options] file comport baudrate Oscillator_in_kHz\n\n");
        DebugPrintf( 1, "Example: lpc21isp test.hex com1 115200 14746\n\n");
        DebugPrintf( 1, "Options: -bin           for uploading binary file\n");
        DebugPrintf( 1, "         -hex           for uploading file in intel hex format (default)\n");
        DebugPrintf( 1, "         -term          for starting terminal after upload\n");
        DebugPrintf( 1, "         -termonly      for starting terminal without an upload\n");
        DebugPrintf( 1, "         -localecho     for local echo in terminal\n");
        DebugPrintf( 1, "         -detectonly    detect only used LPC chiptype (PHILIPSARM only)\n");
        DebugPrintf( 1, "         -debug         for creating a lot of debug infos\n");
        DebugPrintf( 1, "         -wipe          Erase entire device before upload\n");
        DebugPrintf( 1, "         -control       for controlling RS232 lines for easier booting\n");
        DebugPrintf( 1, "                        (Reset = DTR, EnableBootLoader = RTS)\n");
        DebugPrintf( 1, "         -verify        Verify the data in Flash after every writes to\n");
        DebugPrintf( 1, "                        sector. To detect errors in writing to Flash ROM\n");
        DebugPrintf( 1, "         -logfile       for enabling logging of terminal output to lpc21isp.log\n");
        DebugPrintf( 1, "         -ADARM         for downloading to an Analog Devices\n");
        DebugPrintf( 1, "                        ARM microcontroller ADUC70xx\n");
        DebugPrintf( 1, "         -PHILIPSARM    for downloading to a microcontroller from\n");
        DebugPrintf( 1, "                        Philips LPC2000 family (default)\n");
        exit(1);
    }

    if(IspEnvironment->micro == PHILIPS_ARM)
    {
        // If StringOscillator is bigger than 100 MHz, there seems to be something wrong
        if (strlen(IspEnvironment->StringOscillator) > 5)
        {
            DebugPrintf( 1, "Invalid crystal frequency %s\n", IspEnvironment->StringOscillator);
            exit(1);
        }
    }
}

typedef enum
{
    PROGRAM_MODE,
    RUN_MODE
} TARGET_MODE;

/***************************** ResetTarget ******************************/
/**  Resets the target leaving it in either download (program) mode or
run mode.
\param [in] mode the mode to leave the target in.
*/
static void ResetTarget( ISP_ENVIRONMENT *IspEnvironment, TARGET_MODE mode)
{

    if(IspEnvironment->ControlLines)
    {

        switch( mode)
        {
                /* Reset and jump to boot loader.                       */
            case PROGRAM_MODE:
                ControlModemLines(IspEnvironment, 1, 0);
                Sleep(100);
                ClearSerialPortBuffers(IspEnvironment);
                Sleep(100);
                ControlModemLines(IspEnvironment, 0, 0);
                //Longer delay is the Reset signal is conected to an external rest controller
                Sleep(500);
                // Clear the RTS line after having reset the micro
                // Needed for the "GO <Address> <Mode>" ISP command to work */
                ControlModemLines(IspEnvironment, 0, 1);
                break;

                /* Reset and start uploaded program                     */
            case RUN_MODE:
                ControlModemLines(IspEnvironment, 1, 0);
                Sleep(100);
                ClearSerialPortBuffers(IspEnvironment);
                Sleep(100);
                ControlModemLines(IspEnvironment, 0, 0);
                Sleep(100);
                break;
        }
    }
}

/***************************** LoadFile *********************************/
/**  Loads the requested file to download into memory.
\param [in] IspEnvironment  structure containing input filename
*/
static void LoadFile(ISP_ENVIRONMENT *IspEnvironment)
{
    int fd;
    int i;
    int BinaryOffsetDefined;
    unsigned long  Pos;
    unsigned long  FileLength;
    BINARY        *FileContent;              /**< Used to store the content of a hex */
                                             /* file before converting to binary.    */
    unsigned long BinaryMemSize;

    fd = open(IspEnvironment->input_file, O_RDONLY | O_BINARY);
    if(fd == -1)
    {
        DebugPrintf( 1, "Can't open file %s\n", IspEnvironment->input_file);
        exit(1);
    }

    FileLength = lseek(fd, 0L, 2);      // Get file size

    if(FileLength == (size_t)-1)
    {
        DebugPrintf( 1, "\nFileLength = -1 !?!\n");
        exit(1);
    }

    lseek(fd, 0L, 0);

    FileContent = malloc(FileLength);

    BinaryMemSize = FileLength * 2;

    IspEnvironment->BinaryLength = 0;   /* Increase length as needed.       */
    IspEnvironment->BinaryOffset = 0;
    IspEnvironment->StartAddress = 0;
    BinaryOffsetDefined = 0;

    IspEnvironment->BinaryContent = malloc(BinaryMemSize);

    read(fd, FileContent, FileLength);

    close(fd);

    DebugPrintf( 2, "File %s:\n\tloaded...\n", IspEnvironment->input_file);

        // Intel-Hex -> Binary Conversion

    if(IspEnvironment->FormatHex != 0)
    {
        unsigned char  RecordLength;
        unsigned short RecordAddress;
        unsigned long  RealAddress = 0;
        unsigned char  RecordType;
        unsigned char  Hexvalue;
        unsigned long  StartAddress;

        DebugPrintf( 3, "Converting file %s to binary format...\n", IspEnvironment->input_file);

        Pos = 0;
        while(Pos < FileLength)
        {
            if(FileContent[Pos] == '\r')
            {
                Pos++;
                continue;
            }

            if(FileContent[Pos] == '\n')
            {
                Pos++;
                continue;
            }

            if(FileContent[Pos] != ':')
            {
                DebugPrintf( 1, "Missing start of record (':') wrong byte %c / %02X\n", FileContent[Pos], FileContent[Pos]);
                exit(1);
            }

            Pos++;

            RecordLength   = Ascii2Hex(FileContent[Pos++]);
            RecordLength <<= 4;
            RecordLength  |= Ascii2Hex(FileContent[Pos++]);

            DebugPrintf( 4, "RecordLength = %02X\n", RecordLength);

            RecordAddress   = Ascii2Hex(FileContent[Pos++]);
            RecordAddress <<= 4;
            RecordAddress  |= Ascii2Hex(FileContent[Pos++]);
            RecordAddress <<= 4;
            RecordAddress  |= Ascii2Hex(FileContent[Pos++]);
            RecordAddress <<= 4;
            RecordAddress  |= Ascii2Hex(FileContent[Pos++]);

            DebugPrintf( 4, "RecordAddress = %04X\n", RecordAddress);

            RealAddress = RealAddress - (RealAddress & 0xffff) + RecordAddress;

            DebugPrintf( 4, "RealAddress = %08lX\n", RealAddress);

            RecordType      = Ascii2Hex(FileContent[Pos++]);
            RecordType    <<= 4;
            RecordType     |= Ascii2Hex(FileContent[Pos++]);

            DebugPrintf( 4, "RecordType = %02X\n", RecordType);

            if(RecordType == 0x00)          // 00 - Data record
            {
                /*
                 * Binary Offset is defined as soon as first data record read
                 */
                BinaryOffsetDefined = 1;
                     // Memory for binary file big enough ?
                while(RealAddress + RecordLength - IspEnvironment->BinaryOffset > BinaryMemSize)
                {
                    BinaryMemSize <<= 1;
                    IspEnvironment->BinaryContent = realloc(IspEnvironment->BinaryContent, BinaryMemSize);
                }

                    // We need to know, what the highest address is,
                    // how many bytes / sectors we must flash
                if(RealAddress + RecordLength - IspEnvironment->BinaryOffset > IspEnvironment->BinaryLength)
                {
                    IspEnvironment->BinaryLength = RealAddress + RecordLength - IspEnvironment->BinaryOffset;
                    DebugPrintf( 3, "Image size now: %ld\n", IspEnvironment->BinaryLength);
                }

                for(i = 0; i < RecordLength; i++)
                {
                    Hexvalue        = Ascii2Hex(FileContent[Pos++]);
                    Hexvalue      <<= 4;
                    Hexvalue       |= Ascii2Hex(FileContent[Pos++]);
                    IspEnvironment->BinaryContent[RealAddress + i - IspEnvironment->BinaryOffset] = Hexvalue;
                }
            }
            else if(RecordType == 0x01)     // 01 - End of file record
            {
                break;
            }
            else if(RecordType == 0x02)     // 02 - Extended segment address record
            {
                for(i = 0; i < RecordLength * 2; i++)   // double amount of nibbles
                {
                    RealAddress <<= 4;
                    if(i == 0)
                    {
                        RealAddress  = Ascii2Hex(FileContent[Pos++]);
                    }
                    else
                    {
                        RealAddress |= Ascii2Hex(FileContent[Pos++]);
                    }
                }
                RealAddress <<= 4;
            }
            else if(RecordType == 0x03)     // 03 - Start segment address record
            {
                for(i = 0; i < RecordLength * 2; i++)   // double amount of nibbles
                {
                    RealAddress <<= 4;
                    if(i == 0)
                    {
                        RealAddress  = Ascii2Hex(FileContent[Pos++]);
                    }
                    else
                    {
                        RealAddress |= Ascii2Hex(FileContent[Pos++]);
                    }
                }
                RealAddress <<= 8;
            }
            else if(RecordType == 0x04)     // 04 - Extended linear address record, used by IAR
            {
                for(i = 0; i < RecordLength * 2; i++)   // double amount of nibbles
                {
                    RealAddress <<= 4;
                    if(i == 0)
                    {
                        RealAddress  = Ascii2Hex(FileContent[Pos++]);
                    }
                    else
                    {
                        RealAddress |= Ascii2Hex(FileContent[Pos++]);
                    }
                }
                RealAddress <<= 16;
                if(!BinaryOffsetDefined)
                {
                    // set startaddress of BinaryContent
                    // use of LPC_FLASHMASK to allow a memory range, not taking the first
                    // [04] record as actual start-address.
                    IspEnvironment->BinaryOffset = RealAddress & LPC_FLASHMASK;
                }
                else
                {
                    if((RealAddress & LPC_FLASHMASK) != IspEnvironment->BinaryOffset)
                    {
                        DebugPrintf(1, "New Extended Linear Address Record [04] out of memory range\n");
                        DebugPrintf(1, "Current Memory starts at: 0x%08X, new Address is: 0x%08X",
                                       IspEnvironment->BinaryOffset, RealAddress);
                        exit(1);
                    }
                }
            }
            else if(RecordType == 0x05)     // 05 - Start linear address record
            {
               StartAddress = 0;
               for(i = 0; i < RecordLength * 2; i++)   // double amount of nibbles
                {
                    StartAddress <<= 4;
                    if(i == 0)
                    {
                        StartAddress  = Ascii2Hex(FileContent[Pos++]);
                    }
                    else
                    {
                        StartAddress |= Ascii2Hex(FileContent[Pos++]);
                    }
                }
                DebugPrintf( 1,"Start Address = 0x%8X\n", StartAddress);
                IspEnvironment->StartAddress = StartAddress;
              }

            while(FileContent[Pos++] != 0x0a)      // Search till line end
            {
            }
        }

        DebugPrintf( 2, "\tconverted to binary format...\n");

            // When debugging is switched on, output result of conversion to file debugout.bin
        if(debug_level >= 4)
        {
            int fdout;
            fdout = open("debugout.bin", O_RDWR | O_BINARY | O_CREAT | O_TRUNC, 0777);
            write(fdout, IspEnvironment->BinaryContent, IspEnvironment->BinaryLength);
            close(fdout);
        }
    }
    else
    {
        memcpy(IspEnvironment->BinaryContent, FileContent, FileLength);

        IspEnvironment->BinaryLength = FileLength;
    }

    DebugPrintf( 2, "\timage size : %ld\n", IspEnvironment->BinaryLength);

        // check length to flash for correct alignment, can happen with broken ld-scripts
    if (IspEnvironment->BinaryLength % 4 != 0)
    {
        unsigned long NewBinaryLength = ((IspEnvironment->BinaryLength + 3)/4) * 4;

        DebugPrintf( 2, "Warning:  data not aligned to 32 bits, padded (length was %lX, now %lX)\n", IspEnvironment->BinaryLength, NewBinaryLength);

        IspEnvironment->BinaryLength = NewBinaryLength;
    }

}


#define ANALOG_DEVICES_SYNC_CHAR        ((BINARY)0x08)
#define ANALOG_DEVICES_SYNC_RESPONSE    ("ADuC")
#define ANALOG_DEVICES_SYNC_SIZE        (strlen( ANALOG_DEVICES_SYNC_RESPONSE))

typedef struct {
    BINARY product_id[15];
    BINARY version[3];
    BINARY reserved[4];
    BINARY terminator[2];
    } AD_SYNC_RESPONSE;

/***************************** AnalogDevicesSync ************************/
/**  Attempt to synchronize with an Analog Device ARM micro.  Sends a
backspace and reads back the microcontrollers response.  Performs
multiple retries. Exits the program on error, returns to caller in the
case of success.
*/
static void AnalogDevicesSync(ISP_ENVIRONMENT *IspEnvironment)
{
    BINARY sync;                        /* Holds sync command.          */
    AD_SYNC_RESPONSE response;          /* Response from micro.         */
    int sync_attempts;                  /* Number of retries.           */

        /*  Make sure we don't read garbage later instead of the        */
        /* response we expect from the micro.                           */
    ClearSerialPortBuffers(IspEnvironment);

    DebugPrintf( 2, "Synchronizing\n"); /* Progress report.             */

    sync = ANALOG_DEVICES_SYNC_CHAR;    /* Build up sync command.       */

        /*  Perform the actual sync attempt.  First send the sync       */
        /* character, the attempt to read back the response.  For the   */
        /* AD ARM micro this is a fixed length block.  If response is   */
        /* received attempt to validate it by comparing the first       */
        /* characters to those expected.  If the received block does    */
        /* not validate or is incomplete empty the serial buffer and    */
        /* retry.                                                       */
    for(sync_attempts = 0; sync_attempts < 5; sync_attempts++)
    {
        SendComPortBlock( IspEnvironment, &sync, 1);

        if( ReceiveComPortBlockComplete( IspEnvironment, &response, sizeof( response),
            500) == 0)
        {

            if( memcmp( response.product_id, ANALOG_DEVICES_SYNC_RESPONSE,
                ANALOG_DEVICES_SYNC_SIZE) == 0)
            {
                return;
            }
            else
            {
                DumpString( 3, &response, sizeof(response),
                    "Unexpected response to sync attempt ");
            }
        }
        else
        {
            DebugPrintf( 3, "No (or incomplete) answer on sync attempt\n");
        }

        ClearSerialPortBuffers(IspEnvironment);
    }

    DebugPrintf( 1, "No (or unacceptable) answer on sync attempt\n");
    exit(4);
}

typedef struct {
    char start1;
    char start2;
    BINARY bytes;
    char cmd;
    BINARY address_h;
    BINARY address_u;
    BINARY address_m;
    BINARY address_l;
    BINARY data[251];
    } AD_PACKET;

/***************************** AnalogDevicesFormPacket ******************/
/**  Create an Analog Devices communication packet from the constituent
elements.
\param [in] cmd The command being sent, one of 'E' for erase, 'W' for
write, 'V' for verify or 'R' for run..
\param [in] no_bytes the number of data bytes to send with the command in
the packet.
\param [in] address the address to apply the command to.
\param [in] data the data to send with the packet, may be null if no_bytes
is zero.
\param[out] packet that will be filled.
*/
static void AnalogDevicesFormPacket( ISP_ENVIRONMENT *IspEnvironment,
                                     char cmd, int no_bytes, unsigned int address,
                                     const void *data, AD_PACKET *packet)
{
    BINARY checksum;
    const BINARY *data_in;
    int i;

    (void)IspEnvironment; /* never used in this function */

        /*  Some sanity checking on the arguments.  These should only   */
        /* fail if there is a bug in the caller.                        */
        /*  Check 1) that the number of data bytes is in an acceptable  */
        /* range, 2) that we have a non-null pointer if data is being   */
        /* put in the packet and 3) that we have a non-null pointer to  */
        /* the packet to be filled. We just exit with an error message  */
        /* if any of these tests fail.                                  */
    if( (no_bytes < 0) || (no_bytes > 250))
    {
        DebugPrintf( 1,
            "The number of bytes (%d) passed to FormPacket is invalid.\n",
            no_bytes);
        exit( -1);
    }
    if( (data == 0) && (no_bytes != 0))
    {
        DebugPrintf( 1,
            "A null pointer to data paased to FormPacket when data was expected.\n");
        exit( -1);
    }
    if( packet == 0)
    {
        DebugPrintf( 1,
            "A null packet pointer was passed to FormPacket.\n");
        exit( -1);
    }

    checksum = 0;               /*  Checksum starts at zero.            */

    data_in = data;             /*  Pointer pun so we can walk through  */
                                /* the data.                            */

    packet->start1 = 0x7;       /*  The start of the packet is constant.*/
    packet->start2 = 0xE;

        /*  Fill in the rest of the packet and calculate the checksum   */
        /* as we go.                                                    */

        /* The number of bytes is the number of data bytes + the        */
        /* address bytes + the command byte.                            */
    packet->bytes = (BINARY)(no_bytes + 5);

    checksum += packet->bytes;

        /*  The command for the packet being sent.  No error checking   */
        /* done on this.                                                */
    packet->cmd = cmd;

    checksum += cmd;

        /*  Now break up the address and place in the proper packet     */
        /* locations.                                                   */
    packet->address_l = (BINARY)(address & 0xFF);
    packet->address_m = (BINARY)((address >> 8) & 0xFF);
    packet->address_u = (BINARY)((address >> 16) & 0xFF);
    packet->address_h = (BINARY)((address >> 24) & 0xFF);

    checksum += packet->address_l;
    checksum += packet->address_m;
    checksum += packet->address_u;
    checksum += packet->address_h;

        /*  Copy the data bytes into the packet.  We could use memcpy   */
        /* but we have to calculate the checksum anyway.                */
    for( i = 0; i < no_bytes; i++)
    {
        packet->data[i] = data_in[i];
        checksum += data_in[i];
    }

        /*  Finally, add the checksum to the end of the packet.         */
    packet->data[i] = (BINARY)-checksum;
}

#define ANALOG_DEVICES_ACK      0x6
#define ANALOG_DEVICES_NAK      0x7

/***************************** AnalogDevicesSendPacket ******************/
/**  Send a previously form Analog Devices communication.  Retry a
couple of times if needed but fail by exiting the program if no ACK is
forthcoming.
\param [in] packet the packet to send.
*/
static void AnalogDevicesSendPacket( ISP_ENVIRONMENT *IspEnvironment,
                                     const AD_PACKET * packet)
{
    BINARY response;
    int retry = 0;

    do {
        retry++;

                /*  Make sure we don't read garbage later instead of    */
                /* the response we expect from the micro.               */
        ClearSerialPortBuffers(IspEnvironment);

                /*  Send the packet, the size is the number of data     */
                /* bytes in the packet plus 3 bytes worth of header     */
                /* plus checksum.                                       */
        SendComPortBlock( IspEnvironment, packet, packet->bytes + 4);

                /*  Receive the response and check, return to caller    */
                /* if successful.                                       */
        if( ReceiveComPortBlockComplete( IspEnvironment, &response, 1, 5000) == 0)
        {
            if( response == ANALOG_DEVICES_ACK)
            {
                DebugPrintf( 3, "Packet Sent\n");
                return;
            }
            if( response != ANALOG_DEVICES_NAK)
            {
                DebugPrintf( 3, "Unexpected response to packet (%x)\n", (int)response);
            }
            DebugPrintf( 2, "*");
        }
    } while( retry < 3);

    DebugPrintf( 1, "Send packet failed\n");
    exit( -1);
}

/***************************** AnalogDevicesErase ***********************/
/**  Erase the Analog Devices micro.  We take the simple way out and
just erase the whole thing.
*/
static void AnalogDevicesErase(ISP_ENVIRONMENT *IspEnvironment)
{
    BINARY pages;
    AD_PACKET packet;

    pages = 0;
    DebugPrintf( 2, "Erasing .. ");
    AnalogDevicesFormPacket( IspEnvironment, 'E', 1, 0, &pages, &packet);
    AnalogDevicesSendPacket( IspEnvironment, &packet);
    DebugPrintf( 2, "Erased\n");
}

#define AD_PACKET_SIZE (250)

/***************************** AnalogDevicesWrite ***********************/
/**  Write the program.
\param [in] data the program to download to the micro.
\param [in] address where to start placing the program.
\param [in] bytes the size of the progrm to download.
*/
static void AnalogDevicesWrite( ISP_ENVIRONMENT *IspEnvironment,
                                const void *data, long address, size_t bytes)
{
    AD_PACKET packet;
    const BINARY *prog_data;

    DebugPrintf( 2, "Writing %d bytes ", bytes);
    prog_data = data;
    while( bytes > AD_PACKET_SIZE)
    {
        AnalogDevicesFormPacket( IspEnvironment, 'W', AD_PACKET_SIZE, address, prog_data, &packet);
        AnalogDevicesSendPacket( IspEnvironment, &packet);
        address += AD_PACKET_SIZE;
        prog_data += AD_PACKET_SIZE;
        bytes -= AD_PACKET_SIZE;
        DebugPrintf( 2, ".");
    }
    if( bytes > 0)
    {
        AnalogDevicesFormPacket( IspEnvironment, 'W', bytes, address, prog_data, &packet);
        AnalogDevicesSendPacket( IspEnvironment, &packet);
        DebugPrintf( 2, ".");
    }
}

/***************************** AnalogDevicesDownload ********************/
/**  Perform the download into an Analog Devices micro.  As a quick and
 * dirty hack against flash relocations at 0x80000
 * \return 0 if ok, error code else
 * \ToDo: possible to implement the return value instead of calling
 * exit() in sub-functions
 */
static int AnalogDevicesDownload(ISP_ENVIRONMENT *IspEnvironment)
{
    AnalogDevicesSync(IspEnvironment);
    AnalogDevicesErase(IspEnvironment);
    if( IspEnvironment->BinaryLength > 0x80000)
    {
        DebugPrintf( 2, "Note:  Flash remapped 0x80000 to 0.\n");
        AnalogDevicesWrite( IspEnvironment, IspEnvironment->BinaryContent + 0x80000, 0, IspEnvironment->BinaryLength-0x80000);
    }
    else
    {
        AnalogDevicesWrite( IspEnvironment, IspEnvironment->BinaryContent, 0, IspEnvironment->BinaryLength);
    }
    return(0);
}

/* note -- need to rationalize timeouts, use define?
similarly for the number of sync attempts
*/

#endif // !defined COMPILE_FOR_LPC21


static int SendAndVerify(ISP_ENVIRONMENT *IspEnvironment, const char *Command,
                         char *AnswerBuffer, int AnswerLength)
{
    unsigned long realsize;
    int cmdlen;

    SendComPort( IspEnvironment, Command);
    ReceiveComPort( IspEnvironment, AnswerBuffer, AnswerLength-1, &realsize, 2, 5000);
    cmdlen = strlen(Command);
    return (strncmp(AnswerBuffer, Command, cmdlen) == 0
            && strcmp(AnswerBuffer + cmdlen, "0\r\n") == 0);
}

/***************************** PHILIPS Download *********************************/
/**  Download the file from the internal memory image to the philips microcontroller.
 *   This function is visible from outside if COMPILE_FOR_LPC21
 */

#if !defined COMPILE_FOR_LPC21
static
#endif
int PhilipsDownload(ISP_ENVIRONMENT *IspEnvironment)
{
    unsigned long realsize;
    signed char Answer[128];
    char temp[128];
    char *strippedAnswer, *endPtr;
    int  strippedsize;
    int nQuestionMarks;
    int found;
    unsigned long Sector;
    unsigned long SectorLength;
    unsigned long SectorStart, SectorOffset, SectorChunk;
    char tmpString[128];
    char uuencode_table[64];
    int Line;
    unsigned long tmpStringPos;
    unsigned long BlockOffset;
    unsigned long Block;
    unsigned long Pos;
    unsigned long CopyLength;
    int c,k=0,i;
    unsigned long ivt_CRC;          // CRC over interrupt vector table
    unsigned long block_CRC;
    time_t tStartUpload=0, tDoneUpload=0;
    long WatchDogSeconds = 0;
    int WaitForWatchDog = 0;
    char tmp_string[64];

    if(!IspEnvironment->DetectOnly)
    {
            // Build up uuencode table
        uuencode_table[0] = 0x60;           // 0x20 is translated to 0x60 !

        for(i = 1; i < 64; i++)
        {
            uuencode_table[i] = (char)(0x20 + i);
        }

            // Patch 0x14, otherwise it is not running and jumps to boot mode

        ivt_CRC = 0;

            /* Clear the vector at 0x14 so it doesn't affect the checksum: */
        for(i = 0; i < 4; i++)
        {
            IspEnvironment->BinaryContent[i + 0x14] = 0;
        }
            /* Calculate a native checksum of the little endian vector table: */
        for(i = 0; i < (4 * 8);) {
            ivt_CRC += IspEnvironment->BinaryContent[i++];
            ivt_CRC += IspEnvironment->BinaryContent[i++] << 8;
            ivt_CRC += IspEnvironment->BinaryContent[i++] << 16;
            ivt_CRC += IspEnvironment->BinaryContent[i++] << 24;
        }

            /* Negate the result and place in the vector at 0x14 as little endian
             * again. The resulting vector table should checksum to 0. */
        ivt_CRC = (unsigned long)-ivt_CRC;
        for(i = 0; i < 4; i++)
        {
            IspEnvironment->BinaryContent[i + 0x14] = (unsigned char)(ivt_CRC >> (8 * i));
        }

        DebugPrintf( 3, "Position 0x14 patched: ivt_CRC = 0x%08lX\n", ivt_CRC);
    }

    DebugPrintf( 2, "Synchronizing (ESC to abort)");

    PrepareKeyboardTtySettings();

    for(nQuestionMarks = found = 0; !found && nQuestionMarks < 100; nQuestionMarks++)
    {
        if(kbhit())
        {
          if(getch() == 0x1b)
          {
            ResetKeyboardTtySettings();
            DebugPrintf( 2, "\nUser aborted during synchronisation\n");
            return( USER_ABORT_SYNC );
          }
        }

        DebugPrintf( 2, ".");
        SendComPort( IspEnvironment, "?");

        memset(Answer,0,sizeof(Answer));
        ReceiveComPort( IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 1,100);

        strippedAnswer = Answer;
        strippedsize = realsize;
        while((strippedsize > 0) && ((*strippedAnswer == '?') || (*strippedAnswer == 0)))
        {
            strippedAnswer++;
            strippedsize--;
        }

        sprintf(tmp_string, "StrippedAnswer(Length=%ld): '", strippedsize);
        DumpString( 3, strippedAnswer, strippedsize, tmp_string);

        if(strcmp(strippedAnswer, "Bootloader\r\n") == 0 && IspEnvironment->TerminalOnly == 0)
        {
            long chars, xtal;
            unsigned long ticks;
            chars = (17 * IspEnvironment->BinaryLength + 1) / 10;
            WatchDogSeconds = (10 * chars + 5) / atol(IspEnvironment->baud_rate) + 10;
            xtal = atol(IspEnvironment->StringOscillator) * 1000;
            ticks = (unsigned long)WatchDogSeconds * ((xtal + 15) / 16);
            DebugPrintf( 2, "Entering ISP; re-synchronizing (watchdog = %ld seconds)\n", WatchDogSeconds);
            sprintf(temp, "T %lu\r\n", ticks);
            SendComPort( IspEnvironment, temp);
            ReceiveComPort( IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 1,100);
            if(strcmp(Answer, "OK\r\n") != 0)
            {
                ResetKeyboardTtySettings();
                DebugPrintf( 2, "No answer on 'watchdog timer set'\n");
                return( NO_ANSWER_WDT );
            }
            SendComPort( IspEnvironment, "G 10356\r\n");
            Sleep(200);
            nQuestionMarks = 0;
            WaitForWatchDog = 1;
            continue;
        }

        tStartUpload = time(NULL);

        if(strcmp(strippedAnswer, "Synchronized\r\n") == 0)
        {
            found = 1;
        }
#if !defined COMPILE_FOR_LPC21
        else
        {
            ResetTarget(IspEnvironment, PROGRAM_MODE );
        }
#endif
    }

    ResetKeyboardTtySettings();

    if(!found)
    {
        DebugPrintf( 1, " no answer on '?'\n");
        return( NO_ANSWER_QM );
    }

    DebugPrintf( 2, " OK\n");

    SendComPort( IspEnvironment, "Synchronized\r\n");

    ReceiveComPort( IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 2,1000);

    if ((strcmp(Answer, "Synchronized\r\nOK\r\n") != 0) && (strcmp(Answer, "Synchronized\rOK\r\n") != 0))
    {
        DebugPrintf( 1, "No answer on 'Synchronized'\n");
        return( NO_ANSWER_SYNC );
    }

    DebugPrintf( 3, "Synchronized 1\n");

    DebugPrintf( 3, "Setting oscillator\n");

    sprintf( temp, "%s\r\n", IspEnvironment->StringOscillator);
    SendComPort( IspEnvironment, temp);

    ReceiveComPort( IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 2,1000);

    sprintf( temp, "%s\r\nOK\r\n", IspEnvironment->StringOscillator);

    if(strcmp(Answer, temp) != 0)
    {
        DebugPrintf( 1, "No answer on Oscillator-Command\n");
        return( NO_ANSWER_OSC );
    }

    DebugPrintf( 3, "Unlock\n");

    if (!SendAndVerify(IspEnvironment, "U 23130\r\n", Answer, sizeof Answer))
    {
        DebugPrintf( 1, "Unlock-Command:\n");
        PhilipsOutputErrorMessage(GetErrorNumber(Answer));
        return( UNLOCK_ERROR + GetErrorNumber(Answer) );
    }

    DebugPrintf( 2, "Read bootcode version: ");

    SendComPort( IspEnvironment, "K\r\n");

    ReceiveComPort( IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 4,5000);

    if(strncmp(Answer, "K\r\n", 3) != 0)
    {
        DebugPrintf( 1, "no answer on Read Boot Code Version\n");
        return( NO_ANSWER_RBV );
    }

    if(strncmp(Answer, "K\r\n0\r\n", 6) == 0)
    {
        strippedAnswer = Answer + 6;
        /*
        int maj, min, build;
        if (sscanf(strippedAnswer, "%d %d %d", &build, &min, &maj) == 2) {
            maj = min;
            min = build;
            build = 0;
        } // if
        DebugPrintf( 2, "%d.%d.%d\n", maj, min, build);
        */
        DebugPrintf( 2, strippedAnswer );
    }
    else
    {
        DebugPrintf( 2, "unknown\n");
    }

    DebugPrintf( 2, "Read part ID: ");
    SendComPort( IspEnvironment, "J\r\n");

    ReceiveComPort( IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 3,5000);

    if(strncmp(Answer, "J\r\n", 3) != 0)
    {
        DebugPrintf( 1, "no answer on Read Part Id\n");
        return( NO_ANSWER_RPID );
    }

    strippedAnswer = (strncmp(Answer, "J\r\n0\r\n", 6) == 0) ? Answer + 6 : Answer;
    Pos = strtoul(strippedAnswer, &endPtr, 10);
    *endPtr = '\0'; /* delete \r\n */
    for (i = sizeof LPCtypes / sizeof LPCtypes[0] - 1; i > 0 && LPCtypes[i].id != Pos; i--)
        /* nothing */;
    IspEnvironment->DetectedDevice = i;
    if( IspEnvironment->DetectedDevice == 0 ) {
        DebugPrintf( 2, "unknown");
    }
    else {
        DebugPrintf( 2, "LPC%d, %d kiB ROM / %d kiB SRAM",
                        LPCtypes[IspEnvironment->DetectedDevice].Product,
                        LPCtypes[IspEnvironment->DetectedDevice].FlashSize,
                        LPCtypes[IspEnvironment->DetectedDevice].RAMSize);
    }
    DebugPrintf( 2, " (%s)\n", strippedAnswer);

    /* In case of a download to RAM, use full RAM for downloading
     * set the flash parameters to full RAM also.
     * This makes sure that all code is downloaded as one big sector
     */

    if(IspEnvironment->BinaryOffset >= LPC_RAMSTART)
    {
        LPCtypes[IspEnvironment->DetectedDevice].FlashSectors = 1;
        LPCtypes[IspEnvironment->DetectedDevice].MaxCopySize  = LPCtypes[IspEnvironment->DetectedDevice].RAMSize*1024 - (LPC_RAMBASE - LPC_RAMSTART);
        LPCtypes[IspEnvironment->DetectedDevice].SectorTable  = SectorTable_RAM;
        SectorTable_RAM[0] = LPCtypes[IspEnvironment->DetectedDevice].MaxCopySize;
    }
    if (IspEnvironment->DetectOnly)
        return(0);


        // Start with sector 1 and go upward... Sector 0 containing the interrupt vectors
        // will be loaded last, since it contains a checksum and device will re-enter
        // bootloader mode as long as this checksum is invalid.
    DebugPrintf(2, "Will start programming at Sector 1 if possible, and conclude with Sector 0 to ensure that checksum is written last.\n");
    if( LPCtypes[IspEnvironment->DetectedDevice].SectorTable[0] >= IspEnvironment->BinaryLength )
    {
        Sector = 0;
        SectorStart = 0;
    }
    else
    {
        SectorStart = LPCtypes[IspEnvironment->DetectedDevice].SectorTable[0];
        Sector = 1;
    }

    if( IspEnvironment->WipeDevice == 1 )
    {
            DebugPrintf( 2, "Wiping Device. ");
            sprintf(tmpString, "P %ld %ld\r\n", 0, LPCtypes[IspEnvironment->DetectedDevice].FlashSectors-1);
            if (!SendAndVerify(IspEnvironment, tmpString, Answer, sizeof Answer))
            {
                DebugPrintf( 1, "Wrong answer on Prepare-Command\n");
                PhilipsOutputErrorMessage(GetErrorNumber(Answer));
                return( WRONG_ANSWER_PREP + GetErrorNumber(Answer) );
            }

            sprintf(tmpString, "E %ld %ld\r\n", 0, LPCtypes[IspEnvironment->DetectedDevice].FlashSectors-1);
            if (!SendAndVerify(IspEnvironment, tmpString, Answer, sizeof Answer))
            {
                DebugPrintf( 1, "Wrong answer on Erase-Command\n");
                PhilipsOutputErrorMessage(GetErrorNumber(Answer));
                return( WRONG_ANSWER_ERAS + GetErrorNumber(Answer) );
            }
            DebugPrintf( 2, "OK \n");
    }
    while( 1 )
    {
        if (Sector >= LPCtypes[IspEnvironment->DetectedDevice].FlashSectors)
        {
            DebugPrintf( 1, "Program too large; running out of Flash sectors.\n");
            return( PROGRAM_TOO_LARGE );
        }

        DebugPrintf(2, "Sector %ld: ", Sector);
        fflush(stdout);

        if(IspEnvironment->BinaryOffset < LPC_RAMSTART) // Skip Erase when running from RAM
        {
            sprintf(tmpString, "P %ld %ld\r\n", Sector, Sector);
            if (!SendAndVerify(IspEnvironment, tmpString, Answer, sizeof Answer))
            {
                DebugPrintf( 1, "Wrong answer on Prepare-Command (1) (Sector %ld)\n", Sector);
                PhilipsOutputErrorMessage(GetErrorNumber(Answer));
                return( WRONG_ANSWER_PREP + GetErrorNumber(Answer) );
            }

            DebugPrintf( 2, ".");
            fflush(stdout);
						if( IspEnvironment->WipeDevice == 0 )
						{
		            sprintf(tmpString, "E %ld %ld\r\n", Sector, Sector);
		            if (!SendAndVerify(IspEnvironment, tmpString, Answer, sizeof Answer))
		            {
		                DebugPrintf( 1, "Wrong answer on Erase-Command (Sector %ld)\n", Sector);
		                PhilipsOutputErrorMessage(GetErrorNumber(Answer));
		                return( WRONG_ANSWER_ERAS + GetErrorNumber(Answer) );
		            }

		            DebugPrintf( 2, ".");
		            fflush(stdout);
		        }
        }

        SectorLength = LPCtypes[IspEnvironment->DetectedDevice].SectorTable[Sector];
        if(SectorLength > IspEnvironment->BinaryLength - SectorStart)
        {
            SectorLength = IspEnvironment->BinaryLength - SectorStart;
        }

        for(SectorOffset = 0; SectorOffset < SectorLength; SectorOffset += SectorChunk)
        {
            if (SectorOffset > 0)
            {
                // Add a visible marker between segments in a sector
                DebugPrintf(2, "|");  /* means: partial segment copied */
                fflush(stdout);
            }

                // If the Flash ROM sector size is bigger than the number of bytes
                // we can copy from RAM to Flash, we must "chop up" the sector and
                // copy these individually.
                // This is especially needed in the case where a Flash sector is
                // bigger than the amount of SRAM.
            SectorChunk = SectorLength - SectorOffset;
            if(SectorChunk > (unsigned)LPCtypes[IspEnvironment->DetectedDevice].MaxCopySize)
            {
                SectorChunk = LPCtypes[IspEnvironment->DetectedDevice].MaxCopySize;
            }

                // Write multiple of 45 * 4 Byte blocks to RAM, but copy maximum of on sector to Flash
                // In worst case we transfer up to 180 byte to much to RAM
                // but then we can always use full 45 byte blocks and length is multiple of 4
            CopyLength = SectorChunk;
            if((CopyLength % (45 * 4)) != 0)
            {
                CopyLength += ((45 * 4) - (CopyLength % (45 * 4)));
            }
            sprintf(tmpString, "W %ld %ld\r\n", LPC_RAMBASE, CopyLength);

            if (!SendAndVerify(IspEnvironment, tmpString, Answer, sizeof Answer))
            {
                DebugPrintf( 1, "Wrong answer on Write-Command\n");
                PhilipsOutputErrorMessage(GetErrorNumber(Answer));
                return( WRONG_ANSWER_WRIT + GetErrorNumber(Answer) );
            }

            DebugPrintf( 2, ".");
            fflush(stdout);

            block_CRC = 0;
            Line = 0;

                // Transfer blocks of 45 * 4 bytes to RAM
            for(Pos = SectorStart + SectorOffset; (Pos < SectorStart + SectorOffset + CopyLength) && (Pos < IspEnvironment->BinaryLength); Pos += (45 * 4))
            {
                for(Block = 0; Block < 4; Block++)  // Each block 45 bytes
                {
                    DebugPrintf( 2, ".");
                    fflush(stdout);

                        // Uuencode one 45 byte block
                    tmpStringPos = 0;

                    tmpString[tmpStringPos++] = (char)(' ' + 45);  // Encode Length of block

                    for(BlockOffset = 0; BlockOffset < 45; BlockOffset++)
                    {
                        if(IspEnvironment->BinaryOffset < LPC_RAMSTART)
                        { // Flash: use full memory
                            c = IspEnvironment->BinaryContent[Pos + Block * 45 + BlockOffset];
                        }
                        else
                        { // RAM: Skip first 0x200 bytes, these are used by the download program in LPC21xx
                            c = IspEnvironment->BinaryContent[Pos + Block * 45 + BlockOffset + 0x200];
                        }

                        block_CRC += c;

                        k = (k << 8) + (c & 255);

                        if((BlockOffset % 3) == 2)   // Collecting always 3 Bytes, then do processing in 4 Bytes
                        {
                            tmpString[tmpStringPos++] = uuencode_table[(k >> 18) & 63];
                            tmpString[tmpStringPos++] = uuencode_table[(k >> 12) & 63];
                            tmpString[tmpStringPos++] = uuencode_table[(k >>  6) & 63];
                            tmpString[tmpStringPos++] = uuencode_table[ k        & 63];
                        }
                    }

                    tmpString[tmpStringPos++] = '\r';
                    tmpString[tmpStringPos++] = '\n';
                    tmpString[tmpStringPos++] = 0;

                    SendComPort( IspEnvironment, tmpString);

                    ReceiveComPort( IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 1,5000);

                    if(strncmp(Answer, tmpString, tmpStringPos) != 0)
                    {
                        DebugPrintf( 1, "Error on writing data (1)\n");
                        return( ERROR_WRITE_DATA );
                    }

                    Line++;

                    DebugPrintf( 3, "Line = %d\n", Line);

                    if(Line == 20)
                    {
                            // printf("block_CRC = %ld\n", block_CRC);

                        sprintf(tmpString, "%ld\r\n", block_CRC);
                        SendComPort( IspEnvironment, tmpString);

                        ReceiveComPort( IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 2,5000);

                        sprintf(tmpString, "%ld\r\nOK\r\n", block_CRC);
                        if(strcmp(Answer, tmpString) != 0)
                        {
                            DebugPrintf( 1, "Error on writing block_CRC (1)\n");
                            return( ERROR_WRITE_CRC );
                        }

                        Line = 0;
                        block_CRC = 0;
                    }
                }
            }

            if(Line != 0)
            {
                sprintf(tmpString, "%ld\r\n", block_CRC);
                SendComPort( IspEnvironment, tmpString);

                ReceiveComPort( IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 2,5000);

                sprintf(tmpString, "%ld\r\nOK\r\n", block_CRC);
                if(strcmp(Answer, tmpString) != 0)
                {
                    DebugPrintf( 1, "Error on writing block_CRC (2)\n");
                    return( ERROR_WRITE_CRC2 );
                }
            }

            if(IspEnvironment->BinaryOffset < LPC_RAMSTART)
            {
                    // Prepare command must be repeated before every write
                sprintf(tmpString, "P %ld %ld\r\n", Sector, Sector);
                if (!SendAndVerify(IspEnvironment, tmpString, Answer, sizeof Answer))
                {
                    DebugPrintf( 1, "Wrong answer on Prepare-Command (2) (Sector %ld)\n", Sector);
                    PhilipsOutputErrorMessage(GetErrorNumber(Answer));
                    return( WRONG_ANSWER_PREP2 + GetErrorNumber(Answer) );
                }

                   // Round CopyLength up to one of the following values: 512, 1024,
                   // 4096, 8192; but do not exceed the maximum copy size (usually
                   // 8192, but chip-dependent)
                if(CopyLength < 512)
                {
                    CopyLength = 512;
                }
                else if(SectorLength < 1024)
                {
                    CopyLength = 1024;
                }
                else if(SectorLength < 4096)
                {
                    CopyLength = 4096;
                }
                else
                {
                    CopyLength = 8192;
                }
                if(CopyLength > (unsigned)LPCtypes[IspEnvironment->DetectedDevice].MaxCopySize)
                {
                    CopyLength = LPCtypes[IspEnvironment->DetectedDevice].MaxCopySize;
                }

                sprintf(tmpString, "C %ld %ld %ld\r\n", SectorStart + SectorOffset, LPC_RAMBASE, CopyLength);
                if (!SendAndVerify(IspEnvironment, tmpString, Answer, sizeof Answer))
                {
                    DebugPrintf( 1, "Wrong answer on Copy-Command\n");
                    PhilipsOutputErrorMessage(GetErrorNumber(Answer));
                    return( WRONG_ANSWER_COPY + GetErrorNumber(Answer) );
                }
                if (IspEnvironment->Verify)
                {

                		//Avoid compare first 64 bytes.
                		//Because first 64 bytes are re-mapped to flash boot sector,
                		//and the compare result may not be correct.
                		if (SectorStart + SectorOffset<64)
		                		sprintf(tmpString, "M %ld %ld %ld\r\n", 64, LPC_RAMBASE + (64 - SectorStart - SectorOffset), CopyLength-(64 - SectorStart - SectorOffset));
		                else
		                		sprintf(tmpString, "M %ld %ld %ld\r\n", SectorStart + SectorOffset, LPC_RAMBASE, CopyLength);
		                if (!SendAndVerify(IspEnvironment, tmpString, Answer, sizeof Answer))
		                {
		                    DebugPrintf( 1, "Wrong answer on Compare-Command\n");
		                    PhilipsOutputErrorMessage(GetErrorNumber(Answer));
		                    return( WRONG_ANSWER_COPY + GetErrorNumber(Answer) );
		                }
		            }
            }

        }

        DebugPrintf(2, "\n");
        fflush(stdout);

        if((SectorStart + SectorLength) >= IspEnvironment->BinaryLength && Sector!=0)
        {
            Sector = 0;
            SectorStart = 0;
        }
        else if( Sector == 0 ) {
            break;
        }
        else {
            SectorStart += LPCtypes[IspEnvironment->DetectedDevice].SectorTable[Sector];
            Sector++;
        }
    }

    tDoneUpload = time(NULL);
    if (IspEnvironment->Verify)
        DebugPrintf( 2, "Download Finished and Verified correct... taking %d seconds\n", tDoneUpload - tStartUpload);
    else
	    DebugPrintf( 2, "Download Finished... taking %d seconds\n", tDoneUpload - tStartUpload);

    if (WaitForWatchDog)
    {
        DebugPrintf( 2, "Wait for restart, in %d seconds from now\n", WatchDogSeconds - (tDoneUpload - tStartUpload));
    }
    else
    {
        DebugPrintf( 2, "Now launching the brand new code\n" );
        fflush(stdout);

        sprintf(tmpString, "G %ld A\r\n", IspEnvironment->StartAddress);
        SendComPort(IspEnvironment, tmpString); //goto 0 : run this fresh new downloaded code code
        if(IspEnvironment->BinaryOffset < LPC_RAMSTART)
        { // Skip response on G command - show response on Terminal instead
            ReceiveComPort(IspEnvironment, Answer, sizeof(Answer)-1, &realsize, 2, 5000);
            /* the reply string is frequently terminated with a -1 (EOF) because the
             * connection gets broken; zero-terminate the string ourselves
             */
            while (realsize > 0 && Answer[(int)realsize - 1] < 0)
                realsize--;
            Answer[(int)realsize] = '\0';
            /* Better to check only the first 9 chars instead of complete receive buffer,
             * because the answer can contain the output by the started programm
             */
            if(realsize == 0 || strncmp((const char *)Answer, "G 0 A\r\n0\r", 9) != 0)
            {
                DebugPrintf( 2, "Failed to run the new downloaded code: ");
                PhilipsOutputErrorMessage(GetErrorNumber(Answer));
                return( FAILED_RUN + GetErrorNumber(Answer) );
            }
        }
        fflush(stdout);
    }
    return(0);
}


#if !defined COMPILE_FOR_LPC21
/***************************** Terminal *********************************/
/**  Acts as a simple dumb terminal. Press 'ESC' to exit.
*/
static void Terminal( ISP_ENVIRONMENT *IspEnvironment)
{
    int           ch = 0;
    char buffer[128];
    int           fdlogfile = -1;
    unsigned long realsize;

        // When logging is switched on, output terminal output to lpc21isp.log
    if(IspEnvironment->LogFile)
    {
        fdlogfile = open("lpc21isp.log", O_RDWR | O_BINARY | O_CREAT | O_TRUNC, 0777);
    }

    PrepareKeyboardTtySettings();

    DebugPrintf( 1, "Terminal started (press Escape to abort)\n\n");
    fflush(stdout);

    do
    {
            // Check for received characters

        ReceiveComPort( IspEnvironment, buffer, sizeof(buffer) - 1, &realsize, 0,200);

        if( realsize )
        {
            write(1, buffer, realsize);
            fflush(stdout);
            if(IspEnvironment->LogFile)     // When logging is turned on, then copy output to logfile
            {
                write(fdlogfile, buffer, realsize);
            }
        }

            // check for keypress, and write any out the port.
        if ( kbhit() )
        {
            ch = getch();
            if(ch == 0x1b)
            {
                break;
            }
            buffer[0] = (unsigned char)ch;
            buffer[1] = 0;

            if(IspEnvironment->LocalEcho)
            {
                write(1, buffer, 1);
            }

            SendComPort( IspEnvironment, buffer);
        }
    } while(ch != 0x1b);

    DebugPrintf( 1, "\n\nTerminal stopped\n\n");
    fflush(stdout);

    ResetKeyboardTtySettings();

    if(IspEnvironment->LogFile)
    {
        close(fdlogfile);
    }

}

#endif // !defined COMPILE_FOR_LPC21

/***************************** main *************************************/
/**  main. Everything starts from here.
\param [in] argc the number of arguments.
\param [in] argv an array of pointers to the arguments.
*/

#if !defined COMPILE_FOR_LPC21

int main(int argc, char *argv[])
{
    ISP_ENVIRONMENT IspEnvironment;
    int downloadResult = -1;

        // Initialize debug level
    debug_level = 2;

        // Initialize ISP Environment
    IspEnvironment.micro = PHILIPS_ARM;          /**< The type of micro that will be */
                                                 /* programmed.                      */
    IspEnvironment.TerminalAfterUpload = 0;
    IspEnvironment.TerminalOnly = 0;
    IspEnvironment.DetectOnly = 0;
    IspEnvironment.DetectedDevice = 0;          /* index in LPCtypes[] */
    IspEnvironment.FormatHex = 1;
    IspEnvironment.ControlLines = 0;
    IspEnvironment.LogFile = 0;
    IspEnvironment.WipeDevice = 0;
    IspEnvironment.LocalEcho = 0;

        /* Read and parse the command line.                             */
    ReadArguments( &IspEnvironment, argc, argv);
    DebugPrintf( 2, "lpc21isp version " VERSION_STR "\n");

        /* Download requested, read in the input file.                  */
    if(!IspEnvironment.TerminalOnly && !IspEnvironment.DetectOnly)
    {
        LoadFile(&IspEnvironment);
    }

    OpenSerialPort(&IspEnvironment);   /* Open the serial port to the microcontroller. */

    ResetTarget(&IspEnvironment, PROGRAM_MODE);

    ClearSerialPortBuffers(&IspEnvironment);


        /* Perform the requested download.                              */
    if(!IspEnvironment.TerminalOnly)
    {
        switch( IspEnvironment.micro)
        {
            case PHILIPS_ARM:
                downloadResult = PhilipsDownload(&IspEnvironment);
                break;

            case ANALOG_DEVICES_ARM:
                downloadResult = AnalogDevicesDownload(&IspEnvironment);
                break;
        }
        if( downloadResult != 0 )
        {
            CloseSerialPort(&IspEnvironment);
            exit(downloadResult);
        }
    }

    if(IspEnvironment.StartAddress == 0 || IspEnvironment.TerminalOnly)
    {
        /* Only reset target if startaddress = 0
         * Otherwise stay with the running program as started in Download()
         */
        ResetTarget(&IspEnvironment, RUN_MODE);
    }

    IspEnvironment.debug_level = 1;    /* From now on there is no more debug output !! */
                                       /* Therefore switch it off...                   */

        /* User asked for terminal emulation, provide a really dumb     */
        /* terminal.                                                    */
    if(IspEnvironment.TerminalAfterUpload || IspEnvironment.TerminalOnly)
    {
        Terminal(&IspEnvironment);
    }

    CloseSerialPort(&IspEnvironment);  /*  All done, close the serial port to the      */
    return(0);                         /* microcontroller and exit.                    */
}

#endif // !defined COMPILE_FOR_LPC21

/***************************** DumpString ******************************/
/**  Prints an area of memory to stdout. Converts non-printables to hex.
\param [in] level the debug level of the block to be dumped.  If this is
less than or equal to the current debug level than the dump will happen
otherwise this just returns.
\param [in] b pointer to an area of memory.
\param [in] size the length of the memory block to print.
\param [in] prefix string is a pointer to a prefix string.
*/
static void DumpString(int level, const void *b, size_t size, const char *prefix_string)
{
    size_t i;
    const char * s = b;
    unsigned char c;

    DebugPrintf( level, prefix_string );

    DebugPrintf( level, "'" );
    for( i = 0; i < size; i++)
    {
        c = s[i];
        if(c >= 0x20 && c <= 0x7e) /*isprint?*/
        {
            DebugPrintf( level, "%c", c);
        }
        else
        {
            DebugPrintf( level, "(%02X)", c);
        }
    }
    DebugPrintf( level, "'\n" );
}


#if !defined COMPILE_FOR_LPC21
/***************************** Ascii2Hex ********************************/
/**  Converts a hex character to its equivalent number value. In case of an
error rather abruptly terminates the program.
\param [in] c the hex digit to convert.
\return the value of the hex digit.
*/
static unsigned char Ascii2Hex(unsigned char c)
{
    if(c >= '0' && c <= '9')
    {
        return(unsigned char)(c - '0');
    }
    if(c >= 'A' && c <= 'F')
    {
        return(unsigned char)(c - 'A' + 10);
    }
    if(c >= 'a' && c <= 'f')
    {
        return(unsigned char)(c - 'a' + 10);
    }
    DebugPrintf( 1, "Wrong Hex-Nibble %c (%02X)\n", c, c);
    exit(1);
    return(0);  // this "return" will never be reached, but some compilers give a warning if it is not present
}
#endif // !defined COMPILE_FOR_LPC21


/***************************** PhilipsOutputErrorMessage ***********************/
/**  Given an error number find and print the appropriate error message.
\param [in] ErrorNumber The number of the error.
*/
static void PhilipsOutputErrorMessage(unsigned char ErrorNumber)
{
#if !defined COMPILE_FOR_LPC21
    switch(ErrorNumber)
    {
        case   0:
            DebugPrintf( 1, "CMD_SUCCESS");
            break;

        case   1:
            DebugPrintf( 1, "INVALID_COMMAND");
            break;

        case   2:
            DebugPrintf( 1, "SRC_ADDR_ERROR: Source address is not on word boundary.");
            break;
        case   3:
            DebugPrintf( 1, "DST_ADDR_ERROR: Destination address is not on a correct boundary.");
            break;

        case   4:
            DebugPrintf( 1, "SRC_ADDR_NOT_MAPPED: Source address is not mapped in the memory map.\n");
            DebugPrintf( 1, "                     Count value is taken into consideration where applicable.");
            break;

        case   5:
            DebugPrintf( 1, "DST_ADDR_NOT_MAPPED: Destination address is not mapped in the memory map.\n");
            DebugPrintf( 1, "                     Count value is taken into consideration where applicable.");
            break;

        case   6:
            DebugPrintf( 1, "COUNT_ERROR: Byte count is not multiple of 4 or is not a permitted value.");
            break;

        case   7:
            DebugPrintf( 1, "INVALID_SECTOR: Sector number is invalid or end sector number is\n");
            DebugPrintf( 1, "                greater than start sector number.");
            break;

        case   8:
            DebugPrintf( 1, "SECTOR_NOT_BLANK");
            break;

        case   9:
            DebugPrintf( 1, "SECTOR_NOT_PREPARED_FOR_WRITE_OPERATION:\n");
            DebugPrintf( 1, "Command to prepare sector for write operation was not executed.");
            break;

        case  10:
            DebugPrintf( 1, "COMPARE_ERROR: Source and destination data not equal.");
            break;

        case  11:
            DebugPrintf( 1, "BUSY: Flash programming hardware interface is busy.");
            break;

        case  12:
            DebugPrintf( 1, "PARAM_ERROR: Insufficient number of parameters or invalid parameter.");
            break;

        case  13:
            DebugPrintf( 1, "ADDR_ERROR: Address is not on word boundary.");
            break;

        case  14:
            DebugPrintf( 1, "ADDR_NOT_MAPPED: Address is not mapped in the memory map.\n");
            DebugPrintf( 1, "                 Count value is taken in to consideration where applicable.");
            break;

        case  15:
            DebugPrintf( 1, "CMD_LOCKED");
            break;

        case  16:
            DebugPrintf( 1, "INVALID_CODE: Unlock code is invalid.");
            break;

        case  17:
            DebugPrintf( 1, "INVALID_BAUD_RATE: Invalid baud rate setting.");
            break;

        case  18:
            DebugPrintf( 1, "INVALID_STOP_BIT: Invalid stop bit setting.");
            break;

        case 255:                                                                                            break;
        default:
            DebugPrintf( 1, "unknown error %u", ErrorNumber);
            break;
    }
#else // !defined COMPILE_FOR_LPC21
    DebugPrintf( 1, "error (%u), see  PhilipsOutputErrorMessage() in lpc21isp.c for help \n\r", ErrorNumber);
#endif // !defined COMPILE_FOR_LPC21
}


/***************************** GetErrorNumber ***************************/
/**  Find error number in string.  This will normally be the string
returned from the microcontroller.
\param [in] Answer the buffer to search for the error number.
\return the error number found, if no linefeed found before the end of the
string an error value of 255 is returned. If a non-numeric value is found
then it is printed to stdout and an error value of 255 is returned.
*/
static unsigned char GetErrorNumber(const char *Answer)
{
    unsigned int i = 0;

    while(1)
    {
        if(Answer[i] == 0x00) return(255);
        if(Answer[i] == 0x0a) break;
        i++;
    }

    i++;

    if(Answer[i] < '0' || Answer[i] > '9')
    {
        DebugPrintf( 1, "ErrorString: %s", &Answer[i]);
        return(255);
    }

    return (unsigned char)(atoi(&Answer[i]));
}
