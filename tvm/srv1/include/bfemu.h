/** \file bfemu.h
 * Header file of exported Blackfin emulation functions
 *
 * (c) 11/2004 Martin Strubel <hackfin\@section5.ch>
 *
 * $Id: bfemu.h 361 2006-08-29 20:31:36Z strubi $
 *
 */

// Properly handle DLL import/export under windows platforms
#ifndef DLL_API
#if defined (WIN32)
#	if defined (DLL_EXPORTS)
#		define DLL_API __declspec(dllexport)
#	else
#		define DLL_API __declspec(dllimport)
#	endif
#else
#	define DLL_API
#endif
#endif

#include "jtag.h"

/** \example register.c
 *
 * Example remote register dump
 */

/** \example memtest.c
 *
 * Example memory read/write access over JTAG
 */

#define BFIN_REGISTER        unsigned long
#define BFIN_REGISTER16      unsigned short
#define ADDR                 unsigned long

struct _cpu;

#define CPU       struct _cpu *
/** \defgroup JTAG      JTAG initialization
 *
 * Functions to initialize the JTAG chain and parts
 *
 */

/** \defgroup Emulation Emulation state handling
 *
 * These functions control the emulation state of the CPU - 
 * note that the first argument is always a CPU handle which
 * is obtained through jtag_init().
 * 
 */

/** \defgroup Wpu Watchpoint unit/Hardware breakpoints
 *
 * These functions provide control of the Watchpoint unit.
 * Hardware assisted breakpoints (for example, breakpoints in RO
 * memory) are realized via these functions.
 *
 * \note On some silicon revisions, this feature does either not
 *       work at all, or not properly. There are some workarounds in
 *       the software to attempt to make it work.
 *       Nevertheless, these functions are EXPERIMENTAL. Make sure
 *       you read the hardware reference about your chip's
 *       watchpoint unit.
 * 
 */

/** \defgroup RegMem    Register and Memory access
 *
 * Register and memory space read/write
 * 
 * These functions only work if the Blackfin core is in emulation mode.
 * See Emulation for details.
 *
 */

/** \defgroup Peeking   Peek Mode
 *
 * These functions allow to communicate with programs on the target
 * while it is running (not in emulation). This is equivalent to the
 * BTC features of VisualDSP.
 *
 * The developer must link a tiny backend to his program in order to
 * use this feature.
 *
 * The backend contains all functions below with the prefix \c bfp_. 
 * Note that these functions are yet experimental.
 *
 * Features:
 *
 * \li Sending messages to the host (printf)
 * \li Dumping onedimensional data to plot
 *
 */

/** \defgroup AUX       Auxiliary functions and macros
 *
 * These functions help you with detection and debugging of the
 * device chain.
 *
 */

/** \defgroup Error     Error handling
 *
 * Function return value error codes
 */

////////////////////////////////////////////////////////////////////////////
// DEFINES

/** \addtogroup Emulation
 * @{ */

/** CPU state
 */

typedef enum {
	CPUSTATE_RUNNING,                 ///< Normal state (running)
	CPUSTATE_EMULATION,               ///< In emulation mode
	CPUSTATE_STEP,                    ///< In single step mode
	CPUSTATE_IDLE,                    ///< CPU is idle
	CPUSTATE_IN_RESET,                ///< CPU is in reset
	CPUSTATE_HOLDOFF,                 ///< CPU is hold off instruction fetching
	CPUSTATE_COREFAULT,               ///< Core fault
} CpuState;


/* @} */

/**
 * \ingroup Wpu
 *  See wpu_set_watch()
 */

enum {
	WP_DISABLE,                       ///< Disable watchpoint
	WP_READ,                          ///< Set Read-watchpoint
	WP_WRITE,                         ///< Set Write-watchpoint
	WP_ALL                            ///< Set Read & Write WP
};


// EMUDAT register size tokens

#define EMUDAT_SIZE_32  0             ///< EMUDAT register size = 32
#define EMUDAT_SIZE_40  (1 << 7)      ///< EMUDAT register size = 40
#define EMUDAT_SIZE_48  (2 << 7)      ///< EMUDAT register size = 48

// EMUIR register size tokens

#define EMUIR_SIZE_64   0             ///< EMUIR register size = 64
#define EMUIR_SIZE_48   (1 << 4)      ///< EMUIR register size = 48
#define EMUIR_SIZE_32   (2 << 4)      ///< EMUIR register size = 32


////////////////////////////////////////////////////////////////////////////
// CPU identification

/** \addtogroup JTAG
 * @{ */

#define IDCODE_BF533 0x027a50cb       ///< BF533 JTAG ID
#define IDCODE_BF534 0x027c60cb       ///< BF534 JTAG ID
#define IDCODE_BF537 0x027c80cb       ///< BF537 JTAG ID
#define IDCODE_BF539 0x027c40cb       ///< BF539 JTAG ID
#define IDCODE_BF561 0x027bb0cb       ///< BF561 JTAG ID

/* @} */

////////////////////////////////////////////////////////////////////////////
// Register stuff


#define EMULATION_CAUSE(x) ((x >> 6) & 0xf)

#define EMUCAUSE_EMUEXCPT               0x0
#define EMUCAUSE_EMUIN                  0x1
#define EMUCAUSE_WATCHPT                0x2
#define EMUCAUSE_PERFMON0               0x4
#define EMUCAUSE_PERFMON1               0x5
#define EMUCAUSE_STEP                   0x8

/** \addtogroup JTAG
 * @{ */

/** Initialize Blackfin CPUs on JTAG chain
 *
 * \attention This API call has changed from 0.x.
 *
 * \param jtag   The JTAG controller handle to be obtained via jtag_open()
 * \param cpu    Pointer to a an array of CPU handles \c CPU \c cpus[n]
 *               where \c n is the number of devices returned by jtag_open().
 *
 * \return       1: ok, error else.
 */

DLL_API
int           jtag_init(JTAG_CTRL jtag, CPU *cpu);

/** Free all jtag resources
 *
 * \attention    This function API has changed from 0.x
 *
 * \param jtag   The JTAG controller handle to be obtained via jtag_open()
 * \param cpu    Pointer to array of CPUs - see jtag_init().
 */

DLL_API
void          jtag_exit(JTAG_CTRL jtag, CPU *cpu);


/** @} */

/** \addtogroup Emulation
 * @{ */

/** Initialize emulation. This must be called before anything else.
 */

DLL_API
int           emulation_init(CPU cpu);

/**
 * Exit emulation and turn off emulator unit (save power)
 */

DLL_API
int           emulation_exit(CPU cpu);

/** Enter emulation state 
 *
 * \return    CPU state if successful, negative error code else.
 */

DLL_API
int           emulation_enter(CPU cpu);

/** Exit emulation state; 'go'.
 *
 * \return    CPU state if successful, negative error code else.
 */

DLL_API
int           emulation_leave(CPU cpu);

/** Enter single stepping mode
 *
 * \return    CPU state if successful, negative error code else.
 */

DLL_API
int           emulation_enter_singlestep(CPU cpu);

/** Return from emulation (go). 
 *
 * This function does not change the CPU state (i.e. is used in
 * single step mode)
 *
 * \return    CPU state if successful, negative error code else.
 */

DLL_API
int           emulation_go(CPU cpu);

/** Leave single step mode
 *
 * \return    CPU state if successful, negative error code else.
 */

DLL_API
int           emulation_leave_singlestep(CPU cpu);

/** Flush memory changes
 *
 * This function makes sure that all cache lines on the CPU get
 * flushed. This has only an effect if caching is turned on.
 *
 * \return    1: Instruction cache was enabled and flushed
 *            2: Data cache was enabled and flushed
 *            3: both
 *
 */

DLL_API
int           emulation_flush(CPU cpu);


////////////////////////////////////////////////////////////////////////////
// STATUS QUERY

/** Return emulation state for state debugging
 *
 * If an error happened, the return value is a negative error code.
 * Always check.
 *
 * \param cause       Pointer to emulation cause value to be filled in.
 *                    Can be NULL, if value not requested.
 *
 * \return            The current CPU state (see CpuState)
 */

DLL_API
CpuState      emulation_state(CPU cpu, int *cause);

/** @} */

////////////////////////////////////////////////////////////////////////////
// Watchpoint/Hardware breakpoints

/** \addtogroup Wpu
 * @{ */

/** Initialize CPUs watchpoint unit
 *
 * \param enable      1: enable, 0: disable
 *
 * \return            0: ok, < 0: error code. This feature does not work
 *                    on all platforms, read above details about
 *                    watchpoint support.
 *
 */

DLL_API
int           wpu_init(CPU cpu, int enable);

/** Set hardware breakpoint on CPU
 *
 * \param cpu     The cpu handle
 * \param n       The number of the breakpoint [0..5]
 * \param on      1: enable, 0: disable
 */

DLL_API
int           wpu_set_hbreak(CPU cpu, int n, ADDR addr, int enable);

DLL_API
int           wpu_set_watch(CPU cpu, int n, ADDR addr, int mode);

/** Returns an 8 bit vector reflecting the source of the watchpoint.
 * A set bit n means, that the watchpoint event was caused by watchpoint
 * n. [0,5] : Instruction address watchpoints, [6,7]: Data address WP.
 */

DLL_API
int           wpu_cause(CPU cpu);

/** Clear WPU status register
 * A call to this function clears the bit vector (WPSTAT register).
 */

void          wpu_clr(CPU cpu, BFIN_REGISTER r);

/** @} */


/** \addtogroup AUX
 * @{ */


/** Return identification string and revision from CPU id
 *
 * \param rev      pointer to revision variable to be filled in
 * \return         identification string
 */

DLL_API
const
char         *detect_device(CPU cpu, unsigned int *rev);

/** @} */


////////////////////////////////////////////////////////////////////////////
// MID LEVEL

/** \addtogroup RegMem
 * @{ */

/** Read from CPU side memory
 *
 * Note that access speed is faster when \arg addr and \arg size are a
 * multiple of four, or two.
 * 
 * \param addr      Address in Blackfin memory space to read from
 * \param count     Block size in bytes
 * \param buf       Pointer to buffer to store data in
 *
 * \return          0: no error, < 0: error
 */

DLL_API
int           get_memory(CPU cpu, ADDR addr, unsigned long count,
		                           unsigned char *buf);

/** Write to CPU side memory
 *
 * Note that access speed is faster when \arg addr and \arg size are a
 * multiple of four, or two.
 *
 * \param addr      Address in Blackfin memory space to write to
 * \param count     Block size in bytes
 * \param buf       Pointer to buffer containing data
 *
 * \return          0: no error, < 0: error
 */

DLL_API
int           set_memory(CPU cpu, ADDR addr, unsigned long count,
		                           const unsigned char *buf);

#define LDST_32 0                ///< Word size 32 bits
#define LDST_16 1                ///< Word size 16 bits
#define LDST_8  2                ///< Word size 8 bits

/** Read MMR registers
 *
 * Normally, the MMR range starts at \c 0xffc00000 for the supported
 * Blackfin architectures. Use this function when accessing a memory mapped
 * register.
 *
 * \param addr        The MMR register address
 * \param word        Pointer to the word where data is to be stored
 * \param sizecode    Either #LDST_16 or #LDST_32, depending on register size
 *
 */

DLL_API
int           get_memory_word(CPU cpu,
                              ADDR addr, unsigned long *word, int sizecode);

/** Write to MMR registers
 *
 * See also get_memory_word() about the valid memory range.
 *
 * \param addr        The MMR register address to write to
 * \param word        Data to write
 * \param sizecode    Either #LDST_16 or #LDST_32, depending on register size
 *
 */

DLL_API
int           set_memory_word(CPU cpu,
                              ADDR addr, unsigned long word, int sizecode);


/** Get CPU register value
 *
 * \attention     This function API has changed from 0.x
 *
 * This function retrieves the content of a core register via JTAG.
 *
 * \param regno   A register token from enum bfin_registers {..}
 *                (see bfin-registers.h)
 *
 */

/*
DLL_API
BFIN_REGISTER get_cpuregister(CPU cpu, int regno);
*/

DLL_API
int           get_cpuregister(CPU cpu, int regno, BFIN_REGISTER *value);

/** Set CPU core register value
 * \param regno     A register token from enum bfin_registers {..}
 *                  (see bfin-registers.h)
 *
 * \param value     The register value
 *
 */

DLL_API
int           set_cpuregister(CPU cpu, int regno, BFIN_REGISTER value);

/**
 * Save CPU registers to buffer.
 *
 * An action manipulating the CPU registers should always be encapsulated
 * by a save_registers() and restore_registers() call.
 *
 * \param buf    Buffer to save registers to. This must have the size of
 *               the number of registers in the register list below.
 *               If all registers are to be restored
 * \param regs   List of registers to save. Must be terminated with -1.
 *               Do not use this call to access the  the following registers:
 *               REG_I0 <= register <= REG_L3,
 *               REG_A0x, REG_A0w, REG_A1x, REG_A1w,
 *               REG_EMUDAT
 *              
 */

DLL_API
int           save_registers(CPU cpu, BFIN_REGISTER *buf, int *regs);


/** Restore CPU registers from buffer.
 *
 * See save_registers()
 *
 */

DLL_API
int           restore_registers(CPU cpu, BFIN_REGISTER *buf, int *regs);

/** Save DAG registers
 *
 * This call is used to save the DAG registers into a register context
 * buffer. r0 is modified within this call!
 *
 * Make sure you save r0, or encapsulate this call in between
 * save_registers() and restore_registers() including REG_R0 in the
 * register list.
 *
 * \param buf       Pointer to BFIN_REGISTER array[16];
 *                  The layout of the buffer content corresponds to the
 *                  register map in bfin-registers.h, starting from
 *                  REG_I0
 *
 * 
 */

DLL_API
int           save_dagregisters(CPU cpu, BFIN_REGISTER *buf);

/** Save ALU registers.
 *
 * See  save_dagregisters() for behaviour.
 *
 * \param buf       Pointer to BFIN_REGISTER array[4];
 *                  The layout of the buffer content corresponds to the
 *                  register map in bfin-registers.h, starting from
 *                  REG_A0x
 *
 */

DLL_API
int           save_aluregisters(CPU cpu, BFIN_REGISTER *buf);


/** Get value of DBGSTAT register
 */

DLL_API
BFIN_REGISTER get_dbgstat(CPU cpu);


DLL_API
void          get_real_pc(CPU cpu, BFIN_REGISTER *pc);


/** Reset CPU
 *  This function resets the CPU according to the mode value:
 *
 *  \param mode     1: reset Core, 2: reset System, 3: both. A value of 0
 *                  causes no action.
 *  \return         #ERR_NOTREADY (Reset failed) or other error code.
 */

DLL_API
int           cpu_reset(CPU cpu, int mode);

/** @} */

/** \addtogroup Peeking
 * @{ */

enum {
	PEEK_MSG,      ///< Message
	PEEK_BUFFER,   ///< Data buffer
	PEEK_LAST      // must always be last
};

typedef enum {
	T_8BIT,        ///< 8 Bit
	T_16BIT,       ///< 16 Bit
	T_24BIT,       ///< 24 Bit
	T_32BIT        ///< 32 Bit
} PBType;

#define F_ACTIVE      0x01   ///< Buffer is active (update)
#define F_UPDATED     0x02   ///< Buffer was updated

#define LABEL_SIZE 32

/**
 * PeekBuffer type.
 *
 * This can hold any data sent from the target to the host.
 */

typedef struct _peekbuf {
	unsigned char label[LABEL_SIZE];   ///< Label
	unsigned long id;                  ///< Unique 32 bit ID
	unsigned char flags;               ///< Flags, [#F_ACTIVE,#F_UPDATED]
	PBType type;                       ///< Data type
	unsigned long n;                   ///< Buffer size
	unsigned char *data;               ///< Data pointer
} PeekBuffer;

/**
 * A callback function pointer for the bfpeek dispatcher
 */
typedef int (*CBFunc)(struct _peekbuf *);

/** Init peeking mode
 *
 * Peeking can only happen when emulation is not active, i.e.
 * you must call this function before a resume.
 *
 * When halting the CPU and examining data, peek_exit() must be called
 * beforehand.
 */

DLL_API
int           peek_init(CPU cpu);

/** Exit peeking mode
 */

DLL_API
int           peek_exit(CPU cpu);

/** Peeking handler
 *
 * If Peeking mode is active, this function listens to incoming
 * messages through the EMUDAT channel (when using libbfpeek in
 * your target executable). Commands are immediately dispatched
 * and processed according to the callbacks registered with
 *
 * peek_register_callbacks()
 *
 * \return number of peeking events handler or error code
 */

DLL_API
int           peek_listen(CPU cpu);

/** Register callbacks
 *
 * This function registers callbacks for peeking handler events.
 *
 * \param which    One of [#PEEK_MSG, #PEEK_BUFFER]
 * \param fptr     The function pointer to call back on a data
 *                 reception
 */

DLL_API
int           peek_register_callbacks(int which, CBFunc fptr);

/** @} */

/** \addtogroup Error
 * @{ */

/** Error codes
 * 
 */


enum {
	// JTAG engine errors
	ERR_GENERAL = -1024,       ///< Generic error
	ERR_MALLOC,                ///< Memory allocation failure
	ERR_OPEN,                  ///< Could not open device
	ERR_CLAIM,                 ///< Could not claim device
	ERR_WRITE,                 ///< Write error
	ERR_READ,                  ///< Read error
	ERR_TIMEOUT,               ///< Timeout while waiting for data
	ERR_DETECT,                ///< Error trying to detect device
	ERR_UNINITIALIZED,         ///< JTAG chain not initialized (jtag_detect())
	// CPU errors
	ERR_NOTREADY = -512,       ///< Emulation not ready
	ERR_DMA,                   ///< DMA error
	ERR_CORE,                  ///< Core fault
	// Emulation mid level errors
	ERR_STATE = - 127,         ///< Emulation state error
	ERR_UNSUPPORTED,           ///< Unsupported function called
	ERR_ACCESS_SIZE,           ///< Illegal access size (wrong word size)
	ERR_ACCESS_MEMORY,         ///< Illegal memory access
	ERR_ACCESS_WAY,            ///< Cannot use this access way (cross core)
	ERR_OVERFLOW,              ///< Internal buffer overflow
	// Main program errors
	ERR_ARGUMENT,              ///< Wrong argument supplied
	// No error
	ERR_NONE = 0
};

/** @} */

// DEBUGGING (undocumented)
//

DLL_API
void          dump_dbgstat(CPU cpu);
