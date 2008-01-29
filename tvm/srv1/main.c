/*
 * main.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2007-2008 Jon Simpson, Matthew C. Jadud
 */

/* Generic TVM Includes */
#include "interpreter/transputer.h"
#include "interpreter/interpreter.h"
#include "interpreter/instructions.h"
#include "interpreter/ext_chan.h"
#include "interpreter/scheduler.h"
#include "interpreter/pool_alloc.h"
#include "interpreter/mem.h"
#include "interpreter/timer.h"
#include "interpreter/ffi_tables.h"

/* Platform code */
#include "srv.h"

/* Tasty Bytecode */#include "bytecode.h"
/* TVM Preamble */
/* The wrapper currrently doesnt work with vector/MS space, this will prevent programs that use those areas compiling in. */
#if (vs_size > 0)
#error Vector space in use, but not supported on the RCX at this time.
#endif
#if (ms_size > 0)
#error Mobile space in use, but not supported on the RCX at this time.
#endif

/* Registers & Flags */
extern BPOOTER iptr;              /* Instruction pointer              */
extern POOTER  wptr;              /* Workspace pointer                */
extern POOTER  tptr[NUM_PRI];     /* Timer queue pointers             */
extern WORD    tnext[NUM_PRI];    /* Timeout register                 */
extern POOTER  fptr[NUM_PRI];     /* Front pointer (scheduler queue)  */
extern POOTER  bptr[NUM_PRI];     /* Back pointer (scheduler queue)   */

/* The Transputer stack */
extern WORD areg, breg, creg, oreg;

/* Pointers to the memory available to this transputer */
WORD *transputermem, *topmem, *botmem;

/* Memory array */
unsigned int mem_array[memory_size+4];

/* 'Pointers' to start and end of code */
BPOOTER code_start, code_end;

/* Pointers to the start of the various bits of memory */
POOTER workspace_ptr   = 0;
POOTER vectorspace_ptr = 0;

static void srv_ins_not_implemented (void)
{
	uart0SendString((unsigned char *)" >> INS NOT IMPLEMENTED << ");
}

static void srv_ins_invalid (void)
{
	uart0SendString((unsigned char *)" >> INS INVALID << ");
}

void tvm_sleep (void)
{
	/* TODO: Implement sleep. */
}
 
int srv_get_time (void)
{
	return readRTC();
}

void setup_registers(BPOOTER transputer_code, POOTER bottom_mem)
{
	int i;
	iptr = transputer_code;
	wptr = bottom_mem;

	/* Setup Scheduler queues. */
	for (i = 0; i < NUM_PRI; i++) {
		fptr[i] = (WORD *)NOT_PROCESS_P;
		bptr[i] = (WORD *)NOT_PROCESS_P;
		tptr[i] = (WORD *)NOT_PROCESS_P;
	}
	
	/* Stack */
	areg = 0;
	breg = 0;
	creg = 0;
	oreg = 0;
}

int main() {

	init_io(); // Initialise LED, GPIO, serial flow and lasers.
	initRTC();
	init_uarts();
	initPWM();
	
	init_motors(); // Set up motor globals.

	//setPWM(lspeed, rspeed);

	/* Print version info on startup */
	serial_out_version();

	clear_sdram(); // Clears from 0x00000000 to 0x02000000
	camera_setup(); // Sets up the camera, initially 320x256.

	/* Temp vars */
	unsigned int i;
	int runresult;
	
	/* Initialise the TVM memory to zero */
	transputermem = (WORD *)mem_array;
	for (i = 0; i < memory_size + 4; i++)
	{
		transputermem[i] = 0x0000;
	}
	
	/* Initialise pointers */
	code_start = (BPOOTER) transputercode;
	code_end = bpooter_plus(code_start, inst_size);
	topmem = transputermem;
	botmem = topmem + memory_size + 4;
	
	setup_registers((BPOOTER) code_start, botmem);
	
	/* Handlers */
	not_implemented = srv_ins_not_implemented;
	invalid = srv_ins_invalid;
	get_time = srv_get_time;
	special_ffi_table = get_special_hooks();
	
	/* Initialise stackframe */
	initial_stackframe(&wptr, 0, 0, 0, 0, 0);
 
	uart0SendString((unsigned char *)"##TVM Initialiation Complete");
	uart0SendChar('\n');
	delayMS(1000);
	/* Go! */
	runresult = run();
	
	uart0SendString((unsigned char *)"##Out of runloop.");
	uart0SendChar('\n');
	delayMS(1000);

	/* Print appropriate codes for how we dropped out of the runloop
	 * 42					normal (i.e. stack bottom)
	 * 666				deadlock
	 * 5706				halted
	 * 777				debug trap
	 * 96				alignment error
	 * 888				scheduler badness
	 * 999				default death
	 */
	switch(runresult)
	{
		case EXIT_STACK_BOTTOM:
			uart0SendString((unsigned char *)"42 - EXIT_STACK_BOTTOM");
			break;
		case EXIT_DEADLOCK:
			uart0SendString((unsigned char *)"666 - EXIT_DEADLOCK");
			break;
		case EXIT_HALTED:
			uart0SendString((unsigned char *)"5706 - EXIT_HALTED");
			break;
		case EXIT_DEBUG_TRAP:	
			uart0SendString((unsigned char *)"777 - EXIT_DEBUG_TRAP");
			break;
		case EXIT_ALIGN_ERROR:
			uart0SendString((unsigned char *)"96 - EXIT_ALIGN_ERROR");
			break;
		case EXIT_SCHEDULER_BAD_1:
			uart0SendString((unsigned char *)"888 - EXIT_SCHEDULER_BAD");
			break;
		default:
			uart0SendString((unsigned char *)"999 - default");
			break;
	}
	uart0SendChar('\n');
	delayMS(1000);
	/** End of TVM **/
}
