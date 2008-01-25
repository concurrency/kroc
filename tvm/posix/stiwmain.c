/*
 * stiw.c - Simple Transputer Interpreter Wrapper
 *
 * Copyright 2004    Christian L. Jacobsen, Matthew C. Jadud
 *
 * Define STIWTVM if this file is being compiled as an executable which can load
 * in transterpreter bytecode files.
 */
/* #define DISABLE_KEYBOARD_PROC */
/* #define DMEM_DEBUG */
#undef MEM_LAYOUT_DEBUG

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WIN32
#define BYTE WIN_BYTE
#define WORD WIN_WORD
#define UWORD WIN_UWORD
#include <windows.h>
#undef BYTE
#undef WORD
#undef UWORD 
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/timeb.h>
#include <time.h>
#ifdef STIWTVM
#include <limits.h>
#endif /* STIWTVM */
#ifndef WIN32
#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <sys/time.h>
#endif /* !WIN32 */


#ifndef STIWTVM
#include "stiw.h"
#else /* STIWTVM */
/* FIXME: Line below probably ought to be up here, but it gives me an annoying
 * warning when stiw is compiled into a library. I am guessing that I will fix
 * things so that we dont compile this into a library at some point, cos we
 * dont use that anymore on platforms that use this wrapper. */
#include "tbzutil.h"
#endif /* STIWTVM */
#include "occam_debug.h"
#include "unix_io.h" // temporary

/* The keyboard/screen/error handler bytecode */
#include "handlers.h"

/* The ffi stuff :)*/
#include "ffi.h"
#include "tvm_hooks.h"

/* FIXME: FIXME: FIXME: No really... */
#ifdef ENABLE_RUNLOOP_DBG_HOOKS
#define LOCKSTEP_CLOCK
#define LOCKSTEP_MULTIPLIER 10
//#define DISABLE_KEYBOARD_PROC
#define DMEM_DEBUG
/* #define SCHEDULER_ENABLE_BUSYWATIT_HOOK This should be set by compiler */
#endif

/* Values to clear memory to. */
#define MEM_FILL_BYTE 0xAA
#define MEM_FILL_WORD 0xAAAAAAAA

char *filename;

/* Holds the type of file that we are dealing with */
int filetype;

/*Global vars for holding command line arguments*/
char **tvm_argv;
int  tvm_argc;

#ifdef DMEM_DEBUG
#include <search.h>
#include "dmem.h"
#include "jumptbl_sec.h"

void  *memories_root = NULL;

typedef struct memories_t
{
	WORDPTR ptr;
	WORD size;
} memories;

int memories_compare(const void *aa, const void *bb)
{
	memories* a = (memories*) aa;
	memories* b = (memories*) bb;

	if(a->ptr == b->ptr)
	{
		return 0;
	}

	if(a->ptr > b->ptr)
	{
		return 1;
	}

	return -1;
}

	
memories* new_memories()
{
	memories* item = malloc(sizeof(memories));
	memset(item, sizeof(memories), 0);
	return item;
}

void memories_insert(WORDPTR ptr, WORD size)
{
	memories* node = new_memories();
	node->ptr = ptr;
	node->size = size;

	(void) tsearch((void *)node, (void **)&memories_root, memories_compare); 
}

void memories_remove(WORDPTR ptr)
{
	memories search_node;
	memories *node;
	search_node.ptr = ptr;
	search_node.size = 0;

	node = tdelete((void *)&search_node, (void **)&memories_root, memories_compare);
	if(node != 0)
	{
		free(node);
	}
	else
	{
		printf("WARNING: memories_remove, node (ptr=0x%08x) not found!\n", (WORD) ptr);
	}
}

int memories_valid_mem_compare(const void *aa, const void *bb)
{
	memories* a = (memories*) aa;
	memories* b = (memories*) bb;
	WORDPTR top = b->ptr;
	WORDPTR bot = b->ptr + (b->size / WORDLENGTH);
	WORDPTR target = a->ptr;

	/* Check that the node we are searching for is in a */
	if(!a->size == 0)
	{
		abort();
	}

	if(target > top && target < bot)
		return 0;

	if(target < top)
	{
		return -1;
	}

	if(target > b->ptr)
	{
		return 1;
	}

	return 0;
}

int memories_valid_mem(WORDPTR ptr)
{
	void * res;
	memories search_node;
	search_node.ptr = ptr;
	search_node.size = 0;

	res = tfind((void *)&search_node, (void *)memories_root, memories_valid_mem_compare);

	if(res != 0)
		return 1;

	return 0;
}

void ins_malloc_debug()
{
	WORDPTR ptr;
	WORD size;

	/* Pick up the size of the allocated block */
	size = areg;

	/* Do the actual call */
	ins_malloc();
	/* Pick up pointer to allocated block */
	ptr = (WORDPTR) areg;

	/* Store the information away somewhere */
	memories_insert(ptr, size);
}

void ins_mrelease_debug()
{
	WORDPTR ptr;

	/* Pick up the pointer */
	ptr = (WORDPTR) areg;

	/* Do the actual call */
	ins_mrelease();

	/* Remove the entry */
	memories_remove(ptr);
}

void ins_mreleasep_debug()
{
	WORDPTR ptr = (WORDPTR) (((BYTEPTR)wptr) + (areg * WORDLENGTH));

	printf("mrelease_debug: ptr=0x%08x\n", (WORD) ptr);
	ins_mreleasep();

	memories_remove(ptr);
}

void dmem_debug_init()
{
	secondaries[0x11] = ins_mreleasep_debug;
	secondaries[0xE2] = ins_malloc_debug;
	secondaries[0xE3] = ins_mrelease_debug;
}

#endif /* DMEM_DEBUG */

#ifdef LOCKSTEP_CLOCK
WORD lockstep_counter = 0;
void print_timer_queue(void); /* prototype */
void stiw_busywait_hook()
{
	int time = get_time();
	int i;

	for(i = 0; i < NUM_PRI; i++)
	{
		printf("  tnext[%02d]: 0x%08x [%11d]\n", i, (WORD)tnext[i], (WORD)tnext[i]);
	}

	printf("  clock:     0x%08x [%11d]\n", time, time);
	printf("\n");
	print_timer_queue();
	lockstep_counter = lockstep_counter + LOCKSTEP_MULTIPLIER;
}
#endif

/* FIXME: FIXME: FIXME: Need to sort out the memory interface */
void setup_mem(WORD *, WORD);
void print_mem(WORDPTR start, WORDPTR end, WORDPTR point_at, WORD what_is_zero);
void print_stack();
void print_vectorspace();
void print_mobilespace();
#ifdef DMEM_DEBUG
void print_dmem();
#endif

/* Pointers to the memory available to this transputer */
WORD *transputermem; /* This has to be a real pointer, so we can pass real 
												memory to the memory subsystem. */
WORDPTR topmem, botmem;        /* 'Pointers' to top and bottom of memory */
BYTEPTR code_start, code_end; /* 'Pointers' to start and end of code */

/* The various sizes of the various bits of memory */
int workspace_size   = 0;
int vectorspace_size = 0;
int mobilespace_size = 0;
/* Pointers to the start of the various bits of memory */
WORDPTR workspace_ptr   = 0;
WORDPTR vectorspace_ptr = 0;
WORDPTR mobilespace_ptr = 0;
/* Pointers to the code for the error handlers */
BYTEPTR kbh_code_ptr = 0;
BYTEPTR scrh_code_ptr = 0;
BYTEPTR errh_code_ptr = 0;

static inline WORDPTR wordptr_plusb(WORDPTR p, int bytes) {
	return (WORDPTR) byteptr_plus(p, bytes);
}

/* We need these to be actual variables if compiling for STIWTVM */
#ifdef STIWTVM
unsigned char *transputercode;
unsigned char *transputercode_ptr;
int memsize = 0;
int instsize;


#define BYTECODE_VERSION 1

void version(char *progname)
{
	printf("%s version %s\n", progname, VERSION);
}

void usage(char *progname)
{
	printf("  Usage: %s BYTECODEFILE [MEMSIZE]\n\n", progname);
	printf("    BYTECODEFILE must be a valid Transterpreter bytecode file.\n");
	printf("    If MEMSIZE is specified, it is the workspace size of the program;\n");
	printf("    if not specified it will be computed automatically.\n");
}
#endif

void print_crash(int i)
{
	printf("The Transterpreter crashed! (Code: %3d)\n", i);
}

int valid_mem(WORDPTR ptr)
{
	if(ptr >= topmem && ptr <= botmem)
		return 1;

#ifdef DMEM_DEBUG
	return memories_valid_mem(ptr);
#else
	return 0;
#endif
}

#define IN_USER_PROGRAM 1;
#define IN_KEYBOARD_HANDLER 2;
#define IN_SCREEN_HANDLER 3;
#define IN_ERROR_HANDLER 4;
const char status_codes[] = "*ukse";

int valid_code(BYTEPTR ptr)
{
	if(ptr >= code_start && ptr < code_end)
	{
		return IN_USER_PROGRAM;
	}

	if(ptr >= kbh_code_ptr && ptr < kbh_code_ptr + kbh_inst_size)
	{
		return IN_KEYBOARD_HANDLER;
	}

	if(ptr >= scrh_code_ptr && ptr < scrh_code_ptr + scrh_inst_size)
	{
		return IN_SCREEN_HANDLER;
	}

	if(ptr >= errh_code_ptr && ptr < errh_code_ptr + errh_inst_size)
	{
		return IN_ERROR_HANDLER;
	}

	return 0;
}

WORDPTR adj2memstart(WORDPTR ptr)
{
	return (WORDPTR)(ptr - topmem);
}

BYTEPTR adj2codestart(BYTEPTR ptr)
{
	return (BYTEPTR)(ptr - code_start);
}

#define QUEUE_MAX_PRINT 20

void print_scheduling_queue(void)
{
	int i;
#	define SQUEUE_COLS 1
	
	printf("Scheduling Queues: [iptr: * = broken, u = user, k = kybh, s = scrh, e = errh]\n");
	for(i = 0; i < NUM_PRI; i++)
	{
		int count = 0;
		WORDPTR ptr = fptr[i];

		printf("  Scheduling Queue %d: (fptr: 0x%08x (0x%08x))\n", i, (WORD)ptr, (WORD)adj2memstart(ptr));
		if(ptr == (WORDPTR)NOT_PROCESS_P)
		{
			printf("    empty\n");
		}
		else
		{
			int print_count = 0;

			while(ptr != (WORDPTR)NOT_PROCESS_P)
			{
				/* Indent if it is the first thing on a line */
				if(count == 0)
				{
					printf("    ");
				}

		
				/* Print the queue entry */
				{
					BYTEPTR iptr = (BYTEPTR)WORKSPACE_GET(ptr, WS_IPTR);
					const char iptr_status = status_codes[valid_code(iptr)];
					printf("wptr: 0x%08x, iptr: 0x%08x %c, next: 0x%08x -> ", 
							(WORD)ptr, (WORD)iptr, iptr_status, WORKSPACE_GET(ptr, WS_NEXT));
				}

				/* Was this the last entry??? */
				if(ptr == (WORDPTR)NOT_PROCESS_P || ptr == bptr[i])
				{
					printf("end\n");
					break;
				}

				/* Printed one, so add one to count, and point to the next ptr */
				count = (count + 1) % SQUEUE_COLS;
				ptr = (WORDPTR)WORKSPACE_GET(ptr, WS_NEXT);

			
				/* If the new pointer is not in valid memory, things are broken */
				if(!valid_mem(ptr))
				{
					printf("queue points to invalid memory\n");
					break;
				}
			
				/* If we have printed too many things, abort as we might have queue
				 * corruption, and be printing random garbage... */
				if(print_count > QUEUE_MAX_PRINT)
				{
					printf("scheduling queue too long, not printing any more\n");
					break;
				}

				/* If count has been reset to 0, ie we ran out of columns, newline */
				if(count == 0)
				{
					printf("\n");
				}

				/* Count how many lines we have printed, for aborting */
				++print_count;
			}
		}
	}

	printf("\n");

}

void print_timer_queue(void)
{
	int i;
#	define TQUEUE_COLS 1
	
	printf("Timer Queues: [fmt: timeout {next} -> ]\n");
	for(i = 0; i < NUM_PRI; i++)
	{
		int count = 0;
		WORDPTR ptr = tptr[i];

		printf("  Timer Queue %d: (top: 0x%08x (0x%08x))\n", i, (WORD)ptr, (WORD)adj2memstart(ptr));
		if(ptr == (WORDPTR)NOT_PROCESS_P)
		{
			printf("    empty\n");
		}
		else
		{
			int print_count = 0;

			while(1) /* ptr != (WORDPTR)NOT_PROCESS_P)*/
			{
				if(count == 0)
				{
					printf("    ");
				}
				if(ptr == (WORDPTR)NOT_PROCESS_P)
				{
					printf("end\n");
					break;
				}
				else if(ptr < topmem || ptr > botmem)
				{
					printf("timer queue broken\n");
					break;
				}
				else if(print_count > QUEUE_MAX_PRINT)
				{
					printf("timer queue too long, not printing any more");
					break;
				}
				printf("0x%08x [%11d] {0x%08x (0x%08x)} -> ", 
						WORKSPACE_GET(ptr, WS_TIMEOUT),
						WORKSPACE_GET(ptr, WS_TIMEOUT),
						WORKSPACE_GET(ptr, WS_NEXT_T), (unsigned int)adj2memstart((WORDPTR)WORKSPACE_GET(ptr, WS_NEXT_T)));
				ptr = (WORDPTR)WORKSPACE_GET(ptr, WS_NEXT_T);

				count = (count + 1) % TQUEUE_COLS;
				if(count == 0)
				{
					printf("\n");
				}

				++print_count;
			}
		}
	}

	printf("\n");
}

void print_registers(void)
{
	int i; /* General counter */
	int time = get_time();
	BYTEPTR iptr_prime = byteptr_minus(iptr, 1);

	printf("Registers:\n");
	printf("  iptr: 0x%08x (0x%08x) (iptr_prime: 0x%08x (0x%08x))\n", 
			(WORD)iptr, (WORD)adj2codestart(iptr), 
			(WORD)iptr_prime, (WORD)adj2codestart(iptr_prime));
	printf("  wptr: 0x%08x (0x%08x)\n", (WORD)wptr, (WORD)adj2memstart(wptr));
	printf("\n");
	for(i = 0; i < NUM_PRI; i++)
	{
		printf("  fptr[%02d]:  0x%08x  (0x%08x)\n", i, (WORD)fptr[i], (WORD)adj2memstart(fptr[i]));
		printf("  bptr[%02d]:  0x%08x  (0x%08x)\n", i, (WORD)bptr[i], (WORD)adj2memstart(bptr[i]));
		printf("  tptr[%02d]:  0x%08x  (0x%08x)\n", i, (WORD)tptr[i], (WORD)adj2memstart(tptr[i]));
		printf("  tnext[%02d]: 0x%08x [%11d]\n", i, (WORD)tnext[i], (WORD)tnext[i]);
	}
	printf("\n");
	printf("  clock:     0x%08x [%11d]\n", time, time);
	printf("\n");
	printf("  areg:      0x%08x [%11d]\n", areg, areg);
	printf("  breg:      0x%08x [%11d]\n", breg, breg);
	printf("  creg:      0x%08x [%11d]\n", creg, creg);
	printf("\n");
	printf("  oreg:      0x%08x [%11d]\n", oreg, oreg);

}

void print_state(void)
{
	/*int time = get_time();*/
	BYTEPTR ptr;
	//WORDPTR sptr;
	/*int i; *//* General counter */
	//int czeros; /* consequtive zeros */
	/* Since the iptr always points to the next instruciton */
	BYTEPTR iptr_prime = byteptr_minus(iptr, 1);

	print_registers();

	printf("\n");

	print_scheduling_queue();
	print_timer_queue();

	printf("Code: (iptr: 0x%08x %c (0x%08x) (iptr_prime: 0x%08x (0x%08x)))\n", 
			(WORD)iptr, status_codes[valid_code(iptr)], (WORD)adj2codestart(iptr), 
			(unsigned int)iptr_prime, (WORD)adj2codestart(iptr_prime));
	printf("  ");
  for(ptr = iptr_prime - 5; ptr < iptr_prime; ptr++)
  {
		if(valid_code(ptr))
		{
			printf("0x%02x ", read_byte(ptr));
		}
  }
	if(valid_code(ptr))
	{
		printf(">0x%02x< ", read_byte(ptr));
	}
  for(ptr = iptr_prime + 1; ptr < iptr_prime + 1 + 5; ptr++)
  {
		if(valid_code(ptr))
		{
			printf("0x%02x ", read_byte(ptr));
		}
  }
	printf("\n\n");

	print_stack();
	print_vectorspace();
	print_mobilespace();
#ifdef DMEM_DEBUG
	print_dmem();
#endif
#if 0
	czeros = 0;
	printf("Stack: (wptr = 0x%08x (0x%08x)\n", (WORD)wptr, (WORD)adj2memstart(wptr));
	for(sptr = topmem; sptr < botmem; sptr=wordptr_plus(sptr, 1))
	{
		if(read_word(sptr) != 0 || sptr == wptr)
		{
			if(czeros > 4)
			{
				/* Back up */
				sptr = wordptr_minus(sptr, 2);
			}
			czeros = 0;
		}
		else
		{
			czeros = czeros + 1;
		}

		if(czeros <= 2)
		{
			char indicator_l, indicator_r;
			if(valid_code((BYTEPTR)sptr))
			{
				indicator_l = '{';
				indicator_r = '}';
			}
			else if(sptr == wptr)
			{
				indicator_l = '>';
				indicator_r = '<';
			}
			else
			{
				indicator_l = ' ';
				indicator_r = ' ';
			}
			printf(" %c0x%08x (0x%08x): 0x%08x [%11d]%c\n", 
					indicator_l, (WORD)sptr, (WORD)adj2memstart(sptr), read_word(sptr), read_word(sptr), indicator_r);
		}
		else if(czeros == 3)
		{
			printf("  ...\n");
		}
	}
#endif
}

void print_stack()
{
	WORDPTR top = (WORDPTR)((BYTEPTR)workspace_ptr - workspace_size);
	WORDPTR bottom = workspace_ptr;
	
	printf("Stack: (wptr = 0x%08x (0x%08x) (top = 0x%08x, bot = 0x%08x)\n",
			(WORD)wptr, (WORD)adj2memstart(wptr), (WORD)top, (WORD)bottom);

	print_mem(top, bottom, wptr, MEM_FILL_WORD);
}

void print_vectorspace()
{
	if(vectorspace_ptr)
	{
		WORDPTR top = vectorspace_ptr;
		WORDPTR bottom = (WORDPTR)((BYTEPTR)vectorspace_ptr + vectorspace_size);

		printf("Vector space: (wptr = 0x%08x) (top = 0x%08x, bot = 0x%08x)\n", (WORD)wptr, (WORD)top, (WORD)bottom);
		print_mem(top, bottom, wptr, MEM_FILL_WORD);
	}
}

void print_mobilespace()
{
	if(mobilespace_ptr)
	{
		WORDPTR top = mobilespace_ptr;
		WORDPTR bottom = (WORDPTR)((BYTEPTR)mobilespace_ptr + mobilespace_size);

		printf("Mobile space: (wptr = 0x%08x) (top = 0x%08x, bot = 0x%08x)\n", (WORD)wptr, (WORD)top, (WORD)bottom);
		print_mem(top, bottom, wptr, 0x80000000);
	}
}

#ifdef DMEM_DEBUG
void print_dmem_node(const void *ptr, VISIT order, int level)
{
	if (!(order == postorder || order == leaf))
	{
		return;
	}

	memories* node = *(memories **)ptr;
	WORDPTR start = node->ptr;
	WORD size  = node->size / WORDLENGTH;
	WORDPTR end   = start + size;

	printf("Dmem: (top = 0x%08x, bot = 0x%08x, size = %d)\n", 
			(WORD) start, (WORD) end, size);
	print_mem(start, end, wptr, MEM_FILL_WORD);
}

void print_dmem()
{
	twalk(memories_root, print_dmem_node);
}
#endif

void print_mem(WORDPTR start, WORDPTR end, WORDPTR point_at, WORD what_is_zero)
{
	int czeros = 0;
	WORDPTR sptr;

	for(sptr = start; sptr < end; sptr=wordptr_plus(sptr, 1))
	{
		if(read_word(sptr) != what_is_zero || sptr == point_at)
		{
			if(czeros > 4)
			{
				/* Back up */
				sptr = wordptr_minus(sptr, 2);
			}
			czeros = 0;
		}
		else
		{
			czeros = czeros + 1;
		}

		if(czeros <= 2)
		{
			char indicator_l, indicator_r;
			if(valid_code((BYTEPTR)sptr))
			{
				indicator_l = '{';
				indicator_r = '}';
			}
			else if(sptr == point_at)
			{
				indicator_l = '>';
				indicator_r = '<';
			}
			else
			{
				indicator_l = ' ';
				indicator_r = ' ';
			}
			printf(" %c0x%08x (0x%08x): 0x%08x [%11d]%c\n", 
					indicator_l, (WORD)sptr, (WORD)adj2memstart(sptr), read_word(sptr), read_word(sptr), indicator_r);
		}
		else if(czeros == 3)
		{
			printf("  ...\n");
		}
	}
}

void dbg_print_state(void)
{
	static int count = 0;

	printf("--------------------------------------------------------------------------------\n");
	printf("                               # %d - ins: %02x\n", count++, read_byte(iptr));
	printf("--------------------------------------------------------------------------------\n");
#ifdef LOCKSTEP_CLOCK
	lockstep_counter = lockstep_counter + LOCKSTEP_MULTIPLIER;
#endif
}

static void stiw_ins_not_implemented(void)
{
	/* Since the iptr always points to the next instruciton */
	BYTEPTR iptr_prime = iptr - 1;

	print_crash(EXIT_INS_NOT_IMP);
	printf("Instruction (0x%1x) (arg: 0x%1x) @0x%04x (0x%08x) not implemented\n", read_byte(iptr_prime) >> 4, read_byte(iptr_prime) & 0xf, iptr_prime - code_start, (WORD)iptr_prime);
	/* If it is a secondary instruction really, mention that */
	if((read_byte(iptr_prime) >> 4) == 0xf)
	{
		printf("Instruction is SECONDARY instruction #0x%02x\n\n", oreg);
	}
	else
	{
		printf("Instruction is PRIMARY instruction #0x%1x_\n\n", read_byte(iptr_prime) >> 4);
	}
	print_state();
	printf("exiting... (unimplemented instruction)\n");
}

static void stiw_ins_invalid(void)
{
	/* Since the iptr always points to the next instruciton */
	BYTEPTR iptr_prime = iptr - 1;

	print_crash(EXIT_INS_INVALID);
	printf("Instruction (0x%02x) (arg: 0x%02x) @%x (0x%08x) not implemented\n", read_byte(iptr_prime) >> 4, read_byte(iptr_prime) & 0xf, iptr_prime - code_start, (WORD)iptr_prime);
  /* If it is a secondary instruction really, mention that */
	if((read_byte(iptr_prime) >> 4) == 0xf)
	{
		printf("Instruction is SECONDARY instruction #0x%02x\n\n", oreg);
	}
	else
	{
		printf("Instruction is PRIMARY instruction #0x%1x_\n\n", read_byte(iptr_prime) >> 4);
	}
	printf("instruction (iptr: %x) invalid\n", iptr_prime - code_start);
	print_state();
	printf("exiting... (invalid instruction)\n");
}

#if defined LOCKSTEP_CLOCK
static WORD stiwc_get_time(void)
{
	/*fprintf(stderr, "t: %u\n", (WORD)lockstep_counter);*/
	return (WORD)lockstep_counter;
}
#elif !defined WIN32
static WORD stiwc_get_time(void)
{
	struct timeval t;

	gettimeofday(&t, 0);
	return (WORD)((t.tv_sec * 1000000) + t.tv_usec);
}
#elif defined WIN32
static WORD stiwc_get_time(void)
{
	/*
	http://msdn.microsoft.com/library/default.asp?url=/library/en-us/sysinfo/base/filetime_str.asp
	"The FILETIME structure is a 64-bit value representing the number of 
	 100-nanosecond intervals since January 1, 1601 (UTC)."
	*/
	FILETIME time;
	ULARGE_INTEGER usecs;
	
	/* Get the system time */
	GetSystemTimeAsFileTime(&time);
	
	/* Put it into a structure we can work with, and turn it into usecs
	 * See: 
	 *  http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winprog/winprog/ularge_integer_str.asp
	 */
	memcpy(&usecs, &time, sizeof(FILETIME));
	usecs.QuadPart = usecs.QuadPart / 10;

	/* Return the clock, just the lower parts thankyou */
	return usecs.LowPart;
}
#else
static WORD stiwc_get_time(void)
{
	struct timeb milli;

	ftime(&milli);
	/* fprintf(stderr, "t: %u\n", (WORD)(milli.millitm + (milli.time * 1001))); */
	return (WORD)(milli.millitm + (milli.time * 1000));
}
#endif /* WIN32 */

/* Not sure where to define these, they could be platform specific */
#define EXIT_SIGSEGV 139
#define EXIT_SIGBUS 138

static void sigsegv_handler(int num)
{
	/* Set the signal back to the default so we dont get into
	 * some kind of ugly loop should we segfault in here */
	signal(SIGSEGV, SIG_DFL);

	print_crash(EXIT_SIGSEGV);
	printf("Segmentation fault (%d)\n", num);
	printf("\n");
	print_state();
	printf("exiting... (segmentation fault)\n");

	if(code_start <= iptr && iptr < code_end)
	{
		print_debug_info(filename);
	}

	/* 139 */
	exit(EXIT_SIGSEGV); /* This seems to be what a segfaulted program returns */
}

#if ((!defined WIN32) && (!defined CYGWIN))
/* Borland does not have sigbus */
static void sigbus_handler(int num)
{
	/* Set the signal back to the default so we dont get into
	 * some kind of ugly loop should we segfault in here */
	signal(SIGBUS, SIG_DFL);

	print_crash(EXIT_SIGBUS);
	printf("Bus error (%d)\n", num);
	printf("\n");
	print_state();
	printf("exiting... (bus error)\n");

	if(code_start <= iptr && iptr < code_end)
	{
		print_debug_info(filename);
	}

	/* 138 */
	exit(EXIT_SIGBUS); /* This seems to be what a buserrored program returns */
}
#endif

#ifdef ENABLE_SCHED_SYNC
static void sigalrm_handler(int num)
{
	sched_sync = 1;
}
#endif /* ENABLE_SCHED_SYNC */

int get_file_size(FILE *fp, char* file_name, char* app_name)
{
	int instsize;

	/* Obtain the size of the file and deal with errors */
	if(fseek(fp, 0, SEEK_END) != 0)
	{
		printf("%s: an error (#1) occured while optaining the size of %s!\n", app_name, file_name);
		exit(1);
	}
	instsize = ftell(fp);
	if(instsize == -1)
	{
		printf("%s: an error (#2) occured while optaining the size of %s!\n", app_name, file_name);
		exit(1);
	}
	if(fseek(fp, 0, SEEK_SET) != 0)
	{
		printf("%s: an error (#3) occured while optaining the size of %s!\n", app_name, file_name);
		exit(1);
	}

	return instsize;
}

#ifdef STIWTVM
int parse_bytecode_v1(char *filename)
{
	int result;

	/* Read in any FFI information */
	result = setup_ffi_table(filename);
	if(result != 0)
	{
		return result;
	}

	/* There's no information about memory size in this format, so pick an
	 * arbitrary size unless we've been told otherwise. */
	if(memsize == 0)
	{
		workspace_size = 10 * 1024;
	}

	/* Adjust the transputercode pointer to avoid the first 4 characters, the
	 * header */
	transputercode += 4;
	instsize -= 4;

	return 0;
}

int parse_bytecode_v2(char *filename)
{
	int result;

	/* Read in any FFI information */
	result = setup_ffi_table(filename);
	if(result != 0)
	{
		return result;
	}

#ifdef HOST_BIGENDIAN
	workspace_size   = ((int *) transputercode)[1];
	vectorspace_size = ((int *) transputercode)[2];
	mobilespace_size = ((int *) transputercode)[3];
#else
	workspace_size   = SwapFourBytes(((int *) transputercode)[1]);
	vectorspace_size = SwapFourBytes(((int *) transputercode)[2]);
	mobilespace_size = SwapFourBytes(((int *) transputercode)[3]);
#endif

	/* Round up to multiples of 2 and adjust to bytes */
	workspace_size = (workspace_size + (workspace_size & 1)) * WORDLENGTH;
	vectorspace_size = (vectorspace_size + (vectorspace_size & 1)) * WORDLENGTH;
	mobilespace_size = (mobilespace_size + (mobilespace_size & 1)) * WORDLENGTH;

	/* The sizes added to workspace and vectorspace come from CCSP */
	workspace_size = workspace_size + 32 * WORDLENGTH; 
	if(vectorspace_size != 0)
	{
		vectorspace_size = vectorspace_size + 256 * WORDLENGTH;
	}
	/* My adjustments to the sizes */
	/* Initial stack frame, -1 iptr, chans, ms + vs ptrs */
	workspace_size = workspace_size + 6 * WORDLENGTH; 
	/* Add memory for handlers */
	/* (1 channel word + 4 stack frame) * 3 + ws + ws + ws + alignment */
	workspace_size = workspace_size + ((1 * WORDLENGTH + 4 * WORDLENGTH) * 3) + (1 * WORDLENGTH);


	/* Adjust the transputercode pointer to avoid the first 16 characters, the
	 * header */
	transputercode += 16;
	instsize -= 16;

	return 0;
}
#endif 

//Genericifying the setting up of channels.
void init_special_process(WORD *arg, BYTEPTR code, int code_ws_size)
{
	WORDPTR *channel_pointer = (WORDPTR *) arg;
	WORD argv[1];

	/* Allocate one word of memory for the 'CHAN BYTE' channel */
	wptr = wordptr_minus(wptr, 1);
	*channel_pointer = wptr;
	/* Set the channel to NOT.PROCESS.P */
	write_word(*channel_pointer, NOT_PROCESS_P);
	/* Initialise a stack frame */
	argv[0] = *arg;
	init_stackframe(&wptr, 
		1, argv, 		/* one argument */
		(WORDPTR) NULL_P, 	/* no vectorspace */
		(WORDPTR) NULL_P,	/* no mobilespace */
		(WORDPTR) NULL_P,	/* no forking barrier */
		RET_ERROR, 0		/* seterr on return */
	);
#ifdef MEM_LAYOUT_DEBUG
	printf("special process code=0x%08x has wptr=0x%08x\n", code, wptr);
#endif
	/* Put the process on the runqueue */
	add_to_queue((WORD)wptr, (WORD)code);
	/* Allocate enough workspace to be able to run the handler process */
	/* (using *_memsize is probably a bit conservative) */
	wptr = wordptr_minus(wptr, code_ws_size);
}

BYTEPTR copy_bytecode(BYTEPTR dest, unsigned char *src, int size)
{
	int i;

	for(i = 0; i < size; i++)
	{
		write_byte(dest, *src++);
		dest = byteptr_plus(dest, 1);
	}

	return dest;
}

int main(int argc, char *argv[])
{
	int i;
#ifdef STIWTVM
	int result = 1;
#endif

	/* Ultimately we might have a variable number of top level channels,
	 * currently we ALWAYS have keyboard, screen and error, or we die! */
	WORD stackframe_args[3]  = {0, 0, 0};

	/*Setup the 'tvm' command line option variables*/
	tvm_argc = argc;
	tvm_argv = argv;

	/* We need this in sig_handlers */
	filename = argv[1];

	/* Deal with loading in files if we are STIWTVM */
#ifdef STIWTVM
	FILE *bytecode_fp;

	if(argc == 1)
	{
		version(argv[0]);
		usage(argv[0]);

		return 1;
	}

	if(argc != 2 && argc != 3)
	{
		printf("%s: wrong number of arguments!\n", argv[0]);
		usage(argv[0]);

		return 1;
	}

	if(argc == 3)
	{
		char *endptr;
		
		memsize = strtol(argv[2], &endptr, 0);

		if(!(*argv[2] != 0 && *endptr == 0))
		{
			printf("%s: invalid memory size specified!\n", argv[0]);
			usage(argv[0]);

			return 1;
		}
		if(memsize <= 0)
		{

			printf("%s: memory size less than or equal to zero specified!\n", argv[0]);
			usage(argv[0]);

			return 1;
		}
	}

#ifdef __MOBILE_PI_SUPPORT__
	dmem_init();
#ifdef DMEM_DEBUG
	dmem_debug_init();
#endif
#endif

	/* Open the file and deal with errors */
	bytecode_fp = fopen(argv[1], "rb");
	if(bytecode_fp == 0)
	{
		printf("%s: could not open file: %s!\n", argv[0], argv[1]);
		usage(argv[0]);
		return 1;
	}

	/* Get the size of the file */
	instsize = get_file_size(bytecode_fp, argv[1], argv[0]);

	/* Allocate memory to read in the file, and deal with errors... */
	infile_start = transputercode = (unsigned char *)malloc(instsize);
	if(transputercode == 0)
	{
		printf("%s: an error occured while allocating memory to read in file: %s!\n", argv[0], argv[1]);
		return 1;
	}
	/* Check if we got a valid file */
	if(fread(transputercode, 1, 4, bytecode_fp) != 4)
	{
		printf("%s: unexcpected end of file, while reading file: %s!\n", argv[0], argv[1]);
		return 1;
	}
	/* Figure out what kind of file we have */
	if(strncmp("tvm", (char *)transputercode, 3) == 0)
	{
		/* Its a .tbc file, there will (may) be a separate .ffi and .tvmdbg file */
		filetype = TBC;
	} 
	else if(strncmp("tbz", (char *)transputercode, 3) == 0)
	{
		/* Its a .tbz file, it has everything in it (ffi, if required, and debug, if
		 * not stripped) */
		filetype = TBZ;
	}
	else
	{
		printf("%s: %s is not a valid transterpreter bytecode file!\n", argv[0], argv[1]);
		return 1;
	} 
	/* read in the file */
	transputercode_ptr = transputercode + 4;
	while(!feof(bytecode_fp))
	{
		transputercode_ptr += fread(transputercode_ptr, 1, 1024, bytecode_fp);

		if(ferror(bytecode_fp) != 0)
		{
			printf("%s: error (%d) while reading file: %s!\n", argv[0], ferror(bytecode_fp), argv[1]);
			return 1;
		}
	}
	/* Close the file */
	if(fclose(bytecode_fp) != 0)
	{
		printf("%s: error while closing file: %s!\n", argv[0], argv[1]);
		return 1;
	}

	if(filetype == TBC)
	{
		switch(transputercode[3])
		{
			case 0x01:
				result = parse_bytecode_v1(argv[1]);
				break;
			case 0x02:
				result = parse_bytecode_v2(argv[1]);
				break;
			default:
				printf("%s: %s invalid bytecode version (v%02x)!\n", argv[0], argv[1], transputercode[3]);
				return 1;
		}
	}
	else if(filetype == TBZ)
	{
		result = parse_tbz_v0(argv[1]);
	}
	/* If an error occured */
	if(result != 0)
	{
		return result;
	}
#endif /* STIWTVM */

	/* Reserve workspace for the handler processes. */
#ifndef DISABLE_KEYBOARD_PROC
	workspace_size += (kbh_ws_size + (kbh_ws_size & 1)) * WORDLENGTH;
#endif
#ifndef DISABLE_SCREEN_PROC
	workspace_size += (scrh_ws_size + (scrh_ws_size & 1)) * WORDLENGTH;
#endif
#ifndef DISABLE_ERROR_PROC
	workspace_size += (errh_ws_size + (errh_ws_size & 1)) * WORDLENGTH;
#endif

	if (memsize == 0) {
		/* Compute a base memory size, including a small buffer. */
		memsize = workspace_size
			  + vectorspace_size
			  + mobilespace_size
			  + 32 * WORDLENGTH;
	}

#ifndef WORDPTRS_REAL
	/* Reserve instruction space for the handler processes. */
#ifndef DISABLE_KEYBOARD_PROC
	memsize += kbh_inst_size;
#endif
#ifndef DISABLE_SCREEN_PROC
	memsize += scrh_inst_size;
#endif
#ifndef DISABLE_ERROR_PROC
	memsize += errh_inst_size;
#endif

	/* Reserve instruction space for the user program. */
	memsize += instsize;
#endif

	/* Pad to make sure we can refer to all the memory as words. */
	memsize += WORDLENGTH;

	/* This doesn't guarantee that there will be enough memory, but it'll
	 * at least complain if there definitely isn't. */
	if(memsize < workspace_size + vectorspace_size + mobilespace_size)
	{
		fprintf(stderr, "%s: Not enough memory allocated for program (allocated: %d, needed %d)!\n", 
				filename, memsize, workspace_size + vectorspace_size + mobilespace_size);
	}

	/* The layout inside transputermem (which is memsize bytes long) is:

	(WORDPTR topmem and char *transputermem point here)
	- workspace_size bytes of workspace, which consists of:
		- workspace for the user program
	(WORDPTR wptr points here)
		- workspace for the error handler
		- workspace for the screen handler
		- workspace for the keyboard handler
	(WORDPTR workspace_ptr points here)
	(WORDPTR vectorspace_ptr points here)
	- vectorspace_size bytes of vectorspace
	(WORDPTR mobilespace_ptr points here)
	- mobilespace_size bytes of mobilespace
	[if !WORDPTRS_REAL]
	(BYTEPTR code_start points here)
	(BYTEPTR iptr points here)
	- instsize bytes of Transputer bytecode for the user program
	(BYTEPTR code_end points here)
	(BYTEPTR kbh_code_ptr points here)
	- kbh_inst_size bytes of Transputer bytecode for the keyboard handler
	(BYTEPTR scrh_code_ptr points here)
	- scrh_inst_size bytes of Transputer bytecode for the screen handler
	(BYTEPTR errh_code_ptr points here)
	- errh_inst_size bytes of Transputer bytecode for the error handler
	[endif]
	(WORDPTR botmem points here)

	*/

	transputermem = (WORD *)malloc(memsize);
	if(!transputermem)
	{
		printf("%s: could not allocate enough memory (%d bytes)\n", 
				argv[0], memsize);
		exit(1);
	}

	/* Clear all the transputer memory */
	memset(transputermem, MEM_FILL_BYTE, memsize); // memsize is in bytes

#ifdef WORDPTRS_REAL
	topmem = transputermem;
#else
	/* FIXME: This needs to be more generic, the whole ifdef will go when I get a
	 * chance to kill it... */
	setup_mem(transputermem, memsize);
	topmem = 0;
#endif
	botmem = wordptr_plusb(topmem, memsize);

	/* Set up the memory map */
	workspace_ptr = wordptr_plusb(topmem, workspace_size);
	vectorspace_ptr = workspace_ptr;
	mobilespace_ptr = wordptr_plusb(vectorspace_ptr, vectorspace_size);
	code_start = byteptr_plus((BYTEPTR)mobilespace_ptr, mobilespace_size);

	if(!vectorspace_size)
	{
		vectorspace_ptr = (WORDPTR) NULL_P;
	}

	if(mobilespace_size)
	{
		/* Clear the mobilespace to MINT */
		WORDPTR ptr = mobilespace_ptr;
		int i;

		for (i = 0; i < (mobilespace_size / WORDLENGTH); i++)
		{
			write_word(ptr, 0x80000000);
			ptr = wordptr_plus(ptr, 1);
		}
	}
	else
	{
		mobilespace_ptr = (WORDPTR) NULL_P;
	}

#ifdef WORDPTRS_REAL
	/* We can just use the code we loaded directly. */
	code_start = transputercode;
	#ifndef DISABLE_KEYBOARD_PROC
	kbh_code_ptr = (BYTEPTR) kbh_transputercode;
	#endif
	#ifndef DISABLE_SCREEN_PROC
	scrh_code_ptr = (BYTEPTR) scrh_transputercode;
	#endif
	#ifndef DISABLE_ERROR_PROC
	errh_code_ptr = (BYTEPTR) errh_transputercode;
	#endif
#else
	{
		BYTEPTR ptr = code_start;
		ptr = copy_bytecode(ptr, transputercode, instsize);
		#ifndef DISABLE_KEYBOARD_PROC
		kbh_code_ptr = ptr;
		ptr = copy_bytecode(ptr, kbh_transputercode, kbh_inst_size);
		#endif
		#ifndef DISABLE_SCREEN_PROC
		scrh_code_ptr = ptr;
		ptr = copy_bytecode(ptr, scrh_transputercode, scrh_inst_size);
		#endif
		#ifndef DISABLE_ERROR_PROC
		errh_code_ptr = ptr;
		ptr = copy_bytecode(ptr, errh_transputercode, errh_inst_size);
		#endif
	}
#endif
	code_end = code_start + instsize;
	iptr = code_start;
	wptr = workspace_ptr;

#ifdef MEM_LAYOUT_DEBUG
	printf("Memory layout:\n");
	printf(" @topmem = 0x%08x (real address 0x%08x; size 0x%08x (%d))\n", topmem, (unsigned int) transputermem, memsize, memsize);
	printf("  workspace_size = 0x%08x (%d)\n", workspace_size, workspace_size);
	printf(" @wptr = 0x%08x\n", wptr);
	printf(" @workspace_ptr = 0x%08x\n", workspace_ptr);
	printf(" @vectorspace_ptr = 0x%08x\n", vectorspace_ptr);
	printf("  vectorspace_size = 0x%08x (%d)\n", vectorspace_size, vectorspace_size);
	printf(" @mobilespace_ptr = 0x%08x\n", mobilespace_ptr);
	printf("  mobilespace_size = 0x%08x (%d)\n", mobilespace_size, mobilespace_size);
	printf(" @code_start = 0x%08x\n", code_start);
	printf(" @iptr = 0x%08x\n", iptr);
	printf("  instsize = 0x%08x (%d)\n", instsize, instsize);
	printf(" @code_end = 0x%08x\n", code_end);
	printf(" @kbh_code_ptr = 0x%08x\n", kbh_code_ptr);
	printf("  kbh_inst_size = 0x%08x (%d)\n", kbh_inst_size, kbh_inst_size);
	printf(" @scrh_code_ptr = 0x%08x\n", scrh_code_ptr);
	printf("  scrh_inst_size = 0x%08x (%d)\n", scrh_inst_size, scrh_inst_size);
	printf(" @errh_code_ptr = 0x%08x\n", errh_code_ptr);
	printf("  kbh_inst_size = 0x%08x (%d)\n", kbh_inst_size, kbh_inst_size);
	printf(" @botmem = 0x%08x\n", botmem);
#endif

	for(i = 0; i < NUM_PRI; i++)
	{
		fptr[i] = (WORDPTR)NOT_PROCESS_P;
		bptr[i] = (WORDPTR)NOT_PROCESS_P;
		tptr[i] = (WORDPTR)NOT_PROCESS_P;
	}
	areg = 0;
	breg = 0;
	creg = 0;
	oreg = 0;

	not_implemented = stiw_ins_not_implemented;
	invalid = stiw_ins_invalid;

	/* Set up the special FFI Hooks */
	special_ffi_table = get_special_hooks();

	/* Set up KYB, SCR, ERR hooks */
	//ext_chan_table[EXT_CHAN_KYB] = ext_chan_kyb;
	//ext_chan_table[EXT_CHAN_SCR] = ext_chan_scr;
	//ext_chan_table[EXT_CHAN_ERR] = ext_chan_err;

	/* Set up timer hooks */
	get_time = stiwc_get_time;

	/* Set up debug hooks */
#ifdef ENABLE_RUNLOOP_DBG_HOOKS
	runloop_dbg_hook_pre = dbg_print_state;
	runloop_dbg_hook_post = print_state;
#endif
#ifdef LOCKSTEP_CLOCK
	scheduler_busywait_hook = stiw_busywait_hook;
#endif	

#ifndef DISABLE_KEYBOARD_PROC
	/* Initialise the kbhandler process */
	init_special_process(&stackframe_args[0], kbh_code_ptr, kbh_ws_size);
#endif
#ifndef DISABLE_SCREEN_PROC
	/* Initialise the scrhandler process */
	init_special_process(&stackframe_args[1], scrh_code_ptr, scrh_ws_size);
#endif
#ifndef DISABLE_ERROR_PROC
	/* Initialise the errhandler process */
	init_special_process(&stackframe_args[2], errh_code_ptr, errh_ws_size);
#endif

	/* Initialise the stackframe for the "main" PROC */
	initial_stackframe(&wptr, 3, stackframe_args, vectorspace_ptr, mobilespace_ptr, 1);

#ifdef MEM_LAYOUT_DEBUG
	printf("Memory layout after handler/stackframe setup:\n");
	printf(" @wptr = 0x%08x\n", wptr);
	printf(" @stackframe_args[2] (err) = 0x%08x\n", stackframe_args[2]);
	printf(" @stackframe_args[1] (scr) = 0x%08x\n", stackframe_args[1]);
	printf(" @stackframe_args[0] (kb) = 0x%08x\n", stackframe_args[0]);
#endif

	/* Set up our signal handler, to gracefully handle segfaults */
	signal(SIGSEGV, sigsegv_handler);
#if ((!defined WIN32) && (!defined CYGWIN))
	signal(SIGBUS, sigbus_handler);
#endif
#ifdef ENABLE_SCHED_SYNC
	sched_sync = 0;
	signal(SIGALRM, sigalrm_handler);
#endif


	/* Start the interpreter off */
	i = run();

	/* Good exit? */
	if(i == EXIT_STACK_BOTTOM)
	{
		/* print_state(); */
		return 0;
	}

	/* Deadlock? */
	if(i == EXIT_DEADLOCK)
	{
		printf("\n\nTVM: Deadlock!\n");
		if(code_start <= iptr && iptr < code_end)
		{
			print_debug_info(argv[1]);
		}

		return 0;
	}

	/* Badish exit, due to no instruction */
	if(i == EXIT_INS_NOT_IMP || i == EXIT_INS_INVALID)
	{
		if(code_start <= iptr && iptr < code_end)
		{
			print_debug_info(argv[1]);
		}
		return i;
	}

	/* FIXME: For some of these instrucitons, we could be more specific as to what
	 * went wrong.
	 */
	if(i == EXIT_ERROR)
	{
		/* Print the reason for the error */
#define ERROR_RET_VAL 10
		printf("The Transterpreter encountered a runtime error:\n");

		/* In some cases, like DIV we could inspect the registers and provide a more
		 * detailed error message
		 */
		switch(error_flag)
		{
			case EFLAG_SETERR:
				printf("STOP error\n");
				break;
			case EFLAG_CSUB0:
				printf("CSUB0 error\n");
				break;
			case EFLAG_LADD:
				printf("LADD error\n");
				break;
			case EFLAG_LDIV:
				printf("LDIV error\n");
				break;
			case EFLAG_REM:
				printf("REM error\n");
				break;
			case EFLAG_DIV:
				printf("DIV error\n");
				break;
			case EFLAG_LSUB:
				printf("LSUB error\n");
				break;
			case EFLAG_CSNGL:
				printf("CSNGL error\n");
				break;
			case EFLAG_CCNT1:
				printf("CCNT1 error\n");
				break;
			case EFLAG_CWORD:
				printf("CWORD error\n");
				break;
			case EFLAG_ADC:
				printf("ADC error\n");
				break;
			case EFLAG_ADD:
				printf("ADD error\n");
				break;
			case EFLAG_SUB:
				printf("SUB error\n");
				break;
			case EFLAG_MUL:
				printf("MUL error\n");
				break;
			case EFLAG_BAR:
				printf("Barrier underflow\n");
				break;
			case EFLAG_FP:
				printf("Floating point error\n");
				break;
			case EFLAG_ALT:
				printf("ALT'ing error!\n");
				break;
			case EFLAG_CHAN:
				printf("Channel communication error\n");
				break;
			case EFLAG_DMEM:
				printf("Memory allocator error\n");
				break;
			case EFLAG_MT:
				printf("Mobile type error\n");
				break;
			case EFLAG_PROC:
				printf("Process API error\n");
				break;
			case EFLAG_SHUTDOWN:
				printf("Erroneous shutdown\n");
				break;
			default:
				printf("UNKNOWN error\n");
				break;
		}
#ifdef STIWTVM
		print_debug_info(argv[1]);
#endif
		return ERROR_RET_VAL;
	}

	/* Real bad exit probably! */
	print_crash(i);
	print_state();
	print_crash(i);

	if(code_start <= iptr && iptr < code_end)
	{
		print_debug_info(argv[1]);
	}

	return i;
}
