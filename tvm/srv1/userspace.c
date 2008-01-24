#include "userspace.h"

/* Generic TVM Includes */
#include "../../interpreter/transputer.h"
#include "../../interpreter/interpreter.h"
#include "../../interpreter/scheduler.h"
#include "../../interpreter/mem.h"

/* For WS_NEXT */
#include "../../interpreter/instructions.h"

/* SRV-1 Specific */
#include "memory_map.h"

/* Constants for loading usercode
 * At the beginning of the bytecode stream, header information
 * is stored as words.
 *
 * The HEADER_SIZE, however, is in BYTES. It is not stored in
 * the bytecode stream... it is just a constant that I am using
 * in subsequent computation.
 * */
    typedef enum {
        TBC_HEADER  = 0,
        TBC_WS_SIZE = 1,
        TBC_VS_SIZE = 2,
        TBC_MS_SIZE = 3,
        HEADER_SIZE = (4 * WORDLENGTH)
    } userspace_offsets;

/* This is the stopp instruction, which we use as the terminator. The terminator
 * is put as the 'process' to be returned to by the initial stackframe. Having a
 * terminator is necessary as running a program such as:
 * PROC kill()
 *   SKIP
 * :
 * Would otherwise kill the virtual machine as it would have nothing to jump
 * back to at the end of its execution. This very simple terminator has got one
 * significant drawback however: How do we communicate to the monitor that the
 * program is no longer running? Tricky. This is mainly a problem I think
 * because the start_user_program does everything w/o letting the monitor butt
 * in to, for example, inject a proper process to do this which can do some
 * comms.
 *
 * Perhaps this can be rectified by having a deadlock detection process, which
 * scans the run-queue at regular intervals, looking for a run-queue that does
 * not contain processes from the userspace.
 *
 * The way we do this is to set up an initial stackframe. Since we dont pass any
 * arguments to the user program, the slots for areg, breg, and creg are unused,
 * and it is only the iptr which is used once the user program calls its final
 * ret. Thus we need to put a pointer to an area which has the bytecode for
 * STOPP in the slot for iptr. Since areg, breg, and creg are not used we can
 * put the bytecode there.
 *    wptr+4    ....    [.... .... .... ....]
 *    wptr+3    creg    [0x00 0x00 0x00 0x00]
 *    wptr+2    breg    [0x00 0x00 0x00 0x00]
 *    wptr+1    areg    [0x21 0xF5 0x00 0x00]
 *    wptr+0    iptr    [______&Wptr+1______]    <--- wptr
 * The STOPP instruction stops the current process and stores the iptr in the
 * workspace. It stores the iptr at wptr-1. No extra space needs allocating for
 * the storage of the iptr, as it will get stored in what is (on the above
 * diagram wptr+3 due to the deallocation of the stackframe by the userprograms
 * RET instruction). Once the RET and STOPP instructions have executed, the
 * initial stackframe part of the workspace will look like this:
 *    wptr+0    ....    [.... .... .... ....]    <--- wptr
 *    wptr-1    creg    [_______iptr________]
 *    wptr-2    breg    [0x00 0x00 0x00 0x00]
 *    wptr-3    areg    [0x21 0xF5 0x00 0x00]
 *    wptr-4    iptr    [______&Wptr-3______]    
 * Thus the use of the STOPP instruction does not overwrite any data that it
 * should not be touching. Once the STOPP instruction has executed, it simply
 * fetches the next process from the run-queue and runs it.
 */
static char terminator[] = {0x21, 0xF5}; /* Bytecode for STOPP */

void start_user_program (int w[]) { /* []BYTE userspace, VAL INT tbc.length */
	/* Address of the TBC array. */
	BPOOTER userspace_start = (BPOOTER) w[0];
	/* Size of the bytecode array - not used in our calculations. */
	WORD userspace_size = (WORD) w[1];
	/* Length of TBC in userspace array. */
	WORD tbc_length = (WORD) w[2];

	BPOOTER us_code;
	POOTER us_wptr;
	POOTER us_ws;
	POOTER us_vs;
	POOTER us_ms;
	
	/* slinker always outputs big-endian numbers, 
	 * blackfin is little endian hence swap.
	 */
	WORD tbc_header 	= SwapFourBytes(read_mem((POOTER) pooter_plus((POOTER) userspace_start, TBC_HEADER)));
	/* us_code_size	= SwapFourBytes(read_mem((POOTER) pooter_plus((POOTER) userspace_start, TBC_BC_SIZE))); */
	WORD us_ws_size	= SwapFourBytes(read_mem((POOTER) pooter_plus((POOTER) userspace_start, TBC_WS_SIZE)));
	WORD us_vs_size	= SwapFourBytes(read_mem((POOTER) pooter_plus((POOTER) userspace_start, TBC_VS_SIZE)));
	WORD us_ms_size	= SwapFourBytes(read_mem((POOTER) pooter_plus((POOTER) userspace_start, TBC_MS_SIZE)));

	/* We need to allocate the initial stackframe, at least 4 words are needed */
	us_ws_size	= us_ws_size + 4;

	/* As noted above, the HEADER_SIZE is a size in BYTES, and we can see why here. */
	us_code		= bpooter_plus(userspace_start, HEADER_SIZE);
	us_ws		= (POOTER) bpooter_plus(userspace_start, tbc_length);

	/* Make sure the workspace is aligned */
	if (((WORD) us_ws) & (WORDLENGTH - 1)) {
		us_ws	= (POOTER) bpooter_plus(
			(BPOOTER) us_ws,
			WORDLENGTH - (((WORD) us_ws) & (WORDLENGTH - 1))
		);
	}

	/* Calculate the wptr, and add the top-level process to the run queue. */
	/* Remember, the workspace grows down TOWARDS ZERO. Therefore, we want the end
	 * of the user's workspace, and we'll set the us_ws pointer there. */
	us_wptr		= pooter_plus(us_ws, us_ws_size);
	us_vs		= pooter_plus(us_wptr, us_vs_size);
	us_ms		= pooter_plus(us_vs, us_ms_size);

	/* Allocate the initial stackframe, i.e. what a CALL would do.
	 * However we put the terminator code into the initial stackframe as most of
	 * it is not used anyway (it has space for areg, breg and creg, ie params,
	 * but we cannot pass anything to user occam process on the LEGO so they are
	 * empty */
	init_stackframe(&us_wptr, 0, NULL, us_vs, us_ms, 0, RET_REAL, (BPOOTER) terminator);

	/* add_to_queue comes straight out of scheduler.c. It adds the workspace passed
	 * to the runqueue, and stamps the IPTR given (second argument) into that workspace.
	 * This does NOT reschedule, however! So, after invoking this FFI, some sort of 
	 * reschedule must happen (timer, channel comm, RESCHEDULE(), etc.) before we 
	 * have a hope of seeing this new process on the queue.
	 */
	add_to_queue((WORD) us_wptr, (WORD) us_code);
}

void end_user_program (int w[]) { /* []BYTE userspace, VAL INT tbc.length */
    /* Address of the userspace array. */
    BPOOTER userspace_start = (BPOOTER) w[0];
    /* Size of the userspace array. */
    WORD userspace_size = (WORD) w[1];
    /* Length of TBC in userspace. */
    WORD tbc_length = (WORD) w[2];

    /* End of the bytecode array */
    BPOOTER userspace_end = (BPOOTER) bpooter_plus(userspace_start, userspace_size);
    /* Iterator */
    int i;

    /* PRIORITY QUEUES / RUN QUEUES */
    /* This removes processes from the run-queue(s) which are in the userspace */
    for (i = 0; i < NUM_PRI; i++) {
        POOTER prev = NOT_PROCESS_P;
        POOTER ptr = fptr[i];
        while (ptr != (POOTER)NOT_PROCESS_P) {
            /* Remove things from the queue if they are in the userspace */
            if ((ptr > (POOTER)userspace_start) && (ptr <= (POOTER)userspace_end)) {
              /* FIXME: This bit is formatted differently for now, as I copy and
               * pasted it from elsewhere. The formatting in this file should be
               * fixed. */
              if(prev == NOT_PROCESS_P)
              {
	      	fptr[i] = (POOTER)WORKSPACE_GET(ptr, WS_NEXT);
                /* Queue now empty, update back. */
                if(fptr[i] == NOT_PROCESS_P)
                {
                  bptr[i] = NOT_PROCESS_P;
                }
              }
              else
              {
	      	POOTER next = (POOTER) WORKSPACE_GET(ptr, WS_NEXT);
		WORKSPACE_SET(prev, WS_NEXT, (WORD) next);
                /* if we were the back, then update bptr. */
		if(next == NOT_PROCESS_P)
                {
                  bptr[i] = prev;
                }
              }
            }
            else
            {
              /* Did not need to delete this, advance */
              prev = ptr;
            }
	    ptr = (POOTER) WORKSPACE_GET(ptr, WS_NEXT);
      }
    }


    /* Delete things from the timer list that are in the userspace */
    for (i = 0; i < NUM_PRI; i++) {
        POOTER prev = NOT_PROCESS_P;
        POOTER ptr = tptr[i];
        while (ptr != (POOTER)NOT_PROCESS_P) {
            /* If this is INSIDE the process memory, or if it is
             * NOT OUTSIDE the userspace */
            if ((ptr > (POOTER)userspace_start) && (ptr <= (POOTER)userspace_end)) {

                if (prev == NOT_PROCESS_P) {
                    /* Delete the head of the list */
                    tptr[i] = (POOTER)WORKSPACE_GET(ptr, WS_NEXT_T);
                    if(tptr[i] != NOT_PROCESS_P){
                        tnext[i] = (WORD)WORKSPACE_GET(tptr[i], WS_TIMEOUT);	
                    } else {
                        /* Leave the queue untouched */
                    } 
                } else { /* prev != NOT_PROCESS */
                    WORKSPACE_SET(prev, WS_NEXT_T, WORKSPACE_GET(ptr, WS_NEXT_T));
                }
            } else {
                /* If we deleted something, we don't want to change prev... as 
                 * prev is still valid. However, if we did not delete anything, 
                 * that means the current value is still valid, and we should
                 * update the prev pointer. */
                prev = ptr;				
            } /* end else ... */	
            ptr = (POOTER) WORKSPACE_GET(ptr, WS_NEXT_T);
        }
    }
}
