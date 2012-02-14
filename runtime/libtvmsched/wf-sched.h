/*
header for currently external workstealling wait-free scheduler
*/

#include <pthread.h>

/* declare constant for number of virtual processors*/
#define NUMBER_OF_PROCS 4
#define DISPATCH_COUNT 4



/*{{{ */
struct process{
		struct process *next;
		int id;
};
typedef struct process *process_t;
/*}}}*/


/*{{{ */
struct batch {
		struct process *head;
		struct process *tail;
		struct batch *next;
		int stolen;	/* 1 if batch is stolen */
		int window;	/* 1 if batch is in window */
};
typedef struct batch *batch_t;
/*}}}*/



/*{{{ */
struct logical_processor {
		struct batch *head; /*run queue head */
		struct batch *tail; /*run queue tail */
		struct batch *activeQ;
		int dispatch_count;
		struct process *current_process;
		int partner; /* id of processor this processor steals from */
		pthread_mutex_t *run_queue_lock; 	 /* lock on run queue */

		int id; /* identity in global list */
};
typedef struct logical_processor *logical_processor_t;
/*}}}*/



/* declare functions for finegrained interactoins
   now in schedlib.c */
/*{{{ */
void setStolen(batch_t b);
void remove_from_window(batch_t b);
void extend_window(batch_t b);
batch_t dequeue_window_batch(logical_processor_t p);
logical_processor_t selectprocessor(logical_processor_t p);
int isStolen(batch_t b);
int inWindow(batch_t b);

void * test_run(void * arg);
/*}}}*/


/* global list of processor states */
/* contains a 0 if processor currently has no work */
extern int global_procs[NUMBER_OF_PROCS];

/* global list of pointers to processors*/
extern logical_processor_t global_proc_pointer[NUMBER_OF_PROCS];
