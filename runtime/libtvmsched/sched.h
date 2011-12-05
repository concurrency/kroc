/*
header for currently external workstealling wait-free scheduler

*/

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
		struct logical_processor *partner; /*processor this processor steals from */

};
typedef struct logical_processor *logical_processor_t;
/*}}}*/



/* declare functions from lowlevelsched.c
   now in schedlib.c */
/*{{{ */
void setStolen(batch_t b);
void remove_from_window(batch_t b);
void extend_window(batch_t b);
batch_t dequeue_window_batch(logical_processor_t p);
logical_processor_t selectprocessor(logical_processor_t p, int newPartner);
int isStolen(batch_t b);
int inWindow(batch_t b);
/*}}}*/
