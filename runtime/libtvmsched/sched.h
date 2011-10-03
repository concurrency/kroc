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
};
typedef struct logical_processor *logical_processor_t;
/*}}}*/

