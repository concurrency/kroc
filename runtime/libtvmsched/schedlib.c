/*
library of functions for simple scheduling
*/

#include <stdio.h>
#include <stdlib.h>
#include <wf-sched.h>

/*
 * compile with -DLEVEL=x for debuging
 * or -DNODEBUG to not compile debug code
 */
/*{{{*/
#ifndef NODEBUG
#ifndef LEVEL
#define LEVEL 0;
#endif
#define DEBUG(lvl, proc, str) if (LEVEL <= lvl)\
											printf("%d: %s\n", proc, str);

#else
#define DEBUG(lvl, proc, str)
#endif
/*}}}*/


int atomic_swap(logical_processor_t p, int* a, int b)
{
	DEBUG(0, p->id, "atomic_swap: function called");
	/* TODO: atomic magic swaper */
	/* for now we just you mutex lock for testing */



	/* LOCK */
	pthread_mutex_lock(p->run_queue_lock);
	DEBUG(1, p->id, "atomic_swap: run_queue locked");

	if(*a == b)
	{
		/* UNLOCK */
		pthread_mutex_unlock(p->run_queue_lock);
		DEBUG(1, p->id, "atomic_swap: run_queue unlocked");
		DEBUG(0, p->id, "atomic_swap: return b (usually 0)");
		return b;
	}
	else
		*a=b;

	/* UNLOCK */
	pthread_mutex_unlock(p->run_queue_lock);
	DEBUG(1, p->id, "atomic_swap: run_queue unlocked");
	DEBUG(0, p->id, "atomic_swap: return 1");
	return 1;

}



void enqueue(logical_processor_t p)
{
	DEBUG(0, p->id, "enqueue: function called");
	batch_t batch = p->activeQ;

	/* update as not stolen and next pointer*/
	batch->next = NULL;
	batch->stolen = 1; 


	/* link batch into runqueue */
	p->tail->next = batch;

	/* check to see if runqueue is empty */
	if(p->head == NULL)
	{
		DEBUG(1, p->id, "enqueue: runqueue currently empty");
		p->head = batch;	
	}

	p->tail = batch;

	DEBUG(1, p->id, "enqueue: batch linked into runqueue");

	/* the current window size is less the max, batch to window */

	if(p->window_size > MAX_WINDOW_SIZE)
	{
		DEBUG(2, p->id, "enqueue: windowsize > MAX_WINDOW_SIZE, store batch in window");
		p->window_size++;
		batch->window = 1;
	}
	DEBUG(0, p->id, "enqueue: function complete");
}


int local_dequeue (logical_processor_t p)
{
	DEBUG(0, p->id, "local_dequeue: function called");
	/* remove head of runqueue  */
	p->activeQ = p->head;
	p->head = p->head->next;
	DEBUG(1, p->id, "local_dequeue: removed head of runqueue");

	/* check runqueue is not empty */
	/* should not be necessary */
	if(p->activeQ == NULL)
	{
		DEBUG(2, p->id, "local_dequeue: runqueue empty, return 0");
		return 0;
	}

	while((p->activeQ != NULL) && 
			(atomic_swap(p, &(p->activeQ->stolen), 0) == 0))
	{
		DEBUG(1, p->id, "local_dequeue: top of while, dequeued a stolen batch");

		/* batch has been stolen already */ 
		/* do laundry */
		p->window_size--;
	
		/* if active queue is now tail we have gone through 
		 * the runqueue and found nothing, return with failure */

		 /*TODO: should not need both these checks but gets caught in loop
		  on some inputs without it, this indicates problem elsewhere
		  with tails next not pointing to null */
		if(p->activeQ->next == NULL || p->activeQ == p->tail)
		{
			DEBUG(2, p->id, "local_dequeue: dequeud batch=tail, return 0");
			return 0;
		}

		
		/* get next batch in queue */		
		p->activeQ = p->head;
	 	p->head = p->head->next;
		DEBUG(1, p->id, "local_dequeue: dequeue next batch");
	}

	/* batch was dequeued */
	DEBUG(2, p->id, "local_dequeue: successfully dequeued batch");

	/* if batch was in window update window size */
	if(p->activeQ->window == 1)
	{
		DEBUG(1, p->id, "local_dequeue: batch was in window, dec size");
		p->window_size--;
	}

	DEBUG(2, p->id, "local_dequeue: batch dequeue success");
	DEBUG(0, p->id, "local_dequeue: function complete, returning 1");
	return 1; 
}

int atomic_remote_dequeue (logical_processor_t p, logical_processor_t v)
{
	DEBUG(0, p->id, "atomic_remote_dequeue: function called");

	/* get pointer to first batch on victum */
	batch_t b=v->head; 
	 
	if(b==NULL)
	{
		DEBUG(0, p->id, "atomic_remote_dequeue: victum had empty runqueue, return 0");
		return 0;
	}

	while(1)
	{
		if(b == NULL)
			return 0;

		DEBUG(0, p->id, "atomic_remote_dequeue: top of while true");
	 	/* go through victum queue and find first batch in the window */
	 	while(b->window==0)
	 	{
			DEBUG(1, p->id, "atomic_remote_dequeue: batch not in window");
			/* if we get to the last element we are done */
			if(b == NULL || b->next==NULL || b == p->tail)
			{
				DEBUG(2, p->id, "atomic_remote_dequeue: at last batch in run_queue, return 0");
				return 0;
			}
			DEBUG(1, p->id, "atomic_remote_dequeue: getting next batch");
			b = b->next;
	 	}
	 
		DEBUG(0, p->id, "atomic_remote_dequeue: batch in window found");
	 	/* swap window entry with null  
		 * if result is null batch has already been stolen
		 * else steal batch 
		 */
	 	if(atomic_swap(v, &(b->stolen), 0) != 0)
	 	{
			DEBUG(2, p->id, "atomic_remote_dequeue: clear to theft batch, do it and return 1");
			/* maybe i should make a local copy */
			p->activeQ = b;
			return 1;
	 	}
		
		/* dequeue failed, makesure we are not 
		 * at the tail and then restart loop 
		 */
		DEBUG(1, p->id, "atomic_remote_dequeue: batch already stolen");
		if(b->next==NULL || b == v->tail)
		{
			DEBUG(2, p->id, "atomic_remote_dequeue: at last batch in run_queue, return 0");
			return 0;
		}
		b = b->next;
		DEBUG(1, p->id, "atomic_remote_dequeue: get next batch in runqueue");
	}
	DEBUG(0, p->id, "atomic_remote_dequeue: function should never print");
}



int remote_dequeue(logical_processor_t p)
{
	DEBUG(0, p->id, "remote_dequeue: function called");
	int ret; 

	/* declare processor pointer  for victum*/
	logical_processor_t victum;
	victum = selectprocessor(p);
	DEBUG(1, p->id, "remote_dequeue: victum processor selected");

	/* waitfree dequeue from victum */
	ret = atomic_remote_dequeue (p, victum);
	DEBUG(2, p->id, "remote_dequeue: atomic_remote_dequeue called");

	/* if dequeue failed update pointer to next */ 
	/* victum (cannot be current processor) */
	if(ret==0)
	{
		p->partner++; 
		if(p->partner == p->id)
			p->partner++;
		if(p->partner == NUMBER_OF_PROCS) 
			p->partner=0;
		if(p->partner == p->id) /* must recheck this */
			p->partner++;
		DEBUG(1, p->id, "remote_dequeue: partner (victum) pointer updated");
	}

	DEBUG(0, p->id, "remote_dequeue: function complete");
	return ret;
}



void execute(logical_processor_t p)
{
	/*  temp, to show other code is working */
	printf("%d: EXECUTE: executing process %d  \n", 
			(p->id), p->current_process->id);
	/* remove current process from queue */
	p->activeQ->head=p->activeQ->head->next;

	/* decriment dispatch count */
	p->dispatch_count--;
}



void schedule(logical_processor_t p)
{

	DEBUG(4, p->id, "schedule: function called");
	/* if run queue is empty or local dequeue fails run remote dequeue */
	if(p->head!=NULL)
	{
		DEBUG(4, p->id, "schedule: call local_dequeue");
		if(!local_dequeue(p)) 
		{
			DEBUG(4, p->id, "schedule: local_dequeue failed, call remote_dequeue");
			global_procs[p->id]=remote_dequeue(p);
		}
		else
		{
			DEBUG(4, p->id, "schedule: local_dequeue");
			global_procs[p->id]=1;
		}
	}
	else
	{
		DEBUG(4, p->id, "schedule: local runqueue empty, call remote_dequeue");
		global_procs[p->id]=remote_dequeue(p);
	}

	/* only try to execute if you have sucessfully dequeued*/
	if(global_procs[p->id]!=0)
	{
		DEBUG(4, p->id, "schedule: dequeue was sucessful");
		/* set dispatch count */ 
		p->dispatch_count = DISPATCH_COUNT;
		
		while(p->dispatch_count > 0 && p->activeQ->head != NULL)
		{
			DEBUG(4, p->id, "schedule: dispatch_count>0, activeQ not empty");
			/* set current process */
			p->current_process = p->activeQ->head;

			DEBUG(4, p->id, "schedule: executing processes");
			execute(p);
		}
		
		if(p->dispatch_count == 0 && p->activeQ->head != NULL) 
		{
			DEBUG(4, p->id, "schedule: batch not empty,call enqueue");
			enqueue(p); /*all remaining processes are locally enqueued*/
		}
	}
	else
		DEBUG(4, p->id, "schedule: all dequeues failed");
		
	DEBUG(4, p->id, "schedule: function complete");
}


/*
 * selects processor to steal from
 */
logical_processor_t selectprocessor(logical_processor_t p)
{
	DEBUG(4, p->id, "selectprocessor: function called");
	return global_proc_pointer[p->partner];
}

void set_dispatch_count(logical_processor_t p, int count)
{
	DEBUG(4, p->id, "set_dispatch_count: function called");
	p->dispatch_count = count;
}
/* 
 * copy batch by creating new one
 */
batch_t copy_batch(logical_processor_t p, batch_t b)
{
	DEBUG(0, p->id, "copy_batch: function called");
	batch_t temp = malloc(sizeof(struct batch));
	temp->head = b->head;
	temp->tail = b->tail;
	temp->next = b->next;
	temp->window = b->window;
	temp->stolen = b->stolen;

	DEBUG(0, p->id, "copy_batch: function complete");
	return temp;
}
/*
 * pre integration testing
 *
 * this method makes the call out to the scheduler
 */

void * test_run(void * arg)
{
	int i=0;
	int flag=1;
	logical_processor_t p = (logical_processor_t) arg;
	DEBUG(4, p->id, "test_run: args extracted");

	while(1) 
	{
		DEBUG(4, p->id, "test_run: calling schedule");
		schedule(p);

		/* check to see if all processors are done executing */
		flag=1;
		for(i=0;i<NUMBER_OF_PROCS;i++)
			if(global_procs[i]!=0)
			{
				flag=0;
				DEBUG(4, p->id, "test_run: not all processors are out of work");
				break;
			}
		if(flag)		
		{
			DEBUG(4, p->id, "test_run: complete");
			return;
		}
		DEBUG(4, p->id, "test_run: relooping");
	}
}
