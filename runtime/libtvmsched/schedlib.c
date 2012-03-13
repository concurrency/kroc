/*
library of functions for simple scheduling
*/

#include <stdio.h>
#include <stdlib.h>
#include <wf-sched.h>

static int atomic_swap(logical_processor_t p, int* a, int b)
{
	/* TODO: atomic magic swaper */
	/* for now we just you mutex lock for testing */

	/* LOCK */
	pthread_mutex_lock(p->run_queue_lock);

	if(*a == b)
		return b;
	else
		*a=b;
	return 1;

	/* UNLOCK */
	pthread_mutex_unlock(p->run_queue_lock);
}



static void enqueue(logical_processor_t p)
{
	batch_t batch = p->activeQ;
	/* link batch into runqueue */
	p->tail->next = batch;
	p->tail = batch;

	/* the current window size is less the max, batch to window */

	if(p->window_size > MAX_WINDOW_SIZE)
	{
		p->window_size++;
		batch->window = 1;
	}
}

static int local_dequeue (logical_processor_t p)
{
	/* remove head of runqueue  */
	p->activeQ = p->head;
	p->head = p->head->next;

	/* check runqueue is not empty */
	/* should not be necessary */
	if(p->activeQ == NULL)
		return 0;

	while((p->activeQ != NULL) && (atomic_swap(p, &(p->activeQ->stolen), 0) == 0))	
	{

		/* batch has been stolen already */ 
		/* do laundry */
		p->window_size--;
	
		/* if active queue is now tail we have gone through 
		 * the runqueue and found nothing, return with failure */
		if(p->activeQ==p->tail)
		{
			return 0;
		}
		
		/* get next batch in queue */		
		p->activeQ = p->head;
	 	p->head = p->head->next;
	}

	/* batch was dequeued */
	/* if batch was in window update window size */
	if(p->activeQ->window == 1)
		p->window_size--;

	return 1; 
}

static int atomic_remote_dequeue (logical_processor_t p, logical_processor_t v)
{
	/* get pointer to first batch on victum */
	batch_t b=v->head; 
	 
	while(1)
	{
	 	/* go through victum queue and find first batch in the window */
	 	while(b->window==0)
	 	{
			/* if we get to the last element we are done */
			if(b->next==NULL)
				return 0;
			b = b->next;
	 	}
	 
	 	/* swap window entry with null  
		 * if result is null batch has already been stolen
		 * else steal batch 
		 */
	 	if(atomic_swap(v, &(b->stolen), 0) != 0)
	 	{
			p->activeQ = b;
			return 1;
	 	}
		
		/* dequeue failed, makesure we are not 
		 * at the tail and then restart loop 
		 */
		if(b->next==NULL)
			return 0;
		b = b->next;
	}
}



static int remote_dequeue(logical_processor_t p)
{
	int ret; 

	/* declare processor pointer  for victum*/
	logical_processor_t victum;
	victum = selectprocessor(p);

	/* waitfree dequeue from victum */
	ret = atomic_remote_dequeue (p, victum);

	/* update pointer to next victum cannot be current processor*/
	p->partner++; 
	if(p->partner == p->id)
		p->partner++;
	if(p->partner == NUMBER_OF_PROCS) 
		p->partner=0;
	if(p->partner == p->id) /* must recheck this */
		p->partner++;

	return ret;
}



static void execute(logical_processor_t p)
{
	/*  temp, to show other code is working */
	printf("EXECUTE %d: executing process %d  \n", 
			(p->id+1), p->current_process->id);
	/* remove current process from queue */
	p->activeQ->head=p->activeQ->head->next;

	/* decriment dispatch count */
	p->dispatch_count--;
}



static void schedule(logical_processor_t p)
{

	/* if run queue is empty or local dequeue fails run remote dequeue */
	if(p->head!=NULL)
	{
		if(!local_dequeue(p)) 
			global_procs[p->id]=remote_dequeue(p);
		else
			global_procs[p->id]=1;
	}
	else
		global_procs[p->id]=remote_dequeue(p);

	/* only try to execute if you have sucessfully dequeued*/
	if(global_procs[p->id]!=0)
	{
		set_dispatch_count(p, DISPATCH_COUNT);
		
		while(p->dispatch_count > 0 && p->activeQ->head != NULL)
		{
			set_current_process(p);
			execute(p);
		}
		
		if(p->dispatch_count == 0 && p->activeQ->head != NULL) {
			enqueue(p); /*all batches are locally enqueued*/
		}
	}
}


/*
 * selects processor to steal from
 */
static logical_processor_t selectprocessor(logical_processor_t p)
{
	return global_proc_pointer[p->partner];
}

static void * test_run(void * arg)
{
	int i=0;
	int flag=1;
	logical_processor_t p = (logical_processor_t) arg;

	while(1) 
	{
		

		schedule(p);

		/* check to see if all processors are done executing */
		flag=1;
		for(i=0;i<NUMBER_OF_PROCS;i++)
			if(global_procs[i]!=0)
			{
				flag=0;
				break;
			}
		if(flag)		{
			return;
		}
	}
}
