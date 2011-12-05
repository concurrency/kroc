/*
library of functions for simple scheduling
*/
#include <stdio.h>
#include <stdlib.h>
#include <sched.h>


/* 
 * 
 * returns 1 if a batch was dequeued, 0 if not
 */ 
int local_dequeue(logical_processor_t p)
{
//	printf("local_dequeue");
	int usable_batch_found = 0;
	while(!usable_batch_found)
	{
		/* move batch*/	
		p->activeQ = p->head;	
		/* remove from run queue */
		p->head = p->head->next;

		if(inWindow(p->activeQ)) 
		{ /* batch was in window */

			if(!isStolen(p->activeQ))
			{
				/* we found a good batch
				set flag to exit loop */
				usable_batch_found = 1;

				remove_from_window(p->activeQ);
				extend_window(p->activeQ->next);

				return 1;

			}
			else /*batch was already stolen */
			{	
				/* garbage collect */
				/* p->laundry_queue = p->activeQ; */
			}
		}
		else
		{
			/* we found a good batch
			set flag to exit loop */
			usable_batch_found = 1;
			return 1;
		}
	}
}

/* 
 * 
 * returns 1 if a batch was dequeued, 0 if not
 */ 
int remote_dequeue(logical_processor_t p)
{
	int loop_count =0;
	int processor_found = 0;
	int batch_found = 0;
	// declare processor pointer
	logical_processor_t victum;
	while(!processor_found)
	{
		victum = selectprocessor(p, loop_count);

		while((p->activeQ = dequeue_window_batch(victum))!=0 && !batch_found)
		{
			//p->activeQ = dequeue_window_batch(victum);
			
			if(!isStolen(p->activeQ))
			{
				batch_found = 1;
				processor_found = 1;
			}
		
		}
		/* arbitrary counter to ensure hault of loop */
		if(loop_count++ == 4)
			processor_found = 1;

	}
	/* batch was stolen */
	if( batch_found == processor_found)
	{
		setStolen(p->activeQ);
		return 1;
	}
	return 0;
}

void set_current_process(logical_processor_t p)
{
//	printf("set_current_p\n");
	p->current_process = p->activeQ->head;
}

void set_dispatch_count(logical_processor_t p, int count)
{
	p->dispatch_count = count;
}

void execute(logical_processor_t p)
{
	/*  temp, to show other code is working */
//	printf("executing process %d\n", p->current_process->id);
	/* remove current process from queue */
	p->activeQ->head=p->activeQ->head->next;


	/* decriment dispatch count */
	p->dispatch_count--;
}

void queue_batch(logical_processor_t p)
{
	/*	printf("q_batch\n"); */
	/* add batch to end of the queue */	
	p->tail->next = p->activeQ;
	p->tail = p->activeQ;
	/* set active and new tails next to null (empty) */
	p->tail->next = NULL;
	p->activeQ=NULL;

	/* may have to add code her to windowify batch */

}



void schedule(logical_processor_t p)
{
	while(1) //while true
	{
		if(p->head!=NULL)
			local_dequeue(p); // local dequeue
		else 
			remote_dequeue(p);
		set_dispatch_count(p, 1);
		while(p->dispatch_count > 0 && p->activeQ->head != NULL)
		{
			set_current_process(p);
			execute(p);
		}
		if(p->dispatch_count == 0 && p->activeQ->head != NULL)
			queue_batch(p); //all batches are locally enqueued
	}		
}



/* main method to test code */
void main()
{
	/* create some processes/batches/a logical processor*/
	process_t p1 = malloc(sizeof(struct process)); 
	process_t p2 = malloc(sizeof(struct process)); 
	process_t p3 = malloc(sizeof(struct process)); 
	batch_t b1 = malloc(sizeof(struct batch)); 
	batch_t b2 = malloc(sizeof(struct batch)); 
	logical_processor_t lp1 = malloc(sizeof(struct logical_processor)); 
	
	p1->id = 5;
	p2->id = 6;
	p3->id = 7;
	p3->next = NULL;
	p1->next = p2;
	p2->next = NULL;
	b1->next = b2;
	b2->next = NULL;
	b1->head = p1;
	b2->head = p3;
	lp1->tail = b2;
	lp1->head = b1;

	schedule(lp1);

}


/**************************************
 * some helper scheduling functions,  *
 * most low level interaction is here *
 **************************************/

/*
 * returns 1 if batch is in the window
 */
int inWindow(batch_t b)
{
	return b->window;
}


/*
 * returns 1 if batch has beenstolen
 */
int isStolen(batch_t b)
{
	return b->stolen;
}


/*
 * selects processor to steal from
 * 
 * if this is the first time it is called simply return
 * pointer to that processes partner
 * if not, the original partner cannot be stolen from, get a new
 * partner (your partners partner)
 */
logical_processor_t selectprocessor(logical_processor_t p, int newPartner)
{
	/* if new > 0 it is not the first time this method 
	   was called, so we update the partner and move it one
		 on the ring of processors */
	if(newPartner > 0)	
		p->partner = p->partner->partner;	
	
	return p->partner;
}

/*
 * returns a pointer to the stolen batch or 0 if no batch could be stolen
 */
batch_t dequeue_window_batch(logical_processor_t p)
{
	batch_t b = p->head;		
	
	/* as long as batch is not in window or already stolen, get new batch */
	while(!b->window || b->stolen)
	{
		b = b->next;
		if(b == p->tail && (!b->window || b->stolen))
			return 0; //nothing stealable found
	}
	return b;
}


/*
 * add batch b to the window
 */
void extend_window(batch_t b)
{	
	/* if the next batch is already in the window 
	   set b to the next next one */
	while(b->window == 1)
		b = b->next;
	b->window = 1;
}

/*
 * remove batch b from the window
 */
void remove_from_window(batch_t b)
{
	b->window = 0;
}

/*
 * mark stolen batch
 */
void setStolen(batch_t b)
{
	b->stolen=1;
}




