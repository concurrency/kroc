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
	while(1)
	{
		/**************LOCK***********/
		pthread_mutex_lock(p->run_queue_lock);

		if(p->head !=NULL)
		{
			/* move batch*/	
			p->activeQ = p->head;	
			/* remove from run queue */
			p->head = p->head->next;
		}
		else
			return 0; /* no more batches left */

		pthread_mutex_unlock(p->run_queue_lock);
		/************UNLOCK***********/

		if(inWindow(p->activeQ)) 
		{ /* batch was in window */

			if(!isStolen(p->activeQ))
			{
				/* we found a good batch*/

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
			/* we found a good batch */
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
	while(1)
	{
		victum = selectprocessor(p, loop_count);

		/**************LOCK***********/
		pthread_mutex_lock(victum->run_queue_lock);

		while((p->activeQ = dequeue_window_batch(victum))!=0 && !batch_found)
		{
			//p->activeQ = dequeue_window_batch(victum);
			
			if(!isStolen(p->activeQ))
			{
				batch_found = 1;
			}
		}

	   /* batch was stolen */
		if(batch_found)
		{
			setStolen(p->activeQ);
			return 1;
		}

		pthread_mutex_unlock(victum->run_queue_lock);
		/************UNLOCK***********/

		/* arbitrary counter to ensure hault of loop */
		/* 4 or number of virtual processors */
		if(loop_count++ == 4)
			return 0;
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
	/**************LOCK***********/
	pthread_mutex_lock(victum->run_queue_lock);

	/* add batch to end of the queue */	
	p->tail->next = p->activeQ;
	p->tail = p->activeQ;
	/* set active and new tails next to null (empty) */
	p->tail->next = NULL;
	p->activeQ=NULL;

	pthread_mutex_unlock(victum->run_queue_lock);
	/************UNLOCK***********/

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
	while(b->next != NULL && (!b->window || b->stolen))
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
	while(b->next != NULL && b->window == 1)
	{
		b = b->next;
	}
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


/*****************************************
** 
** TTTTTTT   EEEE    SSS   TTTTTTT 
**    T      E      S         T
**    T      EEEE    SSS      T
**    T      E          S     T
**    T      EEEE    SSS      T
**
******************************************/

/* code to set up a virtual processor 
   with everything it needs to test */

void setup_proc(logical_processor_t p)
{
	// create some processes/batches/a logical processor
	process_t p1 = malloc(sizeof(struct process)); 
	process_t p2 = malloc(sizeof(struct process)); 
	process_t p3 = malloc(sizeof(struct process)); 
	batch_t b1 = malloc(sizeof(struct batch)); 
	batch_t b2 = malloc(sizeof(struct batch)); 
	
	p1->id = 5;
	p2->id = 6;
	p3->id = 7;
	p3->next = NULL;
	p1->next = p2;
	p2->next = NULL;
	b1->next = b2;
	b2->next = NULL;
	b2->stolen = 0;
	b2->window = 0;
	b1->head = p1;
	b2->head = p3;
	b1->stolen = 0;
	b1->window = 1;
	p->tail = b2;
	p->head = b1;

}

int main () 
{

  /* Thread pointers */
  pthread_t t1, t2, t3, t4;

  /* Create 4 Virtual Processors */
	logical_processor_t p1 = malloc(sizeof(struct logical_processor)); 
	logical_processor_t p2 = malloc(sizeof(struct logical_processor)); 
	logical_processor_t p3 = malloc(sizeof(struct logical_processor)); 
	logical_processor_t p4 = malloc(sizeof(struct logical_processor)); 


  /* Return values from threads */
  int ret1, ret2, ret3, ret4;
	
	/* initalize a bunch of processes and batces */
	setup_proc(p1);
	setup_proc(p2);
	setup_proc(p3);
	setup_proc(p4);
	/* Initialize the lock. Could do this to an array of locks
	   or similar... but we do need to init each one.
		 This could be in a struct (eg. in the virtual processor
		 or runqueue.
  */
	 pthread_mutex_init(p1->run_queue_lock, NULL);
	 pthread_mutex_init(p2->run_queue_lock, NULL);
	 pthread_mutex_init(p3->run_queue_lock, NULL);
	 pthread_mutex_init(p4->run_queue_lock, NULL);

 /* begin the scheduler, one thread per processor */ 
  ret3 = pthread_create ( &t1, NULL, schedule, (void*) p1);
  ret3 = pthread_create ( &t2, NULL, schedule, (void*) p2);
  ret3 = pthread_create ( &t3, NULL, schedule, (void*) p3);
  ret3 = pthread_create ( &t4, NULL, schedule, (void*) p4);
  
  /* Wait for all of the threads to finish */
  pthread_join (t1, NULL);
  pthread_join (t2, NULL);
  pthread_join (t3, NULL);
  pthread_join (t4, NULL);

  printf ("Threads done.\n");
  
  return 0;
}

