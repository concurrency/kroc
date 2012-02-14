/*
library of functions for simple scheduling
*/

#include <stdio.h>
#include <stdlib.h>
#include <wf-sched.h>

/* 
 * 
 * returns 1 if a batch was dequeued, 0 if not
 */ 
int local_dequeue(logical_processor_t p)
{
//	printf("LD %d: called\n",p->id);
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
//			printf("LD %d: HEAD->NEXT\n",p->id);
		}
		else
		{
			/* make sure you unlock before you leave*/
			pthread_mutex_unlock(p->run_queue_lock);
//			printf("LD %d: batch NOT found\n", p->id);
			return 0; /* no more batches left */
		}


		if(inWindow(p->activeQ)) 
		{ /* batch was in window */
			
//			printf("LD %d: IN WINDOW\n",p->id);
			if(!isStolen(p->activeQ))
			{
//				printf("LD %d: REMOVING, NOT STOLEN\n", p->id);
				/* we found a good batch*/
				
				remove_from_window(p->activeQ);
//				printf("LD %d: REMOVED FROM WINDOW\n", p->id);
				extend_window(p->activeQ->next);
//				printf("LD %d: WINDOW EXTENDED\n",p->id);

//				printf("LD %d: batch found\n",p->id);


//			printf("LD %d: UNLOCK MUTEX\n",p->id);
				pthread_mutex_unlock(p->run_queue_lock);
				/************UNLOCK***********/
//			printf("LD %d: ACHIEVEMENT UNLOCKED\n",p->id);
				return 1;

			}
			else /*batch was already stolen */
			{	
				/* garbage collect */
				
				/* p->laundry_queue = p->activeQ; */
//				printf("LD %d: DO LAUNDRY\n",p->id);

//			printf("LD %d: UNLOCK MUTEX\n",p->id);
				pthread_mutex_unlock(p->run_queue_lock);
				/************UNLOCK***********/
//			printf("LD %d: ACHIEVEMENT UNLOCKED\n",p->id);
				return 0;
			}
		}
		else
		{
			/* we found a good batch */
//			printf("LD %d: batch found\n",p->id);


//		printf("LD %d: UNLOCK MUTEX\n",p->id);
			pthread_mutex_unlock(p->run_queue_lock);
			/************UNLOCK***********/
//		printf("LD %d: ACHIEVEMENT UNLOCKED\n",p->id);
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
//	printf("RD %d: called\n", p->id);
	int loop_count =0;
	int process_found = 0;
	int batch_found = 0;

	/* declare processor pointer */
	logical_processor_t victum;
	victum = selectprocessor(p);
//	printf("RD %d: victum selected\n", p->id);

	/**************LOCK***********/
	pthread_mutex_lock(victum->run_queue_lock);

//	printf("RD %d: victum queue locked\n", p->id);
	p->activeQ = dequeue_window_batch(victum);
//	printf("RD %d: batch dequeued\n", p->id);
	if(p->activeQ != NULL)
	{	
		setStolen(p->activeQ);
//		printf("RD %d: batch stolen\n", p->id);

		pthread_mutex_unlock(victum->run_queue_lock);
		/************UNLOCK***********/
		return 1;
	}
	/* make sure to unlock no matter what path is taken */
	pthread_mutex_unlock(victum->run_queue_lock);
	/************UNLOCK***********/
	
//	printf("RD %d: NOTHING stolen\n", p->id);
	/* update pointer to next victum cannot be current processor*/
	p->partner++; 
	if(p->partner == p->id)
		p->partner++;
	if(p->partner == NUMBER_OF_PROCS) 
		p->partner=0;
	if(p->partner == p->id) /* must recheck this */
		p->partner++;
//	printf("RD %d: next partner: %d\n", p->id, p->partner);
	return 0;
}

void set_current_process(logical_processor_t p)
{
	p->current_process = p->activeQ->head;
}

void set_dispatch_count(logical_processor_t p, int count)
{
	p->dispatch_count = count;
}

void execute(logical_processor_t p)
{
	/*  temp, to show other code is working */
	printf("EXECUTE %d: executing process %d  \n", 
			(p->id+1), p->current_process->id);
	/* remove current process from queue */
	p->activeQ->head=p->activeQ->head->next;

	/* decriment dispatch count */
	p->dispatch_count--;
}


void queue_batch(logical_processor_t p)
{
//	printf("QB %d: called\n", p->id);
	/**************LOCK***********/
	pthread_mutex_lock(p->run_queue_lock);

	/* add batch to end of the queue */	
	p->tail->next = p->activeQ;
	p->tail = p->activeQ;
	/* set active and new tails next to null (empty) */
	p->tail->next = NULL;
	p->activeQ=NULL;

	pthread_mutex_unlock(p->run_queue_lock);
	/************UNLOCK***********/

//	printf("QB %d: completed\n", p->id);
	/* may have to add code her to windowify batch */
}

void schedule(logical_processor_t p)
{
//	printf("SCHED %d: in schedule, partner: %d\n", p->id, p->partner);

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
//		printf("SCHED %d: SET DISPATCH COUNT.\n", p->id);
		set_dispatch_count(p, DISPATCH_COUNT);
		
		while(p->dispatch_count > 0 && p->activeQ->head != NULL)
		{
//			printf("SCHED %d: SET_CURRENT_P\n", p->id);
			set_current_process(p);
//			printf("SCHED %d: EXECUTE_P\n", p->id);
			execute(p);
		}
		
		if(p->dispatch_count == 0 && p->activeQ->head != NULL) {
//			printf("SCHED %d: Q_BATCH\n", p->id);
			queue_batch(p); //all batches are locally enqueued
		}
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
 */
logical_processor_t selectprocessor(logical_processor_t p)
{
	return global_proc_pointer[p->partner];
}

/*
 * returns a pointer to the stolen batch or NULL if no batch could be stolen
 */
batch_t dequeue_window_batch(logical_processor_t p)
{
	batch_t b = p->head;		
	
//  printf("RD: DQW V=%d: dequeueing batch from victum\n", p->id);
	if(b!=NULL)/* assume a batch exists to steal*/
	{
//		printf("RD: DQW V=%d: head not null\n", p->id);
		if(b->next == NULL) /* only one batch */
		{
			if(b->window && !b->stolen) /*can be stolen*/
			{
//			printf("RD: DQW V=%d: dequeueing victum head\n", p->id);
				return b;
			}
			else
				return NULL; /*nothing stealable found*/
		}
		else 
		{
		/* as long as batch is not in window or already stolen, 
		   get new batch */
			while(b->next != NULL && (!b->window || b->stolen))
			{
				b = b->next;
				if(b == p->tail && (!b->window || b->stolen))
				{
//					printf("RD: DQW V=%d: no stealable batch found\n",p->id);
					return NULL; /*nothing stealable found*/
				}
			}
		
//			printf("DQW %d: dequeuing non head batch\n", p->id);
			return b;
		}
	}
	
//	printf("RD: DQW V=%d: no stealable batch found\n", p->id);
	return NULL; /*nothing stealable found*/
}


/*
 * add batch b to the window
 */
void extend_window(batch_t b)
{	
	/* if the next batch is already in the window 
	   set b to the next next one */
	if (b != NULL) 
	{
		while(b->next != NULL && b->window == 1)
			b = b->next;
		b->window = 1;
	}
	/* if b is null there are no more batches in the queue
	and this function does nothing */
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


void * test_run(void * arg)
{
	int i=0;
	int flag=1;
	logical_processor_t p = (logical_processor_t) arg;

	while(1) //while true
	{
		
//		printf("Global Array: ");
//		for(i=0;i<NUMBER_OF_PROCS;i++)
//			printf("%d,",global_procs[i]);
//		printf("\n");

		schedule(p);

		/* check to see if all processors are done executing */
		flag=1;
		for(i=0;i<NUMBER_OF_PROCS;i++)
			if(global_procs[i]!=0)
			{
				flag=0;
				break;
			}
		if(flag)//|| ++meh==20)//|| global_procs[0]==0)
		{
			return;
		}
	}
}
