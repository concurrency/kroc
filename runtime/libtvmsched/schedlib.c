/*
library of functions for simple scheduling
*/

#include <stdio.h>
#include <stdlib.h>
#include <wf-sched.h>

/* global list of processor states */
/* contains a 0 if processor currently has no work */
int global_procs[NUMBER_OF_PROCS];

/* global list of pointers to processors*/
logical_processor_t global_proc_pointer[NUMBER_OF_PROCS];

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

//		printf("LD %d: UNLOCK MUTEX\n",p->id);
		pthread_mutex_unlock(p->run_queue_lock);
		/************UNLOCK***********/
//		printf("LD %d: ACHIEVEMENT UNLOCKED\n",p->id);

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
				return 1;

			}
			else /*batch was already stolen */
			{	
				/* garbage collect */
				/* p->laundry_queue = p->activeQ; */
//				printf("LD %d: DO LAUNDRY\n",p->id);
			}
		}
		else
		{
			/* we found a good batch */
//			printf("LD %d: batch found\n",p->id);
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

	// declare processor pointer
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
			p->id, p->current_process->id);
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
	if(p->head!=NULL) 
	{
		local_dequeue(p); // local dequeue
		/* ensure every one knows you have work */
		global_procs[p->id]=1; //shouldn't be needed
	}
	else  
		global_procs[p->id]=remote_dequeue(p);
	
	/* only try to execute stuff if you have batches to execute */
	if(global_procs[p->id]!=0)
	{
//		printf("SCHED %d: SET DISPATCH COUNT.\n", p->id);
		set_dispatch_count(p, 3);
		
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

		/* if first batch can be stolen */
		if(b->next == NULL && (b->window && !b->stolen))
		{
//			printf("RD: DQW V=%d: dequeueing victum head\n", p->id);
			return b;
		}
		else if(b->next == NULL)
		{
		/* as long as batch is not in window or already stolen, 
		   get new batch */
			while(b->next != NULL && (!b->window || b->stolen))
			{
				b = b->next;
				if(b == p->tail && (!b->window || b->stolen))
				{
//					printf("RD: DQW V=%d: no stealable batch found\n",p->id);
					return NULL; //nothing stealable found
				}
			}
		
//			printf("DQW %d: dequeuing non head batch\n", p->id);
			return b;
		}
	}
	
//	printf("RD: DQW V=%d: no stealable batch found\n", p->id);
	return NULL; //nothing stealable found
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


/*****************************************
** 
** TTTTTTT   EEEE    SSS   TTTTTTT 
**    T      E      S         T
**    T      EEEE    SSS      T
**    T      E          S     T
**    T      EEEE    SSS      T
**
******************************************/

/* temporary code to set up globabl processor list */
void init_global_proc(){
	int i=0;
	for(i=0;i<NUMBER_OF_PROCS;i++)
		global_procs[i]=1;
}

/* code to set up a virtual processor 
   with everything it needs to test */

void setup_proc(logical_processor_t p, int label)
{
	// create some processes/batches/a logical processor
	process_t p1 = malloc(sizeof(struct process)); 
	process_t p2 = malloc(sizeof(struct process)); 
	process_t p3 = malloc(sizeof(struct process)); 
	batch_t b1 = malloc(sizeof(struct batch)); 
	batch_t b2 = malloc(sizeof(struct batch)); 
	
	p1->id = label;
	p2->id = label + 1;
	p3->id = label + 2;
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
  int err;
  
  /* Create 4 Virtual Processors */
	logical_processor_t p1 = malloc(sizeof(struct logical_processor)); 
	logical_processor_t p2 = malloc(sizeof(struct logical_processor)); 
	logical_processor_t p3 = malloc(sizeof(struct logical_processor)); 
	logical_processor_t p4 = malloc(sizeof(struct logical_processor)); 

	/* Return values from threads */
	int ret1, ret2, ret3, ret4;
	
	/* initalize a bunch of processes and batces */
	setup_proc(p1, 0);
	setup_proc(p2, 10);
	setup_proc(p3, 20);
	setup_proc(p4, 30);

	/* link processors for stealing */
	p1->partner = 1;
	p2->partner = 2;
	p3->partner = 3;
	p4->partner = 0;

	/* set up processor ids */
	p1->id = 0;
	p2->id = 1;
	p3->id = 2;
	p4->id = 3;

	/* set up global processor*/
	global_proc_pointer[0]=p1;
	global_proc_pointer[1]=p2;
	global_proc_pointer[2]=p3;
	global_proc_pointer[3]=p4;

	/* set up list of processors, 
	assumes all processors start with work */
	init_global_proc();

	/* Initialize the lock. Could do this to an array of locks
	   or similar... but we do need to init each one.
		 This could be in a struct (eg. in the virtual processor
		 or runqueue.
  */
	 /* this code segfaults 
	   
	 pthread_mutex_init(p1->run_queue_lock, NULL);
	 pthread_mutex_init(p2->run_queue_lock, NULL);
	 pthread_mutex_init(p3->run_queue_lock, NULL);
	 pthread_mutex_init(p4->run_queue_lock, NULL);

	  so i try this way
	 */

	 pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;	
	 p1->run_queue_lock = &m1;
	 pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;	
	 p2->run_queue_lock = &m2;
	 pthread_mutex_t m3 = PTHREAD_MUTEX_INITIALIZER;	
	 p3->run_queue_lock = &m3;
	 pthread_mutex_t m4 = PTHREAD_MUTEX_INITIALIZER;	
	 p4->run_queue_lock = &m4;


 /* begin the scheduler, one thread per processor */
 /*
  ret1 = pthread_create (&t1, NULL, (void *) &schedule, (void *) &p1);
  ret2 = pthread_create (&t2, NULL, (void *) &schedule, (void *) &p2);
  ret3 = pthread_create (&t3, NULL, (void *) &schedule, (void *) &p3);
  ret4 = pthread_create ( &t4, NULL, (void *) &schedule, (void *) &p4);
*/
  
  ret1 = pthread_create (&t1, NULL, test_run, p1);
  ret2 = pthread_create (&t2, NULL, test_run, p2);
  ret3 = pthread_create (&t3, NULL, test_run, p3);
  ret4 = pthread_create (&t4, NULL, test_run, p4);

  printf ("pthread_create called.\n");

  /* Wait for all of the threads to finish */
  printf("JOINING\n");
  err = pthread_join (t1, NULL);
 pthread_join (t2, NULL);
 pthread_join (t3, NULL);
 pthread_join (t4, NULL);
 printf("DONE JOINING\n");
    
  return 0;
}

