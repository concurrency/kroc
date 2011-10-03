/*
library of functions for simple scheduling
*/
#include <stdio.h>
#include <stdlib.h>
#include <sched.h>


void set_active_queue(logical_processor_t p)
{
		printf("set_active_queue\n");
		/* move batch*/	
		p->activeQ = p->head;	
		/* remove from run queue */
		p->head = p->head->next;
}

void set_current_process(logical_processor_t p)
{
		printf("set_current_p\n");
		p->current_process = p->activeQ->head;
}

void set_dispatch_count(logical_processor_t p, int count)
{
		p->dispatch_count = count;
}

void execute(logical_processor_t p)
{
		/*  temp, to show other code is working */
		printf("executing process %d\n", p->current_process->id);
		/* remove current process from queue */
		p->activeQ->head=p->activeQ->head->next;


		/* decriment dispatch count */
		p->dispatch_count--;
}

void queue_batch(logical_processor_t p)
{
		printf("q_batch\n");
		/* add batch to end of the queue */	
		p->tail->next = p->activeQ;
		p->tail = p->activeQ;
		/* set active and new tails next to null (empty), maybe not needed */
		p->tail->next = NULL;
		p->activeQ=NULL;
}

void schedule(logical_processor_t p)
{
	while(p->head!=NULL)
		{
				set_active_queue(p);
				set_dispatch_count(p, 1);
				while(p->dispatch_count > 0 && p->activeQ->head != NULL)
				{
						set_current_process(p);
						execute(p);
				}
				if(p->dispatch_count == 0 && p->activeQ->head != NULL)
						queue_batch(p);
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


