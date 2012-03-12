/*

this code requires some things be added to the structs in wf-sched.h

add:
batch_t offset 			to batches // points to window tail, null if not in window
batch_t stolen 			to batches // points to current batch, null if stolen
batch_t window_tail 		to logical processors //points to window tail


*/


static batch_t atomic_swap_bptr(batch_t b1, batch_t b2)
{
	// TODO: atomic magic swapery
	// return result of swap
}



static void atomic_enqueue(logical_processor_t p, batch_t batch)
{
	/* link batch into runqueue */
	p->tail->next = batch;
	p->tail = batch;

	/* load window state word */
	batch_t local_word = p->window_tail;

	/* generate offset (increment tail of migration window) */
	batch_t offset =  local_word->next;

	/* record offset into batch  */
	batch->offset = offset;  

	/* atomically swap batch pointer with offset  */
	/* if not null batch has been nocked out of window  */
	if (atomic_swap_bptr(offset, batch) != null)
	{
		batch->offset = null;
	}

	/* update window state word */
	p->window_tail=offset;

}

static int atomic_local_dequeue (logical_processor_t p)
{
	/* remove head of runqueue  */
	p->activeQ = p->head;
	p->head = p->head->next;

	while((p->activeQ != null) && (atomic_swap_bptr(null, p->activeQ->stolen) == null))	
	{
		/* batch has been stolen already */ 
		/* do laundry some how */
	
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
	/* where we would update a bitmap if we had one */
	return 1; 
}



static int atomic_remote_dequeue (logical_processor_t p, logical_processor_t v)
{
	batch_t stolenB;
	
	/* get pointer to first batch on victum */
	batch_t b=v->head; 
	 
	while(1)
	{
	 	/* go through victum queue and find first batch in the window */
	 	while(b->offset==null)
	 	{
			/* if we get to the last element we are done */
			if(b->next==null)
				return 0;
			b = b->next;
	 	}
	 
	 	/* swap window entry with null  
		 * if result is null batch has already been stolen
		 * else result is the now stolen batch 
		 */
	 	if(stolenB = atomic_swap_bptr(batch->stolen, null) != null)
	 	{
			p->activeQ = stolenB;
			return 1;
	 	}
		
		/* dequeue failed, makesure we are not 
		 * at the tail and then restart loop 
		 */
		if(b->next==null)
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















