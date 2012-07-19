#include <stddef.h>
#include <ctype.h>
#include <stdio.h>
#include <paging.h>
#include <interrupt.h>

typedef unsigned long off_t;
void halt(void);

int getpagesize(void);
void init_heap(void);

int getpagesize()
{
	return PAGESIZE;
}

/* Source:  http://www.tekpool.com/node/176 */
double atof(const char *s)
{
	double num = 0;
	int radix = 1;
	bool dec = 1;
	int neg = 1;

	while(1)
	{
		switch (*s)
		{
			case '-':
				neg = -neg;
			case '+':
			case ' ':
			case '\t':
				s++;
				continue;
			default:
				break;
		}
		break;
	}

	while (*s)
	{   
		if (isdigit(*s))
		{   
			num = 10.0 * num + (*s - '0');
			if (!dec)
			{   
				radix *= 10; 
			}   
		}   
		else
		{   
			dec = 0;
		}   
		s++;
	}   
	return (num/radix) * neg;
}

/* mmap() called by:
 *   readLUT()
 *   writeLUT()
 *   getCOREID()
 * Always used to map into the file descriptor fd for the non-cacheable memory 
 * device provided by SCC Linux.  The offset is already a page-aligned 
 * address, so we just need to return it. */
void *mmap(void *addr, size_t len, int prot, int flags, int fd, off_t off)
{
	return (void *)off;
}

/* munmap() called by:
 *   readLUT()
 *   writeLUT()
 *   getCOREID()
 * mmap does no actual virtual memory mapping, so munmap has no 
 * responsibilities. */
int munmap(void *addr, size_t length)
{
	return 0;
}

/* Report status and halt the core. */
void exit(int status)
{
	printf("EXIT with status %d\n", status);
	halt();
}

/* perror() -- just print out the error */
void perror(const char *s)
{
	printf("ERROR: %s\n", s);
}

/* open() called by:
 *   readLUT()
 *   writeLUT()
 *   getCOREID()
 * Always used to access non-cacheable memory file from SCC Linux. The file 
 * descriptor we return gets passed to mmap, where we ignore it anyway. So 
 * must simply return a non-error value. */
int open(const char *pathname, int flags)
{
	return 0;
}

/* We don't deal with streams. Just return non-error value. */
int fflush(void *stream)
{
	return 0;
}

/* Memory management functions (malloc and free) needed to get space for 
 * bookkeeping structures that manage MPB space and SHM space.  So set up a 
 * heap in private memory and manage it.  We follow the memory management 
 * model of Embedded Xinu. */

struct memblock
{
	struct memblock *next;
	unsigned int length;
};

#define MBSZ (sizeof(struct memblock))
/* round address up to memblock alignment */
#define roundmb(x)  ((void *)(((MBSZ-1) + (ulong)(x)) & ~(MBSZ-1)))

static struct memblock memlist; /* free memory list */
void *memheap; /* bottom of heap, set by startup.S */
extern void _start(void); /* start of compiled image */

void init_heap()
{
	struct memblock *heap;

	/* round memheap up to memblock alignment */
	heap = (struct memblock *)roundmb(memheap);

	memlist.next = heap;
	memlist.length = (ulong)_start - (ulong)memheap;
	heap->next = NULL;
	heap->length = memlist.length;
}

/* malloc() called by:
 *   RCCE_shmalloc_init()
 *   RCCE_shmalloc()
 *   RCCE_malloc_init()
 *   RCCE_malloc()
 *   RCCE_flag_alloc() */
void *malloc(size_t nbytes)
{
    irqmask im; 
    struct memblock *prev, *curr, *leftover;

    /* we don't allocate 0 bytes. */
    if (0 == nbytes)
    {   
        return NULL;
    }   

    /* round request to size of memblock */
    nbytes = (uint)roundmb(nbytes);

    /* make room for accounting information */
    nbytes += sizeof(struct memblock);

    im = disable();

    prev = &memlist;
    curr = memlist.next;
    while (curr != NULL)
    {   
        if (curr->length == nbytes)
        {   
			/* perfect size */
            prev->next = curr->next;
            memlist.length -= nbytes;

            break;
        }
        else if (curr->length > nbytes)
        {
            /* split block into two */
            leftover = (struct memblock *)((ulong)curr + nbytes);
            prev->next = leftover;
            leftover->next = curr->next;
            leftover->length = curr->length - nbytes;
            memlist.length -= nbytes;

            break;
        }
        prev = curr;
        curr = curr->next;
    }

	if (curr == NULL)
	{
		restore(im);
		return NULL;
	}

	curr->next = curr;
	curr->length = nbytes;

	restore(im);
	return (void *)(curr + 1); /* +1 to skip accounting info */
}

/* free() called by:
 *   RCCE_free()
 *   RCCE_shfree()
 *   RCCE_flag_free() */
void free(void *ptr)
{
	irqmask im;
	struct memblock *block, *next, *prev;
	ulong top, nbytes;

	/* block points to the memblock we want to free */
	block = (struct memblock *)ptr;

	/* back up to accounting information */
	block--;

	nbytes = block->length;

	/* sanity checks */
	if (block->next != block
		|| (ulong)block < (ulong)memheap)
	{
		return;
	}

	im = disable();

	prev = &memlist;
	next = memlist.next;
	while ((next != NULL) && (next < block))
	{
		prev = next;
		next = next->next;
	}

	/* find top of previous memblock */
	if (prev == &memlist)
	{
		top = NULL;
	}
	else
	{
		top = (ulong)prev + prev->length;
	}

	/* make sure block is not overlapping on prev or next blocks */
	if ((top > (ulong)block)
		|| ((next != NULL) && ((ulong)block + nbytes) > (ulong)next))
	{
		restore(im);
		return;
	}

	memlist.length += nbytes;

	/* coalesce with previous block if adjacent */
	if (top == (ulong)block)
	{
		prev->length += nbytes;
		block = prev;
	}
	else
	{
		block->next = next;
		prev->next = block;
	}

	/* coalesce with next block if adjacent */
	if (((ulong)block + block->length) == (ulong)next)
	{
		block->length += next->length;
		block->next = next->next;
	}

	restore(im);
	return;
}
