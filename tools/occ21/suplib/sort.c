/* $Id: sort.c,v 1.1 1996/04/15 10:54:03 djb1 Exp $ */

/*
 *	sub_qsort() implementation
 *	Copyright (C) 1993 Inmos Limited
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
 *  sup_qsort() is compatible with the ANSI standard qsort(), except that
 *              it has the same effect on all hosts.
 *              (The ANSI standard does not define whether qsort() does
 *              or does not reorder equal keys, so different implementations
 *              on different hosts may or may not reorder equal keys.
 *              It is not defined whether sup_qsort() does or does not
 *              reorder equal keys either, but it is guaranteed that its
 *              behaviour is the same on all supported hosts.)
 */

#include <stddef.h>
#include <stdlib.h>

#include "suplib.h"
/* Qsort is implemented using an explicit stack rather than C recursion  */
/* See Sedgewick (Algorithms, Addison Wesley, 1983) for discussion.      */

#define STACKSIZE 32
#define SUBDIVISION_LIMIT 10

#define MASK (sizeof(int)-1)

#define exchange(p1, p2, size)                      \
{                                                   \
    int xsize = size;                               \
    if (((xsize | (int) p1) & MASK)==0)             \
    {   do                                          \
        {   int temp = *(int *)p1;                  \
            *(int *)p1 = *(int *)p2;                \
            *(int *)p2 = temp;                      \
            p1 += sizeof(int);                      \
            p2 += sizeof(int);                      \
            xsize -= sizeof(int);                   \
        } while (xsize != 0);                       \
    }                                               \
    else                                            \
    {   do                                          \
        {   char temp = *p1;                        \
            *p1++ = *p2;                            \
            *p2++ = temp;                           \
            xsize--;                                \
        } while (xsize != 0);                       \
    }                                               \
    p1 -= size;                                     \
    p2 -= size;                                     \
}

#define stack(p, n)                                 \
    (basestack[stackpointer] = (void *) (p),        \
     sizestack[stackpointer++] = (n))



static void partition_sort(void *base, size_t nmemb, size_t size,
                           int (*compar)(const void *, const void *))
{
    int stackpointer = 1;
    void *basestack[STACKSIZE];
    int sizestack[STACKSIZE];
/* The explicit stack that is used here is large enough for any array    */
/* that could exist on this computer - since I stack sub-intervals in a  */
/* careful order I can guarantee that the depth of stack needed is       */
/* bounded by log2(nmemb), so I can certainly handle arrays of up to     */
/* size 2**STACKSIZE, which is bigger than my address space.             */
/* I require that SUBDIVISION_LIMIT >= 2 so that all segments to be      */
/* partitioned have at least 3 items in them. This means that the first, */
/* middle and last items in a segment are distinct (although they may,   */
/* of course, have the same value).                                      */
    basestack[0] = base;
    sizestack[0] = nmemb;
    while (stackpointer!=0)
    {   char *b = (char *) basestack[--stackpointer];
        int n = sizestack[stackpointer];
/* The for loop here marks where we re-enter the code having refined an  */
/* interval.                                                             */
        for (;;)
        {   int halfn = (n >> 1) * size;  /* index of middle in the list */
            char *bmid = b + halfn;
            char *btop;
            if ((n & 1) == 0) halfn -= size;
            btop = bmid + halfn;
/* Sort first, middle and last items in the segment into order. Put the  */
/* smallest and largest at the start and end of the segment (where they  */
/* act as sentinel values in a conveniant way) and use the median of 3   */
/* as my pivot. This happens to ensure that sorted and inverse sorted    */
/* input gets pivoted neatly and costs n*log n operations, even though   */
/* there are STILL worst cases that take about n*n operations.           */
            if ((*compar)(b, bmid) > 0) exchange(b, bmid, size);
            if ((*compar)(bmid, btop) > 0)
            {   if ((*compar)(b, btop) > 0) exchange(b, btop, size);
                exchange(bmid, btop, size);
            }
/* Now I have the first and last elements in place & a useful pivot.     */
            {   char *l;
                char *r = btop - size;
                char *pivot;
                int s1, s2;
                exchange(bmid, r, size);
                l = b;
                pivot = r;
/* Sweep inwards partitioning elements on the basis of the pivot         */
                for (;;)
                {   do l += size; while ((*compar)(l, pivot) < 0);
                    do r -= size; while ((*compar)(r, pivot) > 0);
                    if (r <= l) break;
                    exchange(l, r, size);
                }
/* Move the pivot value to the middle of the array where it wants to be. */
                exchange(l, pivot, size);
/* s1 and s2 get the sizes of the sublists that I have partitioned my    */
/* data into. Note that s1 + s2 = n - 1 since the pivot element is now   */
/* in its correct place.                                                 */
                s1 = ((l - b)/ size) - 1;
                s2 = n - s1 - 1;
/* I am not going to try partitioning things that are smaller than some  */
/* fairly arbitrary small size (SUBDIVISION_LIMIT), and (only) if I will */
/* have to subdivide BOTH remaining intervals I push the larger of them  */
/* onto a stack for later consideration.                                 */
                if (s1 > s2)
/* Here the segment (b, s1) is the larger....                            */
                {   if (s2 > SUBDIVISION_LIMIT)
/* If the smaller segment needs subdividing then a fortiori the larger   */
/* one does, and needs stacking.                                         */
                    {   stack(b, s1);
                        b = l;
                        n = s2;
                    }
/* If just the larger segment needs dividing more I can loop.            */
                    else if (s1 > SUBDIVISION_LIMIT) n = s1;
/* If both segments are now small I break out of the loop.               */
                    else break;
                }
                else
/* Similar considerations apply if (l, s2) is the larger segment.        */
                {   if (s1 > SUBDIVISION_LIMIT)
                    {   stack(l, s2);
                        n = s1;
                    }
                    else if (s2 > SUBDIVISION_LIMIT)
                    {   b = l;
                        n = s2;
                    }
                    else break;
                }
            }
        }
    }
}

void sup_qsort(void *base, size_t nmemb, size_t size,
               int (*compar)(const void *, const void *))
{
/* Sort an array (base) with nmemb items each of size bytes.             */
/* This uses a quicksort method, but one that is arranged to complete in */
/* about n*log(n) steps for sorted and inverse sorted inputs.            */
    char *b, *endp;
    if (size == 0) return;
    if (nmemb > SUBDIVISION_LIMIT)
        partition_sort(base, nmemb, size, compar);
/* Now I do an insertion sort on the array that is left over. This makes */
/* sense because the quicksort activity above has arranged that the      */
/* array is nearly sorted - at most segments of size SUBDIVISION_LIMIT   */
/* contain locally unsorted segments. In this case insertion sort goes   */
/* as fast as anything else. Doing things this way avoids the need for   */
/* a certain amount of partitioning/recursing overhead in the main body  */
/* of the quicksort procedure.                                           */
/* Note: some sorts of bugs in the above quicksort could be compensated  */
/* for here, leaving this entire procedure as just an insertion sort.    */
/* This would yield correct results but would be somewhat slow.          */
    b = (char *) base;
    endp = b + (nmemb-1) * size;
    while (b < endp)
    {   char *b1 = b;
        b += size;
/* Find out where to insert this item.                                   */
        while (b1>=(char *)base && (*compar)(b1, b)>0) b1 -= size;
        b1 += size;
/* The fact that I do not know how large the objects that are being      */
/* sorted are is horrible - I do an exchange here copying things one     */
/* word at a time or one byte at a time depending on the value of size.  */
        if (((size | (int) b) & MASK)==0)
        {   int j;
            for (j=0; j<size; j+=sizeof(int))
            {   char *bx = b + j;
/* Even when moving word by word I use (char *) pointers so as to avoid  */
/* muddles about pointer arithmetic. I cast to (int *) pointers when I   */
/* am about to dereference something.                                    */
                int save = *(int *)bx;
                char *by = bx - size;
                while (by>=b1)
                {   *(int *)(by + size) = *(int *)by;
                    by -= size;
                }
                *(int *)(by + size) = save;
            }
        }
        else
/* If size is not a multiple of wordsize I behave with great caution and copy */
/* byte by byte. I could, of course, copy by words up to the last 1, 2   */
/* or 3 bytes - I count that as too much effort for now.                 */
        {   int j;
            for (j=0; j<size; j++)
            {   char *bx = b + j;
                char save = *bx;
                char *by = bx - size;
                while (by>=b1)
                {   *(by + size) = *by;
                    by -= size;
                }
                *(by + size) = save;
            }
        }
    }
}


/* end of sort.c */
