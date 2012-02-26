/* Embedded Xinu, Copyright (C) 2009.  All rights reserved. */

#ifndef _STDLIB_H_
#define _STDLIB_H_

int abs(int);
long labs(long);
int atoi(char *);
long atol(char *);
void bzero(void *, int);
void qsort(char *, unsigned int, int, int (*)(void));
unsigned long rand(void);
void srand(unsigned int);
void *malloc(unsigned int nbytes);
void free(void *pmem);

#endif                          /* _STDLIB_H_ */
