#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
/* Gives us sleep: */
#include <unistd.h>
/* Lets us seed the random number generator
 * with something reasonable
 */
#include <time.h>
/* Lets use semaphores */
#include <semaphore.h>

/* http://www.yolinux.com/TUTORIALS/LinuxTutorialPosixThreads.html */

/* Forward declare a function to print a message.
 * The types must conform to the pthread expectations
 * of passing a void pointer and returning a void pointer.
 */
void *print_message ( void *ptr);

sem_t mutex;

/* We'll pass a packet of information
 * in to our threads for testing purposes
 */
/* http://www.cs.usfca.edu/~wolber/SoftwareDev/C/CStructs.htm */
typedef struct {
  int uid;
  int delay;
  char *msg;
  /* And, lets make sure we don't clobber each-other */
  /* http://www.csc.villanova.edu/~mdamian/threads/posixsem.html */
  sem_t sem;
} Packet;

int main () {
  /* Thread pointers */
  pthread_t t1, t2, t3;
  /* Packets (to be filled) */
  Packet *p1, *p2, *p3;
  /* Messages for the threads to talk to us. */
  char *msg1 = "Thread One";
  char *msg2 = "Thread Two";
  char *msg3 = "Thread Three";
  /* Return values from threads */
  int ret1, ret2, ret3;

  /* Allocate and fill packets */
  /* http://www.elook.org/programming/c/malloc.html */
  p1 = (Packet*) malloc (sizeof(Packet));
  p2 = (Packet*) malloc (sizeof(Packet));
  p3 = (Packet*) malloc (sizeof(Packet));
  
  /* Delay for some number of seconds */
  /* http://www.java2s.com/Code/C/Math/Generaterandomnumberhowtouserand.htm */
  /* http://www.daniweb.com/software-development/c/threads/169584 */
  srand((unsigned)time(NULL));
  p1->delay = rand() % 10;
  p2->delay = rand() % 10;
  p3->delay = rand() % 10;

  p1->msg = msg1;
  p2->msg = msg2;
  p3->msg = msg3;

  p1->uid = 1;
  p2->uid = 2;
  p3->uid = 3;

  /* Init the semaphore too */
  /* We init pshared to zero because we're not worried about
   * sharing with forked processes. We set value to 0 because
   * we only want one thread in the critical section at a time. */
  sem_init (&mutex, 0, 1);

  /*
  p1->sem = semaphore;
  p2->sem = semaphore;
  p3->sem = semaphore;
  */

  /* When should everyone report? */
  printf ("Thread %d reporting in at %d seconds\n", 1, p1->delay);
  printf ("Thread %d reporting in at %d seconds\n", 2, p2->delay);
  printf ("Thread %d reporting in at %d seconds\n", 3, p3->delay);
  printf ("====================================\n\n");

  
  ret1 = pthread_create ( &t1, NULL, print_message, (void*) p1);
  ret2 = pthread_create ( &t2, NULL, print_message, (void*) p2);
  ret3 = pthread_create ( &t3, NULL, print_message, (void*) p3);
  
  /* Wait for all of the threads to finish */
  pthread_join (t1, NULL);
  pthread_join (t2, NULL);
  pthread_join (t3, NULL);

  printf ("Threads done.\n");
  
  return 0;
}

/* *ptr is a pointer to a Packet */
void *print_message (void *ptr) {
  Packet *pkt;
  int value;

  pkt = (Packet*)ptr;
  /* We all sleep without interrupting criticality. */
  printf ("Waiting for critical region in Thread %d\n", pkt->uid);
  sem_wait (&mutex);
  sleep (pkt->delay);

  /* Only one of us prints at a time. */
  printf ("%s at time %d\n", pkt->msg, pkt->delay);
  sem_post (&mutex);
  pthread_exit(0);
}
