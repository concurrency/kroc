#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
/* Gives us sleep: */
#include <unistd.h>
/* Lets us seed the random number generator
 * with something reasonable
 */
#include <time.h>

/* Compile with
  gcc -o te thread-exploration.c -lpthread
*/

/* http://www.yolinux.com/TUTORIALS/LinuxTutorialPosixThreads.html */

/* Forward declare a function to print a message.
 * The types must conform to the pthread expectations
 * of passing a void pointer and returning a void pointer.
 */
void *print_message ( void *ptr);

/* A global lock for sync: 
   This could be in a struct. 
*/
pthread_mutex_t GLOBAL_LOCK;

/* We'll pass a packet of information
 * in to our threads for testing purposes
 */
/* http://www.cs.usfca.edu/~wolber/SoftwareDev/C/CStructs.htm */
typedef struct {
  int uid;
  int delay;
  char *msg;
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

	/* Initialize the lock. Could do this to an array of locks
	   or similar... but we do need to init each one.
		 This could be in a struct (eg. in the virtual processor
		 or runqueue.
  */
	 pthread_mutex_init(&GLOBAL_LOCK, NULL);

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
	/* Here we wait on the lock. */
	pthread_mutex_lock(&GLOBAL_LOCK);
	/* Then we sleep, simulating work being done. */
  sleep (pkt->delay);

  /* Only one of us prints at a time. */
  printf ("%s at time %d\n", pkt->msg, pkt->delay);
	/* Now we release the lock. Someone else can enter the critical section. */
	pthread_mutex_unlock(&GLOBAL_LOCK);
	/* It is nice to exit threads and report no errors. */
  pthread_exit(0);
}
