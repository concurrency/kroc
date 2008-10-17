#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

#define ELEMENTS 256

static pthread_t	thread[ELEMENTS];
static pthread_mutex_t	mutex[ELEMENTS];
static pthread_cond_t	cond[ELEMENTS];
static volatile int	full[ELEMENTS];
static volatile int	data[ELEMENTS];

static int		cycles;
static int		tokens;

static void send_to (int i, int d)
{
	pthread_mutex_lock (&(mutex[i]));
	while (full[i])
		pthread_cond_wait (&(cond[i]), &(mutex[i]));
	full[i] = 1;
	data[i] = d;
	pthread_cond_signal (&(cond[i]));
	pthread_mutex_unlock (&(mutex[i]));
}

static int recv_from (int i)
{
	int d;
	pthread_mutex_lock (&(mutex[i]));
	while (!full[i])
		pthread_cond_wait (&(cond[i]), &(mutex[i]));
	full[i] = 0;
	d = data[i];
	pthread_cond_signal (&(cond[i]));
	pthread_mutex_unlock (&(mutex[i]));
	return d;
}

static void *root (void *n)
{
	int this = (int) n;
	int next = (this + 1) % ELEMENTS;
	int i, sum, token;

	send_to (next, 1);
	token = recv_from (this);

	fprintf (stdout, "start\n");
	fflush (stdout);

	for (i = 0; i < tokens; ++i)
		send_to (next, i + 1);

	while (cycles > 0) {
		for (i = 0; i < tokens; ++i) {
			token = recv_from (this);
			send_to (next, token + 1);
		}
		cycles--;
	}
	
	sum = 0;
	for (i = 0; i < tokens; ++i)
		sum += recv_from (this);

	fprintf (stdout, "end\n");
	fflush (stdout);
	
	fprintf (stdout, "%d\n", sum);

	send_to (next, 0);
	token = recv_from (this);

	return NULL;
}

static void *element (void *n)
{
	int this = (int) n;
	int next = (this + 1) % ELEMENTS;
	int token;

	do {
		token = recv_from (this);
		send_to (next, token > 0 ? token + 1 : token);
	} while (token);

	return NULL;
}

int main (int argc, char *argv[])
{
	int i;

	if (argc >= 2)
		cycles = atoi (argv[1]);
	else
		cycles = 0;
	if (argc >= 3)
		tokens = atoi (argv[2]);
	else
		tokens = 1;

	for (i = 0; i < ELEMENTS; ++i)
		full[i] = data[i] = 0;

	for (i = ELEMENTS - 1; i >= 0; --i) {
		pthread_mutex_init (&(mutex[i]), NULL);
		pthread_cond_init (&(cond[i]), NULL);

		if (i == 0)
			pthread_create (&(thread[i]), NULL, root, (void *)i);
		else
			pthread_create (&(thread[i]), NULL, element, (void *)i);
		
	}

	pthread_join (thread[0], NULL);

	return 0;
}
