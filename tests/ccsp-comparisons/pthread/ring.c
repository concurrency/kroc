#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

#define ELEMENTS 256

static pthread_t	thread[ELEMENTS];
static pthread_mutex_t	mutex[ELEMENTS];
static pthread_cond_t	cond[ELEMENTS];
static volatile int	data[ELEMENTS];

static pthread_mutex_t	sync_mutex;
static pthread_cond_t	sync_cond;
static int		cycles;

static void *root (void *n)
{
	int this = (int) n;
	int next = (this + 1) % ELEMENTS;
	int token;

	pthread_mutex_lock (&(mutex[this]));

	pthread_mutex_lock (&sync_mutex);
	pthread_cond_signal (&sync_cond);
	pthread_mutex_unlock (&sync_mutex);

	pthread_mutex_lock (&(mutex[next]));
	data[next] = 1;
	pthread_cond_signal (&(cond[next]));
	pthread_mutex_unlock (&(mutex[next]));
	pthread_cond_wait (&(cond[this]), &(mutex[this]));
	token = data[this];

	fprintf (stdout, "start\n");
	fflush (stdout);

	while (cycles > 0) {
		pthread_mutex_lock (&(mutex[next]));
		data[next] = token + 1;
		
		pthread_cond_signal (&(cond[next]));
		pthread_mutex_unlock (&(mutex[next]));
		
		pthread_cond_wait (&(cond[this]), &(mutex[this]));
		token = data[this];
		cycles--;
	}
	
	fprintf (stdout, "end\n");
	fflush (stdout);
	
	fprintf (stdout, "%d\n", token);

	pthread_mutex_lock (&(mutex[next]));
	data[next] = 0;
	pthread_cond_signal (&(cond[next]));
	pthread_mutex_unlock (&(mutex[next]));
	pthread_cond_wait (&(cond[this]), &(mutex[this]));
	
	pthread_mutex_unlock (&(mutex[this]));

	return NULL;
}

static void *element (void *n)
{
	int this = (int) n;
	int next = (this + 1) % ELEMENTS;
	int token;

	pthread_mutex_lock (&(mutex[this]));
	
	pthread_mutex_lock (&sync_mutex);
	pthread_cond_signal (&sync_cond);
	pthread_mutex_unlock (&sync_mutex);

	do {
		pthread_cond_wait (&(cond[this]), &(mutex[this]));
		token = data[this];
		pthread_mutex_lock (&(mutex[next]));
		data[next] = token > 0 ? token + 1 : token;
		pthread_cond_signal (&(cond[next]));
		pthread_mutex_unlock (&(mutex[next]));
	} while (token);

	pthread_mutex_unlock (&(mutex[this]));

	return NULL;
}

int main (int argc, char *argv[])
{
	int i;

	if (argc >= 2)
		cycles = atoi (argv[1]);
	else
		cycles = 0;

	pthread_mutex_init (&sync_mutex, NULL);
	pthread_cond_init (&sync_cond, NULL);

	pthread_mutex_lock (&sync_mutex);

	for (i = ELEMENTS - 1; i >= 0; --i) {
		pthread_mutex_init (&(mutex[i]), NULL);
		pthread_cond_init (&(cond[i]), NULL);

		if (i == 0)
			pthread_create (&(thread[i]), NULL, root, (void *)i);
		else
			pthread_create (&(thread[i]), NULL, element, (void *)i);
		
		pthread_cond_wait (&sync_cond, &sync_mutex);
	}

	pthread_mutex_unlock (&sync_mutex);
	pthread_join (thread[0], NULL);

	return 0;
}
