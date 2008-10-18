#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <pthread.h>

#define N_CYCLES	1024

#define WORLD_SIZE	5

#define LOC_SIZE	4096
#define	LOC_AREA	(LOC_SIZE * LOC_SIZE)
#define LOC_AGENTS	12

#define	LOC_MAX		((LOC_SIZE / 2) - 1)
#define LOC_MIN		(-(LOC_SIZE / 2))

typedef struct _barrier_t {
	pthread_mutex_t mutex;
	pthread_cond_t cond;
	volatile int enrolled;
	volatile int count;
} barrier_t;

typedef struct _chan_t {
	pthread_mutex_t mutex;
	pthread_cond_t cond;
	volatile int full;
	volatile int data;
} chan_t;

typedef struct _cli_svr_t {
	pthread_mutex_t cli_lock;
	chan_t *req;
	chan_t *resp;
} cli_svr_t;

typedef struct _vector_t {
	int	x;
	int	y;
} vector_t;

typedef struct _agent_t {
	int		id;
	int		loc;
	vector_t	pos;
} agent_t;

typedef struct _agent_elem_t {
	int		prev;
	int		next;
	agent_t		data;
} agent_elem_t;

typedef struct _agent_list_t {
	int		size;
	int		used;
	int		head;
	int		tail;
	int		free;
	agent_elem_t	elements[1];
} agent_list_t;

enum {
	LOC_ENTER,
	LOC_MOVE,
	LOC_GET_VIEW,
	LOC_BORROW_INFO,
	LOC_SHUTDOWN,
	LOC_STAY_HERE,
	LOC_GO_THERE
};

enum {
	VIEW_SHUTDOWN = -1
};

#define NEIGHBOURS 8
static const vector_t offsets[] = {
	{ .x = -1, .y = -1 },
	{ .x =  0, .y = -1 },
	{ .x =  1, .y = -1 },
	{ .x = -1, .y =  0 },
	{ .x =  1, .y =  0 },
	{ .x = -1, .y =  1 },
	{ .x =  0, .y =  1 },
	{ .x =  1, .y =  1 },
	{ .x =  0, .y =  0 }
};

static pthread_attr_t thread_attrs;
static int world_size = WORLD_SIZE;
static int loc_agents = LOC_AGENTS;

static inline void vector_add (vector_t *a, const vector_t *b)
{
	a->x += b->x;
	a->y += b->y;
}

static inline void vector_sub (vector_t *a, const vector_t *b)
{
	a->x -= b->x;
	a->y -= b->y;
}

static inline int dir_value (int n)
{
	if (n < LOC_MIN)
		return -1;
	else if (n > LOC_MAX)
		return 1;
	else
		return 0;
}

static inline int in_bounds (vector_t *p)
{
	if (p->x < LOC_MIN || p->x > LOC_MAX)
		return 0;
	else
		return !(p->y < LOC_MIN || p->y > LOC_MAX);
}

static void reset_agent_list (agent_list_t *l) {
	int i;
	
	l->used = 0;
	l->head = -1;
	l->tail = -1;
	l->free = 0;

	for (i = 0; i < l->size - 1; ++i)
		l->elements[i].next = i + 1;
	l->elements[l->size - 1].next = -1;
}

static agent_list_t *alloc_agent_list (void)
{
	agent_list_t *l = (agent_list_t *) malloc (sizeof (agent_list_t));

	l->size = 1;
	reset_agent_list (l);

	return l;
}

static agent_t *add_agent (agent_list_t **list, agent_t *info)
{
	agent_list_t *l = *list;
	int idx;

	if (l->free == -1) {
		int old_size = l->size;
		int new_size = 2 + (old_size - 1) + ((old_size - 1) / 2);
		int ob, nb;
		int i;

		nb = sizeof (agent_list_t) + (sizeof (agent_elem_t) * (new_size - 1));
		ob = sizeof (agent_list_t) + (sizeof (agent_elem_t) * (old_size - 1));

		l = (agent_list_t *) malloc (nb);
		memcpy (l, *list, ob);
		l->size = new_size;
		l->free = old_size;
		for (i = old_size; i < (new_size - 1); ++i) {
			l->elements[i].next = i + 1;
		}
		l->elements[new_size - 1].next = -1;
		free (*list);
		*list = l;
	}

	assert (l->used < l->size);

	idx = l->free;
	l->used++;
	l->free = l->elements[idx].next;
	if (l->tail != -1)
		l->elements[l->tail].next = idx;
	l->elements[idx].prev = l->tail;
	l->elements[idx].next = -1;
	l->tail = idx;

	if (l->head == -1)
		l->head = idx;

	assert (l->used < l->size || (l->used == l->size && l->free == -1));
	
	memcpy (&(l->elements[idx].data), info, sizeof (*info));

	return &(l->elements[idx].data);
}

static void remove_agent (agent_list_t **list, int idx)
{
	agent_list_t *l = *list;
	int prev = l->elements[idx].prev;
	int next = l->elements[idx].next;

	if (prev != -1)
		l->elements[prev].next = next;
	if (next != -1)
		l->elements[next].prev = prev;
	if (l->head == idx)
		l->head = next;
	if (l->tail == idx)
		l->tail = prev;

	l->elements[idx].next = l->free;
	l->used--;
	l->free	= idx;
}

static int find_agent (agent_list_t *l, int id)
{
	int p = l->head;

	assert (p != -1);

	while (l->elements[p].data.id != id) {
		p = l->elements[p].next;
		assert (p != -1);
	}

	return p;
}

static void merge_agents (const vector_t *offset, agent_list_t **result, agent_list_t *loc)
{
	int p = loc->head;

	while (p != -1) {
		agent_t *info = add_agent (result, &(loc->elements[p].data));
		info->pos.x += offset->x * LOC_SIZE;
		info->pos.y += offset->y * LOC_SIZE;
		p = loc->elements[p].next;
	}
}

static barrier_t *alloc_barrier (int count)
{
	barrier_t *b = (barrier_t *) malloc (sizeof (barrier_t));
	
	pthread_mutex_init (&(b->mutex), NULL);
	pthread_cond_init (&(b->cond), NULL);
	b->enrolled = count;
	b->count = count;

	return b;
}

static void bar_sync (barrier_t *b)
{
	pthread_mutex_lock (&(b->mutex));
	if (b->count > 1) {
		b->count--;
		pthread_cond_wait (&(b->cond), &(b->mutex));
	} else {
		b->count = b->enrolled;
		pthread_cond_broadcast (&(b->cond));
	}
	pthread_mutex_unlock (&(b->mutex));
}

static chan_t *alloc_chan (void)
{
	chan_t *chan = (chan_t *) malloc (sizeof (chan_t));
	
	pthread_mutex_init (&(chan->mutex), NULL);
	pthread_cond_init (&(chan->cond), NULL);
	chan->full = 0;
	chan->data = 0;

	return chan;
}

static cli_svr_t *alloc_cli_svr (void)
{
	cli_svr_t *cs = (cli_svr_t *) malloc (sizeof (cli_svr_t));

	pthread_mutex_init (&(cs->cli_lock), NULL);
	cs->req = alloc_chan ();
	cs->resp = alloc_chan ();

	return cs;
}

static void send_to (chan_t *chan, int d)
{
	pthread_mutex_lock (&(chan->mutex));
	while (chan->full)
		pthread_cond_wait (&(chan->cond), &(chan->mutex));
	chan->full = 1;
	chan->data = d;
	pthread_cond_signal (&(chan->cond));
	pthread_mutex_unlock (&(chan->mutex));
}

static int recv_from (chan_t *chan)
{
	int d;
	pthread_mutex_lock (&(chan->mutex));
	while (!chan->full)
		pthread_cond_wait (&(chan->cond), &(chan->mutex));
	chan->full = 0;
	d = chan->data;
	pthread_cond_signal (&(chan->cond));
	pthread_mutex_unlock (&(chan->mutex));
	return d;
}

static void compile_view (cli_svr_t **search, agent_list_t **result)
{
	int i;

	reset_agent_list (*result);

	for (i = 0; i < (NEIGHBOURS + 1); ++i) {
		agent_list_t *agents = NULL;
		cli_svr_t *loc = search[i];

		pthread_mutex_lock (&(loc->cli_lock));
		send_to (loc->req, LOC_BORROW_INFO);
		agents = (agent_list_t *) recv_from (loc->resp);
		merge_agents (&(offsets[i]), result, agents);
		send_to (loc->req, (int) agents);
		pthread_mutex_unlock (&(loc->cli_lock));
	}
}

static void *view (void *param_ptr)
{
	int *params		= (int *) param_ptr;
	cli_svr_t **search	= (cli_svr_t **) params[0];
	agent_list_t *data	= alloc_agent_list ();
	chan_t	*req		= (chan_t *) params[1];
	chan_t *resp		= (chan_t *) params[2];
	int running 		= 1;
	int cycle 		= -1;

	while (running) {
		int msg = recv_from (req);
		if (msg != VIEW_SHUTDOWN) {
			if (cycle != msg) {
				compile_view (search, &data);
				cycle = msg;
			}
			send_to (resp, (int) data);
		} else {
			running = 0;
		}
	}

	free (data);
	free (params);
	/* FIXME: free stuff */
	return NULL;
}

static cli_svr_t **build_search (cli_svr_t *self, cli_svr_t **neighbours)
{
	cli_svr_t **search 	= (cli_svr_t **) malloc (sizeof (cli_svr_t *) * (NEIGHBOURS + 1));
	int i;
	
	for (i = 0; i < NEIGHBOURS; ++i) {
		search[i] = neighbours[i];
	}
	search[NEIGHBOURS] = self;

	return search;
}

static void redirect_agent (cli_svr_t **neighbours, chan_t *resp, agent_t *info)
{
	vector_t o;
	int d = 0;

	o.x = dir_value (info->pos.x);
	o.y = dir_value (info->pos.y);
	while (offsets[d].x != o.x || offsets[d].y != o.y)
		d++;
	
	assert (d < NEIGHBOURS);

	o.x *= LOC_SIZE;
	o.y *= LOC_SIZE;

	vector_sub (&(info->pos), &o);

	send_to (resp, LOC_GO_THERE);
	send_to (resp, (int) neighbours[d]);
}

static void *location (void *param_ptr)
{
	int *params		= (int *) param_ptr;
	int	loc		= params[0];
	cli_svr_t *svr		= (cli_svr_t *) params[1];
	cli_svr_t **neighbours	= (cli_svr_t **) params[2];

	agent_list_t *state	= alloc_agent_list ();
	cli_svr_t *view_cb	= alloc_cli_svr ();
	chan_t *req		= svr->req;
	chan_t *resp		= svr->resp;
	int running 		= 1;

	{
		cli_svr_t **search	= build_search (svr, neighbours);
		int *params		= (int *) malloc (sizeof (int) * 3);
		pthread_t thread;
		int ret;

		params[0] = (int) search;
		params[1] = (int) view_cb->req;
		params[2] = (int) view_cb->resp;

		ret = pthread_create (&thread, &thread_attrs, view, params);
		if (ret != 0) {
			fprintf (stderr, "errno = %d\n", errno);
			assert (ret == 0);
		}
	}

	while (running) {
		int type = recv_from (req);
		
		if (type == LOC_ENTER) {
			agent_t *info = (agent_t *) recv_from (req);

			info->loc = loc;
			if (in_bounds (&(info->pos))) {
				add_agent (&state, info);
				send_to (resp, LOC_STAY_HERE);
			} else {
				redirect_agent (neighbours, resp, info);
			}
		} else if (type == LOC_MOVE) {
			agent_t *info = (agent_t *) recv_from (req);
			int idx = find_agent (state, info->id);

			if (in_bounds (&(info->pos))) {
				memcpy (&(state->elements[idx].data), info, sizeof (*info));
				send_to (resp, LOC_STAY_HERE);
			} else {
				redirect_agent (neighbours, resp, info);
				remove_agent (&state, idx);
			}
		} else if (type == LOC_GET_VIEW) {
			send_to (resp, (int) view_cb);
		} else if (type == LOC_BORROW_INFO) {
			send_to (resp, (int) state);
			recv_from (req);
		} else if (type == LOC_SHUTDOWN) {
			pthread_mutex_lock (&(view_cb->cli_lock));
			send_to (view_cb->req, VIEW_SHUTDOWN);
			pthread_mutex_unlock (&(view_cb->cli_lock));
			running = 0;
		} else {
			assert (0);
		}
	}

	free (params);
	free (state);
	free (svr);
	/* FIXME: free stuff */
	return NULL;
}

static int a_sqrt (const int x, int r)
{
	int last_r = 0;
	int step = 0;

	do {
		last_r = r;
		r = (r + (x / r)) >> 1;
		step++;
	} while (r != last_r && step < 16);

	return r;
}

#define AGENT_STACK 4096
static void *agent (void *param_ptr)
{
	int *params	= (int *) param_ptr;
	
	barrier_t *b	= (barrier_t *) params[4];
	cli_svr_t *loc	= (cli_svr_t *) params[3];
	cli_svr_t *view = NULL;
	int cycle	= 0;

	agent_t info;
	int persona, resp;

	info.id	 	= params[0];
	info.loc	= -1;
	info.pos.x	= params[1];
	info.pos.y	= params[2];
	persona		= info.id * 37;

	printf ("%d %d start\n", cycle, info.id);

	bar_sync (b);

	pthread_mutex_lock (&(loc->cli_lock));
	send_to (loc->req, LOC_ENTER);
	send_to (loc->req, (int) &info);
	resp = recv_from (loc->resp);
	assert (resp == LOC_STAY_HERE);
	send_to (loc->req, LOC_GET_VIEW);
	view = (cli_svr_t *) recv_from (loc->resp);
	pthread_mutex_unlock (&(loc->cli_lock));

	bar_sync (b);

	while (cycle < N_CYCLES) {
		agent_list_t *agents = NULL;
		vector_t force;
		int idx, px, py;
		
		force.x = 0;
		force.y = 0;

		persona = (persona & 65535) * info.pos.x;
		persona = (persona & 65535) * info.pos.y;
		persona = (persona & 65535) * info.loc;

		//printf ("%d %d at %d:%d,%d\n", cycle, info.id, info.loc, info.pos.x, info.pos.y);
		
		cycle++;

		pthread_mutex_lock (&(view->cli_lock));
		send_to (view->req, cycle);
		agents = (agent_list_t *) recv_from (view->resp);
		pthread_mutex_unlock (&(view->cli_lock));

		persona += agents->used;
		px	= persona & 0xff;
		py	= (persona >> 8) & 0xff;
		idx 	= agents->head;
		while (idx != -1) {
			agent_t *a_info = &(agents->elements[idx].data);
			int dx, dy, dx2, dy2, r2, r, f;
			idx		= agents->elements[idx].next;

			if (a_info->id == info.id)
				continue;

			dx = info.pos.x - a_info->pos.x;
			dy = info.pos.y - a_info->pos.y;
			dx = (dx * (px + 1)) / 128;
			dy = (dy * (py + 1)) / 128;
			dx2 = dx * dx;
			dy2 = dy * dy;
			r2 = dx2 + dy2;
			f = (3 * LOC_AREA) / (r2 + 1);
			if (f > LOC_SIZE)
				f = LOC_SIZE;
			
			if (dx2 > dy2)
				r = a_sqrt (r2, abs (dx));
			else if (dy2 > 0)
				r = a_sqrt (r2, abs (dy));
			else
				r = 1;

			if (r2 != 0) {
				force.x += (f * dx) / r;
				force.y += (f * dy) / r;
			} else {
				int x = (cycle & 1) * (-1);
				int y = ((cycle >> 1) & 1) * (-1);
				force.x += f * x;
				force.y += f * y;
			}
		}

		bar_sync (b);

		vector_add (&(info.pos), &force);

		idx = 1;
		do {
			pthread_mutex_lock (&(loc->cli_lock));
			if (idx == 1) {
				send_to (loc->req, LOC_MOVE);
			} else {
				send_to (loc->req, LOC_ENTER);
			}
			send_to (loc->req, (int) &info);
			resp = recv_from (loc->resp);
			if (resp == LOC_STAY_HERE) {
				if (idx > 1) {
					send_to (loc->req, LOC_GET_VIEW);
					view = (cli_svr_t *) recv_from (loc->resp);
				}
				pthread_mutex_unlock (&(loc->cli_lock));
				idx = 0;
			} else if (resp == LOC_GO_THERE) {
				cli_svr_t *new_loc = (cli_svr_t *) recv_from (loc->resp);
				pthread_mutex_unlock (&(loc->cli_lock));
				loc = new_loc;
				idx++;
			} else {
				assert (0);
			}
		} while (idx);
		
		bar_sync (b);
	}

	bar_sync (b);

	printf ("%d %d at %d:%d,%d\n", cycle, info.id, info.loc, info.pos.x, info.pos.y);
	printf ("%d %d end %d\n", cycle, info.id, persona);
	
	bar_sync (b);

	free (params);
	/* FIXME: free stuff ? */
	return NULL;
}

static int wrap (int x, int max)
{
	while (x < 0)
		x += max;
	return x % max;
}

static cli_svr_t **build_neighbours (int loc, cli_svr_t **world)
{
	cli_svr_t **na	= (cli_svr_t **) malloc (sizeof (cli_svr_t *) * NEIGHBOURS);
	int x		= loc % world_size;
	int y		= loc / world_size;
	int i;

	for (i = 0; i < NEIGHBOURS; ++i) {
		int n_x = wrap (x + offsets[i].x, world_size);
		int n_y	= wrap (y + offsets[i].y, world_size);
		int n_loc = (n_y * world_size) + n_x;

		na[i] = world[n_loc];
	}

	return na;
}

static void proc_main (void)
{
	int world_area = world_size * world_size;
	cli_svr_t **world = (cli_svr_t **) malloc (sizeof (cli_svr_t *) * world_area);
	barrier_t *b;
	int i, j, id;

	pthread_attr_init (&thread_attrs);
	pthread_attr_setstacksize (&thread_attrs, 16 * 1024);

	b = alloc_barrier ((world_area * loc_agents) + 1);

	for (i = 0; i < world_area; ++i)
		world[i] = alloc_cli_svr ();
	
	for (i = 0; i < world_area; ++i) {
		cli_svr_t **neighbours = build_neighbours (i, world);
		int *params = (int *) malloc (sizeof (int) * 3);
		pthread_t thread;
		int ret;

		params[0] = i;
		params[1] = (int) world[i];
		params[2] = (int) neighbours;

		ret = pthread_create (&thread, &thread_attrs, location, params);
		if (ret != 0) {
			fprintf (stderr, "errno = %d\n", errno);
			assert (ret == 0);
		}
	}

	for (i = 0, id = 1; i < world_area; ++i) {
		for (j = 0; j < loc_agents; ++j, ++id) {
			int o = LOC_MIN + ((LOC_SIZE / (loc_agents + 4)) * (j + 2));
			int *params = (int *) malloc (sizeof (int) * 5);
			pthread_t thread;
			int ret;

			params[0] = id;
			params[1] = o;
			params[2] = o;
			params[3] = (int) world[i];
			params[4] = (int) b;

			ret = pthread_create (&thread, &thread_attrs, agent, params);
			if (ret != 0) {
				fprintf (stderr, "errno = %d\n", errno);
				assert (ret == 0);
			}
		}
	}

	for (i = 0; i < (N_CYCLES + 2); ++i) {
		bar_sync (b);
		bar_sync (b);
	}
	
	for (i = 0; i < world_area; ++i) {
		pthread_mutex_lock (&(world[i]->cli_lock));
		send_to (world[i]->req, LOC_SHUTDOWN);
		pthread_mutex_unlock (&(world[i]->cli_lock));
	}

	fflush (stdout);
}

int main (int argc, char *argv[])
{
	if (argc >= 2)
		world_size = atoi (argv[1]);
	if (argc >= 3)
		loc_agents = atoi (argv[2]);
	
	proc_main ();
	
	return 0;
}
