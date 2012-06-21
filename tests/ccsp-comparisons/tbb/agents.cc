#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <tbb/parallel_for.h>
#include <tbb/queuing_mutex.h>

#define N_CYCLES	1024

#define WORLD_SIZE	5

#define LOC_SIZE	4096
#define	LOC_AREA	(LOC_SIZE * LOC_SIZE)
#define LOC_AGENTS	12

#define	LOC_MAX		((LOC_SIZE / 2) - 1)
#define LOC_MIN		(-(LOC_SIZE / 2))

typedef struct _vector_t {
	int	x;
	int	y;
} vector_t;

typedef struct _agent_t agent_t;

typedef struct _agent_list_t {
	tbb::queuing_mutex mutex;
	agent_t		*head;
	agent_t		*tail;
	int		size;
} agent_list_t;

struct _agent_t {
	int		id;
	int		loc;
	int		persona;
	vector_t	pos;
	vector_t	force;
	
	agent_list_t	*p;
	agent_t		*prev;
	agent_t		*next;
};

#define NEIGHBOURS 8
static const vector_t offsets[] = {
	{ -1, -1 },
	{ 0, -1 },
	{ 1, -1 },
	{ -1, 0 },
	{ 1, 0 },
	{ -1, 1 },
	{ 0, 1 },
	{ 1, 1 },
	{ 0, 0 }
};

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
	
	l->head = NULL;
	l->tail = NULL;
	l->size = 0;
}

static agent_list_t *alloc_agent_list (void)
{
	agent_list_t *l = new agent_list_t;

	reset_agent_list (l);

	return l;
}

static void add_agent (agent_list_t *l, agent_t *info)
{
	tbb::queuing_mutex::scoped_lock lock(l->mutex);

	info->next = NULL;

	if (l->head == NULL) {
		l->head = l->tail = info;
		info->prev = NULL;
	} else {
		l->tail->next = info;
		info->prev = l->tail;
		l->tail = info;
	}

	l->size++;
}

static void remove_agent (agent_list_t *l, agent_t *info)
{
	tbb::queuing_mutex::scoped_lock lock(l->mutex);

	if (info->prev == NULL) {
		l->head = info->next;
	} else {
		info->prev->next = info->next;
	}
	if (info->next == NULL) {
		l->tail = info->prev;
	} else {
		info->next->prev = info->prev;
	}
	
	l->size--;
}

static int wrap (int x, int max)
{
	while (x < 0)
		x += max;
	return x % max;
}

static int calc_loc (const int loc, const vector_t *v)
{
	int x		= loc % world_size;
	int y		= loc / world_size;
	int n_x 	= wrap (x + v->x, world_size);
	int n_y		= wrap (y + v->y, world_size);
	int n_loc	= (n_y * world_size) + n_x;
	return n_loc;
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

static void init_agent (agent_t *info, agent_list_t **world)
{
	info->persona = info->id * 37;
	info->p = world[info->loc];
	info->force.x = 0;
	info->force.y = 0;
	add_agent (info->p, info);
	printf ("%d %d start\n", 0, info->id);
}

static void agent_view (int cycle, agent_t *info, agent_list_t **world)
{
	agent_list_t *view[NEIGHBOURS + 1];
	vector_t force;
	int i, px, py;
	
	force.x = 0;
	force.y = 0;

	info->persona = (info->persona & 65535) * info->pos.x;
	info->persona = (info->persona & 65535) * info->pos.y;
	info->persona = (info->persona & 65535) * info->loc;

	/* printf ("%d %d at %d:%d,%d\n", cycle, info->id, info->loc, info->pos.x, info->pos.y); */

	for (i = 0; i < (NEIGHBOURS + 1); ++i) {
		const vector_t *offset = &(offsets[i]);
		int loc = calc_loc (info->loc, offset);
		view[i] = world[loc];
		info->persona += view[i]->size;
	}
	
	px	= info->persona & 0xff;
	py	= (info->persona >> 8) & 0xff;
	
	for (i = 0; i < (NEIGHBOURS + 1); ++i) {
		const agent_list_t 	*l	= view[i];
		const vector_t		*offset = &(offsets[i]);
		const int		ox	= offset->x * LOC_SIZE;
		const int		oy	= offset->y * LOC_SIZE;
		agent_t 		*a_info = l->head;

		while (a_info != NULL) {
			int dx, dy, dx2, dy2, r2, r, f;

			if (a_info->id == info->id) {
				a_info = a_info->next;
				continue;
			}

			dx = info->pos.x - (a_info->pos.x + ox);
			dy = info->pos.y - (a_info->pos.y + oy);
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
				int x = ((cycle + 1) & 1) * (-1);
				int y = (((cycle + 1) >> 1) & 1) * (-1);
				force.x += f * x;
				force.y += f * y;
			}

			a_info = a_info->next;
		}
	}
	
	memcpy (&(info->force), &force, sizeof (vector_t));
}

static void agent_move (int cycle, agent_t *info, agent_list_t **world)
{
	vector_add (&(info->pos), &(info->force));

	if (in_bounds (&(info->pos)))
		return;
	
	remove_agent (info->p, info);

	do {
		vector_t o, d;

		o.x = dir_value (info->pos.x);
		o.y = dir_value (info->pos.y);
		d.x = o.x * LOC_SIZE;
		d.y = o.y * LOC_SIZE;

		vector_sub (&(info->pos), &d);

		info->loc	= calc_loc (info->loc, &o);
		info->p 	= world[info->loc];

	} while (!in_bounds (&(info->pos)));

	add_agent (info->p, info);
}

static void proc_main (void)
{
	const int world_area 	= world_size * world_size;
	const int agent_count	= world_area * loc_agents;
	agent_list_t **world = new agent_list_t *[world_area];
	agent_t *agents = new agent_t[agent_count + 1];
	int i, j, id;

	for (i = 0; i < world_area; ++i)
		world[i] = alloc_agent_list ();
	
	for (i = 0, id = 1; i < world_area; ++i) {
		for (j = 0; j < loc_agents; ++j, ++id) {
			int o = LOC_MIN + ((LOC_SIZE / (loc_agents + 4)) * (j + 2));
			agents[id].id = id;
			agents[id].loc = i;
			agents[id].pos.x = o;
			agents[id].pos.y = o;
			init_agent (&(agents[id]), world);
		}
	}

	for (i = 0; i < N_CYCLES; ++i) {
		tbb::parallel_for (1, agent_count + 1,
				   [&](int j) { agent_view (i, &(agents[j]), world); });
		tbb::parallel_for (1, agent_count + 1,
				   [&](int j) { agent_move (i, &(agents[j]), world); });
	}

	for (j = 1; j <= agent_count; ++j) {
		agent_t *info = &(agents[j]);
		printf ("%d %d at %d:%d,%d\n", N_CYCLES, info->id, info->loc, info->pos.x, info->pos.y);
		printf ("%d %d end %d\n", N_CYCLES, info->id, info->persona);
	}
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
