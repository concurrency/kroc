#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <cif.h>

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
	VIEW_BORROW_VIEW,
	VIEW_SHUTDOWN
};

typedef struct _move_req_t {
	int		id;
	vector_t	v;
} move_req_t;

typedef struct _view_req_t {
	int		type;
	int		cycle;
} view_req_t;

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

static agent_list_t *alloc_agent_list (Workspace wptr)
{
	agent_list_t *l = (agent_list_t *) MAlloc (wptr, sizeof (agent_list_t));

	l->size = 1;
	reset_agent_list (l);

	return l;
}

static agent_t *add_agent (Workspace wptr, agent_list_t **list, agent_t *info)
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

		l = (agent_list_t *) MAlloc (wptr, nb);
		memcpy (l, *list, ob);
		l->size = new_size;
		l->free = old_size;
		for (i = old_size; i < (new_size - 1); ++i) {
			l->elements[i].next = i + 1;
		}
		l->elements[new_size - 1].next = -1;
		MRelease (wptr, *list);
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

static void remove_agent (Workspace wptr, agent_list_t **list, int idx)
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

static void merge_agents (Workspace wptr, const vector_t *offset, agent_list_t **result, agent_list_t *loc)
{
	int p = loc->head;

	while (p != -1) {
		agent_t *info = add_agent (wptr, result, &(loc->elements[p].data));
		info->pos.x += offset->x * LOC_SIZE;
		info->pos.y += offset->y * LOC_SIZE;
		p = loc->elements[p].next;
	}
}

static void compile_view (Workspace wptr, mt_cb_t **search, agent_list_t **result)
{
	int i;

	reset_agent_list (*result);

	for (i = 0; i < (NEIGHBOURS + 1); ++i) {
		agent_list_t *agents = NULL;
		mt_cb_t *loc = search[i];

		MTLock (wptr, loc, MT_CB_CLIENT);
		ChanOutInt (wptr, &(loc->channels[0]), LOC_BORROW_INFO);
		MTChanIn (wptr, &(loc->channels[1]), (void *) &agents);
		merge_agents (wptr, &(offsets[i]), result, agents);
		MTChanOut (wptr, &(loc->channels[0]), (void *) &agents);
		MTUnlock (wptr, loc, MT_CB_CLIENT);
	}
}

#define VIEW_STACK 1024
static void view (Workspace wptr)
{
	void *fbar		= ProcGetParam (wptr, 2, void *);
	mt_array_t *sd 		= ProcGetParam (wptr, 1, mt_array_t *);
	mt_cb_t	*svr		= ProcGetParam (wptr, 0, mt_cb_t *);
	mt_cb_t **search	= (mt_cb_t **) sd->data;
	agent_list_t *data	= alloc_agent_list (wptr);
	Channel	*req		= &(svr->channels[0]);
	Channel *resp		= &(svr->channels[1]);
	int running 		= 1;
	int cycle 		= -1;

	while (running) {
		view_req_t msg;

		ChanIn (wptr, req, &msg, sizeof (msg));
		if (msg.type == VIEW_BORROW_VIEW) {
			if (cycle != msg.cycle) {
				compile_view (wptr, search, &data);
				cycle = msg.cycle;
			}
			MTChanOut (wptr, resp, (void *) &data);
			MTChanIn (wptr, req, (void *) &data);
		} else if (msg.type == VIEW_SHUTDOWN) {
			running = 0;
		}
	}

	MRelease (wptr, data);
	MTRelease (wptr, sd);
	MTRelease (wptr, svr);
	MTRelease (wptr, fbar);
}

static mt_array_t *build_search (Workspace wptr, mt_cb_t *self, mt_cb_t **neighbours)
{
	const int cb_type 	= MT_SIMPLE | MT_MAKE_TYPE (MT_CB) | MT_CB_SHARED;
	mt_array_t *sd		= MTAllocArray (wptr, cb_type, 1, (NEIGHBOURS + 1));
	mt_cb_t **search 	= (mt_cb_t **) sd->data;
	int i;
	
	for (i = 0; i < NEIGHBOURS; ++i) {
		search[i] = MTClone (wptr, neighbours[i]);
	}
	search[NEIGHBOURS] = MTClone (wptr, self);

	return sd;
}

static void redirect_agent (Workspace wptr, mt_cb_t **neighbours, Channel *resp, agent_t *info)
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

	ChanOutInt (wptr, resp, LOC_GO_THERE);
	ChanOut (wptr, resp, info, sizeof (*info));
	MTChanOut (wptr, resp, (void *) &(neighbours[d]));
}

#define LOCATION_STACK 1024
static void location (Workspace wptr)
{
	void 	*fbar	= ProcGetParam (wptr, 3, void *);
	mt_array_t *nd 	= ProcGetParam (wptr, 2, mt_array_t *);
	mt_cb_t	*svr	= ProcGetParam (wptr, 1, mt_cb_t *);
	int	loc	= ProcGetParam (wptr, 0, int);

	agent_list_t *state	= alloc_agent_list (wptr);
	mt_cb_t **neighbours	= (mt_cb_t **) nd->data;
	Channel	*req		= &(svr->channels[0]);
	Channel *resp		= &(svr->channels[1]);
	int running 		= 1;

	mt_cb_t	*view_cb	= MTAllocChanType (wptr, 2, true);
	
	{
		mt_array_t *search	= build_search (wptr, svr, neighbours);
		Workspace ws		= ProcAlloc (wptr, 3, VIEW_STACK);

		ProcMTCopy (wptr, ws, 0, view_cb);
		ProcMTMove (wptr, ws, 1, &search);
		ProcMTCopy (wptr, ws, 2, fbar);

		ProcStart (wptr, ws, view);
		
		MTRelease (wptr, view_cb); /* duplicate reference */
	}

	while (running) {
		int type;
		
		ChanInInt (wptr, req, &type);
		
		if (type == LOC_ENTER) {
			agent_t info;

			ChanIn (wptr, req, &info, sizeof (info));
			
			info.loc = loc;
			if (in_bounds (&(info.pos))) {
				ChanOutInt (wptr, resp, LOC_STAY_HERE);
				ChanOut (wptr, resp, &info, sizeof (info));
				add_agent (wptr, &state, &info);
			} else {
				redirect_agent (wptr, neighbours, resp, &info);
			}
		} else if (type == LOC_MOVE) {
			move_req_t update;
			agent_t *info;
			int idx;
			
			ChanIn (wptr, req, &update, sizeof (update));
			
			idx	= find_agent (state, update.id);
			info	= &(state->elements[idx].data);
			vector_add (&(info->pos), &(update.v));
			if (in_bounds (&(info->pos))) {
				ChanOutInt (wptr, resp, LOC_STAY_HERE);
				ChanOut (wptr, resp, info, sizeof (*info));
			} else {
				redirect_agent (wptr, neighbours, resp, info);
				remove_agent (wptr, &state, idx);
			}
		} else if (type == LOC_GET_VIEW) {
			MTChanOut (wptr, resp, (void *) &view_cb);
		} else if (type == LOC_BORROW_INFO) {
			MTChanOut (wptr, resp, (void *) &state);
			MTChanIn (wptr, req, (void *) &state);
		} else if (type == LOC_SHUTDOWN) {
			view_req_t vr;

			vr.type = VIEW_SHUTDOWN;
			MTLock (wptr, view_cb, MT_CB_CLIENT);
			ChanOut (wptr, &(view_cb->channels[0]), &vr, sizeof (vr));
			MTUnlock (wptr, view_cb, MT_CB_CLIENT);
			
			running = 0;
		}
	}

	MTRelease (wptr, view_cb);
	MTRelease (wptr, nd);
	MRelease (wptr, state);
	MTRelease (wptr, svr);
	MTRelease (wptr, fbar);
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
static void agent (Workspace wptr)
{
	void	*fbar	= ProcGetParam (wptr, 5, void *);
	void 	*b	= ProcGetParam (wptr, 4, void *);
	mt_cb_t	*loc	= ProcGetParam (wptr, 3, mt_cb_t *);
	int	cycle	= 0;

	agent_t info;
	mt_cb_t *view = NULL;
	int persona, resp;

	info.id	 	= ProcGetParam (wptr, 0, int);
	info.loc	= -1;
	info.pos.x	= ProcGetParam (wptr, 1, int);
	info.pos.y	= ProcGetParam (wptr, 2, int);
	persona		= info.id * 37;

	printf ("%d %d start\n", cycle, info.id);

	MTSync (wptr, b);

	MTLock (wptr, loc, MT_CB_CLIENT);
	ChanOutInt (wptr, &(loc->channels[0]), LOC_ENTER);
	ChanOut (wptr, &(loc->channels[0]), &info, sizeof (info));
	ChanInInt (wptr, &(loc->channels[1]), &resp);
	assert (resp == LOC_STAY_HERE);
	ChanIn (wptr, &(loc->channels[1]), &info, sizeof (info));
	ChanOutInt (wptr, &(loc->channels[0]), LOC_GET_VIEW);
	MTChanIn (wptr, &(loc->channels[1]), (void *) &view);
	MTUnlock (wptr, loc, MT_CB_CLIENT);

	MTSync (wptr, b);

	while (cycle < N_CYCLES) {
		agent_list_t *agents = NULL;
		move_req_t mr;
		view_req_t vr;
		int idx, px, py;
		
		mr.id	= info.id;
		mr.v.x	= 0;
		mr.v.y	= 0;

		persona = (persona & 65535) * info.pos.x;
		persona = (persona & 65535) * info.pos.y;
		persona = (persona & 65535) * info.loc;

		//printf ("%d %d at %d:%d,%d\n", cycle, info.id, info.loc, info.pos.x, info.pos.y);
		
		cycle++;

		vr.type	= VIEW_BORROW_VIEW;
		vr.cycle = cycle;

		MTLock (wptr, view, MT_CB_CLIENT);
		ChanOut (wptr, &(view->channels[0]), &vr, sizeof (vr));
		MTChanIn (wptr, &(view->channels[1]), (void *) &agents);

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
				mr.v.x += (f * dx) / r;
				mr.v.y += (f * dy) / r;
			} else {
				int x = (cycle & 1) * (-1);
				int y = ((cycle >> 1) & 1) * (-1);
				mr.v.x += f * x;
				mr.v.y += f * y;
			}
			
			Reschedule (wptr);
		}

		MTChanOut (wptr, &(view->channels[0]), (void *) &agents);
		MTUnlock (wptr, view, MT_CB_CLIENT);

		MTSync (wptr, b);

		idx = 1;
		do {
			MTLock (wptr, loc, MT_CB_CLIENT);
			if (idx == 1) {
				ChanOutInt (wptr, &(loc->channels[0]), LOC_MOVE);
				ChanOut (wptr, &(loc->channels[0]), &mr, sizeof (mr));
			} else {
				ChanOutInt (wptr, &(loc->channels[0]), LOC_ENTER);
				ChanOut (wptr, &(loc->channels[0]), &info, sizeof (info));
			}
			ChanInInt (wptr, &(loc->channels[1]), &resp);
			if (resp == LOC_STAY_HERE) {
				ChanIn (wptr, &(loc->channels[1]), &info, sizeof (info));
				if (idx > 1) {
					MTRelease (wptr, view);
					view = NULL;
					ChanOutInt (wptr, &(loc->channels[0]), LOC_GET_VIEW);
					MTChanIn (wptr, &(loc->channels[1]), (void *) &view);
				}
				MTUnlock (wptr, loc, MT_CB_CLIENT);
				idx = 0;
			} else if (resp == LOC_GO_THERE) {
				mt_cb_t *new_loc = NULL;
				ChanIn (wptr, &(loc->channels[1]), &info, sizeof (info));
				MTChanIn (wptr, &(loc->channels[1]), (void *) &new_loc);
				MTUnlock (wptr, loc, MT_CB_CLIENT);
				MTRelease (wptr, loc);
				loc = new_loc;
				idx++;
			}
		} while (idx);
		
		MTSync (wptr, b);
	}

	MTSync (wptr, b);

	printf ("%d %d at %d:%d,%d\n", cycle, info.id, info.loc, info.pos.x, info.pos.y);
	printf ("%d %d end %d\n", cycle, info.id, persona);

	MTRelease (wptr, view);
	MTRelease (wptr, loc);
	MTRelease (wptr, b);
	MTRelease (wptr, fbar);
}

static int wrap (int x, int max)
{
	while (x < 0)
		x += max;
	return x % max;
}

static mt_array_t *build_neighbours (Workspace wptr, int loc, mt_cb_t **world)
{
	const int cb_type = MT_SIMPLE | MT_MAKE_TYPE (MT_CB) | MT_CB_SHARED;
	mt_array_t *nd	= MTAllocArray (wptr, cb_type, 1, NEIGHBOURS);
	mt_cb_t **na	= (mt_cb_t **) nd->data;
	int x		= loc % world_size;
	int y		= loc / world_size;
	int i;

	for (i = 0; i < NEIGHBOURS; ++i) {
		int n_x = wrap (x + offsets[i].x, world_size);
		int n_y	= wrap (y + offsets[i].y, world_size);
		int n_loc = (n_y * world_size) + n_x;

		na[i] = (mt_cb_t *) MTClone (wptr, world[n_loc]);
	}

	return nd;
}

static void proc_main (Workspace wptr)
{
	const int cb_type 	= MT_SIMPLE | MT_MAKE_TYPE (MT_CB) | MT_CB_SHARED;
	int world_area		= world_size * world_size;
	mt_array_t *world_data	= MTAllocArray (wptr, cb_type, 1, world_area);
	mt_cb_t **world		= (mt_cb_t **) world_data->data;
	void *b, *afbar, *lfbar;
	int i, j, id;

	b	= MTAlloc (wptr, MT_MAKE_BARRIER (MT_BARRIER_FULL), 0);
	afbar	= MTAlloc (wptr, MT_MAKE_BARRIER (MT_BARRIER_FORKING), 0);
	lfbar	= MTAlloc (wptr, MT_MAKE_BARRIER (MT_BARRIER_FORKING), 0);

	for (i = 0; i < world_area; ++i)
		world[i] = MTAllocChanType (wptr, 2, true);
	
	for (i = 0; i < world_area; ++i) {
		mt_array_t *neighbours = build_neighbours (wptr, i, world);
		Workspace ws = ProcAlloc (wptr, 4, LOCATION_STACK);

		ProcParam (wptr, ws, 0, i);
		ProcMTCopy (wptr, ws, 1, world[i]);
		ProcMTMove (wptr, ws, 2, &neighbours);
		ProcMTCopy (wptr, ws, 3, lfbar);
		ProcStart (wptr, ws, location);
	}

	for (i = 0, id = 1; i < world_area; ++i) {
		for (j = 0; j < loc_agents; ++j, ++id) {
			int o = LOC_MIN + ((LOC_SIZE / (loc_agents + 4)) * (j + 2));
			Workspace ws = ProcAlloc (wptr, 6, AGENT_STACK);

			ProcParam (wptr, ws, 0, id);
			ProcParam (wptr, ws, 1, o);
			ProcParam (wptr, ws, 2, o);
			ProcMTCopy (wptr, ws, 3, world[i]);
			ProcMTCopy (wptr, ws, 4, b);
			ProcMTCopy (wptr, ws, 5, afbar);

			ProcStart (wptr, ws, agent);
		}
	}

	MTRelease (wptr, b);
	MTSync (wptr, afbar);
	
	for (i = 0; i < world_area; ++i) {
		MTLock (wptr, world[i], MT_CB_CLIENT);
		ChanOutInt (wptr, &(world[i]->channels[0]), LOC_SHUTDOWN);
		MTUnlock (wptr, world[i], MT_CB_CLIENT);
	}

	MTRelease (wptr, world_data);
	MTSync (wptr, lfbar);

	fflush (stdout);

	Shutdown (wptr);
}

int main (int argc, char *argv[])
{
	Workspace p;

	if (argc >= 2)
		world_size = atoi (argv[1]);
	if (argc >= 3)
		loc_agents = atoi (argv[2]);

	if (!ccsp_init ())
		return 1;

	p = ProcAllocInitial (0, 1024 * 1024);
	ProcStartInitial (p, proc_main);

	return 0;
}
