import math, stackless, sys

n_cycles = 256

world_size = 5
world_area = world_size * world_size

loc_size = 4096
loc_area = loc_size * loc_size
loc_agents = 12
loc_max = (loc_size // 2) - 1
loc_min = - (loc_size // 2)

offsets = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1), (0,0)]
offset_dict = {}

LOC_ENTER = 0
LOC_MOVE = 1
LOC_BORROW_INFO = 2
LOC_SHUTDOWN = 3

LOC_STAY_HERE = 0
LOC_GO_THERE = 1

VIEW_BORROW_VIEW = 0
VIEW_SHUTDOWN = 1

BAR_SYNC     = 0
BAR_ENROLL   = 1
BAR_RESIGN   = 2
BAR_SHUTDOWN = 3
class Barrier:
	def __init__(self):
		self.req = stackless.channel()
		self.resp = stackless.channel()
		stackless.tasklet(self.control)(self.req, self.resp)

	def control(self, req, resp):
		enrolled = 0
		count = 0
		running = True
		while running:
			msg = req.receive()
			if msg[0] == BAR_SYNC:
				if count > 1:
					count = count - 1
				else:
					resp.send_sequence(range(enrolled))
					count = enrolled
			elif msg[0] == BAR_ENROLL:
				enrolled = enrolled + msg[1]
				count    = count + msg[1]
			elif msg[0] == BAR_RESIGN:
			        enrolled = enrolled - msg[1]
				count    = count - msg[1]
				if count <= 0:
					resp.send_sequence(range(enrolled))
					count = enrolled
			elif msg[0] == BAR_SHUTDOWN:
				running = False
	
	def sync(self):
		self.req.send((BAR_SYNC,))
		self.resp.receive()
	def enroll(self, n):
		self.req.send((BAR_ENROLL, n))
	def resign(self, n):
		self.req.send((BAR_RESIGN, n))
	def shutdown(self):
		self.req.send((BAR_SHUTDOWN,))


def build_offset_dict():
	d = 0
	for o in offsets:
		offset_dict[o]	= d
		d		= d + 1

def vector_add(a, b):
	return (a[0] + b[0], a[1] + b[1])
def vector_sub(a, b):
	return (a[0] - b[0], a[1] - b[1])
def make_nv(v):
	return (v[0] * loc_size, v[1] * loc_size)

def in_bounds(p):
	if (p[0] < loc_min) or (p[0] > loc_max):
		return False
	elif (p[1] < loc_min) or (p[1] > loc_max):
		return False
	else:
		return True

def dir_value(n):
	if n < loc_min:
		return -1
	elif n > loc_max:
		return 1
	else:
		return 0

def merge_agents(agent_list, v, agents):
	for (id, loc, pos) in agents.values():
		agent_list.append((id, loc, vector_add(pos, v)))

def view(req_chan, search):
	running   = True
	cycle     = -1
	cache     = []
	resp_chan = stackless.channel()
	while running:
		msg = req_chan.receive()
		req = msg[0]

		if req == VIEW_BORROW_VIEW:
			resp = msg[1]
			req_cycle = msg[2]
			if req_cycle != cycle:
				cycle = req_cycle
				cache = []
				for (o, loc) in zip(offsets, search):
					nv = make_nv(o)
					loc.send((LOC_BORROW_INFO, resp_chan))
					agents = resp_chan.receive()
					merge_agents(cache, nv, agents)
					resp_chan.send(agents)
			resp.send(cache)
			cache = resp.receive()
		elif req == VIEW_SHUTDOWN:
			running = False

def redirect_agent(resp, neighbours, info):
	pos       = info[2]
	n_offset  = (dir_value(pos[0]), dir_value(pos[1]))
	direction = offset_dict[n_offset]
	neighbour = neighbours[direction]
	pos       = vector_sub(pos, make_nv(n_offset))
	resp.send((LOC_GO_THERE, neighbour, (info[0], info[1], pos)))

def location(loc, req_chan, neighbours):
	agents = {}
	running	= True
	
	view_chan = stackless.channel()
	stackless.tasklet(view)(view_chan, neighbours + [req_chan])

	while running:
		# msg = (Request, ...)
		msg  = req_chan.receive()
		req  = msg[0]

		if req == LOC_ENTER:
			# msg = (Request, Response, Info)
			# info = (ID, Location, (X, Y)
			resp    = msg[1]
			info	= msg[2]
			id	= info[0]
			if in_bounds(info[2]):
				info = (id, loc, info[2])
				agents[id] = info
				resp.send((LOC_STAY_HERE, info, view_chan))
			else:
				redirect_agent(resp, neighbours, info)
		elif req == LOC_MOVE:
			# msg = (Request, Response, ID, Vector)
			resp    = msg[1]
			id	= msg[2]
			vector	= msg[3]
			info	= agents[id]
			pos	= vector_add (info[2], vector)
			info	= (id, loc, pos)
			if in_bounds(pos):
				agents[id] = info
				resp.send((LOC_STAY_HERE, info, view_chan))
			else:
				agents.pop(id)
				redirect_agent(resp, neighbours, info)
		elif req == LOC_BORROW_INFO:
			# msg = (Request, Response)
			resp    = msg[1]
			resp.send(agents)
			agents = resp.receive()
		elif req == LOC_SHUTDOWN:
			view_chan.send((VIEW_SHUTDOWN,))
			running = False

def quot(x, d):
	r = x / float(d)
	if r < 0.0:
		return int(math.ceil(r))
	else:
		return int(math.floor(r))

def a_sqrt(x, r):
	last_r = 0
	step   = 0
	while (last_r != r) and (step < 16):
		last_r = r
		r      = (r + (x // r)) >> 1
		step   = step + 1
	return r

def agent_start(cycle, info):
	print str(cycle) + " " + str(info[0]) + " start"
def agent_at(cycle, info):
	print str(cycle) + " " + str(info[0]) + " at " + str(info[1]) + ":" + str(info[2][0]) + "," + str(info[2][1])
def agent_end(cycle, info, persona):
	print str(cycle) + " " + str(info[0]) + " end " + str(persona)

def agent(cycle, info, loc, bar):
	persona = info[0] * 37
	resp 	= stackless.channel()

	agent_start(cycle, info)

	bar.sync()

	loc.send((LOC_ENTER, resp, info))
	msg  = resp.receive()
	assert (msg[0] == LOC_STAY_HERE)
	info = msg[1]
	view = msg[2]

	bar.sync()

	while cycle < n_cycles:
		persona = (persona & 65535) * info[2][0]
		persona = (persona & 65535) * info[2][1]
		persona = (persona & 65535) * info[1]
		
		#agent_at(cycle, info)
		
		cycle   = cycle + 1

		view.send((VIEW_BORROW_VIEW, resp, cycle))
		agents = resp.receive()
		
		persona = persona + len(agents)
		px	= persona & 255
		py	= (persona >> 8) & 255
		force	= [0, 0]
		
		for agent in agents:
			if agent[0] == info[0]:
				continue

			d   = vector_sub(info[2], agent[2])
			dx  = quot((d[0] * (px + 1)), 128)
			dy  = quot((d[1] * (py + 1)), 128)
			dx2 = dx * dx
			dy2 = dy * dy
			r2  = dx2 + dy2
			f   = min(loc_size, (3 * loc_area) // (r2 + 1))
			r   = 1

			if dx2 > dy2:
				r = a_sqrt (r2, abs(dx))
			elif dy2 > 0:
				r = a_sqrt (r2, abs(dy))

			if r2 != 0:
				force[0] = force[0] + quot((f * dx), r)
				force[1] = force[1] + quot((f * dy), r)
			else:
				x = (cycle & 1) * (-1)
				y = ((cycle >> 1) & 1) * (-1)
				force[0] = force[0] + (f * x)
				force[1] = force[1] + (f * y)
		resp.send(agents)
		bar.sync()

		loc.send((LOC_MOVE, resp, info[0], tuple(force)))

		moving = True
		while moving:
			msg = resp.receive()
			if msg[0] == LOC_STAY_HERE:
				info   = msg[1]
				view   = msg[2]
				moving = False
			elif msg[0] == LOC_GO_THERE:
				loc    = msg[1]
				info   = msg[2]
				loc.send((LOC_ENTER, resp, info))
		
		bar.sync()
	
	bar.sync()
	agent_at(cycle, info)
	agent_end(cycle, info, persona)

def neighbour_offset(loc, o):
	x = ((loc % world_size) + o[0]) % world_size
	y = ((loc // world_size) + o[1]) % world_size
	return (y * world_size) + x

def add_agents(id, loc, bar):
	for i in range(loc_agents):
		o = loc_min + ((loc_size // (loc_agents + 4)) * (i + 2))
		info = (id + i, -1, (o, o))
		stackless.tasklet(agent)(0, info, loc, bar)
	return (id + loc_agents)

def agents():
	build_offset_dict()
	bar = Barrier()
	bar.enroll(world_area * loc_agents)
	world = [ stackless.channel() for i in range(world_area) ]
	id = 1
	for loc in range(world_area):
		neighbours = [ world[neighbour_offset(loc, o)] for o in offsets ]
		stackless.tasklet(location)(loc, world[loc], neighbours)
		id = add_agents(id, world[loc], bar)
	try:
	        stackless.run()
	except TaskletExit:
		pass

if __name__ == "__main__":
	agents()

