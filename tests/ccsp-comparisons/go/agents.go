package main

import (
	"fmt";
	"flag";
	"os";
	"runtime";
	"strconv";
)

const (
	N_CYCLES	= 1024;
	WORLD_SIZE	= 5;

	LOC_SIZE	= 4096;
	LOC_AREA	= (LOC_SIZE * LOC_SIZE);
	LOC_AGENTS	= 12;

	LOC_MAX	= ((LOC_SIZE / 2) - 1);
	LOC_MIN	= (-(LOC_SIZE / 2));
)

type Vector struct {
	x, y int;
}

type AgentInfo struct {
	id	int;
	loc	int;
	pos	Vector;
}

type AgentList []*AgentInfo
type AgentMap map[int]*AgentInfo

const (
	LOC_ENTER	= iota;
	LOC_MOVE;
	LOC_GET_VIEW;
	LOC_BORROW_INFO;
	LOC_SHUTDOWN;
	LOC_STAY_HERE;
	LOC_GO_THERE;
)

type LocationResp struct {
	cmd	int;
	loc	chan *LocationReq;
	view	chan *ViewReq;
	agents	*AgentMap;
}

type LocationReq struct {
	cmd	int;
	v	*Vector;
	info	*AgentInfo;
	resp	chan *LocationResp;
}

type ViewReq struct {
	cycle	int;
	resp	chan *AgentList;
}

const NEIGHBOURS = 8

var offsets [9]Vector = [9]Vector{
	Vector{-1, -1},
	Vector{ 0, -1},
	Vector{ 1, -1},
	Vector{-1,  0},
	Vector{ 1,  0},
	Vector{-1,  1},
	Vector{ 0,  1},
	Vector{ 1,  1},
	Vector{ 0,  0},
}

func (v *Vector) vector_add(a *Vector) {
	v.x = v.x + a.x;
	v.y = v.y + a.y;
}

func (v *Vector) vector_sub(a *Vector) {
	v.x = v.x - a.x;
	v.y = v.y - a.y;
}

func dir_value(n int) int {
	if n < LOC_MIN {
		return -1
	} else if n > LOC_MAX {
		return 1
	}
	return 0;
}

func (p Vector) in_bounds() bool {
	if p.x < LOC_MIN || p.x > LOC_MAX {
		return true
	}
	return !(p.y < LOC_MIN || p.y > LOC_MAX);
}

func a_sqrt(x, r int) int {
	last_r := 0;
	steps := 0;
	for (r != last_r) && (steps < 16) {
		last_r, r, steps = r, (r+(x/r))>>1, steps+1
	}
	return r;
}

type Barrier chan BarrierReq
type BarrierReq chan int
type BarrierEnd struct {
	barrier	Barrier;
	resp	BarrierReq;
}

func barrier_runner(b Barrier, count int) {
	waiting_list := make([]BarrierReq, count);
	for {
		for n := 0; n < count; n = n + 1 {
			rc := <-b;
			if rc != nil {
				waiting_list[n] = rc
			} else {
				return
			}
		}
		for n := 0; n < count; n = n + 1 {
			waiting_list[n] <- n
		}
	}
}

func new_barrier(count int) Barrier {
	b := make(Barrier, count / 16);
	go barrier_runner(b, count);
	return b;
}

func new_barrier_end(b Barrier) BarrierEnd {
	return BarrierEnd{b, make(BarrierReq)}
}

func (b *BarrierEnd) sync() {
	b.barrier <- b.resp;
	<-b.resp;
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x;
}

func agent_start(log chan string, cycle int, info *AgentInfo) {
	log <- fmt.Sprintf("%d %d start\n", cycle, info.id)
}
func agent_at(log chan string, cycle int, info *AgentInfo) {
	log <- fmt.Sprintf("%d %d at %d:%d,%d\n", cycle, info.id, info.loc, info.pos.x, info.pos.y)
}
func agent_end(log chan string, cycle, persona int, info *AgentInfo) {
	log <- fmt.Sprintf("%d %d end %d\n", cycle, info.id, persona)
}

func agent(cycle int, info AgentInfo, loc chan *LocationReq, bar BarrierEnd, log chan string) {
	persona := info.id * 37;
	loc_resp := make(chan *LocationResp);
	view_resp := make(chan *AgentList);

	agent_start(log, cycle, &info);

	bar.sync();

	loc <- &LocationReq{LOC_ENTER, nil, &info, loc_resp};
	msg := <-loc_resp;
	view_req := msg.view;

	bar.sync();

	for cycle < N_CYCLES {
		persona = (persona & 65535) * info.pos.x;
		persona = (persona & 65535) * info.pos.y;
		persona = (persona & 65535) * info.loc;

		// agent_at (log, cycle, &info);

		cycle = cycle + 1;

		view_req <- &ViewReq{cycle, view_resp};
		agents := <-view_resp;

		persona = persona + len(*agents);
		px	:= persona & 0xff;
		py	:= (persona >> 8) & 0xff;
		force	:= Vector{0, 0};

		for _, oa := range *agents {
			if oa.id == info.id {
				continue
			}

			d := info.pos;
			d.vector_sub(&oa.pos);
			d.x = (d.x * (px + 1)) / 128;
			d.y = (d.y * (py + 1)) / 128;
			dx2 := d.x * d.x;
			dy2 := d.y * d.y;
			r2 := dx2 + dy2;
			f := (3*(LOC_SIZE*LOC_SIZE)) / (r2+1);
			if f > LOC_SIZE {
				f = LOC_SIZE;
			}

			r := 1;
			if dx2 > dy2 {
				r = a_sqrt(r2, abs(d.x))
			} else if dy2 > 0 {
				r = a_sqrt(r2, abs(d.y))
			}

			if r2 != 0 {
				force.x = force.x + ((f * d.x) / r);
				force.y = force.y + ((f * d.y) / r);
			} else {
				x_jitter := (cycle & 1) * (-1);
				y_jitter := ((cycle >> 1) & 1) * (-1);
				force.x = force.x + (f * x_jitter);
				force.y = force.y + (f * y_jitter);
			}
		}

		bar.sync();

		loc <- &LocationReq{LOC_MOVE, &force, &info, loc_resp};
		msg := <-loc_resp;

		switch msg.cmd {
		case LOC_STAY_HERE:
			/* nop */
		case LOC_GO_THERE:
			loc = msg.loc;
			for moving := true; moving; {
				loc <- &LocationReq{LOC_ENTER, nil, &info, loc_resp};
				msg := <-loc_resp;

				switch msg.cmd {
				case LOC_STAY_HERE:
					view_req = msg.view;
					moving = false;
				case LOC_GO_THERE:
					loc = msg.loc
				}
			}
		}

		bar.sync();
	}

	bar.sync();

	agent_at(log, cycle, &info);
	agent_end(log, cycle, persona, &info);

	bar.sync();
}

func viewer(req chan *ViewReq, fov []chan *LocationReq) {
	cycle := -1;
	view := make(AgentList, 0, 64);
	loc_resp := make(chan *LocationResp);

	done := false;
	for !done {
		msg := <-req;
		if msg.cycle >= 0 {
			if msg.cycle != cycle {
				view = view[0:0];
				for dir, loc := range (fov) {
					offset := Vector{LOC_SIZE * offsets[dir].x, LOC_SIZE * offsets[dir].y};
					loc <- &LocationReq{LOC_BORROW_INFO, nil, nil, loc_resp};
					msg := <-loc_resp;
					if len(view)+len(*msg.agents) > cap(view) {
						new_view := make(AgentList, len(view), (len(view)+len(*msg.agents))*2);
						for i, a := range view {
							new_view[i] = a
						}
						view = new_view;
					}
					view_len := len(view);
					view = view[0:(view_len + len(*msg.agents))];
					for _, a := range *msg.agents {
						agent := *a;
						agent.pos.vector_add(&offset);
						view[view_len] = &agent;
						view_len = view_len + 1;
					}
					loc_resp <- msg;
				}
				cycle = msg.cycle;
			}
			msg.resp <- &view;
		} else {
			done = true
		}
	}
}

func (info *AgentInfo) handle_update() (bool, int) {
	dx := dir_value(info.pos.x);
	dy := dir_value(info.pos.y);
	if (dx != 0) || (dy != 0) {
		for d, v := range offsets {
			if (dx == v.x) && (dy == v.y) {
				info.pos.x = info.pos.x - (v.x * LOC_SIZE);
				info.pos.y = info.pos.y - (v.y * LOC_SIZE);
				return true, d;
			}
		}
	}
	return false, 0;
}

func location(loc int, req chan *LocationReq, neighbours []chan *LocationReq) {
	view := make(chan *ViewReq, 16);
	go viewer(view, neighbours);

	agents := make(AgentMap, 16);

	done := false;
	for !done {
		msg := <-req;
		switch msg.cmd {
		case LOC_ENTER:
			moved, dir := msg.info.handle_update();
			if moved {
				msg.resp <- &LocationResp{LOC_GO_THERE, neighbours[dir], nil, nil}
			} else {
				info := msg.info;
				info.loc = loc;
				agents[info.id] = info;
				msg.resp <- &LocationResp{LOC_STAY_HERE, nil, view, nil};
			}
		case LOC_MOVE:
			info := msg.info;
			info.pos.vector_add(msg.v);
			moved, dir := info.handle_update();
			if moved {
				agents[info.id] = nil, false;
				msg.resp <- &LocationResp{LOC_GO_THERE, neighbours[dir], nil, nil};
			} else {
				msg.resp <- &LocationResp{LOC_STAY_HERE, nil, view, nil}
			}
		case LOC_BORROW_INFO:
			msg.resp <- &LocationResp{0, nil, nil, &agents};
			<-msg.resp;
		case LOC_SHUTDOWN:
			done = true
		}
	}

	view <- &ViewReq{-1, nil};
}

func wrap(n, max int) int {
	for n < 0 {
		n = n + max
	}
	return n % max;
}

func agents(world_size, loc_agents int, screen chan string) {
	world_area	:= world_size * world_size;
	world		:= make([]chan *LocationReq, world_area);

	for loc := 0; loc < world_area; loc = loc + 1 {
		world[loc] = make(chan *LocationReq, 16)
	}
	for loc := 0; loc < world_area; loc = loc + 1 {
		this_loc	:= world[loc];
		x		:= loc % world_size;
		y		:= loc / world_size;
		neighbours	:= make([]chan *LocationReq, NEIGHBOURS + 1);
		for i := 0; i < NEIGHBOURS; i = i + 1 {
			n_x := wrap(x+offsets[i].x, world_size);
			n_y := wrap(y+offsets[i].y, world_size);
			neighbours[i] = world[(n_y*world_size)+n_x];
		}
		neighbours[NEIGHBOURS] = this_loc;
		go location(loc, this_loc, neighbours);
	}

	b := new_barrier((loc_agents * world_area) + 1);

	id := 1;
	for loc := 0; loc < world_area; loc = loc + 1 {
		for p := 0; p < loc_agents; p = p + 1 {
			x := LOC_MIN + ((LOC_SIZE / (loc_agents + 4)) * (p + 2));
			y := LOC_MIN + ((LOC_SIZE / (loc_agents + 4)) * (p + 2));
			info := AgentInfo{id, loc, Vector{x, y}};
			go agent(0, info, world[loc], new_barrier_end(b), screen);
			id = id + 1;
		}
	}

	be := new_barrier_end(b);
	for cycle := 0; cycle <= N_CYCLES; cycle = cycle + 1 {
		be.sync();
		be.sync();
	}
	be.sync();
	be.sync();
	b <- nil;

	for loc := 0; loc < world_area; loc = loc + 1 {
		world[loc] <- &LocationReq{LOC_SHUTDOWN, nil, nil, nil}
	}
}

func screen_writer(in chan string) {
	for {
		str := <-in;
		if len(str) == 0 {
			return
		}
		os.Stdout.WriteString(str)
	}
}

func main() {
	flag.Parse();

	threads, _ := strconv.Atoi(os.Getenv("CORES"));
	if threads < 1 {
		threads = 1
	}
	runtime.GOMAXPROCS(threads);

	world_size := WORLD_SIZE;
	if flag.NArg() >= 1 {
		world_size, _ = strconv.Atoi(flag.Arg(0))
	}
	loc_agents := LOC_AGENTS;
	if flag.NArg() >= 2 {
		loc_agents, _ = strconv.Atoi(flag.Arg(1))
	}

	screen := make(chan string, 16);
	go screen_writer(screen);
	agents(world_size, loc_agents, screen);
	screen <- ""
}
