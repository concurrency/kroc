
import java.util.concurrent.*;
import java.util.*;

public class Agents {
	private static final int N_CYCLES = 1024;
	
	private static final int WORLD_SIZE = 5;

	private static final int LOC_SIZE = 4096;
	private static final int LOC_AREA = LOC_SIZE * LOC_SIZE;
	private static final int LOC_AGENTS = 12;
	private static final int LOC_MAX = (LOC_SIZE / 2) - 1;
	private static final int LOC_MIN = -(LOC_SIZE / 2);

	private static final int[][] offsets = {
		{ -1, -1 }, { 0, -1 }, { 1, -1 },
		{ -1,  0 },            { 1,  0 },
		{ -1,  1 }, { 0,  1 }, { 1,  1 },
		
		{  0,  0 }
	};

	private static void waitOrDie (CyclicBarrier b)
	{
		try {
			b.await ();
		} catch (Exception e) {
			throw new Error (e);
		}
	}

	private static Object readOrDie (BlockingQueue q)
	{
		try {
			return q.take ();
		} catch (Exception e) {
			throw new Error (e);
		}
	}

	private static Requester readReqOrDie (BlockingQueue<Requester> q)
	{
		try {
			return q.take ();
		} catch (Exception e) {
			throw new Error (e);
		}
	}
	
	private static void writeOrDie (BlockingQueue q, Object o)
	{
		try {
			q.put (o);
		} catch (Exception e) {
			throw new Error (e);
		}
	}
	
	private static void writeReqOrDie (BlockingQueue<Requester> q, Requester o)
	{
		try {
			q.put (o);
		} catch (Exception e) {
			throw new Error (e);
		}
	}

	private static class AgentInfo {
		public final int id;
		public int x;
		public int y;

		public AgentInfo (int id, int x, int y)
		{
			this.id = id;
			this.x = x;
			this.y = y;
		}

		public void plusOffset (int d)
		{
			x += offsets[d][0] * LOC_SIZE;
			y += offsets[d][1] * LOC_SIZE;
		}
		
		public void minusOffset (int d)
		{
			x -= offsets[d][0] * LOC_SIZE;
			y -= offsets[d][1] * LOC_SIZE;
		}

		public AgentInfo clone ()
		{
			return new AgentInfo (id, x, y);
		}

		public String toString ()
		{
			return "[" + id + " " + x + "," + y + "]";
		}
	}

	private static interface Requester {
		public int 	getCMD ();
		public Object 	read ();
		public void	write (Object o);
	}

	private static class CommandDummy implements Requester {
		private final int cmd;

		public CommandDummy (int cmd)
		{
			this.cmd = cmd;
		}

		public int getCMD ()
		{
			return cmd;
		}

		public Object read ()
		{
			return null;
		}

		public void write (Object o)
		{
			/* NOP */
		}
	}

	private static class View implements Requester, Runnable {
		public static final int SHUTDOWN = -1;
		
		private final BlockingQueue internal_req;
		private final BlockingQueue resp;
		public final BlockingQueue<Requester> req;
		private int cmd;

		private int cycle;
		private ArrayList<AgentInfo> agents;
		private final Location[] search;

		public View (Location[] search)
		{
			this.cycle	= -1;
			this.agents	= new ArrayList<AgentInfo> ();
			this.search	= search;

			this.internal_req = new SynchronousQueue ();
			this.resp	= new ArrayBlockingQueue (1);
			this.req	= new ArrayBlockingQueue<Requester> (128);

			(Executors.defaultThreadFactory ().newThread (this)).start ();
		}
		
		public int getCMD ()
		{
			return cmd;
		}

		public Object read ()
		{
			return readOrDie (internal_req);
		}

		public void write (Object o)
		{
			writeOrDie (resp, o);
		}

		private void buildView (int newCycle)
		{
			cycle = newCycle;
			agents.clear ();

			for (int d = 0; d < search.length; ++d) {
				Location l = search[d];
				this.cmd = Location.BORROW_INFO;
				writeReqOrDie (l.req, this);
				HashSet la = (HashSet) readOrDie (this.resp);
				for (Object a : la) {
					Agent agent = (Agent) a;
					AgentInfo info = agent.info.clone ();
					info.plusOffset (d);
					agents.add (info);
				}
				writeOrDie (this.internal_req, this);
			}
		}

		public void run ()
		{
			boolean running = true;
			while (running) {
				Requester req = readReqOrDie (this.req);
				int reqCycle = req.getCMD ();
				if (reqCycle == SHUTDOWN) {
					running = false;
				} else {
					if (reqCycle != cycle)
						buildView (reqCycle);
					req.write (agents);
				}
			}
		}
	}

	private static class Location implements Runnable {
		public static final int ENTER = 0;
		public static final int MOVE = 1;
		public static final int GET_VIEW = 2;
		public static final int BORROW_INFO = 3;
		public static final int SHUTDOWN = 4;
		public static final int STAY_HERE = 5;
		public static final int GO_THERE = 6;

		public final BlockingQueue init;
		public final BlockingQueue<Requester> req;

		private final int loc;
		private final HashSet<Agent> agents;
		private int cmd;

		public Location (int loc)
		{
			this.loc	= loc;
			this.agents	= new HashSet<Agent> ();
			this.init	= new SynchronousQueue ();
			this.req	= new ArrayBlockingQueue<Requester> (128);
			
			(Executors.defaultThreadFactory ().newThread (this)).start ();
		}
		

		private static int dirValue (int n)
		{
			if (n < LOC_MIN)
				return -1;
			else if (n > LOC_MAX)
				return 1;
			else
				return 0;
		}
		
		private static boolean inBounds (AgentInfo p)
		{
			if (p.x < LOC_MIN || p.x > LOC_MAX)
				return false;
			else
				return !(p.y < LOC_MIN || p.y > LOC_MAX);
		}

		private void redirectAgent (Location[] neighbours, Agent agent)
		{
			int dx = dirValue (agent.info.x);
			int dy = dirValue (agent.info.y);
			int d = 0;

			while (offsets[d][0] != dx || offsets[d][1] != dy)
				d++;

			agent.info.minusOffset (d);

			agent.write (new Integer (GO_THERE));
			agent.write (neighbours[d]);
		}

		public void run ()
		{
			final Location[] neighbours = (Location[]) readOrDie (init);
			final View view;
			{
				final int n = neighbours.length;
				Location[] search = new Location[n + 1];
				for (int i = 0; i < n; ++i) {
					search[i] = neighbours[i];
				}
				search[n] = this;
				view = new View (search);
			}

			boolean running = true;
			while (running) {
				Requester req = readReqOrDie (this.req);
				switch (req.getCMD ()) {
					case ENTER:
						{
							Agent agent = (Agent) req;
							if (inBounds (agent.info)) {
								agents.add (agent);
								agent.write (new Integer (STAY_HERE));
							} else {
								redirectAgent (neighbours, agent);
							}
						}
						break;
					case MOVE:
						{
							Agent agent = (Agent) req;
							if (inBounds (agent.info)) {
								agent.write (new Integer (STAY_HERE));
							} else {
								agents.remove (agent);
								redirectAgent (neighbours, agent);
							}
						}
						break;
					case GET_VIEW:
						req.write (view);
						break;
					case BORROW_INFO:
						req.write (agents);
						req.read ();
						break;
					case SHUTDOWN:
						writeReqOrDie (view.req, new CommandDummy (View.SHUTDOWN));
						running = false;
						break;
				}
			}
		}
	}

	private static class Agent implements Requester, Runnable {
		public final 	int 		id;
		private 	Location 	loc;
		public final 	AgentInfo 	info;
		private 	int 		persona;
		private final 	CyclicBarrier 	b;
		private 	int 		cmd;
		private final	BlockingQueue	resp;

		public Agent (int id, int x, int y, Location start, CyclicBarrier b)
		{
			this.id		= id;
			this.loc	= start;
			this.info	= new AgentInfo (id, x, y);
			this.persona	= id * 37;
			this.b		= b;
			this.cmd	= 0;
			this.resp	= new ArrayBlockingQueue (2);
			
			(Executors.defaultThreadFactory ().newThread (this)).start ();
		}

		public int getCMD ()
		{
			return cmd;
		}

		public Object read ()
		{
			return null;
		}

		public void write (Object o)
		{
			writeOrDie (resp, o);
		}

		private static int aSqrt (final int x, int r)
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

		private void start (int cycle)
		{
			System.out.println ("" + cycle + " " + id + " start");
		}

		private void at (int cycle)
		{
			System.out.println ("" + cycle + " " + id + " at " + loc.loc + ":" + info.x + "," + info.y);
		}

		private void end (int cycle, int persona)
		{
			System.out.println ("" + cycle + " " + id + " end " + persona);
		}

		public void run ()
		{
			View view;
			int cycle = 0;

			start (cycle);

			waitOrDie (b);

			this.cmd = Location.ENTER;
			writeReqOrDie (loc.req, this);
			{
				int msg = (Integer) readOrDie (this.resp);
				assert (msg == Location.STAY_HERE);
			}

			this.cmd = Location.GET_VIEW;
			writeReqOrDie (loc.req, this);
			view = (View) readOrDie (this.resp);

			waitOrDie (b);

			while (cycle < N_CYCLES) {
				persona = (persona & 65535) * info.x;
				persona = (persona & 65535) * info.y;
				persona = (persona & 65535) * loc.loc;

				//at (cycle);

				cycle++;

				this.cmd = cycle;
				writeReqOrDie (view.req, this);
				ArrayList agents = (ArrayList) readOrDie (this.resp);
				
				persona += agents.size ();
				
				int px = persona & 255;
				int py = (persona >> 8) & 255;
				int fx = 0;
				int fy = 0;
				
				for (Object o : agents) {
					final AgentInfo agent = (AgentInfo) o;

					if (agent.id == this.id)
						continue;

					final int _dx = info.x - agent.x;
					final int _dy = info.y - agent.y;

					final int dx = (_dx * (px + 1)) / 128;
					final int dy = (_dy * (py + 1)) / 128;

					final int dx2 = dx * dx;
					final int dy2 = dy * dy;
					final int r2 = dx2 + dy2;
					
					final int f = Math.min (LOC_SIZE, (3 * LOC_AREA) / (r2 + 1));
					
					int r = 1;

					if (dx2 > dy2)
						r = aSqrt (r2, Math.abs (dx));
					else if (dy2 > 0)
						r = aSqrt (r2, Math.abs (dy));

					if (r2 != 0) {
						fx += (f * dx) / r;
						fy += (f * dy) / r;
					} else {
						int x = (cycle & 1) * (-1);
						int y = ((cycle >> 1) & 1) * (-1);
						fx += f * x;
						fy += f * y;
					}
				}

				waitOrDie (b);

				info.x += fx;
				info.y += fy;

				Location new_loc = null;
				boolean moving = true;
				do {
					if (new_loc != null)
						loc = new_loc;

					if (new_loc == null) {
						this.cmd = Location.MOVE;
					} else {
						this.cmd = Location.ENTER;
					}
					writeReqOrDie (loc.req, this);

					int msg = (Integer) readOrDie (this.resp);
					if (msg == Location.STAY_HERE) {
						if (new_loc != null) {
							this.cmd = Location.GET_VIEW;
							writeReqOrDie (loc.req, this);
							view = (View) readOrDie (this.resp);
						}
						moving = false;
					} else if (msg == Location.GO_THERE) {
						new_loc = (Location) readOrDie (this.resp);
					}
				} while (moving);

				waitOrDie (b);
			}

			waitOrDie (b);
			
			at (cycle);
			end (cycle, persona);
			
			waitOrDie (b);
		}
	}

	private static int wrap (int x, int max)
	{
		while (x < 0)
			x += max;
		return x % max;
	}

	public static void main (String[] args)
	{
		int world_size = WORLD_SIZE;
		int loc_agents = LOC_AGENTS;

		if (args.length > 0)
			world_size = Integer.parseInt (args[0]);
		if (args.length > 1)
			loc_agents = Integer.parseInt (args[1]);
		
		final int world_area = world_size * world_size;
		
		final Location[] world = new Location[world_area];
		
		for (int loc = 0; loc < world.length; ++loc) {
			world[loc] = new Location (loc);
		}

		for (int loc = 0; loc < world.length; ++loc) {
			final Location[] neighbours = new Location[offsets.length - 1];
			final int x = loc % world_size;
			final int y = loc / world_size;
			for (int d = 0; d < offsets.length - 1; ++d) {
				final int n_x = wrap (x + offsets[d][0], world_size);
				final int n_y = wrap (y + offsets[d][1], world_size);
				final int n_loc = (n_y * world_size) + n_x;
				neighbours[d] = world[n_loc];
			}
			writeOrDie (world[loc].init, neighbours);
		}

		CyclicBarrier b = new CyclicBarrier ((world_area * loc_agents) + 1);

		int id = 1;
		for (int loc = 0; loc < world_area; ++loc) {
			for (int p = 0; p < loc_agents; ++p) {
				final int o = LOC_MIN + ((LOC_SIZE / (loc_agents + 4)) * (p + 2));
				new Agent (id++, o, o, world[loc], b);
			}
		}

		for (int i = 0; i < N_CYCLES + 1; ++i) {
			waitOrDie (b);
			waitOrDie (b);
		}
		
		for (int i = 0; i < world.length; ++i) {
			writeReqOrDie (world[i].req, new CommandDummy (Location.SHUTDOWN));
		}

		waitOrDie (b);
		waitOrDie (b);
		System.out.flush ();
	}
}
