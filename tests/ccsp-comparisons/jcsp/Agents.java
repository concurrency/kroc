
import java.util.*;
import org.jcsp.lang.*;

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

	private static class View implements CSProcess {
		public static final int SHUTDOWN = -1;
		
		private final AltingChannelInput in;
		private final ChannelOutput out;
		public final SharedChannelOutput req;
		public final SharedChannelInput resp;

		private int cycle;
		private ArrayList<AgentInfo> agents;
		private final Location[] search;

		public View (Location[] search)
		{
			Any2OneChannel svr = Channel.any2one ();
			One2AnyChannel cli = Channel.one2any ();

			this.in		= svr.in ();
			this.req	= svr.out ();
			this.out	= cli.out ();
			this.resp	= cli.in ();

			this.cycle	= -1;
			this.agents	= new ArrayList<AgentInfo> ();
			this.search	= search;
			
			(new ProcessManager (this)).start ();
		}

		private void buildView (int newCycle)
		{
			cycle = newCycle;
			agents.clear ();

			for (int d = 0; d < search.length; ++d) {
				Location l = search[d];
				synchronized (l) {
					l.req.write (new Integer (Location.BORROW_INFO));
					HashSet la = (HashSet) l.resp.read ();
					for (Object a : la) {
						Agent agent = (Agent) a;
						AgentInfo info = agent.info.clone();
						info.plusOffset (d);
						agents.add (info);
					}
					l.req.write (la);
				}
			}
		}

		public void run ()
		{
			boolean running = true;
			while (running) {
				int req = (Integer) in.read ();
				if (req == SHUTDOWN) {
					running = false;
				} else {
					if (req != cycle)
						buildView (req);
					out.write (agents);
				}
			}
		}
	}

	private static class Location implements CSProcess {
		public static final int ENTER = 0;
		public static final int MOVE = 1;
		public static final int GET_VIEW = 2;
		public static final int BORROW_INFO = 3;
		public static final int SHUTDOWN = 4;
		public static final int STAY_HERE = 5;
		public static final int GO_THERE = 6;

		private final AltingChannelInput in;
		private final ChannelOutput out;
		public final SharedChannelOutput req;
		public final SharedChannelInput resp;

		private final int loc;
		private final HashSet<Agent> agents;

		public Location (int loc)
		{
			Any2OneChannel svr = Channel.any2one ();
			One2AnyChannel cli = Channel.one2any ();

			this.in		= svr.in ();
			this.req	= svr.out ();
			this.out	= cli.out ();
			this.resp	= cli.in ();

			this.loc	= loc;
			this.agents	= new HashSet<Agent>();
			
			(new ProcessManager (this)).start ();
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

			out.write (new Integer (GO_THERE));
			out.write (neighbours[d]);
		}

		public void run ()
		{
			final Location[] neighbours = (Location[]) in.read ();
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
				int req = (Integer) in.read ();
				if (req == ENTER) {
					Agent agent = (Agent) in.read ();
					if (inBounds (agent.info)) {
						agents.add (agent);
						out.write (new Integer (STAY_HERE));
					} else {
						redirectAgent (neighbours, agent);
					}
				} else if (req == MOVE) {
					Agent agent = (Agent) in.read ();
					if (inBounds (agent.info)) {
						out.write (new Integer (STAY_HERE));
					} else {
						agents.remove (agent);
						redirectAgent (neighbours, agent);
					}
				} else if (req == GET_VIEW) {
					out.write (view);
				} else if (req == BORROW_INFO) {
					out.write (agents);
					in.read ();
				} else if (req == SHUTDOWN) {
					view.req.write (new Integer (View.SHUTDOWN));
					running = false;
				}
			}
		}
	}
	private static class Agent implements CSProcess {
		public final int id;
		private Location loc;
		public final AgentInfo info;
		private int persona;
		private final Barrier b;

		public Agent (int id, int x, int y, Location start, Barrier b)
		{
			this.id		= id;
			this.loc	= start;
			this.info	= new AgentInfo (id, x, y);
			this.persona	= id * 37;
			this.b		= b;
			(new ProcessManager (this)).start ();
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

			b.sync ();

			synchronized (loc) {
				loc.req.write (new Integer (Location.ENTER));
				loc.req.write (this);
				int msg = (Integer) loc.resp.read ();
				assert (msg == Location.STAY_HERE);
				loc.req.write (new Integer (Location.GET_VIEW));
				view = (View) loc.resp.read ();
			}

			b.sync ();

			while (cycle < N_CYCLES) {
				persona = (persona & 65535) * info.x;
				persona = (persona & 65535) * info.y;
				persona = (persona & 65535) * loc.loc;

				//at (cycle);

				cycle++;

				ArrayList agents;
				synchronized (view) {
					view.req.write (new Integer (cycle));
					agents = (ArrayList) view.resp.read ();
				}
				
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

				b.sync ();

				info.x += fx;
				info.y += fy;

				Location new_loc = null;
				boolean moving = true;
				do {
					if (new_loc != null)
						loc = new_loc;

					synchronized (loc) {
						if (new_loc == null) {
							loc.req.write (new Integer (Location.MOVE));
						} else {
							loc.req.write (new Integer (Location.ENTER));
						}
						loc.req.write (this);

						int msg = (Integer) loc.resp.read ();
						if (msg == Location.STAY_HERE) {
							if (new_loc != null) {
								loc.req.write (new Integer (Location.GET_VIEW));
								view = (View) loc.resp.read ();
							}
							moving = false;
						} else if (msg == Location.GO_THERE) {
							new_loc = (Location) loc.resp.read ();
						}
					}
				} while (moving);

				b.sync ();
			}

			b.sync ();
			
			at (cycle);
			end (cycle, persona);
			
			b.sync ();
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
			world[loc].req.write (neighbours);
		}

		Barrier b = new Barrier ((world_area * loc_agents) + 1);

		int id = 1;
		for (int loc = 0; loc < world_area; ++loc) {
			for (int p = 0; p < loc_agents; ++p) {
				final int o = LOC_MIN + ((LOC_SIZE / (loc_agents + 4)) * (p + 2));
				new Agent (id++, o, o, world[loc], b);
			}
		}

		for (int i = 0; i < N_CYCLES + 1; ++i) {
			b.sync ();
			b.sync ();
		}
		
		for (int i = 0; i < world.length; ++i) {
			synchronized (world[i]) {
				world[i].req.write (new Integer (Location.SHUTDOWN));
			}
		}

		b.sync ();
		b.sync ();
		System.out.flush ();
	}
}
