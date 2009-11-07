
import org.jcsp.lang.*;

public class MTRing {
	private static int ELEMENTS = 256;

	private static class Element implements CSProcess {
		private final ChannelInputInt curr;
		private final ChannelOutputInt next;

		public Element (ChannelInputInt curr, ChannelOutputInt next)
		{
			this.curr = curr;
			this.next = next;
		}

		public void run ()
		{
			for (;;) {
				int token = curr.read ();
				if (token > 0) {
					next.write (token + 1);
				} else {
					next.write (token);
					return;
				}
			}
		}
	}

	private static void root (int cycles, int tokens, ChannelInputInt curr, ChannelOutputInt next)
	{
		int token;

		next.write (1);
		token = curr.read ();

		System.out.println ("start");
		System.out.flush ();
		
		for (int i = 1; i <= tokens; ++i)
			next.write (i);

		while (cycles > 0) {
			for (int i = 0; i < tokens; ++i) {
				token = curr.read ();
				next.write (token + 1);
			}
			cycles--;
		}

		int sum = 0;
		for (int i = 0; i < tokens; ++i)
			sum += curr.read ();

		System.out.println ("end");
		System.out.flush ();

		System.out.println ("" + sum);

		next.write (0);
		token = curr.read ();
	}

	public static void main (String[] args)
	{
		int cycles = 0, tokens = 1;

		if (args.length > 0)
			cycles = Integer.parseInt (args[0]);
		if (args.length > 1)
			tokens = Integer.parseInt (args[1]);

		One2OneChannelInt head = Channel.one2oneInt();
		One2OneChannelInt curr = head;

		for (int i = 0; i < ELEMENTS - 1; ++i)
		{
			One2OneChannelInt next = Channel.one2oneInt();
			(new ProcessManager (new Element (curr.in (), next.out ()))).start();
			curr = next;
		}

		root(cycles, tokens, curr.in (), head.out ());
	}
}
