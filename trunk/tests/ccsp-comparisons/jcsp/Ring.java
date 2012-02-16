
import org.jcsp.lang.*;

public class Ring {
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

	private static void root (int cycles, ChannelInputInt curr, ChannelOutputInt next)
	{
		int token;

		next.write (1);
		token = curr.read ();

		System.out.println ("start");
		System.out.flush ();
		while (cycles > 0) {
			next.write (token + 1);
			token = curr.read ();
			cycles--;
		}
		System.out.println ("end");
		System.out.flush ();

		System.out.println ("" + token);

		next.write (0);
		token = curr.read ();
	}

	public static void main (String[] args)
	{
		int cycles = 0;

		if (args.length > 0)
			cycles = Integer.parseInt (args[0]);

		One2OneChannelInt head = Channel.one2oneInt();
		One2OneChannelInt curr = head;

		for (int i = 0; i < ELEMENTS - 1; ++i)
		{
			One2OneChannelInt next = Channel.one2oneInt();
			(new ProcessManager (new Element (curr.in (), next.out ()))).start();
			curr = next;
		}

		root(cycles, curr.in (), head.out ());
	}
}
