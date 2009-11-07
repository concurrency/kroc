
import java.util.concurrent.*;

public class MTRing {
	private static int ELEMENTS = 256;

	private static int readOrDie (BlockingQueue<Integer> q)
	{
		try {
			return q.take ();
		} catch (Exception e) {
			throw new Error (e);
		}
	}

	private static void writeOrDie (BlockingQueue<Integer> q, int i)
	{
		try {
			q.put (i);
		} catch (Exception e) {
			throw new Error (e);
		}
	}

	private static class Element implements Runnable {
		private final BlockingQueue<Integer> curr;
		private final BlockingQueue<Integer> next;

		public Element (BlockingQueue<Integer> curr, BlockingQueue<Integer> next)
		{
			this.curr = curr;
			this.next = next;
		}

		public void run ()
		{
			for (;;) {
				int token = readOrDie (curr);
				if (token > 0) {
					writeOrDie (next, token + 1);
				} else {
					writeOrDie (next, token);
					return;
				}
			}
		}
	}

	private static void root (int cycles, int tokens, BlockingQueue<Integer> curr, BlockingQueue<Integer> next)
	{
		int token;

		writeOrDie (next, 1);
		token = readOrDie (curr);

		System.out.println ("start");
		System.out.flush ();
		
		for (int i = 1; i <= tokens; ++i)
			writeOrDie (next, i);

		while (cycles > 0) {
			for (int i = 0; i < tokens; ++i) {
				token = readOrDie (curr);
				writeOrDie (next, token + 1);
			}
			cycles--;
		}

		int sum = 0;
		for (int i = 0; i < tokens; ++i)
			sum += readOrDie (curr);

		System.out.println ("end");
		System.out.flush ();

		System.out.println ("" + sum);

		writeOrDie (next, 0);
		token = readOrDie (curr);
	}

	public static void main (String[] args)
	{
		int cycles = 0, tokens = 1;

		if (args.length > 0)
			cycles = Integer.parseInt (args[0]);
		if (args.length > 1)
			tokens = Integer.parseInt (args[1]);

		BlockingQueue<Integer> head = new SynchronousQueue<Integer> ();
		BlockingQueue<Integer> curr = head;

		for (int i = 0; i < ELEMENTS - 1; ++i)
		{
			BlockingQueue<Integer> next = new SynchronousQueue<Integer> ();
			(Executors.defaultThreadFactory ().newThread (
				new Element (curr, next)
			)).start ();
			curr = next;
		}

		root (cycles, tokens, curr, head);
	}
}
