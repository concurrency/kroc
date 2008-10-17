
import stackless, sys

elements = 256

def element(this, next):
	while 1:
		token = this.receive()
		if token > 0:
			next.send(token + 1)
		else:
			next.send(token)
			return

def root(cycles, this, next):
	next.send(1)
	token = this.receive()

	sys.stdout.write("start\n")
	sys.stdout.flush()
	while cycles:
		next.send(token + 1)
		token = this.receive()
		cycles = cycles - 1
	sys.stdout.write("end\n")
	sys.stdout.flush()

	sys.stdout.write(str(token) + "\n")

	next.send(0)
	token = this.receive()

def ring(args):
	global elements
	cycles = 0
	if len(args) > 0:
		cycles = int(args[0])

	head = stackless.channel()
	this = head
	for i in range(elements - 1):
		next = stackless.channel()
		stackless.tasklet(element)(this, next)
		this = next
	
	stackless.tasklet(root)(cycles, this, head)

	try:
	        stackless.run()
	except TaskletExit:
		pass

if __name__ == "__main__":
	ring(sys.argv[1:])

