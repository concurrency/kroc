
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

def root(cycles, tokens, this, next):
	next.send(1)
	token = this.receive()

	sys.stdout.write("start\n")
	sys.stdout.flush()
	
	for i in range(tokens):
		next.send(i + 1)

	while cycles:
		for i in range(tokens):
			token = this.receive()
			next.send(token + 1)
		cycles = cycles - 1
	
	sum = 0
	for i in range(tokens):
		sum += this.receive()

	sys.stdout.write("end\n")
	sys.stdout.flush()

	sys.stdout.write(str(sum) + "\n")

	next.send(0)
	token = this.receive()

def ring(args):
	global elements
	cycles = 0
	tokens = 1
	if len(args) > 0:
		cycles = int(args[0])
	if len(args) > 1:
		tokens = int(args[1])

	head = stackless.channel()
	this = head
	for i in range(elements - 1):
		next = stackless.channel()
		stackless.tasklet(element)(this, next)
		this = next
	
	stackless.tasklet(root)(cycles, tokens, this, head)

	try:
	        stackless.run()
	except TaskletExit:
		pass

if __name__ == "__main__":
	ring(sys.argv[1:])

