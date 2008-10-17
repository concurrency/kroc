
import sys
from pycsp import *

elements = 256

@process
def element(this, next):
	while 1:
		token = this.read()
		if token > 0:
			next.write(token + 1)
		else:
			next.write(token)
			return

def root(cycles, this, next):
	next.write(1)
	token = this.read()

	sys.stdout.write("start\n")
	sys.stdout.flush()
	while cycles:
		next.write(token + 1)
		token = this.read()
		cycles = cycles - 1
	sys.stdout.write("end\n")
	sys.stdout.flush()

	sys.stdout.write(str(token) + "\n")

	next.write(0)
	token = this.read()

def ring(args):
	global elements
	cycles = 0
	if len(args) > 0:
		cycles = int(args[0])

	head = One2OneChannel()
	this = head
	for i in range(elements - 1):
		next = One2OneChannel()
		Spawn(element(this, next))
		this = next
	
	root(cycles, this, head)

if __name__ == "__main__":
	ring(sys.argv[1:])

