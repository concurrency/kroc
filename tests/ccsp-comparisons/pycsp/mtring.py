
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

def root(cycles, tokens, this, next):
	next.write(1)
	token = this.read()

	sys.stdout.write("start\n")
	sys.stdout.flush()
	
	for i in range(tokens):
		next.write(i + 1)

	while cycles:
		for i in range(tokens):
			token = this.read()
			next.write(token + 1)
		cycles = cycles - 1
	
	sum = 0
	for i in range(tokens):
		sum += this.read()

	sys.stdout.write("end\n")
	sys.stdout.flush()

	sys.stdout.write(str(sum) + "\n")

	next.write(0)
	token = this.read()

def ring(args):
	global elements
	cycles = 0
	tokens = 1
	if len(args) > 0:
		cycles = int(args[0])
	if len(args) > 1:
		tokens = int(args[1])

	head = One2OneChannel()
	this = head
	for i in range(elements - 1):
		next = One2OneChannel()
		Spawn(element(this, next))
		this = next
	
	root(cycles, tokens, this, head)

if __name__ == "__main__":
	ring(sys.argv[1:])

