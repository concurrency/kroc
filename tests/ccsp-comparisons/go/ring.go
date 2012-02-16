package main

import (
	"fmt";
	"flag";
	"os";
	"runtime";
	"strconv";
)

const ELEMENTS = 256

func element(this, next chan int) {
	for {
		token := <-this;
		if token > 0 {
			next <- token + 1
		} else {
			next <- token;
			return
		}
	}
}

func root(cycles int, this, next chan int) {
	next <- 1;
	token := <-this;

	os.Stdout.WriteString("start\n");
	for cycles > 0 {
		next <- token + 1;
		token = <-this;
		cycles = cycles - 1
	}
	os.Stdout.WriteString("end\n");

	fmt.Printf("%d\n", token);

	next <- 0;
	<-this;
}

func ring(cycles int) {
	head := make(chan int);
	this := head;

	for i := 0; i < ELEMENTS - 1; i = i + 1 {
		next := make(chan int);
		go element(this, next);
		this = next
	}

	root(cycles, this, head)
}

func main() {
	flag.Parse();

	threads, _ := strconv.Atoi(os.Getenv("CORES"));
	if threads < 1 {
		threads = 1
	}
	runtime.GOMAXPROCS(threads);

	cycles := 0;
	if flag.NArg() >= 1 {
		cycles, _ = strconv.Atoi(flag.Arg(0))
	}

	ring(cycles)
}
