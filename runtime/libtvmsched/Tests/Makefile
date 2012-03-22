PR=4
PS=50
DC=10
WS=5

all:
	./gentest.py ${PR} ${PS} ${DC} ${WS}
	gcc -I. -o SchedTest_${PR}_${PS}_${DC}_${WS}.out schedlib.o SchedTest_${PR}_${PS}_${DC}_${WS}.c -lpthread


one:
	gcc -I. -o test1.out schedlib.o SchedTest_${PR}_${PS}_${DC}_${WS}.c -lpthread

clean:
	rm -f SchedTest_${PR}_${PS}_${DC}_${WS}*

cleanall:
	rm -f SchedTest*
