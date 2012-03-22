OUT=schedlib
LEVEL=0

all:
	gcc -I. -c schedlib.c -DNODEBUG -o ${OUT}.o -lpthread

debug:
	gcc -I. -c schedlib.c -DLEVEL=${LEVEL} -o ${OUT}.o -lpthread

clean:
	rm -f ${OUT}.o
