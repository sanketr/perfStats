all: install
 
install:
	gcc -O2 timer.c -std=c99 -Wall -c -Wno-unused-function
	gcc -O3 digits.c -c -Wall
	gcc -O1 test.c digits.o timer.o -std=c99 -Wall -lgsl -lgslcblas -o test -g
 
clean:
	rm -rf *.o test
