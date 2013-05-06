uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

ifeq ($(uname_S),Linux)
        GCCFLAGS = -I/efs/dist/fsf/gsl/1.12/common/include -L/efs/dist/fsf/gsl/1.12/exec/gcc411/lib
endif


all: install
 
install:
	gcc -O3 digits.c -c -Wall -g
	gcc -O2 timer.c -std=c99 -Wall -c -Wno-unused-function -g $(GCCFLAGS)
	gcc -O2 test.c digits.o timer.o -std=c99 -Wall -o test -g $(GCCFLAGS) -Wl,-Bstatic,-lgsl,-lgslcblas -Wl,-Bdynamic,-lm
	g++ -O3 a.cpp -g -c
	gcc -O3 t.c -g -c
	gcc -O2 test2.c t.o a.o timer.o -std=c99 -Wall -o test2 -g $(GCCFLAGS) -Wl,-Bstatic,-lgsl,-lgslcblas -Wl,-Bdynamic,-lm
 
clean:
	rm -rf *.o test test2 *.dSYM
