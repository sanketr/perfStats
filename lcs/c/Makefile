uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

ifeq ($(uname_S),Linux)
        GCCFLAGS = -I/efs/dist/fsf/gsl/1.12/common/include -L/efs/dist/fsf/gsl/1.12/exec/gcc411/lib
endif


all: install
 
install:
	gcc -O3  v.c -c -g -Wall
	gcc -O3 miller.c -std=c99 -Wall -g -c -DMILLER_COMPILE
	gcc -O2 timer.c -std=c99 -Wall -g -c $(GCCFLAGS) 
	gcc -O2 test.c miller.o timer.o -std=c99 -Wall -o lcs -g $(GCCFLAGS) -lgsl -lgslcblas
 
clean:
	rm -rf *.o lcs *dSYM
