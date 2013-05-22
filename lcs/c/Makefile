uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

ifeq ($(uname_S),Linux)
        GCCFLAGS = -I/efs/dist/fsf/gsl/1.12/common/include -L/efs/dist/fsf/gsl/1.12/exec/gcc411/lib
endif


all: install
 
install:
	gcc -O2 v.c -c -Wall
	gcc -O2 miller.c -std=c99 -Wall -o lcs -g
 
clean:
	rm -rf *.o lcs
