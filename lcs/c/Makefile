uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

ifeq ($(uname_S),Linux)
        GCCFLAGS = -I/efs/dist/fsf/gsl/1.12/common/include -L/efs/dist/fsf/gsl/1.12/exec/gcc411/lib
        DYLIBFLAGS = -shared -fPIC
endif
ifeq ($(uname_S),Darwin)
        DYLIBFLAGS = -bundle -undefined dynamic_lookup -m32
endif


all: install
 
install:
	gcc -O3 miller.c -std=c99 -Wall -g -c -DMILLER_COMPILE
	gcc -O2 timer.c -std=c99 -Wall -g -c $(GCCFLAGS) -Wno-unused-function
	gcc -O2 test.c miller.o timer.o -std=c99 -Wall -o lcs -g $(GCCFLAGS) -lgsl -lgslcblas

qlib:
	gcc -O2 lcs.c utils.c -std=c99 -Wall $(DYLIBFLAGS) -o lcs.so
 
clean:
	rm -rf *.o *.so lcs *dSYM
