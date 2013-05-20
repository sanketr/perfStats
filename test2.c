#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include "timer.h"
#include "a.h"
#include "t.h"


/**
** WARNING: This main function measures performance - don't optimize in gcc beyond O1 
    otherwise you might get incorrect results. For an optimized function that you are 
    measuring, compile it in a separate file with the relevant optimization flags, and 
    call it here - see digits.c included here for an example of how to do it. 
    Keep in mind that if your function is not doing any relevant work (for example, a 
    malloc testing function that just allocates and frees), aggressive optimization 
    passes in compiler will likely remove these calls during dead code elimination pass. 
    So, be careful about the optimizing flags you set for functions to be measured.
**/


int main(){
  //calculate median of rdtsc call overhead - we will subtract it from all the tests we do
  int64_t overhead = calc_rdtsc_overhead();
  printf("Median Clock Measurement Overhead: %ld Cycles\n",overhead);
  uint64_t SIZE = 1024*1024;
  int64_t* array = malloc(SIZE*sizeof(int64_t));
  for(int i=0; i < SIZE; ++i) array[i] = i;
  uint32_t TRIALS=1000000; //measure 1M times - set it to lower value if your function is slow - the faster your function, more measurements you need - in my experiments, for fastest functions, values seem to converge within 1M iterations
  measure_time(CPP Binary Search,TRIALS,bin_long_cpp(array,SIZE,23456),overhead); //CPP binary search
  measure_time(C Binary Search,TRIALS,bin_long_c(array,SIZE,23456),overhead); //C binary search
}
