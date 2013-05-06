#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include "timer.h"
#include "digits.h"
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
  printf("Median Clock Measurement Overhead: %lld Cycles\n",overhead);
  uint64_t testDigit = 1334782398988024;
  uint32_t TRIALS=1000000; //measure 1M times - set it to lower value if your function is slow - the faster your function, more measurements you need - in my experiments, for fastest functions, values seem to converge within 1M iterations
  measure_time(Slow Digits  ,TRIALS,digits10_slow(testDigit),overhead); //default slow digits10
  measure_time(Fast Digits  ,TRIALS,digits10_fast(testDigit),overhead); //fast  digits10
  measure_time(Faster Digits,TRIALS,digits10_faster(testDigit),overhead); //faster digits10 
  measure_time(Small Function,TRIALS,smallFunction(i),overhead); //an example of measuring a very low-overhead function - it should be about 3 cycles on x86_64 (1 push, 1 mov, 1 ret) - we pass it a non-static argument (using loop variable i in measure_time macro body) so that it is not optimized away by the compiler from n calls to 1 call
}
