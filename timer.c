#include "timer.h"
#include<stdio.h>
#include<stdlib.h>
 
uint64_t calc_rdtsc_overhead() {
  uint32_t TRIALS = 1000000;
  uint64_t* times = calloc(TRIALS,sizeof(uint64_t));
 
  uint64_t start=0, end=0;
  for(int i = 0; i < TRIALS; i++){
    start = rdtsc64();
    end = rdtsc64();
    times[i] = (end -start)>0?(end - start):0;
  }
  gsl_sort_ulong((unsigned long*)times,1,TRIALS);
#ifdef DEBUG
  uint64_t median = gsl_stats_ulong_median_from_sorted_data ((unsigned long*)times,1,TRIALS);
  double mean = gsl_stats_ulong_mean(times, 1, TRIALS);
  double sd = gsl_stats_ulong_sd(times, 1, TRIALS);
  uint64_t max = times[TRIALS-1];
#endif
  uint64_t min = times[0];
  free(times);
#ifdef DEBUG
  printf("| Median: %lu | Mean: %6.3f | Std Deviation: %6.3f | Min: %lu | Max: %lu |\n",median,mean,sd,min,max);
#endif
  return min; 
}
