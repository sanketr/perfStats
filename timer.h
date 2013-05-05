#include<inttypes.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_sort.h>

uint64_t calc_rdtsc_overhead();
 
inline unsigned long long __attribute__((always_inline)) rdtsc64() {
  unsigned int hi, lo;
  __asm__ __volatile__(
      "xorl %%eax, %%eax\n\t"
      "cpuid\n\t"
      "rdtsc"
      : "=a"(lo), "=d"(hi)
      : /* no inputs */
      : "rbx", "rcx");
  return ((unsigned long long)hi << 32ull) | (unsigned long long)lo;
}

//macro to generate binary search function for any basic C type
#define SORT_INIT(name, input_t, len_t) \
static len_t bin_##name (const input_t* input, len_t len, input_t key){ \
  len_t start=0, end=len; \
  len_t middle; \
  while (start < end){ \
      middle = (start + end)/2; \
      if(key > input[middle]) \
        start = middle + 1; \
      else end = middle; \
  } \
  return start; \
}


//declare long version of binary search function - we need it for
//measure_time macro below
SORT_INIT(long, int64_t, uint64_t);
//the macro below uses do-while trick to make sure a macro declaration 
//with semi-colon is well-formed - with curly braces, you can't have 
//"{a;b;};". You can have "do{a;b;}while(0);"
#define measure_time(NAME,TRIALS,FUNCTION,OVERHEAD) \
  do{ \
    int64_t median, min, max, start, end; \
    double mean, sd, out1, out2; \
    int64_t* times = calloc(TRIALS, sizeof(int64_t)); \
    for(int64_t i=0; i < TRIALS; i++){ \
      start = rdtsc64(); \
      FUNCTION; \
      end = rdtsc64(); \
      times[i] = end - start - OVERHEAD; \
    } \
    gsl_sort_long((long*)times,1,TRIALS); \
    median = gsl_stats_long_median_from_sorted_data ((long*)times,1,TRIALS); \
    mean = gsl_stats_long_mean((long*)times, 1, TRIALS); \
    sd = gsl_stats_long_sd((long*)times, 1, TRIALS); \
    max = times[TRIALS-1]; \
    min = times[0]; \
    out1 = (100.0*(TRIALS - bin_long(times,TRIALS,(uint64_t)(mean + 2*sd))))/TRIALS; \
    out2 = (100.0*bin_long(times,TRIALS,(uint64_t)mean - 2*sd))/TRIALS; \
    printf(#NAME"  | Median: %lld | Mean: %6.3f | Std Deviation: %6.3f | Min: %lld | Max: %lld | Upper Outliers: %6.3f%% | Lower Outliers: %6.3f%%\n",  median,mean,sd,min,max,out1,out2); \
    free(times); \
  }while(0) \

