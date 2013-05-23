#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "miller.h"
//#include "timer.h"

int main(int argc, char** argv){
  vec* b=malloc(2*sizeof(vec));
  size_t size1=1000;
  if(argc>1) size1 = atoi(argv[1]);
  size_t size2=size1 + size1/10;
  char* i1 = (char*) malloc(size1*sizeof(char));
  char* i2 = (char*) malloc(size2*sizeof(char));
  for(int i=0;i<size1;i++) i1[i]=(char)i%128;
  for(int i=0;i<size2;i++) i2[i]=(char)(i+10)%128;

  b[0].size=size1;
  b[0].vec = i1;
  b[1].size=size2;
  b[1].vec = i2;
  //int64_t overhead = calc_rdtsc_overhead();
  //printf("Median Clock Measurement Overhead: %ld Cycles\n",overhead);
  //uint32_t TRIALS=1;
  vec* res;
  //measure_time(LCS,TRIALS,{res=lcs(b[0],b[1],chrcmp);for(size_t i=0;i<2;i++){vecfree(res[i]);}free(res);},overhead);
  res=lcs(b[0],b[1],chrcmp);
  for(size_t i=0;i<2;i++){vecfree(res[i]);}
  free(res);
 /**
  size_t* idx;
  int k=10;
  while(0<k--){
    vec* res = lcs(b[0],b[1],chrcmp);
    idx = (size_t*) res[0].vec;
    for(size_t i=0;i< res[0].size;i++) printf("%c",( (char*) b[0].vec)[idx[i]]);
    printf("\n");
    idx = res[1].vec;
    for(size_t i=0;i< res[1].size;i++) printf("%c",( (char*) b[1].vec)[idx[i]]);
    printf("\n");
    for(size_t i=0;i<2;i++){vecfree(res[i]);}
    free(res);
  }
 **/

  for(size_t i=0;i<2;i++){vecfree(b[i]);}
  free(b);
  return 0;
}

