#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include "digits.h"
 
uint32_t digits10_slow(uint64_t v){
  uint32_t result = 0;
  do{
    v /= 10;
    ++result;
  }while(v);
  return result;
}
 
uint32_t digits10_fast(uint64_t v){
  uint32_t result = 0;
  for(;;){
    if (v < 10) return result + 1;
    if (v < 100) return result + 2;
    if (v < 1000) return result + 3;
    if (v < 10000) return result + 4;
    v /=10000; result +=4;
  }
  return result;
}
 
uint32_t digits10_faster(uint64_t v) {
  if (v < 10) return 1;
  if (v < 100) return 2;
  if (v < 1000) return 3;
  if (v < 1000000000000) {
    if (v < 100000000) {
      if (v < 1000000) {
        if (v < 10000) return 4;
        return 5 + (v >= 100000);
      }
      return 7 + (v >= 10000000);
    }
    if (v < 10000000000) {
      return 9 + (v >= 1000000000);
    }
    return 11 + (v >= 100000000000);
  }
  return 12 + digits10_faster(v / 1000000000000);
}

uint64_t smallFunction(uint64_t x){
  return x;
}
