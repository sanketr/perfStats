#include <stdio.h>
#include "miller.h"

static void inline __attribute__((always_inline)) insertSnakes(int4v* a,const size_t p, const size_t x, const size_t y, const size_t l){
  snakes tmp = {p,x,y,l};
  int4vinsertv(a,tmp);
}

static void inline __attribute__((always_inline)) insertTest(int4v* a, const size_t count){
  size_t i=0;
  for(;i<count;i++){insertSnakes(a,i,i,i,i);}
}

//TODO: Generic vector wrapper for a,b, function ptr f, size_t k,ct
/**
void inline __attribute__((always_inline)) fsnakes(const a,const b,const k,f,const ct){
  


}
**/

int main(int argc, char** argv){

  int4v a;
  int4vinitv(&a, 16);
  printf("Capacity of array: %d\n", a.cap);
  int i=0;
  insertTest(&a, 27);
  for(i=0;i<a.size;i++) printf("Array element %d -> %d\n", i, a.arr[i].x);
  printf("Capacity of array: %d\n", a.cap);
  int4vfreev(&a);
  printf("Capacity of array: %d\n", a.cap);
  return 0;
}
