#include <stdio.h>
#include "miller.h"

//TODO: Must be thread-safe - no global variables

static void inline __attribute__((always_inline)) inssnakes(int4v* a,const size_t p, const size_t x, const size_t y, const size_t l){
  int4vinsert(a,(snakes){p,x,y,l});
}

static void inline __attribute__((always_inline)) insertTest(int4v* a, const size_t count){
  size_t i=0;
  for(;i<count;i++){inssnakes(a,i,i+1,i+2,i+3);}
}

size_t inline __attribute__((always_inline)) charcomp(vec a,vec b,size_t i,size_t j){
  size_t i1=i;
  char* a1 = (char*)a.vec;
  char* b1 = (char*)b.vec;
  while((i<a.size) && (j<b.size) && a1[i] == b1[j]){i++;j++;}
  return i-i1;
}

//Note: fp, snodes are constant of size m+n+3
void inline __attribute__((always_inline)) fsnakes(vec a,vec b,int4v* snakearr, size_t* fp,size_t* snodes, size_t k,size_t (*cmp)(vec,vec,size_t,size_t),size_t ct){
  size_t n = a.size, m = b.size, offset = n+1;
  size_t kp,xp,yp,x,y;
  bool vert = k < (m-n) ? 1:0; //vertical if below diagonal, horizontal otherwise
  for(;0<ct;ct--){
    if(fp[k+offset-1] + 1 > fp[k+offset+1]){
        kp = k+offset-1;
        yp = fp[kp]+1;
    }
    else{
        kp = k+offset+1;
        yp = fp[kp];
    }
    y=yp;x=y-k;xp=x;
    x += cmp(a,b,x,y);
    y += x-xp; //x-xp=cmp(a,b,x,y)
    fp[k+offset] = y;
    int4vinsert(snakearr,(snakes){snodes[kp],xp,yp,(x-xp)});
    snodes[k+offset] = -1 + int4vsize(snakearr);
    if(vert) k+=1;
    else k-=1; 
  }
}

//TODO: Generic vector wrapper for a,b, function ptr f, size_t k,ct
/**
void inline __attribute__((always_inline)) fsnakes(const a,const b,const k,f,const ct){
  


}
**/

/** 
   Takes a,b, function ptr f, returns indices in a and b for common lcs
** index lcs(a,b,f) 

**/

int main(int argc, char** argv){

  int4v a;
  int4vinit(&a, 16);
  printf("Capacity of array: %d\n", a.cap);
  size_t i=0;
  insertTest(&a, 22);
  for(i=0;i<a.size;i++) printf("Array element %d -> %d\n", i, a.arr[i].len);
  printf("Capacity of array: %d\n", a.cap);
  int4vfree(&a);
  printf("Capacity of array after deleting: %d\n", a.cap);
  return 0;
}
