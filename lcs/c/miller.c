#include <stdio.h>
#include "miller.h"

//TODO: Must be thread-safe - no global variables

static size_t* til(size_t num) { size_t* idx = malloc(num*(sizeof(size_t))); DO(num,idx[i]=i); return idx;}

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
    snodes[k+offset] = -1 + snakearr->size;
    if(vert) k+=1;
    else k-=1; 
  }
}

static vec* lcsh(vec a, vec b,size_t (*cmp)(vec,vec,size_t,size_t)){
  size_t n = a.size, m = b.size, delta = m-n, offset = n+1, p=-1 ,ct=0, k=0;
  size_t* snodes = malloc((m+n+3)*sizeof(size_t)); 
  size_t* fp = malloc((m+n+3)*sizeof(size_t)); 
  for(int i=0;i<m+n+3;i++){ snodes[i]=-1;fp[i]=-1;}
  int4v* snakearr;
  int4vinit(snakearr,(m+n+1)); 
  while(fp[delta+offset] < m){
    p += 1;
    ct = delta+p; k = -1*p;
    fsnakes(a,b,snakearr,fp,snodes,k,cmp,ct);
    ct = p; k = delta+p;
    fsnakes(a,b,snakearr,fp,snodes,k,cmp,ct);
    fsnakes(a,b,snakearr,fp,snodes,delta,cmp,1);
  }
  //TODO: iterate through snakes, and fill in paths int2v vector

  free(fp); free(snodes);
  int4vfree(snakearr);
  return NULL;
} 


//flip a nested vector of vectors - size 2 
static void inline __attribute__((always_inline)) flip(void** vec){
    void* tmp;
    if(vec == NULL) return;
    else{
      tmp=*(vec+1);
      *(vec+1)=*(vec+0);
      *(vec+0)=tmp;
    }
}

vec* lcs(vec a, vec b, size_t  (*cmp)(vec,vec,size_t,size_t)){
  bool flipped = a.size > b.size ? 1:0;
  vec* res;
  if(flipped) res=lcsh(b,a,cmp);
  else res=lcsh(a,b,cmp); 
  if(flipped) flip((void**)&res);
  return res; 
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
