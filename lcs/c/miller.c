/*
*
* Implementation of "An O(NP) Sequence Comparison Algorithm" by Sun Wu, Udi 
* Manber, Gene Myers and Web Miller
* Note: P=(D-(M-N))/2 where D is shortest edit distance, M,N sequence lengths,
* M >= N
*******************************************************************************
* In this implementation, thread-safety is maintained by using non-static local
* variables. To make the implementation simple while being fast, we use custom
* dynamic array snodes to store the snake paths. Once we reach end of both 
* sequences, we just take the last snake path, and follow it back to its pred-
* -ecessor and so on, until we get to the beginning of the sequence. fp stores
* the furthest-point. snodes stores index of previous snake path in dynamic 
* array snakevec.
*******************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>  
#include "miller.h"

//Thread-safe - no global, or local static variables

void vecfree(vec v){ free(v.vec); v.vec=NULL; v.size=0; return;}

size_t int32cmp(const vec a,const vec b,size_t i,size_t j){
  size_t i1=i;
  int32_t const* a1 = (int32_t const*)a.vec;
  int32_t const* b1 = (int32_t const*)b.vec;
  while((i<a.size) && (j<b.size) && a1[i] == b1[j]){i++;j++;}
  return i-i1;
}

size_t chrcmp(vec a,vec b,size_t i,size_t j){
  size_t i1=i;
  char const* a1 = (char const*)a.vec;
  char const* b1 = (char const*)b.vec;
  while((i<a.size) && (j<b.size) && a1[i] == b1[j]){i++;j++;}
  return i-i1;
}

//Note: fp, snodes are constant of size m+n+3 - they represent furthest point, snake nodes
static inline void __attribute__((always_inline)) findsnakes(vec a,vec b,int4v* snakevec, int64_t* fp,int64_t* snodes, int64_t k,size_t (*cmp)(vec,vec,size_t,size_t),size_t ct){
  size_t n = a.size, m = b.size; 
  int64_t offset = (int64_t) n+1;
  size_t kp;
  int64_t xp,yp,x,y;
  bool vert = (k < (int64_t)(m-n)) ? 1:0; //vertical if below diagonal, horizontal otherwise
  for(;0<ct;ct--){
    #ifdef DEBUG
    assert(((int64_t)k+offset+1) < ((int64_t) m+n+3));
    #endif
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
    int4vinsert(snakevec,(snakes){snodes[kp],xp,yp,(size_t)(x-xp)});
    snodes[k+offset] = -1 + snakevec->size;
    if(vert) k+=1;
    else k-=1; 
  }
}

static inline vec* lcsh(vec a,vec b,size_t (*cmp)(vec,vec,size_t,size_t)){
  size_t n = a.size, m = b.size, delta = m-n, offset = n+1, ct=0;
  int64_t _m = (int64_t) m;
  assert(m >= n); //delta must be positive - otherwise result is bad
  int64_t p=-1, k=0;
  int64_t* snodes = (int64_t*) malloc((m+n+3)*sizeof(int64_t)); 
  int64_t* fp = (int64_t*) malloc((m+n+3)*sizeof(int64_t)); 
  for(int i=0;i<m+n+3;i++){ snodes[i]=-1;fp[i]=-1;}
  int4v snakevec;
  int4vinit(&snakevec,(m+n+1)); 
  while(fp[delta+offset] < _m){
    #ifdef DEBUG
    printf("%" PRId64 " %" PRId64" % "PRId64"\n",fp[delta+offset],_m,p);
    #endif
    p += 1;
    ct = delta+p; k = -1*p;
    findsnakes(a,b,&snakevec,fp,snodes,k,cmp,ct);
    ct = p; k = delta+p;
    findsnakes(a,b,&snakevec,fp,snodes,k,cmp,ct);
    findsnakes(a,b,&snakevec,fp,snodes,delta,cmp,1);
  }
  //length of LCS is n-p - so, need only those many indices
  size_t* ax = malloc((n-p)*sizeof(size_t));
  size_t* by = malloc((n-p)*sizeof(size_t));
  size_t i = -1 + snakevec.size;
  size_t j = n-p;
  snakes* snakesv = snakevec.vec;
  while(snakesv[i].p > -1){
    if(snakesv[i].len > 0){
      //insert into x,y size_t arrays corresponding to a and b
      //insert backwards, starting from end of the arrays
      for(size_t k=0; k < snakesv[i].len; k++){
        ax[k+j-snakesv[i].len] = snakesv[i].x + k;
        by[k+j-snakesv[i].len] = snakesv[i].y + k;
        }
      j -= snakesv[i].len;
    }
    //set i to index of previous node in snake array
    i=snakesv[i].p;
  }
  vec* res=malloc(2*sizeof(vec));
  res[0].size = res[1].size = n-p;
  res[0].vec = ax;
  res[1].vec = by;
  free(snodes);
  free(fp); 
  int4vfree(&snakevec);
  return res;
} 

//flip a vector of vectors - works only for size 2 
static void inline __attribute__((always_inline)) flip(vec* vect){
  vec tmp;
  if(vect == NULL) return;
  else{
    tmp=vect[1];
    vect[1]=vect[0];
    vect[0]=tmp;
  }
}

vec* lcs(vec a,vec b, size_t  (*cmp)(vec,vec,size_t,size_t)){
  bool flipped = a.size > b.size ? 1:0;
  vec* res;
  if(flipped){
    res=lcsh(b,a,cmp);flip(res);
   }
  else res=lcsh(a,b,cmp); 
  return res; 
}
