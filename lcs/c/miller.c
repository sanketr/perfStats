#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>  
#include "miller.h"

//Thread-safe - no global, or local static variables

void vecfree(vec v){ free(v.vec); v.vec=NULL; v.size=0; return;}

size_t chrcmp(vec a,vec b,size_t i,size_t j){
  size_t i1=i;
  char* a1 = (char*)a.vec;
  char* b1 = (char*)b.vec;
  while((i<a.size) && (j<b.size) && a1[i] == b1[j]){i++;j++;}
  return i-i1;
}

//Note: fp, snodes are constant of size m+n+3
void inline __attribute__((always_inline)) fsnakes(const vec a,const vec b,int4v* snakearr, int64_t* fp,int64_t* snodes, size_t k,size_t (*cmp)(vec,vec,size_t,size_t),size_t ct){
  size_t n = a.size, m = b.size, offset = n+1;
  size_t kp;
  int64_t xp,yp,x,y;
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
    int4vinsert(snakearr,(snakes){snodes[kp],xp,yp,(size_t)(x-xp)});
    snodes[k+offset] = -1 + snakearr->size;
    if(vert) k+=1;
    else k-=1; 
  }
}

static inline vec* lcsh(const vec a,const vec b,size_t (*cmp)(vec,vec,size_t,size_t)){
  size_t n = a.size, m = b.size, delta = m-n, offset = n+1;
  assert(m >= n); //delta must be positive - otherwise result is bad
  int64_t p=-1, ct=0, k=0;
  int64_t* snodes = (int64_t*) malloc((m+n+3)*sizeof(int64_t)); 
  int64_t* fp = (int64_t*) malloc((m+n+3)*sizeof(int64_t)); 
  for(int i=0;i<m+n+3;i++){ snodes[i]=-1;fp[i]=-1;}
  int4v snakearr;
  int4vinit(&snakearr,(m+n+1)); 
  while(fp[delta+offset] < (int64_t)m){
    p += 1;
    ct = delta+p; k = -1*p;
    fsnakes(a,b,&snakearr,fp,snodes,k,cmp,ct);
    ct = p; k = delta+p;
    fsnakes(a,b,&snakearr,fp,snodes,k,cmp,ct);
    fsnakes(a,b,&snakearr,fp,snodes,delta,cmp,1);
  }
  //length of LCS is n-p - so, need only those many indices
  size_t* ax = malloc((n-p)*sizeof(size_t));
  size_t* by = malloc((n-p)*sizeof(size_t));
  size_t i = -1 + snakearr.size;
  size_t j = n-p-1;
  snakes* snakesv = snakearr.arr;
  while(snakesv[i].p > -1){
    if(snakesv[i].len > 0){
      //insert into x,y size_t arrays corresponding to a and b
      //insert backwards, starting from end of the arrays
      for(size_t k=0; k < snakesv[i].len; k++){
        ax[k+j-snakesv[i].len+1] = snakesv[i].x + k;
        by[k+j-snakesv[i].len+1] = snakesv[i].y + k;
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
  free(fp); free(snodes);
  int4vfree(&snakearr);
  return res;
} 

//flip a nested vector of vectors - works only for size 2 
static void inline __attribute__((always_inline)) flip(vec* vect){
  vec tmp;
  if(vect == NULL) return;
  else{
    tmp=vect[1];
    vect[1]=vect[0];
    vect[0]=tmp;
  }
}

vec* lcs(const vec a,const vec b, size_t  (*cmp)(vec,vec,size_t,size_t)){
  bool flipped = a.size > b.size ? 1:0;
  vec* res;
  if(flipped){res=lcsh(b,a,cmp);flip(res);}
  else res=lcsh(a,b,cmp); 
  return res; 
}
