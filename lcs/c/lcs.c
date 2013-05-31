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
#include <inttypes.h>
#include "lcs.h"
#include "utils.h"


//Thread-safe - no global, or local static variables

static inline I __attribute__((always_inline)) incr(I k){
  return k+1;
}

static inline I __attribute__((always_inline)) decr(I k){
  return k-1;
}

//Note: fp, snodes are constant of size m+n+3 - they represent furthest point, snake nodes
static inline void __attribute__((always_inline)) findsnakes(K a,K b,int4v* snakevec, I* fp,I* snodes, I k,I (*cmp)(K,K,I,I),I ct, I (*op)(I)){
  I offset = 1 + a->n;
  I kp,xp,yp,x,y;
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
    int4vinsert(snakevec,(snakes){snodes[kp],xp,yp,(x-xp)});
    snodes[k+offset] = -1 + snakevec->size;
    k=op(k);
  }
}

static K lcsh(K a,K b,I (*cmp)(K,K,I,I), bool flipped){
  I n = a->n, m = b->n, delta = m-n, offset = n+1, p=0;
  #ifdef DEBUG
  assert(m >= n); //delta must be positive - otherwise result is bad
  #endif
  I* snodes = (I*) malloc((m+n+3)*sizeof(I)); 
  I* fp = (I*) malloc((m+n+3)*sizeof(I)); 
  for(I i=0;i<m+n+3;i++){ snodes[i]=-1;fp[i]=-1;}
  int4v snakevec;
  int4vinit(&snakevec,(m+n+1)); 
  //note since fp is initialized to -1 and m>0, the loop will execute
  //at least once. So, p value is at least 0 since it is decremented 
  //by 1 after exiting the loop
  for(;fp[delta+offset] < m;p++){
    findsnakes(a,b,&snakevec,fp,snodes,-1*p,cmp,delta+p,incr);
    findsnakes(a,b,&snakevec,fp,snodes,delta+p,cmp,p,decr);
    findsnakes(a,b,&snakevec,fp,snodes,delta,cmp,1,decr);
  }
  p-=1;
  //length of LCS is n-p - so, need only those many indices
  K ax = ktn(KI,(n-p));
  K by = ktn(KI,(n-p));
  I i = -1 + snakevec.size;
  I j = n-p;
  snakes* snakesv = snakevec.vec;
  bool done = 0;
  //there will always be one snake since findsnakes executes at least for delta diagonal
  //so need to execute at least once for the case when there is only one snake
  //for(; !done; i=snakesv[i].p,done = snakesv[i].p == -1)
  do{
    if(snakesv[i].len > 0){
      //insert into x,y size_t arrays corresponding to a and b
      //insert backwards, starting from end of the arrays
      for(I l=0; l < snakesv[i].len; l++){
        kI(ax)[l+j-snakesv[i].len] = snakesv[i].x + l;
        kI(by)[l+j-snakesv[i].len] = snakesv[i].y + l;
        }
      j -= snakesv[i].len;
     }
    i=snakesv[i].p;
  }while(i>-1);
  free(snodes);
  free(fp); 
  int4vfree(&snakevec);
  return flipped ? knk(2,by,ax) : knk(2,ax,by);
} 

K lcs(K a,K b){
  bool g=0; 
  if(a->t < 0 || b->t < 0)  return knk(2,ki(ni),ki(ni));
  //check if general list - validate for conforming shape if general list 
  if(a->t == 0){
    if(!validate(a,b)) return knk(2,ki(ni),ki(ni)); //return null if in bad form
    g = 1;
  }
  //if here, both a and b are vectors - check if same types
  if(a->t != b->t) return knk(2,ki(ni),ki(ni));

  if(a->n > b->n) return lcsh(b,a,g?cmpg:cmpv,1);
  else return lcsh(a,b,g?cmpg:cmpv,0); 
}
