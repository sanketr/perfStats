#define KXVER 2
#include "k.h"
#include <inttypes.h>

/* Function to validate two nested lists x and y of depth 1 have same types for
** x[0],y[0] etc.
*/
C validate(K x,K y){
  if(x->t!=0 || y->t!=0) R 0;
  I i=0; //workaround to avoid triggering gcc strict-aliasing warning
  //if here, general lists - at least one element each, and of same type
  if(kK(x)[i]->n != kK(y)[i]->n || kK(x)[i]->t != kK(y)[i]->t) R 0;
  if(kK(x)[i]->t == 0){
  //if here, both are general lists with same number of columns - now, we check type of each column
    I j=0;
    C equal = 1;
    for(;j<kK(x)[i]->n;j++){
      equal = equal && kK(kK(x)[i])[j]->t == kK(kK(y)[i])[j]->t;
      }
    R equal;
  }
  else R 1; //not a mixed list - since types and length are equal, return 1
}

K test(K x,K y){
  R ki(validate(x,y));
}

//quick vector equality comparison - assumption is x,y length are very small, typically 2 or 3
static inline uint8_t __attribute__((always_inline)) equalV(K x,K y,I t){
  I i=0;
  uint8_t equal=1;
  switch(t){
    case KB: case KC:
    case KG: for(;i<x->n;i++) equal = equal && (kG(x)[i] == kG(y)[i]);
             break;
    case KH: for(;i<x->n;i++) equal = equal && (kH(x)[i] == kH(y)[i]);
             break;
    case KM: case KD: case KU: case KV: case KT: case KI:
             for(;i<x->n;i++) equal = equal && (kI(x)[i] == kI(y)[i]);
             break;
    case KP: case KN:
    case KJ: for(;i<x->n;i++) equal = equal && (kJ(x)[i] == kJ(y)[i]);
             break;
    case KE: for(;i<x->n;i++) equal = equal && (kE(x)[i] == kE(y)[i]);
             break;
    case KF: for(;i<x->n;i++) equal = equal && (kF(x)[i] == kF(y)[i]);
             break;
    case KS: for(;i<x->n;i++) equal = equal && (kS(x)[i] == kS(y)[i]);
             break;
    default: return 0;
  }
  R equal;
}

static inline uint8_t __attribute__((always_inline)) equalA(K x,K y,I t){
  switch(t){
    case -KB: case -KC: 
    case -KG: return (x->g == y->g);
    case -KH: return (x->h == y->h);
    case -KM: case -KD: case -KU: case -KV: case -KT: case -KI: return (x->i == y->i);
    case -KP: case -KN: case -KJ: return (x->j == y->j);
    case -KE: return (x->e == y->e);
    case -KF: return (x->f == y->f);
    case -KS: return (x->s == y->s);
  }
  R 0;
}

//NOTE: This function works only for flat lists, not nested lists
static inline uint8_t __attribute__((always_inline)) equalH(K x,K y,I t){
  if(t>0) return equalV(x,y,t); //vectorized equality test
  else if(t<0) return equalA(x,y,t); //atomic equality test
  //if here, it is a general list of atoms - we don't allow list of lists
  I i=0;
  uint8_t equal=1;
  for(;i<x->n;i++) equal = equal && equalA(kK(x)[i],kK(y)[i],kK(x)[i]->t); //assumption: we already validate that x and y are of same types 
  R equal;
}

//general list comparison
I cmpg(K x,K y,I i,I j){
  I ip=i;
  for(;(i<x->n) && (j<y->n) && equalH(kK(x)[i],kK(y)[j],kK(x)[i]->t);i++,j++);
  R i-ip;
}

//Vectorized comparison
I cmpv(K x,K y,I i,I j){
  I ip=i;
  switch(x->t){
    case KB: case KC: case KG: 
             for(;i<x->n && j<y->n && (kG(x)[i] == kG(y)[j]);i++,j++);
             break;
    case KH: for(;i<x->n && j<y->n && (kH(x)[i] == kH(y)[j]);i++,j++);
             break;
    case KM: case KD: case KU: case KV: case KT: case KI: 
             for(;i<x->n && j<y->n && (kI(x)[i] == kI(y)[j]);i++,j++);
             break;
    case KP: case KN: case KJ: for(;i<x->n && j<y->n && (kJ(x)[i] == kJ(y)[j]);i++,j++);
             break;
    case KE: for(;i<x->n && j<y->n && (kE(x)[i] == kE(y)[j]);i++,j++);
             break;
    case KF: for(;i<x->n && j<y->n && (kF(x)[i] == kF(y)[j]);i++,j++);
             break;
    case KS: for(;i<x->n && j<y->n && (kS(x)[i] == kS(y)[j]);i++,j++);
             break;
    default: return 0;
   }
  R i-ip;
}
