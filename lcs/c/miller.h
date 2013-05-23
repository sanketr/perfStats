#include "v.h"

#ifdef MILLER_COMPILE
#define DO(n,x) {int i=0; for (;i<n;i++) x;}
#endif

typedef enum {false, true} bool;

typedef struct{
int64_t p;
int64_t x;
int64_t y;
size_t len;
} snakes;

typedef struct{
  size_t size;
  void* vec;
}vec;
void vecfree(vec);

#ifdef MILLER_COMPILE
DARRAY(int4v,snakes);
#endif

size_t chrcmp(vec,vec,size_t,size_t);
vec* lcs(vec,vec,size_t (*)(vec,vec,size_t,size_t));
