#include "v.h"
#define DO(n,x) {int i=0; for (;i<n;i++) x;}

typedef struct{
size_t p;
size_t x;
size_t y;
size_t len;
} snakes;

typedef struct{
  size_t size;
  size_t stride;
  char type;
  void* vec;
}vec;

DARRAY(int4v,snakes);

typedef enum {false, true} bool;
bool (*compare) (char, char);
