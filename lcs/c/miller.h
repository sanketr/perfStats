#include "v.h"
typedef struct{
size_t p;
size_t x;
size_t y;
size_t len;
} snakes;

typedef struct{
  void* vec;
  size_t size;
}vectors;

DARRAY(int4v,snakes);

typedef enum {false, true} bool;
bool (*compare) (char, char);
