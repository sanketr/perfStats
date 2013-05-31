#include "v.h"
#define KXVER 2
#include "k.h"

typedef enum {false,true} bool;

typedef struct{
I p;
I x;
I y;
I len;
} snakes;

DARRAY(int4v,snakes);

K lcs(K,K);
