#include <inttypes.h>
#include <stdlib.h>

#define SCALING 1.5
#define CONCAT(x,y) x ## y
#define DARRAY(name, type)\
typedef struct {\
  size_t size;\
  size_t cap;\
  type* arr;\
} name;\
\
size_t CONCAT(name,init)(name *v, size_t size){\
  v->size = 0;\
  v->cap = 0;\
  v->arr = (type*) malloc(size*(sizeof(type)));\
  if( v->arr == NULL) return 0;\
  v->cap = size;\
  return size;\
}\
\
void CONCAT(name,insert)(name *v, type e){\
  if(v->arr == NULL) return;\
  if(v->size == v->cap){\
    v->cap *= SCALING;\
    v->arr = (type*) realloc(v->arr, v->cap * sizeof(type));\
    if(v->arr == NULL){\
      v->size=0;\
      v->cap=0;\
      }\
  }\
  v->arr[v->size++] = e;\
}\
\
int inline CONCAT(name,size)(name *v) {\
  if(v->arr == NULL) return 0;\
  else return v->size;\
}\
\
int inline CONCAT(name,cap)(name *v){\
  if(v->arr == NULL) return 0;\
  else return v->cap;\
}\
\
void CONCAT(name,free)(name *v){\
  v->size=0;\
  v->cap=0;\
  free(v->arr);\
  v->arr=NULL;\
}
