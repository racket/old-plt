#ifndef _HOMO_<type-name>_VECTOR_PRIMS_H
#define _HOMO_<type-name>_VECTOR_PRIMS_H

#include <inttypes.h>

typedef struct 
{
  Scheme_Type type;
  short keyex; /* 1 in low bit indicates immutable */
  int length;
  <type> els[1];
} homo_<type-name>_vector;


extern Scheme_Type homo_<type-name>_vector_type;


static homo_<type-name>_vector* make_homo_<type-name>_vector(int length)
{
  homo_<type-name>_vector* vec;

  vec = (homo_<type-name>_vector*)scheme_malloc_atomic(sizeof(<type>) * length + sizeof(homo_<type-name>_vector) - sizeof(<type>));
  vec->type = homo_<type-name>_vector_type;
  vec->length = length;
  vec->keyex = 0;
  return vec;
}

#define HOMO_<caps-type-name>_VECTORP(obj)   SAME_TYPE(SCHEME_TYPE(obj), homo_<type-name>_vector_type)



#endif
