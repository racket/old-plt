#ifndef _HOMO_<type-name>_VECTOR_PRIMS_H
#define _HOMO_<type-name>_VECTOR_PRIMS_H


#ifdef _WIN32 
/* Windows isn't C99 in this regard */
#include <wtypes.h>
typedef UINT32 uint32_t;
typedef UINT16 uint16_t;
typedef UINT8 uint8_t;
typedef INT32 int32_t;
typedef INT16 int16_t;
typedef INT8 int8_t;  
#else
#include <inttypes.h>
#endif

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
