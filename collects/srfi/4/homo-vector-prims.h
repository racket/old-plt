#ifndef _HOMO_<type-name>_VECTOR_PRIMS_H
#define _HOMO_<type-name>_VECTOR_PRIMS_H

#ifdef _WIN32 /* Windows isn't C99 in this regard */
 typedef uint32_t UINT32;
 typedef uint16_t UINT16;
 typedef uint8_t UINT8;
 typedef int32_t INT32;
 typedef int16_t INT16;
 typedef int8_t INT8;  
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
