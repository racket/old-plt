#include "escheme.h"

#include "homo-all-vector-glue.c"

Scheme_Object *get_vector(int argc, Scheme_Object **argv)
{
  Scheme_Object *new_vector = (Scheme_Object *)make_homo_u16_vector(10);

  return new_vector;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  return scheme_false;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  Scheme_Object *funs[2];

  funs[0] = scheme_make_prim_w_arity(set_all_vector_types,
				     "init",
				     1,1);
  funs[1] = scheme_make_prim_w_arity(get_vector,
				     "get-vector",
				     0,0);

  return scheme_values(2,funs);
}

Scheme_Object *scheme_module_name()
{
  return scheme_false;
}
