
#include "escheme.h"


/**************** Copied from plt/src/mzscheme/src/object.c **************/
typedef long ClassVariable;

typedef struct Scheme_Class {
  Scheme_Type type;

  ClassVariable *ivars; /* Order determines order of evaluation */

  union {
    Scheme_Closed_Prim *initf;
    struct {
      Scheme_Instance_Init_Proc *f;
      void *data;
    } insti;
  } piu;
  short priminit;

  short pos;
  struct Scheme_Class **heritage;
  struct Scheme_Class *superclass; /* Redundant, but useful. */
  Scheme_Object *super_init_name;

  short num_args, num_required_args, num_arg_vars;
  short num_ivar, num_private, num_ref;
  short num_public, num_slots; /* num_vslots == num_public */
  Scheme_Object **public_names;
  /* ... */
} Scheme_Class;

typedef struct Scheme_Interface {
  Scheme_Type type;
  short num_names, num_supers;
  Scheme_Object **names;
  short *name_map; /* position in names => interface slot position */
  struct Scheme_Interface **supers; /* all superinterfaces (flattened hierarchy) */
  short *super_offsets; /* superinterface => super's slot position offset */
  Scheme_Object *defname;
} Scheme_Interface;

/*************************************************************************/

Scheme_Object *array_to_list(int c, Scheme_Object **names)
{
  Scheme_Object *p = scheme_null;

  while (c--)
    p = scheme_make_pair(names[c], p);

  return p;
}


Scheme_Object *class_to_names(int argc, Scheme_Object **argv)
{
  Scheme_Class *class = (Scheme_Class *)argv[0];

  if (!SCHEME_CLASSP(argv[0]))
    scheme_wrong_type("class->names", "class", 0, argc, argv);

  return array_to_list(class->num_public, class->public_names);
}

Scheme_Object *interface_to_names(int argc, Scheme_Object **argv)
{
  Scheme_Interface *interface = (Scheme_Interface *)argv[0];

  if (!SCHEME_INTERFACEP(argv[0]))
    scheme_wrong_type("interface->names", "interface", 0, argc, argv);

  return array_to_list(interface->num_names, interface->names);
}

Scheme_Object *interface_to_super_interfaces(int argc, Scheme_Object **argv)
{
  Scheme_Interface *interface = (Scheme_Interface *)argv[0];

  if (!SCHEME_INTERFACEP(argv[0]))
    scheme_wrong_type("interface->super-interfaces", "interface", 0, argc, argv);

  return array_to_list(interface->num_supers, (Scheme_Object**)interface->supers);
}


Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_reload(env);
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("class->names", 
		    scheme_make_prim_w_arity(class_to_names,
					     "class->names",
					     1, 1),
		    env);
  scheme_add_global("interface->names", 
		    scheme_make_prim_w_arity(interface_to_names,
					     "interface->names",
					     1, 1),
		    env);
  scheme_add_global("interface->super-interfaces", 
		    scheme_make_prim_w_arity(interface_to_super_interfaces,
					     "interface->super-interfaces",
					     1, 1),
		    env);

  return scheme_void;
}
