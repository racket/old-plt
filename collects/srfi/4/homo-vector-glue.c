#include "escheme.h"
#include <math.h>

Scheme_Type homo_<type-name>_vector_type;
#include "homo-<type-name>-vector-prims.h"

void set_homo_<type-name>_vector_type(int argc, Scheme_Object **argv)
{
  if (argc != 1 )
    scheme_wrong_count("set_homo_<type-name>_vector_type",
		       1, 1, argc, argv);
  
  Scheme_Object *listptr = argv[0];
  long tmp; 
  
  while(listptr != scheme_null) {
    if (! SCHEME_PAIRP(listptr) ||
	! SCHEME_PAIRP(SCHEME_CAR(listptr)) ||
	! SCHEME_STRINGP(SCHEME_CAR(SCHEME_CAR(listptr))) ||
	! SCHEME_PAIRP(SCHEME_CDR(SCHEME_CAR(listptr))) ||
	! SCHEME_NUMBERP(SCHEME_CAR(SCHEME_CDR(SCHEME_CAR(listptr)))) ||
	(SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(listptr))) != scheme_null)) {
      scheme_wrong_type("set_homo_<type-name>_vector_type",
			"(listof (list/p string? integer?))",0,argc,argv);
    }
    
    if (strcmp(SCHEME_STR_VAL(SCHEME_CAR(SCHEME_CAR(listptr))),"<type-name>") == 0) {
      scheme_get_int_val(SCHEME_CAR(SCHEME_CDR(SCHEME_CAR(listptr))),&tmp);
      homo_<type-name>_vector_type = tmp; // assume cast succeeds.  GRR!
      return;
    }
    listptr = SCHEME_CDR(listptr);
  }
  scheme_signal_error("set_homo_<type-name>_vector_type: type name <type-name> not found in initialization list");
}

