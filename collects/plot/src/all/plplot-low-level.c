#define MZC_SRC_FILE "/usr/home/alex/plt/collects/plot/plplot-low-level.ss"

#include "escheme.h"

/* c-declare literals */

#include "plplot.h"
double * list_to_array(Scheme_Object * list)
       {
        
        double * ar = (double *)scheme_malloc(scheme_list_length(list) * sizeof(double));
        int i = 0;
        while(!SCHEME_NULLP(list))
        {
         Scheme_Object * car = SCHEME_CAR(list);
         double tmp;
                
         tmp = scheme_real_to_double(car);
         ar[i] = tmp;
         list = SCHEME_CDR(list);
         i++;
         }
        
        return ar;
        }
int * int_list_to_array(Scheme_Object * list)
       {
        
        int * ar = (int *)scheme_malloc(scheme_list_length(list) * sizeof(int));
        int i = 0;
        while(!SCHEME_NULLP(list))
        {
         Scheme_Object * car = SCHEME_CAR(list);
         double tmp;
                
         tmp = (int)scheme_real_to_double(car);
         ar[i] = tmp;
         list = SCHEME_CDR(list);
         i++;
         }
        
        return ar;
        }
  
  
double ** list_of_list_to_array(Scheme_Object * list)
   {
    double ** grid = (double *) scheme_malloc(scheme_list_length(list) * sizeof(double *));
           int i = 0 ;
           while(!SCHEME_NULLP(list))
           {
            Scheme_Object * car = SCHEME_CAR(list);
                          grid[i] = list_to_array(car);
                          i++;
                          list = SCHEME_CDR(list);
                          }
           return grid;
           }

/* done with c-declare literals */


/* c-lambda implementations */

Scheme_Object *mzc_cffi_0(int argc, Scheme_Object **argv) {
  int ___arg1;
  int ___arg2;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-setup-page", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
 { long tmp;
  if (scheme_get_int_val(argv[1], &tmp)) {
    ___arg2 = tmp  /* argv[1] */;
  } else {
    scheme_wrong_type("pl-setup-page", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 1, argc, argv);
    return NULL;
  }
 }
 {
plspage(0.,0.,___arg1,___arg2,0,0);
  
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_1(int argc, Scheme_Object **argv) {
  char* ___arg1;
  if (SCHEME_FALSEP(argv[0]) || SCHEME_STRINGP(argv[0])) {
    ___arg1 = (SCHEME_FALSEP(argv[0]) ? NULL : SCHEME_STR_VAL(argv[0]));
  } else {
    scheme_wrong_type("pl-set-device", "string or #f", 0, argc, argv);
    return NULL;
  }
 {
  plsdev(___arg1);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_2(int argc, Scheme_Object **argv) {
  char* ___arg1;
  if (SCHEME_FALSEP(argv[0]) || SCHEME_STRINGP(argv[0])) {
    ___arg1 = (SCHEME_FALSEP(argv[0]) ? NULL : SCHEME_STR_VAL(argv[0]));
  } else {
    scheme_wrong_type("pl-set-output-file", "string or #f", 0, argc, argv);
    return NULL;
  }
 {
  plsfnam(___arg1);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_3(int argc, Scheme_Object **argv) {
 {
plinit();

  
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_4(int argc, Scheme_Object **argv) {
 {
  plend();
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_5(int argc, Scheme_Object **argv) {
  double ___arg1;
  double ___arg2;
  double ___arg3;
  double ___arg4;
  int ___arg5;
  int ___arg6;
  if (SCHEME_REALP(argv[0])) {
    ___arg1 = scheme_real_to_double(argv[0]);
  } else {
    scheme_wrong_type("pl-set-plot-environment", "real number", 0, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[1])) {
    ___arg2 = scheme_real_to_double(argv[1]);
  } else {
    scheme_wrong_type("pl-set-plot-environment", "real number", 1, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[2])) {
    ___arg3 = scheme_real_to_double(argv[2]);
  } else {
    scheme_wrong_type("pl-set-plot-environment", "real number", 2, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[3])) {
    ___arg4 = scheme_real_to_double(argv[3]);
  } else {
    scheme_wrong_type("pl-set-plot-environment", "real number", 3, argc, argv);
    return NULL;
  }
 { long tmp;
  if (scheme_get_int_val(argv[4], &tmp)) {
    ___arg5 = tmp  /* argv[4] */;
  } else {
    scheme_wrong_type("pl-set-plot-environment", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 4, argc, argv);
    return NULL;
  }
 }
 { long tmp;
  if (scheme_get_int_val(argv[5], &tmp)) {
    ___arg6 = tmp  /* argv[5] */;
  } else {
    scheme_wrong_type("pl-set-plot-environment", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 5, argc, argv);
    return NULL;
  }
 }
 {
  plenv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_6(int argc, Scheme_Object **argv) {
  char* ___arg1;
  char* ___arg2;
  char* ___arg3;
  if (SCHEME_STRINGP(argv[0])) {
    ___arg1 = SCHEME_STR_VAL(argv[0]);
  } else {
    scheme_wrong_type("pl-set-labels", "string", 0, argc, argv);
    return NULL;
  }
  if (SCHEME_STRINGP(argv[1])) {
    ___arg2 = SCHEME_STR_VAL(argv[1]);
  } else {
    scheme_wrong_type("pl-set-labels", "string", 1, argc, argv);
    return NULL;
  }
  if (SCHEME_STRINGP(argv[2])) {
    ___arg3 = SCHEME_STR_VAL(argv[2]);
  } else {
    scheme_wrong_type("pl-set-labels", "string", 2, argc, argv);
    return NULL;
  }
 {
  pllab(___arg1, ___arg2, ___arg3);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_7(int argc, Scheme_Object **argv) {
  int ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-plot-line", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
  ___arg2 = argv[1];
  ___arg3 = argv[2];
 {
plline(___arg1,list_to_array(___arg2),list_to_array(___arg3));
              
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_8(int argc, Scheme_Object **argv) {
  double ___arg1;
  double ___arg2;
  double ___arg3;
  double ___arg4;
  if (SCHEME_REALP(argv[0])) {
    ___arg1 = scheme_real_to_double(argv[0]);
  } else {
    scheme_wrong_type("pl-plot-segment", "real number", 0, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[1])) {
    ___arg2 = scheme_real_to_double(argv[1]);
  } else {
    scheme_wrong_type("pl-plot-segment", "real number", 1, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[2])) {
    ___arg3 = scheme_real_to_double(argv[2]);
  } else {
    scheme_wrong_type("pl-plot-segment", "real number", 2, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[3])) {
    ___arg4 = scheme_real_to_double(argv[3]);
  } else {
    scheme_wrong_type("pl-plot-segment", "real number", 3, argc, argv);
    return NULL;
  }
 {
  pljoin(___arg1, ___arg2, ___arg3, ___arg4);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_9(int argc, Scheme_Object **argv) {
  int ___arg1;
  int ___arg2;
  int ___arg3;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-set-background-color", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
 { long tmp;
  if (scheme_get_int_val(argv[1], &tmp)) {
    ___arg2 = tmp  /* argv[1] */;
  } else {
    scheme_wrong_type("pl-set-background-color", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 1, argc, argv);
    return NULL;
  }
 }
 { long tmp;
  if (scheme_get_int_val(argv[2], &tmp)) {
    ___arg3 = tmp  /* argv[2] */;
  } else {
    scheme_wrong_type("pl-set-background-color", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 2, argc, argv);
    return NULL;
  }
 }
 {
  plscolbg(___arg1, ___arg2, ___arg3);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_10(int argc, Scheme_Object **argv) {
  int ___arg1;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-select-colormap0-index", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
 {
  plcol0(___arg1);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_11(int argc, Scheme_Object **argv) {
  int ___arg1;
  int ___arg2;
  int ___arg3;
  int ___arg4;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-set-colormap0-index", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
 { long tmp;
  if (scheme_get_int_val(argv[1], &tmp)) {
    ___arg2 = tmp  /* argv[1] */;
  } else {
    scheme_wrong_type("pl-set-colormap0-index", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 1, argc, argv);
    return NULL;
  }
 }
 { long tmp;
  if (scheme_get_int_val(argv[2], &tmp)) {
    ___arg3 = tmp  /* argv[2] */;
  } else {
    scheme_wrong_type("pl-set-colormap0-index", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 2, argc, argv);
    return NULL;
  }
 }
 { long tmp;
  if (scheme_get_int_val(argv[3], &tmp)) {
    ___arg4 = tmp  /* argv[3] */;
  } else {
    scheme_wrong_type("pl-set-colormap0-index", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 3, argc, argv);
    return NULL;
  }
 }
 {
  plscol0(___arg1, ___arg2, ___arg3, ___arg4);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_12(int argc, Scheme_Object **argv) {
  int ___arg1;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-set-line-width", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
 {
  plwid(___arg1);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_13(int argc, Scheme_Object **argv) {
  double ___arg1;
  double ___arg2;
  double ___arg3;
  double ___arg4;
  double ___arg5;
  char* ___arg6;
  if (SCHEME_REALP(argv[0])) {
    ___arg1 = scheme_real_to_double(argv[0]);
  } else {
    scheme_wrong_type("pl-write-text", "real number", 0, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[1])) {
    ___arg2 = scheme_real_to_double(argv[1]);
  } else {
    scheme_wrong_type("pl-write-text", "real number", 1, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[2])) {
    ___arg3 = scheme_real_to_double(argv[2]);
  } else {
    scheme_wrong_type("pl-write-text", "real number", 2, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[3])) {
    ___arg4 = scheme_real_to_double(argv[3]);
  } else {
    scheme_wrong_type("pl-write-text", "real number", 3, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[4])) {
    ___arg5 = scheme_real_to_double(argv[4]);
  } else {
    scheme_wrong_type("pl-write-text", "real number", 4, argc, argv);
    return NULL;
  }
  if (SCHEME_STRINGP(argv[5])) {
    ___arg6 = SCHEME_STR_VAL(argv[5]);
  } else {
    scheme_wrong_type("pl-write-text", "string", 5, argc, argv);
    return NULL;
  }
 {
  plptex(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_14(int argc, Scheme_Object **argv) {
  Scheme_Object* ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  Scheme_Object* ___arg4;
  ___arg1 = argv[0];
  ___arg2 = argv[1];
  ___arg3 = argv[2];
  ___arg4 = argv[3];
 {
PLcGrid mapping;
               mapping.xg = list_to_array(___arg2);
               mapping.nx = scheme_list_length(___arg2);
               mapping.yg = list_to_array(___arg3);
               mapping.ny = scheme_list_length(___arg3);
               
               plcont(list_of_list_to_array(___arg1),mapping.nx,mapping.ny,
                      1, mapping.nx, 1,mapping.ny,
                      list_to_array(___arg4), scheme_list_length(___arg4),
                      pltr1, (void *) &mapping);
               
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_15(int argc, Scheme_Object **argv) {
  Scheme_Object* ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  Scheme_Object* ___arg4;
  ___arg1 = argv[0];
  ___arg2 = argv[1];
  ___arg3 = argv[2];
  ___arg4 = argv[3];
 {
PLcGrid mapping;
               mapping.xg = list_to_array(___arg2);
               mapping.nx = scheme_list_length(___arg2);
               mapping.yg = list_to_array(___arg3);
               mapping.ny = scheme_list_length(___arg3);
               
               plshades(list_of_list_to_array(___arg1),mapping.nx,mapping.ny, NULL,
                                             0,0,0,0,                      
                                             list_to_array(___arg4), scheme_list_length(___arg4),
                                             1,1,0,
                                             plfill, 1 , pltr1, (void *) &mapping);
               
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_16(int argc, Scheme_Object **argv) {
  int ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  int ___arg4;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-plot-points", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
  ___arg2 = argv[1];
  ___arg3 = argv[2];
 { long tmp;
  if (scheme_get_int_val(argv[3], &tmp)) {
    ___arg4 = tmp  /* argv[3] */;
  } else {
    scheme_wrong_type("pl-plot-points", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 3, argc, argv);
    return NULL;
  }
 }
 {
plpoin(___arg1,list_to_array(___arg2),list_to_array(___arg3),___arg4);
              
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_17(int argc, Scheme_Object **argv) {
  int ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  Scheme_Object* ___arg4;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-x-error-bars", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
  ___arg2 = argv[1];
  ___arg3 = argv[2];
  ___arg4 = argv[3];
 {
plerrx(___arg1,list_to_array(___arg2),list_to_array(___arg3),list_to_array(___arg4));
              
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_18(int argc, Scheme_Object **argv) {
  int ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  Scheme_Object* ___arg4;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-y-error-bars", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
  ___arg2 = argv[1];
  ___arg3 = argv[2];
  ___arg4 = argv[3];
 {
plerry(___arg1,list_to_array(___arg2),list_to_array(___arg3),list_to_array(___arg4));
              
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_19(int argc, Scheme_Object **argv) {
  double ___arg1;
  double ___arg2;
  double ___arg3;
  double ___arg4;
  double ___arg5;
  double ___arg6;
  double ___arg7;
  double ___arg8;
  double ___arg9;
  double ___arg10;
  double ___arg11;
  if (SCHEME_REALP(argv[0])) {
    ___arg1 = scheme_real_to_double(argv[0]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 0, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[1])) {
    ___arg2 = scheme_real_to_double(argv[1]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 1, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[2])) {
    ___arg3 = scheme_real_to_double(argv[2]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 2, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[3])) {
    ___arg4 = scheme_real_to_double(argv[3]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 3, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[4])) {
    ___arg5 = scheme_real_to_double(argv[4]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 4, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[5])) {
    ___arg6 = scheme_real_to_double(argv[5]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 5, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[6])) {
    ___arg7 = scheme_real_to_double(argv[6]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 6, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[7])) {
    ___arg8 = scheme_real_to_double(argv[7]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 7, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[8])) {
    ___arg9 = scheme_real_to_double(argv[8]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 8, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[9])) {
    ___arg10 = scheme_real_to_double(argv[9]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 9, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[10])) {
    ___arg11 = scheme_real_to_double(argv[10]);
  } else {
    scheme_wrong_type("pl-world-3d", "real number", 10, argc, argv);
    return NULL;
  }
 {
  plw3d(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___arg8, ___arg9, ___arg10, ___arg11);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_20(int argc, Scheme_Object **argv) {
  Scheme_Object* ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  ___arg1 = argv[0];
  ___arg2 = argv[1];
  ___arg3 = argv[2];
 {
plot3d(list_to_array(___arg1),list_to_array(___arg2),list_of_list_to_array(___arg3),
                                   scheme_list_length(___arg1),scheme_list_length(___arg2),DRAW_LINEXY,0);
              
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_21(int argc, Scheme_Object **argv) {
  Scheme_Object* ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  ___arg1 = argv[0];
  ___arg2 = argv[1];
  ___arg3 = argv[2];
 {
plmesh(list_to_array(___arg1),list_to_array(___arg2),list_of_list_to_array(___arg3),
                                   scheme_list_length(___arg1),scheme_list_length(___arg2),DRAW_LINEXY);
  
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_22(int argc, Scheme_Object **argv) {
  Scheme_Object* ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  int ___arg4;
  int ___arg5;
  int ___arg6;
  int ___arg7;
  Scheme_Object* ___arg8;
  ___arg1 = argv[0];
  ___arg2 = argv[1];
  ___arg3 = argv[2];
  ___arg4 = SCHEME_TRUEP(argv[3]);
  ___arg5 = SCHEME_TRUEP(argv[4]);
  ___arg6 = SCHEME_TRUEP(argv[5]);
  ___arg7 = SCHEME_TRUEP(argv[6]);
  ___arg8 = argv[7];
 {

  
    
    PLFLT i[2], h[2], l[2], s[2];

  i[0] = 0.0;		/* left boundary */
  i[1] = 1.0;		/* right boundary */

  h[0] = 240; /* blue -> green -> yellow -> */
  h[1] = 0;   /* -> red */

  l[0] = 0.6;
  l[1] = 0.6;

  s[0] = 0.8;
  s[1] = 0.8;

  plscmap1n(256);
  plscmap1l(0, 2, i, h, l, s, NULL);
  {
  int opts = (___arg4 ? DRAW_LINEXY : 0) |
                         (___arg5 ? MAG_COLOR : 0) |
                         (___arg6 ? BASE_CONT : 0) |
                         (___arg7 ? DRAW_SIDES : 0) ;
              plmeshc(list_to_array(___arg1),list_to_array(___arg2),list_of_list_to_array(___arg3),
                                    scheme_list_length(___arg1),scheme_list_length(___arg2),opts,
                                    list_to_array(___arg8),scheme_list_length(___arg8));
              }
  
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_23(int argc, Scheme_Object **argv) {
  char* ___arg1;
  char* ___arg2;
  double ___arg3;
  int ___arg4;
  char* ___arg5;
  char* ___arg6;
  double ___arg7;
  int ___arg8;
  char* ___arg9;
  char* ___arg10;
  double ___arg11;
  int ___arg12;
  if (SCHEME_FALSEP(argv[0]) || SCHEME_STRINGP(argv[0])) {
    ___arg1 = (SCHEME_FALSEP(argv[0]) ? NULL : SCHEME_STR_VAL(argv[0]));
  } else {
    scheme_wrong_type("pl-box3", "string or #f", 0, argc, argv);
    return NULL;
  }
  if (SCHEME_FALSEP(argv[1]) || SCHEME_STRINGP(argv[1])) {
    ___arg2 = (SCHEME_FALSEP(argv[1]) ? NULL : SCHEME_STR_VAL(argv[1]));
  } else {
    scheme_wrong_type("pl-box3", "string or #f", 1, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[2])) {
    ___arg3 = scheme_real_to_double(argv[2]);
  } else {
    scheme_wrong_type("pl-box3", "real number", 2, argc, argv);
    return NULL;
  }
 { long tmp;
  if (scheme_get_int_val(argv[3], &tmp)) {
    ___arg4 = tmp  /* argv[3] */;
  } else {
    scheme_wrong_type("pl-box3", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 3, argc, argv);
    return NULL;
  }
 }
  if (SCHEME_FALSEP(argv[4]) || SCHEME_STRINGP(argv[4])) {
    ___arg5 = (SCHEME_FALSEP(argv[4]) ? NULL : SCHEME_STR_VAL(argv[4]));
  } else {
    scheme_wrong_type("pl-box3", "string or #f", 4, argc, argv);
    return NULL;
  }
  if (SCHEME_FALSEP(argv[5]) || SCHEME_STRINGP(argv[5])) {
    ___arg6 = (SCHEME_FALSEP(argv[5]) ? NULL : SCHEME_STR_VAL(argv[5]));
  } else {
    scheme_wrong_type("pl-box3", "string or #f", 5, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[6])) {
    ___arg7 = scheme_real_to_double(argv[6]);
  } else {
    scheme_wrong_type("pl-box3", "real number", 6, argc, argv);
    return NULL;
  }
 { long tmp;
  if (scheme_get_int_val(argv[7], &tmp)) {
    ___arg8 = tmp  /* argv[7] */;
  } else {
    scheme_wrong_type("pl-box3", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 7, argc, argv);
    return NULL;
  }
 }
  if (SCHEME_FALSEP(argv[8]) || SCHEME_STRINGP(argv[8])) {
    ___arg9 = (SCHEME_FALSEP(argv[8]) ? NULL : SCHEME_STR_VAL(argv[8]));
  } else {
    scheme_wrong_type("pl-box3", "string or #f", 8, argc, argv);
    return NULL;
  }
  if (SCHEME_FALSEP(argv[9]) || SCHEME_STRINGP(argv[9])) {
    ___arg10 = (SCHEME_FALSEP(argv[9]) ? NULL : SCHEME_STR_VAL(argv[9]));
  } else {
    scheme_wrong_type("pl-box3", "string or #f", 9, argc, argv);
    return NULL;
  }
  if (SCHEME_REALP(argv[10])) {
    ___arg11 = scheme_real_to_double(argv[10]);
  } else {
    scheme_wrong_type("pl-box3", "real number", 10, argc, argv);
    return NULL;
  }
 { long tmp;
  if (scheme_get_int_val(argv[11], &tmp)) {
    ___arg12 = tmp  /* argv[11] */;
  } else {
    scheme_wrong_type("pl-box3", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 11, argc, argv);
    return NULL;
  }
 }
 {
  plbox3(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___arg8, ___arg9, ___arg10, ___arg11, ___arg12);
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_24(int argc, Scheme_Object **argv) {
  int ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  Scheme_Object* ___arg4;
  Scheme_Object* ___arg5;
  int ___arg6;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-poly3", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
  ___arg2 = argv[1];
  ___arg3 = argv[2];
  ___arg4 = argv[3];
  ___arg5 = argv[4];
 { long tmp;
  if (scheme_get_int_val(argv[5], &tmp)) {
    ___arg6 = tmp  /* argv[5] */;
  } else {
    scheme_wrong_type("pl-poly3", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 5, argc, argv);
    return NULL;
  }
 }
 {
plpoly3(___arg1, 
                       list_to_array(___arg2),
                       list_to_array(___arg3),
                       list_to_array(___arg4),
                       int_list_to_array(___arg5),
                       ___arg6);
  
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_25(int argc, Scheme_Object **argv) {
  int ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
  Scheme_Object* ___arg4;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-line3", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
  ___arg2 = argv[1];
  ___arg3 = argv[2];
  ___arg4 = argv[3];
 {
plline3(___arg1, 
                       list_to_array(___arg2),
                       list_to_array(___arg3),
                       list_to_array(___arg4));
  
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}
Scheme_Object *mzc_cffi_26(int argc, Scheme_Object **argv) {
  int ___arg1;
  Scheme_Object* ___arg2;
  Scheme_Object* ___arg3;
 { long tmp;
  if (scheme_get_int_val(argv[0], &tmp)) {
    ___arg1 = tmp  /* argv[0] */;
  } else {
    scheme_wrong_type("pl-fill", "exact integer between (expt 2 -31) and (sub1 (expr 2 31)) inclusive", 0, argc, argv);
    return NULL;
  }
 }
  ___arg2 = argv[1];
  ___arg3 = argv[2];
 {
plfill(___arg1,
               list_to_array(___arg2),
               list_to_array(___arg3));
  
#ifdef ___AT_END
  ___AT_END
#undef ___AT_END
#endif
 }
  return scheme_void;

}

/* done with c-lambda implementations */

#include "mzc.h"

/* compiler-written structures */

static const char *SYMBOL_STRS[32] = {
  "pl-setup-page", /* 0 */
  "pl-set-device", /* 1 */
  "pl-set-output-file", /* 2 */
  "pl-init-plot", /* 3 */
  "pl-finish-plot", /* 4 */
  "pl-set-plot-environment", /* 5 */
  "pl-set-labels", /* 6 */
  "pl-plot-line", /* 7 */
  "pl-plot-segment", /* 8 */
  "pl-set-background-color", /* 9 */
  "pl-select-colormap0-index", /* 10 */
  "pl-set-colormap0-index", /* 11 */
  "pl-set-line-width", /* 12 */
  "pl-write-text", /* 13 */
  "pl-2d-contour-plot", /* 14 */
  "pl-2d-shade-plot", /* 15 */
  "pl-plot-points", /* 16 */
  "pl-x-error-bars", /* 17 */
  "pl-y-error-bars", /* 18 */
  "pl-world-3d", /* 19 */
  "pl-plot3d", /* 20 */
  "pl-mesh3d", /* 21 */
  "pl-mesh3dc", /* 22 */
  "pl-box3", /* 23 */
  "pl-poly3", /* 24 */
  "pl-line3", /* 25 */
  "pl-fill", /* 26 */
  "plplot-low-level", /* 27 */
  "mzscheme", /* 28 */
  "lib", /* 29 */
  "#%kernel", /* 30 */
  "list", /* 31 */
}; /* end of SYMBOL_STRS */

static const long SYMBOL_LENS[32] = {
  13, /* 0 */
  13, /* 1 */
  18, /* 2 */
  12, /* 3 */
  14, /* 4 */
  23, /* 5 */
  13, /* 6 */
  12, /* 7 */
  15, /* 8 */
  23, /* 9 */
  25, /* 10 */
  22, /* 11 */
  17, /* 12 */
  13, /* 13 */
  18, /* 14 */
  16, /* 15 */
  14, /* 16 */
  15, /* 17 */
  15, /* 18 */
  11, /* 19 */
  9, /* 20 */
  9, /* 21 */
  10, /* 22 */
  7, /* 23 */
  8, /* 24 */
  8, /* 25 */
  7, /* 26 */
  16, /* 27 */
  8, /* 28 */
  3, /* 29 */
  8, /* 30 */
  4, /* 31 */
}; /* end of SYMBOL_LENS */

static Scheme_Object * SYMBOLS[32];

/* cffi.ss */
static const char STRING_0[8] = {
    99, 102, 102, 105, 46, 115, 115, 0 }; /* end of STRING_0 */

/* compiler */
static const char STRING_1[9] = {
    99, 111, 109, 112, 105, 108, 101, 114, 0 }; /* end of STRING_1 */

/* primitives referenced by the code */
static struct {
  Scheme_Object * glg309__kernel_list;
} P;

/* compiler-written static variables */
static struct {
  /* Write fields as an array to help C compilers */
  /* that don't like really big records. */
  Scheme_Object * _consts_[34];
# define const314 _consts_[0]
# define const313 _consts_[1]
# define const312 _consts_[2]
# define const311 _consts_[3]
# define const310 _consts_[4]
# define const308 _consts_[5]
# define const307 _consts_[6]
# define const306 _consts_[7]
# define const304 _consts_[8]
# define const302 _consts_[9]
# define const300 _consts_[10]
# define const298 _consts_[11]
# define const296 _consts_[12]
# define const294 _consts_[13]
# define const292 _consts_[14]
# define const290 _consts_[15]
# define const288 _consts_[16]
# define const286 _consts_[17]
# define const284 _consts_[18]
# define const282 _consts_[19]
# define const280 _consts_[20]
# define const278 _consts_[21]
# define const276 _consts_[22]
# define const274 _consts_[23]
# define const272 _consts_[24]
# define const270 _consts_[25]
# define const268 _consts_[26]
# define const266 _consts_[27]
# define const264 _consts_[28]
# define const262 _consts_[29]
# define const260 _consts_[30]
# define const258 _consts_[31]
# define const256 _consts_[32]
# define const254 _consts_[33]
} S;

/* compiler-written per-load static variables */
typedef struct Scheme_Per_Load_Statics {
  int dummy;
} Scheme_Per_Load_Statics;

/* compiler-written per-invoke variables for module 0 */
typedef struct Scheme_Per_Invoke_Statics_0 {
  int dummy;
} Scheme_Per_Invoke_Statics_0;
typedef struct Scheme_Per_Invoke_Syntax_Statics_0 {
  int dummy;
} Scheme_Per_Invoke_Syntax_Statics_0;



static void make_symbols()
{
  int i;
  for (i = 32; i--; )
    SYMBOLS[i] = scheme_intern_exact_symbol(SYMBOL_STRS[i], SYMBOL_LENS[i]);
}

static void make_syntax_strings()
{
}

static void gc_registration()
{
    /* register compiler-written static variables with GC */
    scheme_register_extension_global(&SYMBOLS, sizeof(SYMBOLS));
    scheme_register_extension_global(&P, sizeof(P));
    scheme_register_extension_global(&S, sizeof(S));

}

static void init_prims(Scheme_Env * env)
{
   /* primitives referenced by the code */
    P.glg309__kernel_list = scheme_module_bucket(SYMBOLS[30], SYMBOLS[31], -1, env)->val;
}

static void init_constants_0(Scheme_Env * env)
{
#define self_modidx NULL
    Scheme_Object * arg[27];
    Scheme_Thread * pr = scheme_current_thread;
    Scheme_Object ** tail_buf;
    { /* [#f,#f] */
        S.const254 = scheme_make_prim_w_arity(mzc_cffi_0, "pl-setup-page", 2, 2);
    }
    { /* [#f,#f] */
        S.const256 = scheme_make_prim_w_arity(mzc_cffi_1, "pl-set-device", 1, 1);
    }
    { /* [#f,#f] */
        S.const258 = scheme_make_prim_w_arity(mzc_cffi_2, "pl-set-output-file", 1, 1);
    }
    { /* [#f,#f] */
        S.const260 = scheme_make_prim_w_arity(mzc_cffi_3, "pl-init-plot", 0, 0);
    }
    { /* [#f,#f] */
        S.const262 = scheme_make_prim_w_arity(mzc_cffi_4, "pl-finish-plot", 0, 0);
    }
    { /* [#f,#f] */
        S.const264 = scheme_make_prim_w_arity(mzc_cffi_5, "pl-set-plot-environment", 6, 6);
    }
    { /* [#f,#f] */
        S.const266 = scheme_make_prim_w_arity(mzc_cffi_6, "pl-set-labels", 3, 3);
    }
    { /* [#f,#f] */
        S.const268 = scheme_make_prim_w_arity(mzc_cffi_7, "pl-plot-line", 3, 3);
    }
    { /* [#f,#f] */
        S.const270 = scheme_make_prim_w_arity(mzc_cffi_8, "pl-plot-segment", 4, 4);
    }
    { /* [#f,#f] */
        S.const272 = scheme_make_prim_w_arity(mzc_cffi_9, "pl-set-background-color", 3, 3);
    }
    { /* [#f,#f] */
        S.const274 = scheme_make_prim_w_arity(mzc_cffi_10, "pl-select-colormap0-index", 1, 1);
    }
    { /* [#f,#f] */
        S.const276 = scheme_make_prim_w_arity(mzc_cffi_11, "pl-set-colormap0-index", 4, 4);
    }
    { /* [#f,#f] */
        S.const278 = scheme_make_prim_w_arity(mzc_cffi_12, "pl-set-line-width", 1, 1);
    }
    { /* [#f,#f] */
        S.const280 = scheme_make_prim_w_arity(mzc_cffi_13, "pl-write-text", 6, 6);
    }
    { /* [#f,#f] */
        S.const282 = scheme_make_prim_w_arity(mzc_cffi_14, "pl-2d-contour-plot", 4, 4);
    }
    { /* [#f,#f] */
        S.const284 = scheme_make_prim_w_arity(mzc_cffi_15, "pl-2d-shade-plot", 4, 4);
    }
    { /* [#f,#f] */
        S.const286 = scheme_make_prim_w_arity(mzc_cffi_16, "pl-plot-points", 4, 4);
    }
    { /* [#f,#f] */
        S.const288 = scheme_make_prim_w_arity(mzc_cffi_17, "pl-x-error-bars", 4, 4);
    }
    { /* [#f,#f] */
        S.const290 = scheme_make_prim_w_arity(mzc_cffi_18, "pl-y-error-bars", 4, 4);
    }
    { /* [#f,#f] */
        S.const292 = scheme_make_prim_w_arity(mzc_cffi_19, "pl-world-3d", 11, 11);
    }
    { /* [#f,#f] */
        S.const294 = scheme_make_prim_w_arity(mzc_cffi_20, "pl-plot3d", 3, 3);
    }
    { /* [#f,#f] */
        S.const296 = scheme_make_prim_w_arity(mzc_cffi_21, "pl-mesh3d", 3, 3);
    }
    { /* [#f,#f] */
        S.const298 = scheme_make_prim_w_arity(mzc_cffi_22, "pl-mesh3dc", 8, 8);
    }
    { /* [#f,#f] */
        S.const300 = scheme_make_prim_w_arity(mzc_cffi_23, "pl-box3", 12, 12);
    }
    { /* [#f,#f] */
        S.const302 = scheme_make_prim_w_arity(mzc_cffi_24, "pl-poly3", 6, 6);
    }
#undef self_modidx
} /* end of init_constants_0 */

static void init_constants_1(Scheme_Env * env)
{
#define self_modidx NULL
    Scheme_Object * arg[27];
    Scheme_Thread * pr = scheme_current_thread;
    Scheme_Object ** tail_buf;
    { /* [#f,#f] */
        S.const304 = scheme_make_prim_w_arity(mzc_cffi_25, "pl-line3", 4, 4);
    }
    { /* [#f,#f] */
        S.const306 = scheme_make_prim_w_arity(mzc_cffi_26, "pl-fill", 3, 3);
    }
    { /* [3,8] */
        S.const307 = scheme_make_immutable_sized_string((char *)STRING_0, 7, 0);
    }
    { /* [3,18] */
        S.const308 = scheme_make_immutable_sized_string((char *)STRING_1, 8, 0);
    }
    { /* [3,3] */
        arg[0] = SYMBOLS[29];
        arg[1] = S.const307;
        arg[2] = S.const308;
        S.const310 = _scheme_direct_apply_primitive_multi(P.glg309__kernel_list, 3, arg);
    }
    { /* [1,0] */
        Scheme_Object * macapply315;
        Scheme_Object * macapply316;
        macapply315 = SYMBOLS[28];
        macapply316 = S.const310;
        S.const311 = MZC_LIST2(P.glg309__kernel_list, macapply315, macapply316);
    }
    { /* [1,0] */
        Scheme_Object * macapply317;
        Scheme_Object * macapply318;
        macapply317 = SYMBOLS[28];
        macapply318 = SYMBOLS[28];
        S.const312 = MZC_LIST2(P.glg309__kernel_list, macapply317, macapply318);
    }
    { /* [1,0] */
        arg[0] = SYMBOLS[18];
        arg[1] = SYMBOLS[17];
        arg[2] = SYMBOLS[13];
        arg[3] = SYMBOLS[19];
        arg[4] = SYMBOLS[0];
        arg[5] = SYMBOLS[5];
        arg[6] = SYMBOLS[2];
        arg[7] = SYMBOLS[12];
        arg[8] = SYMBOLS[6];
        arg[9] = SYMBOLS[1];
        arg[10] = SYMBOLS[11];
        arg[11] = SYMBOLS[9];
        arg[12] = SYMBOLS[10];
        arg[13] = SYMBOLS[24];
        arg[14] = SYMBOLS[20];
        arg[15] = SYMBOLS[8];
        arg[16] = SYMBOLS[16];
        arg[17] = SYMBOLS[7];
        arg[18] = SYMBOLS[22];
        arg[19] = SYMBOLS[21];
        arg[20] = SYMBOLS[25];
        arg[21] = SYMBOLS[3];
        arg[22] = SYMBOLS[4];
        arg[23] = SYMBOLS[26];
        arg[24] = SYMBOLS[23];
        arg[25] = SYMBOLS[15];
        arg[26] = SYMBOLS[14];
        S.const313 = _scheme_direct_apply_primitive_multi(P.glg309__kernel_list, 27, arg);
    }
    { /* [1,0] */
        arg[0] = SYMBOLS[27];
        arg[1] = S.const311;
        arg[2] = S.const312;
        arg[3] = S.const313;
        arg[4] = scheme_null;
        arg[5] = scheme_null;
        arg[6] = scheme_false;
        S.const314 = _scheme_direct_apply_primitive_multi(P.glg309__kernel_list, 7, arg);
    }
#undef self_modidx
} /* end of init_constants_1 */

static void module_body_0_0(Scheme_Env * env, Scheme_Per_Load_Statics *PLS, long phase_shift, Scheme_Object *self_modidx, Scheme_Per_Invoke_Statics_0 *PMIS)
{
    Scheme_Object * arg[27];
    Scheme_Thread * pr = scheme_current_thread;
    Scheme_Object ** tail_buf;
    { /* [71,2] */
        Scheme_Bucket * Gglg253MoD_pl_setup_page;
        Gglg253MoD_pl_setup_page = scheme_global_bucket(SYMBOLS[0], env);
        scheme_set_global_bucket("define-values", Gglg253MoD_pl_setup_page, S.const254, 1);
    }
    { /* [77,2] */
        Scheme_Bucket * Gglg255MoD_pl_set_device;
        Gglg255MoD_pl_set_device = scheme_global_bucket(SYMBOLS[1], env);
        scheme_set_global_bucket("define-values", Gglg255MoD_pl_set_device, S.const256, 1);
    }
    { /* [81,2] */
        Scheme_Bucket * Gglg257MoD_pl_set_output_file;
        Gglg257MoD_pl_set_output_file = scheme_global_bucket(SYMBOLS[2], env);
        scheme_set_global_bucket("define-values", Gglg257MoD_pl_set_output_file, S.const258, 1);
    }
    { /* [103,2] */
        Scheme_Bucket * Gglg259MoD_pl_init_plot;
        Gglg259MoD_pl_init_plot = scheme_global_bucket(SYMBOLS[3], env);
        scheme_set_global_bucket("define-values", Gglg259MoD_pl_init_plot, S.const260, 1);
    }
    { /* [109,2] */
        Scheme_Bucket * Gglg261MoD_pl_finish_plot;
        Gglg261MoD_pl_finish_plot = scheme_global_bucket(SYMBOLS[4], env);
        scheme_set_global_bucket("define-values", Gglg261MoD_pl_finish_plot, S.const262, 1);
    }
    { /* [114,2] */
        Scheme_Bucket * Gglg263MoD_pl_set_plot_environment;
        Gglg263MoD_pl_set_plot_environment = scheme_global_bucket(SYMBOLS[5], env);
        scheme_set_global_bucket("define-values", Gglg263MoD_pl_set_plot_environment, S.const264, 1);
    }
    { /* [118,2] */
        Scheme_Bucket * Gglg265MoD_pl_set_labels;
        Gglg265MoD_pl_set_labels = scheme_global_bucket(SYMBOLS[6], env);
        scheme_set_global_bucket("define-values", Gglg265MoD_pl_set_labels, S.const266, 1);
    }
    { /* [123,2] */
        Scheme_Bucket * Gglg267MoD_pl_plot_line;
        Gglg267MoD_pl_plot_line = scheme_global_bucket(SYMBOLS[7], env);
        scheme_set_global_bucket("define-values", Gglg267MoD_pl_plot_line, S.const268, 1);
    }
    { /* [130,2] */
        Scheme_Bucket * Gglg269MoD_pl_plot_segment;
        Gglg269MoD_pl_plot_segment = scheme_global_bucket(SYMBOLS[8], env);
        scheme_set_global_bucket("define-values", Gglg269MoD_pl_plot_segment, S.const270, 1);
    }
    { /* [134,2] */
        Scheme_Bucket * Gglg271MoD_pl_set_background_color;
        Gglg271MoD_pl_set_background_color = scheme_global_bucket(SYMBOLS[9], env);
        scheme_set_global_bucket("define-values", Gglg271MoD_pl_set_background_color, S.const272, 1);
    }
    { /* [138,2] */
        Scheme_Bucket * Gglg273MoD_pl_select_colormap0_index;
        Gglg273MoD_pl_select_colormap0_index = scheme_global_bucket(SYMBOLS[10], env);
        scheme_set_global_bucket("define-values", Gglg273MoD_pl_select_colormap0_index, S.const274, 1);
    }
    { /* [142,2] */
        Scheme_Bucket * Gglg275MoD_pl_set_colormap0_index;
        Gglg275MoD_pl_set_colormap0_index = scheme_global_bucket(SYMBOLS[11], env);
        scheme_set_global_bucket("define-values", Gglg275MoD_pl_set_colormap0_index, S.const276, 1);
    }
    { /* [146,2] */
        Scheme_Bucket * Gglg277MoD_pl_set_line_width;
        Gglg277MoD_pl_set_line_width = scheme_global_bucket(SYMBOLS[12], env);
        scheme_set_global_bucket("define-values", Gglg277MoD_pl_set_line_width, S.const278, 1);
    }
    { /* [153,2] */
        Scheme_Bucket * Gglg279MoD_pl_write_text;
        Gglg279MoD_pl_write_text = scheme_global_bucket(SYMBOLS[13], env);
        scheme_set_global_bucket("define-values", Gglg279MoD_pl_write_text, S.const280, 1);
    }
    { /* [159,2] */
        Scheme_Bucket * Gglg281MoD_pl_2d_contour_plot;
        Gglg281MoD_pl_2d_contour_plot = scheme_global_bucket(SYMBOLS[14], env);
        scheme_set_global_bucket("define-values", Gglg281MoD_pl_2d_contour_plot, S.const282, 1);
    }
    { /* [176,2] */
        Scheme_Bucket * Gglg283MoD_pl_2d_shade_plot;
        Gglg283MoD_pl_2d_shade_plot = scheme_global_bucket(SYMBOLS[15], env);
        scheme_set_global_bucket("define-values", Gglg283MoD_pl_2d_shade_plot, S.const284, 1);
    }
    { /* [192,2] */
        Scheme_Bucket * Gglg285MoD_pl_plot_points;
        Gglg285MoD_pl_plot_points = scheme_global_bucket(SYMBOLS[16], env);
        scheme_set_global_bucket("define-values", Gglg285MoD_pl_plot_points, S.const286, 1);
    }
    { /* [198,2] */
        Scheme_Bucket * Gglg287MoD_pl_x_error_bars;
        Gglg287MoD_pl_x_error_bars = scheme_global_bucket(SYMBOLS[17], env);
        scheme_set_global_bucket("define-values", Gglg287MoD_pl_x_error_bars, S.const288, 1);
    }
    { /* [204,2] */
        Scheme_Bucket * Gglg289MoD_pl_y_error_bars;
        Gglg289MoD_pl_y_error_bars = scheme_global_bucket(SYMBOLS[18], env);
        scheme_set_global_bucket("define-values", Gglg289MoD_pl_y_error_bars, S.const290, 1);
    }
    { /* [214,2] */
        Scheme_Bucket * Gglg291MoD_pl_world_3d;
        Gglg291MoD_pl_world_3d = scheme_global_bucket(SYMBOLS[19], env);
        scheme_set_global_bucket("define-values", Gglg291MoD_pl_world_3d, S.const292, 1);
    }
    { /* [220,2] */
        Scheme_Bucket * Gglg293MoD_pl_plot3d;
        Gglg293MoD_pl_plot3d = scheme_global_bucket(SYMBOLS[20], env);
        scheme_set_global_bucket("define-values", Gglg293MoD_pl_plot3d, S.const294, 1);
    }
    { /* [228,2] */
        Scheme_Bucket * Gglg295MoD_pl_mesh3d;
        Gglg295MoD_pl_mesh3d = scheme_global_bucket(SYMBOLS[21], env);
        scheme_set_global_bucket("define-values", Gglg295MoD_pl_mesh3d, S.const296, 1);
    }
    { /* [236,2] */
        Scheme_Bucket * Gglg297MoD_pl_mesh3dc;
        Gglg297MoD_pl_mesh3dc = scheme_global_bucket(SYMBOLS[22], env);
        scheme_set_global_bucket("define-values", Gglg297MoD_pl_mesh3dc, S.const298, 1);
    }
    { /* [273,2] */
        Scheme_Bucket * Gglg299MoD_pl_box3;
        Gglg299MoD_pl_box3 = scheme_global_bucket(SYMBOLS[23], env);
        scheme_set_global_bucket("define-values", Gglg299MoD_pl_box3, S.const300, 1);
    }
    { /* [279,2] */
        Scheme_Bucket * Gglg301MoD_pl_poly3;
        Gglg301MoD_pl_poly3 = scheme_global_bucket(SYMBOLS[24], env);
        scheme_set_global_bucket("define-values", Gglg301MoD_pl_poly3, S.const302, 1);
    }
} /* end of module_body_0_0 */

static void module_body_0_1(Scheme_Env * env, Scheme_Per_Load_Statics *PLS, long phase_shift, Scheme_Object *self_modidx, Scheme_Per_Invoke_Statics_0 *PMIS)
{
    Scheme_Object * arg[27];
    Scheme_Thread * pr = scheme_current_thread;
    Scheme_Object ** tail_buf;
    { /* [292,2] */
        Scheme_Bucket * Gglg303MoD_pl_line3;
        Gglg303MoD_pl_line3 = scheme_global_bucket(SYMBOLS[25], env);
        scheme_set_global_bucket("define-values", Gglg303MoD_pl_line3, S.const304, 1);
    }
    { /* [304,2] */
        Scheme_Bucket * Gglg305MoD_pl_fill;
        Gglg305MoD_pl_fill = scheme_global_bucket(SYMBOLS[26], env);
        scheme_set_global_bucket("define-values", Gglg305MoD_pl_fill, S.const306, 1);
    }
} /* end of module_body_0_1 */

static void module_syntax_body_0_0(Scheme_Env * env, Scheme_Per_Load_Statics *PLS, long phase_shift, Scheme_Object *self_modidx, Scheme_Per_Invoke_Syntax_Statics_0 *PMIS)
{
    Scheme_Object * arg[27];
    Scheme_Thread * pr = scheme_current_thread;
    Scheme_Object ** tail_buf;
    { /* [#f,#f] */
    }
} /* end of module_syntax_body_0_0 */

static void module_invoke_0(Scheme_Env *env, long phase_shift, Scheme_Object *self_modidx, void *pls)
{
    Scheme_Per_Invoke_Statics_0 *PMIS;
    PMIS = (Scheme_Per_Invoke_Statics_0 *)scheme_malloc(sizeof(Scheme_Per_Invoke_Statics_0));
    module_body_0_0(env, (Scheme_Per_Load_Statics *)pls, phase_shift, self_modidx, PMIS);
    module_body_0_1(env, (Scheme_Per_Load_Statics *)pls, phase_shift, self_modidx, PMIS);
}

static void module_invoke_syntax_0(Scheme_Env *env, long phase_shift, Scheme_Object *self_modidx, void *pls)
{
    Scheme_Per_Invoke_Syntax_Statics_0 *PMIS;
    PMIS = (Scheme_Per_Invoke_Syntax_Statics_0 *)scheme_malloc(sizeof(Scheme_Per_Invoke_Syntax_Statics_0));
    module_syntax_body_0_0(env, (Scheme_Per_Load_Statics *)pls, phase_shift, self_modidx, PMIS);
}

static Scheme_Object * top_level_0(Scheme_Env * env, Scheme_Per_Load_Statics *PLS)
{
    Scheme_Object * arg[27];
    Scheme_Thread * pr = scheme_current_thread;
    Scheme_Object ** tail_buf;
    { /* [1,0] */
        return scheme_declare_module(S.const314, module_invoke_0, module_invoke_syntax_0, PLS, SCHEME_CURRENT_ENV(pr));
    }
} /* end of top_level_0 */

Scheme_Object * scheme_reload(Scheme_Env * env)
{
    Scheme_Per_Load_Statics *PLS;
    PLS = (Scheme_Per_Load_Statics *)scheme_malloc(sizeof(Scheme_Per_Load_Statics));
    return top_level_0(env, PLS);
}


void scheme_setup(Scheme_Env * env)
{
    scheme_set_tail_buffer_size(1);
    gc_registration();
    make_symbols();
    make_syntax_strings();
    init_prims(env);
    init_constants_0(env);
    init_constants_1(env);
}


Scheme_Object * scheme_initialize(Scheme_Env * env)
{
    scheme_setup(env);
    return scheme_reload(env);
}


Scheme_Object * scheme_module_name()
{
    return scheme_intern_exact_symbol("plplot-low-level", 16);
}
