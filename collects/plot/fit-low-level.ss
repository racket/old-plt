(module fit-low-level mzscheme
  (require 
   (lib "cffi.ss" "compiler")
   (lib "etc.ss")
   (lib "list.ss"))
  
 (c-declare "#include \"fit.h\"")
 
 (c-declare "
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
 ")
  
 (define fit-internal 
   (c-lambda 
    (scheme-object scheme-object scheme-object
                   scheme-object scheme-object scheme-object) scheme-object
                            "
 double * result_params = do_fit(___arg1,
                                 scheme_list_length(___arg2),
                                 list_to_array(___arg2),
                                 list_to_array(___arg3),
                                 list_to_array(___arg4),
                                 list_to_array(___arg5),
                                 scheme_list_length(___arg6),
                                 list_to_array(___arg6));
 // now make the result_params into a list
 
 int i;
 Scheme_Object * pair = scheme_null;
 
 if(result_params == NULL)
 return scheme_null;
 
 for(i = scheme_list_length(___arg6) - 1; i >= 0 ;i--)    {
       pair = scheme_make_pair(scheme_make_double(result_params[i]),pair);
       }

___result = pair;
"))
   
  (provide fit-internal))
 
  