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

 Scheme_Object * array_to_list(double * dbls, int length)
  {	
    int i;
    Scheme_Object * result = scheme_null;
    for(i = length - 1; i >= 0 ;i--)    {
 	result = scheme_make_pair(scheme_make_double(dbls[i]),result);
       }	
    return result;
  }
 ")
  
 (define fit-internal 
   (c-lambda 
    (scheme-object scheme-object scheme-object
                   scheme-object scheme-object scheme-object) scheme-object
                            "
 {                            
 double * result_params = do_fit(___arg1,
                                 scheme_list_length(___arg2),
                                 list_to_array(___arg2),
                                 list_to_array(___arg3),
                                 list_to_array(___arg4),
                                 list_to_array(___arg5),
                                 scheme_list_length(___arg6),
                                 list_to_array(___arg6));
 // now make the result_params into a list
 

 if(result_params == NULL)
	{
 	___result =  scheme_null;
	}
 else {

 int len = scheme_list_length(___arg6);

 Scheme_Object * fit_final_params = 
	array_to_list(result_params, len);

 Scheme_Object * fit_asym_error = 
	array_to_list (get_asym_error(), len);

 Scheme_Object * fit_asym_error_percent =  
	array_to_list (get_asym_error_percent(), len);

 Scheme_Object * fit_rms = scheme_make_double(get_rms());
 Scheme_Object * fit_varience = scheme_make_double(get_varience());

 ___result = 
	scheme_make_pair(fit_final_params,
         scheme_make_pair(fit_asym_error,	
          scheme_make_pair(fit_asym_error_percent,
	   scheme_make_pair(fit_rms,
	    scheme_make_pair(fit_varience, scheme_null)))));
	}
 }
"))	
   
  (provide fit-internal))
 
  
