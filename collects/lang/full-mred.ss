(module full-mred mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (define argv #())
  (define program (namespace-variable-binding 'program))
  
  (provide argv 
           program
           (all-from mzscheme)
           (all-from (lib "class.ss"))
	   (all-from (lib "mred.ss" "mred"))))
