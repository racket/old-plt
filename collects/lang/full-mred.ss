(module full-mred mzscheme
  (require (lib "mred.ss" "mred"))
  
  (provide (all-from mzscheme)
	   (all-from (lib "mred.ss" "mred"))))
