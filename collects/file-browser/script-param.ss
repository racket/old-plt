(module script-param mzscheme
  (require (lib "unitsig.ss")
	   "file-system.ss")
  
  (provide script-unit-param)
  (define script-unit-param (make-parameter null)))