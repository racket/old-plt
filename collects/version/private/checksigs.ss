(module checksigs mzscheme
  (require (lib "unitsig.ss"))	

  (provide empty^ check-frame^ defs^)

  (define-signature empty^
    ())

  (define-signature check-frame^
    (check-frame))
	
  (define-signature defs^
    (run-thunk
     show-ok 
     show-error-ok
     make-wait-dialog
     show-wait-dialog
     hide-wait-dialog)))



