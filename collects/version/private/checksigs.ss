(module checksigs mzscheme
  (require (lib "unitsig.ss"))	

  (provide empty^ defs^)

  (define-signature empty^
    ())

  (define-signature defs^
    (get-yes-no
     show-ok 
     show-error-ok)))

