(module checksigs mzscheme
  (require (lib "unitsig.ss"))	

  (provide empty^ defs^ args^ progname^)

  (define-signature empty^
    ())

  (define-signature args^
    (collections))

  (define-signature progname^
    (progname))

  (define-signature defs^
    ((open progname^)
     get-yes-no
     show-ok 
     show-error-ok)))


