(module sig mzscheme
  (require (lib "unitsig.ss"))
  (provide gui^ internal-hp^)
  
  (define-signature internal-hp^
    (internal-host
     internal-port))
  
  (define-signature gui^
    (new-help-desk)))
