(module sig mzscheme
  (require (lib "unitsig.ss"))
  (provide gui^)
  
  (define-signature gui^
    (new-help-desk
     show-help-desk
     search-for-docs)))
