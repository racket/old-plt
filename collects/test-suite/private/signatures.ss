(module signatures mzscheme
  (require
   (lib "unitsig.ss"))
  (provide
   tool-phases^
   drscheme-extentions^
   window^
   model^
   case^
   def^
   expand-program^)
  
  (define-signature tool-phases^ (phase1 phase2))
  (define-signature drscheme-extentions^ ())
  
  (define-signature window^ (window%))
  (define-signature model^ (model%))
  
  (define-signature case^ (case%))
  (define-signature def^ (def%))
  (define-signature expand-program^ (expand-program%))
  )