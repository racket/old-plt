(module sig mzscheme
  (require (lib "unitsig.ss"))
  (provide get-info^
           help-window^)
  
  (define-signature get-info^
    (get-language-level
     get-teachpack-names))
  
  (define-signature help-window^
    (new-help-frame
     open-url-from-user
     set-font-size)))