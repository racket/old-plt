(module sig mzscheme
  (require (lib "unitsig.ss"))
  (provide get-info^
           search^
           help-window^)
  
  (define-signature get-info^
    (get-language-level
     get-teachpack-names))
  
  (define-signature search^
    (do-search
     doc-collections-changed))
  
  (define-signature help-window^
    (new-help-frame
     open-url-from-user
     set-font-size)))