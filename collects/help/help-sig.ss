(module help-sig mzscheme
  (require (lib "unitsig.ss")
           "private/sig.ss")
  (provide help:doc-position^
           help^)
  
  (define-signature help:doc-position^
    (user-defined-doc-position))
  
  (define-signature help^
    ((open help-window^)
     doc-collections-changed)))