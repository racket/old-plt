(module contract-helpers mzscheme
  (provide module-source-as-symbol)
  
  ;; module-source-as-symbol : syntax -> symbol
  ;; constructs a symbol for use in the blame error messages
  ;; when blaming the module where stx's occurs.
  (define (module-source-as-symbol stx)
    (let ([src-module (syntax-source-module stx)])
      (cond
        [(symbol? src-module) src-module]
        [(module-path-index? src-module) 
         (let-values ([(module-path mpi) (module-path-index-split src-module)])
           (string->symbol (format "~s" module-path)))]
        [else 'top-level]))))
