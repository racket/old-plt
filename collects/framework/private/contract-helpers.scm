(module contract-helpers mzscheme
  (require (lib "file.ss")
           (lib "moddep.ss" "syntax"))

  (provide module-source-as-symbol)

  ;; module-source-as-symbol : syntax -> symbol
  ;; constructs a symbol for use in the blame error messages
  ;; when blaming the module where stx's occurs.
  (define (module-source-as-symbol stx)
    (let ([src-module (syntax-source-module stx)])
      (cond
        [(symbol? src-module) src-module]
        [(module-path-index? src-module) 
	 (let/ec k
	   (resolve-module-path-index
	    src-module
	    (lambda () (k 'top-level))))]
        [else 'top-level]))))
