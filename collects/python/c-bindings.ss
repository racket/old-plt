(module c-bindings mzscheme
  (require (lib "list.ss")
           (lib "etc.ss"))
  
  (require-for-syntax (lib "etc.ss"))
  
  ;(require-for-syntax "builtin-types-uninitialized.ss")
  ;(require "builtin-types-uninitialized.ss")
  
  (namespace-require "builtin-types-uninitialized.ss")
  (namespace-require "python-node.ss")
  
  (define-syntax (nss stx)
    (syntax-case stx ()
      [(nss (fn args ...) bodies ...)
       #`(nss fn (lambda (args ...) bodies ...))]
      [(nss id value) #`(namespace-set-variable-value! 'id value)]))
  
  (nss python-ns (let ([ns (current-namespace)])
                   (lambda () ns)))
  
  ;(nss (py-ext-init-module 
  
  (define sdir (this-expression-source-directory))
  
  (define path (build-path sdir
                           "c"
                           "cpy-bindings.so"))

  ;; das ist ein Hack.. entschuldigen Sie, bitte...
  
  (nss (spy-add-cpython-type idd tobj)
       (namespace-set-variable-value! (string->symbol
                                       (format "cpy-~a" idd))
                                      tobj))
  
  (load-extension path)
  

;  (define names (filter (lambda (name)
;                          (regexp-match #rx"^spy-" name))
;                        (map symbol->string (namespace-mapped-symbols))))
 
  (define-values (cpy-object
                  cpy-str
                  cpy-list)
    (apply values
           (map eval
                '(cpy-object
                  cpy-str
                  cpy-list))))
  
   
  (provide cpy-object
           cpy-str
           cpy-list
           )
  )
