(module c-bindings mzscheme
  (require (lib "list.ss")
           (lib "file.ss")
           (lib "etc.ss"))
  
  (define-syntax (nss stx)
    (syntax-case stx ()
      [(nss (fn args ...) bodies ...)
       #`(nss fn (lambda (args ...) bodies ...))]
      [(nss id value) #`(namespace-set-variable-value! 'id value)]))
  
  (define python-ns (let ([ns (current-namespace)])
                      (lambda () ns)))
  
  
  (define sdir (this-expression-source-directory))
  
  (define path (build-path sdir
                           "c"
                           "cpy-bindings.so"))

  ;; das ist ein Hack.. entschuldigen Sie, bitte...
  
  (define (spy-add-cpython-type idd tobj)
       (namespace-set-variable-value! (string->symbol
                                       (format "cpy-~a" idd))
                                      tobj))

  (define (here-path fname)
    (find-relative-path (current-directory) (build-path sdir fname)))

  (define gns (current-namespace))
  
  (define (load-c-spy ns)
    (parameterize ([current-namespace gns])
      ;(namespace-require (here-path "builtin-types-uninitialized.ss"))
      (namespace-require (here-path "python-node.ss"))
      (namespace-set-variable-value! 'python-ns python-ns)
      (namespace-set-variable-value! 'spy-add-cpython-type spy-add-cpython-type)
      (load-extension path)
      ))
  
  (load-c-spy (current-namespace))
  

;  (define names (filter (lambda (name)
;                          (regexp-match #rx"^spy-" name))
;                        (map symbol->string (namespace-mapped-symbols))))

  (define-syntax (export-from-c stx)
    (syntax-case stx ()
      [(_ ids ...)
       #`(begin
           (define-values (ids ...)
             (apply values (map eval '(ids ...))))
           (provide ids ...))]))

  (export-from-c cpy-object
                 cpy-str
                 cpy-list
                 make-py-list
                 make-py-string
                 make-py-symbol
                 make-py-tuple
                 make-py-number
                 make-py-code
                 make-py-function
                 py-object->py-string
                 get-py-string
                 get-py-number
                 get-py-list
                 get-py-tuple
                 get-py-dict
                 get-py-file
                 spy-cpython-apply
                 spy-cpython-instanceof
                 spy-cpython-number?
                 spy-cpython-string?
                 spy-cpython-tuple?
                 spy-cpython-list?
                 spy-cpython-get-type-name
                 spy-cpython-getattr/obj
                 spy-cpython-getattr/str
                 spy-cpython-getattr/sym
                 cpy-none
                 )
  
  #|
  (define-values (cpy-object
                  cpy-str
                  cpy-list
                  make-py-list
                  make-py-string
                  make-py-tuple
                  make-py-number
                  make-py-code
                  make-py-function
                  py-object->py-string
                  get-py-string
                  spy-cpython-apply
                  )
    (apply values
           (map eval
                '(cpy-object
                  cpy-str
                  cpy-list
                  make-py-list
                  make-py-string
                  make-py-tuple
                  make-py-number
                  make-py-code
                  make-py-function
                  py-object->py-string
                  get-py-string
                  spy-cpython-apply
                  ))))
  
   
  (provide load-c-spy
           cpy-object
           cpy-str
           cpy-list
           make-py-list
           make-py-string
           make-py-tuple
           make-py-number
           make-py-code
           make-py-function
           py-object->py-string
           get-py-string
           spy-cpython-apply
           )|#
  )
