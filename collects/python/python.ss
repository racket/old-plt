(module python mzscheme
  (require (lib "class.ss")
          ; (lib "list.ss")
           (lib "etc.ss")
           (lib "contract.ss")
          ; "compiler.ss"
           "python-node.ss"
          ; "primitives.ss" ;; need py-object%->string
          ; "read-python.ss"
           "compile-python.ss"
           "python-import.ss"
           ;"base.ss"
           "runtime-support.ss"
           "c-bindings.ss"
           ;"get-base.ss"
           )

  (provide in-ns in-python)

  (provide/contract
   [python (string? . -> . (listof string?))]
   [py-eval (string? . -> . (listof string?))]
   [py-gen (string? . -> . (listof syntax?))]
   [ssd (syntax? . -> . (union pair? symbol? number? string?))]
   [ssd* ((listof syntax?) . -> . (listof (union pair? symbol? number? string?)))]
   [python-ns parameter?]
   [ns-set! (symbol? any? . -> . void?)]
   [ns-get (symbol? . -> . any?)]
   [render-python-value ((union python-node? void?) port? procedure? . -> . void?)]
   [render-python-value/format ((union python-node? void?) port? procedure? . -> . void?)])
  
  (define ssd syntax-object->datum)
  
  (define (ssd* lst) (map ssd lst))
  
  (define (convert-value value)
    ;(namespace-require '(lib "primitives.ss" "python"))
    ;((namespace-variable-value 'py-object%->string) value))
    (get-py-string (py-object->py-string value)))
   ; ((dynamic-require '(lib "primitives.ss" "python") 'py-object%->string) value))

  (define (none? py-value)
    (eqv? py-value py-none))
;    ((dynamic-require '(lib "primitives.ss" "python") 'py-none?) py-value))

  (define (render-python-value/format value port port-write)
    (render-python-value value port port-write))

  (define (render-python-value value port port-write)
    (unless (none? value)
      (let ([to-render (if (python-node? value)
                           (convert-value value)
                           value)])
        (if #f ;port-write
            (port-write to-render)
            (display to-render port)))))

  (define pns (make-python-namespace))
  ;(set-python-namespace-name! pns '__main__)

 ; (set-pyns! pns)

  (define python-ns (make-parameter pns))
  (define python-ss-module-ns (current-namespace))
  
;  (define (python-ns)
;    pns)

  #|
  (define (load-ps)
    (parameterize ([current-namespace pns])
      (load-extension (build-path (this-expression-source-directory)
                                  "c" "stringobject.so")))
    (copy-namespace-bindings pns (current-namespace) #f #f))
|#
  
  (define (py-eval str)
    (map convert-value
         (eval-python (py-gen str) (python-ns))))
  
  ;(require "base.ss")
  ;(dynamic-require "base.ss" #f #f)
  
  (define (py-gen str)
    (py-gen/str str (python-ns)))
  
  (define (python path)
    (map convert-value
         (parameterize ([current-namespace (python-ns)])
           (let ([sol (py-gen/file path)])
             (unless (andmap syntax? sol)
               (error 'python "epexted syntax object list"))
             (eval-python sol (python-ns))))))
;           (eval-python (py-gen/file path) (python-ns)))))

  (define-syntax (in-ns stx)
    (syntax-case stx ()
      [(_ ns exprs ...) #`(parameterize ([current-namespace ns])
                            exprs ...)]))
  
  (define-syntax (in-python stx)
    (syntax-case stx ()
      [(_ exprs ...) #`(parameterize ([current-namespace pns])
                         exprs ...)]))
  
  (define ns-set! namespace-set-variable-value!)

  (define ns-get namespace-variable-value)
  
  )
