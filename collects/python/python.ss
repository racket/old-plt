(module python mzscheme
  (require (lib "class.ss")
          ; (lib "list.ss")
           (lib "etc.ss")
          ; "compiler.ss"
           "python-node.ss"
           "primitives.ss" ;; need py-object%->string
          ; "read-python.ss"
           "compile-python.ss"
           "python-import.ss"
           ;"base.ss"
           "runtime-support.ss"
           "get-base.ss"
           )


  (provide python load-ps py-eval
           ;read-python
           ;python-to-scheme
           ;compile-python
           ;compile-python-ast
           ;parse-python-port
           ;parse-python-file
           render-python-value
           render-python-value/format)

  (define (convert-value value)
   ; (py-object%->string value))
    (namespace-require '(lib "primitives.ss" "python"))
    ((namespace-variable-value 'py-object%->string) value))
   ; ((dynamic-require '(lib "primitives.ss" "python") 'py-object%->string) value))

  (define (none? py-value)
    ((dynamic-require '(lib "primitives.ss" "python") 'py-none?) py-value))

  (define (render-python-value/format value port port-write)
    (render-python-value value port port-write))

  (define (render-python-value value port port-write)
    (unless (none? value)
      (let ([to-render (if (python-node? value)
                           (convert-value value)
                           value)])
        ;(if (python-node? value)
        ;   (format "~a" (py-object%->string value))
        ;   value)])
        (if #f ;port-write
            (port-write to-render)
            (display to-render port)))))

  (define pns (make-python-namespace))
  (set-python-namespace-name! pns '__main__)

  (define (load-ps)
    (parameterize ([current-namespace pns])
      (load-extension (build-path (this-expression-source-directory)
                                  "c" "stringobject.so")))
    (copy-namespace-bindings pns (current-namespace) #f #f))

  (define (py-eval str)
    (let ([is (open-input-string str)])
      (begin0 (map convert-value
                   (eval-python&copy (parameterize ([current-runtime-support-context #'here]
                                                    [current-toplevel-context base-importing-stx])
                                       (compile-python (parse-python-port is "input string"))) pns))
              (close-input-port is))))
  
  (define (python path)
    (let ([results (eval-python&copy (parameterize ([current-runtime-support-context #'here]
                                                    [current-toplevel-context base-importing-stx])
                                       (python-to-scheme path)) pns)])
      ;(let ([port (current-output-port)])
      ;  (for-each (lambda (value)
      ;              (render-python-value value port printf))
      ;            results))
      (map convert-value results)))

  (define-syntax (in-python stx)
    (syntax-case stx ()
      [(_ expr) #`(parameterize ([current-namespace pns])
                    expr)]))
  
  (define ns-set! namespace-set-variable-value!)

  (define ns-get namespace-variable-value)
  
  )
