(module compile-python mzscheme
  
  (require (lib "class.ss")
           (lib "etc.ss")
           (lib "contract.ss")
           "compiler.ss"
           "runtime-support.ss"
           "read-python.ss")

  (provide/contract
   [compile-python ((listof (is-a?/c ast-node%)) . -> . (listof syntax?))]
   [compile-python-ast ((is-a?/c ast-node%) . -> . syntax?)]
   [py-gen/file (string? . -> . (listof syntax?))]
   [py-gen/str (string? namespace? . -> . (listof syntax?))]
   [py-gen/port (input-port? string? namespace? . -> . (listof syntax?))])
  
  
  (define (compile-python ast-list)
    (with-compilation-contexts
     (map compile-python-ast/fast
          ast-list)))
  
  (define (compile-python-ast ast)
    (with-compilation-contexts
     (compile-python-ast/fast ast)))
  
  (define (compile-python-ast/fast ast)
    (syntax-as-top (send ast to-scheme)))
  
  (define-syntax (with-compilation-contexts stx)
    (syntax-case stx ()
      [(_ bodies ...) #`(parameterize ([current-runtime-support-context #'here]
                                       [current-toplevel-context #f]) ;base-importing-stx])
                          ;                  (eval '(require "c-bindings.ss"))
                          bodies ...)]))

  (define (py-gen/file path)
    (compile-python (read-python path)))
  
  (define (py-gen/str str ns)
    (let ([is (open-input-string str)])
      (begin0 (py-gen/port is "input string" ns)
              (close-input-port is))))

  (define (py-gen/port port port-name ns)
    (parameterize ([current-namespace ns])
      (compile-python (read-python-port port port-name))))

  
  (define (syntax-as-top s)
    (if (syntax? s)
        (namespace-syntax-introduce s)
        s))

  
  )
