(module read-python mzscheme
  (require "parser.ss"
           "compiler.ss"
           "compiler-stmt.ss" ; for module-scope%
           "base.ss"
           (lib "mred.ss" "mred")
           (lib "contracts.ss")
           (lib "class.ss"))
  
  (provide/contract (read-python (string? . -> . (listof (is-a?/c ast-node%))))
                    (read-python-port (input-port? (union (is-a?/c text%) string?) . -> . (listof (is-a?/c ast-node%)))))
  
  (define (read-python path)
    (init-bindings (build-ast-from-file path)))
  
  (define (read-python-port port name)
    (init-bindings (build-ast-from-port port name)))

  (define (init-bindings ast-l)
    (let ([scope (make-object module-scope%)])
      (for-each (lambda (a) (send a set-bindings! scope))
                ast-l)
      ast-l))


  )