(module read-python mzscheme
  (require "parser.ss"
           "compiler.ss"
           "compiler-stmt.ss" ; for module-scope%
           ;"base.ss"
           ;(lib "mred.ss" "mred") ; for the text% in the contracts...
           (lib "contract.ss")
           (lib "class.ss"))
  
  (provide/contract (read-python (string? . -> . (listof (is-a?/c ast-node%)))))
  (provide ;read-python read-python-port)
            read-python-port)
;                    (read-python-port (input-port? (union (is-a?/c text%) string?) . -> . (listof (is-a?/c ast-node%)))))
  
  (define (read-python path)
    (init-bindings (build-ast-from-file path)))
  
  (define (read-python-port port name)
    (init-bindings (build-ast-from-port port name)))

  (define (init-bindings ast-l)
    (let ([scope (make-object module-scope%)])
      (for-each (lambda (a) (send a set-bindings! scope))
                ast-l)
      (for-each (lambda (a) (send a check-break/cont #f))
                ast-l)
      ast-l))


  )