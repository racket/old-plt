(module read-python mzscheme
  (require "parser.ss"
           "compiler.ss"
           (lib "mred.ss" "mred")
           (lib "contracts.ss")
           (lib "class.ss"))
  
  (provide/contract (read-python (string? . -> . (listof (is-a?/c ast-node%))))
                    (read-python-port (input-port? (union (is-a?/c text%) string?) . -> . (listof (is-a?/c ast-node%)))))
  
  (define (read-python path)
    (let ((ast (build-ast-from-file path)))
      (for-each (lambda (a) (send a set-bindings! #f)) ast)
      ast))
  
  (define (read-python-port port name)
    (let ((ast (build-ast-from-port port name)))
      (for-each (lambda (a) (send a set-bindings! #f)) ast)
      ast))
  
  )