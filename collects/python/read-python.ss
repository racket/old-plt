(module read-python mzscheme
  (require "parser.ss"
           "compiler.ss"
           (lib "contracts.ss")
           (lib "class.ss"))
  
  (provide/contract (read-python (string? . -> . (listof (is-a?/c ast-node%)))))
  
  (define (read-python path)
    (let ((ast (build-ast-from-file path)))
      (for-each (lambda (a) (send a set-bindings! #f)) ast)
      ast))
  )