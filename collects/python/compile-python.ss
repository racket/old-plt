(module compile-python mzscheme
  
  (require (lib "class.ss")
           "read-python.ss")
  
  (provide compile-python
           compile-python-ast
           (rename read-python parse-python-file)
           (rename read-python-port parse-python-port)
           python-to-scheme)
  
    (define (python-to-scheme path)
      (compile-python (read-python path)))
  
  (define (compile-python ast-list)
    (map compile-python-ast
         ast-list))
  
  (define (compile-python-ast ast)
    (send ast to-scheme))
  
;  (define parse-python-port read-python-port)
  
;  (define parse-python-file read-python)
  
  )
