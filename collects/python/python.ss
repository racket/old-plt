(module python mzscheme
  (require (lib "class.ss")
           "compiler.ss"
           "read-python.ss"
           "base.ss")
  
  ;;;; temporary Python Evaluation module by Daniel ;;;;;;;
  
  (provide python
           read-python
           python-to-scheme
           compile-python
           compile-python-ast
           parse-python-port
           parse-python-file)
  
  (define (python-to-scheme path)
    (compile-python (read-python path)))

  (define (compile-python ast-list)
;    (set-context! #'here)
         (map (lambda (ast)
                (send ast to-scheme))
              ast-list))
  
  (define (compile-python-ast ast)
    (send ast to-scheme))
  
  (define parse-python-port read-python-port)
  
  (define parse-python-file read-python)
  
  (define (python path)
      (let ([m-path ((current-module-name-resolver) '(lib "base.ss" "python") #f #f)]
            [empty-namespace (make-namespace 'empty)]
            [n (current-namespace)])
        (dynamic-require m-path #f)
        (parameterize ([current-namespace empty-namespace])
                          (namespace-attach-module n m-path)
                          (namespace-require m-path)
                          (map eval
                               (python-to-scheme path)))))
  
  )