(module python mzscheme
  (require (lib "class.ss")
           "read-python.ss"
           "runtime-support.ss")
  
  ;;;; temporary Python Evaluation module by Daniel ;;;;;;;
  
  (provide python
           read-python
           python-to-scheme
           compile-python
           parse-python-port
           parse-python-file)
  
  (define (python-to-scheme path)
    (compile-python (read-python path)))

  (define (compile-python ast-list)
    `(begin ,@(map (lambda (ast)
                     (send ast to-scheme))
                   ast-list)))
  
  (define parse-python-port read-python-port)
  
  (define parse-python-file read-python)
  
  (define (python path)
    (let ([results (map eval (python-to-scheme path))])
      (begin
        (for-each (lambda (x)
                    (display x) (newline))
                  results)
        (list-ref results (sub1 (length results))))))
  
  )