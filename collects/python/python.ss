(module python mzscheme
  (require (lib "class.ss")
           "read-python.ss"
           "runtime-support.ss")
  
  ;;;; temporary Python Evaluation module by Daniel ;;;;;;;
  
  (provide python
           read-python
           python-to-scheme)
  
  (define (python-to-scheme path)
    (map (lambda (ast)
           (send ast to-scheme))
         (read-python path)))
  
  (define (python path)
    (let ([results (map eval (python-to-scheme path))])
      (begin
        (for-each (lambda (x)
                    (display x) (newline))
                  results)
        (list-ref results (sub1 (length results))))))
  
  )