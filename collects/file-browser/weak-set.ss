(module weak-set mzscheme
  (provide make-weak-set weak-set-add! weak-set-for-each!)
  
  (define-struct ws (head))
  
  (define (make-weak-set)
    (make-ws null))
  
  (define (weak-set-add! x ws)
    (set-ws-head! ws (cons (make-weak-box x) (ws-head ws))))
  
  (define (weak-set-for-each! f ws)
    (letrec ((for-each
              (lambda (l)
                (cond
                  ((null? l) null)
                  (else
                   (let ((c (weak-box-value (car l))))
                     (cond
                       (c 
                        (f c)
                        (cons (car l) (for-each (cdr l))))
                       (else
                        (for-each (cdr l))))))))))
      (set-ws-head! ws (for-each (ws-head ws))))))
                   