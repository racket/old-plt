
(define-macro begin-elaboration-time 
  (lambda body 
    (eval `(begin ,@body))))

(define-macro begin-construction-time 
  (lambda body 
    (eval `(begin ,@body))))

(begin-elaboration-time
 (define-values (reference-unit reference) (invoke-unit (reference-library "referf.ss"))))

(define-macro reference-library-unit/sig (reference-unit #t #t #t 'reference-library-unit/sig))
(define-macro reference-library-unit (reference-unit #t #t #f 'reference-library-unit))
(define-macro reference-unit/sig (reference-unit #t #f #t 'reference-unit/sig))
(define-macro reference-unit (reference-unit #t #f #f 'reference-unit))

(define-macro reference (reference #t #f))

(reference-library "spidey.ss")
