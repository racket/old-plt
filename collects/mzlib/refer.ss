
(define-macro begin-construction-time 
  (lambda body 
    `(#%begin-elaboration-time ,@body)))

(begin-elaboration-time
 (invoke-open-unit
  (require-library "referf.ss")))

(define-macro require-library-unit/sig (require-unit #t #t #f #t 'require-library-unit/sig))
(define-macro require-library-unit (require-unit #t #t #f #f 'require-library-unit))
(define-macro require-relative-library-unit/sig (require-unit #t #t #t #t 'require-relative-library-unit/sig))
(define-macro require-relative-library-unit (require-unit #t #t #t #f 'require-relative-library-unit))
(define-macro require-unit/sig (require-unit #t #f #f #t 'require-unit/sig))
(define-macro require-unit (require-unit #t #f #f #f 'require-unit))

(define-macro require-file (require-file #t #f #f))

(require-library "spidey.ss")
