(module gl-wrapper-helper mzscheme
  (provide (all-defined-except gl-version glu-version path))
  
  (define path '(lib "gl-prims.ss" "sgl"))
  
  (define gl-version (dynamic-require path 'gl-version))
  (define glu-version (dynamic-require path 'glu-version))
  
  (define-syntax r
    (syntax-rules ()
      ((_ path name)
       (define name (dynamic-require path 'name)))))
  
  (define-for-syntax (make-rxf version num x y)
    (lambda (stx)
      (syntax-case stx ()
        ((_ path name)
         #`(define name (if (>= #,version #,num)
                            (dynamic-require path 'name)
                            (lambda x
                              (error 'name 
                                     (format "requires ~a version ~a or higher" #,x #,y)))))))))
  
  
  (define-syntax r12f (make-rxf #'gl-version 12 "GL" "1.2"))
  (define-syntax r13f (make-rxf #'gl-version 13 "GL" "1.3"))
  (define-syntax glu-r12f (make-rxf #'glu-version 12 "GLU" "1.2"))
  (define-syntax glu-r13f (make-rxf #'glu-version 13 "GLU" "1.3"))
  
  
  (define-syntax r12s
    (syntax-rules ()
      ((_ path name)
       (define name (if (>= gl-version 1.2)
                        (with-handlers ((exn:fail? (lambda (x) -1)))
                          (dynamic-require path 'name))
                        -1)))))
  (define-syntax r13s
    (syntax-rules ()
      ((_ path name)
       (define name (if (>= gl-version 1.3)
                        (with-handlers ((exn:fail? (lambda (x) -1)))
                          (dynamic-require path 'name))
                        -1)))))
  )

