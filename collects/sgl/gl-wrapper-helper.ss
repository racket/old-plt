(module gl-wrapper-helper mzscheme
  (provide (all-defined-except gl-version path))
  
  (define path '(lib "gl-prims.ss" "sgl"))
  
  (define gl-version (dynamic-require path 'gl-version))
  
  (define-syntax (r stx)
    (syntax-case stx ()
      ((_ path name)
       #`(define name (dynamic-require path 'name)))))
  
  (define-syntax (r12f stx)
    (syntax-case stx ()
      ((_ path name)
       #`(define name (if (>= gl-version 1.2)
                          (dynamic-require path 'name)
                          (lambda x
                            (error 'name "requires GL version 1.2 or higher")))))))
  
  (define-syntax (r13f stx)
    (syntax-case stx ()
      ((_ path name)
       #`(define name (if (>= gl-version 1.3)
                          (dynamic-require path 'name)
                          (lambda x
                            (error 'name "requires GL version 1.3 or higher")))))))
  
  (define-syntax (r12s stx)
    (syntax-case stx ()
      ((_ path name)
       #`(define name (if (>= gl-version 1.2)
                          (with-handlers ((exn:application:mismatch? (lambda (x) -1)))
                            (dynamic-require path 'name))
                          -1)))))
  (define-syntax (r13s stx)
    (syntax-case stx ()
      ((_ path name)
       #`(define name (if (>= gl-version 1.3)
                          (with-handlers ((exn:application:mismatch? (lambda (x) -1)))
                            (dynamic-require path 'name))
                          -1)))))
  )

