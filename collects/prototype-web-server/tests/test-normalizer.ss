(require "../normalizer.ss")

(define (empty-env var)
  (error "empty environment"))

(define (extend env vars vals)
  (lambda (var0)
    (let loop ([vars vars]
               [vals vals])
      (cond
        [(null? vars) (env var0)]
        [(eqv? var0 (car vars))
         (car vals)]
        [else (loop (cdr vars) (cdr vals))]))))

;; alpha=/env: environment target-expr target-expr -> boolean
;; are two target expressions alpha-equivalent?
(define (alpha=/env env1 env2 expr1 expr2)
  (syntax-case expr1 (if #%app)
    [(if tst1 csq1)
     (syntax-case expr2 (if)
       [(if tst2 csq2) (and (alpha=/env env1 env2 #'tst1 #'tst2)
                            (alpha=/env env1 env2 #'csq1 #'csq2))]
       [_else #f])]
    [(if tst1 csq1 alt1)
     (syntax-case expr2 (if)
       [(if tst2 csq2 alt2) (and (alpha=/env env1 env2 #'tst1 #'tst2)
                                 (alpha=/env env1 env2 #'csq1 #'csq2)
                                 (alpha=/env env1 env2 #'alt1 #'alt2))]
       [_else #f])]
    [(#%app rator1 rands1 ...)
     (syntax-case expr2 (#%app)
       [(#%app rator2 rands2 ...)
        (and (alpha=/env env1 env2 #'rator1 #'rator2)
             (let loop ([rs1 (syntax->list #'(rands1 ...))]
                        [rs2 (syntax->list #'(rands2 ...))])
               (or (and (null? rs1)
                        (null? rs2))
                   (and (alpha=/env env1 env2 (car rs1) (car rs2))
                        (loop (cdr rs1) (cdr rs2))))))]
       [_else #f])]
    [_else (w-alpha=/env env1 env2 expr1 expr2)]))

;; w-alpha=/env: env target-expr target-expr -> boolean
;; are two target vars or vals alpha-equivalent?
(define (w-alpha=/env env1 env2 expr1 expr2)
  (syntax-case expr1 (#%top #%datum lambda quote)
    [(#%top . var1)
     (syntax-case expr2 (#%top)
       [(#%top . var2)
        (eqv? (syntax-object->datum #'var1)
              (syntax-object->datum #'var2))]
       [_else #f])]
    [(#%datum . datum1)
     (syntax-case expr2 (#%datum)
       [(#%datum . datum2)
        (let ([dat1 (syntax-object->datum #'datum1)]
              [dat2 (syntax-object->datum #'datum2)])
          (eqv? dat1 dat2))]
       [_else #f])]
    [(quote datum1)
     (syntax-case expr2 (quote)
       [(quote datum2)
        (let ([dat1 (syntax-object->datum #'datum1)]
              [dat2 (syntax-object->datum #'datum2)])
          (equal? dat1 dat2))]
       [_else #f])]
    [(lambda (formals1 ...) body1)
     (syntax-case expr2 (lambda)
       [(lambda (formals2 ...) body2)
        (let ([syms (map gensym (syntax->symbols #'(formals1 ...)))])
          (and (= (length syms) (length (syntax->list #'(formals2 ...))))
               (alpha=/env
                (extend env1 (syntax->symbols #'(formals1 ...)) syms)
                (extend env2 (syntax->symbols #'(formals2 ...)) syms)
                #'body1 #'body2)))]
       [_else #f])]
    [x1 (symbol? (syntax-object->datum #'x1))
        (syntax-case expr2 ()
          [x2 (symbol? (syntax-object->datum #'x2))
              (or (module-identifier=? #'x1 #'x2)
                  (eqv? (env1 (syntax-object->datum #'x1))
                        (env2 (syntax-object->datum #'x2))))]
          [_else #f])]
    [_else #f]))

;; convert syntax into a list of symbols
(define (syntax->symbols stx)
  (syntax-case stx ()
    [(vars ...)
     (map
      (lambda (s)
        (syntax-object->datum s))
      (syntax->list #'(vars ...)))]))

;; alph=: target-expr target-expr -> boolean
;; are two target expressions alpha-equivalent?
(define (alpha= expr1 expr2)
  (alpha=/env empty-env empty-env expr1 expr2))

(define-syntax (check-unsupported-lambda stx)
  (syntax-case stx ()
    [(_ expr)
     #'(with-handlers ([(lambda (x) #t)
                        (lambda (the-exn)
                          (string=? "lambda: Not all lambda-expressions supported"
                                    (exn-message the-exn)))])
         expr)]))

(define-syntax (check-unsupported-let stx)
  (syntax-case stx ()
    [(_ expr)
     #'(with-handlers ([(lambda (x) #t)
                        (lambda (the-exn)
                          (string=? "let-values: Not all let-values-expressions supported"
                                    (exn-message the-exn)))])
         expr)]))

;; **************************************************
;; **************************************************
;; ACTUAL TESTS

{and
 ;; BASE CASES
 (alpha= (normalize-term (expand (syntax car)))
         (expand (syntax car)))
 (alpha= (normalize-term (expand (syntax (+ 1 1))))
         (expand (syntax (+ 1 1))))
 (alpha= (normalize-term (expand (syntax (lambda (x) 3))))
         (expand (syntax (lambda (x) 3))))
 
 (alpha= (normalize-term (expand (syntax (lambda (x) x))))
         (expand (syntax (lambda (x) x))))
 
 (alpha= (normalize-term (expand (syntax (lambda (x y z) 3))))
         (expand (syntax (lambda (x y z) 3))))
 (alpha= (normalize-term (expand (syntax (if #t 1))))
         (expand (syntax (if #t 1))))
 (alpha= (normalize-term (expand (syntax (if #t 1 2))))
         (expand (syntax (if #t 1 2))))
 (alpha= (normalize-term (expand (syntax (let ([x 1]) x))))
         (expand (syntax ((lambda (x) x) 1))))
 (alpha= (normalize-term (expand (syntax (void))))
         (expand (syntax (void))))
 (alpha= (normalize-term (expand (syntax (+ 1 2 3))))
         (expand (syntax (+ 1 2 3))))
 (alpha= (normalize-term (expand (syntax ())))
         (expand (syntax ())))
 (alpha= (normalize-term (expand (syntax '(1 2 3))))
         (expand (syntax '(1 2 3))))
 
 ;; INDUCTIVE CASES
 (alpha= (normalize-term (expand (syntax (* (+ 1 2) 3))))
         (expand (syntax ((lambda (x) (* x 3)) (+ 1 2)))))
 
 (alpha= (normalize-term (expand (syntax (if (+ 1 2) 3))))
         (expand (syntax ((lambda (x) (if x 3)) (+ 1 2)))))
 
 (alpha= (normalize-term (expand (syntax (if (+ 1 2) 3 4))))
         (expand (syntax ((lambda (x) (if x 3 4)) (+ 1 2)))))
 
 (alpha= (normalize-term (expand (syntax (* (+ 1)))))
         (expand (syntax ((lambda (x0) (* x0)) (+ 1)))))
 
 (alpha= (normalize-term (expand (syntax (* (+ (+ (+ 1 2) 3) 4) (+ 5 6)))))
         (expand (syntax ((lambda (x0)
                            ((lambda (x1)
                               ((lambda (x2)
                                  ((lambda (x3) (* x2 x3))
                                   (+ 5 6)))
                                (+ x1 4)))
                             (+ x0 3)))
                          (+ 1 2)))))
 
 (alpha= (normalize-term (expand (syntax (* (+ 1 2) (+ 1 (+ 2 (+ 3 4)))))))
         (expand (syntax ((lambda (x0)
                            ((lambda (x1)
                               ((lambda (x2)
                                  ((lambda (x3)
                                     (* x0 x3))
                                   (+ 1 x2)))
                                (+ 2 x1)))
                             (+ 3 4)))
                          (+ 1 2)))))
 
 (alpha= (normalize-term (expand (syntax (if (if #t #f #t) #t #t))))
         (expand (syntax ((lambda (x) (if x #t #t)) (if #t #f #t)))))
 
 (alpha= (normalize-term (expand (syntax (lambda (x) (if (if x 1 2) 3 4)))))
         (expand (syntax (lambda (x)
                           ((lambda (y0) (if y0 3 4))
                            (if x 1 2))))))
 
 (alpha= (normalize-term (expand (syntax ((lambda () 3)))))
         (expand (syntax ((lambda () 3)))))
 
 (alpha= (normalize-term (expand (syntax (if ((lambda () 7)) 1 2))))
         (expand (syntax ((lambda (x) (if x 1 2))
                          ((lambda () 7))))))
 
 (alpha= (normalize-term (expand (syntax ((if #t (lambda () 1) (lambda () 2))))))
         (expand (syntax ((lambda (x) (x)) (if #t (lambda () 1) (lambda () 2))))))
 
 (alpha= (normalize-term (expand (syntax (call/cc (lambda (x) x)))))
         (expand (syntax (call/cc (lambda (x) x)))))
 
 (alpha= (normalize-term (expand (syntax (call/cc (f (g 7))))))
         (expand (syntax ((lambda (x0)
                            ((lambda (x1) (call/cc x1))
                             (f x0)))
                          (g 7)))))
 
 ;; expected error cases:
 (check-unsupported-lambda
  (normalize-term (expand (syntax (lambda (x y z) 3 4)))))
 (check-unsupported-lambda
  (normalize-term (expand (syntax (lambda x x)))))
 (check-unsupported-let
  (normalize-term (expand (syntax (let-values ([(x y) (values 1 2)]) (+ x y))))))
 (check-unsupported-let
  (normalize-term (expand (syntax (let ([x 1] [y 2]) (+ x y))))))
 
 (alpha= (normalize-term (expand (syntax (begin))))
         (expand (syntax (void))))
 
 (alpha= (normalize-term (expand (syntax (begin 1))))
         (syntax (#%datum . 1)))
 
 (alpha= (normalize-term (expand (syntax (begin 1 2 3))))
         (normalize-term (expand (syntax (let ([throw-away 1])
                                           (let ([throw-away 2])
                                             3))))))
 
 (with-handlers ([(lambda (x) #t)
                  (lambda (the-exn) #f)])
   (normalize-term
    (expand
     (syntax
      (cond
        [(null? l) 1]
        [(zero? (car l)) (k 0)]
        [else
         (* (car l) (cdr l))])))))
 #t
 }
