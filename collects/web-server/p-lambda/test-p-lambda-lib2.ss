(require "test-helpers.ss")
(require/expose "p-lambda-lib.ss" (lift normalize-term))

(define (normalize-def a-def)
  (syntax-case a-def (define lambda case-lambda)
    [(define f (lambda formals body-exprs ...))
     (let-values ([(new-body body-defs)
                   (normalize-term #'(begin body-exprs ...))])
       (cons
        #`(define f (lambda formals #,new-body))
        body-defs))]
    [(define f
       (case-lambda cases ...))
     (let-values ([(new-cases case-defs)
                   (normalize-cases (syntax->list #'(cases ...)))])
       (cons #`(define f
                 (case-lambda #,@new-cases))
             case-defs))]))

(define (normalize-cases cases)
  (if (null? cases) (values '() '())
      (with-syntax ([(formals body-exprs ...) (car cases)])
        (let-values ([(new-body body-defs)
                      (normalize-term #'(begin body-exprs ...))]
                     [(rest-cases case-defs) (normalize-cases (cdr cases))])
          (values
           (cons #`(formals #,new-body) rest-cases)
           (append body-defs case-defs))))))


(define myprint void)

(define (p-eval expr)
  (let flatten-begins ([e expr])
    (let ([top-e (expand-to-top-form e)])
      (syntax-case top-e (begin)
        [(begin exprs ...)
         (foldl
          (lambda (e old-val)
            (flatten-begins e))
          (void)
          (syntax->list #'(exprs ...)))]
        [not-a-begin
         (let ([ex (expand top-e)])
           (let-values ([(body defs) (lift ex)])
             (let ([defs
                    (let loop ([defs defs])
                      (if (null? defs) '()
                          (append (normalize-def (car defs))
                                  (loop (cdr defs)))))])
               (let-values ([(body body-defs) (normalize-term body)])
                 (let ([defs (append body-defs defs)])
                   (myprint "defs = ~s~n" defs)
                   (myprint "body = ~s~n" body)
                   (for-each eval defs)
                   (eval body))))))]))))

(define side-effect!
  (let ([i 0])
    (lambda (n)
      (begin0 (= n i) (set! i (add1 n))))))

;; ****************************************
(define (g x) x)
(define (f x y) (+ x y))

(= 1 (p-eval (syntax (g (g 1)))))
(= 1 (p-eval (syntax (+ (g 1)))))
(= 4 (p-eval (syntax (+ 2 (g 2)))))
(= 3 (p-eval (syntax (+ (g 1) (g 2)))))
(= 6 (p-eval (syntax (+ (g 1) (g 2) (g 3)))))
(= 10 (p-eval (syntax (+ (+ (g 1) (g 2)) 7))))
(= 6 (p-eval (syntax (+ (+ (g 1) (g 2)) (g 3)))))
(= 6 (p-eval (syntax (+ (g 1) 2 (g 3)))))
(= 7 (p-eval (syntax (+ (g 1) (+ 1 2) (g 3)))))
(= 6 (p-eval (syntax (+ (g 0) (+ (g 1) (g 2)) (g 3)))))
(= 1 (p-eval (syntax (+ (+ (+ (+ (g 1))))))))
(= 1 (p-eval (syntax (if (g #t) 1 2))))
(= 1 (p-eval (syntax (if (g #t) (g 1) (g 2)))))
(= 3 (p-eval (syntax (if #t
                         (+ (g 1) (g 2))
                         (- (g 1) (g 2))))))
(= 4 (p-eval (syntax (let ([x (if (g #t)
                                  (+ (g 1) (g 2))
                                  (- (g 1) (g 2)))])
                       (+ x 1)))))
(= 3 (p-eval (syntax (+ (if (g #t) (g 1) (g 2))
                        (if (g #f) (g 1) (g 2))))))
(= 7 (p-eval (syntax (if (if (g #t) (g #f) (g #t))
                         (g 6)
                         (g 7)))))
(= 10 (p-eval (syntax (+ 1
                        (if (g #t)
                            (+ 3 (g 2))
                            (+ 4 (g 3)))
                        (g 4)))))

(zero? (p-eval (syntax 0)))
(eqv? 'hello-world
      (p-eval (syntax 'hello-world)))
(p-eval (syntax (if #t #t)))
(p-eval (syntax (if #t #t #f)))
(p-eval (syntax (if #f #f #t)))
(= 2 (p-eval (syntax (+ 1 1))))
(= 3 (p-eval (syntax (+ (+ 1 1) 1))))
(= 11 (p-eval (syntax (f (g 10) 1))))
(= 6 (p-eval (syntax (let-values ([(x y z) (values 1 2 3)]) (+ x y z)))))
(= (let ([x0 7])
     (let ([x1 (f (g  12) 3)]
           [y (f (g 17) x0)])
       (+ x1 y)))
   (p-eval (syntax (let ([x0 7])
                     (let ([x1 (f (g  12) 3)]
                           [y (f (g 17) x0)])
                       (+ x1 y))))))
(= (let ([x 7])
     (let ([x (f (g 12) 3)]
           [y (f (g 17) x)])
       (+ x y)))
   (p-eval (syntax (let ([x 7])
                     (let ([x (f (g 12) 3)]
                           [y (f (g 17) x)])
                       (+ x y))))))

(not (= (let ([x 7])
          (let ([g0 (g 12)])
            (let ([x (f g0 3)])
              (let ([g1 (g 17)])
                (let ([y (f g1 x)])
                  (+ x y))))))
        (p-eval (syntax (let ([x 7])
                          (let ([x (f (g 12) 3)]
                                [y (f (g 17) x)])
                            (+ x y)))))))

(zero? (p-eval (syntax (begin0 0))))
(zero? (p-eval (syntax (begin0 0 1))))
(begin (side-effect! 0) #t)
(and (zero? (p-eval (syntax
                     (begin0
                       (begin (side-effect! 0) 0)
                       (begin (side-effect! 1) 1)
                       (begin (side-effect! 2) 2)))))
     (side-effect! 3))

(define even0?
  (p-eval
   (syntax
    (letrec ([e? (lambda (n)
                   (or (zero? n)
                       (o? (sub1 n))))]
             [o? (lambda (n)
                   (and (not (zero? n))
                        (e? (sub1 n))))])
      e?))))

(even0? 0)
(even0? 8)
(not (even0? 3))

(define even1?
  (p-eval
   (syntax
    (letrec-values ([(e0? e1?)
                     (values
                      (lambda (n)
                        (or (zero? n)
                            (o1? (sub1 n))))
                      (lambda (n)
                        (or (zero? n)
                            (o0? (sub1 n)))))]
                    [(o0? o1?)
                     (values
                      (lambda (n)
                        (and (not (zero? n))
                             (e1? (sub1 n))))
                      (lambda (n)
                        (and (not (zero? n))
                             (e0? (sub1 n)))))])
      e0?))))

(even1? 0)
(not (even1? 1))
(even1? 2)
(not (even1? 3))
(even1? 4)
(not (even1? 5))
(even1? 6)