(require "test-helpers.ss")
(require/expose "p-lambda-lib.ss" (lift normalize-term))

(define (normalize-def a-def)
  (syntax-case a-def (define lambda case-lambda)
    [(define f (lambda formals body-exprs ...))
     (let ([new-body (normalize-term #'(begin body-exprs ...))])
       #`(define f (lambda formals #,new-body)))]
    [(define f
       (case-lambda cases ...))
     (let ([new-cases (normalize-cases (syntax->list #'(cases ...)))])
       #`(define f
           (case-lambda #,@new-cases)))]))

(define (normalize-cases cases)
  (if (null? cases) '()
      (cons
       (with-syntax ([(formals body-exprs ...) (car cases)])
         (let ([new-body (normalize-term #'(begin body-exprs ...))])
           #`(formals #,new-body)))
       (normalize-cases (cdr cases)))))

(define myprint void)

(define (a-eval expr)
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
             (let ([defs (map normalize-def defs)]
                   [body (normalize-term body)])
               (myprint "defs = ~s~n" defs)
               (myprint "body = ~s~n" body)
               (for-each eval defs)
               (eval body))))]))))

(define side-effect!
  (let ([i 0])
    (lambda (n)
      (begin0 (= n i) (set! i (add1 n))))))

;; ****************************************

(zero? (a-eval (syntax 0)))
(eqv? 'hello-world
      (a-eval (syntax 'hello-world)))
(a-eval (syntax (if #t #t)))
(a-eval (syntax (if #t #t #f)))
(a-eval (syntax (if #f #f #t)))
(= 2 (a-eval (syntax (+ 1 1))))
(= 3 (a-eval (syntax (+ (+ 1 1) 1))))
(define (g x) x)
(define (f x y) (+ x y))
(= (let ([x0 7])
     (let ([x1 (f (g  12) 3)]
           [y (f (g 17) x0)])
       (+ x1 y)))
   (a-eval (syntax (let ([x0 7])
                     (let ([x1 (f (g  12) 3)]
                           [y (f (g 17) x0)])
                       (+ x1 y))))))
(= (let ([x 7])
     (let ([x (f (g 12) 3)]
           [y (f (g 17) x)])
       (+ x y)))
   (a-eval (syntax (let ([x 7])
                     (let ([x (f (g 12) 3)]
                           [y (f (g 17) x)])
                       (+ x y))))))

(not (= (let ([x 7])
          (let ([g0 (g 12)])
            (let ([x (f g0 3)])
              (let ([g1 (g 17)])
                (let ([y (f g1 x)])
                  (+ x y))))))
        (a-eval (syntax (let ([x 7])
                          (let ([x (f (g 12) 3)]
                                [y (f (g 17) x)])
                            (+ x y)))))))

(begin (side-effect! 0) #t)
(and (zero? (a-eval (syntax
                     (begin0
                       (begin (side-effect! 0) 0)
                       (begin (side-effect! 1) 1)
                       (begin (side-effect! 2) 2)))))
     (side-effect! 3))

(define even0?
  (a-eval
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
  (a-eval
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

           
     
    